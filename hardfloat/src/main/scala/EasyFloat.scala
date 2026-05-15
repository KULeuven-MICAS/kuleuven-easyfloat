import chisel3._
import chisel3.util.Cat
import circt.stage.ChiselStage
import hardfloat._
import scopt.OParser
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import chisel3.util.experimental.{forceName}

case class Config(
    operation : String = "",
    expWidth : Int = 8,
    sigWidth : Int = 24,
    intWidth : Int = 32,
    outExpWidth : Int = 8,
    outSigWidth : Int = 24,
    outputFile : String = "",
    format : String = "hw",
    opsList : Seq[String] = Seq()
)

// These are provided as utilities and hence we implement their "modules" here
class recFNFromFN(expWidth: Int, sigWidth: Int) extends RawModule {
  override def desiredName = s"recFNFromFN_s${sigWidth}_e${expWidth}"
  val io = IO(new Bundle {
    val in  = Input(Bits((expWidth + sigWidth).W))
    val out = Output(Bits((expWidth + sigWidth + 1).W))
  })

  io.out := recFNFromFN(expWidth, sigWidth, io.in)
}

class fNFromRecFN(expWidth: Int, sigWidth: Int) extends RawModule {
  override def desiredName = s"fNFromRecFN_s${sigWidth}_e${expWidth}"
  val io = IO(new Bundle {
    val in  = Input(Bits((expWidth + sigWidth + 1).W))
    val out = Output(Bits((expWidth + sigWidth).W))
  })

  io.out := fNFromRecFN(expWidth, sigWidth, io.in)
}

// ---- Raw float packed-bus helpers ----------------------------------------
// Berkeley's `RawFloat(expWidth, sigWidth)` bundle is:
//   { isNaN:1, isInf:1, isZero:1, sign:1, sExp:(exp+2), sig:(sig+1) }
// We expose raw-format values as a single `UInt` bus in the order produced
// by `Cat`, so the dialect's typed operand surface can stay uniform with
// the existing recoded-bit-vector ops. Layout, MSB first:
//   [isNaN] [isInf] [isZero] [sign] [sExp(exp+2)] [sig(sig+1)]

object RawFloatBus {
  def pack(raw: hardfloat.RawFloat): UInt =
    Cat(raw.isNaN, raw.isInf, raw.isZero, raw.sign, raw.sExp.asUInt, raw.sig)

  def unpack(expWidth: Int, sigWidth: Int, bus: UInt): hardfloat.RawFloat = {
    val out = Wire(new hardfloat.RawFloat(expWidth, sigWidth))
    out.sig    := bus(sigWidth, 0)
    out.sExp   := bus(sigWidth + expWidth + 2, sigWidth + 1).asSInt
    out.sign   := bus(sigWidth + expWidth + 3)
    out.isZero := bus(sigWidth + expWidth + 4)
    out.isInf  := bus(sigWidth + expWidth + 5)
    out.isNaN  := bus(sigWidth + expWidth + 6)
    out
  }
}

// Recoded float → packed raw bus, used to feed `AddRawFNBus` /
// `MulRawFNBus` from existing recoded SSA values.
class RecFNToRawFNBus(expWidth: Int, sigWidth: Int) extends RawModule {
  override def desiredName = s"RecFNToRawFNBus_s${sigWidth}_e${expWidth}"
  val io = IO(new Bundle {
    val in  = Input(Bits((expWidth + sigWidth + 1).W))
    val out = Output(UInt((expWidth + sigWidth + 7).W))
  })
  io.out := RawFloatBus.pack(hardfloat.rawFloatFromRecFN(expWidth, sigWidth, io.in))
}

// Berkeley `AddRawFN` wrapped with packed raw buses on a/b inputs and a
// packed sig+2 bus on rawOut.
//
// NOTE: `AddRawFN.io.roundingMode` is only consulted to pick the sign of
// zero on complete cancellation. The current hardfloat dialect lowering
// hardcodes RNE (rounding mode = 0) everywhere, so we hardcode it here too.
// When variable rounding modes become a thing in the dialect, plumb the
// rm through.
class AddRawFNBus(expWidth: Int, sigWidth: Int) extends RawModule {
  override def desiredName = s"AddRawFNBus_s${sigWidth}_e${expWidth}"
  val io = IO(new Bundle {
    val subOp      = Input(Bool())
    val a          = Input(UInt((expWidth + sigWidth + 7).W))
    val b          = Input(UInt((expWidth + sigWidth + 7).W))
    val invalidExc = Output(Bool())
    val rawOut     = Output(UInt((expWidth + sigWidth + 9).W))
  })
  val inner = Module(new hardfloat.AddRawFN(expWidth, sigWidth))
  inner.io.subOp        := io.subOp
  inner.io.a            := RawFloatBus.unpack(expWidth, sigWidth, io.a)
  inner.io.b            := RawFloatBus.unpack(expWidth, sigWidth, io.b)
  inner.io.roundingMode := 0.U
  io.invalidExc         := inner.io.invalidExc
  io.rawOut             := RawFloatBus.pack(inner.io.rawOut)
}

// Berkeley `MulRawFN` wrapped with packed raw buses.
class MulRawFNBus(expWidth: Int, sigWidth: Int) extends RawModule {
  override def desiredName = s"MulRawFNBus_s${sigWidth}_e${expWidth}"
  val io = IO(new Bundle {
    val a          = Input(UInt((expWidth + sigWidth + 7).W))
    val b          = Input(UInt((expWidth + sigWidth + 7).W))
    val invalidExc = Output(Bool())
    val rawOut     = Output(UInt((expWidth + sigWidth + 9).W))
  })
  val inner = Module(new hardfloat.MulRawFN(expWidth, sigWidth))
  inner.io.a    := RawFloatBus.unpack(expWidth, sigWidth, io.a)
  inner.io.b    := RawFloatBus.unpack(expWidth, sigWidth, io.b)
  io.invalidExc := inner.io.invalidExc
  io.rawOut     := RawFloatBus.pack(inner.io.rawOut)
}

// Berkeley `RoundRawFNToRecFN` wrapped with a packed sig+2 raw input bus.
// NOTE: `infiniteExc` is hardcoded false — none of our add/mul paths
// produce an exception-tagged infinity. Plumb when divsqrt joins the
// pipeline.
class RoundRawFNToRecFNBus(expWidth: Int, sigWidth: Int) extends RawModule {
  override def desiredName = s"RoundRawFNToRecFNBus_s${sigWidth}_e${expWidth}"
  val io = IO(new Bundle {
    val invalidExc     = Input(Bool())
    val in             = Input(UInt((expWidth + sigWidth + 9).W))
    val roundingMode   = Input(UInt(3.W))
    val detectTininess = Input(UInt(1.W))
    val out            = Output(Bits((expWidth + sigWidth + 1).W))
    val exceptionFlags = Output(Bits(5.W))
  })
  val inner = Module(new hardfloat.RoundRawFNToRecFN(expWidth, sigWidth, 0))
  inner.io.invalidExc     := io.invalidExc
  inner.io.infiniteExc    := false.B
  inner.io.in             := RawFloatBus.unpack(expWidth, sigWidth + 2, io.in)
  inner.io.roundingMode   := io.roundingMode
  inner.io.detectTininess := io.detectTininess
  io.out                  := inner.io.out
  io.exceptionFlags       := inner.io.exceptionFlags
}

// Wraps hardfloat.MulAddRecFN to give it the _s${sigWidth}_e${expWidth} suffix
// convention (Berkeley's own desiredName uses _e_s order).
class MulAddRecFNNamed(expWidth: Int, sigWidth: Int) extends RawModule {
  override def desiredName = s"MulAddRecFN_s${sigWidth}_e${expWidth}"
  val io = IO(new Bundle {
    val op = Input(Bits(2.W))
    val a = Input(Bits((expWidth + sigWidth + 1).W))
    val b = Input(Bits((expWidth + sigWidth + 1).W))
    val c = Input(Bits((expWidth + sigWidth + 1).W))
    val roundingMode = Input(UInt(3.W))
    val detectTininess = Input(UInt(1.W))
    val out = Output(Bits((expWidth + sigWidth + 1).W))
    val exceptionFlags = Output(Bits(5.W))
  })

  val inner = Module(new hardfloat.MulAddRecFN(expWidth, sigWidth))
  inner.io.op := io.op
  inner.io.a := io.a
  inner.io.b := io.b
  inner.io.c := io.c
  inner.io.roundingMode := io.roundingMode
  inner.io.detectTininess := io.detectTininess
  io.out := inner.io.out
  io.exceptionFlags := inner.io.exceptionFlags
}

// Wraps hardfloat.RecFNToRecFN so the emitted module is named with all four widths.
class RecFNToRecFNNamed(inExpWidth: Int, inSigWidth: Int, outExpWidth: Int, outSigWidth: Int) extends RawModule {
  override def desiredName = s"RecFNToRecFN_is${inSigWidth}_ie${inExpWidth}_os${outSigWidth}_oe${outExpWidth}"
  val io = IO(new Bundle {
    val in = Input(Bits((inExpWidth + inSigWidth + 1).W))
    val roundingMode = Input(UInt(3.W))
    val detectTininess = Input(UInt(1.W))
    val out = Output(Bits((outExpWidth + outSigWidth + 1).W))
    val exceptionFlags = Output(Bits(5.W))
  })

  val inner = Module(new hardfloat.RecFNToRecFN(inExpWidth, inSigWidth, outExpWidth, outSigWidth))
  inner.io.in := io.in
  inner.io.roundingMode := io.roundingMode
  inner.io.detectTininess := io.detectTininess
  io.out := inner.io.out
  io.exceptionFlags := inner.io.exceptionFlags
}

// Wraps hardfloat.CompareRecFN to give it a name with widths in it
class CompareRecFNNamed(expWidth: Int, sigWidth: Int) extends RawModule {
  override def desiredName = s"CompareRecFN_s${sigWidth}_e${expWidth}"
  val io = IO(new Bundle {
    val a = Input(Bits((expWidth + sigWidth + 1).W))
    val b = Input(Bits((expWidth + sigWidth + 1).W))
    val signaling = Input(Bool())
    val lt = Output(Bool())
    val eq = Output(Bool())
    val gt = Output(Bool())
    val exceptionFlags = Output(Bits(5.W))
  })

  val inner = Module(new hardfloat.CompareRecFN(expWidth, sigWidth))
  inner.io.a := io.a
  inner.io.b := io.b
  inner.io.signaling := io.signaling
  io.lt := inner.io.lt
  io.eq := inner.io.eq
  io.gt := inner.io.gt
  io.exceptionFlags := inner.io.exceptionFlags
}



// The Wrapper Module that holds all requested components
class EasyFloatTop(configs: Seq[Config]) extends RawModule {
  configs.zipWithIndex.foreach { case (cfg, index) =>
    // Get the module as a RawModule first
    val mod: RawModule = cfg.operation match {
      case "AddRecFN"  => Module(new hardfloat.AddRecFN(cfg.expWidth, cfg.sigWidth))
      case "MulRecFN"  => Module(new hardfloat.MulRecFN(cfg.expWidth, cfg.sigWidth))
      case "recFNFromFN" => Module(new recFNFromFN(cfg.expWidth, cfg.sigWidth))
      case "fNFromRecFN" => Module(new fNFromRecFN(cfg.expWidth, cfg.sigWidth))
      case "RecFNToIN" => Module(new hardfloat.RecFNToIN(cfg.expWidth, cfg.sigWidth, cfg.intWidth))
      case "INToRecFN" => Module(new hardfloat.INToRecFN(cfg.expWidth, cfg.sigWidth, cfg.intWidth))
      case "CompareRecFN" => Module(new CompareRecFNNamed(cfg.expWidth, cfg.sigWidth))
      case "RecFNToRecFN" => Module(new RecFNToRecFNNamed(cfg.expWidth, cfg.sigWidth, cfg.outExpWidth, cfg.outSigWidth))
      case "MulAddRecFN" => Module(new MulAddRecFNNamed(cfg.expWidth, cfg.sigWidth))
      case "RecFNToRawFNBus" => Module(new RecFNToRawFNBus(cfg.expWidth, cfg.sigWidth))
      case "AddRawFNBus" => Module(new AddRawFNBus(cfg.expWidth, cfg.sigWidth))
      case "MulRawFNBus" => Module(new MulRawFNBus(cfg.expWidth, cfg.sigWidth))
      case "RoundRawFNToRecFNBus" => Module(new RoundRawFNToRecFNBus(cfg.expWidth, cfg.sigWidth))
      case _ => throw new Exception(s"Unknown operation: ${cfg.operation}")
    }

    val modWithIo = mod.asInstanceOf[{ def io: Record }]
    modWithIo.io <> DontCare
    dontTouch(modWithIo.io)
  }
}
object EasyFloatGenerator extends App {
  val builder = OParser.builder[Config]
  val parser = {
    import builder._
    OParser.sequence(
      programName("easyfloatgenerator"),
      // Allow passing multiple operations as a comma-separated list
      opt[Seq[String]]('i', "ops").valueName("op1,op2").action((x, c) => c.copy(opsList = x)),
      opt[Int]("expWidth").action((x, c) => c.copy(expWidth = x)),
      opt[Int]("sigWidth").action((x, c) => c.copy(sigWidth = x)),
      opt[Int]("intWidth").action((x, c) => c.copy(intWidth = x)),
      opt[String]("output-file").action((x, c) => c.copy(outputFile = x)),
      opt[String]("format").action((x, c) => c.copy(format = x))
    )
  }

  OParser.parse(parser, args, Config()) match {
    case Some(config) =>
      // Convert the list of strings into a list of Config objects
      val tasks = if (config.opsList.nonEmpty) {
        config.opsList.map { opString =>
          // Parse names like "AddRecFN_s24_e8" or "RecFNToRecFN_is24_ie8_os11_oe5"
          val parts = opString.split("_")
          val opName = parts(0)
          // The dual-width form prefixes widths with "is"/"ie" (input) and
          // "os"/"oe" (output). Single-width ops use bare "s"/"e"/"i".
          def find(prefix: String) =
            parts.find(p => p.startsWith(prefix) && p.drop(prefix.length).forall(_.isDigit))
              .map(_.drop(prefix.length).toInt)
          val isW = find("is")
          val ieW = find("ie")
          val osW = find("os")
          val oeW = find("oe")
          val sW = isW.orElse(parts.find(_.startsWith("s")).map(_.drop(1).toInt)).getOrElse(config.sigWidth)
          val eW = ieW.orElse(parts.find(_.startsWith("e")).map(_.drop(1).toInt)).getOrElse(config.expWidth)
          val iW = parts.find(p => p.startsWith("i") && p.drop(1).forall(_.isDigit))
            .map(_.drop(1).toInt).getOrElse(config.intWidth)
          val oS = osW.getOrElse(config.outSigWidth)
          val oE = oeW.getOrElse(config.outExpWidth)

          Config(
            operation = opName, expWidth = eW, sigWidth = sW, intWidth = iW,
            outExpWidth = oE, outSigWidth = oS,
          )
        }
      } else {
        // Fallback to the single operation if no list is provided
        Seq(Config(config.operation, config.expWidth, config.sigWidth, config.intWidth))
      }

      val defaultOpts = Array("--disable-all-randomization", "--disable-layers=Verification")

      // Generate the MLIR using the Wrapper module
      val ir = config.format match {
        case "hw" => ChiselStage.emitHWDialect(new EasyFloatTop(tasks), firtoolOpts = defaultOpts)
        case "firrtl" => ChiselStage.emitFIRRTLDialect(new EasyFloatTop(tasks), firtoolOpts = defaultOpts)
        case _ => throw new Exception("Unsupported format")
      }
    // Check if an output file was provided; if not, print to stdout
    if (config.outputFile.isEmpty) {
      System.out.print(ir)
    } else {
      Files.write(Paths.get(config.outputFile), ir.getBytes(StandardCharsets.UTF_8))
    }
    case None =>
  }
}
