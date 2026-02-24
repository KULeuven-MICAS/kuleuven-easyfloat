import chisel3._
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
          // Basic logic to parse strings like "AddRecFN_s24_e8"
          val parts = opString.split("_")
          val opName = parts(0)
          // Extract widths from string, otherwise use defaults from CLI
          val sW = parts.find(_.startsWith("s")).map(_.drop(1).toInt).getOrElse(config.sigWidth)
          val eW = parts.find(_.startsWith("e")).map(_.drop(1).toInt).getOrElse(config.expWidth)
          val iW = parts.find(_.startsWith("i")).map(_.drop(1).toInt).getOrElse(config.intWidth)

          Config(operation = opName, expWidth = eW, sigWidth = sW, intWidth = iW)
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
