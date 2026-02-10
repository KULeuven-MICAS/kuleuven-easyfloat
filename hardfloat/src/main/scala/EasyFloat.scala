import chisel3._
import circt.stage.ChiselStage
import hardfloat._
import scopt.OParser
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

case class Config(
    operation: String = "",
    expWidth: Int = 8,
    sigWidth: Int = 24,
    intWidth : Int = 32,
    outputFile: String = "out/generated.mlir",
    format: String = "hw"
)

// These are provided as utilities and hence we implement their "modules" here
class recFNFromFNModule(expWidth: Int, sigWidth: Int) extends RawModule {
  val io = IO(new Bundle {
    val in  = Input(Bits((expWidth + sigWidth).W))
    val out = Output(Bits((expWidth + sigWidth + 1).W))
  })

  io.out := recFNFromFN(expWidth, sigWidth, io.in)
}

class fNFromRecFNModule(expWidth: Int, sigWidth: Int) extends RawModule {
  val io = IO(new Bundle {
    val in  = Input(Bits((expWidth + sigWidth + 1).W))
    val out = Output(Bits((expWidth + sigWidth).W))
  })

  io.out := fNFromRecFN(expWidth, sigWidth, io.in)
}


object EasyFloatGenerator extends App {
  val builder = OParser.builder[Config]
  val parser = {
    import builder._
    OParser.sequence(
      programName("easyfloatgenerator"),
      opt[String]("operation").required().action((x, c) => c.copy(operation = x)),
      opt[Int]("expWidth").action((x, c) => c.copy(expWidth = x)),
      opt[Int]("sigWidth").action((x, c) => c.copy(sigWidth = x)),
      opt[Int]("intWidth").action((x, c) => c.copy(intWidth = x)),
      opt[String]("output-file").action((x, c) => c.copy(outputFile = x)),
      opt[String]("format").action((x, c) => c.copy(format = x))
    )
  }

  val defaultOpts = Array("--disable-all-randomization", "--disable-layers=Verification")
  OParser.parse(parser, args, Config()) match {
    case Some(config) =>
      val gen = () => config.operation match {
        case "AddRecFN" => new hardfloat.AddRecFN(config.expWidth, config.sigWidth)
        case "MulRecFN" => new hardfloat.MulRecFN(config.expWidth, config.sigWidth)
        case "recFNFromFN" => new recFNFromFNModule(config.expWidth, config.sigWidth)
        case "fNFromRecFN" => new fNFromRecFNModule(config.expWidth, config.sigWidth)
        case "RecFNToIN" => new hardfloat.RecFNToIN(config.expWidth, config.sigWidth, config.intWidth)
        case "INToRecFN" => new hardfloat.INToRecFN(config.expWidth, config.sigWidth, config.intWidth)
        case _ => throw new Exception(s"Operation ${config.operation} not supported!")
      }
      val ir = config.format match {
        case "hw" => ChiselStage.emitHWDialect(gen(), firtoolOpts = defaultOpts)
        case "firrtl" => ChiselStage.emitFIRRTLDialect(gen(), firtoolOpts = defaultOpts)
        case _ => throw new Exception(s"Format ${config.format} not supported, only support `hw` or `firrtl`")
      }
      Files.write(Paths.get(config.outputFile), ir.getBytes(StandardCharsets.UTF_8))
      println(s"Successfully generated ${config.operation} with expw = ${config.expWidth} sigw = ${config.sigWidth} at ${config.outputFile}")
    case None =>
  }
}
