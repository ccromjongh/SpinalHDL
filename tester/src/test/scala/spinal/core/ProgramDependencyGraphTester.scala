package spinal.core

import spinal.core._
import spinal.core.internals._
import spinal.tester.SpinalAnyFunSuite
import scala.collection.mutable.ArrayBuffer
import spinal.core.pdg_analysis.BuildPdgPhase

case class GCD(width: Int = 32) extends Component {
  val io = new Bundle {
    val a             = in UInt(width bits)
    val b             = in UInt(width bits)
    val loadValues    = in Bool()
    val result        = out UInt(width bits)
    val resultIsValid = out Bool()
  }

  val x = Reg(UInt(width bits))
  val y = Reg(UInt(width bits))

  val tmp = x + y
  when(x > y) {
    x := x - y
  }.otherwise {
    y := y - x
  }

  when(io.loadValues) {
    x := io.a
    y := io.b
  }

  io.result        := x
  io.resultIsValid := y === U(0)
}

class ProgramDependencyGraphTester extends SpinalAnyFunSuite {
  import spinal.core.sim._
  import spinal.sim._

  def comp = new GCD()

  val config = SpinalConfig(
    mode = SystemVerilog,
    svInterface = true,
    genLineComments = true,
    phasesInserters = ArrayBuffer[(ArrayBuffer[Phase]) => Unit](
      { phases => phases.insert(phases.indexWhere(_.isInstanceOf[PhaseVerilog]), new BuildPdgPhase) }
    )
  )

  SimConfig.withConfig(config).withFstWave.compile(comp).doSim { dut =>
    // Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10)
    // Wait for reset to be done
    dut.clockDomain.waitSampling

    dut.io.a #= 24
    dut.io.b #= 36
    dut.io.loadValues #= true
    // This seems the easiest way to step the clock
    dut.clockDomain.waitActiveEdge
    dut.io.loadValues #= false
    dut.clockDomain.waitActiveEdgeWhere(dut.io.resultIsValid.toBoolean)
    assert(dut.io.resultIsValid.toBoolean == true, "Expecting to find valid result")
    assert(dut.io.result.toLong == 12, s"Result was ${dut.io.result.toLong}, expected 12")
    dut.io.a #= 24
    dut.io.b #= 72
    dut.clockDomain.assertReset
    dut.io.loadValues #= true
    dut.clockDomain.waitActiveEdge
    dut.io.loadValues #= false
    dut.clockDomain.deassertReset
    dut.clockDomain.waitActiveEdgeWhere(dut.io.resultIsValid.toBoolean)
    assert(dut.io.resultIsValid.toBoolean == true, "Expecting to find valid result")
    assert(dut.io.result.toLong == 24, s"Result was ${dut.io.result.toLong}, expected 24")
  }
}
