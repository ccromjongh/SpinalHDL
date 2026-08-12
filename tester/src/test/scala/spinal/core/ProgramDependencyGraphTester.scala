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

  def comp = GCD()

  val config = SpinalConfig(
    mode = SystemVerilog,
    svInterface = true,
    genLineComments = true,
    genPDG = true,
    phasesInserters = ArrayBuffer[(ArrayBuffer[Phase]) => Unit](
      { phases => phases.insert(phases.indexWhere(_.isInstanceOf[PhaseVerilog]), new BuildPdgPhase) }
    )
  )

  SimConfig.withConfig(config).withVcdWave.compile(comp).doSim { dut =>
    // Fork a process to generate the clock on the dut
    // We do not use start reset in this test
    dut.clockDomain.forkStimulus(period = 10, resetCycles = 0)
    // When resetCycles is 0 there is an empty clock period before the first rising edge
    // This 5-unit sleep puts us where the falling edge normally is
    sleep(5)

    dut.io.a #= 24
    dut.io.b #= 36
    dut.io.loadValues #= true
    dut.clockDomain.waitInactiveEdge()
    dut.io.loadValues #= false

    dut.clockDomain.waitInactiveEdgeWhere(dut.io.resultIsValid.toBoolean)
    assert(dut.io.resultIsValid.toBoolean == true, "Expecting to find valid result")
    assert(dut.io.result.toLong == 12, s"Result was ${dut.io.result.toLong}, expected 12")

    dut.io.a #= 24
    dut.io.b #= 72
    dut.clockDomain.assertReset()
    dut.io.loadValues #= true
    dut.clockDomain.waitInactiveEdge()
    dut.io.loadValues #= false
    dut.clockDomain.deassertReset()

    dut.clockDomain.waitInactiveEdgeWhere(dut.io.resultIsValid.toBoolean)
    assert(dut.io.resultIsValid.toBoolean == true, "Expecting to find valid result")
    assert(dut.io.result.toLong == 24, s"Result was ${dut.io.result.toLong}, expected 24")
  }
}

case class Counter(width: Int = 8) extends Component {
  val enable: Bool = in Bool()
  val countOut: UInt = out UInt(width bits)
  private val count = RegInit(U(0, width bits))

  when(enable) {
    count := count + U(1)
  }
  countOut := count
}

class CounterPdgTester extends SpinalAnyFunSuite {
  import spinal.core.sim._
  import spinal.sim._

  def comp = Counter()

  val config = SpinalConfig(
    mode = SystemVerilog,
    svInterface = true,
    genLineComments = true,
    genPDG = true,
    phasesInserters = ArrayBuffer[(ArrayBuffer[Phase]) => Unit](
      { phases => phases.insert(phases.indexWhere(_.isInstanceOf[PhaseVerilog]), new BuildPdgPhase) }
    )
  )

  SimConfig.withConfig(config).withVcdWave.compile(comp).doSim { dut =>
    // Fork a process to generate the clock on the dut
    // We do not use start reset in this test
    dut.clockDomain.forkStimulus(period = 10, resetCycles = 0)

    dut.enable #= true
    dut.clockDomain.waitInactiveEdgeWhere(dut.countOut.toLong == 10)

case class DetectTwoOnes() extends Component {
  val io = new Bundle {
    val input      = in Bool()
    val output     = out Bool()
  }

  object State extends SpinalEnum { val sNone, sOne1, sTwo1s = newElement() }
  val state = RegInit(State.sNone)

  // Tmp signal 1
  val isOne = Bool()
  isOne := io.input
  // Tmp signal 2
  val willBeTwo1s = io.input && (state === State.sOne1 || state === State.sTwo1s)

  io.output := (state === State.sTwo1s)

  private val one: SpinalEnumElement[State.type] = State.sOne1
  val test = state === one
  val test2 = Bits(2 bits)
  test2 := B"01"

  switch(state) {
    is(State.sNone) { when(isOne) { state := State.sOne1 } }
    is(State.sOne1) {
      when(isOne) { state := State.sTwo1s }.otherwise { state := State.sNone }
    }
    is(State.sTwo1s) { when(!isOne) { state := State.sNone } }
  }
}

class DetectTwoOnesTest extends SpinalAnyFunSuite {
  import spinal.core.sim._
  import spinal.sim._

  def comp = DetectTwoOnes()

  val config = SpinalConfig(
    mode = SystemVerilog,
    svInterface = true,
    genLineComments = true,
    genPDG = true,
    phasesInserters = ArrayBuffer[(ArrayBuffer[Phase]) => Unit](
      { phases => phases.insert(phases.indexWhere(_.isInstanceOf[PhaseVerilog]), new BuildPdgPhase) }
    )
  )

  // Inputs and expected results
  val inputs   = Seq(0, 0, 1, 0, 1, 1, 0, 1, 1, 1)
  val expected = Seq(0, 0, 0, 0, 0, 1, 0, 0, 1, 1)

  SimConfig.withConfig(config).withVcdWave.compile(comp).doSim { dut =>
    // Fork a process to generate the clock on the dut
    // We do not use start reset in this test
    dut.clockDomain.forkStimulus(period = 10, resetCycles = 0)

    for (i <- inputs.indices) {
      dut.io.input #= inputs(i).toBoolean
      dut.clockDomain.waitInactiveEdge()
      assert(dut.io.output.toBoolean == expected(i).toBoolean, s"In: ${inputs(i)}, expected out: ${expected(i)}, actual out: ${dut.io.output.toBoolean}")
    }
  }
}
