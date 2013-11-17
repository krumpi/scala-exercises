package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or  3")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or  3")
  }

  test("demux 0 example") {
    val in, out = new Wire
    demux(in, Nil, out :: Nil)
    in.setSignal(false)
    run

    assert(out.getSignal === false, "demux 1")

    in.setSignal(true)
    run

    assert(out.getSignal === true, "demux 1")
  }

  test("demux0 example") {
    val in = new Wire
    val out = new Wire
    demux(in, Nil, out :: Nil)

    in.setSignal(false)
    run
    assert(out.getSignal === false, "demux0 1")

    in.setSignal(true)
    run
    assert(out.getSignal === true, "demux0 2")

    in.setSignal(false)
    run
    assert(out.getSignal === false, "demux0 3")
  }

  test("demux1 example") {
    val in = new Wire
    val c0 = new Wire
    val out0 = new Wire
    val out1 = new Wire
    demux(in, List(c0), List(out1, out0))

    c0.setSignal(false)
    in.setSignal(false)
    run
    assert(out0.getSignal === false, "demux1 1")
    assert(out1.getSignal === false, "demux1 2")

    in.setSignal(true)
    run
    assert(out0.getSignal === true, "demux1 3")
    assert(out1.getSignal === false, "demux1 4")

    c0.setSignal(true)
    run
    assert(out0.getSignal === false, "demux1 5")
    assert(out1.getSignal === true, "demux1 6")
  }

  test("demux2 example") {
    val in = new Wire
    val c0 = new Wire
    val c1 = new Wire
    val out0 = new Wire
    val out1 = new Wire
    val out2 = new Wire
    val out3 = new Wire
    demux(in, List(c0, c1), List(out3, out2, out1, out0))

    c0.setSignal(false)
    c1.setSignal(false)
    in.setSignal(false)
    run
    assert(out0.getSignal === false, "demux2 1")
    assert(out1.getSignal === false, "demux2 2")
    assert(out2.getSignal === false, "demux2 3")
    assert(out3.getSignal === false, "demux2 4")

    c0.setSignal(true)
    run
    assert(out0.getSignal === false, "demux2 1")
    assert(out1.getSignal === false, "demux2 2")
    assert(out2.getSignal === false, "demux2 3")
    assert(out3.getSignal === false, "demux2 4")

    c0.setSignal(false)
    c1.setSignal(true)
    run
    assert(out0.getSignal === false, "demux2 1")
    assert(out1.getSignal === false, "demux2 2")
    assert(out2.getSignal === false, "demux2 3")
    assert(out3.getSignal === false, "demux2 4")

    c0.setSignal(true)
    c1.setSignal(true)
    run
    assert(out0.getSignal === false, "demux2 1")
    assert(out1.getSignal === false, "demux2 2")
    assert(out2.getSignal === false, "demux2 3")
    assert(out3.getSignal === false, "demux2 4")

    in.setSignal(true)
    c0.setSignal(false)
    c1.setSignal(false)
    run
    assert(out0.getSignal === true, "demux2 1")
    assert(out1.getSignal === false, "demux2 2")
    assert(out2.getSignal === false, "demux2 3")
    assert(out3.getSignal === false, "demux2 4")

    c1.setSignal(true)
    run
    assert(out0.getSignal === false, "demux2 1")
    assert(out1.getSignal === true, "demux2 2")
    assert(out2.getSignal === false, "demux2 3")
    assert(out3.getSignal === false, "demux2 4")

    c1.setSignal(false)
    c0.setSignal(true)
    run
    assert(out0.getSignal === false, "demux2 1")
    assert(out1.getSignal === false, "demux2 2")
    assert(out2.getSignal === true, "demux2 3")
    assert(out3.getSignal === false, "demux2 4")

    c1.setSignal(true)
    c0.setSignal(true)
    run
    assert(out0.getSignal === false, "demux2 1")
    assert(out1.getSignal === false, "demux2 2")
    assert(out2.getSignal === false, "demux2 3")
    assert(out3.getSignal === true, "demux2 4")

    println("here")
  }

}
