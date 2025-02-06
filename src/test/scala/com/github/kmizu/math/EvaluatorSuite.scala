package com.github.kmizu.math

import munit.FunSuite

import com.github.kmizu.math.Exp.*

class EvaluatorSuite extends FunSuite {

  test("1 + 1 ==> 2") {
    val e = tInt(1) |+| tInt(1)
    assertEquals(eval(e), 2)
  }

  test("1 + 2 + 3 ==> 6") {
    val e = tInt(1) |+| tInt(2) |+| tInt(3)
    assertEquals(eval(e), 6)
  }

  test("1 - 1 ==> 0") {
    val e = tInt(1) |-| tInt(1)
    assertEquals(eval(e), 0)
  }

  test("1 - 2 ==> -1") {
    val e = tInt(1) |-| tInt(2)
    assertEquals(eval(e), -1)
  }

  test("1 * 1 == 1") {
    val e = tInt(1) |*| tInt(1)
    assertEquals(eval(e), 1)
  }

  test("1 * 0 == 0") {
    val e = tInt(1) |*| tInt(0)
    assertEquals(eval(e), 0)
  }

  test("2 * 2 == 4") {
    val e = tInt(2) |*| tInt(2)
    assertEquals(eval(e), 4)
  }

  test("0 / 1 == 0") {
    val e = tInt(0) |/| tInt(1)
    assertEquals(eval(e), 0)
  }

  test("2 / 1 == 2") {
    val e = tInt(2) |/| tInt(1)
    assertEquals(eval(e), 2)
  }

  test("6 / 2 == 3") {
    // If tDiv is defined as a class, you might use `new tDiv(...)` here.
    val e = tInt(6) |/| tInt(2)
    assertEquals(eval(e), 3)
  }

  test("(1 + (2 * 3) - 1) / 2 == 3") {
    val e = (tInt(1) |+| (tInt(2) |*| tInt(3)) |-| tInt(1)) 
      |/| tInt(2)
    assertEquals(eval(e), 3)
  }
}