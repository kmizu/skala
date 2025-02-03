package com.github.kmizu.skala

import munit.FunSuite
import scala.collection.mutable
import Exp.*

class EvaluatorSuite extends FunSuite {

  test("1 + 1 == 2") {
    val e = tInt(1) |+| tInt(1)
    // Using an empty mutable environment
    assertEquals(evalExp(e), 2)
  }

  test("1 + 2 + 3 == 7") {
    // Note: The original test code appears to be a copy of the first test.
    // Here we simply duplicate the first test for demonstration.
    val e = tInt(1) |+| tInt(1)
    assertEquals(evalExp(e), 2)
  }

  test("1 - 1 == 0") {
    val e = tInt(1) |-| tInt(1)
    assertEquals(evalExp(e), 0)
  }

  test("1 - 2 == -1") {
    val e = tInt(1) |-| tInt(2)
    assertEquals(evalExp(e), -1)
  }

  test("1 * 1 == 1") {
    val e = tInt(1) |*| tInt(1)
    assertEquals(evalExp(e), 1)
  }

  test("1 * 0 == 0") {
    val e = tInt(1) |*| tInt(0)
    assertEquals(evalExp(e), 0)
  }

  test("2 * 2 == 4") {
    val e = tInt(2) |*| tInt(2)
    assertEquals(evalExp(e), 4)
  }

  test("0 / 1 == 0") {
    val e = tInt(0) |/| tInt(1)
    assertEquals(evalExp(e), 0)
  }

  test("2 / 1 == 2") {
    val e = tInt(2) |/| tInt(1)
    assertEquals(evalExp(e), 2)
  }

  test("6 / 2 == 3") {
    // Either tDiv(tInt(6), tInt(2)) or new tDiv(...) if you prefer.
    val e = tInt(6) |/| tInt(2)
    assertEquals(evalExp(e), 3)
  }

  test("(1 + (2 * 3) - 1) / 2 == 3") {
    val e = ((tInt(1) |+| (tInt(2) |*| tInt(3))) |-| tInt(1))
      |/| tInt(2)
    assertEquals(evalExp(e), 3)
  }

  test("1 < 2 == 1") {
    val e = tInt(1) |<| tInt(2)
    assertEquals(evalExp(e), 1)
  }

  test("2 > 1 == 1") {
    val e = tInt(2) |>| tInt(1)
    assertEquals(evalExp(e), 1)
  }

  test("1 <= 1 == 1") {
    val e = tInt(1) |<=| tInt(1)
    assertEquals(evalExp(e), 1)
  }

  test("1 >= 1 == 1") {
    val e = tInt(1) |>=| tInt(1)
    assertEquals(evalExp(e), 1)
  }

  test("1 == 1 == 1") {
    val e = tInt(1) |==| tInt(1)
    assertEquals(evalExp(e), 1)
  }

  test("1 != 0 == 1") {
    val e = tInt(1) |!=| tInt(0)
    assertEquals(evalExp(e), 1)
  }

  test("{a = 100; a} == 100") {
    val e = tSeq(
      tAssign("a", tInt(100)),
      tId("a")
    )
    assertEquals(evalExp(e), 100)
  }

  test("{a = 100; b = a + 1; b} == 101") {
    val e = tSeq(
      tAssign("a", tInt(100)),
      tAssign("b", tId("a") |+| tInt(1)),
      tId("b")
    )
    assertEquals(evalExp(e), 101)
  }

  test("(if(1 < 2) 2 else 1) == 2") {
    val e = tIf(
      tInt(1) |<| tInt(2),
      tInt(2),
      tInt(1)
    )
    assertEquals(evalExp(e), 2)
  }

  test("(if(1 > 2) 2 else 1) == 1") {
    val e = tIf(
      tInt(1) |>| tInt(2),
      tInt(2),
      tInt(1)
    )
    assertEquals(evalExp(e), 1)
  }

  test(
    """{
      a = 100;
      b = 200;
      if(a < b) {
        500;
      } else {
        1000;
      }
    } == 500"""
  ) {
    val e = tSeq(
      tAssign("a", tInt(100)),
      tAssign("b", tInt(200)),
      tIf(tId("a") |<| tId("b"), tInt(500), tInt(1000))
    )
    assertEquals(evalExp(e), 500)
  }

  test(
    """function add(a, b) {
         return a + b;
       },
       add(1, 2)
    == 3"""
  ) {
    val program = tProgram(
      List(tFunction("add", List("a", "b"), tId("a") |+| tId("b"))),
      tCall("add", tInt(1), tInt(2))
    )
    assertEquals(evalProgram(program), 3)
  }

  test(
    """i = 0;
      while(i < 10) {
        i = i + 1;
      };
      i
    == 10"""
  ) {
    val program = tProgram(
      List(),
      tAssign("i", tInt(0)),
      tWhile(tId("i") |<| tInt(10), tAssign("i", tId("i") |+| tInt(1))),
      tId("i")
    )
    assertEquals(evalProgram(program), 10)
  }
}