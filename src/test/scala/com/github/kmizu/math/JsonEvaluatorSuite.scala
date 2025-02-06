package com.github.kmizu.math
import munit.FunSuite

class JsonEvaluatorSuite extends FunSuite {

  test("1 + 1 ==> 2") {
    val e = """["+", 1, 1]"""
    assertEquals(evalJson(e), 2)
  }

  test("1 + 2 + 3 ==> 6") {
    val e = """["+", 1, ["+", 2, 3]]"""
    assertEquals(evalJson(e), 6)
  }

  test("1 - 1 ==> 0") {
    val e = """["-", 1, 1]"""
    assertEquals(evalJson(e), 0)
  }

  test("1 - 2 ==> -1") {
    val e = """["-", 1, 2]"""
    assertEquals(evalJson(e), -1)
  }

  test("1 * 1 ==> 1") {
    val e = """["*", 1, 1]"""
    assertEquals(evalJson(e), 1)
  }

  test("1 * 0 ==> 0") {
    val e = """["*", 1, 0]"""
    assertEquals(evalJson(e), 0)
  }

  test("2 * 2 ==> 4") {
    val e = """["*", 2, 2]"""
    assertEquals(evalJson(e), 4)
  }

  test("0 / 1 ==> 0") {
    val e = """["/", 0, 1]"""
    assertEquals(evalJson(e), 0)
  }

  test("2 / 1 ==> 2") {
    val e = """["/", 2, 1]"""
    assertEquals(evalJson(e), 2)
  }

  test("6 / 2 ==> 3") {
    val e = """["/", 6, 2]"""
    assertEquals(evalJson(e), 3)
  }

  test("(1 + (2 * 3) - 1) / 2 == 3") {
    val e =
      """["/", ["-", ["+", 1, ["*", 2, 3]], 1], 2]"""
    assertEquals(evalJson(e), 3)
  }
}