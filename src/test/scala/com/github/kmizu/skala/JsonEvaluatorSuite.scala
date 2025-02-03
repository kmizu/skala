package com.github.kmizu.skala

import munit.FunSuite

class JsonEvaluatorSuite extends FunSuite {

  test("1 ==> 1") {
    val e = "1"
    assertEquals(evalJsonExp(e), 1)
  }

  test("1 + 2 ==> 3") {
    val e = """["+", 1, 2]"""
    assertEquals(evalJsonExp(e), 3)
  }

  test("1 - 2 ==> -1") {
    val e = """["-", 1, 2]"""
    assertEquals(evalJsonExp(e), -1)
  }

  test("2 * 2 ==> 4") {
    val e = """["*", 2, 2]"""
    assertEquals(evalJsonExp(e), 4)
  }

  test("2 / 2 ==> 1") {
    val e = """["/", 2, 2]"""
    assertEquals(evalJsonExp(e), 1)
  }

  test("1 < 2 ==> 1") {
    val e = """["<", 1, 2]"""
    assertEquals(evalJsonExp(e), 1)
  }

  test("1 > 2 ==> 0") {
    val e = """[">", 1, 2]"""
    assertEquals(evalJsonExp(e), 0)
  }

  test("1 <= 2 ==> 1") {
    val e = """["<=", 1, 2]"""
    assertEquals(evalJsonExp(e), 1)
  }

  test("1 >= 2 ==> 0") {
    val e = """[">=", 1, 2]"""
    assertEquals(evalJsonExp(e), 0)
  }

  test("1 == 1 ==> 1") {
    val e = """["==", 1, 1]"""
    assertEquals(evalJsonExp(e), 1)
  }

  test("1 != 1 ==> 0") {
    val e = """["!=", 1, 1]"""
    assertEquals(evalJsonExp(e), 0)
  }

  test("if(1) then 1 else 2 ==> 1") {
    val e = """["if", 1, 1, 2]"""
    assertEquals(evalJsonExp(e), 1)
  }

  test("{1; 2; 3} ==> 3") {
    val e = """["seq", 1, 2, 3]"""
    assertEquals(evalJsonExp(e), 3)
  }

  test(
    """i = 0;
      |while(i < 10) {
      |  i = i + 1;
      |}
      |i  ==> 10""".stripMargin) {
    val e =
      """["seq", 
           ["assign", "i", 0], 
           ["while", ["<", ["id", "i"], 10], ["assign", "i", ["+", ["id", "i"], 1]]], 
           ["id", "i"]
      ]"""
    assertEquals(evalJsonExp(e), 10)
  }

  test(
    """def add(a, b) { return a + b; },
      |add(1, 2)
      | ==> 3""".stripMargin) {
    val program =
      """[
          ["def", "add", ["a", "b"], ["+", ["id", "a"], ["id", "b"]]],
          ["call", "add", 1, 2]
      ]"""
    assertEquals(evalJsonProgram(program), 3)
  }

  test(
    """def factorial(n) {
      |    if(n == 0) { return 1; } else { return n * factorial(n - 1); }
      |},
      |factorial(5)
      | ==> 120""".stripMargin) {
    val program =
      """[
          ["def", "factorial", ["n"], 
            ["if", ["==", ["id", "n"], 0], 
                1, 
                ["*", ["id", "n"], ["call", "factorial", ["-", ["id", "n"], 1]]]
            ]
          ],
          ["call", "factorial", 5]
      ]"""
    assertEquals(evalJsonProgram(program), 120)
  }
}