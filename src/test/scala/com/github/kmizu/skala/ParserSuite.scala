package com.github.kmizu.skala

import munit.FunSuite
import Parser._
import Exp._

class ParserSuite extends FunSuite {
  test("parse simple arithmetic") {
    val ast = parseExp("1 + 2 * 3")
    val expected = tInt(1) |+| (tInt(2) |*| tInt(3))
    assertEquals(ast, expected)
  }

  test("parse program with function") {
    val source = "function add(a, b) { a + b } add(1, 2)"
    val program = parseProgram(source)
    assertEquals(evalProgramInt(program), 3)
  }

  test("parse arithmetic with precedence and subtraction") {
    val ast = parseExp("1 + 2 * 3 - 4")
    val expected = (tInt(1) |+| (tInt(2) |*| tInt(3))) |-| tInt(4)
    assertEquals(ast, expected)
    assertEquals(evalExpInt(ast), 3)
  }

  test("parse assignments and sequence") {
    val src = "a = 1; b = a + 2; b"
    val program = parseProgram(src)
    assertEquals(evalProgramInt(program), 3)
  }

  test("parse if else expression") {
    val ast = parseExp("if(1 < 2) { 10 } else { 20 }")
    assertEquals(evalExpInt(ast), 10)
  }

  test("parse while loop") {
    val src = "i = 0; while(i < 3) { i = i + 1 }; i"
    val program = parseProgram(src)
    assertEquals(evalProgramInt(program), 3)
  }

  test("parse multiple functions and calls") {
    val src = "function inc(x) { x + 1 } function double(n) { n * 2 } double(inc(3))"
    val program = parseProgram(src)
    assertEquals(evalProgramInt(program), 8)
  }

  test("parse nested blocks") {
    val ast = parseExp("{ 1; { 2 }; 3 }")
    assertEquals(evalExpInt(ast), 3)
  }
}
