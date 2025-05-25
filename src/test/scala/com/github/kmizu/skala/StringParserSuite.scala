package com.github.kmizu.skala

import munit.FunSuite

class StringParserSuite extends FunSuite {
  test("parse string literal") {
    val result = Parser.parseExp("\"hello world\"")
    assert(result == Exp.VString("hello world"))
  }

  test("parse empty string") {
    val result = Parser.parseExp("\"\"")
    assert(result == Exp.VString(""))
  }

  test("parse string with escape sequences") {
    val result = Parser.parseExp("\"hello\\nworld\\ttab\\\"quote\\\"\\\\backslash\"")
    assert(result == Exp.VString("hello\nworld\ttab\"quote\"\\backslash"))
  }

  test("parse string concatenation with ++") {
    val result = Parser.parseExp("\"hello\" ++ \" world\"")
    assert(result == Exp.StringConcat(Exp.VString("hello"), Exp.VString(" world")))
  }

  test("parse multiple string concatenations") {
    val result = Parser.parseExp("\"a\" ++ \"b\" ++ \"c\"")
    assert(result == Exp.StringConcat(
      Exp.StringConcat(Exp.VString("a"), Exp.VString("b")),
      Exp.VString("c")
    ))
  }

  test("parse string in assignment") {
    val result = Parser.parseProgram("s = \"hello\"; s")
    assert(result == Program(
      List(),
      List(
        Exp.Assignment("s", Exp.VString("hello")),
        Exp.Ident("s")
      )
    ))
  }

  test("parse string in function") {
    val result = Parser.parseProgram("""
      function greet(name) {
        "Hello, " ++ name
      }
      greet("World")
    """)
    assert(result.functions.size == 1)
    assert(result.functions.head.name == "greet")
    assert(result.bodies.size == 1)
    assert(result.bodies.head == Exp.Call("greet", List(Exp.VString("World"))))
  }

  test("parse mixed expressions with strings") {
    val result = Parser.parseExp("if (\"hello\" == \"world\") 1 else 0")
    assert(result == Exp.If(
      Exp.BinExp("==", Exp.VString("hello"), Exp.VString("world")),
      Exp.VInt(1),
      Exp.VInt(0)
    ))
  }

  test("parse string concatenation with precedence") {
    val result = Parser.parseExp("\"result: \" ++ (1 + 2)")
    val expected = Exp.StringConcat(
      Exp.VString("result: "),
      Exp.BinExp("+", Exp.VInt(1), Exp.VInt(2))
    )
    assert(result == expected)
  }
}