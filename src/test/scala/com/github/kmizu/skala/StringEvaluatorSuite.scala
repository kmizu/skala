package com.github.kmizu.skala

import munit.FunSuite

class StringEvaluatorSuite extends FunSuite {
  test("string literal") {
    val result = evalExp(Parser.parseExp("\"hello\""))
    assert(result == Value.StringValue("hello"))
  }

  test("string concatenation with ++") {
    val result = evalExp(Parser.parseExp("\"hello\" ++ \" world\""))
    assert(result == Value.StringValue("hello world"))
  }

  test("string concatenation with +") {
    val result = evalExp(Parser.parseExp("\"hello\" + \" world\""))
    assert(result == Value.StringValue("hello world"))
  }

  test("mixed string and int concatenation") {
    val result1 = evalExp(Parser.parseExp("\"count: \" + 42"))
    assert(result1 == Value.StringValue("count: 42"))
    
    val result2 = evalExp(Parser.parseExp("42 + \" items\""))
    assert(result2 == Value.StringValue("42 items"))
  }

  test("string length") {
    val result = evalExp(tStringLength(tString("hello")))
    assert(result == Value.IntValue(5))
  }

  test("string with escape sequences") {
    val result = evalExp(Parser.parseExp("\"hello\\nworld\\ttab\\\"quote\\\"\""))
    assert(result == Value.StringValue("hello\nworld\ttab\"quote\""))
  }

  test("string in variables") {
    val program = Parser.parseProgram("""
      s = "hello";
      t = " world";
      s ++ t
    """)
    val result = evalProgram(program)
    assert(result == Value.StringValue("hello world"))
  }

  test("string comparison") {
    val program = Parser.parseProgram("""
      s1 = "hello";
      s2 = "hello";
      s3 = "world";
      if (s1 == s2) {
        if (s1 != s3) {
          1
        } else {
          0
        }
      } else {
        0
      }
    """)
    val result = evalProgram(program)
    assert(result == Value.IntValue(1))
  }

  test("string in function") {
    val program = Parser.parseProgram("""
      function greet(name) {
        "Hello, " ++ name ++ "!"
      }
      greet("World")
    """)
    val result = evalProgram(program)
    assert(result == Value.StringValue("Hello, World!"))
  }

  test("string operations chained") {
    val program = Parser.parseProgram("""
      s = "a" ++ "b" ++ "c";
      s ++ "d" ++ "e"
    """)
    val result = evalProgram(program)
    assert(result == Value.StringValue("abcde"))
  }
}