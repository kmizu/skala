package com.github.kmizu.tskala

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

  test("parse typed function with string parameter") {
    val result = Parser.parseProgram("""
      function greet(name: String): String {
        "Hello, " ++ name
      }
    """)
    assert(result.functions.size == 1)
    val func = result.functions.head
    assert(func.name == "greet")
    assert(func.params == List(("name", Type.TString)))
    assert(func.returnType == Type.TString)
  }

  test("parse typed function with mixed string and int") {
    val result = Parser.parseProgram("""
      function makeMessage(msg: String, count: Int): String {
        msg ++ ": " ++ count
      }
    """)
    assert(result.functions.size == 1)
    val func = result.functions.head
    assert(func.params == List(("msg", Type.TString), ("count", Type.TInt)))
    assert(func.returnType == Type.TString)
  }

  test("parse string operations in typed context") {
    val result = Parser.parseProgram("""
      s1 = "hello";
      s2 = " world";
      result = s1 ++ s2;
      result
    """)
    assert(result.bodies.size == 4)
    assert(result.bodies(2) == Exp.Assignment("result", 
      Exp.StringConcat(Exp.Ident("s1"), Exp.Ident("s2"))
    ))
  }

  test("parse string in if-else") {
    val result = Parser.parseExp("""if ("test" == "test") "equal" else "not equal"""")
    result match {
      case Exp.If(cond, thenExp, elseExp) =>
        assert(cond == Exp.BinExp("==", Exp.VString("test"), Exp.VString("test")))
        assert(thenExp == Exp.VString("equal"))
        assert(elseExp == Exp.VString("not equal"))
      case _ => fail("Expected If expression")
    }
  }

  test("parse list of strings type") {
    val result = Parser.parseProgram("""
      function getStrings(): List[String] {
        ["hello", "world"]
      }
    """)
    assert(result.functions.size == 1)
    val func = result.functions.head
    assert(func.returnType == Type.TList(Type.TString))
  }
}