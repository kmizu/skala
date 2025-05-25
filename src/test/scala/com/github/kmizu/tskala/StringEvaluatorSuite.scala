package com.github.kmizu.tskala

import munit.FunSuite

class StringEvaluatorSuite extends FunSuite {
  test("string literal") {
    val result = evalExp(Exp.VString("hello"))
    assert(result == Value.StringValue("hello"))
  }

  test("string concatenation with +") {
    val exp = Exp.BinExp("+", Exp.VString("hello"), Exp.VString(" world"))
    val result = evalExp(exp)
    assert(result == Value.StringValue("hello world"))
  }

  test("string concatenation with ++") {
    val exp = Exp.StringConcat(Exp.VString("hello"), Exp.VString(" world"))
    val result = evalExp(exp)
    assert(result == Value.StringValue("hello world"))
  }

  test("string length") {
    val exp = Exp.StringLength(Exp.VString("hello"))
    val result = evalExp(exp)
    assert(result == Value.IntValue(5))
  }

  test("typed string function") {
    val program = Program(
      List(
        Func("greet", List(("name", Type.TString)), Type.TString,
          Exp.StringConcat(
            Exp.StringConcat(Exp.VString("Hello, "), Exp.Ident("name")),
            Exp.VString("!")
          )
        )
      ),
      List(Exp.Call("greet", List(Exp.VString("World"))))
    )
    val result = evalProgram(program)
    assert(result == Value.StringValue("Hello, World!"))
  }

  test("string type checking") {
    val exp = Exp.StringConcat(Exp.VString("hello"), Exp.VString(" world"))
    val tpe = Typer.typeOf(exp, scala.collection.mutable.Map.empty, scala.collection.mutable.Map.empty)
    assert(tpe == Type.TString)
  }

  test("string length type checking") {
    val exp = Exp.StringLength(Exp.VString("hello"))
    val tpe = Typer.typeOf(exp, scala.collection.mutable.Map.empty, scala.collection.mutable.Map.empty)
    assert(tpe == Type.TInt)
  }

  test("type error: string concat with non-string") {
    val exp = Exp.StringConcat(Exp.VString("hello"), Exp.VInt(42))
    intercept[RuntimeException] {
      Typer.typeOf(exp, scala.collection.mutable.Map.empty, scala.collection.mutable.Map.empty)
    }
  }

  test("type error: string length on non-string") {
    val exp = Exp.StringLength(Exp.VInt(42))
    intercept[RuntimeException] {
      Typer.typeOf(exp, scala.collection.mutable.Map.empty, scala.collection.mutable.Map.empty)
    }
  }

  test("parsed string operations") {
    val program = Parser.parseProgram("""
      function concat(s1: String, s2: String): String {
        s1 ++ s2
      }
      s = concat("hello", " world");
      s
    """)
    val result = evalProgram(program)
    assert(result == Value.StringValue("hello world"))
  }

  test("string equality") {
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
}