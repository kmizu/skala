package com.github.kmizu.tskala

import munit.FunSuite
import Exp._
import Type._
import Parser._

class ParserPolymorphismSuite extends FunSuite {
  
  test("parse let expression") {
    val src = "let x = 42 in x + 1"
    val exp = parseExp(src)
    assertEquals(exp, Let("x", VInt(42), BinExp("+", Ident("x"), VInt(1))))
  }
  
  test("parse nested let expressions") {
    val src = "let x = 1 in let y = 2 in x + y"
    val exp = parseExp(src)
    assertEquals(exp, 
      Let("x", VInt(1), 
        Let("y", VInt(2), 
          BinExp("+", Ident("x"), Ident("y"))
        )
      )
    )
  }
  
  test("parse lambda with single parameter") {
    val src = "lambda x -> x + 1"
    val exp = parseExp(src)
    assertEquals(exp, Lambda(List("x"), BinExp("+", Ident("x"), VInt(1))))
  }
  
  test("parse lambda with fn keyword") {
    val src = "fn x -> x * 2"
    val exp = parseExp(src)
    assertEquals(exp, Lambda(List("x"), BinExp("*", Ident("x"), VInt(2))))
  }
  
  test("parse lambda with multiple parameters") {
    val src = "lambda (x, y) -> x + y"
    val exp = parseExp(src)
    assertEquals(exp, Lambda(List("x", "y"), BinExp("+", Ident("x"), Ident("y"))))
  }
  
  test("parse lambda with => syntax") {
    val src = "fn x => x * x"
    val exp = parseExp(src)
    assertEquals(exp, Lambda(List("x"), BinExp("*", Ident("x"), Ident("x"))))
  }
  
  test("parse function application") {
    val src = "(lambda x -> x + 1)(42)"
    val exp = parseExp(src)
    assertEquals(exp, 
      Apply(
        Lambda(List("x"), BinExp("+", Ident("x"), VInt(1))),
        List(VInt(42))
      )
    )
  }
  
  test("parse let with lambda") {
    val src = "let f = lambda x -> x * 2 in f(21)"
    val exp = parseExp(src)
    assertEquals(exp,
      Let("f", 
        Lambda(List("x"), BinExp("*", Ident("x"), VInt(2))),
        Call("f", List(VInt(21)))
      )
    )
  }
  
  test("parse type variables in function signature") {
    val src = """
      function id(x: a): a {
        x
      }
      id(42)
    """
    val prog = parseProgram(src)
    assertEquals(prog.functions.length, 1)
    assertEquals(prog.functions.head.params, List(("x", TVar("a"))))
    assertEquals(prog.functions.head.returnType, TVar("a"))
  }
  
  test("parse function type") {
    val src = """
      function apply(f: (Int) -> Int, x: Int): Int {
        f(x)
      }
      apply(lambda y -> y + 1, 42)
    """
    val prog = parseProgram(src)
    assertEquals(prog.functions.length, 1)
    assertEquals(prog.functions.head.params(0)._2, TFunc(List(TInt), TInt))
  }
  
  test("parse polymorphic list function") {
    val src = """
      function head(lst: List[a]): a {
        lst[0]
      }
      head([1, 2, 3])
    """
    val prog = parseProgram(src)
    assertEquals(prog.functions.head.params, List(("lst", TList(TVar("a")))))
    assertEquals(prog.functions.head.returnType, TVar("a"))
  }
  
  test("parse complex lambda expression") {
    val src = "let compose = fn (f, g) -> fn x -> f(g(x)) in compose"
    val exp = parseExp(src)
    assert(exp match {
      case Let("compose", Lambda(List("f", "g"), Lambda(List("x"), _)), Ident("compose")) => true
      case _ => false
    })
  }
  
  test("parse chained function applications") {
    val src = "f(x)(y)(z)"
    val exp = parseExp(src)
    assertEquals(exp,
      Apply(
        Apply(
          Call("f", List(Ident("x"))),
          List(Ident("y"))
        ),
        List(Ident("z"))
      )
    )
  }
  
  test("parse lambda in list") {
    val src = "[lambda x -> x + 1, lambda x -> x * 2]"
    val exp = parseExp(src)
    assertEquals(exp,
      VList(List(
        Lambda(List("x"), BinExp("+", Ident("x"), VInt(1))),
        Lambda(List("x"), BinExp("*", Ident("x"), VInt(2)))
      ))
    )
  }
  
  test("parse let in while body") {
    val src = """
      while (i < 10) {
        let tmp = i * 2 in
        result = result + tmp;
        i = i + 1
      }
    """
    val exp = parseExp(src)
    assert(exp match {
      case While(_, bodies) => 
        bodies.exists {
          case Let(_, _, _) => true
          case _ => false
        }
      case _ => false
    })
  }
}