package com.github.kmizu.tskala

import munit.FunSuite
import Exp.*
import Type.*

class ListTyperSuite extends FunSuite {

  test("empty list type inference") {
    val exp = tList()
    val typeEnv = scala.collection.mutable.Map[String, Type]()
    val funcEnv = scala.collection.mutable.Map[String, Func]()
    val resultType = Typer.typeOf(exp, typeEnv, funcEnv)
    assertEquals(resultType, TList(TInt)) // Default to List[Int]
  }
  
  test("list of integers") {
    val exp = tList(tInt(1), tInt(2), tInt(3))
    val typeEnv = scala.collection.mutable.Map[String, Type]()
    val funcEnv = scala.collection.mutable.Map[String, Func]()
    val resultType = Typer.typeOf(exp, typeEnv, funcEnv)
    assertEquals(resultType, TList(TInt))
  }
  
  test("list access type checking") {
    val exp = tSeq(
      tAssign("lst", tList(tInt(10), tInt(20))),
      tListAccess(tId("lst"), tInt(0))
    )
    val typeEnv = scala.collection.mutable.Map[String, Type]()
    val funcEnv = scala.collection.mutable.Map[String, Func]()
    val resultType = Typer.typeOf(exp, typeEnv, funcEnv)
    assertEquals(resultType, TInt)
  }
  
  test("list length returns Int") {
    val exp = tListLength(tList(tInt(1), tInt(2), tInt(3)))
    val typeEnv = scala.collection.mutable.Map[String, Type]()
    val funcEnv = scala.collection.mutable.Map[String, Func]()
    val resultType = Typer.typeOf(exp, typeEnv, funcEnv)
    assertEquals(resultType, TInt)
  }
  
  test("list append type checking") {
    val exp = tSeq(
      tAssign("lst", tList(tInt(1), tInt(2))),
      tListAppend(tId("lst"), tInt(3))
    )
    val typeEnv = scala.collection.mutable.Map[String, Type]()
    val funcEnv = scala.collection.mutable.Map[String, Func]()
    val resultType = Typer.typeOf(exp, typeEnv, funcEnv)
    assertEquals(resultType, TList(TInt))
  }
  
  test("nested lists") {
    val exp = tList(
      tList(tInt(1), tInt(2)),
      tList(tInt(3), tInt(4))
    )
    val typeEnv = scala.collection.mutable.Map[String, Type]()
    val funcEnv = scala.collection.mutable.Map[String, Func]()
    val resultType = Typer.typeOf(exp, typeEnv, funcEnv)
    assertEquals(resultType, TList(TList(TInt)))
  }
  
  test("type error: mixed list elements") {
    val exp = tList(tInt(1), tList(tInt(2)))
    val typeEnv = scala.collection.mutable.Map[String, Type]()
    val funcEnv = scala.collection.mutable.Map[String, Func]()
    intercept[Exception] {
      Typer.typeOf(exp, typeEnv, funcEnv)
    }
  }
  
  test("type error: non-integer list index") {
    val exp = tListAccess(tList(tInt(1), tInt(2)), tList(tInt(0)))
    val typeEnv = scala.collection.mutable.Map[String, Type]()
    val funcEnv = scala.collection.mutable.Map[String, Func]()
    intercept[Exception] {
      Typer.typeOf(exp, typeEnv, funcEnv)
    }
  }
  
  test("type error: accessing non-list") {
    val exp = tListAccess(tInt(5), tInt(0))
    val typeEnv = scala.collection.mutable.Map[String, Type]()
    val funcEnv = scala.collection.mutable.Map[String, Func]()
    intercept[Exception] {
      Typer.typeOf(exp, typeEnv, funcEnv)
    }
  }
  
  test("type error: wrong element type in append") {
    val exp = tSeq(
      tAssign("lst", tList(tInt(1), tInt(2))),
      tListAppend(tId("lst"), tList(tInt(3)))
    )
    val typeEnv = scala.collection.mutable.Map[String, Type]()
    val funcEnv = scala.collection.mutable.Map[String, Func]()
    intercept[Exception] {
      Typer.typeOf(exp, typeEnv, funcEnv)
    }
  }
  
  test("list in if condition") {
    val exp = tIf(
      tList(tInt(1)),
      tInt(10),
      tInt(20)
    )
    val typeEnv = scala.collection.mutable.Map[String, Type]()
    val funcEnv = scala.collection.mutable.Map[String, Func]()
    val resultType = Typer.typeOf(exp, typeEnv, funcEnv) // Valid: non-empty list is truthy
    assertEquals(resultType, TInt)
  }
  
  test("program with list operations") {
    val prog = tProgram(
      List(
        tFunction("sumList", List(("lst", TList(TInt))), TInt,
          tSeq(
            tAssign("sum", tInt(0)),
            tAssign("i", tInt(0)),
            tWhile(
              tId("i") |<| tListLength(tId("lst")),
              tAssign("sum", tId("sum") |+| tListAccess(tId("lst"), tId("i"))),
              tAssign("i", tId("i") |+| tInt(1))
            ),
            tId("sum")
          )
        )
      ),
      tCall("sumList", tList(tInt(1), tInt(2), tInt(3)))
    )
    val resultType = Typer.typeCheckProgram(prog)
    assertEquals(resultType, TInt)
  }
}