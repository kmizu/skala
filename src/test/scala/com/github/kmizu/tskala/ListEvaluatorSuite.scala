package com.github.kmizu.tskala

import munit.FunSuite
import Exp.*
import Type.*
import Value.*

class ListEvaluatorSuite extends FunSuite {
  
  test("empty list") {
    val exp = tList()
    val result = evalExp(exp)
    assertEquals(ListValue(List()), result)
  }
  
  test("list with integers") {
    val exp = tList(tInt(1), tInt(2), tInt(3))
    val result = evalExp(exp)
    assertEquals(ListValue(List(IntValue(1), IntValue(2), IntValue(3))), result)
  }
  
  test("list access") {
    val exp = tSeq(
      tAssign("lst", tList(tInt(10), tInt(20), tInt(30))),
      tListAccess(tId("lst"), tInt(1))
    )
    val result = evalExp(exp)
    assertEquals(IntValue(20), result)
  }
  
  test("list length") {
    val exp = tListLength(tList(tInt(1), tInt(2), tInt(3), tInt(4)))
    val result = evalExp(exp)
    assertEquals(IntValue(4), result)
  }
  
  test("list append") {
    val exp = tSeq(
      tAssign("lst", tList(tInt(1), tInt(2))),
      tAssign("lst2", tListAppend(tId("lst"), tInt(3))),
      tListLength(tId("lst2"))
    )
    val result = evalExp(exp)
    assertEquals(IntValue(3), result)
  }
  
  test("nested lists") {
    val exp = tList(
      tList(tInt(1), tInt(2)),
      tList(tInt(3), tInt(4))
    )
    val result = evalExp(exp)
    assertEquals(
      ListValue(List(
        ListValue(List(IntValue(1), IntValue(2))),
        ListValue(List(IntValue(3), IntValue(4)))
      )), 
      result
    )
  }
  
  test("list operations with functions") {
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
      tCall("sumList", tList(tInt(1), tInt(2), tInt(3), tInt(4)))
    )
    val result = evalProgram(prog)
    assertEquals(IntValue(10), result)
  }
  
  test("list equality") {
    val exp = tSeq(
      tAssign("lst1", tList(tInt(1), tInt(2))),
      tAssign("lst2", tList(tInt(1), tInt(2))),
      tId("lst1") |==| tId("lst2")
    )
    val result = evalExp(exp)
    assertEquals(IntValue(1), result) // true
  }
  
  test("list index out of bounds") {
    val exp = tListAccess(tList(tInt(1), tInt(2)), tInt(5))
    intercept[RuntimeException] {
      evalExp(exp)
    }
  }
  
  test("negative list index") {
    val exp = tListAccess(tList(tInt(1), tInt(2)), tInt(-1))
    intercept[RuntimeException] {
      evalExp(exp)
    }
  }
}