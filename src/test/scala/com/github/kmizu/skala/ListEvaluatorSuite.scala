package com.github.kmizu.skala

import org.junit.Test
import org.junit.Assert.*
import Exp.*

class ListEvaluatorSuite extends munit.FunSuite {
  
  test("empty list") {
    val exp = tList()
    val result = evalExp(exp)
    assertEquals(Value.ListValue(List()), result)
  }
  
  test("list with integers") {
    val exp = tList(tInt(1), tInt(2), tInt(3))
    val result = evalExp(exp)
    assertEquals(Value.ListValue(List(Value.IntValue(1), Value.IntValue(2), Value.IntValue(3))), result)
  }
  
  test("list access") {
    val exp = tSeq(
      tAssign("lst", tList(tInt(10), tInt(20), tInt(30))),
      tListAccess(tId("lst"), tInt(1))
    )
    val result = evalExp(exp)
    assertEquals(Value.IntValue(20), result)
  }
  
  test("list length") {
    val exp = tListLength(tList(tInt(1), tInt(2), tInt(3), tInt(4)))
    val result = evalExp(exp)
    assertEquals(Value.IntValue(4), result)
  }
  
  test("list append") {
    val exp = tSeq(
      tAssign("lst", tList(tInt(1), tInt(2))),
      tAssign("lst2", tListAppend(tId("lst"), tInt(3))),
      tListLength(tId("lst2"))
    )
    val result = evalExp(exp)
    assertEquals(Value.IntValue(3), result)
  }
  
  test("nested lists") {
    val exp = tList(
      tList(tInt(1), tInt(2)),
      tList(tInt(3), tInt(4))
    )
    val result = evalExp(exp)
    assertEquals(
      Value.ListValue(List(
        Value.ListValue(List(Value.IntValue(1), Value.IntValue(2))),
        Value.ListValue(List(Value.IntValue(3), Value.IntValue(4)))
      )), 
      result
    )
  }
  
  test("list in if condition") {
    val exp = tIf(
      tList(tInt(1)), // non-empty list is truthy
      tInt(10),
      tInt(20)
    )
    val result = evalExp(exp)
    assertEquals(Value.IntValue(10), result)
  }
  
  test("empty list in if condition") {
    val exp = tIf(
      tList(), // empty list is falsy
      tInt(10),
      tInt(20)
    )
    val result = evalExp(exp)
    assertEquals(Value.IntValue(20), result)
  }
  
  test("list operations with variables") {
    val prog = tProgram(
      List(),
      tAssign("nums", tList(tInt(1), tInt(2), tInt(3))),
      tAssign("sum", tInt(0)),
      tAssign("i", tInt(0)),
      tWhile(
        tId("i") |<| tListLength(tId("nums")),
        tAssign("sum", tId("sum") |+| tListAccess(tId("nums"), tId("i"))),
        tAssign("i", tId("i") |+| tInt(1))
      ),
      tId("sum")
    )
    val result = evalProgram(prog)
    assertEquals(Value.IntValue(6), result)
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