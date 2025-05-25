package com.github.kmizu.skala

import org.junit.Test
import org.junit.Assert.*
import Parser.*
import Exp.*

class ListParserSuite extends munit.FunSuite {
  
  test("parse empty list") {
    val exp = parseExp("[]")
    assertEquals(VList(List()), exp)
  }
  
  test("parse list with elements") {
    val exp = parseExp("[1, 2, 3]")
    assertEquals(VList(List(VInt(1), VInt(2), VInt(3))), exp)
  }
  
  test("parse list access") {
    val exp = parseExp("myList[0]")
    assertEquals(ListAccess(Ident("myList"), VInt(0)), exp)
  }
  
  test("parse nested list access") {
    val exp = parseExp("matrix[i][j]")
    assertEquals(
      ListAccess(ListAccess(Ident("matrix"), Ident("i")), Ident("j")), 
      exp
    )
  }
  
  test("parse list length") {
    val exp = parseExp("length(myList)")
    assertEquals(ListLength(Ident("myList")), exp)
  }
  
  test("parse list append") {
    val exp = parseExp("append(myList, 42)")
    assertEquals(ListAppend(Ident("myList"), VInt(42)), exp)
  }
  
  test("parse nested lists") {
    val exp = parseExp("[[1, 2], [3, 4]]")
    assertEquals(
      VList(List(
        VList(List(VInt(1), VInt(2))),
        VList(List(VInt(3), VInt(4)))
      )),
      exp
    )
  }
  
  test("parse list in expression") {
    val exp = parseExp("[1, 2, 3][1] + 5")
    assertEquals(
      BinExp("+", ListAccess(VList(List(VInt(1), VInt(2), VInt(3))), VInt(1)), VInt(5)),
      exp
    )
  }
  
  test("parse program with lists") {
    val prog = parseProgram("""
      nums = [10, 20, 30];
      total = nums[0] + nums[1] + nums[2]
    """)
    assertEquals(
      Program(
        List(),
        List(
          Assignment("nums", VList(List(VInt(10), VInt(20), VInt(30)))),
          Assignment("total", 
            BinExp("+", 
              BinExp("+", 
                ListAccess(Ident("nums"), VInt(0)),
                ListAccess(Ident("nums"), VInt(1))
              ),
              ListAccess(Ident("nums"), VInt(2))
            )
          )
        )
      ),
      prog
    )
  }
}