package com.github.kmizu.tskala

import munit.FunSuite
import Parser.*
import Exp.*
import Type.*

class ParserSuite extends FunSuite {

  test("parse simple arithmetic") {
    val ast = parseExp("1 + 2 * 3")
    val expected = tInt(1) |+| (tInt(2) |*| tInt(3))
    assertEquals(ast, expected)
  }

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
  
  test("parse typed function with list parameter") {
    val src = "function sumList(lst: List[Int]): Int { lst }"
    val program = parseProgram(src)
    assertEquals(
      Program(
        List(Func("sumList", List(("lst", TList(TInt))), TInt, Ident("lst"))),
        List()
      ),
      program
    )
  }
  
  test("parse typed function returning list") {
    val src = "function makeList(n: Int): List[Int] { [n, n + 1] }"
    val program = parseProgram(src)
    assertEquals(
      Program(
        List(Func("makeList", List(("n", TInt)), TList(TInt), 
          VList(List(Ident("n"), Ident("n") |+| VInt(1))))),
        List()
      ),
      program
    )
  }
  
  test("parse program with list operations") {
    val src = """
      function processLists(a: List[Int], b: List[Int]): Int {
        length(a) + length(b)
      }
      processLists([1, 2], [3, 4, 5])
    """
    val program = parseProgram(src)
    assertEquals(
      Program(
        List(
          Func("processLists", 
            List(("a", TList(TInt)), ("b", TList(TInt))), 
            TInt,
            ListLength(Ident("a")) |+| ListLength(Ident("b"))
          )
        ),
        List(
          Call("processLists", List(
            VList(List(VInt(1), VInt(2))),
            VList(List(VInt(3), VInt(4), VInt(5)))
          ))
        )
      ),
      program
    )
  }
  
  test("parse nested list types") {
    val src = "function identity(matrix: List[List[Int]]): List[List[Int]] { matrix }"
    val program = parseProgram(src)
    assertEquals(
      Program(
        List(
          Func("identity", 
            List(("matrix", TList(TList(TInt)))), 
            TList(TList(TInt)),
            Ident("matrix")
          )
        ),
        List()
      ),
      program
    )
  }
  
  test("parse list in expression") {
    val exp = parseExp("[1, 2, 3][1] + 5")
    assertEquals(
      BinExp("+", ListAccess(VList(List(VInt(1), VInt(2), VInt(3))), VInt(1)), VInt(5)),
      exp
    )
  }
  
  test("parse if-else with lists") {
    val exp = parseExp("if (length(lst) > 0) { lst[0] } else { 0 }")
    assertEquals(
      If(
        ListLength(Ident("lst")) |>| VInt(0),
        ListAccess(Ident("lst"), VInt(0)),
        VInt(0)
      ),
      exp
    )
  }
  
  test("parse while with list operations") {
    val src = """
      i = 0;
      while (i < length(lst)) {
        sum = sum + lst[i];
        i = i + 1
      }
    """
    val program = parseProgram(src)
    assertEquals(
      Program(
        List(),
        List(
          Assignment("i", VInt(0)),
          While(
            Ident("i") |<| ListLength(Ident("lst")),
            List(
              Assignment("sum", Ident("sum") |+| ListAccess(Ident("lst"), Ident("i"))),
              Assignment("i", Ident("i") |+| VInt(1))
            )
          )
        )
      ),
      program
    )
  }
}