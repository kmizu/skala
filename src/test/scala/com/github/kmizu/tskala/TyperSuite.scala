package com.github.kmizu.tskala

import munit.FunSuite
import Exp.*
import Type.*
import Typer.*

class TyperSuite extends FunSuite {

  test("typeCheckProgram: valid arithmetic program") {
    // Program: a sequence with two expressions: first 1, then 2+3.
    // The type of the final expression is TInt.
    val prog = tProgram(
      functions = List(),
      bodies = tSeq(
        tInt(1),
        tInt(2) |+| tInt(3)
      )
    )
    assertEquals(typeCheckProgram(prog), TInt)
  }

  test("typeCheckProgram: valid program with a function call") {
    // Define a function: add(a: Int, b: Int): Int = a + b
    val addFunc = tFunction(
      name = "add",
      params = List(("a", TInt), ("b", TInt)),
      returnType = TInt,
      body = tId("a") |+| tId("b")
    )

    // Program: call add(10, 20)
    val prog = tProgram(
      functions = List(addFunc),
      bodies = tCall("add", tInt(10), tInt(20))
    )
    assertEquals(typeCheckProgram(prog), TInt)
  }

  test("typeCheckProgram: invalid program with wrong number of arguments in function call") {
    // Define a function: negate(x: Int): Int = 0 - x
    val negateFunc = tFunction(
      name = "negate",
      params = List(("x", TInt)),
      returnType = TInt,
      body = tInt(0) |-| tId("x")
    )
    // Program: incorrectly call negate with two arguments.
    val prog = tProgram(
      functions = List(negateFunc),
      bodies = tCall("negate", tInt(10), tInt(20))
    )
    val error = intercept[Exception] {
      typeCheckProgram(prog)
    }
    assert(error.getMessage.contains("Type error in function call 'negate': expected 1 arguments, got 2"))
  }

  test("typeCheckProgram: invalid program with non-boolean if condition") {
    // Program: an if expression whose condition is an integer literal (should be Boolean).
    val prog = tProgram(
      functions = List(),
      bodies = tIf(
        condition = tInt(1),  // Invalid: condition should be TBool.
        thenClause  = tInt(2),
        elseClause  = tInt(3)
      )
    )
    val error = intercept[Exception] {
      typeCheckProgram(prog)
    }
    // Note: The error message will include the toString of the type, which is "TInt" for an integer.
    assert(error.getMessage.contains("Type error in if condition: expected Bool, got TInt"))
  }

  test("typeCheckProgram: invalid program with mismatched if branches") {
    // Program: an if expression whose then branch is TInt and else branch is TBool.
    val prog = tProgram(
      functions = List(),
      bodies = tIf(
        condition  = tInt(1) |==| tInt(2),  // Comparison returns TBool.
        thenClause = tInt(1),                // TInt
        elseClause = tInt(1) |==| tInt(1)     // TBool
      )
    )
    val error = intercept[Exception] {
      typeCheckProgram(prog)
    }
    assert(error.getMessage.contains("Type error in if branches: then branch has type TInt but else branch has type TBool"))
  }
}