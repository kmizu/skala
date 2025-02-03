package com.github.kmizu.tskala

import munit.FunSuite
import play.api.libs.json.Json

class JsonEvaluatorSuite extends FunSuite {

  test("evaluateJson: addition") {
    val json = """["+", 2, 3]"""
    val result = evalJsonExp(json)
    assertEquals(result, 5)
  }

  test("evaluateJson: nested arithmetic") {
    // Evaluates (1+2) * 3
    val json = """["*", ["+", 1, 2], 3]"""
    val result = evalJsonExp(json)
    assertEquals(result, 9)
  }

  test("evaluateJson: if expression true branch") {
    // If (2 < 3) then 100 else 200. Since 2 < 3 is true, the result should be 100.
    val json = """["if", ["<", 2, 3], 100, 200]"""
    val result = evalJsonExp(json)
    assertEquals(result, 100)
  }

  test("evaluateJson: sequence with assignment and use of identifier") {
    // Sequence: assign x = 10, then compute x + 5.
    val json =
      """["seq",
        |  ["assign", "x", 10],
        |  ["+", ["id", "x"], 5]
        |]""".stripMargin
    val result = evalJsonExp(json)
    assertEquals(result, 15)
  }

  test("evaluateJson: while loop") {
    // Sequence:
    //   1. assign x = 3
    //   2. while (x > 0) { assign x = x - 1 }
    //   3. return x (which should be 0 after the loop)
    val json =
      """["seq",
        |  ["assign", "x", 3],
        |  ["while", [">", ["id", "x"], 0], ["assign", "x", ["-", ["id", "x"], 1]]],
        |  ["id", "x"]
        |]""".stripMargin
    val result = evalJsonExp(json)
    assertEquals(result, 0)
  }

  test("evaluateJson: unknown operator should throw an exception") {
    val json = """["unknown", 1, 2]"""
    val ex = intercept[Exception] {
      evalJsonExp(json)
    }
    assert(ex.getMessage.contains("Unknown operator: unknown"))
  }

  test("evaluateJsonProgram: simple program without functions") {
    // A complete program is represented as a JSON array.
    // In this test, there are no function definitions.
    // The program has a single body: a sequence that assigns x and then computes x + 5.
    val json =
      """[
        |  ["seq", ["assign", "x", 10], ["+", ["id", "x"], 5]]
        |]""".stripMargin
    val result = evalJsonProgram(json)
    assertEquals(result, 15)
  }

  test("evaluateJsonProgram: invalid program (not an array)") {
    val json = """{"op": "+"}"""
    val ex = intercept[Exception] {
      evalJsonProgram(json)
    }
    assert(ex.getMessage.contains("Program must be represented as a JSON array"))
  }

  test("evaluateJson: addition") {
    val json = """["+", 2, 3]"""
    val result = evalJsonExp(json)
    assertEquals(result, 5)
  }

  test("evaluateJson: nested arithmetic") {
    // Evaluates (1+2) * 3
    val json = """["*", ["+", 1, 2], 3]"""
    val result = evalJsonExp(json)
    assertEquals(result, 9)
  }

  test("evaluateJson: if expression true branch") {
    // If (2 < 3) then 100 else 200. Since 2 < 3 is true, the result should be 100.
    val json = """["if", ["<", 2, 3], 100, 200]"""
    val result = evalJsonExp(json)
    assertEquals(result, 100)
  }

  test("evaluateJson: sequence with assignment and use of identifier") {
    // Sequence: assign x = 10, then compute x + 5.
    val json =
      """["seq",
        |  ["assign", "x", 10],
        |  ["+", ["id", "x"], 5]
        |]""".stripMargin
    val result = evalJsonExp(json)
    assertEquals(result, 15)
  }

  test("evaluateJson: while loop") {
    // Sequence:
    //   1. assign x = 3
    //   2. while (x > 0) { assign x = x - 1 }
    //   3. return x (which should be 0 after the loop)
    val json =
      """["seq",
        |  ["assign", "x", 3],
        |  ["while", [">", ["id", "x"], 0], ["assign", "x", ["-", ["id", "x"], 1]]],
        |  ["id", "x"]
        |]""".stripMargin
    val result = evalJsonExp(json)
    assertEquals(result, 0)
  }

  test("evaluateJson: unknown operator should throw an exception") {
    val json = """["unknown", 1, 2]"""
    val ex = intercept[Exception] {
      evalJsonExp(json)
    }
    assert(ex.getMessage.contains("Unknown operator: unknown"))
  }

  test("evaluateJsonProgram: simple program without functions") {
    // A complete program is represented as a JSON array.
    // In this test, there are no function definitions.
    // The program has a single body: a sequence that assigns x and then computes x + 5.
    val json =
      """[
        |  ["seq", ["assign", "x", 10], ["+", ["id", "x"], 5]]
        |]""".stripMargin
    val result = evalJsonProgram(json)
    assertEquals(result, 15)
  }

  test("evaluateJsonProgram: invalid program (not an array)") {
    val json = """{"op": "+"}"""
    val ex = intercept[Exception] {
      evalJsonProgram(json)
    }
    assert(ex.getMessage.contains("Program must be represented as a JSON array"))
  }

  // --- Additional tests for functions with types ---

  test("evaluateJsonProgram: valid typed function 'square' and call") {
    // Define a function 'square' that computes x*x.
    // Expected JSON format:
    // [
    //   ["def", "square", [["x", "TInt"]], "TInt", ["*", ["id", "x"], ["id", "x"]]],
    //   ["call", "square", 6]
    // ]
    val json =
      """[
        |  ["def", "square", [["x", "TInt"]], "TInt", ["*", ["id", "x"], ["id", "x"]]],
        |  ["call", "square", 6]
        |]""".stripMargin
    val result = evalJsonProgram(json)
    assertEquals(result, 36)
  }

  test("evaluateJsonProgram: valid typed function 'add' and call") {
    // Define a function 'add' that adds two integers.
    // Expected JSON format:
    // [
    //   ["def", "add", [["a", "TInt"], ["b", "TInt"]], "TInt", ["+", ["id", "a"], ["id", "b"]]],
    //   ["call", "add", 10, 20]
    // ]
    val json =
      """[
        |  ["def", "add", [["a", "TInt"], ["b", "TInt"]], "TInt", ["+", ["id", "a"], ["id", "b"]]],
        |  ["call", "add", 10, 20]
        |]""".stripMargin
    val result = evalJsonProgram(json)
    assertEquals(result, 30)
  }

  test("evaluateJsonProgram: function call with too few arguments") {
    // Define a function 'sub' that subtracts its second parameter from the first.
    // Then call it with only one argument so that parameter "b" is unbound.
    val json =
      """[
        |  ["def", "sub", [["a", "TInt"], ["b", "TInt"]], "TInt", ["-", ["id", "a"], ["id", "b"]]],
        |  ["call", "sub", 10]
        |]""".stripMargin
    val ex = intercept[Exception] {
      evalJsonProgram(json)
    }
    // The error message should indicate that an identifier (e.g. "b") was not defined.
    assert(ex.getMessage.contains("Undefined identifier"))
  }

  test("evaluateJsonProgram: invalid function definition format") {
    // Here the parameter list is not an array.
    val json =
      """[
        |  ["def", "badFunc", "not an array", "TInt", ["+", 1, 2]],
        |  ["call", "badFunc", 1]
        |]""".stripMargin
    val ex = intercept[Exception] {
      evalJsonProgram(json)
    }
    assert(ex.getMessage.contains("Function parameters must be an array"))
  }
}