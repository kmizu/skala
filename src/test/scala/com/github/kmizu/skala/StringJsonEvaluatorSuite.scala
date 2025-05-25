package com.github.kmizu.skala

import munit.FunSuite
import play.api.libs.json._

class StringJsonEvaluatorSuite extends FunSuite {
  test("string literal") {
    val result = evalJsonExp("\"hello world\"")
    assert(result == Value.StringValue("hello world"))
  }

  test("string concatenation with ++") {
    val json = """["++", "hello", " world"]"""
    val result = evalJsonExp(json)
    assert(result == Value.StringValue("hello world"))
  }

  test("string concatenation with string-concat") {
    val json = """["string-concat", "hello", " world"]"""
    val result = evalJsonExp(json)
    assert(result == Value.StringValue("hello world"))
  }

  test("string length") {
    val json = """["string-length", "hello"]"""
    val result = evalJsonExp(json)
    assert(result == Value.IntValue(5))
  }

  test("nested string operations") {
    val json = """["string-length", ["++", "hello", " world"]]"""
    val result = evalJsonExp(json)
    assert(result == Value.IntValue(11))
  }

  test("string operations in program") {
    val json = """[
      ["assign", "greeting", "Hello"],
      ["assign", "name", "World"],
      ["++", ["id", "greeting"], ["++", ", ", ["id", "name"]]]
    ]"""
    val result = evalJsonProgram(json)
    assert(result == Value.StringValue("Hello, World"))
  }

  test("string in function") {
    val json = """[
      ["def", "greet", ["name"], ["++", "Hello, ", ["++", ["id", "name"], "!"]]],
      ["call", "greet", "Alice"]
    ]"""
    val result = evalJsonProgram(json)
    assert(result == Value.StringValue("Hello, Alice!"))
  }

  test("string comparison") {
    val json = """[
      ["assign", "s1", "hello"],
      ["assign", "s2", "hello"],
      ["assign", "s3", "world"],
      ["if", ["==", ["id", "s1"], ["id", "s2"]],
        ["if", ["!=", ["id", "s1"], ["id", "s3"]], 1, 0],
        0
      ]
    ]"""
    val result = evalJsonProgram(json)
    assert(result == Value.IntValue(1))
  }

  test("mixed string concatenation with +") {
    val json = """["+", "Count: ", 42]"""
    val result = evalJsonExp(json)
    assert(result == Value.StringValue("Count: 42"))
  }
}