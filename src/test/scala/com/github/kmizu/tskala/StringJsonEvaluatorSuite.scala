package com.github.kmizu.tskala

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

  test("string concatenation with +") {
    val json = """["+", "hello", " world"]"""
    val result = evalJsonExp(json)
    assert(result == Value.StringValue("hello world"))
  }

  test("typed string function") {
    val json = """[
      ["def", "greet", [["name", "TString"]], "TString", 
        ["++", ["++", ["string", "Hello, "], ["id", "name"]], ["string", "!"]]
      ],
      ["call", "greet", "World"]
    ]"""
    val result = evalJsonProgram(json)
    assert(result == Value.StringValue("Hello, World!"))
  }

  test("string operations in typed program") {
    val json = """[
      ["def", "makeMessage", [["title", "TString"], ["msg", "TString"]], "TString",
        ["++", ["id", "title"], ["++", ["string", ": "], ["id", "msg"]]]
      ],
      ["call", "makeMessage", "Items", "42 items found"]
    ]"""
    val result = evalJsonProgram(json)
    assert(result == Value.StringValue("Items: 42 items found"))
  }

  test("string equality in typed context") {
    val json = """[
      ["def", "areEqual", [["s1", "TString"], ["s2", "TString"]], "TInt",
        ["if", ["==", ["id", "s1"], ["id", "s2"]], 1, 0]
      ],
      ["call", "areEqual", "hello", "hello"]
    ]"""
    val result = evalJsonProgram(json)
    assert(result == Value.IntValue(1))
  }

  test("string list operations") {
    val json = """[
      ["assign", "strings", ["list", "hello", "world", "test"]],
      ["string-length", ["list-access", ["id", "strings"], 1]]
    ]"""
    val result = evalJsonProgram(json)
    assert(result == Value.IntValue(5))
  }
}