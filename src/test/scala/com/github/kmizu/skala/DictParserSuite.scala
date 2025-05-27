package com.github.kmizu.skala
import munit.FunSuite

class DictParserSuite extends FunSuite {
  import Exp.*
  import Parser.*

  test("parse empty dictionary") {
    assertEquals(
      parseExp("{}"),
      VDict(List.empty)
    )
  }

  test("parse dictionary with string keys") {
    assertEquals(
      parseExp("{\"name\": \"John\", \"age\": 30}"),
      VDict(List(
        (VString("name"), VString("John")),
        (VString("age"), VInt(30))
      ))
    )
  }

  test("parse dictionary with integer keys") {
    assertEquals(
      parseExp("{1: \"one\", 2: \"two\"}"),
      VDict(List(
        (VInt(1), VString("one")),
        (VInt(2), VString("two"))
      ))
    )
  }

  test("parse dictionary with expression values") {
    assertEquals(
      parseExp("{\"sum\": 10 + 20, \"product\": 3 * 4}"),
      VDict(List(
        (VString("sum"), BinExp("+", VInt(10), VInt(20))),
        (VString("product"), BinExp("*", VInt(3), VInt(4)))
      ))
    )
  }

  test("parse dictionary access") {
    // Dictionary access is done via DictAccess built-in function
    // This test creates a dict variable and shows it can be parsed
    val code = """d = {"x": 10, "y": 20}"""
    val result = parseExp(code)
    assert(result.isInstanceOf[Assignment])
    result match {
      case Assignment(name, VDict(entries)) =>
        assertEquals(name, "d")
        assertEquals(entries.length, 2)
      case _ => fail("Expected assignment with dict literal")
    }
  }

  test("parse dictionary operations") {
    // Test parsing keys function
    val keysCode = "keys({\"a\": 1, \"b\": 2})"
    val keysResult = parseExp(keysCode)
    assertEquals(
      keysResult,
      DictKeys(VDict(List(
        (VString("a"), VInt(1)),
        (VString("b"), VInt(2))
      )))
    )

    // Test parsing values function
    val valuesCode = "values({\"x\": 10})"
    val valuesResult = parseExp(valuesCode)
    assertEquals(
      valuesResult,
      DictValues(VDict(List(
        (VString("x"), VInt(10))
      )))
    )

    // Test parsing size function
    val sizeCode = "size({})"
    val sizeResult = parseExp(sizeCode)
    assertEquals(
      sizeResult,
      DictSize(VDict(List.empty))
    )

    // Test parsing contains function
    val containsCode = "contains({\"x\": 1}, \"x\")"
    val containsResult = parseExp(containsCode)
    assertEquals(
      containsResult,
      DictContains(
        VDict(List((VString("x"), VInt(1)))),
        VString("x")
      )
    )
  }

  test("parse nested dictionaries") {
    assertEquals(
      parseExp("{\"outer\": {\"inner\": 42}}"),
      VDict(List(
        (VString("outer"), VDict(List(
          (VString("inner"), VInt(42))
        )))
      ))
    )
  }

  test("parse dictionary with list values") {
    assertEquals(
      parseExp("{\"numbers\": [1, 2, 3]}"),
      VDict(List(
        (VString("numbers"), VList(List(VInt(1), VInt(2), VInt(3))))
      ))
    )
  }

  test("parse dictionary in program") {
    val code = """
      function getValue(dict, key) {
        dict
      }
      
      d = {"x": 10, "y": 20};
      getValue(d, "x")
    """
    val result = parseProgram(code)
    assertEquals(result.functions.length, 1)
    assertEquals(result.functions.head.name, "getValue")
  }

  test("distinguish between empty dict and empty block") {
    // Empty braces should be parsed as dictionary
    assertEquals(
      parseExp("{}"),
      VDict(List.empty)
    )
    
    // Block with semicolon
    val blockCode = "{x = 10;}"
    val blockResult = parseExp(blockCode)
    assert(blockResult.isInstanceOf[SeqExp])
  }

  test("parse dictionary with complex keys") {
    val code = "{\"key\" ++ \"1\": 100}"
    val result = parseExp(code)
    assertEquals(
      result,
      VDict(List(
        (StringConcat(VString("key"), VString("1")), VInt(100))
      ))
    )
  }
}