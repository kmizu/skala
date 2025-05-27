package com.github.kmizu.tskala
import munit.FunSuite

class DictJsonEvaluatorSuite extends FunSuite {
  import Value.*

  test("evaluate dictionary literal via JSON") {
    val json = """["dict", ["string", "name"], ["string", "John"], ["string", "city"], ["string", "NYC"]]"""
    val result = evalJsonExp(json)
    assertEquals(
      result,
      DictValue(Map(
        StringValue("name") -> StringValue("John"),
        StringValue("city") -> StringValue("NYC")
      ))
    )
  }

  test("evaluate empty dictionary via JSON") {
    val json = """["dict"]"""
    assertEquals(
      evalJsonExp(json),
      DictValue(Map.empty)
    )
  }

  test("evaluate dictionary access via JSON") {
    val json = """["dict-access", ["dict", ["string", "x"], ["+", 5, 5]], ["string", "x"]]"""
    assertEquals(
      evalJsonExp(json),
      IntValue(10)
    )
  }

  test("evaluate dictionary set via JSON") {
    val json = """["dict-set", ["dict", ["string", "x"], 10], ["string", "y"], 20]"""
    assertEquals(
      evalJsonExp(json),
      DictValue(Map(
        StringValue("x") -> IntValue(10),
        StringValue("y") -> IntValue(20)
      ))
    )
  }

  test("evaluate dictionary keys via JSON") {
    val json = """["dict-keys", ["dict", ["string", "a"], 1, ["string", "b"], 2]]"""
    val result = evalJsonExp(json).asList
    assert(result.toSet == Set(StringValue("a"), StringValue("b")))
  }

  test("evaluate dictionary values via JSON") {
    val json = """["dict-values", ["dict", ["string", "x"], 10, ["string", "y"], 20]]"""
    val result = evalJsonExp(json).asList
    assert(result.toSet == Set(IntValue(10), IntValue(20)))
  }

  test("evaluate dictionary size via JSON") {
    val json = """["dict-size", ["dict", ["string", "a"], 1, ["string", "b"], 2, ["string", "c"], 3]]"""
    assertEquals(
      evalJsonExp(json),
      IntValue(3)
    )
  }

  test("evaluate dictionary contains via JSON") {
    val json1 = """["dict-contains", ["dict", ["string", "x"], 10], ["string", "x"]]"""
    assertEquals(evalJsonExp(json1), IntValue(1))
    
    val json2 = """["dict-contains", ["dict", ["string", "x"], 10], ["string", "y"]]"""
    assertEquals(evalJsonExp(json2), IntValue(0))
  }

  test("evaluate dictionary with integer keys via JSON") {
    val json = """["dict-access", ["dict", 1, ["string", "one"], 2, ["string", "two"]], 1]"""
    assertEquals(
      evalJsonExp(json),
      StringValue("one")
    )
  }

  test("evaluate dictionary in program via JSON") {
    val json = """[
      ["def", "getValue", [["dict", "TDict[TString,TInt]"], ["key", "TString"]], "TInt",
        ["dict-access", ["id", "dict"], ["id", "key"]]
      ],
      ["assign", "myDict", ["dict", ["string", "x"], 10, ["string", "y"], 20]],
      ["call", "getValue", ["id", "myDict"], ["string", "y"]]
    ]"""
    assertEquals(evalJsonProgram(json), IntValue(20))
  }

  test("evaluate nested dictionaries via JSON") {
    val json = """["dict-access", 
      ["dict-access", 
        ["dict", 
          ["string", "outer"], 
          ["dict", ["string", "inner"], 42]
        ], 
        ["string", "outer"]
      ],
      ["string", "inner"]
    ]"""
    assertEquals(evalJsonExp(json), IntValue(42))
  }

  test("evaluate dictionary with list values via JSON") {
    val json = """["dict-access", 
      ["dict", 
        ["string", "numbers"], 
        ["list", 1, 2, 3]
      ], 
      ["string", "numbers"]
    ]"""
    val result = evalJsonExp(json).asList
    assertEquals(result, List(IntValue(1), IntValue(2), IntValue(3)))
  }

  test("evaluate dictionary operations in sequence via JSON") {
    val json = """["seq",
      ["assign", "d", ["dict", ["string", "a"], 1]],
      ["assign", "d", ["dict-set", ["id", "d"], ["string", "b"], 2]],
      ["assign", "d", ["dict-set", ["id", "d"], ["string", "c"], 3]],
      ["dict-size", ["id", "d"]]
    ]"""
    assertEquals(evalJsonExp(json), IntValue(3))
  }
}