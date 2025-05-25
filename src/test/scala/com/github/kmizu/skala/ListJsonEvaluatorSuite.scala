package com.github.kmizu.skala

import munit.FunSuite

class ListJsonEvaluatorSuite extends FunSuite {

  test("empty list") {
    val e = """["list"]"""
    assertEquals(evalJsonExp(e), Value.ListValue(List()))
  }

  test("list with integers") {
    val e = """["list", 1, 2, 3]"""
    assertEquals(evalJsonExp(e), Value.ListValue(List(Value.IntValue(1), Value.IntValue(2), Value.IntValue(3))))
  }
  
  test("list access") {
    val e = """["seq", 
                 ["assign", "lst", ["list", 10, 20, 30]], 
                 ["list-access", ["id", "lst"], 1]
               ]"""
    assertEquals(evalJsonExpInt(e), 20)
  }
  
  test("list length") {
    val e = """["list-length", ["list", 1, 2, 3, 4]]"""
    assertEquals(evalJsonExpInt(e), 4)
  }
  
  test("list append") {
    val e = """["seq",
                 ["assign", "lst", ["list", 1, 2]],
                 ["assign", "lst2", ["list-append", ["id", "lst"], 3]],
                 ["list-length", ["id", "lst2"]]
               ]"""
    assertEquals(evalJsonExpInt(e), 3)
  }
  
  test("nested lists") {
    val e = """["list", ["list", 1, 2], ["list", 3, 4]]"""
    assertEquals(
      evalJsonExp(e), 
      Value.ListValue(List(
        Value.ListValue(List(Value.IntValue(1), Value.IntValue(2))),
        Value.ListValue(List(Value.IntValue(3), Value.IntValue(4)))
      ))
    )
  }
  
  test("list in if condition - truthy") {
    val e = """["if", ["list", 1], 10, 20]"""
    assertEquals(evalJsonExpInt(e), 10)
  }
  
  test("list in if condition - falsy") {
    val e = """["if", ["list"], 10, 20]"""
    assertEquals(evalJsonExpInt(e), 20)
  }
  
  test("list operations in program") {
    val program = """[
      ["seq",
        ["assign", "nums", ["list", 1, 2, 3]],
        ["assign", "sum", 0],
        ["assign", "i", 0],
        ["while", ["<", ["id", "i"], ["list-length", ["id", "nums"]]],
          ["seq",
            ["assign", "sum", ["+", ["id", "sum"], ["list-access", ["id", "nums"], ["id", "i"]]]],
            ["assign", "i", ["+", ["id", "i"], 1]]
          ]
        ],
        ["id", "sum"]
      ]
    ]"""
    assertEquals(evalJsonProgramInt(program), 6)
  }
}