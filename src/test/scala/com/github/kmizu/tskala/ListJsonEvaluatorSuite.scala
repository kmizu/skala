package com.github.kmizu.tskala

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
  
  test("list operations in typed program") {
    val program = """[
      ["def", "sumList", [["lst", "TList[TInt]"]], "TInt",
        ["seq",
          ["assign", "sum", 0],
          ["assign", "i", 0],
          ["while", ["<", ["id", "i"], ["list-length", ["id", "lst"]]],
            ["seq",
              ["assign", "sum", ["+", ["id", "sum"], ["list-access", ["id", "lst"], ["id", "i"]]]],
              ["assign", "i", ["+", ["id", "i"], 1]]
            ]
          ],
          ["id", "sum"]
        ]
      ],
      ["call", "sumList", ["list", 1, 2, 3, 4]]
    ]"""
    assertEquals(evalJsonProgramInt(program), 10)
  }
  
  test("list equality") {
    val e = """["seq",
                 ["assign", "lst1", ["list", 1, 2]],
                 ["assign", "lst2", ["list", 1, 2]],
                 ["==", ["id", "lst1"], ["id", "lst2"]]
               ]"""
    assertEquals(evalJsonExpInt(e), 1) // true
  }
  
  test("function returning list") {
    val program = """[
      ["def", "makeList", [["n", "TInt"]], "TList[TInt]",
        ["list", ["id", "n"], ["+", ["id", "n"], 1], ["+", ["id", "n"], 2]]
      ],
      ["list-length", ["call", "makeList", 10]]
    ]"""
    assertEquals(evalJsonProgramInt(program), 3)
  }
}