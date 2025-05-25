package com.github.kmizu.tskala

import munit.FunSuite
import Parser.*
import Value.*

class ListParserIntegrationSuite extends FunSuite {
  
  test("parse and evaluate list literal") {
    val src = "[1, 2, 3]"
    val exp = parseExp(src)
    val result = evalExp(exp)
    assertEquals(result, ListValue(List(IntValue(1), IntValue(2), IntValue(3))))
  }
  
  test("parse and evaluate list operations") {
    val src = """
      lst = [10, 20, 30];
      sum = lst[0] + lst[1] + lst[2];
      sum
    """
    val prog = parseProgram(src)
    val result = evalProgram(prog)
    assertEquals(result, IntValue(60))
  }
  
  test("parse and evaluate typed function with lists") {
    val src = """
      function sumList(lst: List[Int]): Int {
        sum = 0;
        i = 0;
        while (i < length(lst)) {
          sum = sum + lst[i];
          i = i + 1
        };
        sum
      }
      sumList([1, 2, 3, 4, 5])
    """
    val prog = parseProgram(src)
    val result = evalProgram(prog)
    assertEquals(result, IntValue(15))
  }
  
  test("parse and evaluate nested lists") {
    val src = """
      function sumMatrix(matrix: List[List[Int]]): Int {
        sum = 0;
        i = 0;
        while (i < length(matrix)) {
          j = 0;
          while (j < length(matrix[i])) {
            sum = sum + matrix[i][j];
            j = j + 1
          };
          i = i + 1
        };
        sum
      }
      sumMatrix([[1, 2], [3, 4], [5, 6]])
    """
    val prog = parseProgram(src)
    val result = evalProgram(prog)
    assertEquals(result, IntValue(21))
  }
  
  test("parse and type check list append") {
    val src = """
      function addToList(lst: List[Int], n: Int): List[Int] {
        append(lst, n)
      }
      result = addToList([1, 2], 3);
      length(result)
    """
    val prog = parseProgram(src)
    val result = evalProgram(prog)
    assertEquals(result, IntValue(3))
  }
  
  test("type error: mixing types in list") {
    val src = """
      function bad(): List[Int] {
        [1, [2, 3]]
      }
      bad()
    """
    val prog = parseProgram(src)
    intercept[Exception] {
      evalProgram(prog) // Should fail during type checking
    }
  }
  
  test("parse list comparison") {
    val src = """
      lst1 = [1, 2, 3];
      lst2 = [1, 2, 3];
      lst3 = [1, 2, 4];
      if (lst1 == lst2) {
        if (lst1 == lst3) { 2 } else { 1 }
      } else {
        0
      }
    """
    val prog = parseProgram(src)
    val result = evalProgram(prog)
    assertEquals(result, IntValue(1)) // lst1 == lst2 is true, lst1 == lst3 is false
  }
}