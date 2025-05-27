package com.github.kmizu.tskala
import munit.FunSuite

class DictEvaluatorSuite extends FunSuite {
  import Exp.*
  import Value.*

  test("evaluate dictionary literal") {
    assertEquals(
      evalExp(tDict(
        (tString("name"), tString("John")),
        (tString("city"), tString("NYC"))
      )),
      DictValue(Map(
        StringValue("name") -> StringValue("John"),
        StringValue("city") -> StringValue("NYC")
      ))
    )
  }

  test("evaluate empty dictionary") {
    assertEquals(
      evalExp(tDict()),
      DictValue(Map.empty)
    )
  }

  test("evaluate dictionary access") {
    val dict = tDict(
      (tString("x"), tInt(10)),
      (tString("y"), tInt(20))
    )
    assertEquals(
      evalExp(tDictAccess(dict, tString("x"))),
      IntValue(10)
    )
    assertEquals(
      evalExp(tDictAccess(dict, tString("y"))),
      IntValue(20)
    )
  }

  test("evaluate dictionary access with non-existent key") {
    val dict = tDict((tString("x"), tInt(10)))
    intercept[RuntimeException] {
      evalExp(tDictAccess(dict, tString("y")))
    }
  }

  test("evaluate dictionary set") {
    val dict = tDict(
      (tString("x"), tInt(10))
    )
    val updated = tDictSet(dict, tString("y"), tInt(20))
    assertEquals(
      evalExp(updated),
      DictValue(Map(
        StringValue("x") -> IntValue(10),
        StringValue("y") -> IntValue(20)
      ))
    )
  }

  test("evaluate dictionary set - update existing key") {
    val dict = tDict(
      (tString("x"), tInt(10))
    )
    val updated = tDictSet(dict, tString("x"), tInt(50))
    assertEquals(
      evalExp(updated),
      DictValue(Map(
        StringValue("x") -> IntValue(50)
      ))
    )
  }

  test("evaluate dictionary keys") {
    val dict = tDict(
      (tString("a"), tInt(1)),
      (tString("b"), tInt(2)),
      (tString("c"), tInt(3))
    )
    val keys = evalExp(tDictKeys(dict)).asList
    assert(keys.toSet == Set(
      StringValue("a"),
      StringValue("b"),
      StringValue("c")
    ))
  }

  test("evaluate dictionary values") {
    val dict = tDict(
      (tString("x"), tInt(10)),
      (tString("y"), tInt(20)),
      (tString("z"), tInt(30))
    )
    val values = evalExp(tDictValues(dict)).asList
    assert(values.toSet == Set(
      IntValue(10),
      IntValue(20),
      IntValue(30)
    ))
  }

  test("evaluate dictionary size") {
    assertEquals(
      evalExp(tDictSize(tDict())),
      IntValue(0)
    )
    assertEquals(
      evalExp(tDictSize(tDict(
        (tString("a"), tInt(1)),
        (tString("b"), tInt(2))
      ))),
      IntValue(2)
    )
  }

  test("evaluate dictionary contains") {
    val dict = tDict(
      (tString("x"), tInt(10)),
      (tString("y"), tInt(20))
    )
    assertEquals(
      evalExp(tDictContains(dict, tString("x"))),
      IntValue(1)
    )
    assertEquals(
      evalExp(tDictContains(dict, tString("z"))),
      IntValue(0)
    )
  }

  test("evaluate dictionary with integer keys") {
    val dict = tDict(
      (tInt(1), tString("one")),
      (tInt(2), tString("two"))
    )
    assertEquals(
      evalExp(tDictAccess(dict, tInt(1))),
      StringValue("one")
    )
    assertEquals(
      evalExp(tDictAccess(dict, tInt(2))),
      StringValue("two")
    )
  }

  test("evaluate dictionary in program with variables") {
    val program = tProgram(
      List.empty,
      tAssign("d", tDict(
        (tString("x"), tInt(10)),
        (tString("y"), tInt(20))
      )),
      tAssign("v", tDictAccess(tId("d"), tString("x"))),
      tAssign("d2", tDictSet(tId("d"), tString("z"), tInt(30))),
      tDictAccess(tId("d2"), tString("z"))
    )
    assertEquals(evalProgram(program), IntValue(30))
  }

  test("evaluate dictionary with functions") {
    val program = tProgram(
      List(
        tFunction("getDictValue", List(("dict", Type.TDict(Type.TString, Type.TInt)), ("key", Type.TString)), Type.TInt,
          tDictAccess(tId("dict"), tId("key"))
        )
      ),
      tAssign("myDict", tDict(
        (tString("x"), tInt(100)),
        (tString("y"), tInt(200))
      )),
      tCall("getDictValue", tId("myDict"), tString("y"))
    )
    assertEquals(evalProgram(program), IntValue(200))
  }

  test("evaluate dictionary with list values") {
    val dict = tDict(
      (tString("numbers"), tList(tInt(1), tInt(2), tInt(3))),
      (tString("moreNumbers"), tList(tInt(4), tInt(5), tInt(6)))
    )
    val numbers = evalExp(tDictAccess(dict, tString("numbers"))).asList
    assertEquals(numbers, List(IntValue(1), IntValue(2), IntValue(3)))
  }

  test("evaluate nested dictionaries") {
    val innerDict1 = tDict(
      (tString("x"), tInt(10))
    )
    val innerDict2 = tDict(
      (tString("y"), tInt(20))
    )
    val outerDict = tDict(
      (tString("dict1"), innerDict1),
      (tString("dict2"), innerDict2)
    )
    val result = evalExp(tDictAccess(tDictAccess(outerDict, tString("dict1")), tString("x")))
    assertEquals(result, IntValue(10))
  }

  test("evaluate dictionary toBool") {
    val emptyDict = tDict()
    val nonEmptyDict = tDict((tString("x"), tInt(1)))
    
    // Empty dict is false in if condition
    assertEquals(
      evalExp(tIf(emptyDict, tInt(1), tInt(0))),
      IntValue(0)
    )
    
    // Non-empty dict is true in if condition  
    assertEquals(
      evalExp(tIf(nonEmptyDict, tInt(1), tInt(0))),
      IntValue(1)
    )
  }
}