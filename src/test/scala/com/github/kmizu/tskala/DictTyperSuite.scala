package com.github.kmizu.tskala
import munit.FunSuite
import scala.collection.mutable

class DictTyperSuite extends FunSuite {
  import Exp.*
  import Type.*
  import Typer.*

  test("type check dictionary literal") {
    val dict = tDict(
      (tString("name"), tString("John")),
      (tString("age"), tString("30"))
    )
    assertEquals(
      typeOf(dict, mutable.Map.empty, mutable.Map.empty),
      TDict(TString, TString)
    )
  }

  test("type check empty dictionary") {
    val dict = tDict()
    val resultType = typeOf(dict, mutable.Map.empty, mutable.Map.empty)
    // Empty dictionaries now correctly infer to polymorphic types
    assert(resultType match {
      case TDict(TVar(_), TVar(_)) => true
      case _ => false
    })
  }

  test("type check heterogeneous dictionary keys - should fail") {
    val dict = tDict(
      (tString("x"), tInt(10)),
      (tInt(1), tInt(20))
    )
    intercept[Exception] {
      typeOf(dict, mutable.Map.empty, mutable.Map.empty)
    }
  }

  test("type check heterogeneous dictionary values - should fail") {
    val dict = tDict(
      (tString("x"), tInt(10)),
      (tString("y"), tString("hello"))
    )
    intercept[Exception] {
      typeOf(dict, mutable.Map.empty, mutable.Map.empty)
    }
  }

  test("type check dictionary access") {
    val varEnv = mutable.Map[String, Type]()
    varEnv("dict") = TDict(TString, TInt)
    
    assertEquals(
      typeOf(tDictAccess(tId("dict"), tString("key")), varEnv, mutable.Map.empty),
      TInt
    )
  }

  test("type check dictionary access with wrong key type") {
    val varEnv = mutable.Map[String, Type]()
    varEnv("dict") = TDict(TString, TInt)
    
    intercept[Exception] {
      typeOf(tDictAccess(tId("dict"), tInt(1)), varEnv, mutable.Map.empty)
    }
  }

  test("type check dictionary set") {
    val varEnv = mutable.Map[String, Type]()
    varEnv("dict") = TDict(TString, TInt)
    
    assertEquals(
      typeOf(tDictSet(tId("dict"), tString("key"), tInt(100)), varEnv, mutable.Map.empty),
      TDict(TString, TInt)
    )
  }

  test("type check dictionary set with wrong value type") {
    val varEnv = mutable.Map[String, Type]()
    varEnv("dict") = TDict(TString, TInt)
    
    intercept[Exception] {
      typeOf(tDictSet(tId("dict"), tString("key"), tString("wrong")), varEnv, mutable.Map.empty)
    }
  }

  test("type check dictionary keys") {
    val varEnv = mutable.Map[String, Type]()
    varEnv("dict") = TDict(TString, TInt)
    
    assertEquals(
      typeOf(tDictKeys(tId("dict")), varEnv, mutable.Map.empty),
      TList(TString)
    )
  }

  test("type check dictionary values") {
    val varEnv = mutable.Map[String, Type]()
    varEnv("dict") = TDict(TString, TInt)
    
    assertEquals(
      typeOf(tDictValues(tId("dict")), varEnv, mutable.Map.empty),
      TList(TInt)
    )
  }

  test("type check dictionary size") {
    val varEnv = mutable.Map[String, Type]()
    varEnv("dict") = TDict(TString, TInt)
    
    assertEquals(
      typeOf(tDictSize(tId("dict")), varEnv, mutable.Map.empty),
      TInt
    )
  }

  test("type check dictionary contains") {
    val varEnv = mutable.Map[String, Type]()
    varEnv("dict") = TDict(TString, TInt)
    
    assertEquals(
      typeOf(tDictContains(tId("dict"), tString("key")), varEnv, mutable.Map.empty),
      TBool
    )
  }

  test("type check nested dictionaries") {
    val innerDict = tDict((tString("x"), tInt(10)))
    val outerDict = tDict((tString("inner"), innerDict))
    
    assertEquals(
      typeOf(outerDict, mutable.Map.empty, mutable.Map.empty),
      TDict(TString, TDict(TString, TInt))
    )
  }

  test("type check dictionary with list values") {
    val dict = tDict(
      (tString("numbers"), tList(tInt(1), tInt(2))),
      (tString("more"), tList(tInt(3), tInt(4)))
    )
    
    assertEquals(
      typeOf(dict, mutable.Map.empty, mutable.Map.empty),
      TDict(TString, TList(TInt))
    )
  }

  test("type check dictionary in function") {
    val funcEnv = mutable.Map[String, Func]()
    val func = tFunction(
      "sumDictValues",
      List(("dict", TDict(TString, TInt))),
      TInt,
      tInt(100) // Dummy implementation
    )
    funcEnv("sumDictValues") = func
    
    typeCheckFunction(func, funcEnv) // Should not throw
  }

  test("type check dictionary operations on non-dict type") {
    val varEnv = mutable.Map[String, Type]()
    varEnv("notDict") = TInt
    
    intercept[Exception] {
      typeOf(tDictKeys(tId("notDict")), varEnv, mutable.Map.empty)
    }
    
    intercept[Exception] {
      typeOf(tDictAccess(tId("notDict"), tString("key")), varEnv, mutable.Map.empty)
    }
  }

  test("type check dictionary in if condition") {
    val emptyDict = tDict()
    val expr = tIf(emptyDict, tInt(1), tInt(0))
    
    assertEquals(
      typeOf(expr, mutable.Map.empty, mutable.Map.empty),
      TInt
    )
  }
}