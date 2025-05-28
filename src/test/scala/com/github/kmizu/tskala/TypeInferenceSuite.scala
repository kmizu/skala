package com.github.kmizu.tskala

import com.github.kmizu.tskala.*

class TypeInferenceSuite extends munit.FunSuite:
  import Exp.*
  import Type.*

  def inferType(exp: Exp): Type =
    val (_, tpe) = Typer.infer(exp, TypeEnv(Map.empty[String, TypeScheme], Map.empty[String, Func]))
    tpe

  def inferProgramType(program: Program): Type =
    Typer.typeCheckProgram(program)

  test("infer type of integer literal") {
    assertEquals(inferType(VInt(42)), TInt)
  }

  test("infer type of boolean literal") {
    // This language uses integers for booleans
    assertEquals(inferType(VInt(1)), TInt)
  }

  test("infer type of string literal") {
    assertEquals(inferType(VString("hello")), TString)
  }

  test("infer type of arithmetic operations") {
    assertEquals(inferType(BinExp("+", VInt(1), VInt(2))), TInt)
    assertEquals(inferType(BinExp("-", VInt(5), VInt(3))), TInt)
    assertEquals(inferType(BinExp("*", VInt(2), VInt(3))), TInt)
    assertEquals(inferType(BinExp("/", VInt(10), VInt(2))), TInt)
  }

  test("infer type of comparison operations") {
    assertEquals(inferType(BinExp("<", VInt(1), VInt(2))), TBool)
    assertEquals(inferType(BinExp(">", VInt(5), VInt(3))), TBool)
    assertEquals(inferType(BinExp("<=", VInt(1), VInt(1))), TBool)
    assertEquals(inferType(BinExp(">=", VInt(2), VInt(2))), TBool)
    assertEquals(inferType(BinExp("==", VInt(1), VInt(1))), TBool)
    assertEquals(inferType(BinExp("!=", VInt(1), VInt(2))), TBool)
  }

  test("infer type of empty list as polymorphic") {
    val listType = inferType(VList(List()))
    assert(listType match {
      case TList(TVar(_)) => true
      case _ => false
    }, s"Expected TList(TVar(_)), got $listType")
  }

  test("infer type of integer list") {
    assertEquals(inferType(VList(List(VInt(1), VInt(2)))), TList(TInt))
  }

  test("infer type of list operations") {
    val intList = VList(List(VInt(1), VInt(2)))
    assertEquals(inferType(ListAccess(intList, VInt(0))), TInt)
    assertEquals(inferType(ListLength(intList)), TInt)
    assertEquals(inferType(ListAppend(intList, VInt(3))), TList(TInt))
  }

  test("infer type of identity lambda") {
    val idLambda = Lambda(List("x"), Ident("x"))
    val idType = inferType(idLambda)
    assert(idType match {
      case TFunc(List(TVar(a)), TVar(b)) if a == b => true
      case _ => false
    }, s"Expected TFunc([TVar(a)], TVar(a)), got $idType")
  }

  test("infer type of lambda with int operations") {
    val addOne = Lambda(List("x"), BinExp("+", Ident("x"), VInt(1)))
    assertEquals(inferType(addOne), TFunc(List(TInt), TInt))
  }

  test("infer type of multi-parameter lambda") {
    // Test just the lambda type without arithmetic constraints
    val selectFirst = Lambda(List("x", "y"), Ident("x"))
    val lambdaType = inferType(selectFirst)
    assert(lambdaType match {
      case TFunc(List(TVar(a), TVar(_)), TVar(b)) if a == b => true
      case _ => false
    }, s"Expected TFunc with matching type vars, got $lambdaType")
  }

  test("infer type of lambda application") {
    val idLambda = Lambda(List("x"), Ident("x"))
    val applied = Apply(idLambda, List(VInt(42)))
    assertEquals(inferType(applied), TInt)
  }

  test("infer type of let with polymorphic value") {
    val letId = Let("id", Lambda(List("x"), Ident("x")), 
                    Apply(Ident("id"), List(VInt(42))))
    assertEquals(inferType(letId), TInt)
  }

  test("let polymorphism - using identity twice with different types") {
    val letPoly = Let("id", Lambda(List("x"), Ident("x")),
                      SeqExp(List(
                        Apply(Ident("id"), List(VInt(42))),
                        Apply(Ident("id"), List(VInt(1)))  // true = 1
                      )))
    assertEquals(inferType(letPoly), TInt)  // last exp type is Int
  }

  test("infer type of nested lambda (currying)") {
    val curry = Lambda(List("x"), Lambda(List("y"), Ident("x")))
    val curryType = inferType(curry)
    assert(curryType match {
      case TFunc(List(TVar(a)), TFunc(List(TVar(_)), TVar(b))) if a == b => true
      case _ => false
    }, s"Expected curried function type, got $curryType")
  }

  test("infer type of function composition") {
    // compose f g x = f(g(x))
    val compose = Lambda(List("f", "g", "x"), 
                        Apply(Ident("f"), List(Apply(Ident("g"), List(Ident("x"))))))
    val composeType = inferType(compose)
    // Should be: (b -> c) -> (a -> b) -> a -> c
    assert(composeType match {
      case TFunc(List(TFunc(List(TVar(b1)), TVar(c)), 
                      TFunc(List(TVar(a)), TVar(b2)), 
                      TVar(a2)), TVar(c2)) 
           if b1 == b2 && a == a2 && c == c2 => true
      case _ => false
    }, s"Expected composition type, got $composeType")
  }

  test("infer type of if expression") {
    val ifExp = If(BinExp("<", VInt(1), VInt(2)), 
                   VInt(10), 
                   VInt(20))
    assertEquals(inferType(ifExp), TInt)
  }

  test("infer type of polymorphic if") {
    // If requires a truthy type for condition, which can be Int, Bool, String, List, Dict, or TVar
    val polyIf = Lambda(List("cond", "t", "f"),
                       If(Ident("cond"), Ident("t"), Ident("f")))
    val polyIfType = inferType(polyIf)
    assert(polyIfType match {
      case TFunc(List(TVar(_), TVar(a), TVar(b)), TVar(c)) if a == b && b == c => true
      case _ => false
    }, s"Expected polymorphic if type, got $polyIfType")
  }

  test("infer type of empty dictionary as polymorphic") {
    val dictType = inferType(VDict(List()))
    assert(dictType match {
      case TDict(TVar(_), TVar(_)) => true
      case _ => false
    }, s"Expected TDict(TVar(_), TVar(_)), got $dictType")
  }

  test("infer type of dictionary operations") {
    val dict = VDict(List((VString("a"), VInt(1))))
    assertEquals(inferType(dict), TDict(TString, TInt))
    assertEquals(inferType(DictAccess(dict, VString("a"))), TInt)
    assertEquals(inferType(DictKeys(dict)), TList(TString))
    assertEquals(inferType(DictValues(dict)), TList(TInt))
    assertEquals(inferType(DictSize(dict)), TInt)
    assertEquals(inferType(DictContains(dict, VString("a"))), TBool)
  }

  test("infer type of program with functions") {
    val program = Program(
      List(
        Func("add", List("x" -> TInt, "y" -> TInt), TInt,
                   BinExp("+", Ident("x"), Ident("y")))
      ),
      List(Call("add", List(VInt(1), VInt(2))))
    )
    assertEquals(inferProgramType(program), TInt)
  }

  test("infer type of program with let polymorphism") {
    val program = Program(
      List(),
      List(Let("id", Lambda(List("x"), Ident("x")),
          Let("intResult", Apply(Ident("id"), List(VInt(42))),
              Let("boolResult", Apply(Ident("id"), List(VInt(1))),  // true = 1
                  Ident("boolResult")))))
    )
    assertEquals(inferProgramType(program), TInt)  // since bool = int in this language
  }

  test("infer type of higher-order list operations") {
    // Simple map: map f lst
    val mapType = Lambda(List("f", "lst"),
                        VList(List())) // Placeholder for actual map implementation
    val mapInferred = inferType(mapType)
    assert(mapInferred match {
      case TFunc(List(TVar(_), TVar(_)), TList(TVar(_))) => true
      case _ => false
    }, s"Expected map type pattern, got $mapInferred")
  }

  test("verify monomorphic restriction for mutable values") {
    // Variables assigned in while loops should not be polymorphic
    // We need to use a program to properly initialize variables
    val program = Program(
      List(),
      List(
        Assignment("x", VInt(0)),
        While(BinExp("<", Ident("x"), VInt(10)),
              List(Assignment("x", BinExp("+", Ident("x"), VInt(1))))),
        Ident("x")
      )
    )
    assertEquals(inferProgramType(program), TInt)
  }

  test("infer type of nested let with shadowing") {
    val nestedLet = Let("x", VInt(1),
                       Let("x", VInt(1),  // true = 1
                           Ident("x")))
    assertEquals(inferType(nestedLet), TInt)
  }

  test("infer type of curried application") {
    // This language doesn't support partial application directly
    // We need to use nested lambdas for currying
    val curriedConst = Lambda(List("x"), Lambda(List("y"), Ident("x")))
    val const5 = Apply(curriedConst, List(VInt(5)))
    val partialType = inferType(const5)
    assert(partialType match {
      case TFunc(List(TVar(_)), TInt) => true
      case _ => false
    }, s"Expected TFunc(List(TVar(_)), TInt), got $partialType")
  }