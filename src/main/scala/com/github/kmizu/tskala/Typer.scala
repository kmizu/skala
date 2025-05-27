package com.github.kmizu.tskala

import scala.collection.mutable
import Exp._
import Type._

object Typer {

  /**
   * Compute the type of an expression.
   *
   * @param e an expression node
   * @param typeEnv  a mutable mapping from variable names to their types
   * @param funcEnv  an immutable mapping from function names to their definitions
   * @return the type (Type) of the AST node, if it type–checks
   */
  def typeOf(e: Exp, typeEnv: mutable.Map[String, Type], funcEnv: mutable.Map[String, Func]): Type = {
    def typeOfRec(e: Exp): Type = e match {
      // Integer literals always have type TInt.
      case VInt(_) => 
        TInt
      // String literals always have type TString.
      case VString(_) =>
        TString
      // Look up identifiers in the variable environment.
      case Ident(name) =>
        typeEnv.getOrElse(name, sys.error(s"Undefined identifier: $name"))
      // Binary expressions.
      case BinExp("+", lhs, rhs) =>
        val lType = typeOfRec(lhs)
        val rType = typeOfRec(rhs)
        (lType, rType) match {
          case (TInt, TInt) => TInt
          case (TString, TString) => TString
          case _ => sys.error(s"Type error in + operator: operands must both be Int or both be String, got $lType and $rType")
        }
      case BinExp(op@("-" | "*" | "/"), lhs, rhs) =>
        val lType = typeOfRec(lhs)
        val rType = typeOfRec(rhs)
        if (lType == TInt && rType == TInt) TInt
        else
          sys.error(s"Type error in arithmetic operator $op: expected Int operands, got $lType and $rType")
      case BinExp(op@("<" | ">" | "<=" | ">="), lhs, rhs) =>
        val lType = typeOfRec(lhs)
        val rType = typeOfRec(rhs)
        if (lType == TInt && rType == TInt) TBool
        else
          sys.error(s"Type error in comparison operator $op: expected Int operands, got $lType and $rType")
      case BinExp(op@("==" | "!="), lhs, rhs) =>
        val lType = typeOfRec(lhs)
        val rType = typeOfRec(rhs)
        if (lType == rType) TBool
        else
          sys.error(s"Type error in comparison operator $op: operands must have the same type, got $lType and $rType")
      case BinExp(op, lhs, rhs) =>
        sys.error(s"Unknown operator in type checking: $op")
      // If–expressions: the condition must be Boolean and the two branches must have the same type.
      case If(condition, thenClause, elseClause) =>
        val condType = typeOfRec(condition)
        // Allow Bool, Int, String, List, and Dict types in if conditions (truthy/falsy semantics)
        condType match {
          case TBool | TInt | TString | TList(_) | TDict(_, _) => // OK
          case _ => sys.error(s"Type error in if condition: expected Bool, got $condType")
        }
        val thenType = typeOfRec(thenClause)
        val elseType = typeOfRec(elseClause)
        if (thenType != elseType)
          sys.error(s"Type error in if branches: then branch has type $thenType but else branch has type $elseType")
        thenType
      // Sequence expressions: type–check each expression in order and return the type of the last one.
      case SeqExp(bodies) =>
        var resultType: Type = TInt  // default value; will be updated below
        bodies.foreach { expr =>
          resultType = typeOfRec(expr)
        }
        resultType
      // While loops: the condition must be Boolean and we check the bodies.
      // (Since the evaluator returns 0 for while loops, we assign them type TInt.)
      case While(condition, bodies) =>
        val condType = typeOfRec(condition)
        // Allow Bool, Int, String, List, and Dict types in while conditions (truthy/falsy semantics)
        condType match {
          case TBool | TInt | TString | TList(_) | TDict(_, _) => // OK
          case _ => sys.error(s"Type error in while condition: expected Bool, got $condType")
        }
        bodies.foreach { expr =>
          typeOfRec(expr)
        }
        TInt
      // Assignments: type–check the right–hand side and
      // if the variable already exists, its type must match the new value.
      // Otherwise, we “declare” it in the environment.
      case Assignment(name, expression) =>
        val exprType = typeOfRec(expression)
        if (typeEnv.contains(name)) {
          val existingType = typeEnv(name)
          if (existingType != exprType)
            sys.error(s"Type error in assignment to '$name': variable already has type $existingType, but expression is $exprType")
        } else {
          typeEnv(name) = exprType  // implicit declaration
        }
        exprType
      // Function calls: look up the function and check that the argument list agrees with the parameters.
      case Call(name, args) =>
        funcEnv.get(name) match {
          case Some(Func(_, params, retType, _)) =>
            if (params.length != args.length)
              sys.error(s"Type error in function call '$name': expected ${params.length} arguments, got ${args.length}")
            params.zip(args).foreach { case ((paramName, paramType), arg) =>
              val argType = typeOfRec(arg)
              if (argType != paramType)
                sys.error(s"Type error in function call '$name': expected argument of type $paramType for parameter '$paramName', but got $argType")
            }
            retType
          case None =>
            sys.error(s"Undefined function in type checking: '$name'")
        }
      // List literal: all elements must have the same type
      case VList(elements) =>
        if (elements.isEmpty) {
          // Empty list - we need to infer the type from context or use a default
          // For now, we'll default to List[Int]
          TList(TInt)
        } else {
          val firstType = typeOfRec(elements.head)
          elements.tail.foreach { elem =>
            val elemType = typeOfRec(elem)
            if (elemType != firstType)
              sys.error(s"Type error in list literal: all elements must have the same type, but found $firstType and $elemType")
          }
          TList(firstType)
        }
      // List access: list[index]
      case ListAccess(list, index) =>
        val listType = typeOfRec(list)
        val indexType = typeOfRec(index)
        if (indexType != TInt)
          sys.error(s"Type error in list access: index must be Int, got $indexType")
        listType match {
          case TList(elemType) => elemType
          case _ => sys.error(s"Type error in list access: expected List type, got $listType")
        }
      // List length: returns Int
      case ListLength(list) =>
        val listType = typeOfRec(list)
        listType match {
          case TList(_) => TInt
          case _ => sys.error(s"Type error in list length: expected List type, got $listType")
        }
      // List append: returns a new list of the same type
      case ListAppend(list, element) =>
        val listType = typeOfRec(list)
        val elemType = typeOfRec(element)
        listType match {
          case TList(expectedElemType) =>
            if (elemType != expectedElemType)
              sys.error(s"Type error in list append: expected element of type $expectedElemType, got $elemType")
            listType
          case _ => sys.error(s"Type error in list append: expected List type, got $listType")
        }
      // String length: returns Int
      case StringLength(str) =>
        val strType = typeOfRec(str)
        if (strType != TString)
          sys.error(s"Type error in string length: expected String, got $strType")
        TInt
      // String concatenation: returns String
      case StringConcat(lhs, rhs) =>
        val lType = typeOfRec(lhs)
        val rType = typeOfRec(rhs)
        if (lType != TString || rType != TString)
          sys.error(s"Type error in string concatenation: expected String operands, got $lType and $rType")
        TString
      // Dictionary literal: all keys must have same type, all values must have same type
      case VDict(entries) =>
        if (entries.isEmpty) {
          // Empty dict - default to Dict[String, Int]
          TDict(TString, TInt)
        } else {
          val (firstKey, firstValue) = entries.head
          val keyType = typeOfRec(firstKey)
          val valueType = typeOfRec(firstValue)
          entries.tail.foreach { case (k, v) =>
            val kType = typeOfRec(k)
            val vType = typeOfRec(v)
            if (kType != keyType)
              sys.error(s"Type error in dict literal: all keys must have the same type, but found $keyType and $kType")
            if (vType != valueType)
              sys.error(s"Type error in dict literal: all values must have the same type, but found $valueType and $vType")
          }
          TDict(keyType, valueType)
        }
      // Dictionary access: dict[key]
      case DictAccess(dict, key) =>
        val dictType = typeOfRec(dict)
        val keyType = typeOfRec(key)
        dictType match {
          case TDict(expectedKeyType, valueType) =>
            if (keyType != expectedKeyType)
              sys.error(s"Type error in dict access: expected key of type $expectedKeyType, got $keyType")
            valueType
          case _ => sys.error(s"Type error in dict access: expected Dict type, got $dictType")
        }
      // Dictionary set: returns updated dictionary
      case DictSet(dict, key, value) =>
        val dictType = typeOfRec(dict)
        val keyType = typeOfRec(key)
        val valueType = typeOfRec(value)
        dictType match {
          case TDict(expectedKeyType, expectedValueType) =>
            if (keyType != expectedKeyType)
              sys.error(s"Type error in dict set: expected key of type $expectedKeyType, got $keyType")
            if (valueType != expectedValueType)
              sys.error(s"Type error in dict set: expected value of type $expectedValueType, got $valueType")
            dictType
          case _ => sys.error(s"Type error in dict set: expected Dict type, got $dictType")
        }
      // Dictionary keys: returns List of keys
      case DictKeys(dict) =>
        val dictType = typeOfRec(dict)
        dictType match {
          case TDict(keyType, _) => TList(keyType)
          case _ => sys.error(s"Type error in dict keys: expected Dict type, got $dictType")
        }
      // Dictionary values: returns List of values
      case DictValues(dict) =>
        val dictType = typeOfRec(dict)
        dictType match {
          case TDict(_, valueType) => TList(valueType)
          case _ => sys.error(s"Type error in dict values: expected Dict type, got $dictType")
        }
      // Dictionary size: returns Int
      case DictSize(dict) =>
        val dictType = typeOfRec(dict)
        dictType match {
          case TDict(_, _) => TInt
          case _ => sys.error(s"Type error in dict size: expected Dict type, got $dictType")
        }
      // Dictionary contains: returns Bool
      case DictContains(dict, key) =>
        val dictType = typeOfRec(dict)
        val keyType = typeOfRec(key)
        dictType match {
          case TDict(expectedKeyType, _) =>
            if (keyType != expectedKeyType)
              sys.error(s"Type error in dict contains: expected key of type $expectedKeyType, got $keyType")
            TBool
          case _ => sys.error(s"Type error in dict contains: expected Dict type, got $dictType")
        }
    }
    typeOfRec(e)
  }

  /**
   * Type–check a function definition.
   *
   * This method builds a new variable environment that maps the function parameters
   * to their declared types, then type–checks the function body and compares the result
   * with the declared return type.
   */
  def typeCheckFunction(func: Func, funcEnv: mutable.Map[String, Func]): Unit = {
    // Create a fresh variable environment for the function body.
    val typeEnv = mutable.Map[String, Type]()
    // Add each parameter to the environment.
    func.params.foreach { case (paramName, paramType) =>
      typeEnv(paramName) = paramType
    }
    // Compute the type of the body.
    val bodyType = typeOf(func.body, typeEnv, funcEnv)
    if (bodyType != func.returnType)
      sys.error(s"Type error in '${func.name}': body type $bodyType does not match declared return type ${func.returnType}")
  }

  /**
   * Type–check an entire program.
   *
   * First, we build a function environment from the top–level function definitions,
   * then we type–check each function and finally the program bodies.
   *
   * @param prog the AST for the program
   * @return the type of the final body expression (typically expected to be TInt)
   */
  def typeCheckProgram(prog: Program): Type = {
    val functions = prog.functions
    val bodies = prog.bodies
    // Build the function environment.
    val env: Map[String, Func] = functions.collect {
      case f @ Func(name, params, retType, body) => name -> f
    }.toMap
    val funcEnv = mutable.Map.from(env)

    // Type–check each function definition.
    functions.foreach { func =>
      typeCheckFunction(func, funcEnv)
    }

    // Type–check the top–level bodies.
    val typeEnv = mutable.Map[String, Type]()
    var resultType: Type = TInt  // default value
    bodies.foreach { expr =>
      resultType = typeOf(expr, typeEnv, funcEnv)
    }
    resultType
  }
}