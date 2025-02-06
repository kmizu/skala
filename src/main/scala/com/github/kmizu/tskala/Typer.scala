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
      // Look up identifiers in the variable environment.
      case Ident(name) =>
        typeEnv.getOrElse(name, sys.error(s"Undefined identifier: $name"))
      // Binary expressions.
      case BinExp(op@("+" | "-" | "*" | "/"), lhs, rhs) =>
        val lType = typeOfRec(lhs)
        val rType = typeOfRec(rhs)
        if (lType == TInt && rType == TInt) TInt
        else
          sys.error(s"Type error in arithmetic operator $op: expected Int operands, got $lType and $rType")
      case BinExp(op@("<" | ">" | "<=" | ">=" | "==" | "!="), lhs, rhs) =>
        val lType = typeOfRec(lhs)
        val rType = typeOfRec(rhs)
        if (lType == TInt && rType == TInt) TBool
        else
          sys.error(s"Type error in comparison operator $op: expected Int operands, got $lType and $rType")
      case BinExp(op, lhs, rhs) =>
        sys.error(s"Unknown operator in type checking: $op")
      // If–expressions: the condition must be Boolean and the two branches must have the same type.
      case If(condition, thenClause, elseClause) =>
        val condType = typeOfRec(condition)
        if (condType != TBool)
          sys.error(s"Type error in if condition: expected Bool, got $condType")
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
        if (condType != TBool)
          sys.error(s"Type error in while condition: expected Bool, got $condType")
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