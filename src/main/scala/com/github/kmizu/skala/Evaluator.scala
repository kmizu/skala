package com.github.kmizu.skala
import scala.collection.mutable
import scala.collection.mutable.{Map => MMap}

def evalProgram(program: Program): Int = {
  // Use a mutable map as our environment.
  val varEnv = mutable.Map[String, Int]()
  val funcEnv = mutable.Map[String, Func]()
  val functions = program.functions
  val bodies = program.bodies
  // Store functions in the environment.
  functions.foreach {
    case f @ Func(name, _, _) =>
      funcEnv(name) = f
  }
  // Evaluate the bodies sequentially.
  var result: Int = 0
  bodies.foreach { body =>
    result = eval(body, varEnv, funcEnv)
  }
  result
}

def evalExp(e: Exp): Int = {
  eval(e, mutable.Map.empty, mutable.Map.empty)
}

/**
 * Evaluate an expression (or statement) given the current environment.
 *
 * The environment maps names to values, where values can be integers or functions.
 */
import scala.collection.mutable.{Map => MMap}
def eval(e: Exp, varEnv: MMap[String, Int], funcEnv: MMap[String, Func]): Int = {
  def evalRec(e: Exp): Int = {
    e match {
    // Binary expressions: arithmetic and comparisons.
      case Exp.BinExp("+", lhs, rhs) =>
        evalRec(lhs) + evalRec(rhs)
      case Exp.BinExp("-", lhs, rhs) =>
        evalRec(lhs) - evalRec(rhs)
      case Exp.BinExp("*", lhs, rhs) =>
        evalRec(lhs) * evalRec(rhs)
      case Exp.BinExp("/", lhs, rhs) =>
        evalRec(lhs) / evalRec(rhs)
      case Exp.BinExp("<", lhs, rhs) =>
        if (evalRec(lhs) < evalRec(rhs)) 1 else 0
      case Exp.BinExp(">", lhs, rhs) =>
        if (evalRec(lhs) > evalRec(rhs)) 1 else 0
      case Exp.BinExp("<=", lhs, rhs) =>
        if (evalRec(lhs) <= evalRec(rhs)) 1 else 0
      case Exp.BinExp(">=", lhs, rhs) =>
        if (evalRec(lhs) >= evalRec(rhs)) 1 else 0
      case Exp.BinExp("==", lhs, rhs) =>
        if (evalRec(lhs) == evalRec(rhs)) 1 else 0
      case Exp.BinExp("!=", lhs, rhs) =>
        if (evalRec(lhs) != evalRec(rhs)) 1 else 0
      case Exp.BinExp(op, lhs, rhs) =>
        sys.error(s"Unknown operator $op")
      // Sequence: evaluate each expression in order and return the result of the last one.
      case Exp.SeqExp(bodies) =>
        var result: Int = 0
        bodies.foreach { expr =>
          result = evalRec(expr)
        }
        result
      // If expressions: evaluate the condition, then one branch or the other.
      case Exp.If(condition, thenClause, elseClause) =>
        if (evalRec(condition) != 0)
          evalRec(thenClause)
        else
          evalRec(elseClause)
      // While loop: evaluate condition and repeatedly evaluate the bodies until false.
      case Exp.While(condition, bodies) =>
        while (evalRec(condition) != 0) {
          bodies.foreach(body => evalRec(body))
        }
        0
      // Assignment: evaluate the expression and update the environment.
      case Exp.Assignment(name, expression) =>
        val v = evalRec(expression)
        varEnv(name) = v
        v
      // Identifiers: look up their value in the environment.
      case Exp.Ident(name) =>
        varEnv.getOrElse(name, sys.error(s"Undefined identifier: $name"))
      // Function calls: evaluate the arguments, create a new environment, bind parameters and evaluate the function body.
      case Exp.Call(name, args) =>
        funcEnv.get(name) match {
          case Some(Func(_, params, body)) =>
            val argValues = args.map(evalRec(_))
            // Create a new environment that copies the current environment.
            val newEnv = mutable.Map[String, Int]() ++ varEnv
            params.zip(argValues).foreach { case (param, argVal) =>
              newEnv(param) = argVal
            }
            eval(body, newEnv, funcEnv)
          case None =>
            throw new Exception(s"Function $name is not defined")
        }
      // Integer literal: return its value.
      case Exp.VInt(value) =>
        value
    }
  }
  evalRec(e)
}