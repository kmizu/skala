package com.github.kmizu.tskala
import scala.collection.mutable
import Typer.*

def evalProgram(program: Program): Int = {
  // Use a mutable map as our environment.
  val varEnv = mutable.Map[String, Int]()
  val funcEnv = mutable.Map[String, Func]()
  typeCheckProgram(program)
  val functions = program.functions
  val bodies = program.bodies
  functions.foreach {
    case f @ Func(name, _, _, _) =>
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
  typeOf(e, mutable.Map.empty, mutable.Map.empty)
  eval(e, mutable.Map.empty, mutable.Map.empty)
}

/**
 * Evaluate an expression (or statement) given the current environment.
 *
 * The environment maps names to values, where values can be integers or functions.
 */
def eval(e: Exp, vEnv: mutable.Map[String, Int], fEnv: mutable.Map[String, Func]): Int = {
  def evalRec(e: Exp, env: mutable.Map[String, Int]): Int = {
    e match {
    // Binary expressions: arithmetic and comparisons.
      case Exp.BinExp(op, lhs, rhs) =>
        op match {
          case "+" =>
            evalRec(lhs, env) + evalRec(rhs, env)
          case "-" =>
            evalRec(lhs, env) - evalRec(rhs, env)
          case "*" =>
            evalRec(lhs, env) * evalRec(rhs, env)
          case "/" =>
            evalRec(lhs, env) / evalRec(rhs, env)
          case "<" =>
            if (evalRec(lhs, env) < evalRec(rhs, env)) 1 else 0
          case ">" =>
            if (evalRec(lhs, env) > evalRec(rhs, env)) 1 else 0
          case "<=" =>
            if (evalRec(lhs, env) <= evalRec(rhs, env)) 1 else 0
          case ">=" =>
            if (evalRec(lhs, env) >= evalRec(rhs, env)) 1 else 0
          case "==" =>
            if (evalRec(lhs, env) == evalRec(rhs, env)) 1 else 0
          case "!=" =>
            if (evalRec(lhs, env) != evalRec(rhs, env)) 1 else 0
          case _ =>
            throw new Exception(s"Unknown operator $op")
        }
      // Sequence: evaluate each expression in order and return the result of the last one.
      case Exp.SeqExp(bodies) =>
        var result: Int = 0
        bodies.foreach { expr =>
          result = evalRec(expr, env)
        }
        result
      // If expressions: evaluate the condition, then one branch or the other.
      case Exp.If(condition, thenClause, elseClause) =>
        if (evalRec(condition, env) != 0)
          evalRec(thenClause, env)
        else
          evalRec(elseClause, env)
      // While loop: evaluate condition and repeatedly evaluate the bodies until false.
      case Exp.While(condition, bodies) =>
        while (evalRec(condition, env) != 0) {
          bodies.foreach(body => evalRec(body, env))
        }
        0
      // Assignment: evaluate the expression and update the environment.
      case Exp.Assignment(name, expression) =>
        val v = evalRec(expression, env)
        env(name) = v
        v
      // Identifiers: look up their value in the environment.
      case Exp.Ident(name) =>
        env.getOrElse(name, throw new Exception(s"Undefined identifier: $name"))
      // Function calls: evaluate the arguments, create a new environment, bind parameters and evaluate the function body.
      case Exp.Call(name, args) =>
        fEnv.get(name) match {
          case Some(Func(_, tParams, _, body)) =>
            val params = tParams.map(_._1)
            val argValues = args.map(evalRec(_, env))
            // Create a new environment that copies the current environment.
            val newEnv = mutable.Map[String, Int]() ++ env
            params.zip(argValues).foreach { case (param, argVal) =>
              newEnv(param) = argVal
            }
            evalRec(body, newEnv)
          case None =>
            throw new Exception(s"Function $name is not defined")
        }
      // Integer literal: return its value.
      case Exp.VInt(value) =>
        value
    }
  }
  evalRec(e, vEnv)
}