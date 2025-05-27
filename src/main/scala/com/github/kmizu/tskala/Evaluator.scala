package com.github.kmizu.tskala
import scala.collection.mutable
import Typer.*
import Value.*

def evalProgramInt(program: Program): Int = {
  evalProgram(program).asInt
}

def evalProgram(program: Program): Value = {
  // Use a mutable map as our environment.
  val varEnv = mutable.Map[String, Value]()
  val funcEnv = mutable.Map[String, Func]()
  typeCheckProgram(program)
  val functions = program.functions
  val bodies = program.bodies
  functions.foreach {
    case f @ Func(name, _, _, _) =>
      funcEnv(name) = f
  }
  // Evaluate the bodies sequentially.
  var result: Value = IntValue(0)
  bodies.foreach { body =>
    result = eval(body, varEnv, funcEnv)
  }
  result
}

def evalExpInt(e: Exp): Int = {
  evalExp(e).asInt
}

def evalExp(e: Exp): Value = {
  val initialEnv = TypeEnv(Map.empty, Map.empty)
  val (_, resultType, _) = inferWithEnv(e, initialEnv)
  eval(e, mutable.Map.empty, mutable.Map.empty)
}

/**
 * Evaluate an expression (or statement) given the current environment.
 *
 * The environment maps names to values, where values can be integers or functions.
 */
def eval(e: Exp, vEnv: mutable.Map[String, Value], fEnv: mutable.Map[String, Func]): Value = {
  def evalRec(e: Exp, env: mutable.Map[String, Value]): Value = {
    e match {
    // Binary expressions: arithmetic and comparisons.
      case Exp.BinExp(op, lhs, rhs) =>
        op match {
          case "+" =>
            // Check types for string concatenation
            (evalRec(lhs, env), evalRec(rhs, env)) match {
              case (StringValue(s1), StringValue(s2)) => StringValue(s1 + s2)
              case (l, r) => IntValue(l.asInt + r.asInt)
            }
          case "-" =>
            IntValue(evalRec(lhs, env).asInt - evalRec(rhs, env).asInt)
          case "*" =>
            IntValue(evalRec(lhs, env).asInt * evalRec(rhs, env).asInt)
          case "/" =>
            IntValue(evalRec(lhs, env).asInt / evalRec(rhs, env).asInt)
          case "<" =>
            IntValue(if (evalRec(lhs, env).asInt < evalRec(rhs, env).asInt) 1 else 0)
          case ">" =>
            IntValue(if (evalRec(lhs, env).asInt > evalRec(rhs, env).asInt) 1 else 0)
          case "<=" =>
            IntValue(if (evalRec(lhs, env).asInt <= evalRec(rhs, env).asInt) 1 else 0)
          case ">=" =>
            IntValue(if (evalRec(lhs, env).asInt >= evalRec(rhs, env).asInt) 1 else 0)
          case "==" =>
            IntValue(if (evalRec(lhs, env) == evalRec(rhs, env)) 1 else 0)
          case "!=" =>
            IntValue(if (evalRec(lhs, env) != evalRec(rhs, env)) 1 else 0)
          case _ =>
            throw new Exception(s"Unknown operator $op")
        }
      // Sequence: evaluate each expression in order and return the result of the last one.
      case Exp.SeqExp(bodies) =>
        var result: Value = IntValue(0)
        bodies.foreach { expr =>
          result = evalRec(expr, env)
        }
        result
      // If expressions: evaluate the condition, then one branch or the other.
      case Exp.If(condition, thenClause, elseClause) =>
        if (evalRec(condition, env).toBool)
          evalRec(thenClause, env)
        else
          evalRec(elseClause, env)
      // While loop: evaluate condition and repeatedly evaluate the bodies until false.
      case Exp.While(condition, bodies) =>
        while (evalRec(condition, env).toBool) {
          bodies.foreach(body => evalRec(body, env))
        }
        IntValue(0)
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
        // First check if it's a variable containing a function value
        env.get(name) match {
          case Some(FunctionValue(params, body, closure)) =>
            val argValues = args.map(evalRec(_, env))
            if (params.length != args.length) {
              throw new Exception(s"Function expects ${params.length} arguments but got ${args.length}")
            }
            // Create new environment from closure
            val newEnv = closure.clone()
            params.zip(argValues).foreach { case (param, argVal) =>
              newEnv(param) = argVal
            }
            evalRec(body, newEnv)
          case _ =>
            // Try regular function lookup
            fEnv.get(name) match {
              case Some(Func(_, tParams, _, body)) =>
                val params = tParams.map(_._1)
                val argValues = args.map(evalRec(_, env))
                // Create a new environment that copies the current environment.
                val newEnv = mutable.Map[String, Value]() ++ env
                params.zip(argValues).foreach { case (param, argVal) =>
                  newEnv(param) = argVal
                }
                evalRec(body, newEnv)
              case None =>
                throw new Exception(s"Function $name is not defined")
            }
        }
      // Integer literal: return its value.
      case Exp.VInt(value) =>
        IntValue(value)
      // String literal: return its value.
      case Exp.VString(value) =>
        StringValue(value)
      // List literal: evaluate all elements
      case Exp.VList(elements) =>
        ListValue(elements.map(e => evalRec(e, env)))
      // List access: get element at index
      case Exp.ListAccess(list, index) =>
        val listVal = evalRec(list, env).asList
        val indexVal = evalRec(index, env).asInt
        if (indexVal < 0 || indexVal >= listVal.length) {
          throw new RuntimeException(s"List index out of bounds: $indexVal")
        }
        listVal(indexVal)
      // List length: return the length of the list
      case Exp.ListLength(list) =>
        IntValue(evalRec(list, env).asList.length)
      // List append: create a new list with element appended
      case Exp.ListAppend(list, element) =>
        val listVal = evalRec(list, env).asList
        val elemVal = evalRec(element, env)
        ListValue(listVal :+ elemVal)
      // String length: return the length of the string
      case Exp.StringLength(str) =>
        IntValue(evalRec(str, env).asString.length)
      // String concatenation: concatenate two strings
      case Exp.StringConcat(lhs, rhs) =>
        StringValue(evalRec(lhs, env).asString + evalRec(rhs, env).asString)
      // Dictionary literal: evaluate all key-value pairs
      case Exp.VDict(entries) =>
        val evaluatedEntries = entries.map { case (k, v) =>
          (evalRec(k, env), evalRec(v, env))
        }
        DictValue(evaluatedEntries.toMap)
      // Dictionary access: get value for key
      case Exp.DictAccess(dict, key) =>
        val dictVal = evalRec(dict, env).asDict
        val keyVal = evalRec(key, env)
        dictVal.getOrElse(keyVal, throw new RuntimeException(s"Key not found in dictionary: $keyVal"))
      // Dictionary set: create new dictionary with updated key-value pair
      case Exp.DictSet(dict, key, value) =>
        val dictVal = evalRec(dict, env).asDict
        val keyVal = evalRec(key, env)
        val valueVal = evalRec(value, env)
        DictValue(dictVal + (keyVal -> valueVal))
      // Dictionary keys: return list of all keys
      case Exp.DictKeys(dict) =>
        ListValue(evalRec(dict, env).asDict.keys.toList)
      // Dictionary values: return list of all values
      case Exp.DictValues(dict) =>
        ListValue(evalRec(dict, env).asDict.values.toList)
      // Dictionary size: return the number of entries
      case Exp.DictSize(dict) =>
        IntValue(evalRec(dict, env).asDict.size)
      // Dictionary contains: check if key exists
      case Exp.DictContains(dict, key) =>
        val dictVal = evalRec(dict, env).asDict
        val keyVal = evalRec(key, env)
        IntValue(if (dictVal.contains(keyVal)) 1 else 0)
      // Let binding: evaluate value, bind to name, evaluate body
      case Exp.Let(name, value, body) =>
        val valueResult = evalRec(value, env)
        val oldValue = env.get(name)
        env(name) = valueResult
        val result = evalRec(body, env)
        // Restore old binding if any
        oldValue match {
          case Some(v) => env(name) = v
          case None => env.remove(name)
        }
        result
      // Lambda: create closure
      case Exp.Lambda(params, body) =>
        // Capture current environment
        val closure = env.clone()
        FunctionValue(params, body, closure)
      // Apply: evaluate function and apply to arguments
      case Exp.Apply(func, args) =>
        val funcVal = evalRec(func, env)
        funcVal match {
          case FunctionValue(params, body, closure) =>
            val argValues = args.map(evalRec(_, env))
            if (params.length != args.length) {
              throw new Exception(s"Function expects ${params.length} arguments but got ${args.length}")
            }
            // Create new environment from closure
            val newEnv = closure.clone()
            params.zip(argValues).foreach { case (param, argVal) =>
              newEnv(param) = argVal
            }
            evalRec(body, newEnv)
          case _ =>
            throw new Exception(s"Attempted to call non-function value: $funcVal")
        }
    }
  }
  evalRec(e, vEnv)
}