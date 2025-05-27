package com.github.kmizu.skala
import scala.collection.mutable
import scala.collection.mutable.{Map => MMap}
import Value.*

def evalProgramInt(program: Program): Int = {
  evalProgram(program).asInt
}

def evalProgram(program: Program): Value = {
  // Use a mutable map as our environment.
  val varEnv = mutable.Map[String, Value]()
  val funcEnv = mutable.Map[String, Func]()
  val functions = program.functions
  val bodies = program.bodies
  // Store functions in the environment.
  functions.foreach {
    case f @ Func(name, _, _) =>
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
  eval(e, mutable.Map.empty, mutable.Map.empty)
}

/**
 * Evaluate an expression (or statement) given the current environment.
 *
 * The environment maps names to values, where values can be integers or functions.
 */
import scala.collection.mutable.{Map => MMap}
def eval(e: Exp, varEnv: MMap[String, Value], funcEnv: MMap[String, Func]): Value = {
  def evalRec(e: Exp): Value = {
    e match {
    // Binary expressions: arithmetic and comparisons.
      case Exp.BinExp("+", lhs, rhs) =>
        // Check if either operand is a string for string concatenation
        (evalRec(lhs), evalRec(rhs)) match {
          case (StringValue(s1), StringValue(s2)) => StringValue(s1 + s2)
          case (StringValue(s), IntValue(i)) => StringValue(s + i)
          case (IntValue(i), StringValue(s)) => StringValue(i + s)
          case (l, r) => IntValue(l.asInt + r.asInt)
        }
      case Exp.BinExp("-", lhs, rhs) =>
        IntValue(evalRec(lhs).asInt - evalRec(rhs).asInt)
      case Exp.BinExp("*", lhs, rhs) =>
        IntValue(evalRec(lhs).asInt * evalRec(rhs).asInt)
      case Exp.BinExp("/", lhs, rhs) =>
        IntValue(evalRec(lhs).asInt / evalRec(rhs).asInt)
      case Exp.BinExp("<", lhs, rhs) =>
        IntValue(if (evalRec(lhs).asInt < evalRec(rhs).asInt) 1 else 0)
      case Exp.BinExp(">", lhs, rhs) =>
        IntValue(if (evalRec(lhs).asInt > evalRec(rhs).asInt) 1 else 0)
      case Exp.BinExp("<=", lhs, rhs) =>
        IntValue(if (evalRec(lhs).asInt <= evalRec(rhs).asInt) 1 else 0)
      case Exp.BinExp(">=", lhs, rhs) =>
        IntValue(if (evalRec(lhs).asInt >= evalRec(rhs).asInt) 1 else 0)
      case Exp.BinExp("==", lhs, rhs) =>
        IntValue(if (evalRec(lhs) == evalRec(rhs)) 1 else 0)
      case Exp.BinExp("!=", lhs, rhs) =>
        IntValue(if (evalRec(lhs) != evalRec(rhs)) 1 else 0)
      case Exp.BinExp(op, lhs, rhs) =>
        sys.error(s"Unknown operator $op")
      // Sequence: evaluate each expression in order and return the result of the last one.
      case Exp.SeqExp(bodies) =>
        var result: Value = IntValue(0)
        bodies.foreach { expr =>
          result = evalRec(expr)
        }
        result
      // If expressions: evaluate the condition, then one branch or the other.
      case Exp.If(condition, thenClause, elseClause) =>
        if (evalRec(condition).toBool)
          evalRec(thenClause)
        else
          evalRec(elseClause)
      // While loop: evaluate condition and repeatedly evaluate the bodies until false.
      case Exp.While(condition, bodies) =>
        while (evalRec(condition).toBool) {
          bodies.foreach(body => evalRec(body))
        }
        IntValue(0)
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
            val newEnv = mutable.Map[String, Value]() ++ varEnv
            params.zip(argValues).foreach { case (param, argVal) =>
              newEnv(param) = argVal
            }
            eval(body, newEnv, funcEnv)
          case None =>
            throw new Exception(s"Function $name is not defined")
        }
      // Integer literal: return its value.
      case Exp.VInt(value) =>
        IntValue(value)
      // String literal: return its value.
      case Exp.VString(value) =>
        StringValue(value)
      // List literal: evaluate all elements
      case Exp.VList(elements) =>
        ListValue(elements.map(evalRec(_)))
      // List access: get element at index
      case Exp.ListAccess(list, index) =>
        val listVal = evalRec(list).asList
        val indexVal = evalRec(index).asInt
        if (indexVal < 0 || indexVal >= listVal.length) {
          throw new RuntimeException(s"List index out of bounds: $indexVal")
        }
        listVal(indexVal)
      // List length: return the length of the list
      case Exp.ListLength(list) =>
        IntValue(evalRec(list).asList.length)
      // List append: create a new list with element appended
      case Exp.ListAppend(list, element) =>
        val listVal = evalRec(list).asList
        val elemVal = evalRec(element)
        ListValue(listVal :+ elemVal)
      // String length: return the length of the string
      case Exp.StringLength(str) =>
        IntValue(evalRec(str).asString.length)
      // String concatenation
      case Exp.StringConcat(lhs, rhs) =>
        StringValue(evalRec(lhs).asString + evalRec(rhs).asString)
      // Dictionary literal: evaluate all key-value pairs
      case Exp.VDict(entries) =>
        val evaluatedEntries = entries.map { case (k, v) =>
          (evalRec(k), evalRec(v))
        }
        DictValue(evaluatedEntries.toMap)
      // Dictionary access: get value for key
      case Exp.DictAccess(dict, key) =>
        val dictVal = evalRec(dict).asDict
        val keyVal = evalRec(key)
        dictVal.getOrElse(keyVal, throw new RuntimeException(s"Key not found in dictionary: $keyVal"))
      // Dictionary set: create new dictionary with updated key-value pair
      case Exp.DictSet(dict, key, value) =>
        val dictVal = evalRec(dict).asDict
        val keyVal = evalRec(key)
        val valueVal = evalRec(value)
        DictValue(dictVal + (keyVal -> valueVal))
      // Dictionary keys: return list of all keys
      case Exp.DictKeys(dict) =>
        ListValue(evalRec(dict).asDict.keys.toList)
      // Dictionary values: return list of all values
      case Exp.DictValues(dict) =>
        ListValue(evalRec(dict).asDict.values.toList)
      // Dictionary size: return the number of entries
      case Exp.DictSize(dict) =>
        IntValue(evalRec(dict).asDict.size)
      // Dictionary contains: check if key exists
      case Exp.DictContains(dict, key) =>
        val dictVal = evalRec(dict).asDict
        val keyVal = evalRec(key)
        IntValue(if (dictVal.contains(keyVal)) 1 else 0)
    }
  }
  evalRec(e)
}