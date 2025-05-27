package com.github.kmizu.tskala

import scala.collection.mutable

enum Value {
  case IntValue(value: Int)
  case StringValue(value: String)
  case ListValue(elements: List[Value])
  case DictValue(entries: Map[Value, Value])
  case FunctionValue(params: List[String], body: Exp, closure: mutable.Map[String, Value])
  
  def asInt: Int = this match {
    case IntValue(v) => v
    case _ => throw new RuntimeException(s"Expected Int but got $this")
  }
  
  def asString: String = this match {
    case StringValue(v) => v
    case _ => throw new RuntimeException(s"Expected String but got $this")
  }
  
  def asList: List[Value] = this match {
    case ListValue(elements) => elements
    case _ => throw new RuntimeException(s"Expected List but got $this")
  }
  
  def asDict: Map[Value, Value] = this match {
    case DictValue(entries) => entries
    case _ => throw new RuntimeException(s"Expected Dict but got $this")
  }
  
  def asFunction: (List[String], Exp, mutable.Map[String, Value]) = this match {
    case FunctionValue(params, body, closure) => (params, body, closure)
    case _ => throw new RuntimeException(s"Expected Function but got $this")
  }
  
  def toBool: Boolean = this match {
    case IntValue(v) => v != 0
    case StringValue(v) => v.nonEmpty
    case ListValue(elements) => elements.nonEmpty
    case DictValue(entries) => entries.nonEmpty
    case FunctionValue(_, _, _) => true
  }
}