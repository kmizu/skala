package com.github.kmizu.skala

enum Value {
  case IntValue(value: Int)
  case StringValue(value: String)
  case ListValue(elements: List[Value])
  case DictValue(entries: Map[Value, Value])
  
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
  
  def toBool: Boolean = this match {
    case IntValue(v) => v != 0
    case StringValue(v) => v.nonEmpty
    case ListValue(elements) => elements.nonEmpty
    case DictValue(entries) => entries.nonEmpty
  }
}