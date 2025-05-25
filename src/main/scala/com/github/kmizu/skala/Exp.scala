package com.github.kmizu.skala
import scala.collection.mutable

// Top-level constructs
case class Program(functions: List[Func], bodies: List[Exp])
case class Func(name: String, params: List[String], body: Exp)
enum Exp {
  // Expession nodes
  case BinExp(op: String, lhs: Exp, rhs: Exp)
  case If(condition: Exp, thenClause: Exp, elseClause: Exp)
  case SeqExp(bodies: List[Exp])
  case While(condition: Exp, bodies: List[Exp])
  case Call(name: String, args: List[Exp])
  case Assignment(name: String, expression: Exp)
  case VInt(value: Int)
  case VString(value: String)
  case VList(elements: List[Exp])
  case ListAccess(list: Exp, index: Exp)
  case ListLength(list: Exp)
  case ListAppend(list: Exp, element: Exp)
  case StringLength(str: Exp)
  case StringConcat(lhs: Exp, rhs: Exp)
  case Ident(name: String)
}
extension (a: Exp) {
  def |+|(b: Exp): Exp = BinExp("+", a, b)
  def |-|(b: Exp): Exp = BinExp("-", a, b)
  def |*|(b: Exp): Exp = BinExp("*", a, b)
  def |/|(b: Exp): Exp = BinExp("/", a, b)
  def |<|(b: Exp): Exp = BinExp("<", a, b)
  def |>|(b: Exp): Exp = BinExp(">", a, b)
  def |<=|(b: Exp): Exp = BinExp("<=", a, b)
  def |>=|(b: Exp): Exp = BinExp(">=", a, b)
  def |==|(b: Exp): Exp = BinExp("==", a, b)
  def |!=|(b: Exp): Exp = BinExp("!=", a, b)
  def |++|(b: Exp): Exp = StringConcat(a, b)
}
import Exp.*
def tProgram(functions: List[Func], bodies: Exp*): Program = {
  Program(functions, bodies.toList)
}

def tFunction(name: String, params: List[String], body: Exp): Func = {
  Func(name, params, body)
}

def tInt(value: Int): Exp = VInt(value)
def tAssign(name: String, value: Exp): Exp = Assignment(name, value)
def tId(name: String): Exp = Ident(name)
def tSeq(expressions: Exp*): Exp = SeqExp(expressions.toList)
def tCall(name: String, args: Exp*): Exp = Call(name, args.toList)
def tWhile(condition: Exp, bodies: Exp*): Exp = While(condition, bodies.toList)
def tIf(condition: Exp, thenClause: Exp, elseClause: Exp): Exp = {
  If(condition, thenClause, elseClause)
}
def tList(elements: Exp*): Exp = VList(elements.toList)
def tListAccess(list: Exp, index: Exp): Exp = ListAccess(list, index)
def tListLength(list: Exp): Exp = ListLength(list)
def tListAppend(list: Exp, element: Exp): Exp = ListAppend(list, element)
def tString(value: String): Exp = VString(value)
def tStringLength(str: Exp): Exp = StringLength(str)
def tStringConcat(lhs: Exp, rhs: Exp): Exp = StringConcat(lhs, rhs)