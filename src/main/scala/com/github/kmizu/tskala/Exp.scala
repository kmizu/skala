package com.github.kmizu.tskala
import scala.collection.mutable

case class Program(functions: List[Func], bodies: List[Exp])
case class Func(name: String, params: List[(String, Type)], returnType: Type, body: Exp)
enum Exp {
  // Expession nodes
  case BinExp(op: String, lhs: Exp, rhs: Exp)
  case If(condition: Exp, thenClause: Exp, elseClause: Exp)
  case SeqExp(bodies: List[Exp])
  case While(condition: Exp, bodies: List[Exp])
  case Call(name: String, args: List[Exp])
  case Assignment(name: String, expression: Exp)
  case VInt(value: Int)
  case Ident(name: String)
}
import Exp.*
def tProgram(functions: List[Func], bodies: Exp*): Program = {
  Program(functions, bodies.toList)
}

def tFunction(name: String, params: List[(String, Type)], returnType: Type, body: Exp): Func = {
  Func(name, params, returnType, body)
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
