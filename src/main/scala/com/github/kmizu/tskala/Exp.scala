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
  case Apply(func: Exp, args: List[Exp])
  case Assignment(name: String, expression: Exp)
  case Let(name: String, value: Exp, body: Exp)
  case Lambda(params: List[String], body: Exp)
  case VInt(value: Int)
  case VString(value: String)
  case VList(elements: List[Exp])
  case ListAccess(list: Exp, index: Exp)
  case ListLength(list: Exp)
  case ListAppend(list: Exp, element: Exp)
  case StringLength(str: Exp)
  case StringConcat(lhs: Exp, rhs: Exp)
  case VDict(entries: List[(Exp, Exp)])
  case DictAccess(dict: Exp, key: Exp)
  case DictSet(dict: Exp, key: Exp, value: Exp)
  case DictKeys(dict: Exp)
  case DictValues(dict: Exp)
  case DictSize(dict: Exp)
  case DictContains(dict: Exp, key: Exp)
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
def tList(elements: Exp*): Exp = VList(elements.toList)
def tListAccess(list: Exp, index: Exp): Exp = ListAccess(list, index)
def tListLength(list: Exp): Exp = ListLength(list)
def tListAppend(list: Exp, element: Exp): Exp = ListAppend(list, element)
def tString(value: String): Exp = VString(value)
def tStringLength(str: Exp): Exp = StringLength(str)
def tStringConcat(lhs: Exp, rhs: Exp): Exp = StringConcat(lhs, rhs)
def tDict(entries: (Exp, Exp)*): Exp = VDict(entries.toList)
def tDictAccess(dict: Exp, key: Exp): Exp = DictAccess(dict, key)
def tDictSet(dict: Exp, key: Exp, value: Exp): Exp = DictSet(dict, key, value)
def tDictKeys(dict: Exp): Exp = DictKeys(dict)
def tDictValues(dict: Exp): Exp = DictValues(dict)
def tDictSize(dict: Exp): Exp = DictSize(dict)
def tDictContains(dict: Exp, key: Exp): Exp = DictContains(dict, key)
def tLet(name: String, value: Exp, body: Exp): Exp = Let(name, value, body)
def tLambda(params: List[String], body: Exp): Exp = Lambda(params, body)
def tApply(func: Exp, args: Exp*): Exp = Apply(func, args.toList)
