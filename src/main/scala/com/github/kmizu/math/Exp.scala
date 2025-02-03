package com.github.kmizu.math

enum Exp {
  case BinExp(operator: String, lhs: Exp, rhs: Exp)
  case VInt(value: Int)
}
import Exp.*
def tInt(value: Int): Exp = VInt(value)
extension (a: Exp) {
  def |+|(b: Exp): Exp = BinExp("+", a, b)
  def |-|(b: Exp): Exp = BinExp("-", a, b)
  def |*|(b: Exp): Exp = BinExp("*", a, b)
  def |/|(b: Exp): Exp = BinExp("/", a, b)
}