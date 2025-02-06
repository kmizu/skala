package com.github.kmizu.math

def eval(expr: Exp): Int = expr match {
  case Exp.VInt(value) =>
    value
  case Exp.BinExp(op, lhs, rhs) =>
    op match {
      case "+" => eval(lhs) + eval(rhs)
      case "-" => eval(lhs) - eval(rhs)
      case "*" => eval(lhs) * eval(rhs)
      case "/" => eval(lhs) / eval(rhs)
      case _   => throw new AssertionError(s"Unknown operator: $op")
    }
}