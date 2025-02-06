package com.github.kmizu.math

import play.api.libs.json._

/** JsValue を再帰的に Exp に変換する関数
  *
  * - JsNumber の場合は整数リテラルに変換します。
  * - JsArray で要素数が 3 ならば、最初の要素を演算子、
  *   2 番目と 3 番目の要素を左右の式として再帰的に変換します。
  * - それ以外の場合は例外をスローします。
  */
def translateToAst(json: JsValue): Exp = json match {
  case JsNumber(n) =>
    tInt(n.toInt)
  case JsArray(arr) if arr.size == 3 =>
    val op = arr(0).asInstanceOf[JsString].value
    val lhs = translateToAst(arr(1))
    val rhs = translateToAst(arr(2))
    Exp.BinExp(op, lhs, rhs)
}

/** JSON 文字列をパースし AST に変換、その後評価して結果を返します。 */
def evalJson(jsonString: String): Int = {
  val json: JsValue = Json.parse(jsonString)
  val ast: Exp   = translateToAst(json)
  eval(ast)
}