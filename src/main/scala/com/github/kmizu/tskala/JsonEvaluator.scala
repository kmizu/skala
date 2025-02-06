package com.github.kmizu.tskala
import play.api.libs.json._
import scala.collection.mutable

/** Translates a Play JSON value into an AST node.
 *
 * This function follows the convention:
 *   - If the JSON is an array then the first element (a string) indicates the operator.
 *   - Otherwise, if the JSON is a number, it is translated to an integer literal.
 *   - For unknown cases an exception is thrown.
 */
def translateToAst(json: JsValue): Exp = json match {
  case JsArray(values) if values.nonEmpty =>
    values.head match {
      case JsString(op) =>
        op match
          case "+" =>
            translateToAst(values(1)) |+| translateToAst(values(2))
          case "-" =>
            translateToAst(values(1)) |-| translateToAst(values(2))
          case "*" =>
            translateToAst(values(1)) |*| translateToAst(values(2))
          case "/" =>
            translateToAst(values(1)) |/| translateToAst(values(2))
          case "<" =>
            translateToAst(values(1)) |<| translateToAst(values(2))
          case ">" =>
            translateToAst(values(1)) |>| translateToAst(values(2))
          case "<=" =>
            translateToAst(values(1)) |<=| translateToAst(values(2))
          case ">=" =>
            translateToAst(values(1)) |>=| translateToAst(values(2))
          case "==" =>
            translateToAst(values(1)) |==| translateToAst(values(2))
          case "!=" =>
            translateToAst(values(1)) |!=| translateToAst(values(2))
          case "seq" =>
            // Translate all elements after the operator.
            tSeq(values.tail.toSeq.map(translateToAst)*)
          case "if" =>
            // Expecting: ["if", condition, thenClause, elseClause]
            tIf(translateToAst(values(1)), translateToAst(values(2)), translateToAst(values(3)))
          case "while" =>
            // Expecting: ["while", condition, body]
            tWhile(translateToAst(values(1)), translateToAst(values(2)))
          case "assign" =>
            // Expecting: ["assign", name, expression]
            // Here the name is given as a JSON string.
            tAssign(values(1).as[String], translateToAst(values(2)))
          case "id" =>
            // Expecting: ["id", name]
            tId(values(1).as[String])
          case "call" =>
            // Expecting: ["call", functionName, arg1, arg2, ...]
            tCall(values(1).as[String], values.drop(2).toSeq.map(translateToAst)*)
          case other =>
            throw new Exception(s"Unknown operator: $other")
      case _ =>
        throw new Exception("The first element of the array must be a string operator")
    }
  case JsNumber(n) =>
    tInt(n.toInt)
  case _ =>
    throw new Exception("Not implemented for: " + Json.stringify(json))
}

/** Evaluates a JSON string representing a single expression.
  *
  * This function parses the JSON string, translates it into an AST, and then evaluates it.
  */
def evalJsonExp(jsonString: String): Int = {
  val json = Json.parse(jsonString)
  val ast  = translateToAst(json)
  evalExp(ast)
}

/** Evaluates a JSON string representing a complete program.
  *
  * The JSON program is assumed to be an array where each element is either a function definition or
  * a body expression. A function definition is represented as an array whose first element is "def".
  */
def evalJsonProgram(jsonString: String): Any =
  val json = Json.parse(jsonString)
  json match {
    case JsArray(elems) =>
      // Partition definitions (whose first element is "def") from other expressions.
      val (defs, bodies) = elems.partition {
        case JsArray(values) =>
          values.headOption match
            case Some(JsString("def")) => true
            case _                     => false
        case _ => false
      }
      val fEnv = mutable.Map[String, Func]()
      // Process function definitions.
      defs.foreach {
        case JsArray(values) if values.size >= 4 =>
          // Expected format: ["def", name, [params...], returnType, body]
          val name   = values(1).as[String]
          val params = values(2) match {
            case JsArray(ps) => 
              ps.map{p => 
                val vs = p.as[JsArray].value
                vs(0).as[String] -> Type.valueOf(vs(1).as[String])
              }.toList
            case _ => throw new Exception("Function parameters must be an array")
          }
          val returnType = Type.valueOf(values(3).as[String] )
          val body = translateToAst(values(4))
          fEnv(name) = tFunction(name, params, returnType, body)
        case _ =>
          throw new Exception("Invalid function definition format")
      }
      val vEnv = mutable.Map[String, Int]()
      // Evaluate the bodies sequentially.
      var result: Any = 0
      bodies.foreach { jsVal =>
        val ast = translateToAst(jsVal)
        result = eval(ast, vEnv, fEnv)
      }
      result
    case _ =>
      throw new Exception("Program must be represented as a JSON array")
}