package com.github.kmizu.skala
import play.api.libs.json._
import scala.collection.mutable
import Value.*

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
        op match {
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
          case "list" =>
            // Expecting: ["list", elem1, elem2, ...]
            tList(values.tail.toSeq.map(translateToAst)*)
          case "list-access" =>
            // Expecting: ["list-access", list, index]
            tListAccess(translateToAst(values(1)), translateToAst(values(2)))
          case "list-length" =>
            // Expecting: ["list-length", list]
            tListLength(translateToAst(values(1)))
          case "list-append" =>
            // Expecting: ["list-append", list, element]
            tListAppend(translateToAst(values(1)), translateToAst(values(2)))
          case "string" =>
            // Expecting: ["string", "value"]
            tString(values(1).as[String])
          case "string-length" =>
            // Expecting: ["string-length", str]
            tStringLength(translateToAst(values(1)))
          case "string-concat" | "++" =>
            // Expecting: ["string-concat", str1, str2] or ["++", str1, str2]
            tStringConcat(translateToAst(values(1)), translateToAst(values(2)))
          case "dict" =>
            // Expecting: ["dict", key1, value1, key2, value2, ...]
            val kvPairs = values.tail.toList
            if (kvPairs.length % 2 != 0) {
              throw new Exception("Dict must have even number of key-value arguments")
            }
            val entries = kvPairs.grouped(2).map { group =>
              group match {
                case List(k: JsValue, v: JsValue) => (translateToAst(k), translateToAst(v))
                case _ => throw new Exception("Invalid dict entry")
              }
            }.toSeq
            tDict(entries*)
          case "dict-access" =>
            // Expecting: ["dict-access", dict, key]
            tDictAccess(translateToAst(values(1)), translateToAst(values(2)))
          case "dict-set" =>
            // Expecting: ["dict-set", dict, key, value]
            tDictSet(translateToAst(values(1)), translateToAst(values(2)), translateToAst(values(3)))
          case "dict-keys" =>
            // Expecting: ["dict-keys", dict]
            tDictKeys(translateToAst(values(1)))
          case "dict-values" =>
            // Expecting: ["dict-values", dict]
            tDictValues(translateToAst(values(1)))
          case "dict-size" =>
            // Expecting: ["dict-size", dict]
            tDictSize(translateToAst(values(1)))
          case "dict-contains" =>
            // Expecting: ["dict-contains", dict, key]
            tDictContains(translateToAst(values(1)), translateToAst(values(2)))
          case other =>
            throw new Exception(s"Unknown operator: $other")
        }
      case _ =>
        throw new Exception("The first element of the array must be a string operator")
    }
  case JsNumber(n) =>
    tInt(n.toInt)
  case JsString(s) =>
    tString(s)
  case _ =>
    throw new Exception("Not implemented for: " + Json.stringify(json))
}

/** Evaluates a JSON string representing a complete program.
  *
  * The JSON program is assumed to be an array where each element is either a function definition or
  * a body expression. A function definition is represented as an array whose first element is "def".
  */
def evalJsonProgramInt(jsonString: String): Int = {
  evalJsonProgram(jsonString).asInt
}

def evalJsonProgram(jsonString: String): Value = {
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
          // Expected format: ["def", name, [params...], body]
          val name   = values(1).as[String]
          val params = values(2) match
            case JsArray(ps) => ps.map(_.as[String]).toList
            case _ => throw new Exception("Function parameters must be an array")
          val body = translateToAst(values(3))
          fEnv(name) = tFunction(name, params, body)
        case _ =>
          throw new Exception("Invalid function definition format")
      }
      val vEnv = mutable.Map[String, Value]()
      // Evaluate the bodies sequentially.
      var result: Value = IntValue(0)
      bodies.foreach { jsVal =>
        val ast = translateToAst(jsVal)
        result = eval(ast, vEnv, fEnv)
      }
      result
    case _ =>
    throw new Exception("Program must be represented as a JSON array")
  }
}

/** Evaluates a JSON string representing a single expression or statement.
  *
  * This function parses the JSON string, translates it into an AST, and then evaluates it.
  */
def evalJsonExpInt(jsonString: String): Int = {
  evalJsonExp(jsonString).asInt
}

def evalJsonExp(jsonString: String): Value = {
  val json = Json.parse(jsonString)
  val ast  = translateToAst(json)
  evalExp(ast)
}