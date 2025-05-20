package com.github.kmizu.skala

import scala.collection.mutable.ListBuffer

object Parser {
  sealed trait Token
  case class Number(value: Int) extends Token
  case class Ident(value: String) extends Token
  case class Keyword(value: String) extends Token
  case class Symbol(value: String) extends Token
  case object EOF extends Token

  private val keywords = Set("if", "else", "while", "function")

  private def tokenize(src: String): List[Token] = {
    val tokens = ListBuffer[Token]()
    var i = 0
    while (i < src.length) {
      src(i) match {
        case c if c.isWhitespace =>
          i += 1
        case c if c.isDigit =>
          var j = i
          while (j < src.length && src(j).isDigit) j += 1
          tokens += Number(src.substring(i, j).toInt)
          i = j
        case c if c.isLetter || c == '_' =>
          var j = i
          while (j < src.length && (src(j).isLetterOrDigit || src(j) == '_')) j += 1
          val word = src.substring(i, j)
          if (keywords(word)) tokens += Keyword(word) else tokens += Ident(word)
          i = j
        case '=' =>
          if (i + 1 < src.length && src(i + 1) == '=') {
            tokens += Symbol("==")
            i += 2
          } else {
            tokens += Symbol("=")
            i += 1
          }
        case '!' if i + 1 < src.length && src(i + 1) == '=' =>
          tokens += Symbol("!=")
          i += 2
        case '<' =>
          if (i + 1 < src.length && src(i + 1) == '=') {
            tokens += Symbol("<=")
            i += 2
          } else {
            tokens += Symbol("<")
            i += 1
          }
        case '>' =>
          if (i + 1 < src.length && src(i + 1) == '=') {
            tokens += Symbol(">=")
            i += 2
          } else {
            tokens += Symbol(">")
            i += 1
          }
        case c @ ('+' | '-' | '*' | '/' | '(' | ')' | '{' | '}' | ';' | ',') =>
          tokens += Symbol(c.toString)
          i += 1
        case _ =>
          sys.error(s"Unknown character: ${src(i)}")
      }
    }
    tokens += EOF
    tokens.toList
  }

  private class TokenStream(tokens: List[Token]) {
    private var pos = 0
    def peek: Token = if (pos < tokens.length) tokens(pos) else EOF
    def consume(): Token = { val t = peek; pos += 1; t }

    def expectSymbol(sym: String): Unit = peek match {
      case Symbol(`sym`) => consume()
      case t             => sys.error(s"Expected symbol '$sym' but found $t")
    }
    def expectKeyword(kw: String): Unit = peek match {
      case Keyword(`kw`) => consume()
      case t             => sys.error(s"Expected keyword '$kw' but found $t")
    }
    def consumeIdent(): String = peek match {
      case Ident(name) => consume(); name
      case t           => sys.error(s"Expected identifier but found $t")
    }
    def consumeNumber(): Int = peek match {
      case Number(v) => consume(); v
      case t         => sys.error(s"Expected number but found $t")
    }
    def acceptSymbol(sym: String): Boolean = peek match {
      case Symbol(`sym`) => consume(); true
      case _             => false
    }
    def acceptKeyword(kw: String): Boolean = peek match {
      case Keyword(`kw`) => consume(); true
      case _             => false
    }
    def isEOF: Boolean = peek == EOF
  }

  private class ParserImpl(tokens: List[Token]) {
    private val ts = new TokenStream(tokens)

    def parseProgram(): Program = {
      val funcs = ListBuffer[Func]()
      while (ts.acceptKeyword("function")) {
        funcs += parseFunctionBody()
      }
      val bodies = ListBuffer[Exp]()
      while (!ts.isEOF) {
        bodies += parseExpression()
        ts.acceptSymbol(";")
      }
      Program(funcs.toList, bodies.toList)
    }

    private def parseFunctionBody(): Func = {
      val name = ts.consumeIdent()
      ts.expectSymbol("(")
      val params = ListBuffer[String]()
      if (!ts.acceptSymbol(")")) {
        params += ts.consumeIdent()
        while (ts.acceptSymbol(",")) {
          params += ts.consumeIdent()
        }
        ts.expectSymbol(")")
      }
      val body = parseBlockOrExp()
      Func(name, params.toList, body)
    }

    def parseExpression(): Exp = parseAssignment()

    private def parseAssignment(): Exp = {
      val lhs = parseComparison()
      lhs match {
        case Exp.Ident(name) if ts.acceptSymbol("=") =>
          val rhs = parseExpression()
          Exp.Assignment(name, rhs)
        case _ => lhs
      }
    }

    private def parseComparison(): Exp = {
      var e = parseAdditive()
      while (ts.peek match {
          case Symbol("<" | ">" | "<=" | ">=" | "==" | "!=") => true
          case _ => false
        }) {
        val Symbol(op) = ts.consume().asInstanceOf[Symbol]
        val rhs = parseAdditive()
        e = Exp.BinExp(op, e, rhs)
      }
      e
    }

    private def parseAdditive(): Exp = {
      var e = parseMultiplicative()
      while (ts.peek match {
          case Symbol("+") | Symbol("-") => true
          case _ => false
        }) {
        val Symbol(op) = ts.consume().asInstanceOf[Symbol]
        val rhs = parseMultiplicative()
        e = Exp.BinExp(op, e, rhs)
      }
      e
    }

    private def parseMultiplicative(): Exp = {
      var e = parsePrimary()
      while (ts.peek match {
          case Symbol("*") | Symbol("/") => true
          case _ => false
        }) {
        val Symbol(op) = ts.consume().asInstanceOf[Symbol]
        val rhs = parsePrimary()
        e = Exp.BinExp(op, e, rhs)
      }
      e
    }

    private def parsePrimary(): Exp = ts.peek match {
      case Number(v) => ts.consume(); Exp.VInt(v)
      case Ident(name) =>
        ts.consume()
        if (ts.acceptSymbol("(")) {
          val args = ListBuffer[Exp]()
          if (!ts.acceptSymbol(")")) {
            args += parseExpression()
            while (ts.acceptSymbol(",")) {
              args += parseExpression()
            }
            ts.expectSymbol(")")
          }
          Exp.Call(name, args.toList)
        } else {
          Exp.Ident(name)
        }
      case Symbol("(") =>
        ts.consume()
        val e = parseExpression()
        ts.expectSymbol(")")
        e
      case Symbol("{") =>
        parseBlock()
      case Keyword("if") =>
        ts.consume()
        ts.expectSymbol("(")
        val cond = parseExpression()
        ts.expectSymbol(")")
        val thenClause = parseBlockOrExp()
        ts.expectKeyword("else")
        val elseClause = parseBlockOrExp()
        Exp.If(cond, thenClause, elseClause)
      case Keyword("while") =>
        ts.consume()
        ts.expectSymbol("(")
        val cond = parseExpression()
        ts.expectSymbol(")")
        val bodies = parseBlock() match {
          case Exp.SeqExp(bs) => bs
          case other          => List(other)
        }
        Exp.While(cond, bodies)
      case t =>
        sys.error(s"Unexpected token: $t")
    }

    private def parseBlock(): Exp = {
      ts.expectSymbol("{")
      val exprs = ListBuffer[Exp]()
      while (!ts.acceptSymbol("}")) {
        exprs += parseExpression()
        ts.acceptSymbol(";")
      }
      Exp.SeqExp(exprs.toList)
    }

    private def parseBlockOrExp(): Exp = ts.peek match {
      case Symbol("{") => parseBlock()
      case _            => parseExpression()
    }
  }

  def parseProgram(src: String): Program = {
    val p = new ParserImpl(tokenize(src))
    p.parseProgram()
  }

  def parseExp(src: String): Exp = {
    val p = new ParserImpl(tokenize(src))
    p.parseExpression()
  }
}
