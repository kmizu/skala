package com.github.kmizu.tskala

import scala.collection.mutable.ListBuffer

// Token types
sealed trait Token
case class Number(value: Int) extends Token
case class StringLit(value: String) extends Token
case class Ident(value: String) extends Token
case class Keyword(value: String) extends Token
case class Symbol(value: String) extends Token
case object EOF extends Token

// Lexer for tokenization
object Lexer {
  private val keywords = Set("if", "else", "while", "function", "length", "append", "Int", "Bool", "String", "List")

  def tokenize(src: String): List[Token] = {
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
        case '"' =>
          i += 1 // skip opening quote
          var j = i
          val sb = new StringBuilder
          while (j < src.length && src(j) != '"') {
            if (src(j) == '\\' && j + 1 < src.length) {
              // Handle escape sequences
              j += 1
              src(j) match {
                case 'n' => sb.append('\n')
                case 't' => sb.append('\t')
                case 'r' => sb.append('\r')
                case '\\' => sb.append('\\')
                case '"' => sb.append('"')
                case c => sb.append(c) // Unknown escape, just add the character
              }
              j += 1
            } else {
              sb.append(src(j))
              j += 1
            }
          }
          if (j < src.length) j += 1 // skip closing quote
          tokens += StringLit(sb.toString)
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
        case ':' =>
          tokens += Symbol(":")
          i += 1
        case '+' =>
          if (i + 1 < src.length && src(i + 1) == '+') {
            tokens += Symbol("++")
            i += 2
          } else {
            tokens += Symbol("+")
            i += 1
          }
        case c @ ('-' | '*' | '/' | '(' | ')' | '{' | '}' | ';' | ',' | '[' | ']') =>
          tokens += Symbol(c.toString)
          i += 1
        case _ =>
          sys.error(s"Unknown character: ${src(i)}")
      }
    }
    tokens += EOF
    tokens.toList
  }
}

object Parser {
  case class ParseError(message: String) extends Exception(message)

  private class TokenStream(tokens: List[Token]) {
    private var pos = 0
    
    def peek: Token = if (pos < tokens.length) tokens(pos) else EOF
    def consume(): Token = { val t = peek; pos += 1; t }

    def expectSymbol(sym: String): Either[ParseError, Unit] = peek match {
      case Symbol(`sym`) => consume(); Right(())
      case t             => Left(ParseError(s"Expected symbol '$sym' but found $t"))
    }
    
    def expectKeyword(kw: String): Either[ParseError, Unit] = peek match {
      case Keyword(`kw`) => consume(); Right(())
      case t             => Left(ParseError(s"Expected keyword '$kw' but found $t"))
    }
    
    def consumeIdent(): Either[ParseError, String] = peek match {
      case Ident(name) => consume(); Right(name)
      case t           => Left(ParseError(s"Expected identifier but found $t"))
    }
    
    def consumeNumber(): Either[ParseError, Int] = peek match {
      case Number(v) => consume(); Right(v)
      case t         => Left(ParseError(s"Expected number but found $t"))
    }
    
    def consumeString(): Either[ParseError, String] = peek match {
      case StringLit(v) => consume(); Right(v)
      case t            => Left(ParseError(s"Expected string literal but found $t"))
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
    
    // Helper methods for backward compatibility
    def expectSymbolOrFail(sym: String): Unit = expectSymbol(sym) match {
      case Left(e) => throw e
      case Right(_) => ()
    }
    
    def expectKeywordOrFail(kw: String): Unit = expectKeyword(kw) match {
      case Left(e) => throw e 
      case Right(_) => ()
    }
    
    def consumeIdentOrFail(): String = consumeIdent() match {
      case Left(e) => throw e
      case Right(v) => v
    }
    
    def consumeNumberOrFail(): Int = consumeNumber() match {
      case Left(e) => throw e
      case Right(v) => v
    }
    
    def consumeStringOrFail(): String = consumeString() match {
      case Left(e) => throw e
      case Right(v) => v
    }
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
      val name = ts.consumeIdentOrFail()
      ts.expectSymbolOrFail("(")
      val params = ListBuffer[(String, Type)]()
      if (!ts.acceptSymbol(")")) {
        // Parse typed parameters: name : Type
        params += parseTypedParam()
        while (ts.acceptSymbol(",")) {
          params += parseTypedParam()
        }
        ts.expectSymbolOrFail(")")
      }
      ts.expectSymbolOrFail(":")
      val returnType = parseType()
      val body = parseBlockOrExp()
      Func(name, params.toList, returnType, body)
    }
    
    private def parseTypedParam(): (String, Type) = {
      val name = ts.consumeIdentOrFail()
      ts.expectSymbolOrFail(":")
      val typ = parseType()
      (name, typ)
    }
    
    private def parseType(): Type = {
      ts.peek match {
        case Keyword("Int") =>
          ts.consume()
          Type.TInt
        case Keyword("Bool") =>
          ts.consume()
          Type.TBool
        case Keyword("String") =>
          ts.consume()
          Type.TString
        case Keyword("List") =>
          ts.consume()
          ts.expectSymbolOrFail("[")
          val elemType = parseType()
          ts.expectSymbolOrFail("]")
          Type.TList(elemType)
        case t =>
          throw ParseError(s"Expected type but found $t")
      }
    }

    def parseExpression(): Exp = parseAssignment()

    private def parseAssignment(): Exp = {
      val e = parseOr()
      if (ts.acceptSymbol("=")) {
        e match {
          case Exp.Ident(name) =>
            val rhs = parseAssignment()
            Exp.Assignment(name, rhs)
          case _ =>
            sys.error("Left-hand side of assignment must be an identifier")
        }
      } else {
        e
      }
    }

    private def parseOr(): Exp = parseComparison()

    private def parseComparison(): Exp = {
      var e = parseAdditive()
      while (ts.peek match {
          case Symbol("<") | Symbol(">") | Symbol("<=") | Symbol(">=") | Symbol("==") | Symbol("!=") => true
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
          case Symbol("+") | Symbol("-") | Symbol("++") => true
          case _ => false
        }) {
        val Symbol(op) = ts.consume().asInstanceOf[Symbol]
        val rhs = parseMultiplicative()
        e = op match {
          case "++" => Exp.StringConcat(e, rhs)
          case _ => Exp.BinExp(op, e, rhs)
        }
      }
      e
    }

    private def parseMultiplicative(): Exp = {
      var e = parsePostfix()
      while (ts.peek match {
          case Symbol("*") | Symbol("/") => true
          case _ => false
        }) {
        val Symbol(op) = ts.consume().asInstanceOf[Symbol]
        val rhs = parsePostfix()
        e = Exp.BinExp(op, e, rhs)
      }
      e
    }

    private def parsePostfix(): Exp = {
      var e = parsePrimary()
      var done = false
      while (!done) {
        ts.peek match {
          case Symbol("[") =>
            ts.consume()
            val index = parseExpression()
            ts.expectSymbolOrFail("]")
            e = Exp.ListAccess(e, index)
          case _ =>
            done = true
        }
      }
      e
    }

    private def parsePrimary(): Exp = ts.peek match {
      case Number(v) => ts.consume(); Exp.VInt(v)
      case StringLit(v) => ts.consume(); Exp.VString(v)
      case Ident(name) =>
        ts.consume()
        if (ts.acceptSymbol("(")) {
          val args = ListBuffer[Exp]()
          if (!ts.acceptSymbol(")")) {
            args += parseExpression()
            while (ts.acceptSymbol(",")) {
              args += parseExpression()
            }
            ts.expectSymbolOrFail(")")
          }
          Exp.Call(name, args.toList)
        } else {
          Exp.Ident(name)
        }
      case Symbol("[") =>
        ts.consume()
        val elements = ListBuffer[Exp]()
        if (!ts.acceptSymbol("]")) {
          elements += parseExpression()
          while (ts.acceptSymbol(",")) {
            elements += parseExpression()
          }
          ts.expectSymbolOrFail("]")
        }
        Exp.VList(elements.toList)
      case Symbol("(") =>
        ts.consume()
        val e = parseExpression()
        ts.expectSymbolOrFail(")")
        e
      case Symbol("{") =>
        parseBlock()
      case Keyword("if") =>
        ts.consume()
        ts.expectSymbolOrFail("(")
        val cond = parseExpression()
        ts.expectSymbolOrFail(")")
        val thenClause = parseBlockOrExp()
        ts.expectKeywordOrFail("else")
        val elseClause = parseBlockOrExp()
        Exp.If(cond, thenClause, elseClause)
      case Keyword("while") =>
        ts.consume()
        ts.expectSymbolOrFail("(")
        val cond = parseExpression()
        ts.expectSymbolOrFail(")")
        val bodies = parseBlock() match {
          case Exp.SeqExp(bs) => bs
          case other          => List(other)
        }
        Exp.While(cond, bodies)
      case Keyword("length") =>
        ts.consume()
        ts.expectSymbolOrFail("(")
        val list = parseExpression()
        ts.expectSymbolOrFail(")")
        Exp.ListLength(list)
      case Keyword("append") =>
        ts.consume()
        ts.expectSymbolOrFail("(")
        val list = parseExpression()
        ts.expectSymbolOrFail(",")
        val element = parseExpression()
        ts.expectSymbolOrFail(")")
        Exp.ListAppend(list, element)
      case t =>
        sys.error(s"Unexpected token: $t")
    }

    private def parseBlock(): Exp = {
      ts.expectSymbolOrFail("{")
      val exprs = ListBuffer[Exp]()
      while (!ts.acceptSymbol("}")) {
        exprs += parseExpression()
        ts.acceptSymbol(";")
      }
      Exp.SeqExp(exprs.toList)
    }

    private def parseBlockOrExp(): Exp = ts.peek match {
      case Symbol("{") => 
        val block = parseBlock()
        // If block contains only one expression, unwrap it
        block match {
          case Exp.SeqExp(List(single)) => single
          case other => other
        }
      case _ => parseExpression()
    }
  }

  def parseProgram(src: String): Program = {
    val p = new ParserImpl(Lexer.tokenize(src))
    p.parseProgram()
  }

  def parseExp(src: String): Exp = {
    val p = new ParserImpl(Lexer.tokenize(src))
    p.parseExpression()
  }
}