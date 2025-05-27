package com.github.kmizu.skala

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
  private val keywords = Set("if", "else", "while", "function", "length", "append", "keys", "values", "size", "contains")

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
        case '+' =>
          if (i + 1 < src.length && src(i + 1) == '+') {
            tokens += Symbol("++")
            i += 2
          } else {
            tokens += Symbol("+")
            i += 1
          }
        case c @ ('-' | '*' | '/' | '(' | ')' | '{' | '}' | ';' | ',' | '[' | ']' | ':') =>
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
    def peekAt(offset: Int): Option[Token] = 
      if (pos + offset < tokens.length) Some(tokens(pos + offset)) else None
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
      val params = ListBuffer[String]()
      if (!ts.acceptSymbol(")")) {
        params += ts.consumeIdentOrFail()
        while (ts.acceptSymbol(",")) {
          params += ts.consumeIdentOrFail()
        }
        ts.expectSymbolOrFail(")")
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
        // Look ahead to distinguish between block and dictionary
        if (isDictionary()) {
          parseDict()
        } else {
          parseBlock()
        }
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
      case Keyword("keys") =>
        ts.consume()
        ts.expectSymbolOrFail("(")
        val dict = parseExpression()
        ts.expectSymbolOrFail(")")
        Exp.DictKeys(dict)
      case Keyword("values") =>
        ts.consume()
        ts.expectSymbolOrFail("(")
        val dict = parseExpression()
        ts.expectSymbolOrFail(")")
        Exp.DictValues(dict)
      case Keyword("size") =>
        ts.consume()
        ts.expectSymbolOrFail("(")
        val dict = parseExpression()
        ts.expectSymbolOrFail(")")
        Exp.DictSize(dict)
      case Keyword("contains") =>
        ts.consume()
        ts.expectSymbolOrFail("(")
        val dict = parseExpression()
        ts.expectSymbolOrFail(",")
        val key = parseExpression()
        ts.expectSymbolOrFail(")")
        Exp.DictContains(dict, key)
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

    private def isDictionary(): Boolean = {
      // Look ahead to see if this is a dictionary or block
      // Dictionary: {} or {key: value, ...}
      // Block: {statements...}
      ts.peekAt(1) match {
        case Some(Symbol("}")) => true // Empty braces = dict
        case Some(_) =>
          // Look for pattern: expr : expr
          // We need to be careful not to consume tokens
          var i = 1
          var depth = 0
          var foundColon = false
          var foundSemicolon = false
          
          while (i < 50 && ts.peekAt(i).isDefined && !foundColon && !foundSemicolon) {
            ts.peekAt(i) match {
              case Some(Symbol("{")) => depth += 1; i += 1
              case Some(Symbol("}")) => 
                if (depth == 0) return false // End of our block
                depth -= 1; i += 1
              case Some(Symbol(":")) if depth == 0 => foundColon = true
              case Some(Symbol(";")) if depth == 0 => foundSemicolon = true
              case Some(Symbol("=")) if depth == 0 => foundSemicolon = true // Assignment = block
              case _ => i += 1
            }
          }
          
          foundColon && !foundSemicolon
        case None => false
      }
    }

    private def parseDict(): Exp = {
      ts.expectSymbolOrFail("{")
      val entries = ListBuffer[(Exp, Exp)]()
      if (!ts.acceptSymbol("}")) {
        // Parse first entry
        val key = parseExpression()
        ts.expectSymbolOrFail(":")
        val value = parseExpression()
        entries += ((key, value))
        
        // Parse remaining entries
        while (ts.acceptSymbol(",")) {
          val k = parseExpression()
          ts.expectSymbolOrFail(":")
          val v = parseExpression()
          entries += ((k, v))
        }
        ts.expectSymbolOrFail("}")
      }
      Exp.VDict(entries.toList)
    }

    private def parseBlockOrExp(): Exp = ts.peek match {
      case Symbol("{") => parseBlock()
      case _            => parseExpression()
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
