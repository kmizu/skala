# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Skala is a toy programming language implemented in Scala 3, designed for learning minimal programming language implementation. It consists of three modules with increasing complexity:
- `math`: Simple arithmetic expression evaluator
- `skala`: Untyped language with variables, functions, control flow, and lists
- `tskala`: Typed version of Skala with Int and Bool types

## Build Commands

```bash
# Compile the project
sbt compile

# Run all tests
sbt test

# Run tests for a specific module
sbt "testOnly com.github.kmizu.math.*"
sbt "testOnly com.github.kmizu.skala.*"
sbt "testOnly com.github.kmizu.tskala.*"

# Run a specific test suite
sbt "testOnly com.github.kmizu.skala.EvaluatorSuite"

# Interactive Scala REPL with project classes
sbt console

# Clean build artifacts
sbt clean
```

## Code Architecture

### Package Structure
- **com.github.kmizu.math**: Basic arithmetic expression evaluator
  - `Exp.scala`: AST definitions using Scala enums
  - `ExpEvaluator.scala`: Direct evaluator using pattern matching
  - `JSONExprEvaluator.scala`: JSON-based AST evaluation

- **com.github.kmizu.skala**: Main untyped language
  - `Exp.scala`: Full AST with variables, functions, control flow, and lists
  - `Value.scala`: Runtime values (IntValue, ListValue)
  - `Parser.scala`: Hand-written recursive descent parser with integrated lexer
  - `Evaluator.scala`: Tree-walking interpreter with mutable environment
  - `JsonEvaluator.scala`: Alternative JSON-based evaluation

- **com.github.kmizu.tskala**: Typed variant
  - `Type.scala`: Type definitions (TInt, TBool, TList)
  - `Value.scala`: Runtime values (IntValue, ListValue)
  - `Parser.scala`: Type-aware parser supporting type annotations and list syntax
  - `Typer.scala`: Type checker implementation with list support
  - `Exp.scala`: Typed expressions including list operations
  - `Evaluator.scala`: Type-safe interpreter

### Key Design Patterns
1. **AST Representation**: Uses Scala 3 enums for algebraic data types
2. **DSL Construction**: Provides helper methods (tInt, tAdd, etc.) for direct AST creation
3. **Environment Management**: Mutable Map[String, Int] for variables and functions
4. **Error Handling**: Custom RuntimeException subclasses for evaluation errors
5. **JSON Support**: Alternative to parsing - allows AST input via JSON format

### Testing Approach
- Comprehensive test suites using MUnit
- Tests organized by functionality (arithmetic, variables, functions, control flow)
- Both direct DSL tests and JSON-based tests for each evaluator
- Parser tests verify syntax handling and error cases