package com.github.kmizu.tskala

import scala.collection.mutable
import Type._
import TypeInference._
import UnificationEngine._

// This is the new refactored Typer that delegates to organized modules
object Typer {
  
  // Use the refactored type inference implementation
  private val refactored = TyperRefactored
  
  def infer(exp: Exp, env: TypeEnvironment): (Substitution, Type) = 
    refactored.infer(exp, env)
  
  def inferWithEnv(exp: Exp, env: TypeEnvironment): (Substitution, Type, TypeEnvironment) = 
    refactored.inferWithEnv(exp, env)
  
  def typeCheckFunction(func: Func, env: TypeEnvironment): TypeEnvironment = 
    refactored.typeCheckFunction(func, env)
  
  def typeCheckProgram(prog: Program): Type = 
    refactored.typeCheckProgram(prog)
  
  // Compatibility wrapper for old tests
  def typeOf(e: Exp, typeEnv: mutable.Map[String, Type], funcEnv: mutable.Map[String, Func]): Type = 
    refactored.typeOf(e, typeEnv, funcEnv)
  
  def typeCheckFunction(func: Func, funcEnv: mutable.Map[String, Func]): Unit = 
    refactored.typeCheckFunction(func, funcEnv)
}