package com.github.kmizu.tskala

import Type._
import Exp._

object TypeInference {
  
  def generalize(env: TypeEnvironment, t: Type): TypeScheme = {
    val envFreeVars = env.freeTypeVars
    val typeFreeVars = freeTypeVars(t)
    val generalizedVars = (typeFreeVars -- envFreeVars).toList.sorted
    TypeScheme(generalizedVars, t)
  }
  
  def instantiate(scheme: TypeScheme): Type = {
    val freshVars = scheme.typeVars.map(_ -> freshTypeVar()).toMap
    substitute(freshVars, scheme.tpe)
  }
  
  trait InferenceResult {
    def substitution: Substitution
    def tpe: Type
  }
  
  case class SimpleInferenceResult(substitution: Substitution, tpe: Type) extends InferenceResult
  
  case class EnvironmentInferenceResult(
    substitution: Substitution, 
    tpe: Type, 
    env: TypeEnvironment
  ) extends InferenceResult
}