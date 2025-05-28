package com.github.kmizu.tskala

import Type._

case class Substitution(map: Map[String, Type]) {
  def apply(t: Type): Type = substitute(map, t)
  
  def apply(scheme: TypeScheme): TypeScheme = 
    TypeScheme(scheme.typeVars, substitute(map -- scheme.typeVars, scheme.tpe))
  
  def apply(env: TypeEnvironment): TypeEnvironment = 
    env.mapSchemes(apply)
  
  def compose(that: Substitution): Substitution = 
    Substitution(that.map ++ map.view.mapValues(t => substitute(that.map, t)).toMap)
}

object Substitution {
  val empty = Substitution(Map.empty)
  
  def single(name: String, tpe: Type): Substitution = 
    Substitution(Map(name -> tpe))
}