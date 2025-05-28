package com.github.kmizu.tskala

import Type._

trait TypeEnvironment {
  def vars: Map[String, TypeScheme]
  def funcs: Map[String, Func]
  
  def +(binding: (String, TypeScheme)): TypeEnvironment
  def ++(bindings: Map[String, TypeScheme]): TypeEnvironment
  def withFunc(name: String, func: Func): TypeEnvironment
  def lookupVar(name: String): Option[TypeScheme]
  def lookupFunc(name: String): Option[Func]
  def freeTypeVars: Set[String]
  def mapSchemes(f: TypeScheme => TypeScheme): TypeEnvironment
}

case class TypeEnv(
  vars: Map[String, TypeScheme],
  funcs: Map[String, Func]
) extends TypeEnvironment {
  
  def +(binding: (String, TypeScheme)): TypeEnvironment = 
    copy(vars = vars + binding)
  
  def ++(bindings: Map[String, TypeScheme]): TypeEnvironment = 
    copy(vars = vars ++ bindings)
  
  def withFunc(name: String, func: Func): TypeEnvironment =
    copy(funcs = funcs + (name -> func))
  
  def lookupVar(name: String): Option[TypeScheme] = 
    vars.get(name)
  
  def lookupFunc(name: String): Option[Func] = 
    funcs.get(name)
  
  def freeTypeVars: Set[String] = 
    vars.values.flatMap(scheme => Type.freeTypeVars(scheme.tpe) -- scheme.typeVars).toSet
  
  def mapSchemes(f: TypeScheme => TypeScheme): TypeEnvironment =
    copy(vars = vars.view.mapValues(f).toMap)
}

object TypeEnvironment {
  def empty: TypeEnvironment = TypeEnv(Map.empty, Map.empty)
  
  def withFunctions(funcs: Map[String, Func]): TypeEnvironment = 
    TypeEnv(Map.empty, funcs)
}