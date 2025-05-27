package com.github.kmizu.tskala

enum Type {
  case TInt
  case TBool
  case TString
  case TList(elementType: Type)
  case TDict(keyType: Type, valueType: Type)
  case TVar(name: String)
  case TFunc(paramTypes: List[Type], returnType: Type)
}

case class TypeScheme(typeVars: List[String], tpe: Type)

object Type {
  var typeVarCounter = 0
  
  def freshTypeVar(): Type = {
    typeVarCounter += 1
    TVar(s"t$typeVarCounter")
  }
  
  def freeTypeVars(tpe: Type): Set[String] = tpe match {
    case TInt | TBool | TString => Set.empty
    case TVar(name) => Set(name)
    case TList(elem) => freeTypeVars(elem)
    case TDict(key, value) => freeTypeVars(key) ++ freeTypeVars(value)
    case TFunc(params, ret) => params.flatMap(freeTypeVars).toSet ++ freeTypeVars(ret)
  }
  
  def freeTypeVars(scheme: TypeScheme): Set[String] = 
    freeTypeVars(scheme.tpe) -- scheme.typeVars
  
  def substitute(subst: Map[String, Type], tpe: Type): Type = tpe match {
    case TInt | TBool | TString => tpe
    case TVar(name) => subst.getOrElse(name, tpe)
    case TList(elem) => TList(substitute(subst, elem))
    case TDict(key, value) => TDict(substitute(subst, key), substitute(subst, value))
    case TFunc(params, ret) => TFunc(params.map(substitute(subst, _)), substitute(subst, ret))
  }
}
