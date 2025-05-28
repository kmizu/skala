package com.github.kmizu.tskala

import Type._

object UnificationEngine {
  
  case class UnificationError(message: String) extends RuntimeException(message)
  
  def unify(t1: Type, t2: Type): Substitution = (t1, t2) match {
    case (TInt, TInt) => Substitution.empty
    case (TBool, TBool) => Substitution.empty
    case (TString, TString) => Substitution.empty
    
    case (TVar(name1), TVar(name2)) if name1 == name2 => 
      Substitution.empty
    
    case (TVar(name), t) => 
      if (occursIn(name, t))
        throw UnificationError(s"Occurs check failed: $name occurs in $t")
      else
        Substitution.single(name, t)
    
    case (t, TVar(name)) => 
      if (occursIn(name, t))
        throw UnificationError(s"Occurs check failed: $name occurs in $t")
      else
        Substitution.single(name, t)
    
    case (TList(elem1), TList(elem2)) => 
      unify(elem1, elem2)
    
    case (TDict(key1, val1), TDict(key2, val2)) =>
      val s1 = unify(key1, key2)
      val s2 = unify(s1(val1), s1(val2))
      s2.compose(s1)
    
    case (TFunc(params1, ret1), TFunc(params2, ret2)) =>
      if (params1.length != params2.length)
        throw UnificationError(s"Cannot unify functions with different arities: ${params1.length} vs ${params2.length}")
      
      unifyList(params1, params2) match {
        case (subst, _) =>
          val retSubst = unify(subst(ret1), subst(ret2))
          retSubst.compose(subst)
      }
    
    case _ => 
      throw UnificationError(s"Cannot unify $t1 with $t2")
  }
  
  def unifyList(types1: List[Type], types2: List[Type]): (Substitution, List[Type]) = {
    if (types1.length != types2.length)
      throw UnificationError(s"Cannot unify lists of different lengths")
    
    val (finalSubst, unifiedTypes) = types1.zip(types2).foldLeft((Substitution.empty, List.empty[Type])) {
      case ((accSubst, accTypes), (t1, t2)) =>
        val s = unify(accSubst(t1), accSubst(t2))
        val composedSubst = s.compose(accSubst)
        (composedSubst, accTypes :+ composedSubst(t1))
    }
    (finalSubst, unifiedTypes)
  }
  
  private def occursIn(name: String, t: Type): Boolean = 
    freeTypeVars(t).contains(name)
}