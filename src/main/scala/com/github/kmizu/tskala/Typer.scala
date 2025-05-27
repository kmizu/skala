package com.github.kmizu.tskala

import scala.collection.mutable
import Exp._
import Type._

object Typer {
  case class TypeEnv(
    vars: Map[String, TypeScheme],
    funcs: Map[String, Func]
  ) {
    def +(binding: (String, TypeScheme)): TypeEnv = 
      copy(vars = vars + binding)
    
    def ++(bindings: Map[String, TypeScheme]): TypeEnv = 
      copy(vars = vars ++ bindings)
    
    def freeTypeVars: Set[String] = 
      vars.values.flatMap(scheme => Type.freeTypeVars(scheme.tpe) -- scheme.typeVars).toSet
  }
  
  case class Substitution(map: Map[String, Type]) {
    def apply(t: Type): Type = substitute(map, t)
    
    def apply(scheme: TypeScheme): TypeScheme = 
      TypeScheme(scheme.typeVars, substitute(map -- scheme.typeVars, scheme.tpe))
    
    def apply(env: TypeEnv): TypeEnv = 
      env.copy(vars = env.vars.view.mapValues(apply).toMap)
    
    def compose(that: Substitution): Substitution = 
      Substitution(that.map ++ map.view.mapValues(t => substitute(that.map, t)).toMap)
  }
  
  object Substitution {
    val empty = Substitution(Map.empty)
  }
  
  def generalize(env: TypeEnv, t: Type): TypeScheme = {
    val envFreeVars = env.freeTypeVars
    val typeFreeVars = freeTypeVars(t)
    val generalizedVars = (typeFreeVars -- envFreeVars).toList.sorted
    TypeScheme(generalizedVars, t)
  }
  
  def instantiate(scheme: TypeScheme): Type = {
    val freshVars = scheme.typeVars.map(_ -> freshTypeVar()).toMap
    substitute(freshVars, scheme.tpe)
  }
  
  def unify(t1: Type, t2: Type): Substitution = (t1, t2) match {
    case (TInt, TInt) => Substitution.empty
    case (TBool, TBool) => Substitution.empty
    case (TString, TString) => Substitution.empty
    case (TVar(name1), TVar(name2)) if name1 == name2 => Substitution.empty
    case (TVar(name), t) => 
      if (freeTypeVars(t).contains(name))
        sys.error(s"Occurs check failed: $name occurs in $t")
      else
        Substitution(Map(name -> t))
    case (t, TVar(name)) => 
      if (freeTypeVars(t).contains(name))
        sys.error(s"Occurs check failed: $name occurs in $t")
      else
        Substitution(Map(name -> t))
    case (TList(elem1), TList(elem2)) => 
      unify(elem1, elem2)
    case (TDict(key1, val1), TDict(key2, val2)) =>
      val s1 = unify(key1, key2)
      val s2 = unify(s1(val1), s1(val2))
      s2.compose(s1)
    case (TFunc(params1, ret1), TFunc(params2, ret2)) =>
      if (params1.length != params2.length)
        sys.error(s"Cannot unify functions with different arities: ${params1.length} vs ${params2.length}")
      val paramPairs = params1.zip(params2)
      val (finalSubst, _) = paramPairs.foldLeft((Substitution.empty, Substitution.empty)) {
        case ((accSubst, composedSubst), (p1, p2)) =>
          val s = unify(composedSubst(p1), composedSubst(p2))
          (accSubst.compose(s), composedSubst.compose(s))
      }
      val retSubst = unify(finalSubst(ret1), finalSubst(ret2))
      retSubst.compose(finalSubst)
    case _ => 
      sys.error(s"Cannot unify $t1 with $t2")
  }
  
  def infer(exp: Exp, env: TypeEnv): (Substitution, Type) = exp match {
    case VInt(_) => (Substitution.empty, TInt)
    
    case VString(_) => (Substitution.empty, TString)
    
    case Ident(name) =>
      env.vars.get(name) match {
        case Some(scheme) => (Substitution.empty, instantiate(scheme))
        case None => sys.error(s"Undefined identifier: $name")
      }
    
    case BinExp("+", lhs, rhs) =>
      val (s1, t1) = infer(lhs, env)
      val (s2, t2) = infer(rhs, s1(env))
      val resultType = freshTypeVar()
      val s3 = unify(s2(t1), s2(t2))
      val s4 = s3(s2(t1)) match {
        case TInt => unify(s3(resultType), TInt)
        case TString => unify(s3(resultType), TString)
        case t => sys.error(s"Type error in + operator: operands must be Int or String, got $t")
      }
      (s4.compose(s3).compose(s2).compose(s1), s4(resultType))
    
    case BinExp(op@("-" | "*" | "/"), lhs, rhs) =>
      val (s1, t1) = infer(lhs, env)
      val (s2, t2) = infer(rhs, s1(env))
      val s3 = unify(s2(t1), TInt)
      val s4 = unify(s3(s2(t2)), TInt)
      (s4.compose(s3).compose(s2).compose(s1), TInt)
    
    case BinExp(op@("<" | ">" | "<=" | ">="), lhs, rhs) =>
      val (s1, t1) = infer(lhs, env)
      val (s2, t2) = infer(rhs, s1(env))
      val s3 = unify(s2(t1), TInt)
      val s4 = unify(s3(s2(t2)), TInt)
      (s4.compose(s3).compose(s2).compose(s1), TBool)
    
    case BinExp(op@("==" | "!="), lhs, rhs) =>
      val (s1, t1) = infer(lhs, env)
      val (s2, t2) = infer(rhs, s1(env))
      val s3 = unify(s2(t1), s2(t2))
      (s3.compose(s2).compose(s1), TBool)
    
    case If(condition, thenClause, elseClause) =>
      val (s1, condType) = infer(condition, env)
      // Allow truthy types
      condType match {
        case TBool | TInt | TString | TList(_) | TDict(_, _) | TVar(_) => // OK
        case _ => sys.error(s"Type error in if condition: invalid type $condType")
      }
      val (s2, thenType) = infer(thenClause, s1(env))
      val (s3, elseType) = infer(elseClause, s2(s1(env)))
      val s4 = unify(s3(thenType), elseType)
      (s4.compose(s3).compose(s2).compose(s1), s4(elseType))
    
    case SeqExp(bodies) if bodies.isEmpty =>
      (Substitution.empty, TInt)
    
    case SeqExp(bodies) =>
      val (finalSubst, finalType) = bodies.foldLeft((Substitution.empty, TInt: Type)) {
        case ((accSubst, _), expr) =>
          val (s, t) = infer(expr, accSubst(env))
          (s.compose(accSubst), t)
      }
      (finalSubst, finalType)
    
    case While(condition, bodies) =>
      val (s1, condType) = infer(condition, env)
      // Allow truthy types
      condType match {
        case TBool | TInt | TString | TList(_) | TDict(_, _) | TVar(_) => // OK
        case _ => sys.error(s"Type error in while condition: invalid type $condType")
      }
      // For while loops, we need to ensure variables assigned inside are visible
      val (finalSubst, _, _) = bodies.foldLeft((s1, TInt: Type, env)) { 
        case ((accSubst, _, accEnv), expr) =>
          val (s, t, newEnv) = inferWithEnv(expr, accSubst(accEnv))
          (s.compose(accSubst), t, newEnv)
      }
      (finalSubst, TInt)
    
    case Assignment(name, expression) =>
      val (s1, exprType) = infer(expression, env)
      // For assignments, we need to check if variable exists
      env.vars.get(name) match {
        case Some(scheme) =>
          val existingType = instantiate(scheme)
          val s2 = unify(s1(exprType), s1(existingType))
          (s2.compose(s1), s2(s1(exprType)))
        case None =>
          // New variable - will be added to environment by caller
          (s1, exprType)
      }
    
    case Call(name, args) =>
      env.funcs.get(name) match {
        case Some(Func(_, params, retType, _)) =>
          if (params.length != args.length)
            sys.error(s"Function '$name' expects ${params.length} arguments, got ${args.length}")
          
          val (finalSubst, argTypes) = args.foldLeft((Substitution.empty, List.empty[Type])) {
            case ((accSubst, types), arg) =>
              val (s, t) = infer(arg, accSubst(env))
              (s.compose(accSubst), types :+ t)
          }
          
          val funcType = TFunc(params.map(_._2), retType)
          val freshFuncType = instantiate(TypeScheme(freeTypeVars(funcType).toList, funcType))
          
          freshFuncType match {
            case TFunc(paramTypes, freshRetType) =>
              val unifySubst = paramTypes.zip(argTypes).foldLeft(finalSubst) {
                case (accSubst, (paramType, argType)) =>
                  val s = unify(accSubst(paramType), accSubst(argType))
                  s.compose(accSubst)
              }
              (unifySubst, unifySubst(freshRetType))
            case _ => sys.error("Internal error: function type expected")
          }
          
        case None => sys.error(s"Undefined function: '$name'")
      }
    
    case VList(elements) =>
      if (elements.isEmpty) {
        val elemType = freshTypeVar()
        (Substitution.empty, TList(elemType))
      } else {
        val (s1, firstType) = infer(elements.head, env)
        val (finalSubst, _) = elements.tail.foldLeft((s1, firstType)) {
          case ((accSubst, expectedType), elem) =>
            val (s, elemType) = infer(elem, accSubst(env))
            val unifySubst = unify(s(accSubst(expectedType)), s(elemType))
            (unifySubst.compose(s).compose(accSubst), unifySubst(s(elemType)))
        }
        (finalSubst, TList(finalSubst(firstType)))
      }
    
    case ListAccess(list, index) =>
      val (s1, listType) = infer(list, env)
      val (s2, indexType) = infer(index, s1(env))
      val elemType = freshTypeVar()
      val s3 = unify(s2(s1(listType)), TList(elemType))
      val s4 = unify(s3(s2(indexType)), TInt)
      (s4.compose(s3).compose(s2).compose(s1), s4(s3(elemType)))
    
    case ListLength(list) =>
      val (s1, listType) = infer(list, env)
      val elemType = freshTypeVar()
      val s2 = unify(s1(listType), TList(elemType))
      (s2.compose(s1), TInt)
    
    case ListAppend(list, element) =>
      val (s1, listType) = infer(list, env)
      val (s2, elemType) = infer(element, s1(env))
      val expectedElemType = freshTypeVar()
      val s3 = unify(s2(s1(listType)), TList(expectedElemType))
      val s4 = unify(s3(s2(elemType)), s3(expectedElemType))
      (s4.compose(s3).compose(s2).compose(s1), s4(s3(TList(expectedElemType))))
    
    case StringLength(str) =>
      val (s1, strType) = infer(str, env)
      val s2 = unify(s1(strType), TString)
      (s2.compose(s1), TInt)
    
    case StringConcat(lhs, rhs) =>
      val (s1, lType) = infer(lhs, env)
      val (s2, rType) = infer(rhs, s1(env))
      val s3 = unify(s2(s1(lType)), TString)
      val s4 = unify(s3(s2(rType)), TString)
      (s4.compose(s3).compose(s2).compose(s1), TString)
    
    case VDict(entries) =>
      if (entries.isEmpty) {
        val keyType = freshTypeVar()
        val valueType = freshTypeVar()
        (Substitution.empty, TDict(keyType, valueType))
      } else {
        val (firstKey, firstValue) = entries.head
        val (s1, keyType) = infer(firstKey, env)
        val (s2, valueType) = infer(firstValue, s1(env))
        
        val finalSubst = entries.tail.foldLeft(s2.compose(s1)) {
          case (accSubst, (k, v)) =>
            val (sk, kType) = infer(k, accSubst(env))
            val (sv, vType) = infer(v, sk(accSubst(env)))
            val s3 = unify(sv(sk(accSubst(keyType))), sv(kType))
            val s4 = unify(s3(sv(sk(accSubst(valueType)))), s3(sv(vType)))
            s4.compose(s3).compose(sv).compose(sk).compose(accSubst)
        }
        (finalSubst, TDict(finalSubst(keyType), finalSubst(valueType)))
      }
    
    case DictAccess(dict, key) =>
      val (s1, dictType) = infer(dict, env)
      val (s2, keyType) = infer(key, s1(env))
      val expectedKeyType = freshTypeVar()
      val valueType = freshTypeVar()
      val s3 = unify(s2(s1(dictType)), TDict(expectedKeyType, valueType))
      val s4 = unify(s3(s2(keyType)), s3(expectedKeyType))
      (s4.compose(s3).compose(s2).compose(s1), s4(s3(valueType)))
    
    case DictSet(dict, key, value) =>
      val (s1, dictType) = infer(dict, env)
      val (s2, keyType) = infer(key, s1(env))
      val (s3, valueType) = infer(value, s2(s1(env)))
      val expectedKeyType = freshTypeVar()
      val expectedValueType = freshTypeVar()
      val s4 = unify(s3(s2(s1(dictType))), TDict(expectedKeyType, expectedValueType))
      val s5 = unify(s4(s3(s2(keyType))), s4(expectedKeyType))
      val s6 = unify(s5(s4(s3(valueType))), s5(s4(expectedValueType)))
      (s6.compose(s5).compose(s4).compose(s3).compose(s2).compose(s1), 
       s6(s5(s4(TDict(expectedKeyType, expectedValueType)))))
    
    case DictKeys(dict) =>
      val (s1, dictType) = infer(dict, env)
      val keyType = freshTypeVar()
      val valueType = freshTypeVar()
      val s2 = unify(s1(dictType), TDict(keyType, valueType))
      (s2.compose(s1), TList(s2(keyType)))
    
    case DictValues(dict) =>
      val (s1, dictType) = infer(dict, env)
      val keyType = freshTypeVar()
      val valueType = freshTypeVar()
      val s2 = unify(s1(dictType), TDict(keyType, valueType))
      (s2.compose(s1), TList(s2(valueType)))
    
    case DictSize(dict) =>
      val (s1, dictType) = infer(dict, env)
      val keyType = freshTypeVar()
      val valueType = freshTypeVar()
      val s2 = unify(s1(dictType), TDict(keyType, valueType))
      (s2.compose(s1), TInt)
    
    case DictContains(dict, key) =>
      val (s1, dictType) = infer(dict, env)
      val (s2, keyType) = infer(key, s1(env))
      val expectedKeyType = freshTypeVar()
      val valueType = freshTypeVar()
      val s3 = unify(s2(s1(dictType)), TDict(expectedKeyType, valueType))
      val s4 = unify(s3(s2(keyType)), s3(expectedKeyType))
      (s4.compose(s3).compose(s2).compose(s1), TBool)
    
    case Let(name, value, body) =>
      val (s1, valueType) = infer(value, env)
      val generalizedType = generalize(s1(env), valueType)
      val newEnv = s1(env) + (name -> generalizedType)
      val (s2, bodyType) = infer(body, newEnv)
      (s2.compose(s1), bodyType)
    
    case Lambda(params, body) =>
      val paramTypes = params.map(_ => freshTypeVar())
      val paramBindings = params.zip(paramTypes).map {
        case (name, tpe) => name -> TypeScheme(Nil, tpe)
      }.toMap
      val bodyEnv = env ++ paramBindings
      val (s, bodyType) = infer(body, bodyEnv)
      val inferredParamTypes = paramTypes.map(s(_))
      (s, TFunc(inferredParamTypes, bodyType))
    
    case Apply(func, args) =>
      val (s1, funcType) = infer(func, env)
      val (finalSubst, argTypes) = args.foldLeft((s1, List.empty[Type])) {
        case ((accSubst, types), arg) =>
          val (s, t) = infer(arg, accSubst(env))
          (s.compose(accSubst), types :+ t)
      }
      
      val retType = freshTypeVar()
      val expectedFuncType = TFunc(argTypes, retType)
      val s2 = unify(finalSubst(funcType), expectedFuncType)
      (s2.compose(finalSubst), s2(retType))
    
    case _ => sys.error(s"Unhandled expression in type inference: $exp")
  }
  
  def inferWithEnv(exp: Exp, env: TypeEnv): (Substitution, Type, TypeEnv) = exp match {
    case Assignment(name, expr) =>
      val (s, t) = infer(expr, env)
      val generalizedType = generalize(s(env), t)
      (s, t, s(env) + (name -> generalizedType))
    
    case Let(name, value, body) =>
      val (s1, valueType) = infer(value, env)
      val generalizedType = generalize(s1(env), valueType)
      val newEnv = s1(env) + (name -> generalizedType)
      val (s2, bodyType, finalEnv) = inferWithEnv(body, newEnv)
      // Let bindings don't affect the outer environment
      (s2.compose(s1), bodyType, s2(s1(env)))
    
    case SeqExp(bodies) if bodies.isEmpty =>
      (Substitution.empty, TInt, env)
    
    case SeqExp(bodies) =>
      val (finalSubst, finalType, finalEnv) = bodies.foldLeft((Substitution.empty, TInt: Type, env)) {
        case ((accSubst, _, accEnv), expr) =>
          val (s, t, newEnv) = inferWithEnv(expr, accSubst(accEnv))
          (s.compose(accSubst), t, newEnv)
      }
      (finalSubst, finalType, finalEnv)
    
    case While(condition, bodies) =>
      val (s1, condType) = infer(condition, env)
      // Allow truthy types
      condType match {
        case TBool | TInt | TString | TList(_) | TDict(_, _) | TVar(_) => // OK
        case _ => sys.error(s"Type error in while condition: invalid type $condType")
      }
      // For while loops, we need to ensure variables assigned inside persist in the environment
      val (finalSubst, _, finalEnv) = bodies.foldLeft((s1, TInt: Type, env)) { 
        case ((accSubst, _, accEnv), expr) =>
          val (s, t, newEnv) = inferWithEnv(expr, accSubst(accEnv))
          (s.compose(accSubst), t, newEnv)
      }
      (finalSubst, TInt, finalEnv)
      
    case _ =>
      val (s, t) = infer(exp, env)
      (s, t, s(env))
  }
  
  def typeCheckFunction(func: Func, env: TypeEnv): TypeEnv = {
    // Create environment with function parameters
    val paramBindings = func.params.map {
      case (name, tpe) => name -> TypeScheme(Nil, tpe)
    }.toMap
    
    val bodyEnv = env ++ paramBindings
    val (s, bodyType, _) = inferWithEnv(func.body, bodyEnv)
    
    // Unify inferred body type with declared return type
    val s2 = unify(s(bodyType), s(func.returnType))
    val finalSubst = s2.compose(s)
    
    // Update function environment with inferred types
    val inferredFunc = func.copy(
      params = func.params.map { case (name, tpe) => name -> finalSubst(tpe) },
      returnType = finalSubst(func.returnType)
    )
    
    env.copy(funcs = env.funcs + (func.name -> inferredFunc))
  }
  
  def typeCheckProgram(prog: Program): Type = {
    // Initialize environment with functions
    val initialFuncs = prog.functions.map(f => f.name -> f).toMap
    val initialEnv = TypeEnv(Map.empty, initialFuncs)
    
    // Type check all functions
    val funcEnv = prog.functions.foldLeft(initialEnv) { (env, func) =>
      typeCheckFunction(func, env)
    }
    
    // Type check main program body
    val (_, resultType, _) = prog.bodies.foldLeft((Substitution.empty, TInt: Type, funcEnv)) {
      case ((accSubst, _, env), expr) =>
        val (s, t, newEnv) = inferWithEnv(expr, accSubst(env))
        (s.compose(accSubst), t, newEnv)
    }
    
    resultType
  }
  
  // Compatibility wrapper for old tests
  def typeOf(e: Exp, typeEnv: mutable.Map[String, Type], funcEnv: mutable.Map[String, Func]): Type = {
    val vars = typeEnv.map { case (k, v) => k -> TypeScheme(Nil, v) }.toMap
    val funcs = funcEnv.toMap
    val env = TypeEnv(vars, funcs)
    val (_, tpe) = infer(e, env)
    tpe
  }
  
  def typeCheckFunction(func: Func, funcEnv: mutable.Map[String, Func]): Unit = {
    val env = TypeEnv(Map.empty, funcEnv.toMap)
    typeCheckFunction(func, env)
    ()
  }
}