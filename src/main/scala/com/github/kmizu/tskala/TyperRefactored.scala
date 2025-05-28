package com.github.kmizu.tskala

import scala.collection.mutable
import Type._
import TypeInference._
import UnificationEngine._

object TyperRefactored {
  
  // Type inference for literals
  object LiteralInference {
    def infer(exp: Exp, env: TypeEnvironment): (Substitution, Type) = exp match {
      case Exp.VInt(_) => (Substitution.empty, TInt)
      case Exp.VString(_) => (Substitution.empty, TString)
      case _ => throw new IllegalArgumentException(s"Not a literal: $exp")
    }
  }
  
  // Type inference for arithmetic operations
  object ArithmeticInference {
    def infer(op: String, lhs: Exp, rhs: Exp, env: TypeEnvironment): (Substitution, Type) = op match {
      case "+" => inferAddition(lhs, rhs, env)
      case "-" | "*" | "/" => inferNumericBinOp(lhs, rhs, env)
      case "<" | ">" | "<=" | ">=" => inferComparison(lhs, rhs, env)
      case "==" | "!=" => inferEquality(lhs, rhs, env)
      case _ => throw new IllegalArgumentException(s"Unknown binary operator: $op")
    }
    
    private def inferAddition(lhs: Exp, rhs: Exp, env: TypeEnvironment): (Substitution, Type) = {
      val (s1, t1) = TyperRefactored.infer(lhs, env)
      val (s2, t2) = TyperRefactored.infer(rhs, s1(env))
      val resultType = freshTypeVar()
      val s3 = unify(s2(t1), s2(t2))
      val s4 = s3(s2(t1)) match {
        case TInt => unify(s3(resultType), TInt)
        case TString => unify(s3(resultType), TString)
        case t => sys.error(s"Type error in + operator: operands must be Int or String, got $t")
      }
      (s4.compose(s3).compose(s2).compose(s1), s4(resultType))
    }
    
    private def inferNumericBinOp(lhs: Exp, rhs: Exp, env: TypeEnvironment): (Substitution, Type) = {
      val (s1, t1) = TyperRefactored.infer(lhs, env)
      val (s2, t2) = TyperRefactored.infer(rhs, s1(env))
      val s3 = unify(s2(t1), TInt)
      val s4 = unify(s3(s2(t2)), TInt)
      (s4.compose(s3).compose(s2).compose(s1), TInt)
    }
    
    private def inferComparison(lhs: Exp, rhs: Exp, env: TypeEnvironment): (Substitution, Type) = {
      val (s1, t1) = TyperRefactored.infer(lhs, env)
      val (s2, t2) = TyperRefactored.infer(rhs, s1(env))
      val s3 = unify(s2(t1), TInt)
      val s4 = unify(s3(s2(t2)), TInt)
      (s4.compose(s3).compose(s2).compose(s1), TBool)
    }
    
    private def inferEquality(lhs: Exp, rhs: Exp, env: TypeEnvironment): (Substitution, Type) = {
      val (s1, t1) = TyperRefactored.infer(lhs, env)
      val (s2, t2) = TyperRefactored.infer(rhs, s1(env))
      val s3 = unify(s2(t1), s2(t2))
      (s3.compose(s2).compose(s1), TBool)
    }
  }
  
  // Type inference for control flow
  object ControlFlowInference {
    def inferIf(condition: Exp, thenClause: Exp, elseClause: Exp, env: TypeEnvironment): (Substitution, Type) = {
      val (s1, condType) = TyperRefactored.infer(condition, env)
      // Allow truthy types
      condType match {
        case TBool | TInt | TString | TList(_) | TDict(_, _) | TVar(_) => // OK
        case _ => sys.error(s"Type error in if condition: invalid type $condType")
      }
      val (s2, thenType) = TyperRefactored.infer(thenClause, s1(env))
      val (s3, elseType) = TyperRefactored.infer(elseClause, s2(s1(env)))
      val s4 = unify(s3(thenType), elseType)
      (s4.compose(s3).compose(s2).compose(s1), s4(elseType))
    }
    
    def inferWhile(condition: Exp, bodies: List[Exp], env: TypeEnvironment): (Substitution, Type, TypeEnvironment) = {
      val (s1, condType) = TyperRefactored.infer(condition, env)
      // Allow truthy types
      condType match {
        case TBool | TInt | TString | TList(_) | TDict(_, _) | TVar(_) => // OK
        case _ => sys.error(s"Type error in while condition: invalid type $condType")
      }
      // For while loops, we need to ensure variables assigned inside persist in the environment
      val (finalSubst, _, finalEnv) = bodies.foldLeft((s1, TInt: Type, env)) { 
        case ((accSubst, _, accEnv), expr) =>
          val (s, t, newEnv) = TyperRefactored.inferWithEnv(expr, accSubst(accEnv))
          (s.compose(accSubst), t, newEnv)
      }
      (finalSubst, TInt, finalEnv)
    }
    
    def inferSeq(bodies: List[Exp], env: TypeEnvironment): (Substitution, Type, TypeEnvironment) = {
      if (bodies.isEmpty) {
        (Substitution.empty, TInt, env)
      } else {
        val (finalSubst, finalType, finalEnv) = bodies.foldLeft((Substitution.empty, TInt: Type, env)) {
          case ((accSubst, _, accEnv), expr) =>
            val (s, t, newEnv) = TyperRefactored.inferWithEnv(expr, accSubst(accEnv))
            (s.compose(accSubst), t, newEnv)
        }
        (finalSubst, finalType, finalEnv)
      }
    }
  }
  
  // Type inference for collections
  object CollectionInference {
    def inferList(elements: List[Exp], env: TypeEnvironment): (Substitution, Type) = {
      if (elements.isEmpty) {
        val elemType = freshTypeVar()
        (Substitution.empty, TList(elemType))
      } else {
        val (s1, firstType) = TyperRefactored.infer(elements.head, env)
        val (finalSubst, _) = elements.tail.foldLeft((s1, firstType)) {
          case ((accSubst, expectedType), elem) =>
            val (s, elemType) = TyperRefactored.infer(elem, accSubst(env))
            val unifySubst = unify(s(accSubst(expectedType)), s(elemType))
            (unifySubst.compose(s).compose(accSubst), unifySubst(s(elemType)))
        }
        (finalSubst, TList(finalSubst(firstType)))
      }
    }
    
    def inferListAccess(list: Exp, index: Exp, env: TypeEnvironment): (Substitution, Type) = {
      val (s1, listType) = TyperRefactored.infer(list, env)
      val (s2, indexType) = TyperRefactored.infer(index, s1(env))
      val elemType = freshTypeVar()
      val s3 = unify(s2(s1(listType)), TList(elemType))
      val s4 = unify(s3(s2(indexType)), TInt)
      (s4.compose(s3).compose(s2).compose(s1), s4(s3(elemType)))
    }
    
    def inferDict(entries: List[(Exp, Exp)], env: TypeEnvironment): (Substitution, Type) = {
      if (entries.isEmpty) {
        val keyType = freshTypeVar()
        val valueType = freshTypeVar()
        (Substitution.empty, TDict(keyType, valueType))
      } else {
        val (firstKey, firstValue) = entries.head
        val (s1, keyType) = TyperRefactored.infer(firstKey, env)
        val (s2, valueType) = TyperRefactored.infer(firstValue, s1(env))
        
        val finalSubst = entries.tail.foldLeft(s2.compose(s1)) {
          case (accSubst, (k, v)) =>
            val (sk, kType) = TyperRefactored.infer(k, accSubst(env))
            val (sv, vType) = TyperRefactored.infer(v, sk(accSubst(env)))
            val s3 = unify(sv(sk(accSubst(keyType))), sv(kType))
            val s4 = unify(s3(sv(sk(accSubst(valueType)))), s3(sv(vType)))
            s4.compose(s3).compose(sv).compose(sk).compose(accSubst)
        }
        (finalSubst, TDict(finalSubst(keyType), finalSubst(valueType)))
      }
    }
  }
  
  // Type inference for functions
  object FunctionInference {
    def inferCall(name: String, args: List[Exp], env: TypeEnvironment): (Substitution, Type) = {
      env.lookupFunc(name) match {
        case Some(Func(_, params, retType, _)) =>
          if (params.length != args.length)
            sys.error(s"Function '$name' expects ${params.length} arguments, got ${args.length}")
          
          val (finalSubst, argTypes) = args.foldLeft((Substitution.empty, List.empty[Type])) {
            case ((accSubst, types), arg) =>
              val (s, t) = TyperRefactored.infer(arg, accSubst(env))
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
    }
    
    def inferLambda(params: List[String], body: Exp, env: TypeEnvironment): (Substitution, Type) = {
      val paramTypes = params.map(_ => freshTypeVar())
      val paramBindings = params.zip(paramTypes).map {
        case (name, tpe) => name -> TypeScheme(Nil, tpe)
      }.toMap
      val bodyEnv = env ++ paramBindings
      val (s, bodyType) = TyperRefactored.infer(body, bodyEnv)
      val inferredParamTypes = paramTypes.map(s(_))
      (s, TFunc(inferredParamTypes, bodyType))
    }
    
    def inferApply(func: Exp, args: List[Exp], env: TypeEnvironment): (Substitution, Type) = {
      val (s1, funcType) = TyperRefactored.infer(func, env)
      val (finalSubst, argTypes) = args.foldLeft((s1, List.empty[Type])) {
        case ((accSubst, types), arg) =>
          val (s, t) = TyperRefactored.infer(arg, accSubst(env))
          (s.compose(accSubst), types :+ t)
      }
      
      val retType = freshTypeVar()
      val expectedFuncType = TFunc(argTypes, retType)
      val s2 = unify(finalSubst(funcType), expectedFuncType)
      (s2.compose(finalSubst), s2(retType))
    }
  }
  
  // Main type inference function
  def infer(exp: Exp, env: TypeEnvironment): (Substitution, Type) = exp match {
    case e: Exp.VInt => LiteralInference.infer(e, env)
    case e: Exp.VString => LiteralInference.infer(e, env)
    
    case Exp.Ident(name) =>
      env.lookupVar(name) match {
        case Some(scheme) => (Substitution.empty, instantiate(scheme))
        case None => sys.error(s"Undefined identifier: $name")
      }
    
    case Exp.BinExp(op, lhs, rhs) => ArithmeticInference.infer(op, lhs, rhs, env)
    
    case Exp.If(cond, thenC, elseC) => ControlFlowInference.inferIf(cond, thenC, elseC, env)
    
    case Exp.SeqExp(bodies) => 
      val (s, t, _) = ControlFlowInference.inferSeq(bodies, env)
      (s, t)
    
    case Exp.While(cond, bodies) => 
      val (s, t, _) = ControlFlowInference.inferWhile(cond, bodies, env)
      (s, t)
    
    case Exp.Assignment(name, expr) =>
      val (s1, exprType) = infer(expr, env)
      env.lookupVar(name) match {
        case Some(scheme) =>
          val existingType = instantiate(scheme)
          val s2 = unify(s1(exprType), s1(existingType))
          (s2.compose(s1), s2(s1(exprType)))
        case None =>
          (s1, exprType)
      }
    
    case Exp.Call(name, args) => FunctionInference.inferCall(name, args, env)
    
    case Exp.VList(elems) => CollectionInference.inferList(elems, env)
    case Exp.ListAccess(list, idx) => CollectionInference.inferListAccess(list, idx, env)
    case Exp.ListLength(list) =>
      val (s1, listType) = infer(list, env)
      val elemType = freshTypeVar()
      val s2 = unify(s1(listType), TList(elemType))
      (s2.compose(s1), TInt)
    
    case Exp.ListAppend(list, elem) =>
      val (s1, listType) = infer(list, env)
      val (s2, elemType) = infer(elem, s1(env))
      val expectedElemType = freshTypeVar()
      val s3 = unify(s2(s1(listType)), TList(expectedElemType))
      val s4 = unify(s3(s2(elemType)), s3(expectedElemType))
      (s4.compose(s3).compose(s2).compose(s1), s4(s3(TList(expectedElemType))))
    
    case Exp.StringLength(str) =>
      val (s1, strType) = infer(str, env)
      val s2 = unify(s1(strType), TString)
      (s2.compose(s1), TInt)
    
    case Exp.StringConcat(lhs, rhs) =>
      val (s1, lType) = infer(lhs, env)
      val (s2, rType) = infer(rhs, s1(env))
      val s3 = unify(s2(s1(lType)), TString)
      val s4 = unify(s3(s2(rType)), TString)
      (s4.compose(s3).compose(s2).compose(s1), TString)
    
    case Exp.VDict(entries) => CollectionInference.inferDict(entries, env)
    
    case Exp.DictAccess(dict, key) =>
      val (s1, dictType) = infer(dict, env)
      val (s2, keyType) = infer(key, s1(env))
      val expectedKeyType = freshTypeVar()
      val valueType = freshTypeVar()
      val s3 = unify(s2(s1(dictType)), TDict(expectedKeyType, valueType))
      val s4 = unify(s3(s2(keyType)), s3(expectedKeyType))
      (s4.compose(s3).compose(s2).compose(s1), s4(s3(valueType)))
    
    case Exp.DictSet(dict, key, value) =>
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
    
    case Exp.DictKeys(dict) =>
      val (s1, dictType) = infer(dict, env)
      val keyType = freshTypeVar()
      val valueType = freshTypeVar()
      val s2 = unify(s1(dictType), TDict(keyType, valueType))
      (s2.compose(s1), TList(s2(keyType)))
    
    case Exp.DictValues(dict) =>
      val (s1, dictType) = infer(dict, env)
      val keyType = freshTypeVar()
      val valueType = freshTypeVar()
      val s2 = unify(s1(dictType), TDict(keyType, valueType))
      (s2.compose(s1), TList(s2(valueType)))
    
    case Exp.DictSize(dict) =>
      val (s1, dictType) = infer(dict, env)
      val keyType = freshTypeVar()
      val valueType = freshTypeVar()
      val s2 = unify(s1(dictType), TDict(keyType, valueType))
      (s2.compose(s1), TInt)
    
    case Exp.DictContains(dict, key) =>
      val (s1, dictType) = infer(dict, env)
      val (s2, keyType) = infer(key, s1(env))
      val expectedKeyType = freshTypeVar()
      val valueType = freshTypeVar()
      val s3 = unify(s2(s1(dictType)), TDict(expectedKeyType, valueType))
      val s4 = unify(s3(s2(keyType)), s3(expectedKeyType))
      (s4.compose(s3).compose(s2).compose(s1), TBool)
    
    case Exp.Let(name, value, body) =>
      val (s1, valueType) = infer(value, env)
      val generalizedType = generalize(s1(env), valueType)
      val newEnv = s1(env) + (name -> generalizedType)
      val (s2, bodyType) = infer(body, newEnv)
      (s2.compose(s1), bodyType)
    
    case Exp.Lambda(params, body) => FunctionInference.inferLambda(params, body, env)
    case Exp.Apply(func, args) => FunctionInference.inferApply(func, args, env)
  }
  
  def inferWithEnv(exp: Exp, env: TypeEnvironment): (Substitution, Type, TypeEnvironment) = exp match {
    case Exp.Assignment(name, expr) =>
      val (s, t) = infer(expr, env)
      val generalizedType = generalize(s(env), t)
      (s, t, s(env) + (name -> generalizedType))
    
    case Exp.Let(name, value, body) =>
      val (s1, valueType) = infer(value, env)
      val generalizedType = generalize(s1(env), valueType)
      val newEnv = s1(env) + (name -> generalizedType)
      val (s2, bodyType, finalEnv) = inferWithEnv(body, newEnv)
      // Let bindings don't affect the outer environment
      (s2.compose(s1), bodyType, s2(s1(env)))
    
    case Exp.SeqExp(bodies) => ControlFlowInference.inferSeq(bodies, env)
    case Exp.While(cond, bodies) => ControlFlowInference.inferWhile(cond, bodies, env)
    
    case _ =>
      val (s, t) = infer(exp, env)
      (s, t, s(env))
  }
  
  def typeCheckFunction(func: Func, env: TypeEnvironment): TypeEnvironment = {
    val paramBindings = func.params.map {
      case (name, tpe) => name -> TypeScheme(Nil, tpe)
    }.toMap
    
    val bodyEnv = env ++ paramBindings
    val (s, bodyType, _) = inferWithEnv(func.body, bodyEnv)
    
    val s2 = unify(s(bodyType), s(func.returnType))
    val finalSubst = s2.compose(s)
    
    val inferredFunc = func.copy(
      params = func.params.map { case (name, tpe) => name -> finalSubst(tpe) },
      returnType = finalSubst(func.returnType)
    )
    
    env.withFunc(func.name, inferredFunc)
  }
  
  def typeCheckProgram(prog: Program): Type = {
    val initialFuncs = prog.functions.map(f => f.name -> f).toMap
    val initialEnv = TypeEnvironment.withFunctions(initialFuncs)
    
    val funcEnv = prog.functions.foldLeft(initialEnv) { (env, func) =>
      typeCheckFunction(func, env)
    }
    
    val (_, resultType, _) = prog.bodies.foldLeft((Substitution.empty, TInt: Type, funcEnv)) {
      case ((accSubst, _, env), expr) =>
        val (s, t, newEnv) = inferWithEnv(expr, accSubst(env))
        (s.compose(accSubst), t, newEnv)
    }
    
    resultType
  }
  
  // Compatibility wrappers
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