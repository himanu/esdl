// Written in the D programming language.

// Copyright: Coverify Systems Technology 2012 - 2015
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

module esdl.data.rand;

import esdl.data.obdd;

import std.traits: isIntegral, isBoolean, isArray, isStaticArray, isDynamicArray;
import esdl.data.bvec: isBitVector;
import std.algorithm : min, max;
import esdl.data.bstr;

import std.exception: enforce;
import std.range: ElementType;

/// C++ type static_cast for down-casting when you are sure
private import std.typetuple: staticIndexOf;
private import std.traits: BaseClassesTuple, ParameterTypeTuple; // required for staticIndexOf

static alias Unconst(T) = T;
static alias Unconst(T: const U, U) = U;

// For a given class, this template returns the Solver for first
// class in the ancestory that has Randomization mixin -- if there is
// none, returns _esdl__SolverRoot
template _esdl__SolverBase(T) {
  static if(is(T == class) &&
	    is(T B == super) &&
	    is(B[0] == class)) {
    alias U = B[0];
    // check if the base class has Randomization
    // static if(__traits(compiles, _esdl__SolverResolve!U)) {
    static if(__traits(compiles, U._esdl__thisHasRandomization()) &&
	      is(U == typeof(U._esdl__thisHasRandomization()))) {
      alias _esdl__SolverBase = _esdl__SolverResolve!U.Type;
    }
    else {
      alias _esdl__SolverBase = _esdl__SolverBase!U;
    }
  }
  else {
    alias _esdl__SolverBase = _esdl__SolverRoot;
  }
}


mixin template _esdl__SolverMixin()
{
  class _esdl__Constraint(string _esdl__CstString, string NAME):
    Constraint!_esdl__CstString
  {
    this() {
      super(this.outer, NAME,
	    cast(uint) this.outer._esdl__cstsList.length);
    }
    // This mixin writes out the bdd functions after parsing the
    // constraint string at compile time
    // mixin(constraintXlate(_esdl__CstString));
    override CstBlock getCstExpr() {
      mixin("return this.outer._esdl__cst_func_" ~ NAME ~ "();");
    }
	
    debug(CONSTRAINTS) {
      pragma(msg, constraintXlate(_esdl__CstString));
    }
  }

  class _esdl__Constraint(string _esdl__CstString):
    Constraint!_esdl__CstString
  {
    this(string name) {
      super(this.outer, name, cast(uint) this.outer._esdl__cstsList.length);
    }
    // This mixin writes out the bdd functions after parsing the
    // constraint string at compile time
    mixin(constraintXlate(_esdl__CstString));
    debug(CONSTRAINTS) {
      pragma(msg, constraintXlate(_esdl__CstString));
    }
  }

  class _esdl__Constraint(string _esdl__CstString, size_t N):
    Constraint!_esdl__CstString
  {
    long[N] _withArgs;

    void withArgs(ARG...)(ARG values) if(allIntengral!ARG) {
      static assert(ARG.length == N);
      foreach(i, v; values) {
	_withArgs[i] = v;
      }
    }

    this(string name) {
      super(this.outer, name, cast(uint) this.outer._esdl__cstsList.length);
    }

    long _esdl__arg(size_t VAR)() {
      static assert(VAR < N, "Can not map specified constraint with argument: @" ~
		    VAR.stringof);
      return _withArgs[VAR];
    }

    // This mixin writes out the bdd functions after parsing the
    // constraint string at compile time
    mixin(constraintXlate(_esdl__CstString));
    debug(CONSTRAINTS) {
      pragma(msg, "// randomizeWith!\n");
      pragma(msg, constraintXlate(_esdl__CstString));
    }
  }

  void _esdl__with(string _esdl__CstString, ARG...)(ARG values) {
    auto cstWith = new _esdl__Constraint!(_esdl__CstString, ARG.length)("randWith");
    cstWith.withArgs(values);
    _esdl__cstWith = cstWith;
  }

  auto ref _esdl__vec(L)(ref L l, string name="unnamed") {
    import std.traits: isIntegral, isArray;
    static if (isIntegral!L || isBitVector!L) {
      // import std.stdio;
      // writeln("Creating VarVec, ", name);
      return new RndVec!(L, _esdl__norand, 0)(name, l);
    }
    else static if(isArray!L) {
      // import std.stdio;
      // writeln("Creating VarVecArr, ", name);
      return new RndVecArr!(L, _esdl__norand, 0)(name, l);
    }
    else {
      return l;
    }
   }

  auto const ref _esdl__vec(L)(const ref L l, string name="unnamed") {
    import std.traits: isIntegral, isArray;
    static if (isIntegral!L || isBitVector!L) {
      // import std.stdio;
      // writeln("Creating VarVec, ", name);
      return new RndVec!(L, _esdl__norand, 0)(name, l);
    }
    else static if(isArray!L) {
      // import std.stdio;
      // writeln("Creating VarVecArr, ", name);
      return new RndVecArr!(L, _esdl__norand, 0)(name, l);
    }
    else {
      return l;
    }
   }


  auto _esdl__vec(L)(L l, string name="unnamed") {
    import std.traits: isIntegral;
    import esdl.data.bvec: isBitVector;
    static if (isIntegral!L || isBitVector!L) {
      return new ConstVec!L(l);
    }
    else {
      return l;
    }
  }
  
  mixin(_esdl__randsMixin!_esdl__T);

  debug(CONSTRAINTS) {
    pragma(msg, "// _esdl__randsMixin!" ~ _esdl__T.stringof ~ "\n");
    pragma(msg, _esdl__randsMixin!_esdl__T);
  }
}

class _esdl__SolverStruct(_esdl__T): _esdl__SolverBase!_esdl__T
{
  _esdl__T* _esdl__outer;
  void _esdl__setValRef(ref _esdl__T outer) {
    if (_esdl__outer !is &outer) {
      _esdl__outer = &outer;
      _esdl__setObjOuter(true);
    }
  }
  this(uint seed, bool isSeeded, string name, ref _esdl__T outer,
	      _esdl__SolverRoot parent=null) {
    _esdl__outer = &outer;
    static if(_esdl__baseHasRandomization!_esdl__T) {
      super(seed, isSeeded, name, outer, parent);
    }
    else {
      super(seed, isSeeded, name, parent);
    }
    _esdl__initRands();
    _esdl__initCsts();
  }

  mixin _esdl__SolverMixin;
}

template _esdl__SolverResolve(T) {
  // static if(__traits(compiles, T._esdl__hasRandomization)) {
  static if(is(T == class)) {
    // alias _esdl__SolverResolve = T._esdl__Solver;
    alias Type          = T._esdl__Solver;
  }
  else {
    // alias _esdl__SolverResolve = _esdl__SolverStruct!T;
    alias Type          = _esdl__SolverStruct!T;
  }
}

T _esdl__staticCast(T, F)(const F from)
  if(is(F == class) && is(T == class)
     // make sure that F is indeed amongst the base classes of T
     && staticIndexOf!(F, BaseClassesTuple!T) != -1
     )
    in {
      // assert statement will not be compiled for production release
      assert((from is null) || cast(T)from !is null);
    }
body {
  return cast(T) cast(void*) from;
 }

enum _esdl__norand;

template rand(N...) {
  import std.typetuple;
  static if(CheckRandParams!N) {
    struct rand
    {
      static if(N.length > 0 && N[0] != false) {
	enum maxBounds = TypeTuple!N;
	alias ElementRand = rand!(N[0..$-1]);
      }
    }
  }
}

template CheckRandParams(N...) {
  static if(N.length == 1 && N[0] == false) {
    enum bool CheckRandParams = true;
  }
  else {
    enum bool CheckRandParams = CheckRandParamsLoop!N;
  }
}


// Make sure that all the parameters are of type size_t
template CheckRandParamsLoop(N...) {
  static if(N.length > 0) {
    import std.traits;
    static if(!is(typeof(N[0]) == bool) && // do not confuse bool as size_t
	      is(typeof(N[0]) : size_t)) {
      static assert(N[0] != 0, "Can not have arrays with size 0");
      static assert(N[0] > 0, "Can not have arrays with negative size");
      enum bool CheckRecurse = CheckRandParamsLoop!(N[1..$]);
      enum bool CheckRandParamsLoop = CheckRecurse;
    }
    else {
      static assert(false, "Only positive integral values are allowed as array dimensions");
      enum bool CheckRandParamsLoop = false;
    }
  }
  else {
    enum bool CheckRandParamsLoop = true;
  }
}

template _esdl__baseHasRandomization(T) {
  static if(is(T B == super)
	    && is(B[0] == class)) {
    alias U = B[0];
    static if(__traits(compiles, U._esdl__hasRandomization)) {
      enum bool _esdl__baseHasRandomization = true;
    }
    else {
      enum bool _esdl__baseHasRandomization = _esdl__baseHasRandomization!U;
    }
  }
    else {
      enum bool _esdl__baseHasRandomization = false;
    }
}

auto _esdl__logicOr(P, Q)(P p, Q q) {
  CstBddExpr _p;
  CstBddExpr _q;
  static if(is(P == bool)) {
    _p = new CstBddConst(p);
  }
  else {
    _p = p;
  }
  static if(is(Q == bool)) {
    _q = new CstBddConst(q);
  }
  else {
    _q = q;
  }
  return _p.logicOr(_q);
}

auto _esdl__logicAnd(P, Q)(P p, Q q) {
  CstBddExpr _p;
  CstBddExpr _q;
  static if(is(P == bool)) {
    _p = new CstBddConst(p);
  }
  else {
    _p = p;
  }
  static if(is(Q == bool)) {
    _q = new CstBddConst(q);
  }
  else {
    _q = q;
  }
  return p.logicAnd(q);
}


auto _esdl__lth(P, Q)(P p, Q q) {
  static if(is(P: CstVecExpr)) {
    return p.lth(q);
  }
  else static if(is(Q: CstVecExpr)) {
    return q.gte(q);
  }
  else static if((isBitVector!P || isIntegral!P) &&
		 (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p < q);
  }
}

auto _esdl__lte(P, Q)(P p, Q q) {
  static if(is(P: CstVecExpr)) {
    return p.lte(q);
  }
  else static if(is(Q: CstVecExpr)) {
    return q.gth(q);
  }
  else static if((isBitVector!P || isIntegral!P) &&
	    (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p <= q);
  }
}

auto _esdl__gth(P, Q)(P p, Q q) {
  static if(is(P: CstVecExpr)) {
    return p.gth(q);
  }
  else static if(is(Q: CstVecExpr)) {
    return q.lte(q);
  }
  else static if((isBitVector!P || isIntegral!P) &&
	    (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p > q);
  }
}

auto _esdl__gte(P, Q)(P p, Q q) {
  static if(is(P: CstVecExpr)) {
    return p.gte(q);
  }
  else static if(is(Q: CstVecExpr)) {
    return q.lth(q);
  }
  else static if((isBitVector!P || isIntegral!P) &&
	    (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p >= q);
  }
}

auto _esdl__equ(P, Q)(P p, Q q) {
  static if(is(P: CstVecExpr)) {
    return p.equ(q);
  }
  else static if(is(Q: CstVecExpr)) {
    return q.equ(q);
  }
  else static if((isBitVector!P || isIntegral!P) &&
	    (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p == q);
  }
}

auto _esdl__neq(P, Q)(P p, Q q) {
  static if(is(P: CstVecExpr)) {
    return p.neq(q);
  }
  else static if(is(Q: CstVecExpr)) {
    return q.neq(q);
  }
  else static if((isBitVector!P || isIntegral!P) &&
	    (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p != q);
  }
}

template _esdl__RandRandomize(T, int I=0)
{
  static if(I == T.tupleof.length) {
    enum _esdl__RandRandomize = "";
  }
  else {
    alias RAND = getRandAttr!(T, I);
    static if(! (__traits(isSame, RAND, _esdl__norand) ||
		 is(getRandAttr!(T, I) == rand!false))) {
      enum NAME = __traits(identifier, T.tupleof[I]);
      enum _esdl__RandRandomize =
	"    _esdl__" ~ NAME ~ ".doRandomize(solver);\n" ~
	_esdl__RandRandomize!(T, I+1);
    }
    else {
      enum _esdl__RandRandomize = _esdl__RandRandomize!(T, I+1);
    }
  }
}

template _esdl__RandInits(T, int I=0)
{
  static if(I == T.tupleof.length) {
    enum _esdl__RandInits = "";
  }
  else {
    alias RAND = getRandAttr!(T, I);
    static if(! (__traits(isSame, RAND, _esdl__norand) ||
		 is(getRandAttr!(T, I) == rand!false))) {
      enum NAME = __traits(identifier, T.tupleof[I]);
      enum II = I.stringof;
      alias L = typeof(T.tupleof[I]);
      static if(isBitVector!L || isIntegral!L) {
	enum _esdl__RandInits =
	  "    _esdl__" ~ NAME ~ " = new typeof(_esdl__" ~
	  NAME ~ ")(\"" ~ NAME ~ "\", this._esdl__outer.tupleof[" ~
	  II ~ "]);\n" ~ "    _esdl__randsList ~= _esdl__" ~
	  NAME ~ ";\n" ~ _esdl__RandInits!(T, I+1);
      }
      else static if(is(L == class)) {
	enum _esdl__RandInits =
	  "    assert(this._esdl__outer.tupleof[" ~ II ~
	  "] !is null);\n    _esdl__" ~ NAME ~
	  // using static classes now -- normal new therefor
	  // " = this._esdl__outer.tupleof[" ~ II ~
	  // "].new typeof(_esdl__" ~ NAME ~
	  " = new typeof(_esdl__" ~ NAME ~
	  ")(this._esdl__outer._esdl__randSeed, " ~
	  "this._esdl__outer._esdl__randSeeded, \"" ~
	  NAME ~ "\", this._esdl__outer.tupleof[" ~ II ~
	  "], this);\n    _esdl__randsList ~= _esdl__" ~ NAME ~
	  "._esdl__randsList;\n" ~ _esdl__RandInits!(T, I+1);
      }
      else static if(is(L == struct)) {
	enum _esdl__RandInits =
	  "    _esdl__" ~ NAME ~ " = new typeof(_esdl__" ~
	  NAME ~ ")(this._esdl__outer._esdl__randSeed, " ~
	  "this._esdl__outer._esdl__randSeeded, \"" ~
	  NAME ~ "\", this._esdl__outer.tupleof[" ~ II ~
	  "], this);\n    _esdl__randsList ~= _esdl__" ~ NAME ~
	  "._esdl__randsList;\n" ~ _esdl__RandInits!(T, I+1);
      }
      else static if(is(L == U*, U) && is(U == struct)) {
	enum _esdl__RandInits =
	  "    assert(this._esdl__outer.tupleof[" ~ II ~
	  "] !is null);\n" ~
	  "    _esdl__" ~ NAME ~ " = new typeof(_esdl__" ~
	  NAME ~ ")(this._esdl__outer._esdl__randSeed, " ~
	  "this._esdl__outer._esdl__randSeeded, \"" ~
	  NAME ~ "\", *(this._esdl__outer.tupleof[" ~ II ~
	  "]), this);\n    _esdl__randsList ~= _esdl__" ~ NAME ~
	  "._esdl__randsList;\n" ~ _esdl__RandInits!(T, I+1);
      }
      else {			/* Arrays */
	enum _esdl__RandInits =
	  "    _esdl__" ~ NAME ~ " = new typeof(_esdl__" ~ NAME ~
	  ")(\"" ~ NAME ~ "\", this._esdl__outer.tupleof[" ~ II ~
	  "]);\n" ~ "    _esdl__randsList ~= _esdl__" ~ NAME ~
	  ";\n" ~ _esdl__RandInits!(T, I+1);
      }
      /* else { */
      /* 	static assert(false, "Unhandled type: " ~ L.stringof); */
      /* } */
    }
    else {
      enum _esdl__RandInits = _esdl__RandInits!(T, I+1);
    }
  }
}

template _esdl__RandSetOuter(T, int I=0)
{
  static if(I == T.tupleof.length) {
    enum _esdl__RandSetOuter = "";
  }
  else static if(is(getRandAttr!(T, I) == rand!false)) {
      enum string _esdl__RandSetOuter = _esdl__RandSetOuter!(T, I+1);
    }
  else static if(! __traits(isSame, getRandAttr!(T, I), _esdl__norand)) {
      enum NAME = __traits(identifier, T.tupleof[I]);
      alias L = typeof(T.tupleof[I]);
      static if(isBitVector!L || isIntegral!L) {
	enum _esdl__RandSetOuter =
 	  "    _esdl__" ~ NAME ~
	  "._esdl__setValRef(this._esdl__outer.tupleof[" ~ I.stringof ~
	  "]);\n" ~ _esdl__RandSetOuter!(T, I+1);
      }
      else static if(is(L == class)) {
	enum _esdl__RandSetOuter =
	  "    assert(this._esdl__outer." ~ NAME ~
	  " !is null);\n    _esdl__" ~ NAME ~
	  "._esdl__setValRef(this._esdl__outer." ~ NAME ~
	  ");\n" ~ _esdl__RandSetOuter!(T, I+1);
      }
      else static if(is(L == struct)) {
	enum _esdl__RandSetOuter =
	  "    _esdl__" ~ NAME ~
	  "._esdl__setValRef(this._esdl__outer." ~ NAME ~
	  ");\n" ~ _esdl__RandSetOuter!(T, I+1);
      }
      else static if(is(L == U*, U) && is(U == struct)) {
	enum _esdl__RandSetOuter =
	  "    _esdl__" ~ NAME ~
	  "._esdl__setValRef(*(this._esdl__outer." ~ NAME ~
	  "));\n" ~ _esdl__RandSetOuter!(T, I+1);
      }
      else {			/* arrays */
	enum _esdl__RandSetOuter =
 	  "    _esdl__" ~ NAME ~
	  "._esdl__setValRef(this._esdl__outer.tupleof[" ~ I.stringof ~
	  "]);\n" ~ _esdl__RandSetOuter!(T, I+1);
	// static assert(false, "Unhandled type: " ~ L.stringof);
      }
    }
    else {
      enum _esdl__RandSetOuter = _esdl__RandSetOuter!(T, I+1);
    }
}


template _esdl__RandDeclVars(T, int I=0)
{
  static if(I == T.tupleof.length) {
    enum _esdl__RandDeclVars = "";
  }
  else static if(is(getRandAttr!(T, I) == rand!false)) {
    enum string _esdl__RandDeclVars = _esdl__RandDeclVars!(T, I+1);
  }
  else static if(__traits(isSame, getRandAttr!(T, I), _esdl__norand)) {
    enum string _esdl__RandDeclVars = _esdl__RandDeclVars!(T, I+1);
  }
  else {
    // pragma(msg, __traits(identifier, T.tupleof[I]));
    enum string _esdl__RandDeclVars =
      "  _esdl__Rand!(_esdl__T, " ~ I.stringof ~ ") _esdl__" ~
      __traits(identifier, T.tupleof[I]) ~	";\n" ~
      _esdl__RandDeclVars!(T, I+1);
  }
}

template _esdl__RandDeclFuncs(T, int I=0)
{
  static if(I == T.tupleof.length) {
    enum _esdl__RandDeclFuncs = "";
  }
  else static if(is(getRandAttr!(T, I) == rand!false)) {
      enum string _esdl__RandDeclFuncs = _esdl__RandDeclFuncs!(T, I+1);
    }
    else {
      alias L = typeof(T.tupleof[I]);
      enum NAME = __traits(identifier, T.tupleof[I]);
      // skip the constraints and _esdl__ variables
      static if(is(L f == Constraint!C, immutable (char)[] C) ||
		(NAME.length > 8 && NAME[0..7] == "_esdl__")) {
	enum _esdl__RandDeclFuncs = _esdl__RandDeclFuncs!(T, I+1);
      }
      else static if(is(getRandAttr!(T, I) == rand!false)) {
	  enum _esdl__RandDeclFuncs =
	    _esdl__RandDeclFuncs!(T, I+1);
	}
      else static if(__traits(isSame, getRandAttr!(T, I), _esdl__norand)) {
	// When NAME is used explicitly, D looks for access
	// permissions, this works fine if the _esdl__SolverRoot
	// template is instantiated as a mixin inside the class that
	// has mixin Randomization
	// We want to eliminate the need to mixin Randomization for
	// the classes/structs that are hierarchical, so we need to
	// use explicit tupleof[N], since that does not use access
	// permissions
	// enum _esdl__RandDeclFuncs =
	//   "  const auto " ~ NAME ~ "() { return this._esdl__outer." ~ NAME ~ "; }\n" ~
	  enum _esdl__RandDeclFuncs =
	    "  const ref auto " ~ NAME ~ "() { return this._esdl__outer.tupleof[" ~ I.stringof ~ "]; }\n" ~
	    _esdl__RandDeclFuncs!(T, I+1);
	}
	else {
	  enum _esdl__RandDeclFuncs =
	    "  auto " ~ NAME ~ "() { return _esdl__" ~ NAME ~ "; }\n" ~
	    _esdl__RandDeclFuncs!(T, I+1);
	}
    }
}

template _esdl__ConstraintsDefDecl(T)
{
  enum _esdl__ConstraintsDefDecl = "  Constraint!(_esdl__ConstraintDefaults!(_esdl__T, 0)) _esdl__defaultConstraint;\n";
}

template _esdl__ConstraintDefaults(T, int I=0)
{
  static if(I == T.tupleof.length) {
    enum _esdl__ConstraintDefaults = "";
  }
  else {
    alias randAttr = getRandAttr!(T, I);
    static if((! is(randAttr == rand!false)) &&
	      is(randAttr == rand!A, A...)) {
      enum string _esdl__ConstraintDefaults =
	_esdl__ConstraintDefaults!(__traits(identifier, T.tupleof[I]), 0, A) ~
	_esdl__ConstraintDefaults!(T, I+1);
    }
    else {
      enum string _esdl__ConstraintDefaults = _esdl__ConstraintDefaults!(T, I+1);
    }
      
  }
}

template _esdl__ConstraintDefaults(string NAME, int I, A...) {
  static if(A.length == 0) {
    enum string _esdl__ConstraintDefaults = "";
  }
  else {
    static if(I == 0) {
      enum string ARR = NAME;
    }
    else {
      enum J = I - 1;
      enum string ARR = "_esdl__elem_" ~ NAME ~ "_" ~ J.stringof;
    }
    enum string ELEM = "_esdl__elem_" ~ NAME ~ "_" ~ I.stringof;
    enum string _esdl__ConstraintDefaultsLength = ARR ~ ".length" ~ " <= "
      ~ A[0].stringof ~ ";\n";
    
    static if(A.length == 1) {
      enum string _esdl__ConstraintDefaults = _esdl__ConstraintDefaultsLength;
    }
    else {
      enum string _esdl__ConstraintDefaults = _esdl__ConstraintDefaultsLength
	~ "foreach(" ~ ELEM ~ "; " ~ ARR ~ ") {\n" ~
	_esdl__ConstraintDefaults!(NAME, I+1, A[1..$]) ~ "}";
    }
  }
}

template _esdl__ConstraintsDecl(T, int I=0)
{
  static if(I == T.tupleof.length) {
    enum _esdl__ConstraintsDecl = "";
  }
  else static if(is(getRandAttr!(T, I) == rand!false)) {
      enum string _esdl__ConstraintsDecl = _esdl__ConstraintsDecl!(T, I+1);
    }
    else {
      alias L = typeof(T.tupleof[I]);
      enum NAME = __traits(identifier, T.tupleof[I]);
      // skip the constraints and _esdl__ variables
      static if(is(L f == Constraint!C, immutable (char)[] C)) {
	enum CST = _esdl__constraintString!(T, I);
	enum _esdl__ConstraintsDecl =
	  "  enum string _esdl__string_" ~ NAME ~
	  " = _esdl__constraintString!(_esdl__T, " ~ I.stringof ~ ");\n" ~
	  cast(string) constraintXlate(CST, "_esdl__cst_func_" ~ NAME) ~
	  "  Constraint!(_esdl__string_" ~ NAME ~ ") " ~
	  NAME ~ ";\n" ~ _esdl__ConstraintsDecl!(T, I+1);
      }
      else {
	enum _esdl__ConstraintsDecl = _esdl__ConstraintsDecl!(T, I+1);
      }
    }
}

template _esdl__constraintString(T, int I)
{
  alias L = typeof(T.tupleof[I]);
  static if(is(L f == Constraint!C, immutable (char)[] C)) {
    enum string _esdl__constraintString = C;
  }
  else {
    static assert(false);
  }
}

template _esdl__CstInits(T, int I=0)
{
  static if(I == T.tupleof.length) {
    enum _esdl__CstInits = "    _esdl__defaultConstraint = new _esdl__Constraint!(_esdl__ConstraintDefaults!(_esdl__T, 0))(\"_esdl__defaultConstraint\");
    _esdl__cstsList ~= _esdl__defaultConstraint;\n";
  }
  else static if(is(getRandAttr!(T, I) == rand!false)) {
      enum string _esdl__CstInits = _esdl__CstInits!(T, I+1);
    }
    else {
      alias L = typeof(T.tupleof[I]);
      enum string NAME = __traits(identifier, T.tupleof[I]);
      static if(is(L f == Constraint!C, immutable (char)[] C)) {
	enum string _esdl__CstInits = "    " ~ NAME ~
	  " = new _esdl__Constraint!(_esdl__string_" ~ NAME ~
	  ",\"" ~ NAME ~ "\")();\n    _esdl__cstsList ~= " ~
	  NAME ~ // ";\n    this._esdl__outer." ~ NAME ~ " = " ~ NAME ~
	  ";\n" ~ _esdl__CstInits!(T, I+1);
      }
      else static if(hasRandAttr!(T, I) && is(L == class)) {
	  enum string _esdl__CstInits = "    _esdl__cstsList ~= " ~ NAME ~
	    "._esdl__cstsList;\n" ~ _esdl__CstInits!(T, I+1);
	}
	else {
	  enum string _esdl__CstInits = _esdl__CstInits!(T, I+1);
	}
    }
}


// generates the code for rand structure inside the class object getting
// randomized
template _esdl__ListRands(T, int I=0) {
  import std.typetuple;
  static if(I == T.tupleof.length) {
    alias _esdl__ListRands = TypeTuple!();
  }
  else {
    static if(hasRandAttr!(T, I)) {
      alias _esdl__ListRands = TypeTuple!(T, I, _esdl__ListRands!(T, I+1));
    }
    else {
      alias _esdl__ListRands = _esdl__ListRands!(T, I+1);
    }
  }
}

template hasRandAttr(T, int I=0) {
  enum hasRandAttr = hasRandInList!(__traits(getAttributes, T.tupleof[I]));
}

template hasRandInList(A...) {
  static if(A.length == 0) {
    enum bool hasRandInList = false;
  }
  else static if(__traits(isSame, A[0], rand) ||
		 is(A[0] unused: rand!M, M...)) {
      enum bool hasRandInList = true;
    }
    else {
      enum bool hasRandInList = hasRandInList!(A[1..$]);
    }
}

template getRandAttr(T, string R) {
  alias getRandAttr = scanRandAttr!(__traits(getAttributes,
					     __traits(getMember, T, R)));
}

template getRandAttrN(alias R, int N) {
  static if(is(R == rand!A, A...)) {
    static assert(A.length > N);
    enum int getRandAttrN = A[N];
  }
  else {
    static assert(false);
  }
}

template getRandAttr(T, int I, int N) {
  alias Attr = getRandAttr!(T, I);
  static if(is(Attr == rand!A, A...)) {
    static assert(A.length > N);
    enum int getRandAttr = A[N];
  }
  else {
    static assert(false);
  }
}

template getRandAttr(T, int I) {
  alias getRandAttr = scanRandAttr!(__traits(getAttributes, T.tupleof[I]));
}

template scanRandAttr(A...) {
  static if(A.length == 0) {
    alias scanRandAttr = _esdl__norand;
  }
  else static if(__traits(isSame, A[0], rand) ||
		 is(A[0] unused: rand!M, M...)) {
      static assert(__traits(isSame, scanRandAttr!(A[1..$]), _esdl__norand));
      alias scanRandAttr = A[0];
    }
    else {
      alias scanRandAttr = scanRandAttr!(A[1..$]);
    }
}

template isVarSigned(L) {
  import std.traits: isIntegral, isSigned;
  static if(isBitVector!L)
    enum bool isVarSigned = L.ISSIGNED;
  else static if(isIntegral!L)
	 enum bool isVarSigned = isSigned!L;
    else
    static assert(false, "isVarSigned: Can not determine sign of type " ~
		  typeid(L));
}


void _esdl__randomize(T) (T t, _esdl__ConstraintBase withCst = null) {
  static if(__traits(compiles, t.preRandomize())) {
    t.preRandomize();
  }

  t._esdl__initSolver();
  if(withCst is null && t._esdl__solverInst._esdl__cstWith !is null) {
    t._esdl__solverInst._esdl__cstWith = withCst;
    t._esdl__solverInst._esdl__cstWithChanged = true;
  }
  else {
    t._esdl__solverInst._esdl__cstWithChanged = false;
  }

  useBuddy(t._esdl__solverInst._esdl__buddy);

  foreach(rnd; t._esdl__solverInst._esdl__randsList) {
    rnd._esdl__reset();
  }

  t._esdl__solverInst.solve(t);

  t._esdl__solverInst.doRandomize(t._esdl__solverInst);

  // _esdl__setRands(t, t._esdl__solverInst._esdl__randsList,
  //		  t._esdl__solverInst._esdl__rGen);

  static if(__traits(compiles, t.postRandomize())) {
    t.postRandomize();
  }
}

abstract class _esdl__ConstraintBase
{
  this(_esdl__SolverRoot eng, string name, string constraint, uint index) {
    _cstEng = eng;
    _name = name;
    _constraint = constraint;
    _index = index;
  }
  immutable @rand!false string _constraint;
  protected @rand!false bool _enabled = true;
  protected @rand!false _esdl__SolverRoot _cstEng;
  protected @rand!false string _name;
  // index in the constraint Database
  protected @rand!false uint _index;

  bool isEnabled() {
    return _enabled;
  }

  void enable() {
    _enabled = true;
  }

  void disable() {
    _enabled = false;
  }

  BDD getConstraintBDD() {
    BDD retval = _cstEng._esdl__buddy.one();
    return retval;
  }

  string name() {
    return _name;
  }

  abstract CstBlock getCstExpr();
}

static char[] constraintXlate(string CST, string NAME="") {
  import esdl.data.cstx;
  CstParser parser = CstParser(CST);
  return parser.translate(NAME);
}

abstract class Constraint(string C): _esdl__ConstraintBase
{
  this(_esdl__SolverRoot eng, string name, uint index) {
    super(eng, name, C, index);
  }
};

class _esdl__RandGen
{
  import std.random;
  import esdl.data.bvec;

  private Random _gen;

  private Bit!32 _bv;

  private ubyte _bi = 32;

  this(uint _seed) {
    _gen = Random(_seed);
  }

  void seed(uint _seed) {
    _gen.seed(_seed);
  }

  bool flip() {
    if(_bi > 31) {
      _bi = 0;
      _bv = uniform!"[]"(0, uint.max, _gen);
    }
    return cast(bool) _bv[_bi++];
  }

  double get() {
    return uniform(0.0, 1.0, _gen);
  }

  @property T gen(T)() {
    static if(isIntegral!T || isBoolean!T) {
      T val = uniform!(T)(_gen);
      return val;
    }
    else static if(isBitVector!T) {
	T val;
	val.randomize(_gen);
	return val;
      }
      else {
	static assert(false);
      }
  }

  @property void gen(T)(ref T t) {
    static if(isIntegral!T || isBoolean!T) {
      t = uniform!(T)(_gen);
    }
    else static if(isBitVector!T) {
	t.randomize(_gen);
      }
      else {
	static assert(false);
      }
  }

  @property auto gen(T1, T2)(T1 a, T2 b)
    if(isIntegral!T1 && isIntegral!T2) {
      return uniform(a, b, _gen);
    }

  @property void gen(T, T1, T2)(ref T t, T1 a, T2 b)
    if(isIntegral!T1 && isIntegral!T2) {
      t = uniform(a, b, _gen);
    }
}

// Todo -- Make it a struct
class CstStage {
  int _id = -1;
  // List of randomized variables associated with this stage. A
  // variable can be associated with only one stage
  CstVecPrim[] _rndVars;
  // The Bdd expressions that apply to this stage
  CstBddExpr[] _bddExprs;
  // These are unresolved idx variables
  RndIterBase[] _idxVars;
  // These are the length variables that this stage will solve
  // CstVecPrim[] _preReqs;
  CstBddExpr[] _bddExprsWithUnmetReqs;
  
  BDD _solveBDD;
  ~this() {
    _solveBDD.reset();
  }
  
  void id(uint i) {
    _id = i;
  }

  uint id() {
    return _id;
  }

  bool solved() {
    if(_id != -1) return true;
    else return false;
  }

  // returns true if there are idx variables that need solving
  bool hasIdxs() {
    foreach(idx; _idxVars) {
      if(! idx.isUnrollable()) return true;
    }
    return false;
  }

  bool allReqsResolved() {
    CstBddExpr[] bddExprsWithUnmetReqs;
    foreach(expr; _bddExprsWithUnmetReqs) {
      if(expr.preReqs().length !=0) {
	// import std.stdio;
	// writeln(expr.name ~ " needs preReqs resolution, ", expr.preReqs());
	bddExprsWithUnmetReqs ~= expr;
      }
    }
    _bddExprsWithUnmetReqs = bddExprsWithUnmetReqs;
    return _bddExprsWithUnmetReqs.length == 0;
  }
}

abstract class _esdl__SolverRoot {
  // Keep a list of constraints in the class
  _esdl__ConstraintBase[] _esdl__cstsList;
  _esdl__ConstraintBase _esdl__cstWith;
  bool _esdl__cstWithChanged;

  // ParseTree parseList[];
  CstVecPrim[] _esdl__randsList;
  _esdl__RandGen _esdl__rGen;

  _esdl__RandGen _esdl__getRandGen() {
    return _esdl__rGen;
  }

  Buddy _esdl__buddy;

  BddDomain[] _domains;
  // BddDomain* _domains;

  _esdl__SolverRoot _parent = null;

  bool _esdl__isSeeded = false;

  CstBlock _esdl__cstStatements;

  CstStage[] savedStages;
  
  this(uint seed, bool isSeeded, string name,
       _esdl__SolverRoot parent=null) {
    debug(NOCONSTRAINTS) {
      assert(false, "Constraint engine started");
    }
    _esdl__rGen = new _esdl__RandGen(seed);
    _esdl__isSeeded = isSeeded;
    if(parent is null) {
      _esdl__buddy = new Buddy(400, 400);
      _esdl__cstStatements = new CstBlock();
    }
    else {
      _parent = parent;
    }
  }

  ~this() {
    _esdl__cstsList.length   = 0;
    _esdl__cstWith           = null;
    _esdl__cstWithChanged    = true;
    _esdl__randsList.length  = 0;
  }

  void _esdl__initRands() {}
  void _esdl__initCsts() {}

  void doRandomize(_esdl__SolverRoot solver) {}

  // list of constraint statements to solve at a given stage
  void addCstStage(CstVecPrim prim, ref CstStage[] cstStages) {
    if(prim !is null) {
      if(prim.stage() is null) {
	CstStage stage = new CstStage();
	cstStages ~= stage;
	prim.stage = stage;
	stage._rndVars ~= prim;
	// cstStages[stage]._rndVars ~= prim;
      }
    }
    else {
      // FIXME -- just to check is Prim can ever be null
      // If this assert statement is not hit, take out the
      // if condition
      assert(false, "Prim can be null -- kill this assert");
    }
  }

  void addCstStage(CstBddExpr expr, ref CstStage[] cstStages) {
    // uint stage = cast(uint) _cstStages.length;
    auto vecs = expr.getSolvables();
    CstStage stage;
    foreach(ref vec; vecs) {
      if(vec !is null) {
	if(vec.stage() is null) {
	  if(stage is null) {
	    stage = new CstStage();
	    cstStages ~= stage;
	  }
	  vec.stage = stage;
	  stage._rndVars ~= vec;
	  // cstStages[stage]._rndVars ~= vec;
	}
	if(stage !is vec.stage()) { // need to merge stages
	  mergeCstStages(stage, vec.stage(), cstStages);
	  stage = vec.stage();
	}
      }
      else {
	// FIXME -- just to check is vec can ever be null
	// If this assert statement is not hit, take out the
	// if condition
	assert(false, "Vec can be null -- kill this assert");
      }
    }
    stage._bddExprs ~= expr;
    stage._bddExprsWithUnmetReqs = stage._bddExprs;
  }

  void mergeCstStages(CstStage fromStage, CstStage toStage,
			     ref CstStage[] cstStages) {
    if(fromStage is null) {
      // fromStage has not been created yet
      return;
    }
    foreach(ref vec; fromStage._rndVars) {
      vec.stage = toStage;
    }
    toStage._rndVars ~= fromStage._rndVars;
    toStage._bddExprs ~= fromStage._bddExprs;
    if(cstStages[$-1] is fromStage) {
      cstStages.length -= 1;
    }
    else {
      fromStage._rndVars.length = 0;
      fromStage._bddExprs.length = 0;
    }
  }

  void initDomains(T)(T t) {
    if(_domains.length is 0 || _esdl__cstWithChanged is true) {
      uint domIndex = 0;
      int[] domList;

      _esdl__cstStatements._esdl__reset(); // start empty

      // take all the constraints -- even if disabled
      foreach(ref _esdl__ConstraintBase cst; _esdl__cstsList) {
	_esdl__cstStatements ~= cst.getCstExpr();
      }

      if(_esdl__cstWith !is null) {
	_esdl__cstStatements ~= _esdl__cstWith.getCstExpr();
      }

      foreach(stmt; _esdl__cstStatements._exprs) {
        foreach(vec; stmt.getRndPrims()) {
	  if(vec.domIndex != uint.max) {
	    vec.domIndex = uint.max;
	  }
        }
      }

      foreach(stmt; _esdl__cstStatements._exprs) {
	foreach(vec; stmt.getRndPrims()) {
	  if(vec.domIndex == uint.max) {
	    vec.domIndex = domIndex++;
	    domList ~= vec.bitcount;
	  }
	}
      }

      _esdl__buddy.clearAllDomains();
      _domains = _esdl__buddy.extDomain(domList);

    }
  }
  
  void solve(T)(T t) {
    // writeln("Solving BDD for number of constraints = ", _esdl__cstsList.length);

    initDomains(t);

    // _esdl__cstStatements._esdl__reset(); // start empty
    
    // take all the constraints -- even if disabled
    if (_esdl__cstStatements.isEmpty()) {
      foreach(ref _esdl__ConstraintBase cst; _esdl__cstsList) {
	_esdl__cstStatements ~= cst.getCstExpr();
      }
      if(_esdl__cstWith !is null) {
	_esdl__cstStatements ~= _esdl__cstWith.getCstExpr();
      }
    }

    // First we solve the constraint groups that are responsible for
    // setting the length of the rand!n dynamic arrays. After each
    // such constraint group is resolved, we go back and expand the
    // constraint expressions that depend on the IDX Variables.

    // Once we have unrolled all the IDXS, we go ahead and resolve
    // everything that remains.


    CstStage[] unsolvedStages;	// unresolved stages -- all

    int stageIdx=0;
    CstBddExpr[] unsolvedExprs = _esdl__cstStatements._exprs;	// unstaged Expressions -- all
    while(unsolvedExprs.length > 0 || unsolvedStages.length > 0) {
      CstBddExpr[] cstExprs = unsolvedExprs;
      unsolvedExprs.length = 0;
      CstStage[] cstStages = unsolvedStages;
      unsolvedStages.length = 0;

      CstBddExpr[] urExprs;	// unrolled expressions
      // unroll all the unrollable expressions
      foreach(expr; cstExprs) {
	if(expr.unrollableIdx() is null) {
	  // import std.stdio;
	  // writeln("Looking at: ", expr.name());
	  urExprs ~= expr;
	}
	else {
	  // import std.stdio;
	  // writeln("Unrolling: ", expr.name());
	  auto unrolled = expr.unroll();
	  // for (size_t i=0; i!=unrolled.length; ++i)
	  //   {
	  //     writeln("Unrolled as: ", unrolled[i].name());
	  //   }
	  urExprs ~= unrolled;
	}
      }

      foreach(expr; urExprs) {
	if(expr.idxVars().length is 0) {
	  // import std.stdio;
	  // writeln("Adding expression ", expr.name(), " to staged");
	  addCstStage(expr, cstStages);
	}
	else {
	  unsolvedExprs ~= expr;
	}
      }

      foreach(stage; cstStages) {
	if(stage !is null &&
	   // stage._rndVars.length !is 0 &&
	   stage.allReqsResolved()) {
	  solveStage(stage, stageIdx);
	}
	else {
	  // assert(stage._rndVars.length !is 0);
	  unsolvedStages ~= stage;
	}
      }
      
    }
  }

  void solveStage(CstStage stage, ref int stageIdx) {
    import std.conv;
    // initialize the bdd vectors
    BDD solveBDD = _esdl__buddy.one();
    foreach(vec; stage._rndVars) {
      if(vec.stage is stage) {
	if(vec.bddvec.isNull()) {
	  vec.bddvec = _esdl__buddy.buildVec(_domains[vec.domIndex], vec.signed);
	}
	// BDD primBdd = vec.getPrimBdd(_esdl__buddy);
	// if(! primBdd.isOne()) {
	//   solveBDD = solveBDD & primBdd;
	// }
      }
    }

    // make the bdd tree
    auto exprs = stage._bddExprs;
    bool refreshed = false;
    foreach (expr; exprs) {
      refreshed |= expr.refresh(stage, _esdl__buddy);
    }
    if ((! refreshed) &&
	savedStages.length > stageIdx &&
	savedStages[stageIdx]._bddExprs == stage._bddExprs) {
      stage._solveBDD = savedStages[stageIdx]._solveBDD;
      solveBDD = stage._solveBDD;
    }
    else {
      foreach(expr; exprs) {
	// import std.stdio;
	// writeln(expr.name());
	solveBDD = solveBDD & expr.getBDD(stage, _esdl__buddy);
	// writeln(expr.name());
      }
      stage._solveBDD = solveBDD;
    }

    double[uint] bddDist;
    solveBDD.satDist(bddDist);

    auto solution = solveBDD.randSatOne(this._esdl__rGen.get(),
					bddDist);
    auto solVecs = solution.toVector();

    byte[] bits;
    if(solVecs.length != 0) {
      bits = solVecs[0];
    }

    foreach(vec; stage._rndVars) {
      ulong value;
      enum WORDSIZE = 8 * value.sizeof;
      auto bitvals = solveBDD.getIndices(vec.domIndex);
      foreach(uint i, ref j; bitvals) {
	uint pos = i % WORDSIZE;
	uint word = i / WORDSIZE;
	if(bits.length == 0 || bits[j] == -1) {
	  value = value + ((cast(size_t) _esdl__rGen.flip()) << pos);
	}
	else if(bits[j] == 1) {
	  value = value + ((cast(ulong) 1) << pos);
	}
	if(pos == WORDSIZE - 1 || i == bitvals.length - 1) {
	  vec.value(value, word);
	  value = 0;
	}
      }
    }
    stage.id(stageIdx);

    // save for future reference
    while (savedStages.length <= stageIdx) {
      savedStages ~= new CstStage();
    }
    assert(savedStages[stageIdx] !is stage);
    auto swapping = stage;
    stage = savedStages[stageIdx];
    savedStages[stageIdx++] = swapping;
  }

  void printSolution() {
    // import std.stdio;
    // writeln("There are solutions: ", _theBDD.satCount());
    // writeln("Distribution: ", dist);
    // auto randSol = _theBDD.randSat(randGen, dist);
    // auto solution = _theBDD.fullSatOne();
    // solution.printSetWith_Domains();
  }
}


// generates the code for rand structure inside the class object getting
// randomized
string _esdl__randsMixin(T)() {
  T t;
  string randsMixin;

  // _esdl__initRands and _esdl__initCsts are templatized to make sure
  // that it is not overridable
  string rand_randrand =
    "  override void doRandomize(_esdl__SolverRoot solver) {\n" ~
    "    super.doRandomize(solver);\n" ~
    _esdl__RandRandomize!T ~ "  }\n";
  string rand_inits =
    "  void _esdl__initRands()() {\n" ~
    _esdl__RandInits!T ~ "  }\n";
  string cst_inits =
    "  void _esdl__initCsts()() {\n" ~
    _esdl__CstInits!T ~ "  }\n";
  string rand_decls = _esdl__RandDeclFuncs!T ~ _esdl__RandDeclVars!T;
  string rand_set_outer = "  void _esdl__setObjOuter()(bool changed) {\n" ~
    _esdl__RandSetOuter!T ~ "  }\n";
  string cst_decls = _esdl__ConstraintsDefDecl!T ~ _esdl__ConstraintsDecl!T;

  randsMixin = rand_decls ~ rand_inits ~ cst_inits ~ cst_decls ~
    rand_set_outer ~ rand_randrand;

  return randsMixin;
}

class Randomizable {
  mixin Randomization;
}

mixin template Randomization()
{
  alias _esdl__T = typeof(this);
  
  // While making _esdl__Solver class non-static nested class also
  // works as far as dlang compilers are concerned, do not do that
  // since in that case _esdl__outer object would have an implicit
  // pointer to the outer object which can not be changed
  static class _esdl__Solver: _esdl__SolverBase!_esdl__T
  {
    _esdl__T _esdl__outer;
    void _esdl__setValRef()(_esdl__T outer) {
      if (_esdl__outer !is outer) {
	_esdl__outer = outer;
	_esdl__setObjOuter(true);
      }
    }
    this(uint seed, bool isSeeded, string name, _esdl__T outer,
	 _esdl__SolverRoot parent=null) {
      _esdl__outer = outer;
      static if(_esdl__baseHasRandomization!_esdl__T) {
	super(seed, isSeeded, name, outer, parent);
      }
      else {
	super(seed, isSeeded, name, parent);
      }
      _esdl__initRands();
      _esdl__initCsts();
    }

    mixin _esdl__SolverMixin;
  }

  enum bool _esdl__hasRandomization = true;
  alias _esdl__Type = typeof(this);

  static _esdl__Type _esdl__thisHasRandomization()() {
    return null;
  }

  alias _esdl__SolverThis = _esdl__SolverResolve!_esdl__Type.Type;

  // final auto _esdl__randEval(string NAME)() {
  //   return mixin(NAME);
  // }

  static if(// is(_esdl__T: Randomizable) ||
	    __traits(compiles, _esdl__solverInst)) {
    override void _esdl__virtualRandomize(_esdl__ConstraintBase withCst = null) {
      _esdl__randomize(this, withCst);
    }
    override _esdl__SolverThis _esdl__getSolver() {
      return _esdl__staticCast!_esdl__SolverThis(_esdl__solverInst);
    }
    override void _esdl__initSolver() {
      if (_esdl__solverInst is null) {
	_esdl__solverInst =
	  new _esdl__SolverThis(_esdl__randSeed, _esdl__randSeeded,
				typeid(_esdl__Type).stringof[8..$-1], this);
	static if(__traits(compiles, _esdl__setupSolver())) {
	  _esdl__setupSolver();
	}
      }
      else {
	_esdl__getSolver()._esdl__setObjOuter(false);
      }
    }
  }
  else {
    @rand!false _esdl__SolverRoot _esdl__solverInst;
    @rand!false uint _esdl__randSeed;
    @rand!false bool _esdl__randSeeded;
    _esdl__SolverThis _esdl__getSolver() {
      return _esdl__staticCast!_esdl__SolverThis(_esdl__solverInst);
    }
    void _esdl__virtualRandomize(_esdl__ConstraintBase withCst = null) {
      _esdl__randomize(this, withCst);
    }
    void useThisBuddy() {
      import esdl.data.obdd;
      useBuddy(_esdl__solverInst._esdl__buddy);
    }
    void seedRandom(int seed) {
      _esdl__randSeed = seed;
      _esdl__randSeeded = true;
      if(_esdl__solverInst !is null) {
	_esdl__solverInst._esdl__rGen.seed(seed);
      }
    }
    bool _esdl__isRandSeeded() {
      return _esdl__randSeeded;
    }
    alias srandom = seedRandom;	// SV names the similar method srandom
    void _esdl__initSolver() {
      if (_esdl__solverInst is null) {
	_esdl__solverInst =
	  new _esdl__SolverThis(_esdl__randSeed, _esdl__randSeeded,
				typeid(_esdl__Type).stringof[8..$-1], this);
	static if(__traits(compiles, _esdl__setupSolver())) {
	  _esdl__setupSolver();
	}
      }
      else {
	_esdl__getSolver()._esdl__setObjOuter(false);
      }
    }
  }
  // static if(_esdl__baseHasRandomization!_esdl__Type) {
  // }
}

void randomizeWith(string C, T, ARG...)(ref T t, ARG values)
  if(is(T == class) && allIntengral!ARG) {
    t._esdl__initSolver();
    // The idea is that if the end-user has used the randomization
    // mixin then _esdl__RandType would be already available as an
    // alias and we can use virtual randomize method in such an
    // eventuality.
    // static if(is(typeof(t._esdl__RandType) == T)) {
    if(t._esdl__solverInst._esdl__cstWith is null ||
       t._esdl__solverInst._esdl__cstWith._constraint != C) {
      t._esdl__getSolver()._esdl__with!(C)(values);
      t._esdl__solverInst._esdl__cstWithChanged = true;
      // auto withCst =
      //	new Constraint!(C, "_esdl__withCst",
      //			T, ARG.length)(t, "_esdl__withCst");
      // withCst.withArgs(values);
      // t._esdl__solverInst._esdl__cstWith = withCst;
    }
    else {
      alias CST = _esdl__SolverResolve!T.Type._esdl__Constraint!(C, ARG.length);
      auto cstWith = _esdl__staticCast!CST(t._esdl__solverInst._esdl__cstWith);
      cstWith.withArgs(values);
      t._esdl__solverInst._esdl__cstWithChanged = false;
    }
    t._esdl__virtualRandomize(t._esdl__solverInst._esdl__cstWith);
  }

void randomize(T)(T t) {
  // FIXME
  // first check if the there are @rand or Constraint definitions but
  // missing mixin Randomization for some of the hierarchies
  t._esdl__virtualRandomize();
}

// FIXME add bitvectors to this template filter
template allIntengral(ARG...) {
  static if(ARG.length == 0) {
    enum bool allIntengral = true;
  }
  else static if(isIntegral!(ARG[0])) {
      enum bool allIntengral = allIntengral!(ARG[1..$]);
    }
    else enum bool allIntengral = false;
}

// All the operations that produce a BddVec
enum CstBinVecOp: byte
  {   AND,
      OR ,
      XOR,
      ADD,
      SUB,
      MUL,
      DIV,
      REM,
      LSH,
      RSH,
      BITINDEX,
      }

// All the operations that produce a Bdd
enum CstBinBddOp: byte
  {   LTH,
      LTE,
      GTH,
      GTE,
      EQU,
      NEQ,
      }

interface CstVecPrim
{
  abstract string name();
  // abstract void doRandomize(_esdl__SolverRoot solver);
  abstract bool isRand();
  // abstract ulong value();

  abstract void value(ulong v, int word=0);
  abstract CstStage stage();
  abstract void stage(CstStage s);
  abstract void _esdl__reset();
  abstract bool isVecArr();
  abstract uint domIndex();
  abstract void domIndex(uint s);
  abstract uint bitcount();
  abstract bool signed();
  abstract BddVec bddvec();
  abstract void bddvec(BddVec b);
  abstract CstVecPrim[] getPrimLens();
  abstract void solveBefore(CstVecPrim other);
  abstract void addPreRequisite(CstVecPrim other);

  // this method is used for getting implicit constraints that are required for
  // dynamic arrays and for enums
  abstract BDD getPrimBdd(Buddy buddy);
  abstract void resetPrimeBdd();
  final bool solved() {
    if(isRand()) {
      return stage() !is null && stage().solved();
    }
    else {
      return true;
    }
  }
}


// proxy class for reading in the constraints lazily
// An abstract class that returns a vector on evaluation
abstract class CstVecExpr
{
  // alias toBdd this;

  // alias evaluate this;

  abstract string name();
  
  CstBddExpr toBdd() {
    auto zero = new ConstVec!int(0);
    return new CstVec2BddExpr(this, zero, CstBinBddOp.NEQ);
  }

  // Array of indexes this expression has to resolve before it can be
  // convertted into an BDD
  abstract RndIterBase[] idxVars();

  // List of Array Variables
  abstract CstVecPrim[] preReqs();

  bool isConst() {
    return false;
  }

  // get all the primary bdd vectors that constitute a given bdd
  // expression
  // The idea here is that we need to solve all the bdd vectors of a
  // given constraint statement together. And so, given a constraint
  // equation, we want to list out the elements that need to be
  // grouped together.
  abstract CstVecPrim[] getRndPrims();

  // get all the primary bdd vectors that would be solved together
  CstVecPrim[] getSolvables() {
    return getRndPrims();
  }
  
  // get the list of stages this expression should be avaluated in
  // abstract CstStage[] getStages();
  abstract BddVec getBDD(CstStage stage, Buddy buddy);

  // refresh the _valvec if the current value is not the same as previous value
  abstract bool refresh(CstStage stage, Buddy buddy);

  abstract long evaluate();

  abstract CstVecExpr unroll(RndIterBase l, uint n);

  CstVec2VecExpr opBinary(string op)(CstVecExpr other)
  {
    static if(op == "&") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.AND);
    }
    static if(op == "|") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.OR);
    }
    static if(op == "^") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.XOR);
    }
    static if(op == "+") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.ADD);
    }
    static if(op == "-") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.SUB);
    }
    static if(op == "*") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.MUL);
    }
    static if(op == "/") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.DIV);
    }
    static if(op == "%") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.REM);
    }
    static if(op == "<<") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.LSH);
    }
    static if(op == ">>") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.RSH);
    }
  }

  CstVec2VecExpr opBinary(string op, Q)(Q q)
    if(isBitVector!Q || isIntegral!Q)
      {
  	auto qq = new ConstVec!Q(q);
  	static if(op == "&") {
  	  return new CstVec2VecExpr(this, qq, CstBinVecOp.AND);
  	}
  	static if(op == "|") {
  	  return new CstVec2VecExpr(this, qq, CstBinVecOp.OR);
  	}
  	static if(op == "^") {
  	  return new CstVec2VecExpr(this, qq, CstBinVecOp.XOR);
  	}
  	static if(op == "+") {
  	  return new CstVec2VecExpr(this, qq, CstBinVecOp.ADD);
  	}
  	static if(op == "-") {
  	  return new CstVec2VecExpr(this, qq, CstBinVecOp.SUB);
  	}
  	static if(op == "*") {
  	  return new CstVec2VecExpr(this, qq, CstBinVecOp.MUL);
  	}
  	static if(op == "/") {
  	  return new CstVec2VecExpr(this, qq, CstBinVecOp.DIV);
  	}
  	static if(op == "%") {
  	  return new CstVec2VecExpr(this, qq, CstBinVecOp.REM);
  	}
  	static if(op == "<<") {
  	  return new CstVec2VecExpr(this, qq, CstBinVecOp.LSH);
  	}
  	static if(op == ">>") {
  	  return new CstVec2VecExpr(this, qq, CstBinVecOp.RSH);
  	}
      }

  CstVec2VecExpr opBinaryRight(string op, Q)(Q q)
    if(isBitVector!Q || isIntegral!Q)
      {
	auto qq = new ConstVec!Q(q);
	static if(op == "&") {
	  return new CstVec2VecExpr(qq, this, CstBinVecOp.AND);
	}
	static if(op == "|") {
	  return new CstVec2VecExpr(qq, this, CstBinVecOp.OR);
	}
	static if(op == "^") {
	  return new CstVec2VecExpr(qq, this, CstBinVecOp.XOR);
	}
	static if(op == "+") {
	  return new CstVec2VecExpr(qq, this, CstBinVecOp.ADD);
	}
	static if(op == "-") {
	  return new CstVec2VecExpr(qq, this, CstBinVecOp.SUB);
	}
	static if(op == "*") {
	  return new CstVec2VecExpr(qq, this, CstBinVecOp.MUL);
	}
	static if(op == "/") {
	  return new CstVec2VecExpr(qq, this, CstBinVecOp.DIV);
	}
	static if(op == "%") {
	  return new CstVec2VecExpr(qq, this, CstBinVecOp.REM);
	}
	static if(op == "<<") {
	  return new CstVec2VecExpr(qq, this, CstBinVecOp.LSH);
	}
	static if(op == ">>") {
	  return new CstVec2VecExpr(qq, this, CstBinVecOp.RSH);
	}
      }

  CstVecExpr opIndex(CstVecExpr index)
  {
    // assert(false, "Index operation defined only for Arrays");
    return new CstVecSliceExpr(this, index);
  }

  CstVecExpr opSlice(P)(P p)
    if(isIntegral!P || isBitVector!P) {
      return new CstVecSliceExpr(this, new ConstVec!P(p));
    }

  CstVecExpr opSlice(CstVecExpr lhs, CstVecExpr rhs)
  {
    return new CstVecSliceExpr(this, lhs, rhs);
  }

  CstVecExpr opSlice(P, Q)(P p, Q q)
    if((isIntegral!P || isBitVector!P) && (isIntegral!Q || isBitVector!Q)) {
      return new CstVecSliceExpr(this, new ConstVec!P(p),
				 new ConstVec!Q(q));
    }

  CstVec2BddExpr lth(Q)(Q q)
    if(isBitVector!Q || isIntegral!Q) {
      auto qq = new ConstVec!Q(q);
      return this.lth(qq);
    }

  CstVec2BddExpr lth(CstVecExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.LTH);
  }

  CstVec2BddExpr lte(Q)(Q q)
    if(isBitVector!Q || isIntegral!Q) {
      auto qq = new ConstVec!Q(q);
      return this.lte(qq);
    }

  CstVec2BddExpr lte(CstVecExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.LTE);
  }

  CstVec2BddExpr gth(Q)(Q q)
    if(isBitVector!Q || isIntegral!Q) {
      auto qq = new ConstVec!Q(q);
      return this.gth(qq);
    }

  CstVec2BddExpr gth(CstVecExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.GTH);
  }

  CstVec2BddExpr gte(Q)(Q q)
    if(isBitVector!Q || isIntegral!Q) {
      auto qq = new ConstVec!Q(q);
      return this.gte(qq);
    }

  CstVec2BddExpr gte(CstVecExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.GTE);
  }

  CstVec2BddExpr equ(Q)(Q q)
    if(isBitVector!Q || isIntegral!Q) {
      auto qq = new ConstVec!Q(q);
      return this.equ(qq);
    }

  CstVec2BddExpr equ(CstVecExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.EQU);
  }

  CstVec2BddExpr neq(Q)(Q q)
    if(isBitVector!Q || isIntegral!Q) {
      auto qq = new ConstVec!Q(q);
      return this.neq(qq);
    }

  CstVec2BddExpr neq(CstVecExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.NEQ);
  }

  CstNotBddExpr opUnary(string op)() {
    static if(op == "*") {	// "!" in cstx is translated as "*"
      return new CstNotBddExpr(this.toBdd());
    }
  }

  CstBdd2BddExpr implies(CstBddExpr other) {
    return new CstBdd2BddExpr(this.toBdd(), other, CstBddOp.LOGICIMP);
  }

  // CstBdd2BddExpr implies(CstVecExpr other)
  // {
  //   return new CstBdd2BddExpr(this.toBdd(), other.toBdd(), CstBddOp.LOGICIMP);
  // }

  CstBdd2BddExpr logicOr(CstBddExpr other) {
    return new CstBdd2BddExpr(this.toBdd(), other, CstBddOp.LOGICOR);
  }

  // CstBdd2BddExpr logicOr(CstVecExpr other)
  // {
  //   return new CstBdd2BddExpr(this.toBdd(), other.toBdd(), CstBddOp.LOGICOR);
  // }

  CstBdd2BddExpr logicAnd(CstBddExpr other) {
    return new CstBdd2BddExpr(this.toBdd(), other, CstBddOp.LOGICAND);
  }

  // CstBdd2BddExpr logicAnd(CstVecExpr other)
  // {
  //   return new CstBdd2BddExpr(this.toBdd(), other.toBdd(), CstBddOp.LOGICAND);
  // }

  bool isOrderingExpr() {
    return false;		// only CstVecOrderingExpr return true
  }

}


mixin template EnumConstraints(T) {
  static if(is(T == enum)) {
    BDD _primBdd;
    override BDD getPrimBdd(Buddy buddy) {
      // return this.bddvec.lte(buddy.buildVec(_maxValue));
      import std.traits;
      if(_primBdd.isZero()) {
	_primBdd = buddy.zero();
	foreach(e; EnumMembers!T) {
	  _primBdd = _primBdd | this.bddvec.equ(buddy.buildVec(e));
	}
      }
      return _primBdd;
    }
    override void resetPrimeBdd() {
      _primBdd.reset();
    }
  }
}

template _esdl__Rand(T, int I)
{
  import std.traits;
  alias L = typeof(T.tupleof[I]);
  alias RAND = getRandAttr!(T, I);
  static if(__traits(isSame, RAND, _esdl__norand)) {
    static if(isArray!L) {
      alias _esdl__Rand = RndVecArr!(L, _esdl__norand, 0);
    }
    else static if(isBitVector!L || isIntegral!L) {
      alias _esdl__Rand = RndVec!(L, _esdl__norand, 0);
    }
    else static if(is(L == class) || is(L == struct)) {
      alias _esdl__Rand = _esdl__SolverResolve!L.Type;
    }
    else static if(is(L == U*, U) && is(U == struct)) {
      alias _esdl__Rand = _esdl__SolverResolve!U.Type;
    }
  }
  else {
    static if(isArray!L) {
      alias _esdl__Rand = RndVecArr!(L, RAND, 0);
    }
    else static if(isBitVector!L || isIntegral!L) {
      alias _esdl__Rand = RndVec!(L, RAND, 0);
    }
    else static if(is(L == class) || is(L == struct)) {
      alias _esdl__Rand = _esdl__SolverResolve!L.Type;
    }
    else static if(is(L == U*, U) && is(U == struct)) {
      alias _esdl__Rand = _esdl__SolverResolve!U.Type;
    }
  }
}

template ElementTypeN(T, int N=0)
{
  import std.range;		// ElementType
  static if(N==0) {
    alias ElementTypeN = T;
  }
  else {
    alias ElementTypeN = ElementTypeN!(ElementType!T, N-1);
  }
}

template _esdl__ArrOrder(T, int N=0) {
  import std.traits;
  import std.range;
  static if(isArray!T) {
    enum int _esdl__ArrOrder = 1 + _esdl__ArrOrder!(ElementType!T) - N;
  }
  else {
    enum int _esdl__ArrOrder = 0;
  }
}

template _esdl__ArrOrder(T, int I, int N=0) {
  enum int _esdl__ArrOrder = _esdl__ArrOrder!(typeof(T.tupleof[I])) - N;
}

// This class represents an unrolled Foreach idx at vec level
abstract class RndIterBase: CstVecExpr
{
  string _name;

  override string name() {
    return name;
  }

  this(string name) {
    _name = name;
  }

  // _idxVar will point to the array this RndIterBase is tied to

  uint maxVal();

  // this will not return the arrVar since the length variable is
  // not getting constrained here
  override CstVecPrim[] preReqs() {
    return [];
  }

  bool isUnrollable();

  // get all the primary bdd vectors that constitute a given bdd expression
  override CstVecPrim[] getRndPrims();

  // get the list of stages this expression should be avaluated in
  // override CstStage[] getStages() {
  //   return arrVar.arrLen.getStages();
  // }

  override bool refresh(CstStage s, Buddy buddy) {
    assert(false, "Can not refresh for a Idx Variable without unrolling");
  }
  
  override BddVec getBDD(CstStage stage, Buddy buddy) {
    assert(false, "Can not getBDD for a Idx Variable without unrolling");
  }

  override long evaluate() {
    assert(false, "Can not evaluate a Idx Variable without unrolling");
  }

}

// Consolidated Proxy Class
// template RndVecBase(T, int I, int N=0) {
//   alias RndVecBase = RndVecBase!(typeof(T.tupleof[I]),
// 				     getRandAttr!(T, I), N);
// }

abstract class RndVecBase(V, alias R, int N)
  if(_esdl__ArrOrder!(V, N) == 0): CstVecExpr, CstVecPrim
{
  enum HAS_RAND_ATTRIB = (! __traits(isSame, R, _esdl__norand));

  alias E = ElementTypeN!(V, N);
  alias RV = typeof(this);
  // enum int ORDER = _esdl__ArrOrder!(T, I, N);

  string _name;
  BddVec _valvec;
  

  static if (HAS_RAND_ATTRIB) {
    mixin EnumConstraints!E;

    CstVecPrim[] _preReqs;
    BddVec       _domvec;
    E            _val;

    uint         _domIndex = uint.max;
    CstStage     _stage = null;
  }
  else {
    Unconst!E _val;
  }

  ~this() {
    resetPrimeBdd();
    static if (HAS_RAND_ATTRIB) {
      _domvec.reset();
    }
    _valvec.reset();
  }

  override string name() {
    return _name;
  }

  void _esdl__reset() {
    static if (HAS_RAND_ATTRIB) {
      stage = null;
    }
  }

  bool isVecArr() {
    return false;
  }

  BDD getPrimBdd(Buddy buddy) {
    return buddy.one();
  }

  void resetPrimeBdd() { }
  
  abstract ulong value();
  
  override long evaluate() {
    static if (HAS_RAND_ATTRIB) {
      if(! this.isRand || stage().solved()) {
	return value();
      }
      else {
	import std.conv;
	assert(false, "Rand variable " ~ _name ~
	       " evaluation in wrong stage: " ~ stage()._id.to!string);
      }
    }
    else {
      return value();
    }
  }

  bool isRand() {
    static if (HAS_RAND_ATTRIB) {
      return true;
    }
    else {
      return false;
    }
  }

  CstStage stage() {
    static if (HAS_RAND_ATTRIB) {
      return _stage;
    }
    else {
      assert(false);
    }
  }

  void stage(CstStage s) {
    static if (HAS_RAND_ATTRIB) {
      _stage = s;
    }
    else {
      assert(false);
    }
  }

  uint domIndex() {
    static if (HAS_RAND_ATTRIB) {
      return _domIndex;
    }
    else {
      assert(false);
    }
  }

  void domIndex(uint s) {
    static if (HAS_RAND_ATTRIB) {
      _domIndex = s;
    }
    else {
      assert(false);
    }
  }

  BddVec bddvec() {
    static if (HAS_RAND_ATTRIB) {
      return _domvec;
    }
    else {
      return _valvec;
    }
  }

  void bddvec(BddVec b) {
    static if (HAS_RAND_ATTRIB) {
      _domvec = b;
    }
    else {
      assert(false);
    }
  }

  uint bitcount() {
    static if(isIntegral!E)        return E.sizeof * 8;
    else static if(isBitVector!E)  return E.SIZE;
    else static assert(false, "bitcount can not operate on: " ~ E.stringof);
  }

  bool signed() {
    static if(isVarSigned!E) {
      return true;
    }
    else  {
      return false;
    }
  }

  S to(S)()
    if (is(S == string)) {
      import std.conv;
      static if (HAS_RAND_ATTRIB) {
	if (isRand) {
	  return "RAND#" ~ _name ~ ":" ~ value().to!string();
	}
	else {
	  return "VAL#" ~ _name ~ ":" ~ value().to!string();
	}
      }
      else {
	return "VAR#" ~ _name ~ ":" ~ value().to!string();
      }
    }

  override string toString() {
    return this.to!string();
  }

  void solveBefore(CstVecPrim other) {
    static if (HAS_RAND_ATTRIB) {
      other.addPreRequisite(this);
    }
    else {
      assert(false);
    }
  }

  void addPreRequisite(CstVecPrim prim) {
    static if (HAS_RAND_ATTRIB) {
      _preReqs ~= prim;
    }
    else {
      assert(false);
    }
  }

  void build() {}

  abstract E getVal();

  private bool refreshVal(Buddy buddy) {
    auto val = getVal();
    if ((! _valvec.isNull) && _val == val) {
      return false;
    }
    else {
      _val = val;
      _valvec = buddy.buildVec(val);
      return true;
    }
  }
  
  override bool refresh(CstStage s, Buddy buddy) {
    static if (HAS_RAND_ATTRIB) {
      assert(stage(), "Stage not set for " ~ this.name());
      if(this.isRand && s is stage()) {
	return false;
      }
      else if((! this.isRand) ||
	      this.isRand && stage().solved()) { // work with the value
	return refreshVal(buddy);
      }
      else {
	assert(false, "Constraint evaluation in wrong stage");
      }
    }
    else {
      return refreshVal(buddy);
    }
  }
  
  override BddVec getBDD(CstStage s, Buddy buddy) {
    static if (HAS_RAND_ATTRIB) {
      assert(stage(), "Stage not set for " ~ this.name());
      if(this.isRand && s is stage()) {
	return _domvec;
      }
      else if((! this.isRand) ||
	      this.isRand && stage().solved()) { // work with the value
	return _valvec;
      }
      else {
	assert(false, "Constraint evaluation in wrong stage");
      }
    }
    else {
      return _valvec;
    }
  }
}

// T represents the type of the declared array/non-array member
// N represents the level of the array-elements we have to traverse
// for the elements this RndVec represents
template RndVec(T, int I, int N=0)
  if(_esdl__ArrOrder!(T, I, N) == 0) {
  alias RndVec = RndVec!(typeof(T.tupleof[I]),
			     getRandAttr!(T, I), N);
}

class RndVec(V, alias R, int N) if(N == 0 && _esdl__ArrOrder!(V, N) == 0):
  RndVecBase!(V, R, N)
    {
      import std.traits;
      import std.range;
      import esdl.data.bvec;

      V* _var;

    
      this(string name, ref V var) {
	// import std.stdio;
	// writeln("New ", name);
	_name = name;
	_var = &var;
      }

      void _esdl__setValRef(ref V var) {
	_var = &var;
      }
      
      override CstVecPrim[] preReqs() {
	static if (HAS_RAND_ATTRIB) {
	  return _preReqs;
	}
	else {
	  return [];
	}
      }

      override RndIterBase[] idxVars() {
	return [];
      }

      override CstVecPrim[] getRndPrims() {
	static if (HAS_RAND_ATTRIB) {
	  if(isRand) return [this];
	  else return [];
	}
	else {
	  return [];
	}
      }

      CstVecPrim[] getPrimLens() {
	assert(false);
      }

      override RV unroll(RndIterBase l, uint n) {
	// idxVars is always empty
	return this;
      }

      void doRandomize(_esdl__SolverRoot solver) {
	static if (HAS_RAND_ATTRIB) {
	  if(stage is null) {
	    solver._esdl__getRandGen().gen(*_var);
	  }
	}
	else {
	  assert(false);
	}
      }

      override E getVal() {
	return *_var;
      }

      override ulong value() {
	return cast(long) (*_var);
      }

      void value(ulong v, int word = 0) {
	static if (HAS_RAND_ATTRIB) {
	  static if(isIntegral!V) {
	    if(word == 0) {
	      *_var = cast(V) v; // = cast(V) toBitVec(v      }
	    }
	    else {
	      static if(size_t.sizeof == 4 && (is(V == long) || is(V == ulong))) {
		assert(word == 1);	// 32 bit machine with long integral
		V val = v;
		val = val << (8 * size_t.sizeof);
		*_var += val;
	      }
	      else {
		assert(false, "word has to be 0 for integrals");
	      }
	    }
	  }
	  else {
	    _var._setNthWord(v, word); // = cast(V) toBitVec(v);
	  }
	}
	else {
	  assert(false);
	}
      }
    }

class RndVec(V, alias R, int N=0) if(N != 0 && _esdl__ArrOrder!(V, N) == 0):
  RndVecBase!(V, R, N)
    {
      import std.traits;
      import std.range;
      import esdl.data.bvec;

      alias P = RndVecArr!(V, R, N-1);
      P _parent;

      CstVecExpr _indexExpr = null;
      int _index = 0;

      this(string name, P parent, CstVecExpr indexExpr) {
	_name = name;
	_parent = parent;
	_indexExpr = indexExpr;
      }

      this(string name, P parent, uint index) {
	_name = name;
	_parent = parent;
	_index = index;
      }

      override CstVecPrim[] preReqs() {
	if(_indexExpr) {
	  static if (HAS_RAND_ATTRIB) {
	    return _preReqs ~ _parent.arrLen() ~
	      _parent.preReqs() ~ _indexExpr.preReqs();
	  }
	  else {
	    return _parent.preReqs() ~ _indexExpr.preReqs();
	  }
	}
	else {
	  static if (HAS_RAND_ATTRIB) {
	    return _preReqs ~ _parent.arrLen() ~ _parent.preReqs();
	  }
	  else {
	    return _parent.preReqs();
	  }
	}
      }

      override RndIterBase[] idxVars() {
	if(_indexExpr) {
	  return _parent.idxVars() ~ _indexExpr.idxVars();
	}
	else {
	  return _parent.idxVars();
	}
      }

      CstVecPrim[] getPrimLens() {
	assert(false);
      }

      // What is required here
      // we could be dealing with an _index or an _indexExpr. Further
      // the indexExpr could be either a solvable constraint expression
      // or an array length iterator that iterates over the length of
      // the given array. To add to the complexity here, we could have a
      // case where the given element has a parent that too a
      // non-elementary one (involving non-integer indexes). In such
      // cases we need to list all the elements of the array that could
      // finally represent the given element that we are currently
      // dealing with.
      override CstVecPrim[] getRndPrims() {
	CstVecPrim[] prims;
	static if (HAS_RAND_ATTRIB) {
	  if(_indexExpr) {
	    // FIXME -- if the expression has been solved
	    // return _parent.getRndPrims(_indexExpr.evaluate()) ;
	    prims = _indexExpr.getRndPrims();
	    foreach(pp; _parent.getRndPrims(-1)) {
	      prims ~= pp;
	    }
	  }
	  else {
	    foreach(pp; _parent.getRndPrims(_index)) {
	      prims ~= pp;
	    }
	  }
	  return prims;
	}
	else {
	  if(_indexExpr) {
	    // FIXME -- if the expression has been solved
	    // return _parent.getRndPrims(_indexExpr.evaluate()) ;
	    prims = _indexExpr.getRndPrims() ~ _parent.getRndPrims();
	  }
	  else {
	    // foreach(pp; _parent.getRndPrims(_index)) {
	    //   prims ~= pp;
	    // }
	    prims = _parent.getRndPrims();
	  }
	  return prims;
	}
      }

      override RV unroll(RndIterBase l, uint n) {
	bool idx = false;
	foreach(var; idxVars()) {
	  if(l is var) {
	    idx = true;
	    break;
	  }
	}
	if(! idx) return this;
	else {
	  if(_indexExpr) {
	    auto vec = _parent.unroll(l,n)[_indexExpr.unroll(l,n)];
	    // import std.stdio;
	    // writeln(_indexExpr.name(), " has been unrolled as: ",
	    // 	  _indexExpr.unroll(l,n).name());
	    return vec;
	    // return _parent.unroll(l,n)[_indexExpr.unroll(l,n)];
	  }
	  else {
	    return _parent.unroll(l,n)[_index];
	  }
	}
      }

      void doRandomize(_esdl__SolverRoot solver) {
	static if (HAS_RAND_ATTRIB) {
	  if(stage is null) {
	    E val;
	    solver._esdl__getRandGen().gen(val);
	    value(val);
	  }
	}
	else {
	  assert(false, name());
	}
      }

      override E getVal() {
	if(_indexExpr) {
	  return _parent.getVal(cast(size_t) _indexExpr.evaluate());
	}
	else {
	  return _parent.getVal(this._index);
	}
      }

      override ulong value() {
	if(_indexExpr) {
	  return _parent.getVal(_indexExpr.evaluate());
	}
	else {
	  return _parent.getVal(this._index);
	}
      }

      void value(ulong v, int word = 0) {
	static if (HAS_RAND_ATTRIB) {
	  // import std.stdio;
	  // writeln("Setting value of ", this.name(), " to: ", v);
	  // writeln("Parent length value of ", this.name(),
	  // 	      " is: ", _parent.getLen());
	  if(_indexExpr) {
	    _parent.setVal(v, word, cast(size_t) _indexExpr.evaluate());
	  }
	  else {
	    return _parent.setVal(v, word, _index);
	  }
	}
	else {
	  assert(false);
	}
      }
    }

// Arrays (Multidimensional arrays as well)
// template RndVecArrBase(T, int I, int N=0)
//   if(_esdl__ArrOrder!(T, I, N) != 0) {
//   alias RndVecArrBase = RndVecArrBase!(typeof(T.tupleof[I]),
// 					   getRandAttr!(T, I), N);
// }

abstract class RndVecArrBase(V, alias R, int N=0)
  if(_esdl__ArrOrder!(V, N) != 0): CstVecPrim
{
  enum HAS_RAND_ATTRIB = (! __traits(isSame, R, _esdl__norand));

  alias L = ElementTypeN!(V, N);
  alias E = ElementTypeN!(V, N+1);
  enum int ORDER = _esdl__ArrOrder!(V, N);

  static if(ORDER > 1) {
    alias EV = RndVecArr!(V, R, N+1);
  }
  else {
    alias EV = RndVec!(V, R, N+1);
  }

  EV[] _elems;

  string _name;
  override string name() {
    return _name;
  }

  size_t maxArrLen() {
    static if (HAS_RAND_ATTRIB) {
      static if(isStaticArray!L) {
	return L.length;
      }
      else {
	return getRandAttrN!(R, N);
      }
    }
    else {
      return L.length;
    }
  }

  static if (HAS_RAND_ATTRIB) {
    CstVecPrim[] _preReqs;

    void opIndexAssign(EV c, size_t idx) {
      _elems[idx] = c;
    }
  }

  bool isVecArr() {
    return true;
  }

  BDD getPrimBdd(Buddy buddy) {
    return buddy.one();
  }

  void resetPrimeBdd() { }

  // override CstVec2VecExpr opIndex(CstVecExpr idx) {
  //   return new CstVec2VecExpr(this, idx, CstBinVecOp.IDXINDEX);
  // }

  bool isRand() {
    assert(false, "isRand not implemented for RndVecArrBase");
  }

  ulong value() {
    assert(false, "value not implemented for RndVecArrBase");
  }

  void value(ulong v, int word = 0) {
    assert(false, "value not implemented for RndVecArrBase");
  }

  void stage(CstStage s) {
    assert(false, "stage not implemented for RndVecArrBase");
  }

  uint domIndex() {
    assert(false, "domIndex not implemented for RndVecArrBase");
  }

  void domIndex(uint s) {
    assert(false, "domIndex not implemented for RndVecArrBase");
  }

  uint bitcount() {
    assert(false, "bitcount not implemented for RndVecArrBase");
  }

  bool signed() {
    assert(false, "signed not implemented for RndVecArrBase");
  }

  BddVec bddvec() {
    assert(false, "bddvec not implemented for RndVecArrBase");
  }

  void bddvec(BddVec b) {
    assert(false, "bddvec not implemented for RndVecArrBase");
  }

  void solveBefore(CstVecPrim other) {
    static if (HAS_RAND_ATTRIB) {
      other.addPreRequisite(this);
    }
    else {
      assert(false);
    }
  }

  void addPreRequisite(CstVecPrim prim) {
    static if (HAS_RAND_ATTRIB) {
      _preReqs ~= prim;
    }
    else {
      assert(false);
    }
  }

}

template RndVecArr(T, int I, int N=0)
  if(_esdl__ArrOrder!(T, I, N) != 0) {
  alias RndVecArr = RndVecArr!(typeof(T.tupleof[I]),
				   getRandAttr!(T, I), N);
}

// Arrays (Multidimensional arrays as well)
class RndVecArr(V, alias R, int N=0)
  if(N == 0 && _esdl__ArrOrder!(V, N) != 0):
    RndVecArrBase!(V, R, N)
      {
	alias RV = typeof(this);
	RndLen!RV _arrLen;

	alias RAND=R;

	V* _var;

	void _esdl__setValRef(ref V var) {
	  _var = &var;
	}
      
	static if (HAS_RAND_ATTRIB) {
	  static if(isStaticArray!V) {
	    static assert(__traits(isSame, R, rand));
	    enum int maxLen = V.length;
	    this(string name, ref V var) {
	      _name = name;
	      _var = &var;
	      _arrLen = new RndLen!RV(name ~ ".len", this);
	    }
	  }

	  static if(isDynamicArray!V) {
	    enum int maxLen = getRandAttrN!(R, N);
	    this(string name, ref V var) {
	      _name = name;
	      _var = &var;
	      _arrLen = new RndLen!RV(name ~ ".len", this);
	    }
	  }
	}
	else {
	  this(string name, ref V var) {
	    _name = name;
	    _var = &var;
	    _arrLen = new RndLen!RV(name ~ ".len", this);
	  }
	}

	CstVecPrim[] preReqs() {
	  static if (HAS_RAND_ATTRIB) {
	    return _preReqs;		// N = 0 -- no _parent
	  }
	  else {
	    return [];
	  }
	}

	RndIterBase[] idxVars() {
	  return [];
	}

	static if (HAS_RAND_ATTRIB) {
	  EV[] getRndPrims(int idx) {
	    if(idx < 0) return _elems;
	    else return [_elems[idx]];
	  }
	}

	CstVecPrim[] getRndPrims() {
	  static if (HAS_RAND_ATTRIB) {
	    CstVecPrim[] prims;
	    foreach(elem; _elems) {
	      prims ~= elem;
	    }
	    return prims;
	  }
	  else {
	    return [];
	  }
	}

	CstVecPrim[] getPrimLens() {
	  static if (HAS_RAND_ATTRIB) {
	    CstVecPrim[] prims;
	    if(_arrLen.isRand) prims ~= _arrLen;
	    return prims;
	  }
	  else {
	    return [];
	  }
	}
    
	RV unroll(RndIterBase l, uint n) {
	  return this;
	}

	static private auto getVal(A, N...)(ref A arr, N idx)
	  if(isArray!A && N.length > 0 && isIntegral!(N[0])) {
	    static if(N.length == 1) return arr[idx[0]];
	    else {
	      return getVal(arr[idx[0]], idx[1..$]);
	    }
	  }

	auto getVal(J...)(J idx) if(isIntegral!(J[0])) {
	  return getVal(*_var, cast(size_t) idx);
	}

	static if (HAS_RAND_ATTRIB) {
	  static private void setVal(A, N...)(ref A arr, ulong v, int word, N idx)
	    if(isArray!A && N.length > 0 && isIntegral!(N[0])) {
	      static if(N.length == 1) {
		alias AE = ElementType!A;
		static if(isIntegral!AE) {
		  if(word == 0) {
		    arr[idx[0]] = cast(AE) v;
		  }
		  else {
		    // static if(size_t.sizeof == 4) {
		    //   assert(word == 1);	// 32 bit machine with long integral
		    //   AE val = v;
		    //   val = val << (8 * size_t.sizeof);
		    //   arr[idx[0]] += val;
		    // }
		    // else {
		    assert(false, "word has to be 0 for integrals");
		  }
		}
		else {
		  arr[idx[0]]._setNthWord(v, word);
		}
	      }
	      else {
		setVal(arr[idx[0]], v, word, idx[1..$]);
	      }
	    }

	  static private void setLen(A, N...)(ref A arr, size_t v, N idx)
	    if(isArray!A) {
	      static if(N.length == 0) {
		static if(isDynamicArray!A) {
		  arr.length = v;
		  // import std.stdio;
		  // writeln(arr, " idx: ", N.length);
		}
		else {
		  assert(false, "Can not set length of a fixed length array");
		}
	      }
	      else {
		// import std.stdio;
		// writeln(arr, " idx: ", N.length);
		setLen(arr[idx[0]], v, idx[1..$]);
	      }
	    }
	}
	else {
	  void setLen(N...)(size_t v, N idx) {
	    // setLen(*_var, v, idx);
	    assert(false, "Can not set value for VarVecArr");
	  }
	}

	void setVal(J...)(ulong v, int word, J idx) if(isIntegral!(J[0])) {
	  static if (HAS_RAND_ATTRIB) {
	    setVal(*_var, v, word, idx);
	  }
	  else {
	    assert(false);
	  }
	}

	static private size_t getLen(A, N...)(ref A arr, N idx) if(isArray!A) {
	  static if(N.length == 0) return arr.length;
	  else {
	    return getLen(arr[idx[0]], idx[1..$]);
	  }
	}

	size_t getLen(N...)(N idx) {
	  return getLen(*_var, idx);
	}

	void setLen(N...)(size_t v, N idx) {
	  static if (HAS_RAND_ATTRIB) {
	    setLen(*_var, v, idx);
	  }
	  else {
	    assert(false);
	  }
	}

	EV opIndex(CstVecExpr idx) {
	  if(idx.isConst()) {
	    assert(_elems.length > 0, "_elems for expr " ~ this.name() ~
		   " have not been built");
	    // if(idx.evaluate() >= _elems.length || idx.evaluate() < 0 || _elems is null) {
	    // 	import std.stdio;
	    // 	writeln(this.name(), ":", idx.evaluate());
	    // }
	    // import std.stdio;
	    // writeln(idx.evaluate());
	    return _elems[cast(size_t) idx.evaluate()];
	  }
	  else {
	    static if(isStaticArray!E) {
	      // static array
	      return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	    }
	    else static if(isDynamicArray!E) {
	      // dynamic array
	      return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	    }
	    else {
	      return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	    }
	  }
	}

	EV opIndex(size_t idx) {
	  build();
	  assert(_elems[idx]._indexExpr is null);
	  return _elems[idx];
	}

	void doRandomize(_esdl__SolverRoot solver) {
	  static if (HAS_RAND_ATTRIB) {
	    if(_elems.length == 0) this.build();
	    assert(arrLen !is null);
	    for(size_t i=0; i != arrLen.evaluate(); ++i) {
	      this[i].doRandomize(solver);
	    }
	  }
	  else {
	    assert(false);
	  }
	}

	auto elements() {
	  this.build();
	  auto idx = arrLen.makeIdxVar();
	  return this[idx];
	}

	bool built() {
	  return (_elems.length == maxArrLen() &&
		  _elems[0] !is null);
	}
    
	void build() {
	  if(built()) return;
	  _elems.length = maxArrLen();
	  // static if(isIntegral!E || isBitVector!E) {
	  // if(! built()) {
	  for (uint i=0; i!=maxArrLen; ++i) {
	    if(_elems[i] is null) {
	      import std.conv: to;
	      _elems[i] = new EV(_name ~ "[" ~ i.to!string() ~ "]", this, i);
	      if(_elems[i].isVecArr()) {
		_elems[i].build();
	      }
	    }
	  }
	}
	
	auto iterator() {
	  this.build();
	  auto idx = arrLen.makeIdxVar();
	  return idx;
	}

	RndLen!RV length() {
	  return _arrLen;
	}

	RndLen!RV arrLen() {
	  return _arrLen;
	}

	void _esdl__reset() {
	  _arrLen.stage = null;
	  foreach(elem; _elems) {
	    if(elem !is null) {
	      elem._esdl__reset();
	    }
	  }
	}

	CstStage stage() {
	  return arrLen().stage();
	}

      }

class RndVecArr(V, alias R, int N=0) if(N != 0 && _esdl__ArrOrder!(V, N) != 0):
  RndVecArrBase!(V, R, N)
    {
      import std.traits;
      import std.range;
      import esdl.data.bvec;
      alias P = RndVecArr!(V, R, N-1);
      P _parent;
      CstVecExpr _indexExpr = null;
      int _index = 0;

      alias RV = typeof(this);
      RndLen!RV _arrLen;

      alias RAND=R;
      
      this(string name, P parent, CstVecExpr indexExpr) {
	_name = name;
	_parent = parent;
	_indexExpr = indexExpr;
	_arrLen = new RndLen!RV(name ~ ".len", this);
      }

      this(string name, P parent, uint index) {
	_name = name;
	_parent = parent;
	_index = index;
	_arrLen = new RndLen!RV(name ~ ".len", this);
      }

      CstVecPrim[] preReqs() {
	CstVecPrim[] req;
	static if (HAS_RAND_ATTRIB) {
	  req = _preReqs ~ _parent.arrLen();
	}
	if(_indexExpr) {
	  return req ~ _indexExpr.preReqs() ~ _parent.preReqs();
	}
	else {
	  return req ~ _parent.preReqs();
	}
      }

      RndIterBase[] idxVars() {
	if(_indexExpr) {
	  return _parent.idxVars() ~ _indexExpr.idxVars();
	}
	else {
	  return _parent.idxVars();
	}
      }

      EV[] getRndPrims(int idx) {
	EV[] prims;
	static if (HAS_RAND_ATTRIB) {
	  if(_indexExpr) {
	    foreach(pp; _parent.getRndPrims(-1)) {
	      if(idx < 0) prims ~= pp._elems;
	      else prims ~= pp._elems[idx];
	    }
	  }
	  else {
	    foreach(pp; _parent.getRndPrims(_index)) {
	      if(idx < 0) prims ~= pp._elems;
	      else prims ~= pp._elems[idx];
	    }
	  }
	}
	return prims;
      }
    
      // This is slightly tricky in case we are pointing directly to
      // just one element of the array, this function should return just
      // that element. But it could be that the index or an upper
      // hierarchy index is not a constant, but an iterator or a
      // randomized epression. In that case, we shall have to return
      // more primary elements
      CstVecPrim[] getRndPrims() {
	CstVecPrim[] prims;
	static if (HAS_RAND_ATTRIB) {
	  if(_indexExpr) {
	    prims = _indexExpr.getRndPrims();
	    foreach(pp; _parent.getRndPrims(-1)) {
	      prims ~= pp;
	    }
	  }
	  else {
	    foreach(pp; _parent.getRndPrims(_index)) {
	      prims ~= pp;
	    }
	  }
	}
	else {
	  prims ~= _parent.getPrimLens();
	  if(_indexExpr) {
	    prims ~= _indexExpr.getRndPrims() ~ _parent.getPrimLens();
	  }
	}
	return prims;
      }

      CstVecPrim[] getPrimLens() {
	// if(_index.idxVars.length is 0)
	if(_indexExpr is null) {
	  return [_parent[_index].arrLen()];
	  // return prims ~ _parent[_index].getPrimLens();
	}
	if(_indexExpr.isConst()) {
	  CstVecPrim[] prims;
	  prims ~= _parent[cast(size_t) _indexExpr.evaluate()].getPrimLens();
	  return prims;
	}
	else {
	  CstVecPrim[] prims;
	  foreach(p; getRndPrims()) {
	    // import std.stdio;
	    // writeln(_parent.name(), " ", p.name());
	    prims ~= p.getPrimLens();
	  }
	  return prims;
	}
      }

      RV unroll(RndIterBase l, uint n) {
	bool idx = false;
	foreach(var; idxVars()) {
	  if(l is var) {
	    idx = true;
	    break;
	  }
	}
	if(! idx) return this;
	else {
	  // return new RV(name ~ "unrolled_" ~ n.to!string,
	  // 	      _parent.unroll(), _index.unroll());
	  if(_indexExpr) {
	    return _parent.unroll(l,n)[_indexExpr.unroll(l,n)];
	  }
	  else {
	    return _parent.unroll(l,n)[_index];
	  }
	}
      }

      static private size_t getLen(A, N...)(ref A arr, N idx) if(isArray!A) {
	static if(N.length == 0) return arr.length;
	else {
	  return getLen(arr[idx[0]], idx[1..$]);
	}
      }

      static if (HAS_RAND_ATTRIB) {
	static private void setLen(A, N...)(ref A arr, size_t v, N idx)
	  if(isArray!A) {
	    static if(N.length == 0) {
	      static if(isDynamicArray!A) {
		arr.length = v;
		// import std.stdio;
		// writeln(arr, " idx: ", N.length);
	      }
	      else {
		assert(false, "Can not set length of a fixed length array");
	      }
	    }
	    else {
	      // import std.stdio;
	      // writeln(arr, " idx: ", N.length);
	      setLen(arr[idx[0]], v, idx[1..$]);
	    }
	  }

	void setLen(N...)(size_t v, N idx) {
	  _parent.setLen(v, _index, idx);
	}
      }

      size_t getLen(N...)(N idx) {
	return _parent.getLen(_index, idx);
      }

      static private auto getVal(A, N...)(ref A arr, N idx)
	if(isArray!A && N.length > 0 && isIntegral!(N[0])) {
	static if(N.length == 1) return arr[idx[0]];
	else {
	  return getVal(arr[idx[0]], idx[1..$]);
	}
      }

      auto getVal(N...)(N idx) if(isIntegral!(N[0])) {
	if(_indexExpr) {
	  assert(_indexExpr.isConst());
	  return _parent.getVal(cast(size_t) _indexExpr.evaluate(), idx);
	}
	else {
	  return _parent.getVal(this._index, idx);
	}
      }

      static private void setVal(A, N...)(ref A arr, ulong v, int word, N idx)
	if(isArray!A && N.length > 0 && isIntegral!(N[0])) {
	  static if(N.length == 1) {
	    alias AE = ElementType!A;
	    static if(isIntegral!AE) {
	      if(word == 0) {
		arr[idx[0]] = cast(AE) v;
	      }
	      else {
		// static if(size_t.sizeof == 4) {
		//   assert(word == 1);	// 32 bit machine with long integral
		//   AE val = v;
		//   val = val << (8 * size_t.sizeof);
		//   arr[idx[0]] += val;
		// }
		// else {
		assert(false, "word has to be 0 for integrals");
	      }
	    }
	    else {
	      arr[idx[0]]._setNthWord(v, word);
	    }
	  }
	  else {
	    setVal(arr[idx[0]], v, word, idx[1..$]);
	  }
	}

      void setVal(N...)(ulong v, int word, N idx) if(isIntegral!(N[0])) {
	static if (HAS_RAND_ATTRIB) {
	  if(_indexExpr) {
	    assert(_indexExpr.isConst());
	    _parent.setVal(v, word, cast(size_t) _indexExpr.evaluate(), idx);
	  }
	  else {
	    _parent.setVal(v, word, _index, idx);
	  }
	}
	else {
	  assert(false);
	}
      }

      EV opIndex(CstVecExpr idx) {
	if(idx.isConst()) {
	  assert(_elems.length > 0, "_elems for expr " ~ this.name() ~
		 " have not been built");
	  // if(idx.evaluate() >= _elems.length || idx.evaluate() < 0 || _elems is null) {
	  // 	import std.stdio;
	  // 	writeln(this.name(), ":", idx.evaluate());
	  // }
	  // import std.stdio;
	  // writeln(idx.evaluate());
	  return _elems[cast(size_t) idx.evaluate()];
	}
	else {
	  static if(isStaticArray!E) {
	    // static array
	    return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	  }
	  else static if(isDynamicArray!E) {
	    // dynamic array
	    return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	  }
	  else {
	    return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	  }
	}
      }

      EV opIndex(size_t idx) {
	build();
	assert(_elems[idx]._indexExpr is null);
	return _elems[idx];
      }

      void doRandomize(_esdl__SolverRoot solver) {
	static if (HAS_RAND_ATTRIB) {
	  if(_elems.length == 0) this.build();
	  assert(arrLen !is null);
	  for(size_t i=0; i != arrLen.evaluate(); ++i) {
	    this[i].doRandomize(solver);
	  }
	}
	else {
	  assert(false);
	}
      }

      auto elements() {
	this.build();
	auto idx = arrLen.makeIdxVar();
	return this[idx];
      }

      bool built() {
	return (_elems.length == maxArrLen() &&
		_elems[0] !is null);
      }
    
      void build() {
	if(built()) return;
	_elems.length = maxArrLen();
	// static if(isIntegral!E || isBitVector!E) {
	// if(! built()) {
	for (uint i=0; i!=maxArrLen; ++i) {
	  if(_elems[i] is null) {
	    import std.conv: to;
	    _elems[i] = new EV(_name ~ "[" ~ i.to!string() ~ "]", this, i);
	    if(_elems[i].isVecArr()) {
	      _elems[i].build();
	    }
	  }
	}
      }
	
      auto iterator() {
	this.build();
	auto idx = arrLen.makeIdxVar();
	return idx;
      }

      RndLen!RV length() {
	return _arrLen;
      }

      RndLen!RV arrLen() {
	return _arrLen;
      }

      void _esdl__reset() {
	_arrLen.stage = null;
	foreach(elem; _elems) {
	  if(elem !is null) {
	    elem._esdl__reset();
	  }
	}
      }

      CstStage stage() {
	return arrLen().stage();
      }

    }


abstract class RndObjBase(V, alias R, int N)
  if(_esdl__ArrOrder!(V, N) == 0): CstVecPrim
{
  enum HAS_RAND_ATTRIB = (! __traits(isSame, R, _esdl__norand));

  alias E = ElementTypeN!(V, N);
  alias RV = typeof(this);
  // enum int ORDER = _esdl__ArrOrder!(T, I, N);

  string _name;

  E            _val;

  ~this() {}

  override string name() {
    return _name;
  }

  void _esdl__reset() {}

  bool isVecArr() {
    return false;
  }

  BDD getPrimBdd(Buddy buddy) {
    return buddy.one();
  }

  void resetPrimeBdd() { }

  bool isRand() {
    static if (HAS_RAND_ATTRIB) {
      return true;
    }
    else {
      return false;
    }
  }

}

// T represents the type of the declared array/non-array member
// N represents the level of the array-elements we have to traverse
// for the elements this RndObj represents
template RndObj(T, int I, int N=0)
  if(_esdl__ArrOrder!(T, I, N) == 0) {
  alias RndObj = RndObj!(typeof(T.tupleof[I]),
			     getRandAttr!(T, I), N);
}

class RndObj(V, alias R, int N) if(N == 0 && _esdl__ArrOrder!(V, N) == 0):
  RndObjBase!(V, R, N)
    {
      import std.traits;
      import std.range;
      import esdl.data.bvec;

      V* _var;

    
      this(string name, ref V var) {
	// import std.stdio;
	// writeln("New ", name);
	_name = name;
	_var = &var;
      }

      void _esdl__setValRef(ref V var) {
	_var = &var;
      }
      
      override RndIterBase[] idxVars() {
	return [];
      }

      override CstVecPrim[] getRndPrims() {
	static if (HAS_RAND_ATTRIB) {
	  if(isRand) return [this];
	  else return [];
	}
	else {
	  return [];
	}
      }

      CstVecPrim[] getPrimLens() {
	assert(false);
      }

      override RV unroll(RndIterBase l, uint n) {
	// idxVars is always empty
	return this;
      }

      void doRandomize(_esdl__SolverRoot solver) { }

      override E getVal() {
	return *_var;
      }
    }

class RndObj(V, alias R, int N=0) if(N != 0 && _esdl__ArrOrder!(V, N) == 0):
  RndObjBase!(V, R, N)
    {
      import std.traits;
      import std.range;
      import esdl.data.bvec;

      alias P = RndObjArr!(V, R, N-1);
      P _parent;

      CstVecExpr _indexExpr = null;
      int _index = 0;

      this(string name, P parent, CstVecExpr indexExpr) {
	_name = name;
	_parent = parent;
	_indexExpr = indexExpr;
      }

      this(string name, P parent, uint index) {
	_name = name;
	_parent = parent;
	_index = index;
      }

      override CstVecPrim[] preReqs() {
	if(_indexExpr) {
	  return _parent.preReqs() ~ _indexExpr.preReqs();
	}
	else {
	  return _parent.preReqs();
	}
      }

      override RndIterBase[] idxVars() {
	if(_indexExpr) {
	  return _parent.idxVars() ~ _indexExpr.idxVars();
	}
	else {
	  return _parent.idxVars();
	}
      }

      CstVecPrim[] getPrimLens() {
	assert(false);
      }

      // What is required here
      // we could be dealing with an _index or an _indexExpr. Further
      // the indexExpr could be either a solvable constraint expression
      // or an array length iterator that iterates over the length of
      // the given array. To add to the complexity here, we could have a
      // case where the given element has a parent that too a
      // non-elementary one (involving non-integer indexes). In such
      // cases we need to list all the elements of the array that could
      // finally represent the given element that we are currently
      // dealing with.
      override CstVecPrim[] getRndPrims() {
	CstVecPrim[] prims;
	static if (HAS_RAND_ATTRIB) {
	  if(_indexExpr) {
	    // FIXME -- if the expression has been solved
	    // return _parent.getRndPrims(_indexExpr.evaluate()) ;
	    prims = _indexExpr.getRndPrims();
	    foreach(pp; _parent.getRndPrims(-1)) {
	      prims ~= pp;
	    }
	  }
	  else {
	    foreach(pp; _parent.getRndPrims(_index)) {
	      prims ~= pp;
	    }
	  }
	  return prims;
	}
	else {
	  if(_indexExpr) {
	    // FIXME -- if the expression has been solved
	    // return _parent.getRndPrims(_indexExpr.evaluate()) ;
	    prims = _indexExpr.getRndPrims() ~ _parent.getRndPrims();
	  }
	  else {
	    // foreach(pp; _parent.getRndPrims(_index)) {
	    //   prims ~= pp;
	    // }
	    prims = _parent.getRndPrims();
	  }
	  return prims;
	}
      }

      override RV unroll(RndIterBase l, uint n) {
	bool idx = false;
	foreach(var; idxVars()) {
	  if(l is var) {
	    idx = true;
	    break;
	  }
	}
	if(! idx) return this;
	else {
	  if(_indexExpr) {
	    auto vec = _parent.unroll(l,n)[_indexExpr.unroll(l,n)];
	    // import std.stdio;
	    // writeln(_indexExpr.name(), " has been unrolled as: ",
	    // 	  _indexExpr.unroll(l,n).name());
	    return vec;
	    // return _parent.unroll(l,n)[_indexExpr.unroll(l,n)];
	  }
	  else {
	    return _parent.unroll(l,n)[_index];
	  }
	}
      }

      void doRandomize(_esdl__SolverRoot solver) {
	static if (HAS_RAND_ATTRIB) {
	  if(stage is null) {
	    E val;
	    solver._esdl__getRandGen().gen(val);
	    value(val);
	  }
	}
	else {
	  assert(false, name());
	}
      }

      override E getVal() {
	if(_indexExpr) {
	  return _parent.getVal(cast(size_t) _indexExpr.evaluate());
	}
	else {
	  return _parent.getVal(this._index);
	}
      }

      ulong value() {
	if(_indexExpr) {
	  return _parent.getVal(_indexExpr.evaluate());
	}
	else {
	  return _parent.getVal(this._index);
	}
      }

      void value(ulong v, int word = 0) {
	static if (HAS_RAND_ATTRIB) {
	  // import std.stdio;
	  // writeln("Setting value of ", this.name(), " to: ", v);
	  // writeln("Parent length value of ", this.name(),
	  // 	      " is: ", _parent.getLen());
	  if(_indexExpr) {
	    _parent.setVal(v, word, cast(size_t) _indexExpr.evaluate());
	  }
	  else {
	    return _parent.setVal(v, word, _index);
	  }
	}
	else {
	  assert(false);
	}
      }
    }

// Arrays (Multidimensional arrays as well)
// template RndObjArrBase(T, int I, int N=0)
//   if(_esdl__ArrOrder!(T, I, N) != 0) {
//   alias RndObjArrBase = RndObjArrBase!(typeof(T.tupleof[I]),
// 					   getRandAttr!(T, I), N);
// }

abstract class RndObjArrBase(V, alias R, int N=0)
  if(_esdl__ArrOrder!(V, N) != 0): CstVecPrim
{
  enum HAS_RAND_ATTRIB = (! __traits(isSame, R, _esdl__norand));

  alias L = ElementTypeN!(V, N);
  alias E = ElementTypeN!(V, N+1);
  enum int ORDER = _esdl__ArrOrder!(V, N);

  static if(ORDER > 1) {
    alias EV = RndObjArr!(V, R, N+1);
  }
  else {
    alias EV = RndObj!(V, R, N+1);
  }

  EV[] _elems;

  string _name;
  override string name() {
    return _name;
  }

  size_t maxArrLen() {
    static if (HAS_RAND_ATTRIB) {
      static if(isStaticArray!L) {
	return L.length;
      }
      else {
	return getRandAttrN!(R, N);
      }
    }
    else {
      return L.length;
    }
  }

  static if (HAS_RAND_ATTRIB) {
    CstVecPrim[] _preReqs;

    void opIndexAssign(EV c, size_t idx) {
      _elems[idx] = c;
    }
  }

  bool isVecArr() {
    return true;
  }

  BDD getPrimBdd(Buddy buddy) {
    return buddy.one();
  }

  void resetPrimeBdd() { }

  // override CstVec2VecExpr opIndex(CstVecExpr idx) {
  //   return new CstVec2VecExpr(this, idx, CstBinVecOp.IDXINDEX);
  // }

  bool isRand() {
    assert(false, "isRand not implemented for RndObjArrBase");
  }

  ulong value() {
    assert(false, "value not implemented for RndObjArrBase");
  }

  void value(ulong v, int word = 0) {
    assert(false, "value not implemented for RndObjArrBase");
  }

  void stage(CstStage s) {
    assert(false, "stage not implemented for RndObjArrBase");
  }

  uint domIndex() {
    assert(false, "domIndex not implemented for RndObjArrBase");
  }

  void domIndex(uint s) {
    assert(false, "domIndex not implemented for RndObjArrBase");
  }

  uint bitcount() {
    assert(false, "bitcount not implemented for RndObjArrBase");
  }

  bool signed() {
    assert(false, "signed not implemented for RndObjArrBase");
  }

  BddVec bddvec() {
    assert(false, "bddvec not implemented for RndObjArrBase");
  }

  void bddvec(BddVec b) {
    assert(false, "bddvec not implemented for RndObjArrBase");
  }

  void solveBefore(CstVecPrim other) {
    static if (HAS_RAND_ATTRIB) {
      other.addPreRequisite(this);
    }
    else {
      assert(false);
    }
  }

  void addPreRequisite(CstVecPrim prim) {
    static if (HAS_RAND_ATTRIB) {
      _preReqs ~= prim;
    }
    else {
      assert(false);
    }
  }

}

template RndObjArr(T, int I, int N=0)
  if(_esdl__ArrOrder!(T, I, N) != 0) {
  alias RndObjArr = RndObjArr!(typeof(T.tupleof[I]),
				   getRandAttr!(T, I), N);
}

// Arrays (Multidimensional arrays as well)
class RndObjArr(V, alias R, int N=0)
  if(N == 0 && _esdl__ArrOrder!(V, N) != 0):
    RndObjArrBase!(V, R, N)
      {
	alias RV = typeof(this);
	RndLen!RV _arrLen;

	alias RAND=R;

	V* _var;

	void _esdl__setValRef(ref V var) {
	  _var = &var;
	}
      
	static if (HAS_RAND_ATTRIB) {
	  static if(isStaticArray!V) {
	    static assert(__traits(isSame, R, rand));
	    enum int maxLen = V.length;
	    this(string name, ref V var) {
	      _name = name;
	      _var = &var;
	      _arrLen = new RndLen!RV(name ~ ".len", this);
	    }
	  }

	  static if(isDynamicArray!V) {
	    enum int maxLen = getRandAttrN!(R, N);
	    this(string name, ref V var) {
	      _name = name;
	      _var = &var;
	      _arrLen = new RndLen!RV(name ~ ".len", this);
	    }
	  }
	}
	else {
	  this(string name, ref V var) {
	    _name = name;
	    _var = &var;
	    _arrLen = new RndLen!RV(name ~ ".len", this);
	  }
	}

	CstVecPrim[] preReqs() {
	  static if (HAS_RAND_ATTRIB) {
	    return _preReqs;		// N = 0 -- no _parent
	  }
	  else {
	    return [];
	  }
	}

	RndIterBase[] idxVars() {
	  return [];
	}

	static if (HAS_RAND_ATTRIB) {
	  EV[] getRndPrims(int idx) {
	    if(idx < 0) return _elems;
	    else return [_elems[idx]];
	  }
	}

	CstVecPrim[] getRndPrims() {
	  static if (HAS_RAND_ATTRIB) {
	    CstVecPrim[] prims;
	    foreach(elem; _elems) {
	      prims ~= elem;
	    }
	    return prims;
	  }
	  else {
	    return [];
	  }
	}

	CstVecPrim[] getPrimLens() {
	  static if (HAS_RAND_ATTRIB) {
	    CstVecPrim[] prims;
	    if(_arrLen.isRand) prims ~= _arrLen;
	    return prims;
	  }
	  else {
	    return [];
	  }
	}
    
	RV unroll(RndIterBase l, uint n) {
	  return this;
	}

	static private auto getVal(A, N...)(ref A arr, N idx)
	  if(isArray!A && N.length > 0 && isIntegral!(N[0])) {
	    static if(N.length == 1) return arr[idx[0]];
	    else {
	      return getVal(arr[idx[0]], idx[1..$]);
	    }
	  }

	auto getVal(J...)(J idx) if(isIntegral!(J[0])) {
	  return getVal(*_var, cast(size_t) idx);
	}

	static if (HAS_RAND_ATTRIB) {
	  static private void setVal(A, N...)(ref A arr, ulong v, int word, N idx)
	    if(isArray!A && N.length > 0 && isIntegral!(N[0])) {
	      static if(N.length == 1) {
		alias AE = ElementType!A;
		static if(isIntegral!AE) {
		  if(word == 0) {
		    arr[idx[0]] = cast(AE) v;
		  }
		  else {
		    // static if(size_t.sizeof == 4) {
		    //   assert(word == 1);	// 32 bit machine with long integral
		    //   AE val = v;
		    //   val = val << (8 * size_t.sizeof);
		    //   arr[idx[0]] += val;
		    // }
		    // else {
		    assert(false, "word has to be 0 for integrals");
		  }
		}
		else {
		  arr[idx[0]]._setNthWord(v, word);
		}
	      }
	      else {
		setVal(arr[idx[0]], v, word, idx[1..$]);
	      }
	    }

	  static private void setLen(A, N...)(ref A arr, size_t v, N idx)
	    if(isArray!A) {
	      static if(N.length == 0) {
		static if(isDynamicArray!A) {
		  arr.length = v;
		  // import std.stdio;
		  // writeln(arr, " idx: ", N.length);
		}
		else {
		  assert(false, "Can not set length of a fixed length array");
		}
	      }
	      else {
		// import std.stdio;
		// writeln(arr, " idx: ", N.length);
		setLen(arr[idx[0]], v, idx[1..$]);
	      }
	    }
	}
	else {
	  void setLen(N...)(size_t v, N idx) {
	    // setLen(*_var, v, idx);
	    assert(false, "Can not set value for VarVecArr");
	  }
	}

	void setVal(J...)(ulong v, int word, J idx) if(isIntegral!(J[0])) {
	  static if (HAS_RAND_ATTRIB) {
	    setVal(*_var, v, word, idx);
	  }
	  else {
	    assert(false);
	  }
	}

	static private size_t getLen(A, N...)(ref A arr, N idx) if(isArray!A) {
	  static if(N.length == 0) return arr.length;
	  else {
	    return getLen(arr[idx[0]], idx[1..$]);
	  }
	}

	size_t getLen(N...)(N idx) {
	  return getLen(*_var, idx);
	}

	void setLen(N...)(size_t v, N idx) {
	  static if (HAS_RAND_ATTRIB) {
	    setLen(*_var, v, idx);
	  }
	  else {
	    assert(false);
	  }
	}

	EV opIndex(CstVecExpr idx) {
	  if(idx.isConst()) {
	    assert(_elems.length > 0, "_elems for expr " ~ this.name() ~
		   " have not been built");
	    // if(idx.evaluate() >= _elems.length || idx.evaluate() < 0 || _elems is null) {
	    // 	import std.stdio;
	    // 	writeln(this.name(), ":", idx.evaluate());
	    // }
	    // import std.stdio;
	    // writeln(idx.evaluate());
	    return _elems[cast(size_t) idx.evaluate()];
	  }
	  else {
	    static if(isStaticArray!E) {
	      // static array
	      return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	    }
	    else static if(isDynamicArray!E) {
	      // dynamic array
	      return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	    }
	    else {
	      return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	    }
	  }
	}

	EV opIndex(size_t idx) {
	  build();
	  assert(_elems[idx]._indexExpr is null);
	  return _elems[idx];
	}

	void doRandomize(_esdl__SolverRoot solver) {
	  static if (HAS_RAND_ATTRIB) {
	    if(_elems.length == 0) this.build();
	    assert(arrLen !is null);
	    for(size_t i=0; i != arrLen.evaluate(); ++i) {
	      this[i].doRandomize(solver);
	    }
	  }
	  else {
	    assert(false);
	  }
	}

	auto elements() {
	  this.build();
	  auto idx = arrLen.makeIdxVar();
	  return this[idx];
	}

	bool built() {
	  return (_elems.length == maxArrLen() &&
		  _elems[0] !is null);
	}
    
	void build() {
	  if(built()) return;
	  _elems.length = maxArrLen();
	  // static if(isIntegral!E || isBitVector!E) {
	  // if(! built()) {
	  for (uint i=0; i!=maxArrLen; ++i) {
	    if(_elems[i] is null) {
	      import std.conv: to;
	      _elems[i] = new EV(_name ~ "[" ~ i.to!string() ~ "]", this, i);
	      if(_elems[i].isVecArr()) {
		_elems[i].build();
	      }
	    }
	  }
	}
	
	auto iterator() {
	  this.build();
	  auto idx = arrLen.makeIdxVar();
	  return idx;
	}

	RndLen!RV length() {
	  return _arrLen;
	}

	RndLen!RV arrLen() {
	  return _arrLen;
	}

	void _esdl__reset() {
	  _arrLen.stage = null;
	  foreach(elem; _elems) {
	    if(elem !is null) {
	      elem._esdl__reset();
	    }
	  }
	}

	CstStage stage() {
	  return arrLen().stage();
	}

      }

class RndObjArr(V, alias R, int N=0) if(N != 0 && _esdl__ArrOrder!(V, N) != 0):
  RndObjArrBase!(V, R, N)
    {
      import std.traits;
      import std.range;
      import esdl.data.bvec;
      alias P = RndObjArr!(V, R, N-1);
      P _parent;
      CstVecExpr _indexExpr = null;
      int _index = 0;

      alias RV = typeof(this);
      RndLen!RV _arrLen;

      alias RAND=R;
      
      this(string name, P parent, CstVecExpr indexExpr) {
	_name = name;
	_parent = parent;
	_indexExpr = indexExpr;
	_arrLen = new RndLen!RV(name ~ ".len", this);
      }

      this(string name, P parent, uint index) {
	_name = name;
	_parent = parent;
	_index = index;
	_arrLen = new RndLen!RV(name ~ ".len", this);
      }

      CstVecPrim[] preReqs() {
	CstVecPrim[] req;
	static if (HAS_RAND_ATTRIB) {
	  req = _preReqs ~ _parent.arrLen();
	}
	if(_indexExpr) {
	  return req ~ _indexExpr.preReqs() ~ _parent.preReqs();
	}
	else {
	  return req ~ _parent.preReqs();
	}
      }

      RndIterBase[] idxVars() {
	if(_indexExpr) {
	  return _parent.idxVars() ~ _indexExpr.idxVars();
	}
	else {
	  return _parent.idxVars();
	}
      }

      EV[] getRndPrims(int idx) {
	EV[] prims;
	static if (HAS_RAND_ATTRIB) {
	  if(_indexExpr) {
	    foreach(pp; _parent.getRndPrims(-1)) {
	      if(idx < 0) prims ~= pp._elems;
	      else prims ~= pp._elems[idx];
	    }
	  }
	  else {
	    foreach(pp; _parent.getRndPrims(_index)) {
	      if(idx < 0) prims ~= pp._elems;
	      else prims ~= pp._elems[idx];
	    }
	  }
	}
	return prims;
      }
    
      // This is slightly tricky in case we are pointing directly to
      // just one element of the array, this function should return just
      // that element. But it could be that the index or an upper
      // hierarchy index is not a constant, but an iterator or a
      // randomized epression. In that case, we shall have to return
      // more primary elements
      CstVecPrim[] getRndPrims() {
	CstVecPrim[] prims;
	static if (HAS_RAND_ATTRIB) {
	  if(_indexExpr) {
	    prims = _indexExpr.getRndPrims();
	    foreach(pp; _parent.getRndPrims(-1)) {
	      prims ~= pp;
	    }
	  }
	  else {
	    foreach(pp; _parent.getRndPrims(_index)) {
	      prims ~= pp;
	    }
	  }
	}
	else {
	  prims ~= _parent.getPrimLens();
	  if(_indexExpr) {
	    prims ~= _indexExpr.getRndPrims() ~ _parent.getPrimLens();
	  }
	}
	return prims;
      }

      CstVecPrim[] getPrimLens() {
	// if(_index.idxVars.length is 0)
	if(_indexExpr is null) {
	  return [_parent[_index].arrLen()];
	  // return prims ~ _parent[_index].getPrimLens();
	}
	if(_indexExpr.isConst()) {
	  CstVecPrim[] prims;
	  prims ~= _parent[cast(size_t) _indexExpr.evaluate()].getPrimLens();
	  return prims;
	}
	else {
	  CstVecPrim[] prims;
	  foreach(p; getRndPrims()) {
	    // import std.stdio;
	    // writeln(_parent.name(), " ", p.name());
	    prims ~= p.getPrimLens();
	  }
	  return prims;
	}
      }

      RV unroll(RndIterBase l, uint n) {
	bool idx = false;
	foreach(var; idxVars()) {
	  if(l is var) {
	    idx = true;
	    break;
	  }
	}
	if(! idx) return this;
	else {
	  // return new RV(name ~ "unrolled_" ~ n.to!string,
	  // 	      _parent.unroll(), _index.unroll());
	  if(_indexExpr) {
	    return _parent.unroll(l,n)[_indexExpr.unroll(l,n)];
	  }
	  else {
	    return _parent.unroll(l,n)[_index];
	  }
	}
      }

      static private size_t getLen(A, N...)(ref A arr, N idx) if(isArray!A) {
	static if(N.length == 0) return arr.length;
	else {
	  return getLen(arr[idx[0]], idx[1..$]);
	}
      }

      static if (HAS_RAND_ATTRIB) {
	static private void setLen(A, N...)(ref A arr, size_t v, N idx)
	  if(isArray!A) {
	    static if(N.length == 0) {
	      static if(isDynamicArray!A) {
		arr.length = v;
		// import std.stdio;
		// writeln(arr, " idx: ", N.length);
	      }
	      else {
		assert(false, "Can not set length of a fixed length array");
	      }
	    }
	    else {
	      // import std.stdio;
	      // writeln(arr, " idx: ", N.length);
	      setLen(arr[idx[0]], v, idx[1..$]);
	    }
	  }

	void setLen(N...)(size_t v, N idx) {
	  _parent.setLen(v, _index, idx);
	}
      }

      size_t getLen(N...)(N idx) {
	return _parent.getLen(_index, idx);
      }

      static private auto getVal(A, N...)(ref A arr, N idx)
	if(isArray!A && N.length > 0 && isIntegral!(N[0])) {
	static if(N.length == 1) return arr[idx[0]];
	else {
	  return getVal(arr[idx[0]], idx[1..$]);
	}
      }

      auto getVal(N...)(N idx) if(isIntegral!(N[0])) {
	if(_indexExpr) {
	  assert(_indexExpr.isConst());
	  return _parent.getVal(cast(size_t) _indexExpr.evaluate(), idx);
	}
	else {
	  return _parent.getVal(this._index, idx);
	}
      }

      static private void setVal(A, N...)(ref A arr, ulong v, int word, N idx)
	if(isArray!A && N.length > 0 && isIntegral!(N[0])) {
	  static if(N.length == 1) {
	    alias AE = ElementType!A;
	    static if(isIntegral!AE) {
	      if(word == 0) {
		arr[idx[0]] = cast(AE) v;
	      }
	      else {
		// static if(size_t.sizeof == 4) {
		//   assert(word == 1);	// 32 bit machine with long integral
		//   AE val = v;
		//   val = val << (8 * size_t.sizeof);
		//   arr[idx[0]] += val;
		// }
		// else {
		assert(false, "word has to be 0 for integrals");
	      }
	    }
	    else {
	      arr[idx[0]]._setNthWord(v, word);
	    }
	  }
	  else {
	    setVal(arr[idx[0]], v, word, idx[1..$]);
	  }
	}

      void setVal(N...)(ulong v, int word, N idx) if(isIntegral!(N[0])) {
	static if (HAS_RAND_ATTRIB) {
	  if(_indexExpr) {
	    assert(_indexExpr.isConst());
	    _parent.setVal(v, word, cast(size_t) _indexExpr.evaluate(), idx);
	  }
	  else {
	    _parent.setVal(v, word, _index, idx);
	  }
	}
	else {
	  assert(false);
	}
      }

      EV opIndex(CstVecExpr idx) {
	if(idx.isConst()) {
	  assert(_elems.length > 0, "_elems for expr " ~ this.name() ~
		 " have not been built");
	  // if(idx.evaluate() >= _elems.length || idx.evaluate() < 0 || _elems is null) {
	  // 	import std.stdio;
	  // 	writeln(this.name(), ":", idx.evaluate());
	  // }
	  // import std.stdio;
	  // writeln(idx.evaluate());
	  return _elems[cast(size_t) idx.evaluate()];
	}
	else {
	  static if(isStaticArray!E) {
	    // static array
	    return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	  }
	  else static if(isDynamicArray!E) {
	    // dynamic array
	    return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	  }
	  else {
	    return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	  }
	}
      }

      EV opIndex(size_t idx) {
	build();
	assert(_elems[idx]._indexExpr is null);
	return _elems[idx];
      }

      void doRandomize(_esdl__SolverRoot solver) {
	static if (HAS_RAND_ATTRIB) {
	  if(_elems.length == 0) this.build();
	  assert(arrLen !is null);
	  for(size_t i=0; i != arrLen.evaluate(); ++i) {
	    this[i].doRandomize(solver);
	  }
	}
	else {
	  assert(false);
	}
      }

      auto elements() {
	this.build();
	auto idx = arrLen.makeIdxVar();
	return this[idx];
      }

      bool built() {
	return (_elems.length == maxArrLen() &&
		_elems[0] !is null);
      }
    
      void build() {
	if(built()) return;
	_elems.length = maxArrLen();
	// static if(isIntegral!E || isBitVector!E) {
	// if(! built()) {
	for (uint i=0; i!=maxArrLen; ++i) {
	  if(_elems[i] is null) {
	    import std.conv: to;
	    _elems[i] = new EV(_name ~ "[" ~ i.to!string() ~ "]", this, i);
	    if(_elems[i].isVecArr()) {
	      _elems[i].build();
	    }
	  }
	}
      }
	
      auto iterator() {
	this.build();
	auto idx = arrLen.makeIdxVar();
	return idx;
      }

      RndLen!RV length() {
	return _arrLen;
      }

      RndLen!RV arrLen() {
	return _arrLen;
      }

      void _esdl__reset() {
	_arrLen.stage = null;
	foreach(elem; _elems) {
	  if(elem !is null) {
	    elem._esdl__reset();
	  }
	}
      }

      CstStage stage() {
	return arrLen().stage();
      }

    }

class RndIter(RV): RndIterBase, CstVecPrim
{
  RV _arrVar;

  RV arrVar() {
    return _arrVar;
  }

  this(RV arrVar) {
    super("idxVar");
    _arrVar = arrVar;
    _arrVar._arrLen.idxVar(this);
  }

  override RndIterBase[] idxVars() {
    return _arrVar.idxVars() ~ this;
  }

  override uint maxVal() {
    if(! this.isUnrollable()) {
      assert(false, "Can not find maxVal since the " ~
	     "Idx Variable is unrollable");
    }
    // import std.stdio;
    // writeln("maxVal for arrVar: ", _arrVar.name(), " is ",
    // 	    _arrVar.arrLen.value);
    return cast(uint) _arrVar.arrLen.value;
  }

  override bool isUnrollable() {
    if(! _arrVar.arrLen.isRand()) return true;
    if(_arrVar.arrLen.stage !is null &&
       _arrVar.arrLen.stage.solved()) return true;
    else return false;
  }

  // get all the primary bdd vectors that constitute a given bdd expression
  override CstVecPrim[] getRndPrims() {
    return []; // _arrVar.arrLen.getRndPrims();
  }

  CstVecPrim[] getPrimLens() {
    return [_arrVar.arrLen];
  }

  bool isRand() {
    return _arrVar.arrLen.isRand();
  }
  ulong value() {
    return _arrVar.arrLen.value();
  }
  void value(ulong v, int word = 0) {
    // import std.stdio;
    // writeln("Setting value for arrlen for ", _arrVar.name, " to ", v);
    assert(word == 0);
    _arrVar.arrLen.value(v);
  }
  void doRandomize(_esdl__SolverRoot solver) {
    assert(false);
  }
  CstStage stage() {
    return _arrVar.arrLen.stage();
  }
  void stage(CstStage s) {
    _arrVar.arrLen.stage(s);
  }
  uint domIndex() {
    return _arrVar.arrLen.domIndex;
  }
  void domIndex(uint s) {
    _arrVar.arrLen.domIndex(s);
  }
  uint bitcount() {
    return _arrVar.arrLen.bitcount();
  }
  bool signed() {
    // return _arrVar.arrLen.signed();
    return false;
  }
  BddVec bddvec() {
    return _arrVar.arrLen.bddvec();
  }
  void bddvec(BddVec b) {
    _arrVar.bddvec(b);
  }
  override string name() {
    string n = _arrVar.arrLen.name();
    return n[0..$-3] ~ "iter";
  }
  override CstVecExpr unroll(RndIterBase l, uint n) {
    // import std.stdio;
    // writeln("unrolling: ", arrVar.name());
    if(this !is l) {
      return _arrVar.unroll(l,n).arrLen().makeIdxVar();
    }
    else {
      return new ConstVec!size_t(n);
    }
  }

  void _esdl__reset() {
    stage = null;
  }

  bool isVecArr() {
    return false;
  }

  BDD getPrimBdd(Buddy buddy) {
    return buddy.one();
  }

  void resetPrimeBdd() { }

  void solveBefore(CstVecPrim other) {
    assert(false);
  }

  void addPreRequisite(CstVecPrim other) {
    assert(false);
  }

}

class RndLen(RV): CstVecExpr, CstVecPrim
{

  enum HAS_RAND_ATTRIB = (! __traits(isSame, RV.RAND, _esdl__norand));

  // This bdd has the constraint on the max length of the array
  BDD _primBdd;
  
  RndIter!RV _idxVar;

  RV _parent;

  BddVec _domvec;
  BddVec _valvec;
  size_t _val;
  
  uint _domIndex = uint.max;
  CstStage _stage = null;

  string _name;

  CstVecPrim[] _preReqs;

  override string name() {
    return _name;
  }

  this(string name, RV parent) {
    _name = name;
    _parent = parent;
  }

  ~this() {
    _domvec.reset();
    _valvec.reset();
    _primBdd.reset();
  }

  override CstVecPrim[] preReqs() {
    return _preReqs ~ _parent.preReqs();
  }

  override RndIterBase[] idxVars() {
    return _parent.idxVars();
  }

  override CstVecPrim[] getRndPrims() {
    return _parent.getPrimLens();
  }

  CstVecPrim[] getPrimLens() {
    assert(false);
  }
  
  private bool refreshNoRand(Buddy buddy) {
    auto val = _parent.getLen();
    if ((! _valvec.isNull()) && _val == val) {
      return false;
    }
    else {
      _val = val;
      _valvec = buddy.buildVec(val);
      return true;
    }
  }

  // override CstStage[] getStages() {
  //   CstStage[] stages;
  //   if(isRand) stages = [this.stage()];
  //   return stages;
  // }
  override bool refresh(CstStage s, Buddy buddy) {
    static if (HAS_RAND_ATTRIB) {
      assert(stage() !is null, "stage null for: " ~ name());
      if(this.isRand && stage() is s) {
	return false;
      }
      else if((! this.isRand) ||
	      this.isRand && stage().solved()) { // work with the value
	return refreshNoRand(buddy);
      }
      else {
	assert(false, "Constraint evaluation in wrong stage");
      }
    }
    else {
      return refreshNoRand(buddy);
    }
  }

  override BddVec getBDD(CstStage s, Buddy buddy) {
    static if (HAS_RAND_ATTRIB) {
      assert(stage() !is null, "stage null for: " ~ name());
      if(this.isRand && stage() is s) {
	return _domvec;
      }
      else if((! this.isRand) ||
	      this.isRand && stage().solved()) { // work with the value
	return _valvec;
      }
      else {
	assert(false, "Constraint evaluation in wrong stage");
      }
    }
    else {
      return _valvec;
    }
  }

  override long evaluate() {
    static if (HAS_RAND_ATTRIB) {
      if(! this.isRand || stage().solved()) {
	return value();
      }
      else {
	import std.conv;
	assert(false, "Rand variable " ~ _name ~ " evaluation in wrong stage: " ~
	       stage()._id.to!string);
      }
    }
    else {
      return value();
    }
  }

  void doRandomize(_esdl__SolverRoot solver) {
    assert(false);
  }
  
  bool isRand() {
    static if (HAS_RAND_ATTRIB) {
      import std.traits;
      if (isStaticArray!(RV.L)) return false;
      else return true;
    }
    else {
      return false;
    }
  }

  CstStage stage() {
    static if (HAS_RAND_ATTRIB) {
      return _stage;
    }
    else {
      assert(false);
    }
  }

  void stage(CstStage s) {
    static if (HAS_RAND_ATTRIB) {
      _stage = s;
    }
    else {
      assert(false);
    }
  }

  uint domIndex() {
    static if (HAS_RAND_ATTRIB) {
      return _domIndex;
    }
    else {
      assert(false);
    }
  }

  void domIndex(uint s) {
    static if (HAS_RAND_ATTRIB) {
      _domIndex = s;
    }
    else {
      assert(false);
    }
  }

  BddVec bddvec() {
    static if (HAS_RAND_ATTRIB) {
      return _domvec;
    }
    else {
      return _valvec;
    }
  }

  void bddvec(BddVec b) {
    _domvec = b;
  }

  T to(T)()
    if(is(T == string)) {
      import std.conv;
      if(isRand) {
	return "RAND-" ~ "#" ~ _name ~ ":" ~ value().to!string();
      }
      else {
	return "VAL#" ~ _name ~ ":" ~ value().to!string();
      }
    }

  override string toString() {
    return this.to!string();
  }

  BDD getPrimBdd(Buddy buddy) {
    if(_primBdd.isZero()) {
      _primBdd = this.bddvec.lte(buddy.buildVec(_parent.maxArrLen));
    }
    return _primBdd;
  }
  
  void resetPrimeBdd() {
    _primBdd.reset();
  }

  void idxVar(RndIter!RV var) {
    _idxVar = var;
  }

  RndIter!RV idxVar() {
    return _idxVar;
  }

  RndIter!RV makeIdxVar() {
    if(_idxVar is null) {
      _idxVar = new RndIter!RV(_parent);
    }
    return _idxVar;
  }

  uint bitcount() {
    uint i = 1;
    for (size_t c=1; c < _parent.maxArrLen; c *= 2) {
      i++;
    }
    return i;
  }

  bool signed() {
    return false;
  }

  ulong value() {
    return _parent.getLen();
  }

  void value(ulong v, int word = 0) {
    assert(word == 0);
    // import std.stdio;
    // writeln("Setting value for arrlen for ", _parent.name, " to ", v);
    // import std.stdio;
    // writeln("Setting length for array: ", _parent.name(), " to ", v);
    _parent.setLen(cast(size_t) v);
    // writeln("Getting length for array: ", _parent.name(), " as ", _parent.getLen());
    
  }

  override RndLen!RV unroll(RndIterBase l, uint n) {
    return _parent.unroll(l,n).arrLen();
  }

  void _esdl__reset() {
    stage = null;
  }

  bool isVecArr() {
    return false;
  }

  void solveBefore(CstVecPrim other) {
    other.addPreRequisite(this);
  }

  void addPreRequisite(CstVecPrim prim) {
    _preReqs ~= prim;
  }
}



class ConstVec(T = int): ValVec!T
{
  import std.conv;

  immutable T _val;			// the value of the constant
  BddVec _valvec;

  override string name() {
    return _val.to!string();
  }

  this(T value) {
    _val = value;
  }

  override bool refresh(CstStage stage, Buddy buddy) {
    if (_valvec.isNull()) {
      _valvec = buddy.buildVec(_val);
      return true;
    }
    else {
      return false;
    }
  }
  
  override BddVec getBDD(CstStage stage, Buddy buddy) {
    return _valvec;
  }

  const T getVal() {
    return _val;
  }

  override long evaluate() {
    return _val;
  }

}

abstract class ValVec(T = int): CstVecExpr, CstVecPrim
{
  override CstVecPrim[] preReqs() {
    return [];
  }

  override RndIterBase[] idxVars() {
    return [];
  }

  override bool isConst() {
    return true;
  }

  override CstVecPrim[] getRndPrims() {
    return [];
  }

  CstVecPrim[] getPrimLens() {
    assert(false);
  }

  bool isRand() {
    return false;
  }

  ulong value() {
    assert(false);
  }

  void value(ulong v, int word = 0) {
    assert(false);
  }

  void doRandomize(_esdl__SolverRoot solver) {
    assert(false);
  }
  
  CstStage stage() {
    assert(false, "no stage for ConstVec");
  }

  void stage(CstStage s) {
    assert(false, "no stage for ConstVec");
  }

  uint domIndex() {
    assert(false, "no domIndex for ConstVec");
  }

  void domIndex(uint s) {
    assert(false, "no domIndex for ConstVec");
  }

  uint bitcount() {
    assert(false, "no bitcount for ConstVec");
  }

  bool signed() {
    return isVarSigned!T;
  }

  BddVec bddvec() {
    assert(false, "no bddvec for ConstVec");
  }

  void bddvec(BddVec b) {
    assert(false, "no bddvec for ConstVec");
  }

  void _esdl__reset() {
    stage = null;
  }

  bool isVecArr() {
    return false;
  }

  BDD getPrimBdd(Buddy buddy) {
    return buddy.one();
  }

  void resetPrimeBdd() { }

  override CstVecExpr unroll(RndIterBase l, uint n) {
    return this;
  }
  
  void solveBefore(CstVecPrim other) {
    assert(false);
  }

  void addPreRequisite(CstVecPrim other) {
    assert(false);
  }

}

// This class would hold two(bin) vector nodes and produces a vector
// only after processing those two nodes
class CstVec2VecExpr: CstVecExpr
{
  import std.conv;

  CstVecExpr _lhs;
  CstVecExpr _rhs;
  CstBinVecOp _op;

  // CstVecPrim[] _preReqs;
  override CstVecPrim[] preReqs() {
    CstVecPrim[] reqs;
    foreach(req; _lhs.preReqs() ~ _rhs.preReqs()) {
      if(! req.solved()) {
	reqs ~= req;
      }
    }
    return reqs;
  }
  RndIterBase[] _idxVars;
  override RndIterBase[] idxVars() {
    return _idxVars;
  }

  override string name() {
    return "( " ~ _lhs.name ~ " " ~ _op.to!string() ~ " " ~ _rhs.name ~ " )";
  }

  override CstVecPrim[] getRndPrims() {
    return _lhs.getRndPrims() ~ _rhs.getRndPrims();
  }

  override CstVecPrim[] getSolvables() {
    CstVecPrim[] solvables;
    foreach(solvable; _lhs.getSolvables() ~ _rhs.getSolvables()) {
      if(! solvable.solved()) {
	bool add = true;
	foreach(req; this.preReqs()) {
	  if(req is solvable) {
	    add = false;
	  }
	}
	if(add) {
	  solvables ~= solvable;
	}
      }
    }
    return solvables;
  }

  override bool refresh(CstStage stage, Buddy buddy) {
    auto l = _lhs.refresh(stage, buddy);
    auto r = _rhs.refresh(stage, buddy);
    return r || l;
  }
  
  override BddVec getBDD(CstStage stage, Buddy buddy) {
    if(this.idxVars.length !is 0) {
      assert(false,
	     "CstVec2VecExpr: Need to unroll the idxVars" ~
	     " before attempting to solve BDD");
    }

    // auto lvec = _lhs.getBDD(stage, buddy);
    // auto rvec = _rhs.getBDD(stage, buddy);

    final switch(_op) {
    case CstBinVecOp.AND: return _lhs.getBDD(stage, buddy) &
	_rhs.getBDD(stage, buddy);
    case CstBinVecOp.OR:  return _lhs.getBDD(stage, buddy) |
	_rhs.getBDD(stage, buddy);
    case CstBinVecOp.XOR: return _lhs.getBDD(stage, buddy) ^
	_rhs.getBDD(stage, buddy);
    case CstBinVecOp.ADD: return _lhs.getBDD(stage, buddy) +
	_rhs.getBDD(stage, buddy);
    case CstBinVecOp.SUB: return _lhs.getBDD(stage, buddy) -
	_rhs.getBDD(stage, buddy);
    case CstBinVecOp.MUL:
      if(_rhs.isConst()) return _lhs.getBDD(stage, buddy) *
			   _rhs.evaluate();
      if(_lhs.isConst()) return _lhs.evaluate() *
			   _rhs.getBDD(stage, buddy);
      return _lhs.getBDD(stage, buddy) * _rhs.getBDD(stage, buddy);
    case CstBinVecOp.DIV:
      if(_rhs.isConst()) return _lhs.getBDD(stage, buddy) /
			   _rhs.evaluate();
      return _lhs.getBDD(stage, buddy) / _rhs.getBDD(stage, buddy);
    case CstBinVecOp.REM:
      if(_rhs.isConst()) return _lhs.getBDD(stage, buddy) %
			   _rhs.evaluate();
      return _lhs.getBDD(stage, buddy) % _rhs.getBDD(stage, buddy);
    case CstBinVecOp.LSH:
      if(_rhs.isConst()) return _lhs.getBDD(stage, buddy) <<
			   _rhs.evaluate();
      return _lhs.getBDD(stage, buddy) << _rhs.getBDD(stage, buddy);
    case CstBinVecOp.RSH:
      if(_rhs.isConst()) return _lhs.getBDD(stage, buddy) >>
			   _rhs.evaluate();
      return _lhs.getBDD(stage, buddy) >> _rhs.getBDD(stage, buddy);
    case CstBinVecOp.BITINDEX:
      assert(false, "BITINDEX is not implemented yet!");
    }
  }

  override long evaluate() {
    auto lvec = _lhs.evaluate();
    auto rvec = _rhs.evaluate();

    final switch(_op) {
    case CstBinVecOp.AND: return lvec &  rvec;
    case CstBinVecOp.OR:  return lvec |  rvec;
    case CstBinVecOp.XOR: return lvec ^  rvec;
    case CstBinVecOp.ADD: return lvec +  rvec;
    case CstBinVecOp.SUB: return lvec -  rvec;
    case CstBinVecOp.MUL: return lvec *  rvec;
    case CstBinVecOp.DIV: return lvec /  rvec;
    case CstBinVecOp.REM: return lvec %  rvec;
    case CstBinVecOp.LSH: return lvec << rvec;
    case CstBinVecOp.RSH: return lvec >> rvec;
    case CstBinVecOp.BITINDEX:
      assert(false, "BITINDEX is not implemented yet!");
    }
  }

  override CstVec2VecExpr unroll(RndIterBase l, uint n) {
    bool idx = false;
    foreach(var; idxVars()) {
      if(l is var) {
	idx = true;
	break;
      }
    }
    if(! idx) return this;
    else {
      return new CstVec2VecExpr(_lhs.unroll(l, n), _rhs.unroll(l, n), _op);
    }
  }

  this(CstVecExpr lhs, CstVecExpr rhs, CstBinVecOp op) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
    _idxVars = lhs.idxVars ~ rhs.idxVars;
    // foreach(var; lhs.idxVars ~ rhs.idxVars) {
    //   bool add = true;
    //   foreach(l; _idxVars) {
    // 	if(l is var) add = false;
    // 	break;
    //   }
    //   if(add) _idxVars ~= var;
    // }
  }
}

class CstVecSliceExpr: CstVecExpr
{
  CstVecExpr _vec;
  CstVecExpr _lhs;
  CstVecExpr _rhs;

  // CstVecPrim[] _preReqs;
  override CstVecPrim[] preReqs() {
    CstVecPrim[] reqs;
    if(_rhs is null) {
      foreach(req; _vec.preReqs() ~ _lhs.preReqs()) {
	if(! req.solved()) {
	  reqs ~= req;
	}
      }
    }
    else {
      foreach(req; _vec.preReqs() ~ _lhs.preReqs() ~ _rhs.preReqs()) {
	if(! req.solved()) {
	  reqs ~= req;
	}
      }
    }
    return reqs;
  }
  
  RndIterBase[] _idxVars;
  override RndIterBase[] idxVars() {
    return _idxVars;
  }

  override string name() {
    return _vec.name() ~ "[ " ~ _lhs.name() ~ " .. " ~ _rhs.name() ~ " ]";
  }

  override CstVecPrim[] getRndPrims() {
    if(_rhs is null) {
      return _vec.getRndPrims() ~ _lhs.getRndPrims();
    }
    else {
      return _vec.getRndPrims() ~ _lhs.getRndPrims() ~ _rhs.getRndPrims();
    }
  }

   override CstVecPrim[] getSolvables() {
    return _vec.getSolvables();
  }

  override bool refresh(CstStage stage, Buddy buddy) {
    auto l = _lhs.refresh(stage, buddy);
    auto r = _rhs.refresh(stage, buddy);
    return r || l;
  }
  
  override BddVec getBDD(CstStage stage, Buddy buddy) {
    if(this.idxVars.length !is 0) {
      assert(false,
	     "CstVecSliceExpr: Need to unroll the idxVars" ~
	     " before attempting to solve BDD");
    }

    auto vec  = _vec.getBDD(stage, buddy);
    size_t lvec = cast(size_t) _lhs.evaluate();
    size_t rvec = lvec;
    if(_rhs is null) {
      rvec = lvec + 1;
    }
    else {
      rvec = cast(size_t) _rhs.evaluate();
    }
    return vec[lvec..rvec];
  }

  override long evaluate() {
    // auto vec  = _vec.evaluate();
    // auto lvec = _lhs.evaluate();
    // auto rvec = _rhs.evaluate();

    assert(false, "Can not evaluate a CstVecSliceExpr!");
  }

  override CstVecSliceExpr unroll(RndIterBase l, uint n) {
    bool idx = false;
    foreach(var; idxVars()) {
      if(l is var) {
	idx = true;
	break;
      }
    }
    if(! idx) return this;
    else {
      if(_rhs is null) {
	return new CstVecSliceExpr(_vec.unroll(l, n), _lhs.unroll(l, n));
      }
      else {
	return new CstVecSliceExpr(_vec.unroll(l, n),
				   _lhs.unroll(l, n), _rhs.unroll(l, n));
      }
    }
  }

  this(CstVecExpr vec, CstVecExpr lhs, CstVecExpr rhs=null) {
    _vec = vec;
    _lhs = lhs;
    _rhs = rhs;
    auto idxVars = vec.idxVars ~ lhs.idxVars;
    if(rhs !is null) {
      idxVars ~= rhs.idxVars;
    }
    foreach(var; idxVars) {
      bool add = true;
      foreach(l; _idxVars) {
	if(l is var) add = false;
	break;
      }
      if(add) _idxVars ~= var;
    }
  }
}

class CstNotVecExpr: CstVecExpr
{
  override string name() {
    return "CstNotVecExpr";
  }
}

enum CstBddOp: byte
  {   LOGICAND,
      LOGICOR ,
      LOGICIMP,
      }

abstract class CstBddExpr
{
  string name();

  // In case this expr is unRolled, the _idxVars here would be empty
  RndIterBase[] _idxVars;

  abstract bool refresh(CstStage stage, Buddy buddy);
  
  RndIterBase[] idxVars() {
    return _idxVars;
  }

  // CstVecPrim[] _preReqs;

  abstract CstVecPrim[] preReqs();

  // unroll recursively untill no unrolling is possible
  CstBddExpr[] unroll() {
    CstBddExpr[] retval;
    auto idx = this.unrollableIdx();
    if(idx is null) {
      return [this];
    }
    else {
      foreach(expr; this.unroll(idx)) {
	// import std.stdio;
	// writeln(this.name(), " unrolled expr: ", expr.name());
	// writeln(expr.name());
	if(expr.unrollableIdx() is null) retval ~= expr;
	else retval ~= expr.unroll();
      }
    }
    return retval;
  }

  CstBddExpr[] unroll(RndIterBase l) {
    CstBddExpr[] retval;
    if(! l.isUnrollable()) {
      assert(false, "RndIterBase is not unrollabe yet");
    }
    auto max = l.maxVal();
    // import std.stdio;
    // writeln("maxVal is ", max);
    for (uint i = 0; i != max; ++i) {
      retval ~= this.unroll(l, i);
    }
    return retval;
  }

  RndIterBase unrollableIdx() {
    foreach(idx; _idxVars) {
      if(idx.isUnrollable()) return idx;
    }
    return null;
  }

  abstract CstBddExpr unroll(RndIterBase l, uint n);

  abstract CstVecPrim[] getRndPrims();

  final CstVecPrim[] getSolvables() {
    CstVecPrim[] solvables;
    foreach(prim; getRndPrims()) {
      if(! prim.solved()) {
	bool add = true;
	foreach(req; this.preReqs()) {
	  if(req is prim) {
	    add = false;
	  }
	}
	if(add) {
	  solvables ~= prim;
	}
      }
    }
    return solvables;
  }

  // abstract CstStage[] getStages();

  abstract BDD getBDD(CstStage stage, Buddy buddy);

  CstBdd2BddExpr opBinary(string op)(CstBddExpr other)
  {
    static if(op == "&") {
      return new CstBdd2BddExpr(this, other, CstBddOp.LOGICAND);
    }
    static if(op == "|") {
      return new CstBdd2BddExpr(this, other, CstBddOp.LOGICOR);
    }
    static if(op == ">>") {
      return new CstBdd2BddExpr(this, other, CstBddOp.LOGICIMP);
    }
  }

  CstNotBddExpr opUnary(string op)()
  {
    static if(op == "*") {	// "!" in cstx is translated as "*"
      return new CstNotBddExpr(this);
    }
  }

  CstBdd2BddExpr implies(CstBddExpr other)
  {
    return new CstBdd2BddExpr(this, other, CstBddOp.LOGICIMP);
  }

  CstBdd2BddExpr implies(CstVecExpr other)
  {
    return new CstBdd2BddExpr(this, other.toBdd(), CstBddOp.LOGICIMP);
  }

  CstBdd2BddExpr logicOr(CstBddExpr other)
  {
    return new CstBdd2BddExpr(this, other, CstBddOp.LOGICOR);
  }

  CstBdd2BddExpr logicOr(CstVecExpr other)
  {
    return new CstBdd2BddExpr(this, other.toBdd(), CstBddOp.LOGICOR);
  }

  CstBdd2BddExpr logicAnd(CstBddExpr other)
  {
    return new CstBdd2BddExpr(this, other, CstBddOp.LOGICAND);
  }

  CstBdd2BddExpr logicAnd(CstVecExpr other)
  {
    return new CstBdd2BddExpr(this, other.toBdd(), CstBddOp.LOGICAND);
  }

}

class CstBdd2BddExpr: CstBddExpr
{
  import std.conv;

  CstBddExpr _lhs;
  CstBddExpr _rhs;
  CstBddOp _op;

  override bool refresh(CstStage stage, Buddy buddy) {
    auto l = _lhs.refresh(stage, buddy);
    auto r = _rhs.refresh(stage, buddy);
    return r || l;
  }
  
  
  override string name() {
    return "( " ~ _lhs.name ~ " " ~ _op.to!string ~ " " ~ _rhs.name ~ " )";
  }

  override CstVecPrim[] preReqs() {
    CstVecPrim[] reqs;
    foreach(req; _lhs.preReqs() ~ _rhs.preReqs()) {
      if(! req.solved()) {
	reqs ~= req;
      }
    }
    return reqs;
  }

  override CstVecPrim[] getRndPrims() {
    return _lhs.getRndPrims() ~ _rhs.getRndPrims();
  }


  override BDD getBDD(CstStage stage, Buddy buddy) {
    if(this.idxVars.length !is 0) {
      assert(false,
	     "CstBdd2BddExpr: Need to unroll the idxVars" ~
	     " before attempting to solve BDD");
    }
    auto lvec = _lhs.getBDD(stage, buddy);
    auto rvec = _rhs.getBDD(stage, buddy);

    BDD retval;
    final switch(_op) {
    case CstBddOp.LOGICAND: retval = lvec &  rvec; break;
    case CstBddOp.LOGICOR:  retval = lvec |  rvec; break;
    case CstBddOp.LOGICIMP: retval = lvec >> rvec; break;
    }
    return retval;
  }

  override CstBdd2BddExpr unroll(RndIterBase l, uint n) {
    bool idx = false;
    foreach(var; idxVars()) {
      if(l is var) {
	idx = true;
	break;
      }
    }
    if(! idx) return this;
    else {
      return new CstBdd2BddExpr(_lhs.unroll(l, n), _rhs.unroll(l, n), _op);
    }
  }

  this(CstBddExpr lhs, CstBddExpr rhs, CstBddOp op) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
    foreach(var; lhs.idxVars ~ rhs.idxVars) {
      bool add = true;
      foreach(l; _idxVars) {
	if(l is var) add = false;
	break;
      }
      if(add) _idxVars ~= var;
    }
  }
}

class CstIteBddExpr: CstBddExpr
{
  override string name() {
    return "CstIteBddExpr";
  }

  override bool refresh(CstStage stage, Buddy buddy) {
    assert(false);
  }
}

class CstVec2BddExpr: CstBddExpr
{
  import std.conv;

  CstVecExpr _lhs;
  CstVecExpr _rhs;
  CstBinBddOp _op;

  override string name() {
    return "( " ~ _lhs.name ~ " " ~ _op.to!string ~ " " ~ _rhs.name ~ " )";
  }

  override bool refresh(CstStage stage, Buddy buddy) {
    auto l = _lhs.refresh(stage, buddy);
    auto r = _rhs.refresh(stage, buddy);
    return r || l;
  }
  
  override CstVecPrim[] preReqs() {
    CstVecPrim[] reqs;
    foreach(req; _lhs.preReqs() ~ _rhs.preReqs()) {
      if(! req.solved()) {
	reqs ~= req;
      }
    }
    return reqs;
  }
    
  override CstVecPrim[] getRndPrims() {
    return _lhs.getRndPrims() ~ _rhs.getRndPrims();
  }

  override BDD getBDD(CstStage stage, Buddy buddy) {
    if(this.idxVars.length !is 0) {
      assert(false,
	     "CstVec2BddExpr: Need to unroll the idxVars" ~
	     " before attempting to solve BDD");
    }
    auto lvec = _lhs.getBDD(stage, buddy);
    auto rvec = _rhs.getBDD(stage, buddy);

    BDD retval;
    final switch(_op) {
    case CstBinBddOp.LTH: retval = lvec.lth(rvec); break;
    case CstBinBddOp.LTE: retval = lvec.lte(rvec); break;
    case CstBinBddOp.GTH: retval = lvec.gth(rvec); break;
    case CstBinBddOp.GTE: retval = lvec.gte(rvec); break;
    case CstBinBddOp.EQU: retval = lvec.equ(rvec); break;
    case CstBinBddOp.NEQ: retval = lvec.neq(rvec); break;
    }
    return retval;
  }

  override CstVec2BddExpr unroll(RndIterBase l, uint n) {
    // import std.stdio;
    // writeln(_lhs.name() ~ " " ~ _op.to!string ~ " " ~ _rhs.name() ~ " Getting unrolled!");
    bool idx = false;
    foreach(var; idxVars()) {
      if(l is var) {
	idx = true;
	break;
      }
    }
    if(! idx) return this;
    else {
      // writeln("RHS: ", _rhs.unroll(l, n).name());
      // writeln("LHS: ", _lhs.unroll(l, n).name());
      return new CstVec2BddExpr(_lhs.unroll(l, n), _rhs.unroll(l, n), _op);
    }
  }

  this(CstVecExpr lhs, CstVecExpr rhs, CstBinBddOp op) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
    foreach(var; lhs.idxVars ~ rhs.idxVars) {
      bool add = true;
      foreach(l; _idxVars) {
	if(l is var) add = false;
	break;
      }
      if(add) _idxVars ~= var;
    }
  }
}

class CstBddConst: CstBddExpr
{
  immutable bool _expr;

  this(bool expr) {
    _expr = expr;
  }

  override bool refresh(CstStage stage, Buddy buddy) {
    return false;
  }
  
  override BDD getBDD(CstStage stage, Buddy buddy) {
    if(_expr) return buddy.one();
    else return buddy.zero();
  }

  override string name() {
    if(_expr) return "TRUE";
    else return "FALSE";
  }

  override CstVecPrim[] getRndPrims() {
    return [];
  }

  override CstVecPrim[] preReqs() {
    return [];
  }

  override CstBddConst unroll(RndIterBase l, uint n) {
    return this;
  }

}

class CstNotBddExpr: CstBddExpr
{
  CstBddExpr _expr;

  override string name() {
    return "( " ~ "!" ~ " " ~ _expr.name ~ " )";
  }

  override bool refresh(CstStage stage, Buddy buddy) {
    return _expr.refresh(stage, buddy);
  }
  
  override CstVecPrim[] preReqs() {
    return _expr.preReqs();
  }

  override CstVecPrim[] getRndPrims() {
    return _expr.getRndPrims();
  }

  override BDD getBDD(CstStage stage, Buddy buddy) {
    if(this.idxVars.length !is 0) {
      assert(false,
	     "CstBdd2BddExpr: Need to unroll the idxVars" ~
	     " before attempting to solve BDD");
    }
    auto bdd = _expr.getBDD(stage, buddy);
    return (~ bdd);
  }

  override CstNotBddExpr unroll(RndIterBase l, uint n) {
    bool shouldUnroll = false;
    foreach(var; idxVars()) {
      if(l is var) {
	shouldUnroll = true;
	break;
      }
    }
    if(! shouldUnroll) return this;
    else {
      return new CstNotBddExpr(_expr.unroll(l, n));
    }
  }

  this(CstBddExpr expr) {
    _expr = expr;
    _idxVars = expr.idxVars;
    // _preReqs = expr.preReqs;
  }
}

class CstBlock: CstBddExpr
{
  CstBddExpr[] _exprs;
  bool[] _booleans;

  override bool refresh(CstStage stage, Buddy buddy) {
    bool result = false;
    foreach (expr; _exprs) {
      result |= expr.refresh(stage, buddy);
    }
    return result;
  }
  
  
  override string name() {
    string name_ = "";
    foreach(expr; _exprs) {
      name_ ~= " & " ~ expr.name() ~ "\n";
    }
    return name_;
  }

  override CstVecPrim[] preReqs() {
    assert(false);
  }
    
  void _esdl__reset() {
    _exprs.length = 0;
  }

  bool isEmpty() {
    return _exprs.length == 0;
  }
  
  override CstVecPrim[] getRndPrims() {
    assert(false);
  }

  override CstBlock unroll(RndIterBase l, uint n) {
    assert(false, "Can not unroll a CstBlock");
  }

  override BDD getBDD(CstStage stage, Buddy buddy) {
    assert(false, "getBDD not implemented for CstBlock");
  }

  void opOpAssign(string op)(bool other)
    if(op == "~") {
      _booleans ~= other;
    }

  void opOpAssign(string op)(CstBddExpr other)
    if(op == "~") {
      _exprs ~= other;
    }

  void opOpAssign(string op)(CstVecExpr other)
    if(op == "~") {
      _exprs ~= other.toBdd();
    }

  void opOpAssign(string op)(CstBlock other)
    if(op == "~") {
      if(other is null) return;
      foreach(expr; other._exprs) {
	_exprs ~= expr;
      }
      foreach(boolean; other._booleans) {
	_booleans ~= boolean;
      }
    }
}
