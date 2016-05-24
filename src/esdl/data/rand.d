// Written in the D programming language.

// Copyright: Coverify Systems Technology 2012 - 2015
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

module esdl.data.rand;

import esdl.data.obdd;

import std.traits: isIntegral, isBoolean;
import esdl.data.bvec: isBitVector;
import std.algorithm : min, max;
import esdl.data.bstr;

import std.exception: enforce;

/// C++ type static_cast for down-casting when you are sure
private import std.typetuple: staticIndexOf;
private import std.traits: BaseClassesTuple, ParameterTypeTuple; // required for staticIndexOf

// For a given class, this template returns the SolverEnv for first
// class in the ancestory that has Randomization mixin -- if there is
// none, returns _esdl__SolverEnvRoot
template _esdl__SolverEnvBase(T) {
  static if(is(T == class) &&
	    is(T B == super) &&
	    is(B[0] == class)) {
    alias U = B[0];
    // check if the base class has Randomization
    // static if(__traits(compiles, _esdl__Solver!U)) {
    static if(__traits(compiles, U._esdl__thisHasRandomization()) &&
	      is(U == typeof(U._esdl__thisHasRandomization()))) {
      alias _esdl__SolverEnvBase = _esdl__Solver!U;
    }
    else {
      alias _esdl__SolverEnvBase = _esdl__SolverEnvBase!U;
    }
  }
  else {
    alias _esdl__SolverEnvBase = _esdl__SolverEnvRoot;
  }
}

string _esdl__SolverMixin(string name) {
  return "class " ~ name ~ q{(_esdl__T): _esdl__SolverEnvBase!_esdl__T
    {
      static if(is(_esdl__T == struct)) {
	_esdl__T* _esdl__outer;
	public void _esdl__setOuter()(ref _esdl__T outer) {
	  _esdl__outer = &outer;
	}
	public this(uint seed, string name, ref _esdl__T outer,
		    _esdl__SolverEnvRoot parent=null) {
	  _esdl__outer = &outer;
	  static if(_esdl__baseHasRandomization!_esdl__T) {
	    super(seed, name, outer, parent);
	  }
	  else {
	    super(seed, name, parent);
	  }
	  _esdl__initRands();
	  _esdl__initCsts();
	}
      }

      static if(is(_esdl__T == class)) {
	_esdl__T _esdl__outer;
	public void _esdl__setOuter()(_esdl__T outer) {
	  _esdl__outer = outer;
	}
	public this(uint seed, string name, _esdl__T outer,
		    _esdl__SolverEnvRoot parent=null) {
	  _esdl__outer = outer;
	  static if(_esdl__baseHasRandomization!_esdl__T) {
	    super(seed, name, outer, parent);
	  }
	  else {
	    super(seed, name, parent);
	  }
	  _esdl__initRands();
	  _esdl__initCsts();
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

      class _esdl__Constraint(string _esdl__CstString, size_t N): Constraint!_esdl__CstString
	{
	  long[N] _withArgs;

	  void withArgs(V...)(V values) if(allIntengral!V) {
	    static assert(V.length == N);
	    foreach(i, v; values) {
	      _withArgs[i] = v;
	    }
	  }

	  this(string name) {
	    super(this.outer, name, cast(uint) this.outer._esdl__cstsList.length);
	  }

	  public long _esdl__arg(size_t VAR)() {
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

      void _esdl__with(string _esdl__CstString, V...)(V values) {
	auto cstWith = new _esdl__Constraint!(_esdl__CstString, V.length)("randWith");
	cstWith.withArgs(values);
	_esdl__cstWith = cstWith;
      }

      mixin(_esdl__randsMixin!_esdl__T);

      debug(CONSTRAINTS) {
	pragma(msg, "// _esdl__randsMixin!" ~ _esdl__T.stringof ~ "\n");
	pragma(msg, _esdl__randsMixin!_esdl__T);
      }
    }
  };
}

mixin(_esdl__SolverMixin("_esdl__SolverEnvStruct"));

template _esdl__Solver(T) {
  // static if(__traits(compiles, T._esdl__hasRandomization)) {
  static if(is(T == class)) {
    alias _esdl__Solver = T._esdl__SolverEnv!T;
  }
  else {
    alias _esdl__Solver = _esdl__SolverEnvStruct!T;
  }
}

public T _esdl__staticCast(T, F)(const F from)
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

public auto _esdl__logicOr(P, Q)(P p, Q q) {
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

public auto _esdl__logicAnd(P, Q)(P p, Q q) {
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


public auto _esdl__lth(P, Q)(P p, Q q) {
  static if(is(P: RndVecExpr)) {
    return p.lth(q);
  }
  static if(is(Q: RndVecExpr)) {
    return q.gte(q);
  }
  static if((isBitVector!P || isIntegral!P) &&
	    (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p < q);
  }
}

public auto _esdl__lte(P, Q)(P p, Q q) {
  static if(is(P: RndVecExpr)) {
    return p.lte(q);
  }
  static if(is(Q: RndVecExpr)) {
    return q.gth(q);
  }
  static if((isBitVector!P || isIntegral!P) &&
	    (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p <= q);
  }
}

public auto _esdl__gth(P, Q)(P p, Q q) {
  static if(is(P: RndVecExpr)) {
    return p.gth(q);
  }
  static if(is(Q: RndVecExpr)) {
    return q.lte(q);
  }
  static if((isBitVector!P || isIntegral!P) &&
	    (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p > q);
  }
}

public auto _esdl__gte(P, Q)(P p, Q q) {
  static if(is(P: RndVecExpr)) {
    return p.gte(q);
  }
  static if(is(Q: RndVecExpr)) {
    return q.lth(q);
  }
  static if((isBitVector!P || isIntegral!P) &&
	    (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p >= q);
  }
}

public auto _esdl__equ(P, Q)(P p, Q q) {
  static if(is(P: RndVecExpr)) {
    return p.equ(q);
  }
  static if(is(Q: RndVecExpr)) {
    return q.equ(q);
  }
  static if((isBitVector!P || isIntegral!P) &&
	    (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p == q);
  }
}

public auto _esdl__neq(P, Q)(P p, Q q) {
  static if(is(P: RndVecExpr)) {
    return p.neq(q);
  }
  static if(is(Q: RndVecExpr)) {
    return q.neq(q);
  }
  static if((isBitVector!P || isIntegral!P) &&
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
	"    _esdl__" ~ NAME ~ ".doRandomization();\n" ~
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
      alias L = typeof(T.tupleof[I]);
      static if(isBitVector!L || isIntegral!L) {
	enum _esdl__RandInits =
	  "    _esdl__" ~ NAME ~ " = new typeof(_esdl__" ~
	  NAME ~ ")(\"" ~ NAME ~ "\", this);\n" ~
	  "    _esdl__randsList ~= _esdl__" ~ NAME ~ ";\n" ~
	  _esdl__RandInits!(T, I+1);
      }
      else static if(is(L == class)) {
	enum _esdl__RandInits =
	  "    assert(this._esdl__outer." ~ NAME ~
	  " !is null);\n    _esdl__" ~ NAME ~
	  " = new typeof(_esdl__" ~ NAME ~
	  ")(this._esdl__outer._esdl__randSeed, \"" ~
	  NAME ~ "\", this._esdl__outer." ~ NAME ~
	  ", this);\n    _esdl__randsList ~= _esdl__" ~ NAME ~
	  "._esdl__randsList;\n" ~ _esdl__RandInits!(T, I+1);
      }
      else static if(is(L == struct)) {
	enum _esdl__RandInits =
	  "    _esdl__" ~ NAME ~ " = new typeof(_esdl__" ~
	  NAME ~ ")(this._esdl__outer._esdl__randSeed, \"" ~
	  NAME ~ "\", this._esdl__outer." ~ NAME ~
	  ", this);\n    _esdl__randsList ~= _esdl__" ~ NAME ~
	  "._esdl__randsList;\n" ~ _esdl__RandInits!(T, I+1);
      }
      else static if(is(L == U*, U) && is(U == struct)) {
	enum _esdl__RandInits =
	  "    assert(this._esdl__outer." ~ NAME ~
	  " !is null);\n    _esdl__" ~ NAME ~
	  " = new typeof(_esdl__" ~ NAME ~
	  ")(this._esdl__outer._esdl__randSeed, \"" ~
	  NAME ~ "\", *(this._esdl__outer." ~ NAME ~
	  "), this);\n    _esdl__randsList ~= _esdl__" ~ NAME ~
	  "._esdl__randsList;\n" ~ _esdl__RandInits!(T, I+1);
      }
      else {			/* Arrays */
	enum _esdl__RandInits =
	  "    _esdl__" ~ NAME ~ " = new typeof(_esdl__" ~ NAME ~
	  ")(\"" ~ NAME ~ "\", this);\n" ~
	  "    _esdl__randsList ~= _esdl__" ~ NAME ~ ";\n" ~
	  _esdl__RandInits!(T, I+1);
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
	enum _esdl__RandSetOuter = _esdl__RandSetOuter!(T, I+1);
      }
      else static if(is(L == class)) {
	enum _esdl__RandSetOuter =
	  "    assert(this._esdl__outer." ~ NAME ~
	  " !is null);\n    _esdl__" ~ NAME ~
	  "._esdl__setOuter(this._esdl__outer." ~ NAME ~
	  ");\n" ~ _esdl__RandSetOuter!(T, I+1);
      }
      else static if(is(L == struct)) {
	enum _esdl__RandSetOuter =
	  "    _esdl__" ~ NAME ~
	  "._esdl__setOuter(this._esdl__outer." ~ NAME ~
	  ");\n" ~ _esdl__RandSetOuter!(T, I+1);
      }
      else static if(is(L == U*, U) && is(U == struct)) {
	enum _esdl__RandSetOuter =
	  "    _esdl__" ~ NAME ~
	  "._esdl__setOuter(*(this._esdl__outer." ~ NAME ~
	  "));\n" ~ _esdl__RandSetOuter!(T, I+1);
      }
      else {			/* arrays */
	enum _esdl__RandSetOuter = _esdl__RandSetOuter!(T, I+1);
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
      enum _esdl__RandDeclVars = _esdl__RandDeclVars!(T, I+1);
    }
    else {
      enum _esdl__RandDeclVars =
	"  _esdl__Rand!(_esdl__T, " ~ I.stringof ~ ") _esdl__" ~
	__traits(identifier, T.tupleof[I]) ~	";\n" ~ _esdl__RandDeclVars!(T, I+1);
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
	// When NAME is used explicitly, D looks for public access
	// permissions, this works fine if the _esdl__SolverEnvRoot
	// template is instantiated as a mixin inside the class that
	// has mixin Randomization
	// We want to eliminate the need to mixin Randomization for
	// the classes/structs that are hierarchical, so we need to
	// use explicit tupleof[N], since that does not use access
	// permissions
	// enum _esdl__RandDeclFuncs =
	  //   "  const auto " ~ NAME ~ "() { return this._esdl__outer." ~ NAME ~ "; }\n" ~
	  enum _esdl__RandDeclFuncs =
	    "  const auto " ~ NAME ~ "() { return this._esdl__outer.tupleof[" ~ I.stringof ~ "]; }\n" ~
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
	enum _esdl__ConstraintsDecl =
	  "  Constraint!(_esdl__constraintString!(_esdl__T, " ~ I.stringof ~ ")) " ~
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
	  " = new _esdl__Constraint!(_esdl__constraintString!(_esdl__T, " ~
	  I.stringof ~ "))(\"" ~ NAME ~ "\");\n    _esdl__cstsList ~= " ~
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
  alias hasRandAttr = hasRandInList!(__traits(getAttributes, T.tupleof[I]));
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


public void _esdl__randomize(T) (T t, _esdl__ConstraintBase withCst = null) {
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

  t._esdl__solverInst.doRandomization();

  // _esdl__setRands(t, t._esdl__solverInst._esdl__randsList,
  //		  t._esdl__solverInst._esdl__rGen);

  static if(__traits(compiles, t.postRandomize())) {
    t.postRandomize();
  }
}

abstract class _esdl__ConstraintBase
{
  this(_esdl__SolverEnvRoot eng, string name, string constraint, uint index) {
    _cstEng = eng;
    _name = name;
    _constraint = constraint;
    _index = index;
  }
  immutable string _constraint;
  protected bool _enabled = true;
  protected _esdl__SolverEnvRoot _cstEng;
  protected string _name;
  // index in the constraint Database
  protected uint _index;

  public bool isEnabled() {
    return _enabled;
  }

  public void enable() {
    _enabled = true;
  }

  public void disable() {
    _enabled = false;
  }

  public BDD getConstraintBDD() {
    BDD retval = _cstEng._esdl__buddy.one();
    return retval;
  }

  public string name() {
    return _name;
  }

  abstract public CstBlock getCstExpr();
}

abstract class Constraint(string C): _esdl__ConstraintBase
{
  this(_esdl__SolverEnvRoot eng, string name, uint index) {
    super(eng, name, C, index);
  }

  static char[] constraintXlate(string CST) {
    import esdl.data.cstx;
    CstParser parser = CstParser(CST);
    return parser.translate();
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

  public bool flip() {
    if(_bi > 31) {
      _bi = 0;
      _bv = uniform!"[]"(0, uint.max, _gen);
    }
    return cast(bool) _bv[_bi++];
  }

  public double get() {
    return uniform(0.0, 1.0, _gen);
  }

  @property public T gen(T)() {
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

  @property public void gen(T)(ref T t) {
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

  @property public auto gen(T1, T2)(T1 a, T2 b)
    if(isIntegral!T1 && isIntegral!T2) {
      return uniform(a, b, _gen);
    }

  @property public void gen(T, T1, T2)(ref T t, T1 a, T2 b)
    if(isIntegral!T1 && isIntegral!T2) {
      t = uniform(a, b, _gen);
    }
}

// Todo -- Make it a struct
class CstStage {
  int _id = -1;
  // List of randomized variables associated with this stage. A
  // variable can be associated with only one stage
  RndVecPrim[] _rndVecs;
  // The Bdd expressions that apply to this stage
  CstBddExpr[] _bddExprs;
  // These are unresolved idx variables
  RndVecIterVar[] _idxVars;
  // These are the length variables that this stage will solve
  // RndVecPrim[] _preReqs;
  CstBddExpr[] _bddExprsWithUnmetReqs;
  

  public void id(uint i) {
    _id = i;
  }

  public uint id() {
    return _id;
  }

  public bool solved() {
    if(_id != -1) return true;
    else return false;
  }

  // returns true if there are idx variables that need solving
  public bool hasIdxs() {
    foreach(idx; _idxVars) {
      if(! idx.isUnrollable()) return true;
    }
    return false;
  }

  public bool allReqsResolved() {
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

abstract class _esdl__SolverEnvRoot {
  // Keep a list of constraints in the class
  public _esdl__ConstraintBase[] _esdl__cstsList;
  public _esdl__ConstraintBase _esdl__cstWith;
  bool _esdl__cstWithChanged;

  // ParseTree parseList[];
  public RndVecPrim[] _esdl__randsList;
  _esdl__RandGen _esdl__rGen;

  public _esdl__RandGen _esdl__getRandGen() {
    return _esdl__rGen;
  }

  Buddy _esdl__buddy;

  // BddDomain[] _domains;
  BddDomain* _domains;

  _esdl__SolverEnvRoot _parent = null;

  CstBlock _esdl__cstStatements;

  this(uint seed, string name, _esdl__SolverEnvRoot parent=null) {
    debug(NOCONSTRAINTS) {
      assert(false, "Constraint engine started");
    }
    _esdl__rGen = new _esdl__RandGen(seed);
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
    _esdl__cstWith          = null;
    _esdl__cstWithChanged  = true;
    _esdl__randsList.length = 0;
  }

  public void _esdl__initRands() {}
  public void _esdl__initCsts() {}

  public void doRandomization() {}

  // list of constraint statements to solve at a given stage
  public void addCstStage(RndVecPrim prim, ref CstStage[] cstStages) {
    if(prim !is null) {
      if(prim.stage() is null) {
	CstStage stage = new CstStage();
	cstStages ~= stage;
	prim.stage = stage;
	stage._rndVecs ~= prim;
	// cstStages[stage]._rndVecs ~= prim;
      }
    }
    else {
      // FIXME -- just to check is Prim can ever be null
      // If this assert statement is not hit, take out the
      // if condition
      assert(false, "Prim can be null -- kill this assert");
    }
  }

  public void addCstStage(CstBddExpr expr, ref CstStage[] cstStages) {
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
	  stage._rndVecs ~= vec;
	  // cstStages[stage]._rndVecs ~= vec;
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

  public void mergeCstStages(CstStage fromStage, CstStage toStage,
			     ref CstStage[] cstStages) {
    if(fromStage is null) {
      // fromStage has not been created yet
      return;
    }
    foreach(ref vec; fromStage._rndVecs) {
      vec.stage = toStage;
    }
    toStage._rndVecs ~= fromStage._rndVecs;
    toStage._bddExprs ~= fromStage._bddExprs;
    if(cstStages[$-1] is fromStage) {
      cstStages.length -= 1;
    }
    else {
      fromStage._rndVecs.length = 0;
      fromStage._bddExprs.length = 0;
    }
  }

  void initDomains(T)(T t) {
    if(_domains is null || _esdl__cstWithChanged is true) {
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
        foreach(vec; stmt.getPrims()) {
      	if(vec.domIndex != uint.max) {
      	  vec.domIndex = uint.max;
      	}
        }
      }

      foreach(stmt; _esdl__cstStatements._exprs) {
	// import std.stdio;
	// writeln("initDomains Expression: ", stmt.name());
	foreach(vec; stmt.getPrims()) {
	  if(vec.domIndex == uint.max) {
	    // writeln("initDomains Vector: ", vec.name());
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

    _esdl__cstStatements._esdl__reset(); // start empty
    
    // take all the constraints -- even if disabled
    foreach(ref _esdl__ConstraintBase cst; _esdl__cstsList) {
      _esdl__cstStatements ~= cst.getCstExpr();
    }
    if(_esdl__cstWith !is null) {
      _esdl__cstStatements ~= _esdl__cstWith.getCstExpr();
    }

    // First we solve the constraint groups that are responsible for
    // setting the length of the rand!n dynamic arrays. After each
    // such constraint group is resolved, we go back and expand the
    // constraint expressions that depend on the IDX Variables.

    // Once we have unrolled all the IDXS, we go ahead and resolve
    // everything that remains.


    // Ok before we start looking at the constraints, we create a
    // stage for each and every @rand that we have at hand
    CstStage[] unsolvedStages;	// unresolved stages -- all
    // foreach(rnd; _esdl__randsList) {
    //   if(! rnd.isVecArr()) {
    // 	addCstStage(rnd, unsolvedStages);
    //   }
    //   else {
    //   }
    // }

    int stageIdx=0;
    // This variable is true when all the array lengths have been resolved
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
	   // stage._rndVecs.length !is 0 &&
	   stage.allReqsResolved()) {
	  solveStage(stage, stageIdx);
	}
	else {
	  // assert(stage._rndVecs.length !is 0);
	  unsolvedStages ~= stage;
	}
      }
      
    }
  }

  void solveStage(CstStage stage, ref int stageIdx) {
    import std.conv;
    // initialize the bdd vectors
    BDD solveBDD = _esdl__buddy.one();
    foreach(vec; stage._rndVecs) {
      if(vec.stage is stage) {
	if(vec.bddvec.isNull()) {
	  // import std.stdio;
	  // writeln(vec.name(), vec.domIndex);
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

    foreach(expr; exprs) {
      // import std.stdio;
      // writeln(expr.name());
      solveBDD = solveBDD & expr.getBDD(stage, _esdl__buddy);
      // writeln(expr.name());
    }

    // The idea is that we apply the max length constraint only if
    // there is another constraint on the lenght. If there is no
    // other constraint, then the array is taken care of later at
    // the time of setting the non-constrained random variables


    double[uint] bddDist;
    solveBDD.satDist(bddDist);

    auto solution = solveBDD.randSatOne(this._esdl__rGen.get(),
					bddDist);
    auto solVecs = solution.toVector();

    byte[] bits;
    if(solVecs.length != 0) {
      bits = solVecs[0];
    }

    foreach(vec; stage._rndVecs) {
      size_t value;
      enum WORDSIZE = 8 * size_t.sizeof;
      auto bitvals = solveBDD.getIndices(vec.domIndex);
      foreach(uint i, ref j; bitvals) {
	uint pos = i % WORDSIZE;
	uint word = i / WORDSIZE;
	if(bits.length == 0 || bits[j] == -1) {
	  value = value + ((cast(size_t) _esdl__rGen.flip()) << pos);
	}
	else if(bits[j] == 1) {
	  value = value + ((cast(size_t) 1) << pos);
	}
	if(pos == WORDSIZE - 1 || i == bitvals.length - 1) {
	  vec.value(value, word);
	  value = 0;
	}
      }
      // vec.value = value;
      // vec.bddvec = null;
    }
    if(stage !is null) stage.id(stageIdx);
    ++stageIdx;
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
    "  override public void doRandomization() {\n" ~
    "    super.doRandomization();\n" ~
    _esdl__RandRandomize!T ~ "  }\n";
  string rand_inits =
    "  public void _esdl__initRands()() {\n" ~
    _esdl__RandInits!T ~ "  }\n";
  string cst_inits =
    "  public void _esdl__initCsts()() {\n" ~
    _esdl__CstInits!T ~ "  }\n";
  string rand_decls = _esdl__RandDeclFuncs!T ~ _esdl__RandDeclVars!T;
  string rand_set_outer = "  public void _esdl__setObjOuter()() {\n" ~
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
  static {
    mixin(_esdl__SolverMixin("_esdl__SolverEnv"));
  }
  
  enum bool _esdl__hasRandomization = true;
  alias _esdl__Type = typeof(this);

  static public _esdl__Type _esdl__thisHasRandomization()() {
    return null;
  }

  alias _esdl__SolverThis = _esdl__Solver!_esdl__Type;

  // final auto _esdl__randEval(string NAME)() {
  //   return mixin(NAME);
  // }

  static if(// is(_esdl__T: Randomizable) ||
	    __traits(compiles, _esdl__solverInst)) {
    override public void _esdl__virtualRandomize(_esdl__ConstraintBase withCst = null) {
      _esdl__randomize(this, withCst);
    }
    override public _esdl__SolverThis _esdl__getSolver() {
      return _esdl__staticCast!_esdl__SolverThis(_esdl__solverInst);
    }
    override public void _esdl__initSolver() {
      if (_esdl__solverInst is null) {
	_esdl__solverInst =
	  new _esdl__SolverThis(_esdl__randSeed, typeid(_esdl__Type).stringof[8..$-1], this);
      }
      else {
	_esdl__getSolver()._esdl__setObjOuter();
      }
    }
  }
  else {
    _esdl__SolverEnvRoot _esdl__solverInst;
    public uint _esdl__randSeed;
    public _esdl__SolverThis _esdl__getSolver() {
      return _esdl__staticCast!_esdl__SolverThis(_esdl__solverInst);
    }
    public void _esdl__virtualRandomize(_esdl__ConstraintBase withCst = null) {
      _esdl__randomize(this, withCst);
    }
    void useThisBuddy() {
      import esdl.data.obdd;
      useBuddy(_esdl__solverInst._esdl__buddy);
    }
    public void seedRandom(int seed) {
      _esdl__randSeed = seed;
      if(_esdl__solverInst !is null) {
	_esdl__solverInst._esdl__rGen.seed(seed);
      }
    }
    alias srandom = seedRandom;	// SV names the similar method srandom
    public void _esdl__initSolver() {
      if (_esdl__solverInst is null) {
	_esdl__solverInst =
	  new _esdl__SolverThis(_esdl__randSeed, typeid(_esdl__Type).stringof[8..$-1], this);
      }
      else {
	_esdl__getSolver()._esdl__setObjOuter();
      }
    }
  }
  // static if(_esdl__baseHasRandomization!_esdl__Type) {
  // }
}

public void randomizeWith(string C, T, V...)(ref T t, V values)
  if(is(T == class) && allIntengral!V) {
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
      //			T, V.length)(t, "_esdl__withCst");
      // withCst.withArgs(values);
      // t._esdl__solverInst._esdl__cstWith = withCst;
    }
    else {
      alias CST = _esdl__Solver!(T)._esdl__Constraint!(C, V.length);
      auto cstWith = _esdl__staticCast!CST(t._esdl__solverInst._esdl__cstWith);
      cstWith.withArgs(values);
      t._esdl__solverInst._esdl__cstWithChanged = false;
    }
    t._esdl__virtualRandomize(t._esdl__solverInst._esdl__cstWith);
  }

public void randomize(T)(T t) {
  // FIXME
  // first check if the there are @rand or Constraint definitions but
  // missing mixin Randomization for some of the hierarchies
  t._esdl__virtualRandomize();
}

// FIXME add bitvectors to this template filter
template allIntengral(V...) {
  static if(V.length == 0) {
    enum bool allIntengral = true;
  }
  else static if(isIntegral!(V[0])) {
      enum bool allIntengral = allIntengral!(V[1..$]);
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

interface RndVecPrim
{
  abstract string name();
  abstract void doRandomization();
  abstract public bool isRand();
  abstract public long value();
  abstract public void value(size_t v, int word=0);
  abstract public CstStage stage();
  abstract public void stage(CstStage s);
  abstract public void _esdl__reset();
  abstract public bool isVecArr();
  abstract public uint domIndex();
  abstract public void domIndex(uint s);
  abstract public uint bitcount();
  abstract public bool signed();
  abstract public BddVec bddvec();
  abstract public void bddvec(BddVec b);
  abstract public RndVecPrim[] getPrimLens();
  abstract public void solveBefore(RndVecPrim other);
  abstract public void addPreRequisite(RndVecPrim other);

  // this method is used for getting implicit constraints that are required for
  // dynamic arrays and for enums
  abstract public BDD getPrimBdd(Buddy buddy);
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
abstract class RndVecExpr
{
  // alias toBdd this;

  // alias evaluate this;

  abstract string name();
  
  public CstBddExpr toBdd() {
    auto zero = new RndVecConst!int(0);
    return new RndVec2BddExpr(this, zero, CstBinBddOp.NEQ);
  }

  // Array of indexes this expression has to resolve before it can be
  // convertted into an BDD
  abstract public RndVecIterVar[] idxVars();

  // List of Array Variables
  abstract public RndVecPrim[] preReqs();

  bool isConst() {
    return false;
  }

  // get all the primary bdd vectors that constitute a given bdd
  // expression
  // The idea here is that we need to solve all the bdd vectors of a
  // given constraint statement together. And so, given a constraint
  // equation, we want to list out the elements that need to be
  // grouped together.
  abstract public RndVecPrim[] getPrims();

  // get all the primary bdd vectors that would be solved together
  public RndVecPrim[] getSolvables() {
    return getPrims();
  }
  
  // get the list of stages this expression should be avaluated in
  // abstract public CstStage[] getStages();
  abstract public BddVec getBDD(CstStage stage, Buddy buddy);

  abstract public long evaluate();

  abstract public RndVecExpr unroll(RndVecIterVar l, uint n);

  public RndVec2VecExpr opBinary(string op)(RndVecExpr other)
  {
    static if(op == "&") {
      return new RndVec2VecExpr(this, other, CstBinVecOp.AND);
    }
    static if(op == "|") {
      return new RndVec2VecExpr(this, other, CstBinVecOp.OR);
    }
    static if(op == "^") {
      return new RndVec2VecExpr(this, other, CstBinVecOp.XOR);
    }
    static if(op == "+") {
      return new RndVec2VecExpr(this, other, CstBinVecOp.ADD);
    }
    static if(op == "-") {
      return new RndVec2VecExpr(this, other, CstBinVecOp.SUB);
    }
    static if(op == "*") {
      return new RndVec2VecExpr(this, other, CstBinVecOp.MUL);
    }
    static if(op == "/") {
      return new RndVec2VecExpr(this, other, CstBinVecOp.DIV);
    }
    static if(op == "%") {
      return new RndVec2VecExpr(this, other, CstBinVecOp.REM);
    }
    static if(op == "<<") {
      return new RndVec2VecExpr(this, other, CstBinVecOp.LSH);
    }
    static if(op == ">>") {
      return new RndVec2VecExpr(this, other, CstBinVecOp.RSH);
    }
  }

  public RndVec2VecExpr opBinary(string op, Q)(Q q)
    if(isBitVector!Q || isIntegral!Q)
      {
	auto qq = new RndVecConst!Q(q);
	static if(op == "&") {
	  return new RndVec2VecExpr(this, qq, CstBinVecOp.AND);
	}
	static if(op == "|") {
	  return new RndVec2VecExpr(this, qq, CstBinVecOp.OR);
	}
	static if(op == "^") {
	  return new RndVec2VecExpr(this, qq, CstBinVecOp.XOR);
	}
	static if(op == "+") {
	  return new RndVec2VecExpr(this, qq, CstBinVecOp.ADD);
	}
	static if(op == "-") {
	  return new RndVec2VecExpr(this, qq, CstBinVecOp.SUB);
	}
	static if(op == "*") {
	  return new RndVec2VecExpr(this, qq, CstBinVecOp.MUL);
	}
	static if(op == "/") {
	  return new RndVec2VecExpr(this, qq, CstBinVecOp.DIV);
	}
	static if(op == "%") {
	  return new RndVec2VecExpr(this, qq, CstBinVecOp.REM);
	}
	static if(op == "<<") {
	  return new RndVec2VecExpr(this, qq, CstBinVecOp.LSH);
	}
	static if(op == ">>") {
	  return new RndVec2VecExpr(this, qq, CstBinVecOp.RSH);
	}
      }

  public RndVec2VecExpr opBinaryRight(string op, Q)(Q q)
    if(isBitVector!Q || isIntegral!Q)
      {
	auto qq = new RndVecConst!Q(q);
	static if(op == "&") {
	  return new RndVec2VecExpr(qq, this, CstBinVecOp.AND);
	}
	static if(op == "|") {
	  return new RndVec2VecExpr(qq, this, CstBinVecOp.OR);
	}
	static if(op == "^") {
	  return new RndVec2VecExpr(qq, this, CstBinVecOp.XOR);
	}
	static if(op == "+") {
	  return new RndVec2VecExpr(qq, this, CstBinVecOp.ADD);
	}
	static if(op == "-") {
	  return new RndVec2VecExpr(qq, this, CstBinVecOp.SUB);
	}
	static if(op == "*") {
	  return new RndVec2VecExpr(qq, this, CstBinVecOp.MUL);
	}
	static if(op == "/") {
	  return new RndVec2VecExpr(qq, this, CstBinVecOp.DIV);
	}
	static if(op == "%") {
	  return new RndVec2VecExpr(qq, this, CstBinVecOp.REM);
	}
	static if(op == "<<") {
	  return new RndVec2VecExpr(qq, this, CstBinVecOp.LSH);
	}
	static if(op == ">>") {
	  return new RndVec2VecExpr(qq, this, CstBinVecOp.RSH);
	}
      }

  public RndVecExpr opIndex(RndVecExpr index)
  {
    // assert(false, "Index operation defined only for Arrays");
    return new RndVecSliceExpr(this, index);
  }

  public RndVecExpr opSlice(P)(P p)
    if(isIntegral!P || isBitVector!P) {
      return new RndVecSliceExpr(this, new RndVecConst!P(p));
    }

  public RndVecExpr opSlice(RndVecExpr lhs, RndVecExpr rhs)
  {
    return new RndVecSliceExpr(this, lhs, rhs);
  }

  public RndVecExpr opSlice(P, Q)(P p, Q q)
    if((isIntegral!P || isBitVector!P) && (isIntegral!Q || isBitVector!Q)) {
      return new RndVecSliceExpr(this, new RndVecConst!P(p),
				 new RndVecConst!Q(q));
    }

  public RndVec2BddExpr lth(Q)(Q q)
    if(isBitVector!Q || isIntegral!Q) {
      auto qq = new RndVecConst!Q(q);
      return this.lth(qq);
    }

  public RndVec2BddExpr lth(RndVecExpr other) {
    return new RndVec2BddExpr(this, other, CstBinBddOp.LTH);
  }

  public RndVec2BddExpr lte(Q)(Q q)
    if(isBitVector!Q || isIntegral!Q) {
      auto qq = new RndVecConst!Q(q);
      return this.lte(qq);
    }

  public RndVec2BddExpr lte(RndVecExpr other) {
    return new RndVec2BddExpr(this, other, CstBinBddOp.LTE);
  }

  public RndVec2BddExpr gth(Q)(Q q)
    if(isBitVector!Q || isIntegral!Q) {
      auto qq = new RndVecConst!Q(q);
      return this.gth(qq);
    }

  public RndVec2BddExpr gth(RndVecExpr other) {
    return new RndVec2BddExpr(this, other, CstBinBddOp.GTH);
  }

  public RndVec2BddExpr gte(Q)(Q q)
    if(isBitVector!Q || isIntegral!Q) {
      auto qq = new RndVecConst!Q(q);
      return this.gte(qq);
    }

  public RndVec2BddExpr gte(RndVecExpr other) {
    return new RndVec2BddExpr(this, other, CstBinBddOp.GTE);
  }

  public RndVec2BddExpr equ(Q)(Q q)
    if(isBitVector!Q || isIntegral!Q) {
      auto qq = new RndVecConst!Q(q);
      return this.equ(qq);
    }

  public RndVec2BddExpr equ(RndVecExpr other) {
    return new RndVec2BddExpr(this, other, CstBinBddOp.EQU);
  }

  public RndVec2BddExpr neq(Q)(Q q)
    if(isBitVector!Q || isIntegral!Q) {
      auto qq = new RndVecConst!Q(q);
      return this.neq(qq);
    }

  public RndVec2BddExpr neq(RndVecExpr other) {
    return new RndVec2BddExpr(this, other, CstBinBddOp.NEQ);
  }

  public CstNotBddExpr opUnary(string op)() {
    static if(op == "*") {	// "!" in cstx is translated as "*"
      return new CstNotBddExpr(this.toBdd());
    }
  }

  public CstBdd2BddExpr implies(CstBddExpr other) {
    return new CstBdd2BddExpr(this.toBdd(), other, CstBddOp.LOGICIMP);
  }

  // public CstBdd2BddExpr implies(RndVecExpr other)
  // {
  //   return new CstBdd2BddExpr(this.toBdd(), other.toBdd(), CstBddOp.LOGICIMP);
  // }

  public CstBdd2BddExpr logicOr(CstBddExpr other) {
    return new CstBdd2BddExpr(this.toBdd(), other, CstBddOp.LOGICOR);
  }

  // public CstBdd2BddExpr logicOr(RndVecExpr other)
  // {
  //   return new CstBdd2BddExpr(this.toBdd(), other.toBdd(), CstBddOp.LOGICOR);
  // }

  public CstBdd2BddExpr logicAnd(CstBddExpr other) {
    return new CstBdd2BddExpr(this.toBdd(), other, CstBddOp.LOGICAND);
  }

  // public CstBdd2BddExpr logicAnd(RndVecExpr other)
  // {
  //   return new CstBdd2BddExpr(this.toBdd(), other.toBdd(), CstBddOp.LOGICAND);
  // }

  bool isOrderingExpr() {
    return false;		// only RndVecOrderingExpr return true
  }

}


class RndVecLen(RV): RndVecExpr, RndVecPrim
{

  // This bdd has the constraint on the max length of the array
  BDD _primBdd;
  
  RndVecIter!RV _idxVar;

  RV _parent;

  BddVec _bddvec;
  uint _domIndex = uint.max;
  CstStage _stage = null;

  string _name;

  RndVecPrim[] _preReqs;

  override string name() {
    return _name;
  }

  public this(string name, RV parent) {
    _name = name;
    _parent = parent;
  }

  override public RndVecPrim[] preReqs() {
    return _preReqs ~ _parent.preReqs();
  }

  override RndVecIterVar[] idxVars() {
    return _parent.idxVars();
  }

  public auto getSolver() {
    if(_parent is null) {
      assert(false, "No parent associated with RndVecLen");
    }
    return _parent.getSolver();
  }

  override public RndVecPrim[] getPrims() {
    return _parent.getPrimLens();
  }

  public RndVecPrim[] getPrimLens() {
    assert(false);
  }
  
  // override public CstStage[] getStages() {
  //   CstStage[] stages;
  //   if(isRand) stages = [this.stage()];
  //   return stages;
  // }

  override public BddVec getBDD(CstStage s, Buddy buddy) {
    assert(stage() !is null, "stage null for: " ~ name());
    if(this.isRand && stage() is s) {
      return _bddvec;
    }
    else if((! this.isRand) ||
	    this.isRand && stage().solved()) { // work with the value
      return buddy.buildVec(value());
    }
    else {
      assert(false, "Constraint evaluation in wrong stage");
    }
  }

  override public long evaluate() {
    if(! this.isRand || stage().solved()) {
      return value();
    }
    else {
      import std.conv;
      assert(false, "Rand variable " ~ _name ~ " evaluation in wrong stage: " ~
	     stage()._id.to!string);
    }
  }

  void doRandomization() {
    assert(false);
  }
  
  public bool isRand() {
    import std.traits;
    if(isStaticArray!(RV.L)) return false;
    else return true;
  }

  public CstStage stage() {
    return _stage;
  }

  public void stage(CstStage s) {
    _stage = s;
  }

  public uint domIndex() {
    return _domIndex;
  }

  public void domIndex(uint s) {
    _domIndex = s;
  }

  public BddVec bddvec() {
    return _bddvec;
  }

  public void bddvec(BddVec b) {
    _bddvec = b;
  }

  public T to(T)()
    if(is(T == string)) {
      import std.conv;
      if(isRand) {
	return "RAND-" ~ "#" ~ _name ~ ":" ~ value().to!string();
      }
      else {
	return "VAL#" ~ _name ~ ":" ~ value().to!string();
      }
    }

  override public string toString() {
    return this.to!string();
  }

  public BDD getPrimBdd(Buddy buddy) {
    if(_primBdd.isZero()) {
      _primBdd = this.bddvec.lte(buddy.buildVec(_parent.maxArrLen));
    }
    return _primBdd;
  }
  
  public void idxVar(RndVecIter!RV var) {
    _idxVar = var;
  }

  public RndVecIter!RV idxVar() {
    return _idxVar;
  }

  public RndVecIter!RV makeIdxVar() {
    if(_idxVar is null) {
      _idxVar = new RndVecIter!RV(_parent);
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

  public long value() {
    return _parent.getLen();
  }

  public void value(size_t v, int word = 0) {
    assert(word == 0);
    // import std.stdio;
    // writeln("Setting value for arrlen for ", _parent.name, " to ", v);
    // import std.stdio;
    // writeln("Setting length for array: ", _parent.name(), " to ", v);
    _parent.setLen(cast(size_t) v);
    // writeln("Getting length for array: ", _parent.name(), " as ", _parent.getLen());
    
  }

  override public RndVecLen!RV unroll(RndVecIterVar l, uint n) {
    return _parent.unroll(l,n).arrLen();
  }

  void _esdl__reset() {
    stage = null;
  }

  bool isVecArr() {
    return false;
  }

  void solveBefore(RndVecPrim other) {
    other.addPreRequisite(this);
  }

  void addPreRequisite(RndVecPrim prim) {
    _preReqs ~= prim;
  }
}

mixin template EnumConstraints(T) {
  static if(is(T == enum)) {
    BDD _primBdd;
    override public BDD getPrimBdd(Buddy buddy) {
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
  }
}

template _esdl__Rand(T, int I)
{
  import std.traits;
  alias L = typeof(T.tupleof[I]);
  static if(isArray!L) {
    alias _esdl__Rand = RndVecArr!(T, I);
  }
  else static if(isBitVector!L || isIntegral!L) {
    alias _esdl__Rand = RndVec!(T, I);
  }
  else static if(is(L == class) || is(L == struct)) {
    alias _esdl__Rand = _esdl__Solver!L;
  }
  else static if(is(L == U*, U) && is(U == struct)) {
    alias _esdl__Rand = _esdl__Solver!U;
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

template _esdl__ArrOrder(T) {
  import std.traits;
  import std.range;
  static if(isArray!T) {
    enum int _esdl__ArrOrder = 1 + _esdl__ArrOrder!(ElementType!T);
  }
  else {
    enum int _esdl__ArrOrder = 0;
  }
}

template _esdl__ArrOrder(T, int I, int N=0) {
  enum int _esdl__ArrOrder = _esdl__ArrOrder!(typeof(T.tupleof[I])) - N;
}


// T represents the type of the declared array/non-array member
// N represents the level of the array-elements we have to traverse
// for the elements this RndVec represents
class RndVec(T, int I, int N=0) if(_esdl__ArrOrder!(T, I, N) == 0): RndVecExpr, RndVecPrim
{
  import std.traits;
  import std.range;
  import esdl.data.bvec;

  alias L = typeof(T.tupleof[I]);
  alias R = getRandAttr!(T, I);
  alias E = ElementTypeN!(L, N);
  alias RV = typeof(this);
  // enum int ORDER = _esdl__ArrOrder!(T, I, N);

  string _name;

  RndVecPrim[] _preReqs;

  override string name() {
    return _name;
  }

  mixin EnumConstraints!E;

  BddVec _bddvec;
  uint _domIndex = uint.max;
  CstStage _stage = null;

  void _esdl__reset() {
    stage = null;
  }

  bool isVecArr() {
    return false;
  }

  BDD getPrimBdd(Buddy buddy) {
    return buddy.one();
  }

  override public BddVec getBDD(CstStage s, Buddy buddy) {
    assert(stage(), "Stage not set for " ~ this.name());
    if(this.isRand && s is stage()) {
      return _bddvec;
    }
    else if((! this.isRand) ||
	    this.isRand && stage().solved()) { // work with the value
      return buddy.buildVec(value());
    }
    else {
      assert(false, "Constraint evaluation in wrong stage");
    }
  }

  override public long evaluate() {
    if(! this.isRand || stage().solved()) {
      return value();
    }
    else {
      import std.conv;
      assert(false, "Rand variable " ~ _name ~
	     " evaluation in wrong stage: " ~ stage()._id.to!string);
    }
  }

  public bool isRand() {
    return true;
  }

  public CstStage stage() {
    return _stage;
  }

  public void stage(CstStage s) {
    _stage = s;
  }

  public uint domIndex() {
    return _domIndex;
  }

  public void domIndex(uint s) {
    _domIndex = s;
  }

  public BddVec bddvec() {
    return _bddvec;
  }

  public void bddvec(BddVec b) {
    _bddvec = b;
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

  public S to(S)()
    if(is(S == string)) {
      import std.conv;
      if(isRand) {
	return "RAND-" ~ "#" ~ _name ~ ":" ~ value().to!string();
      }
      else {
	return "VAL#" ~ _name ~ ":" ~ value().to!string();
      }
    }

  override public string toString() {
    return this.to!string();
  }

  void solveBefore(RndVecPrim other) {
    other.addPreRequisite(this);
  }

  void addPreRequisite(RndVecPrim prim) {
    _preReqs ~= prim;
  }

  void build() {}
    
  static if(N == 0) {
    alias _esdl__SolverThis = _esdl__Solver!T._esdl__Solver;
    _esdl__SolverThis _solver;

    public this(string name, _esdl__SolverThis solver) {
      _name = name;
      _solver = solver;
    }

    override RndVecPrim[] preReqs() {
      return _preReqs;
    }

    override RndVecIterVar[] idxVars() {
      return [];
    }

    override public RndVecPrim[] getPrims() {
      if(isRand) return [this];
      else return [];
    }

    public RndVecPrim[] getPrimLens() {
      assert(false);
    }

    override public RV unroll(RndVecIterVar l, uint n) {
      // idxVars is always empty
      return this;
    }

    public void doRandomization() {
      if(stage is null) {
	_solver._esdl__getRandGen().gen(_solver._esdl__outer.tupleof[I]);
      }
    }

    public _esdl__SolverThis getSolver() {
      return _solver;
    }

    public long value() {
      return cast(long) (_solver._esdl__outer.tupleof[I]);
    }

    public void value(size_t v, int word = 0) {
      static if(isIntegral!L) {
	if(word == 0) {
	  _solver._esdl__outer.tupleof[I] = cast(L) v; // = cast(L) toBitVec(v      }
	}
	else {
	  static if(size_t.sizeof == 4 && (is(L == long) || is(L == ulong))) {
	    assert(word == 1);	// 32 bit machine with long integral
	    L val = v;
	    val = val << (8 * size_t.sizeof);
	    _solver._esdl__outer.tupleof[I] += val;
	  }
	  else {
	    assert(false, "word has to be 0 for integrals");
	  }
	}
      }
      else {
	_solver._esdl__outer.tupleof[I]._setNthWord(v, word); // = cast(L) toBitVec(v);
      }
    }
  }

  else {			// if (N != 0)
    alias P = RndVecArr!(T, I, N-1);
    P _parent;
    RndVecExpr _indexExpr = null;
    int _index = 0;

    public this(string name, P parent, RndVecExpr indexExpr) {
      _name = name;
      _parent = parent;
      _indexExpr = indexExpr;
    }

    public this(string name, P parent, uint index) {
      _name = name;
      _parent = parent;
      _index = index;
    }

    override RndVecPrim[] preReqs() {
      if(_indexExpr) {
	return _preReqs ~ _parent.arrLen() ~
	  _parent.preReqs() ~ _indexExpr.preReqs();
      }
      else {
	return _preReqs ~ _parent.arrLen() ~ _parent.preReqs();
      }
    }

    override RndVecIterVar[] idxVars() {
      if(_indexExpr) {
	return _parent.idxVars() ~ _indexExpr.idxVars();
      }
      else {
	return _parent.idxVars();
      }
    }

    public RndVecPrim[] getPrimLens() {
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
    override public RndVecPrim[] getPrims() {
      RndVecPrim[] prims;
      if(_indexExpr) {
	// FIXME -- if the expression has been solved
	// return _parent.getPrims(_indexExpr.evaluate()) ;
	prims = _indexExpr.getPrims();
	foreach(pp; _parent.getPrims(-1)) {
	  prims ~= pp;
	}
      }
      else {
	foreach(pp; _parent.getPrims(_index)) {
	  prims ~= pp;
	}
      }
      return prims;
    }

    override public RV unroll(RndVecIterVar l, uint n) {
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

    public void doRandomization() {
      if(stage is null) {
	E val;
	getSolver()._esdl__getRandGen().gen(val);
	value(val);
      }
    }

    public _esdl__Solver!(T)._esdl__Solver getSolver() {
      if(_parent is null) {
	assert(false, "No parent associated with RndVec");
      }
      return _parent.getSolver();
    }

    long value() {
      if(_indexExpr) {
	return _parent.getVal(cast(size_t) _indexExpr.evaluate());
      }
      else {
	return _parent.getVal(_index);
      }
    }

    void value(size_t v, int word = 0) {
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
  }
};

// Arrays (Multidimensional arrays as well)
class RndVecArr(T, int I, int N=0) if(_esdl__ArrOrder!(T, I, N) != 0): RndVecPrim
{
  import std.traits;
  import std.range;
  import esdl.data.bvec;
  alias M = typeof(T.tupleof[I]);
  alias L = ElementTypeN!(M, N);
  alias R = getRandAttr!(T, I);
  alias E = ElementTypeN!(M, N+1);
  alias RV = typeof(this);
  enum int ORDER = _esdl__ArrOrder!(T, I, N);

  static if(ORDER > 1) {
    alias EV = RndVecArr!(T, I, N+1);
  }
  else {
    alias EV = RndVec!(T, I, N+1);
  }

  EV[] _elems;

  RndVecLen!RV _arrLen;
  RndVecPrim[] _preReqs;

  string _name;
  override string name() {
    return _name;
  }

  size_t maxArrLen() {
    static if(isStaticArray!L) {
      return L.length;
    }
    else {
      return getRandAttr!(T, I, N);
    }
  }

  void opIndexAssign(EV c, size_t idx) {
    _elems[idx] = c;
  }

  public RndVecLen!RV length() {
    return _arrLen;
  }

  public RndVecLen!RV arrLen() {
    return _arrLen;
  }

  public bool isVecArr() {
    return true;
  }

  BDD getPrimBdd(Buddy buddy) {
    return buddy.one();
  }

  public void doRandomization() {
    if(_elems.length == 0) this.build();
    assert(arrLen !is null);
    for(size_t i=0; i != arrLen.evaluate(); ++i) {
      this[i].doRandomization();
    }
  }

  // override public RndVec2VecExpr opIndex(RndVecExpr idx) {
  //   return new RndVec2VecExpr(this, idx, CstBinVecOp.IDXINDEX);
  // }

  public EV opIndex(RndVecExpr idx) {
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

  public EV opIndex(size_t idx) {
    build();
    assert(_elems[idx]._indexExpr is null);
    return _elems[idx];
  }

  auto elements() {
    this.build();
    auto idx = arrLen.makeIdxVar();
    return this[idx];
  }

  auto iterator() {
    this.build();
    auto idx = arrLen.makeIdxVar();
    return idx;
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
    // }
    // }
    // else static if(isDynamicArray!E) {
    // 	// if(! built()) {
    // 	for (size_t i=0; i!=maxArrLen; ++i) {
    // 	  if(this[i] is null) {
    // 	    import std.conv: to;
    // 	    this[i] = new EV(_name ~ "[" ~ i.to!string() ~ "]",
    // 			     this, new RndVecConst!size_t(i));
    // 	  }
    // 	}
    // 	// }
    //   }
    // else static if(isStaticArray!E) {
    // 	// if(! built()) {
    // 	for (size_t i=0; i!=maxArrLen; ++i) {
    // 	  if(this[i] is null) {
    // 	    import std.conv: to;
    // 	    this[i] = new EV(_name ~ "[" ~ i.to!string() ~ "]",
    // 			     this, new RndVecConst!size_t(i));
    // 	  }
    // 	}
    // 	// }
    //   }
  }
	
  public void _esdl__reset() {
    _arrLen.stage = null;
    foreach(elem; _elems) {
      if(elem !is null) {
	elem._esdl__reset();
      }
    }
  }

  static private size_t getLen(A, N...)(ref A arr, N idx) if(isArray!A) {
    static if(N.length == 0) return arr.length;
    else {
      return getLen(arr[idx[0]], idx[1..$]);
    }
  }

  static private void setLen(A, N...)(ref A arr, size_t v, N idx) if(isArray!A) {
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

  static private long getVal(A, N...)(ref A arr, N idx) if(isArray!A &&
							   N.length > 0 &&
							   isIntegral!(N[0])) {
    static if(N.length == 1) return arr[idx[0]];
    else {
      return getVal(arr[idx[0]], idx[1..$]);
    }
  }

  static private void setVal(A, N...)(ref A arr, size_t v, int word, N idx)
    if(isArray!A && N.length > 0 && isIntegral!(N[0])) {
      static if(N.length == 1) {
	alias AE = ElementType!A;
	static if(isIntegral!AE) {
	  if(word == 0) {
	    arr[idx[0]] = cast(AE) v;
	  }
	  else {
	    static if(size_t.sizeof == 4) {
	      assert(word == 1);	// 32 bit machine with long integral
	      AE val = v;
	      val = val << (8 * size_t.sizeof);
	      arr[idx[0]] += val;
	    }
	    else {
	      assert(false, "word has to be 0 for integrals");
	    }
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

  public bool isRand() {
    assert(false, "isRand not implemented for RndVecVar");
  }

  public long value() {
    assert(false, "value not implemented for RndVecVar");
  }

  public void value(size_t v, int word = 0) {
    assert(false, "value not implemented for RndVecVar");
  }

  public CstStage stage() {
    return arrLen().stage();
    // assert(false, "stage not implemented for RndVecVar");
  }

  public void stage(CstStage s) {
    assert(false, "stage not implemented for RndVecVar");
  }

  public uint domIndex() {
    assert(false, "domIndex not implemented for RndVecVar");
  }

  public void domIndex(uint s) {
    assert(false, "domIndex not implemented for RndVecVar");
  }

  public uint bitcount() {
    assert(false, "bitcount not implemented for RndVecVar");
  }

  public bool signed() {
    assert(false, "signed not implemented for RndVecVar");
  }

  public BddVec bddvec() {
    assert(false, "bddvec not implemented for RndVecVar");
  }

  public void bddvec(BddVec b) {
    assert(false, "bddvec not implemented for RndVecVar");
  }

  void solveBefore(RndVecPrim other) {
    other.addPreRequisite(this);
  }

  void addPreRequisite(RndVecPrim prim) {
    _preReqs ~= prim;
  }

  static if(N == 0) {
    _esdl__Solver!(T)._esdl__Solver _solver;

    static if(isStaticArray!L) {
      static assert(__traits(isSame, R, rand));
      enum int maxLen = L.length;
      public this(string name, _esdl__Solver!(T)._esdl__Solver solver) {
	_name = name;
	_solver = solver;
	_arrLen = new RndVecLen!RV(name ~ ".len", this);
      }
    }

    static if(isDynamicArray!L) {
      enum int maxLen = getRandAttr!(T, I, N);
      public this(string name, _esdl__Solver!(T)._esdl__Solver solver) {
	_name = name;
	_solver = solver;
	_arrLen = new RndVecLen!RV(name ~ ".len", this);

      }
    }

    public RndVecPrim[] preReqs() {
      return _preReqs;		// N = 0 -- no _parent
    }

    RndVecIterVar[] idxVars() {
      return [];
    }

    public EV[] getPrims(int idx) {
      if(idx < 0) return _elems;
      else return [_elems[idx]];
    }

    public RndVecPrim[] getPrims() {
      RndVecPrim[] prims;
      foreach(elem; _elems) {
	prims ~= elem;
      }
      return prims;
    }

    public RndVecPrim[] getPrimLens() {
      RndVecPrim[] prims;
      if(_arrLen.isRand) prims ~= _arrLen;
      return prims;
    }
    
    public RV unroll(RndVecIterVar l, uint n) {
      return this;
    }

    public _esdl__Solver!(T)._esdl__Solver getSolver() {
      return _solver;
    }


    public long getVal(J...)(J idx) if(isIntegral!(J[0])) {
      return getVal(_solver._esdl__outer.tupleof[I], idx);
    }

    public void setVal(J...)(size_t v, int word, J idx) if(isIntegral!(J[0])) {
      setVal(_solver._esdl__outer.tupleof[I], v, word, idx);
    }

    public size_t getLen(N...)(N idx) {
      return getLen(_solver._esdl__outer.tupleof[I], idx);
    }

    public void setLen(N...)(size_t v, N idx) {
      setLen(_solver._esdl__outer.tupleof[I], v, idx);
    }

  }
  
  else {
    alias P = RndVecArr!(T, I, N-1);
    P _parent;
    RndVecExpr _indexExpr = null;
    int _index = 0;

    public this(string name, P parent, RndVecExpr indexExpr) {
      _name = name;
      _parent = parent;
      _indexExpr = indexExpr;
      _arrLen = new RndVecLen!RV(name ~ ".len", this);
    }

    public this(string name, P parent, uint index) {
      _name = name;
      _parent = parent;
      _index = index;
      _arrLen = new RndVecLen!RV(name ~ ".len", this);
    }

    public RndVecPrim[] preReqs() {
      if(_indexExpr) {
	return  _preReqs ~ _parent.arrLen() ~
	  _indexExpr.preReqs() ~ _parent.preReqs();
      }
      else {
	return _preReqs ~ _parent.arrLen() ~ _parent.preReqs();
      }
    }

    RndVecIterVar[] idxVars() {
      if(_indexExpr) {
	return _parent.idxVars() ~ _indexExpr.idxVars();
      }
      else {
	return _parent.idxVars();
      }
    }

    public EV[] getPrims(int idx) {
      EV[] prims;
      if(_indexExpr) {
	foreach(pp; _parent.getPrims(-1)) {
	  if(idx < 0) prims ~= pp._elems;
	  else prims ~= pp._elems[idx];
	}
      }
      else {
	foreach(pp; _parent.getPrims(_index)) {
	  if(idx < 0) prims ~= pp._elems;
	  else prims ~= pp._elems[idx];
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
    public RndVecPrim[] getPrims() {
      RndVecPrim[] prims;
      if(_indexExpr) {
	prims = _indexExpr.getPrims();
	foreach(pp; _parent.getPrims(-1)) {
	  prims ~= pp;
	}
      }
      else {
	foreach(pp; _parent.getPrims(_index)) {
	  prims ~= pp;
	}
      }
      return prims;
    }

    public RndVecPrim[] getPrimLens() {
      // if(_index.idxVars.length is 0)
      if(_indexExpr is null) {
	return [_parent[_index].arrLen()];
      }
      if(_indexExpr.isConst()) {
	RndVecPrim[] prims;
	prims ~= _parent[cast(size_t) _indexExpr.evaluate()].getPrimLens();
	return prims;
      }
      else {
	RndVecPrim[] prims;
	foreach(p; getPrims()) {
	  // import std.stdio;
	  // writeln(_parent.name(), " ", p.name());
	  prims ~= p.getPrimLens();
	}
	return prims;
      }
    }

    public RV unroll(RndVecIterVar l, uint n) {
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

    public _esdl__Solver!(T)._esdl__Solver getSolver() {
      if(_parent is null) {
	assert(false, "No parent associated with RndVec");
      }
      return _parent.getSolver();
    }

    public size_t getLen(N...)(N idx) {
      return _parent.getLen(_index, idx);
    }

    public void setLen(N...)(size_t v, N idx) {
      _parent.setLen(v, _index, idx);
    }

    public long getVal(N...)(N idx) if(isIntegral!(N[0])) {
      if(_indexExpr) {
	assert(_indexExpr.isConst());
	return _parent.getVal(cast(size_t) _indexExpr.evaluate(), idx);
      }
      else {
	return _parent.getVal(_index, idx);
      }
    }

    public void setVal(N...)(size_t v, int word, N idx) if(isIntegral!(N[0])) {
      if(_indexExpr) {
	assert(_indexExpr.isConst());
	_parent.setVal(v, word, cast(size_t) _indexExpr.evaluate(), idx);
      }
      else {
	_parent.setVal(v, word, _index, idx);
      }
    }
  }
};

class RndVecIter(RV): RndVecIterVar, RndVecPrim
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

  override RndVecIterVar[] idxVars() {
    return _arrVar.idxVars() ~ this;
  }

  public auto getSolver() {
    if(_arrVar is null) {
      assert(false, "No arrVar associated RndVecIterVar");
    }
    return _arrVar.getSolver();
  }

  override uint maxVal() {
    if(! this.isUnrollable()) {
      assert(false, "Can not find maxVal since the "
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
  override public RndVecPrim[] getPrims() {
    return []; // _arrVar.arrLen.getPrims();
  }

  public RndVecPrim[] getPrimLens() {
    return [_arrVar.arrLen];
  }

  public bool isRand() {
    return _arrVar.arrLen.isRand();
  }
  public long value() {
    return _arrVar.arrLen.value();
  }
  public void value(size_t v, int word = 0) {
    // import std.stdio;
    // writeln("Setting value for arrlen for ", _arrVar.name, " to ", v);
    assert(word == 0);
    _arrVar.arrLen.value(cast(size_t) v);
  }
  void doRandomization() {
    assert(false);
  }
  public CstStage stage() {
    return _arrVar.arrLen.stage();
  }
  public void stage(CstStage s) {
    _arrVar.arrLen.stage(s);
  }
  public uint domIndex() {
    return _arrVar.arrLen.domIndex;
  }
  public void domIndex(uint s) {
    _arrVar.arrLen.domIndex(s);
  }
  public uint bitcount() {
    return _arrVar.arrLen.bitcount();
  }
  public bool signed() {
    // return _arrVar.arrLen.signed();
    return false;
  }
  public BddVec bddvec() {
    return _arrVar.arrLen.bddvec();
  }
  public void bddvec(BddVec b) {
    _arrVar.bddvec(b);
  }
  override public string name() {
    string n = _arrVar.arrLen.name();
    return n[0..$-3] ~ "iter";
  }
  override public RndVecExpr unroll(RndVecIterVar l, uint n) {
    // import std.stdio;
    // writeln("unrolling: ", arrVar.name());
    if(this !is l) {
      return _arrVar.unroll(l,n).arrLen().makeIdxVar();
    }
    else {
      return new RndVecConst!size_t(n);
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

  void solveBefore(RndVecPrim other) {
    assert(false);
  }

  void addPreRequisite(RndVecPrim other) {
    assert(false);
  }

}
				    
// This class represents an unrolled Foreach idx at vec level
abstract class RndVecIterVar: RndVecExpr
{
  string _name;

  override string name() {
    return name;
  }

  this(string name) {
    _name = name;
  }

  // _idxVar will point to the array this RndVecIterVar is tied to

  uint maxVal();

  // this will not return the arrVar since the length variable is
  // not getting constrained here
  override RndVecPrim[] preReqs() {
    return [];
  }

  bool isUnrollable();

  // get all the primary bdd vectors that constitute a given bdd expression
  override public RndVecPrim[] getPrims();

  // get the list of stages this expression should be avaluated in
  // override public CstStage[] getStages() {
  //   return arrVar.arrLen.getStages();
  // }

  override public BddVec getBDD(CstStage stage, Buddy buddy) {
    assert(false, "Can not getBDD for a Idx Variable without unrolling");
  }

  override public long evaluate() {
    assert(false, "Can not evaluate a Idx Variable without unrolling");
  }

}

class RndVecConst(T = int): RndVecExpr, RndVecPrim
{
  import std.conv;

  T _value;			// the value of the constant

  override string name() {
    return _value.to!string();
  }

  public this(T value) {
    _value = value;
  }

  override public RndVecPrim[] preReqs() {
    return [];
  }

  override RndVecIterVar[] idxVars() {
    return [];
  }

  override bool isConst() {
    return true;
  }

  override public RndVecPrim[] getPrims() {
    return [];
  }

  public RndVecPrim[] getPrimLens() {
    assert(false);
  }

  // override public CstStage[] getStages() {
  //   return [];
  // }

  override public BddVec getBDD(CstStage stage, Buddy buddy) {
    return buddy.buildVec(_value);
  }

  override public long evaluate() {
    return _value;
  }

  public bool isRand() {
    return false;
  }

  public long value() {
    assert(false);
  }

  public void value(size_t v, int word = 0) {
    assert(false);
  }

  void doRandomization() {
    assert(false);
  }
  
  public CstStage stage() {
    assert(false, "no stage for RndVecConst");
  }

  public void stage(CstStage s) {
    assert(false, "no stage for RndVecConst");
  }

  public uint domIndex() {
    assert(false, "no domIndex for RndVecConst");
  }

  public void domIndex(uint s) {
    assert(false, "no domIndex for RndVecConst");
  }

  public uint bitcount() {
    assert(false, "no bitcount for RndVecConst");
  }

  public bool signed() {
    return isVarSigned!T;
  }

  public BddVec bddvec() {
    assert(false, "no bddvec for RndVecConst");
  }

  public void bddvec(BddVec b) {
    assert(false, "no bddvec for RndVecConst");
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

  override public RndVecExpr unroll(RndVecIterVar l, uint n) {
    return this;
  }
  
  void solveBefore(RndVecPrim other) {
    assert(false);
  }

  void addPreRequisite(RndVecPrim other) {
    assert(false);
  }

}

// This class would hold two(bin) vector nodes and produces a vector
// only after processing those two nodes
class RndVec2VecExpr: RndVecExpr
{
  import std.conv;

  RndVecExpr _lhs;
  RndVecExpr _rhs;
  CstBinVecOp _op;

  // RndVecPrim[] _preReqs;
  override RndVecPrim[] preReqs() {
    RndVecPrim[] reqs;
    foreach(req; _lhs.preReqs() ~ _rhs.preReqs()) {
      if(! req.solved()) {
	reqs ~= req;
      }
    }
    return reqs;
  }
  RndVecIterVar[] _idxVars;
  override RndVecIterVar[] idxVars() {
    return _idxVars;
  }

  override public string name() {
    return "( " ~ _lhs.name ~ " " ~ _op.to!string() ~ " " ~ _rhs.name ~ " )";
  }

  override public RndVecPrim[] getPrims() {
    return _lhs.getPrims() ~ _rhs.getPrims();
  }

  override public RndVecPrim[] getSolvables() {
    RndVecPrim[] solvables;
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

  override public BddVec getBDD(CstStage stage, Buddy buddy) {
    if(this.idxVars.length !is 0) {
      assert(false,
	     "RndVec2VecExpr: Need to unroll the idxVars"
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

  override public long evaluate() {
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

  override public RndVec2VecExpr unroll(RndVecIterVar l, uint n) {
    bool idx = false;
    foreach(var; idxVars()) {
      if(l is var) {
	idx = true;
	break;
      }
    }
    if(! idx) return this;
    else {
      return new RndVec2VecExpr(_lhs.unroll(l, n), _rhs.unroll(l, n), _op);
    }
  }

  public this(RndVecExpr lhs, RndVecExpr rhs, CstBinVecOp op) {
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

class RndVecSliceExpr: RndVecExpr
{
  RndVecExpr _vec;
  RndVecExpr _lhs;
  RndVecExpr _rhs;

  // RndVecPrim[] _preReqs;
  override RndVecPrim[] preReqs() {
    RndVecPrim[] reqs;
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
  
  RndVecIterVar[] _idxVars;
  override RndVecIterVar[] idxVars() {
    return _idxVars;
  }

  override public string name() {
    return _vec.name() ~ "[ " ~ _lhs.name() ~ " .. " ~ _rhs.name() ~ " ]";
  }

  override public RndVecPrim[] getPrims() {
    if(_rhs is null) {
      return _vec.getPrims() ~ _lhs.getPrims();
    }
    else {
      return _vec.getPrims() ~ _lhs.getPrims() ~ _rhs.getPrims();
    }
  }

   override public RndVecPrim[] getSolvables() {
    return _vec.getSolvables();
  }

  // override public CstStage[] getStages() {
  //   import std.exception;

  //   return _vec.getStages();
  //   // enforce(_vec.getStages.length <= 1 &&
  //   //	    _lhs.getStages.length <= 1 &&
  //   //	    _rhs.getStages.length <= 1);

  //   // if(_lhs.getStages.length is 0) return _rhs.getStages;
  //   // else if(_rhs.getStages.length is 0) return _lhs.getStages;
  //   // else {
  //   //   // import std.algorithm: max;
  //   //   // Stages need to be merged
  //   //   // uint stage = max(_lhs.getStages[0], _rhs.getStages[0]);
  //   //   // return [stage];
  //   //   return _lhs.getStages;
  //   // }
  // }

  override public BddVec getBDD(CstStage stage, Buddy buddy) {
    if(this.idxVars.length !is 0) {
      assert(false,
	     "RndVecSliceExpr: Need to unroll the idxVars"
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

  override public long evaluate() {
    // auto vec  = _vec.evaluate();
    // auto lvec = _lhs.evaluate();
    // auto rvec = _rhs.evaluate();

    assert(false, "Can not evaluate a RndVecSliceExpr!");
  }

  override public RndVecSliceExpr unroll(RndVecIterVar l, uint n) {
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
	return new RndVecSliceExpr(_vec.unroll(l, n), _lhs.unroll(l, n));
      }
      else {
	return new RndVecSliceExpr(_vec.unroll(l, n),
				   _lhs.unroll(l, n), _rhs.unroll(l, n));
      }
    }
  }

  public this(RndVecExpr vec, RndVecExpr lhs, RndVecExpr rhs=null) {
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
    // _preReqs = vec.preReqs ~ lhs.preReqs;
    // if(rhs !is null) {
    //   _preReqs ~= rhs.preReqs;
    // }
    // foreach(arrVar; preReqs) {
    //   bool add = true;
    //   foreach(l; _preReqs) {
    // 	if(l is arrVar) add = false;
    // 	break;
    //   }
    //   if(add) _preReqs ~= arrVar;
    // }
  }
}

class CstNotVecExpr: RndVecExpr
{
  override public string name() {
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
  public string name();

  // In case this expr is unRolled, the _idxVars here would be empty
  RndVecIterVar[] _idxVars;

  public RndVecIterVar[] idxVars() {
    return _idxVars;
  }

  // RndVecPrim[] _preReqs;

  abstract public RndVecPrim[] preReqs();

  // unroll recursively untill no unrolling is possible
  public CstBddExpr[] unroll() {
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

  public CstBddExpr[] unroll(RndVecIterVar l) {
    CstBddExpr[] retval;
    if(! l.isUnrollable()) {
      assert(false, "RndVecIterVar is not unrollabe yet");
    }
    auto max = l.maxVal();
    // import std.stdio;
    // writeln("maxVal is ", max);
    for (uint i = 0; i != max; ++i) {
      retval ~= this.unroll(l, i);
    }
    return retval;
  }

  public RndVecIterVar unrollableIdx() {
    foreach(idx; _idxVars) {
      if(idx.isUnrollable()) return idx;
    }
    return null;
  }

  abstract public CstBddExpr unroll(RndVecIterVar l, uint n);

  abstract public RndVecPrim[] getPrims();

  final public RndVecPrim[] getSolvables() {
    RndVecPrim[] solvables;
    foreach(prim; getPrims()) {
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

  // abstract public CstStage[] getStages();

  abstract public BDD getBDD(CstStage stage, Buddy buddy);

  public CstBdd2BddExpr opBinary(string op)(CstBddExpr other)
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

  public CstNotBddExpr opUnary(string op)()
  {
    static if(op == "*") {	// "!" in cstx is translated as "*"
      return new CstNotBddExpr(this);
    }
  }

  public CstBdd2BddExpr implies(CstBddExpr other)
  {
    return new CstBdd2BddExpr(this, other, CstBddOp.LOGICIMP);
  }

  public CstBdd2BddExpr implies(RndVecExpr other)
  {
    return new CstBdd2BddExpr(this, other.toBdd(), CstBddOp.LOGICIMP);
  }

  public CstBdd2BddExpr logicOr(CstBddExpr other)
  {
    return new CstBdd2BddExpr(this, other, CstBddOp.LOGICOR);
  }

  public CstBdd2BddExpr logicOr(RndVecExpr other)
  {
    return new CstBdd2BddExpr(this, other.toBdd(), CstBddOp.LOGICOR);
  }

  public CstBdd2BddExpr logicAnd(CstBddExpr other)
  {
    return new CstBdd2BddExpr(this, other, CstBddOp.LOGICAND);
  }

  public CstBdd2BddExpr logicAnd(RndVecExpr other)
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

  override public string name() {
    return "( " ~ _lhs.name ~ " " ~ _op.to!string ~ " " ~ _rhs.name ~ " )";
  }

  override RndVecPrim[] preReqs() {
    RndVecPrim[] reqs;
    foreach(req; _lhs.preReqs() ~ _rhs.preReqs()) {
      if(! req.solved()) {
	reqs ~= req;
      }
    }
    return reqs;
  }

  override public RndVecPrim[] getPrims() {
    return _lhs.getPrims() ~ _rhs.getPrims();
  }

  // override public CstStage[] getStages() {
  //   CstStage[] stages;

  //   foreach(lstage; _lhs.getStages) {
  //     bool already = false;
  //     foreach(stage; stages) {
  //	if(stage is lstage) {
  //	  already = true;
  //	}
  //     }
  //     if(! already) stages ~= lstage;
  //   }
  //   foreach(rstage; _rhs.getStages) {
  //     bool already = false;
  //     foreach(stage; stages) {
  //	if(stage is rstage) {
  //	  already = true;
  //	}
  //     }
  //     if(! already) stages ~= rstage;
  //   }

  //   return stages;
  // }

  override public BDD getBDD(CstStage stage, Buddy buddy) {
    if(this.idxVars.length !is 0) {
      assert(false,
	     "CstBdd2BddExpr: Need to unroll the idxVars"
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

  override public CstBdd2BddExpr unroll(RndVecIterVar l, uint n) {
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

  public this(CstBddExpr lhs, CstBddExpr rhs, CstBddOp op) {
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

    // _preReqs = lhs.preReqs ~ rhs.preReqs;
    // foreach(arrVar; lhs.preReqs ~ rhs.preReqs) {
    //   bool add = true;
    //   foreach(l; _preReqs) {
    // 	if(l is arrVar) add = false;
    // 	break;
    //   }
    //   if(add) _preReqs ~= arrVar;
    // }
  }
}

class CstIteBddExpr: CstBddExpr
{
  override public string name() {
    return "CstIteBddExpr";
  }
}

class RndVec2BddExpr: CstBddExpr
{
  import std.conv;

  RndVecExpr _lhs;
  RndVecExpr _rhs;
  CstBinBddOp _op;

  override public string name() {
    return "( " ~ _lhs.name ~ " " ~ _op.to!string ~ " " ~ _rhs.name ~ " )";
  }

  override RndVecPrim[] preReqs() {
    RndVecPrim[] reqs;
    foreach(req; _lhs.preReqs() ~ _rhs.preReqs()) {
      if(! req.solved()) {
	reqs ~= req;
      }
    }
    return reqs;
  }
    
  // override public CstStage[] getStages() {
  //   import std.exception;
  //   enforce(_lhs.getStages.length <= 1 &&
  //	    _rhs.getStages.length <= 1);

  //   if(_lhs.getStages.length is 0) return _rhs.getStages;
  //   else if(_rhs.getStages.length is 0) return _lhs.getStages;
  //   else {
  //     // import std.algorithm: max;
  //     // uint stage = max(_lhs.getStages[0], _rhs.getStages[0]);
  //     // return [stage];
  //     return _lhs.getStages;
  //   }
  // }

  override public RndVecPrim[] getPrims() {
    return _lhs.getPrims() ~ _rhs.getPrims();
  }

  override public BDD getBDD(CstStage stage, Buddy buddy) {
    if(this.idxVars.length !is 0) {
      assert(false,
	     "RndVec2BddExpr: Need to unroll the idxVars"
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

  override public RndVec2BddExpr unroll(RndVecIterVar l, uint n) {
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
      return new RndVec2BddExpr(_lhs.unroll(l, n), _rhs.unroll(l, n), _op);
    }
  }

  public this(RndVecExpr lhs, RndVecExpr rhs, CstBinBddOp op) {
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
    // _preReqs = lhs.preReqs ~ rhs.preReqs;
    // foreach(arrVar; lhs.preReqs ~ rhs.preReqs) {
    //   bool add = true;
    //   foreach(l; _preReqs) {
    // 	if(l is arrVar) add = false;
    // 	break;
    //   }
    //   if(add) _preReqs ~= arrVar;
    // }
  }
}

class CstBddConst: CstBddExpr
{
  bool _expr;

  public this(bool expr) {
    _expr = expr;
  }

  override public BDD getBDD(CstStage stage, Buddy buddy) {
    if(_expr) return buddy.one();
    else return buddy.zero();
  }

  override public string name() {
    if(_expr) return "TRUE";
    else return "FALSE";
  }

  override public RndVecPrim[] getPrims() {
    return [];
  }

  override public RndVecPrim[] preReqs() {
    return [];
  }

  override public CstBddConst unroll(RndVecIterVar l, uint n) {
    return this;
  }

}

class CstNotBddExpr: CstBddExpr
{
  CstBddExpr _expr;

  override public string name() {
    return "( " ~ "!" ~ " " ~ _expr.name ~ " )";
  }

  override RndVecPrim[] preReqs() {
    return _expr.preReqs();
  }

  override public RndVecPrim[] getPrims() {
    return _expr.getPrims();
  }

  // override public CstStage[] getStages() {
  //   return _expr.getStages();
  // }

  override public BDD getBDD(CstStage stage, Buddy buddy) {
    if(this.idxVars.length !is 0) {
      assert(false,
	     "CstBdd2BddExpr: Need to unroll the idxVars"
	     " before attempting to solve BDD");
    }
    auto bdd = _expr.getBDD(stage, buddy);
    return (~ bdd);
  }

  override public CstNotBddExpr unroll(RndVecIterVar l, uint n) {
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

  public this(CstBddExpr expr) {
    _expr = expr;
    _idxVars = expr.idxVars;
    // _preReqs = expr.preReqs;
  }
}

class CstBlock: CstBddExpr
{
  CstBddExpr[] _exprs;
  bool[] _booleans;

  override public string name() {
    string name_ = "";
    foreach(expr; _exprs) {
      name_ ~= " & " ~ expr.name() ~ "\n";
    }
    return name_;
  }

  override RndVecPrim[] preReqs() {
    assert(false);
  }
    
  public void _esdl__reset() {
    _exprs.length = 0;
  }

  override public RndVecPrim[] getPrims() {
    assert(false);
  }

  override public CstBlock unroll(RndVecIterVar l, uint n) {
    assert(false, "Can not unroll a CstBlock");
  }

  // override public CstStage[] getStages() {
  //   CstStage[] stages;

  //   foreach(expr; _exprs) {
  //     foreach(lstage; expr.getStages) {
  //	bool already = false;
  //	foreach(stage; stages) {
  //	  if(stage is lstage) {
  //	    already = true;
  //	  }
  //	}
  //	if(! already) stages ~= lstage;
  //     }
  //   }

  //   return stages;
  // }

  override public BDD getBDD(CstStage stage, Buddy buddy) {
    assert(false, "getBDD not implemented for CstBlock");
  }

  public void opOpAssign(string op)(bool other)
    if(op == "~") {
      _booleans ~= other;
    }

  public void opOpAssign(string op)(CstBddExpr other)
    if(op == "~") {
      _exprs ~= other;
    }

  public void opOpAssign(string op)(RndVecExpr other)
    if(op == "~") {
      _exprs ~= other.toBdd();
    }

  public void opOpAssign(string op)(CstBlock other)
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
