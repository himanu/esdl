// Written in the D programming language.

// Copyright: Coverify Systems Technology 2012 - 2014
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

template _esdl__SolverUpcast(T) {
  static if(is(T B == super)
	    && is(B[0] == class)) {
    alias U = B[0];
    // check if the base class has Randomization
    // static if(__traits(compiles, U._esdl__Solver)) {
    static if(__traits(compiles, U._esdl__thisHasRandomization()) &&
	      is(U == typeof(U._esdl__thisHasRandomization()))) {
      alias _esdl__SolverUpcast = U._esdl__SolverRand!U;
    }
    else {
      alias _esdl__SolverUpcast = _esdl__SolverUpcast!U;
    }
  }
  else {
    alias _esdl__SolverUpcast = _esdl__SolverBase;
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
    return p < q;
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
    return p <= q;
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
    return p > q;
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
    return p >= q;
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
    return p == q;
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
    return p != q;
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
	"    _esdl__" ~ NAME ~ ".randomize();\n" ~
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
      static if(is(L == class)) {
	enum _esdl__RandInits =
	  "    assert(this._esdl__outer." ~ NAME ~
	  " !is null);\n    _esdl__" ~ NAME ~
	  " = new typeof(_esdl__" ~ NAME ~
	  ")(this._esdl__outer._esdl__randSeed, \"" ~
	  NAME ~ "\", this._esdl__outer." ~ NAME ~
	  ", this);\n    _esdl__randsList ~= _esdl__" ~ NAME ~
	  "._esdl__randsList;\n" ~ _esdl__RandInits!(T, I+1);
      }
      else {
	enum _esdl__RandInits =
	  "    _esdl__" ~ NAME ~ " = new typeof(_esdl__" ~ NAME ~
	  ")(\"" ~ NAME ~ "\", true, this);\n" ~
	  "    _esdl__randsList ~= _esdl__" ~ NAME ~ ";\n" ~
	  _esdl__RandInits!(T, I+1);
      }
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
      static if(is(L == class)) {
	enum _esdl__RandSetOuter =
	  "    assert(this._esdl__outer." ~ NAME ~
	  " !is null);\n    _esdl__" ~ NAME ~
	  "._esdl__setOuter(this._esdl__outer." ~ NAME ~
	  ");\n" ~ _esdl__RandSetOuter!(T, I+1);
      }
      else {
	enum _esdl__RandSetOuter = _esdl__RandSetOuter!(T, I+1);
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
	  enum _esdl__RandDeclFuncs =
	    "  const auto " ~ NAME ~ "() { return this._esdl__outer." ~ NAME ~ "; }\n" ~
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
    static if(is(randAttr == rand!A, A...)) {
      enum string _esdl__ConstraintDefaults =
	_esdl__ConstraintDefaults!(T.tupleof[I].stringof, 0, A) ~
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
	enum string _esdl__CstInits =
	  "    " ~ NAME ~ " = new _esdl__Constraint!(_esdl__constraintString!(_esdl__T, "
	  ~ I.stringof ~ "))(\"" ~
	  NAME ~ "\");\n    _esdl__cstsList ~= " ~ NAME ~
	  // ";\n    this._esdl__outer." ~ NAME ~ " = " ~ NAME ~
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
      // static if(hasRandAttr!(__traits(getAttributes, T.tupleof[I]))) {
      alias _esdl__ListRands = TypeTuple!(T, I, _esdl__ListRands!(T, I+1));
    }
    else {
      // alias aaaa = getRandAttr!(__traits(getAttributes, T.tupleof[I]));
      // static if(__traits(isSame, aaaa, _esdl__norand)) {
      //	pragma(msg, getRandAttr!(__traits(getAttributes, T.tupleof[I])));
      // }

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
    static assert(false, "isVarSigned: Can not determine sign of type " ~ typeid(L));
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

  t._esdl__solverInst.randomize();

  // _esdl__setRands(t, t._esdl__solverInst._esdl__randsList,
  //		  t._esdl__solverInst._esdl__rGen);

  static if(__traits(compiles, t.postRandomize())) {
    t.postRandomize();
  }
}

abstract class _esdl__ConstraintBase
{
  this(_esdl__SolverBase eng, string name, string constraint, uint index) {
    _cstEng = eng;
    _name = name;
    _constraint = constraint;
    _index = index;
  }
  immutable string _constraint;
  protected bool _enabled = true;
  protected _esdl__SolverBase _cstEng;
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
  this(_esdl__SolverBase eng, string name, uint index) {
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
      T result = uniform!(T)(_gen);
      return result;
    }
    else static if(isBitVector!T) {
	T result;
	result.randomize(_gen);
	return result;
      }
    // else static if(is(T: RandomizableIntf)) {
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
    // else static if(is(T: RandomizableIntf)) {
    //	// int seed = uniform!(int)(_gen);
    //	// t.seedRandom(seed);
    //	// t.randomize();
    //   }
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
  RndVecIndexVar[] _idxVars;
  // These are the length variables that this stage will solve
  RndVecPrim[] _arrVars;

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
}

abstract class _esdl__SolverBase {
  // Keep a list of constraints in the class
  protected _esdl__ConstraintBase[] _esdl__cstsList;
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

  _esdl__SolverBase _parent = null;

  CstBlock _esdl__cstStatements;

  this(uint seed, string name, _esdl__SolverBase parent=null) {
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

  public void randomize() {}

  public void markCstStageIdxs(CstBddExpr expr) {
    auto vecs = expr.getPrims();
    foreach(ref vec; vecs) {
      if(vec !is null) {
	auto stage = vec.stage();
	if(stage !is null) {
	  stage._idxVars ~= expr.idxVars;
	}
      }
      else {
	// FIXME -- just to check is vec can ever be null
	// If this assert statement is not hit, take out the
	// if condition
	assert(false, "Vec can be null -- kill this assert");
      }
    }
  }

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
    auto vecs = expr.getPrims();
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
    stage._arrVars ~= expr.arrVars();
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
      foreach(vec; stmt.getPrims()) {
	if(vec.domIndex == uint.max) {
	  vec.domIndex = domIndex++;
	  domList ~= vec.bitcount;
	}
      }
    }

    _esdl__buddy.clearAllDomains();
    _domains = _esdl__buddy.extDomain(domList);

  }

  void solve(T)(T t) {
    // writeln("Solving BDD for number of constraints = ", _esdl__cstsList.length);

    // if(_domains.length == 0 || _esdl__cstWithChanged is true) {
    if(_domains is null || _esdl__cstWithChanged is true) {
      initDomains(t);
    }

    CstStage[] cstStages;

    _esdl__cstStatements._esdl__reset(); // start empty
    
    // take all the constraints -- even if disabled
    foreach(ref _esdl__ConstraintBase cst; _esdl__cstsList) {
      _esdl__cstStatements ~= cst.getCstExpr();
    }

    if(_esdl__cstWith !is null) {
      _esdl__cstStatements ~= _esdl__cstWith.getCstExpr();
    }

    auto cstExprs = _esdl__cstStatements._exprs;
    auto unsolvedExprs = cstExprs;	// unstaged Expressions -- all
    auto unsolvedStages = cstStages;	// unresolved stages -- all

    // First we solve the constraint groups that are responsible for
    // setting the length of the rand!n dynamic arrays. After each
    // such constraint group is resolved, we go back and expand the
    // constraint expressions that depend on the IDX Variables.

    // Once we have unrolled all the IDXS, we go ahead and resolve
    // everything that remains.

    int stageIdx=0;

    // This variable is true when all the array lengths have been resolved
    bool allArrayLengthsResolved = false;

    // Ok before we start looking at the constraints, we create a
    // stage for each and every @rand that we have at hand
    foreach(rnd; _esdl__randsList) {
      if(rnd !is null && (! rnd.isVecArr()) &&
	 rnd.domIndex != uint.max) {
	addCstStage(rnd, unsolvedStages);
      }
      else {
      }
    }

    while(unsolvedExprs.length > 0 || unsolvedStages.length > 0) {
      cstExprs = unsolvedExprs;
      unsolvedExprs.length = 0;
      auto urExprs = unsolvedExprs;	// unrolled expressions
      cstStages = unsolvedStages;
      unsolvedStages.length = 0;

      if(! allArrayLengthsResolved) {
	allArrayLengthsResolved = true;
	foreach(expr; urExprs) {
	  if(expr._arrVars.length !is 0) {
	    allArrayLengthsResolved = false;
	  }
	}
	foreach(stage; cstStages) {
	  if(stage._arrVars.length !is 0) {
	    allArrayLengthsResolved = false;
	  }
	}
      }

      // unroll all the unrollable expressions
      foreach(expr; cstExprs) {
	if(expr.unrollable() is null) {
	  urExprs ~= expr;
	}
	else {
	  urExprs ~= expr.unroll();
	}
      }

      foreach(expr; urExprs) {
	if(expr.idxVars().length is 0) {
	  addCstStage(expr, cstStages);
	}
      }

      foreach(expr; urExprs) {
	if(expr.idxVars().length !is 0) {
	  // We want to mark the stages that are dependent on a
	  // idxVar -- so that when these idxs get resolved, we are
	  // able to factor in more constraints into these stages and
	  // then resolve
	  markCstStageIdxs(expr);
	  unsolvedExprs ~= expr;
	}
      }

      foreach(stage; cstStages) {
	if(stage !is null &&
	   stage._rndVecs.length !is 0) {
	  if(allArrayLengthsResolved) {
	    solveStage(stage, stageIdx);
	  }
	  // resolve allArrayLengthsResolved
	  else {
	    allArrayLengthsResolved = true;
	    if(stage.hasIdxs() is 0 &&
	       stage._arrVars.length !is 0) {
	      solveStage(stage, stageIdx);
	      allArrayLengthsResolved = false;
	    }
	    else {
	      unsolvedStages ~= stage;
	    }
	  }
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
      solveBDD = solveBDD & expr.getBDD(stage, _esdl__buddy);
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
      vec.value = 0;	// init
      foreach(uint i, ref j; solveBDD.getIndices(vec.domIndex)) {
	if(bits.length == 0 || bits[j] == -1) {
	  vec.value = vec.value + ((cast(ulong) _esdl__rGen.flip()) << i);
	}
	else if(bits[j] == 1) {
	  vec.value = vec.value + (1L << i);
	}
      }
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
    "  override public void randomize() {\n" ~
    "    super.randomize();\n" ~
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
  // enum bool _esdl__hasRandomization = true;
  // _esdl__SolverBase _esdl__solverInst;
  // public uint _esdl__randSeed;
  // public auto _esdl__thisHasRandomization()() {
  //   return this;
  // }
  // public _esdl__SolverBase _esdl__getSolver() {
  //   return _esdl__solverInst;
  // }
  // public void _esdl__virtualRandomize(_esdl__ConstraintBase withCst = null) {}
  // void useThisBuddy() {
  //   import esdl.data.obdd;
  //   useBuddy(_esdl__solverInst._esdl__buddy);
  // }
  // public void seedRandom(int seed) {
  //   _esdl__randSeed = seed;
  //   if(_esdl__solverInst !is null) {
  //     _esdl__solverInst._esdl__rGen.seed(seed);
  //   }
  // }
  // alias srandom = seedRandom;	// SV names the similar method srandom
  // public void _esdl__initSolver() {
  //   assert(false, "_esdl__initSolver should never be called for Randomizable object");
  // }
}

mixin template Randomization()
{
  enum bool _esdl__hasRandomization = true;
  alias _esdl__Type = typeof(this);

  static public _esdl__Type _esdl__thisHasRandomization()() {
    return null;
  }

  alias _esdl__Solver = _esdl__SolverRand!_esdl__Type;

  static class _esdl__SolverRand(_esdl__T): _esdl__SolverUpcast!_esdl__T
  {
    _esdl__T _esdl__outer;
    // alias _esdl__outer this;

    public void _esdl__setOuter()(_esdl__T outer) {
      _esdl__outer = outer;
    }

    public this(uint seed, string name, _esdl__T outer, _esdl__SolverBase parent=null) {
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

  final auto _esdl__randEval(string NAME)() {
    return mixin(NAME);
  }

  static if(// is(_esdl__T: Randomizable) ||
	    __traits(compiles, _esdl__solverInst)) {
    override public void _esdl__virtualRandomize(_esdl__ConstraintBase withCst = null) {
      _esdl__randomize(this, withCst);
    }
    override public _esdl__Solver _esdl__getSolver() {
      return _esdl__staticCast!_esdl__Solver(_esdl__solverInst);
    }
    override public void _esdl__initSolver() {
      if (_esdl__solverInst is null) {
	_esdl__solverInst =
	  new _esdl__Solver(_esdl__randSeed, typeid(_esdl__Type).stringof[8..$-1], this);
      }
      else {
	_esdl__getSolver()._esdl__setObjOuter();
      }
    }

  }
  else {
    _esdl__SolverBase _esdl__solverInst;
    public uint _esdl__randSeed;
    public _esdl__Solver _esdl__getSolver() {
      return _esdl__staticCast!_esdl__Solver(_esdl__solverInst);
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
	  new _esdl__Solver(_esdl__randSeed, typeid(_esdl__Type).stringof[8..$-1], this);
      }
      else {
	_esdl__getSolver()._esdl__setObjOuter();
      }
    }

  }

  static if(_esdl__baseHasRandomization!_esdl__Type) {
  }
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
      alias CST = T._esdl__Solver._esdl__Constraint!(C, V.length);
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
      LSH,
      RSH,
      IDXINDEX,
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

// proxy class for reading in the constraints lazily
// An abstract class that returns a vector on evaluation
abstract class RndVecExpr
{
  // alias toBdd this;

  public CstBddExpr toBdd() {
    auto zero = new RndVecConst(0, true);
    return new RndVec2BddExpr(this, zero, CstBinBddOp.NEQ);
  }

  // Array of indexes this expression has to resolve before it can be
  // convertted into an BDD
  RndVecIndexVar[] _idxVars;
  public RndVecIndexVar[] idxVars() {
    return _idxVars;
  }

  // List of Array Variables
  RndVecPrim[] _arrVars;
  public RndVecPrim[] arrVars() {
    return _arrVars;
  }

  // get all the primary bdd vectors that constitute a given bdd expression
  abstract public RndVecPrim[] getPrims();

  // get the list of stages this expression should be avaluated in
  // abstract public CstStage[] getStages();
  abstract public BddVec getBDD(CstStage stage, Buddy buddy);

  abstract public long evaluate();

  abstract public RndVecExpr unroll(RndVecIndexVar l, uint n);

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
	auto qq = new RndVecConst(q, isVarSigned!Q);
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
	auto qq = new RndVecConst(q, isVarSigned!Q);
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

  public RndVecPrim opIndex(size_t other)
  {
    assert(false, "Index operation defined only for Arrays");
  }

  public RndVecExpr opSlice(RndVecExpr lhs, RndVecExpr rhs)
  {
    return new RndVecSliceExpr(this, lhs, rhs);
  }

  public RndVecExpr opSlice(P, Q)(P p, Q q) if((isIntegral!P || isBitVector!P) &&
					       (isIntegral!Q || isBitVector!Q))
    {
      return new RndVecSliceExpr(this, new RndVecConst(p, isVarSigned!P),
				 new RndVecConst(q, isVarSigned!Q));
    }

  public RndVec2BddExpr lth(Q)(Q q) if(isBitVector!Q || isIntegral!Q) {
    auto qq = new RndVecConst(q, isVarSigned!Q);
    return this.lth(qq);
  }

  public RndVec2BddExpr lth(RndVecExpr other) {
    return new RndVec2BddExpr(this, other, CstBinBddOp.LTH);
  }

  public RndVec2BddExpr lte(Q)(Q q) if(isBitVector!Q || isIntegral!Q) {
    auto qq = new RndVecConst(q, isVarSigned!Q);
    return this.lte(qq);
  }

  public RndVec2BddExpr lte(RndVecExpr other) {
    return new RndVec2BddExpr(this, other, CstBinBddOp.LTE);
  }

  public RndVec2BddExpr gth(Q)(Q q) if(isBitVector!Q || isIntegral!Q) {
    auto qq = new RndVecConst(q, isVarSigned!Q);
    return this.gth(qq);
  }

  public RndVec2BddExpr gth(RndVecExpr other) {
    return new RndVec2BddExpr(this, other, CstBinBddOp.GTH);
  }

  public RndVec2BddExpr gte(Q)(Q q) if(isBitVector!Q || isIntegral!Q) {
    auto qq = new RndVecConst(q, isVarSigned!Q);
    return this.gte(qq);
  }

  public RndVec2BddExpr gte(RndVecExpr other) {
    return new RndVec2BddExpr(this, other, CstBinBddOp.GTE);
  }

  public RndVec2BddExpr equ(Q)(Q q) if(isBitVector!Q || isIntegral!Q) {
    auto qq = new RndVecConst(q, isVarSigned!Q);
    return this.equ(qq);
  }

  public RndVec2BddExpr equ(RndVecExpr other) {
    return new RndVec2BddExpr(this, other, CstBinBddOp.EQU);
  }

  public RndVec2BddExpr neq(Q)(Q q) if(isBitVector!Q || isIntegral!Q) {
    auto qq = new RndVecConst(q, isVarSigned!Q);
    return this.neq(qq);
  }

  public RndVec2BddExpr neq(RndVecExpr other) {
    return new RndVec2BddExpr(this, other, CstBinBddOp.NEQ);
  }

  public CstNotBddExpr opUnary(string op)()
  {
    static if(op == "*") {	// "!" in cstx is translated as "*"
      return new CstNotBddExpr(this.toBdd());
    }
  }

  public CstBdd2BddExpr implies(CstBddExpr other)
  {
    return new CstBdd2BddExpr(this.toBdd(), other, CstBddOp.LOGICIMP);
  }

  // public CstBdd2BddExpr implies(RndVecExpr other)
  // {
  //   return new CstBdd2BddExpr(this.toBdd(), other.toBdd(), CstBddOp.LOGICIMP);
  // }

  public CstBdd2BddExpr logicOr(CstBddExpr other)
  {
    return new CstBdd2BddExpr(this.toBdd(), other, CstBddOp.LOGICOR);
  }

  // public CstBdd2BddExpr logicOr(RndVecExpr other)
  // {
  //   return new CstBdd2BddExpr(this.toBdd(), other.toBdd(), CstBddOp.LOGICOR);
  // }

  public CstBdd2BddExpr logicAnd(CstBddExpr other)
  {
    return new CstBdd2BddExpr(this.toBdd(), other, CstBddOp.LOGICAND);
  }

  // public CstBdd2BddExpr logicAnd(RndVecExpr other)
  // {
  //   return new CstBdd2BddExpr(this.toBdd(), other.toBdd(), CstBddOp.LOGICAND);
  // }

  public string name();
}

abstract class RndVecPrim: RndVecExpr
{
  string _name;
  override string name() {
    return _name;
  }

  public this(string name) {
    _name = name;
  }

  abstract void randomize();
  abstract public _esdl__SolverBase getSolver();
  abstract public bool isRand();
  abstract public long value();
  abstract public void value(long v);
  abstract public CstStage stage();
  abstract public void stage(CstStage s);
  public void _esdl__reset() {
    stage = null;
  }
  public bool isVecArr() {
    return false;
  }
  abstract public uint domIndex();
  abstract public void domIndex(uint s);
  abstract public uint bitcount();
  abstract public bool signed();
  abstract public BddVec bddvec();
  abstract public void bddvec(BddVec b);

  // this method is used for getting implicit constraints that are required for
  // dynamic arrays and for enums
  public BDD getPrimBdd(Buddy buddy) {
    return buddy.one();
  }
  
  override public RndVecPrim unroll(RndVecIndexVar l, uint n) {
    return this;
  }
}

class RndVecArrLen(RV): RndVecPrim
{

  // This bdd has the constraint on the max length of the array
  BDD _primBdd;
  
  size_t _maxArrLen;
  RndVecIndex!RV _idxVar;

  RV _parent;

  BddVec _bddvec;
  uint _domIndex = uint.max;
  CstStage _stage = null;
  bool _isRand;

  override public _esdl__SolverBase getSolver() {
    if(_parent is null) {
      assert(false, "No parent associated with RndVecArrLen");
    }
    return _parent.getSolver();
  }

  override public RndVecPrim[] getPrims() {
    RndVecPrim[] _prims;
    if(isRand) _prims = [this];
    return _prims;
  }

  // override public CstStage[] getStages() {
  //   CstStage[] stages;
  //   if(isRand) stages = [this.stage()];
  //   return stages;
  // }

  override public BddVec getBDD(CstStage stage, Buddy buddy) {
    if(this.isRand && stage is _stage) {
      return _bddvec;
    }
    else if((! this.isRand) ||
	    this.isRand && _stage.solved()) { // work with the value
      return buddy.buildVec(value());
    }
    else {
      assert(false, "Constraint evaluation in wrong stage");
    }
  }

  override public long evaluate() {
    if(! this.isRand || _stage.solved()) {
      return value();
    }
    else {
      import std.conv;
      assert(false, "Rand variable " ~ _name ~ " evaluation in wrong stage: " ~
	     _stage._id.to!string);
    }
  }

  override void randomize() {
    assert(false);
  }
  
  override public bool isRand() {
    return _isRand;
  }

  override public CstStage stage() {
    return _stage;
  }

  override public void stage(CstStage s) {
    _stage = s;
  }

  override public uint domIndex() {
    return _domIndex;
  }

  override public void domIndex(uint s) {
    _domIndex = s;
  }

  override public BddVec bddvec() {
    return _bddvec;
  }

  override public void bddvec(BddVec b) {
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

  public this(string name, long maxArrLen, bool isRand, RV parent) {
    super(name);
    _isRand = isRand;
    _maxArrLen = maxArrLen;
    _isRand = isRand;
    _parent = parent;
  }

  override public BDD getPrimBdd(Buddy buddy) {
    if(_primBdd.isZero()) {
      _primBdd = this.bddvec.lte(buddy.buildVec(_maxArrLen));
    }
    return _primBdd;
  }
  
  public void idxVar(RndVecIndex!RV var) {
    _idxVar = var;
  }

  public RndVecIndex!RV idxVar() {
    return _idxVar;
  }

  public RndVecIndex!RV makeIdxVar() {
    if(_idxVar is null) {
      _idxVar = new RndVecIndex!RV(_parent);
    }
    return _idxVar;
  }

  override uint bitcount() {
    return 32;
  }

  override bool signed() {
    return false;
  }

  override public long value() {
    return _parent.getLen();
  }

  override public void value(long v) {
    _parent.setLen(v);
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

template _esdl__Rand(T, string S, int I=0)
{
  static if(I == T.tupleof.length) {
    static assert(false, "There is no member named " ~ S ~ " in class " ~ T.stringof);
  }
  enum NAME = __traits(identifier, T.tupleof[I]);
  static if(NAME == S) {
    alias _esdl__Rand = _esdl__Rand!(T, I);
  }
  else {
    alias _esdl__Rand = _esdl__Rand!(T, S, I+I);
  }
}

template _esdl__Rand(T, int I)
{
  import std.traits;
  alias L = typeof(T.tupleof[I]);
  static if(isArray!L) {
    alias _esdl__Rand = RndVec!(T, I);
  }
  else static if(isBitVector!L || isIntegral!L) {
      alias _esdl__Rand = RndVec!(T, I);
    }
  else static if(is(L == class)) {
      alias _esdl__Rand = L._esdl__Solver;
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
class RndVec(T, int I, int N=0): RndVecPrim
{
  import std.traits;
  import std.range;
  import esdl.data.bvec;
  alias L = typeof(T.tupleof[I]);
  alias R = getRandAttr!(T, I);
  alias E = ElementTypeN!(L, N);

  static if(_esdl__ArrOrder!(T, I, N) == 0) {
    mixin EnumConstraints!E;

    BddVec _bddvec;
    uint _domIndex = uint.max;
    CstStage _stage = null;
    bool _isRand;

    override public RndVecPrim[] getPrims() {
      RndVecPrim[] _prims;
      if(isRand) _prims = [this];
      return _prims;
    }

    override public BddVec getBDD(CstStage stage, Buddy buddy) {
      if(this.isRand && stage is _stage) {
	return _bddvec;
      }
      else if((! this.isRand) ||
	      this.isRand && _stage.solved()) { // work with the value
	return buddy.buildVec(value());
      }
      else {
	assert(false, "Constraint evaluation in wrong stage");
      }
    }

    override public long evaluate() {
      if(! this.isRand || _stage.solved()) {
	return value();
      }
      else {
	import std.conv;
	assert(false, "Rand variable " ~ _name ~
	       " evaluation in wrong stage: " ~ _stage._id.to!string);
      }
    }

    override public bool isRand() {
      return _isRand;
    }

    override public CstStage stage() {
      return _stage;
    }

    override public void stage(CstStage s) {
      _stage = s;
    }

    override public uint domIndex() {
      return _domIndex;
    }

    override public void domIndex(uint s) {
      _domIndex = s;
    }

    override public BddVec bddvec() {
      return _bddvec;
    }

    override public void bddvec(BddVec b) {
      _bddvec = b;
    }

    override uint bitcount() {
      static if(isIntegral!E)        return E.sizeof * 8;
      else static if(isBitVector!E)  return E.SIZE;
	else static assert(false, "bitcount can not operate on: " ~ E.stringof);
    }

    override bool signed() {
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


    static if(N == 0) {
      T._esdl__Solver _solver;

      public this(string name, bool isRand, T._esdl__Solver solver) {
	super(name);
	_isRand = isRand;
	_solver = solver;
      }

      override public void randomize() {
	if(stage is null) {
	  _solver._esdl__getRandGen().gen(_solver._esdl__outer.tupleof[I]);
	}
      }

      override public _esdl__SolverBase getSolver() {
	return _solver;
      }

      override public long value() {
	return cast(long) (_solver._esdl__outer.tupleof[I]);
      }

      override public void value(long v) {
	_solver._esdl__outer.tupleof[I] = cast(L) toBitVec(v);
      }
    }

    else {			// if (N != 0)
      alias P = RndVec!(T, I, N-1);
      P _parent;
      ulong _index;

      public this(string name, bool isRand, P parent,
		  ulong index) {
	super(name);
	_isRand = isRand;
	_parent = parent;
	_index = index;
      }

      override public void randomize() {
	if(stage is null) {
	  E val;
	  getSolver()._esdl__getRandGen().gen(val);
	  value(val);
	}
      }

      override public _esdl__SolverBase getSolver() {
	if(_parent is null) {
	  assert(false, "No parent associated with RndVec");
	}
	return _parent.getSolver();
      }

      override long value() {
	return _parent.getVal(_index);
      }

      override void value(long v) {
	_parent.setVal(v, _index);
      }
    }

  }
  else
  {
    alias EE = ElementTypeN!(L, N+1);

    alias RV = typeof(this);

    RndVecArrLen!RV _arrLen;

    RndVec!(T, I, N+1)[] _elems;
    bool _elemIsRand;

    void opIndexAssign(RndVec!(T, I, N+1) c, size_t idx) {
      _elems[idx] = c;
    }

    public RndVecArrLen!RV length() {
      return _arrLen;
    }

    public RndVecArrLen!RV arrLen() {
      return _arrLen;
    }

    override public string name() {
      return _name;
    }

    override public bool isVecArr() {
      return true;
    }

    override public RndVecPrim[] getPrims() {
      RndVecPrim[] result;
      foreach(elem; _elems) {
	result ~= elem;
      }
      return result;
    }

    override public void randomize() {
      if(_elems.length == 0) this.build();
      assert(arrLen !is null);
      for(size_t i=0; i != arrLen.evaluate(); ++i) {
	this[i].randomize();
      }
    }

    override public RndVec2VecExpr opIndex(RndVecExpr idx) {
      return new RndVec2VecExpr(this, idx, CstBinVecOp.IDXINDEX);
    }

    override public RndVecPrim opIndex(size_t idx) {
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
      return _elems.length != 0;
    }
    
    void build() {
      _elems.length = maxArrLen();
      static if(isIntegral!EE || isBitVector!EE) {
	// if(! built()) {
	for (size_t i=0; i!=maxArrLen; ++i) {
	  if(this[i] is null) {
	    import std.conv: to;
	    this[i] = new RndVec!(T, I, N+1)(_name ~ "[" ~ i.to!string() ~ "]",
					     true, this, i);
	  }
	}
	// }
      }
      else static if(isDynamicArray!EE) {
	  // if(! built()) {
	  for (size_t i=0; i!=maxArrLen; ++i) {
	    if(this[i] is null) {
	      import std.conv: to;
	      this[i] = new RndVec!(T, I, N+1)(_name ~ "[" ~ i.to!string() ~ "]",
					       4, true, true, this, i);
	    }
	  }
	  // }
	}
      else static if(isStaticArray!EE) {
	  // if(! built()) {
	  for (size_t i=0; i!=maxArrLen; ++i) {
	    if(this[i] is null) {
	      import std.conv: to;
	      this[i] = new RndVec!(T, I, N+1)(_name ~ "[" ~ i.to!string() ~ "]",
					       E.length, false, true, this, i);
	    }
	  }
	  // }
	}
    }
	
    override public void _esdl__reset() {
      _arrLen.stage = null;
      foreach(elem; _elems) {
	if(elem !is null) {
	  elem._esdl__reset();
	}
      }
    }

    override public RndVecPrim[] arrVars() {
      if(_arrLen.isRand()) return [this];
      else return [];
    }

    size_t maxArrLen() {
      return _arrLen._maxArrLen;
    }
      
    static private long getLen_(A, N...)(ref A arr, N idx)
      if(isArray!A) {
	static if(N.length == 0) return arr.length;
	else {
	  return getLen_(arr[idx[0]], idx[1..$]);
	}
      }

    static private void setLen_(A, N...)(ref A arr, long v, N idx)
      if(isArray!A) {
	static if(N.length == 0) {
	  static if(isDynamicArray!A) {
	    arr.length = v;
	  }
	  else {
	    assert(false, "Can not set length of a fixed length array");
	  }
	}
	else {
	  setLen_(arr[idx[0]], v, idx[1..$]);
	}
      }

    static private long getVal(A, N...)(ref A arr, N idx)
      if(isArray!A && N.length > 0) {
	static if(N.length == 1) return arr[idx[0]];
	else {
	  return getVal(arr[idx[0]], idx[1..$]);
	}
      }

    static private void setVal(A, N...)(ref A arr, long v, N idx)
      if(isArray!A && N.length > 0) {
	static if(N.length == 1) {
	  alias E = ElementType!A;
	  arr[idx[0]] = cast(E) v;
	}
	else {
	  setVal(arr[idx[0]], v, idx[1..$]);
	}
      }

    public long getLen_(N...)(N idx) {
      return getLen_(_solver._esdl__outer.tupleof[I], idx);
    }

    public void setLen_(N...)(long v, N idx) {
      setLen_(_solver._esdl__outer.tupleof[I], v, idx);
    }

    public long getLen() {
      return getLen_(_solver._esdl__outer.tupleof[I]);
    }

    public void setLen(long v) {
      setLen_(_solver._esdl__outer.tupleof[I], v);
    }

    override public BddVec getBDD(CstStage stage, Buddy buddy) {
      assert(false, "getBDD not implemented for RndVecArrVar");
    }

    override public long evaluate() {
      assert(false, "evaluate not implemented for RndVecArrVar");
    }

    override public bool isRand() {
      assert(false, "isRand not implemented for RndVecArrVar");
    }

    override public long value() {
      assert(false, "value not implemented for RndVecArrVar");
    }

    override public void value(long v) {
      assert(false, "value not implemented for RndVecArrVar");
    }

    override public CstStage stage() {
      assert(false, "stage not implemented for RndVecArrVar");
    }

    override public void stage(CstStage s) {
      assert(false, "stage not implemented for RndVecArrVar");
    }

    override public uint domIndex() {
      assert(false, "domIndex not implemented for RndVecArrVar");
    }

    override public void domIndex(uint s) {
      assert(false, "domIndex not implemented for RndVecArrVar");
    }

    override public uint bitcount() {
      assert(false, "bitcount not implemented for RndVecArrVar");
    }

    override public bool signed() {
      assert(false, "signed not implemented for RndVecArrVar");
    }

    override public BddVec bddvec() {
      assert(false, "bddvec not implemented for RndVecArrVar");
    }

    override public void bddvec(BddVec b) {
      assert(false, "bddvec not implemented for RndVecArrVar");
    }

    static if(N == 0) {
      T._esdl__Solver _solver;

      static if(__traits(isSame, R, rand)) {
	static assert(isStaticArray!L);
	public this(string name, bool elemIsRand, T._esdl__Solver solver) {
	  size_t maxLen = L.length;
	  super(name);
	  _elemIsRand = elemIsRand;
	  _solver = solver;
	  _arrLen = new RndVecArrLen!RV(name, maxLen, false, this);
	}
      }

      static if(is(R: rand!M, M...)) {
	enum int maxLen = M[0];
	public this(string name, bool elemIsRand, T._esdl__Solver solver) {
	  super(name);
	  _elemIsRand = elemIsRand;
	  _solver = solver;
	  _arrLen = new RndVecArrLen!RV(name, maxLen, true, this);

	}
      }

      override public _esdl__SolverBase getSolver() {
	return _solver;
      }


      public long getVal(J...)(J idx) {
	return getVal(_solver._esdl__outer.tupleof[I], idx);
      }

      public void setVal(J...)(long v, J idx) {
	setVal(_solver._esdl__outer.tupleof[I], v, idx);
      }
    }
    else {
      alias P = RndVec!(T, I, N-1);
      P _parent;
      ulong _index;

      public this(string name, long maxArrLen, bool isRand, bool elemIsRand,
		  P parent, ulong index) {
	super(name);
	_parent = parent;
	_index = index;
	_arrLen = new RndVecArrLen!RV(name, maxArrLen, isRand, this);
      }

      override public _esdl__SolverBase getSolver() {
	if(_parent is null) {
	  assert(false, "No parent associated with RndVecArr");
	}
	return _parent.getSolver();
      }

      public long getLen(N...)(N idx) {
	return _parent.getLen(_index, idx);
      }

      public void setLen(N...)(long v, N idx) {
	_parent.setLen(v, _index, idx);
      }

      public long getVal(N...)(N idx) {
	return _parent.getVal(_index, idx);
      }

      public void setVal(N...)(long v, N idx) {
	_parent.setVal(v, _index, idx);
      }
    }
  }
};

class RndVecIndex(RV): RndVecIndexVar
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

  override public _esdl__SolverBase getSolver() {
    if(_arrVar is null) {
      assert(false, "No arrVar associated RndVecIndexVar");
    }
    return _arrVar.getSolver();
  }

  override uint maxVal() {
    if(! this.isUnrollable()) {
      assert(false, "Can not find maxVal since the "
	     "Idx Variable is unrollable");
    }
    return cast(uint) _arrVar.arrLen.value;
  }

  override bool isUnrollable(RndVecPrim arrVar) {
    if(arrVar is _arrVar) {
      return true;
    }
    else {
      return false;
    }
  }

  override bool isUnrollable() {
    if(! _arrVar.arrLen.isRand()) return true;
    if(_arrVar.arrLen.stage !is null &&
       _arrVar.arrLen.stage.solved()) return true;
    else return false;
  }

  // get all the primary bdd vectors that constitute a given bdd expression
  override public RndVecPrim[] getPrims() {
    return _arrVar.arrLen.getPrims();
  }

  override public bool isRand() {
    return _arrVar.arrLen.isRand();
  }
  override public long value() {
    return _arrVar.arrLen.value();
  }
  override public void value(long v) {
    _arrVar.arrLen.value(v);
  }
  override void randomize() {
    assert(false);
  }
  override public CstStage stage() {
    return _arrVar.arrLen.stage();
  }
  override public void stage(CstStage s) {
    _arrVar.arrLen.stage(s);
  }
  override public uint domIndex() {
    return _arrVar.arrLen.domIndex;
  }
  override public void domIndex(uint s) {
    _arrVar.arrLen.domIndex(s);
  }
  override public uint bitcount() {
    return _arrVar.arrLen.bitcount();
  }
  override public bool signed() {
    return _arrVar.arrLen.signed();
  }
  override public BddVec bddvec() {
    return _arrVar.arrLen.bddvec();
  }
  override public void bddvec(BddVec b) {
    _arrVar.bddvec(b);
  }
  override public string name() {
    return _arrVar.arrLen.name();
  }
  override public RndVecPrim unroll(RndVecIndexVar l, uint n) {
    if(this !is l) return this;
    else return new RndVecConst(n, false);
  }
}
				    
// This class represents an unrolled Foreach idx at vec level
abstract class RndVecIndexVar: RndVecPrim
{
  this(string name) {
    super(name);
  }

  // _idxVar will point to the array this RndVecIndexVar is tied to

  uint maxVal();

  override RndVecIndexVar[] idxVars() {
    return [this];
  }

  // this will not return the arrVar since the length variable is
  // not getting constrained here
  override RndVecPrim[] arrVars() {
    return [];
  }

  bool isUnrollable(RndVecPrim arrVar);

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

class RndVecConst: RndVecPrim
{
  import std.conv;

  long _value;			// the value of the constant
  bool _signed;

  public this(long value, bool signed) {
    super(value.to!string());
    _value = value;
    _signed = signed;
  }

  override public _esdl__SolverBase getSolver() {
    assert(false, "No Solver associated with RndVecConst");
  }

  override public RndVecPrim[] getPrims() {
    return [];
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

  override public bool isRand() {
    return false;
  }

  override public long value() {
    return _value;
  }

  override public void value(long v) {
    _value = value;
  }

  override void randomize() {
    assert(false);
  }
  
  override public CstStage stage() {
    assert(false, "no stage for RndVecConst");
  }

  override public void stage(CstStage s) {
    assert(false, "no stage for RndVecConst");
  }

  override public uint domIndex() {
    assert(false, "no domIndex for RndVecConst");
  }

  override public void domIndex(uint s) {
    assert(false, "no domIndex for RndVecConst");
  }

  override public uint bitcount() {
    assert(false, "no bitcount for RndVecConst");
  }

  override public bool signed() {
    return _signed;
  }

  override public BddVec bddvec() {
    assert(false, "no bddvec for RndVecConst");
  }

  override public void bddvec(BddVec b) {
    assert(false, "no bddvec for RndVecConst");
  }

  override public string name() {
    return _name;
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

  override public string name() {
    return "( " ~ _lhs.name ~ " " ~ _op.to!string() ~ " )";
  }

  override public RndVecPrim[] getPrims() {
    if(_op !is CstBinVecOp.IDXINDEX) {
      return _lhs.getPrims() ~ _rhs.getPrims();
    }
    else {
      // IDX
      // first make sure that the _lhs is an array
      auto lhs = cast(RndVecPrim) _lhs;
      // FIXME -- what if the IDXINDEX is used with non-rand array?
      assert(lhs !is null && lhs.isVecArr(), "IDXINDEX can not work with non-arrays");
      if(_rhs.idxVars.length is 0) {
	return [_lhs[_rhs.evaluate()]];
      }
      else {
	return _lhs.getPrims();
      }
    }
  }

  // override public CstStage[] getStages() {
  //   import std.exception;

  //   enforce(_lhs.getStages.length <= 1 &&
  //	    _rhs.getStages.length <= 1);

  //   if(_lhs.getStages.length is 0) return _rhs.getStages;
  //   else if(_rhs.getStages.length is 0) return _lhs.getStages;
  //   else {
  //     // import std.algorithm: max;
  //     // Stages need to be merged
  //     // uint stage = max(_lhs.getStages[0], _rhs.getStages[0]);
  //     // return [stage];
  //     return _lhs.getStages;
  //   }
  // }

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
    case CstBinVecOp.MUL: return _lhs.getBDD(stage, buddy) *
	_rhs.getBDD(stage, buddy);
    case CstBinVecOp.DIV: return _lhs.getBDD(stage, buddy) /
	_rhs.getBDD(stage, buddy);
    case CstBinVecOp.LSH: return _lhs.getBDD(stage, buddy) <<
	_rhs.getBDD(stage, buddy);
    case CstBinVecOp.RSH: return _lhs.getBDD(stage, buddy) >>
	_rhs.getBDD(stage, buddy);
    case CstBinVecOp.IDXINDEX:
      return _lhs[_rhs.evaluate()].getBDD(stage, buddy);
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
    case CstBinVecOp.LSH: return lvec << rvec;
    case CstBinVecOp.RSH: return lvec >> rvec;
    case CstBinVecOp.IDXINDEX: return _lhs[rvec].evaluate();
    case CstBinVecOp.BITINDEX:
      assert(false, "BITINDEX is not implemented yet!");
    }
  }

  override public RndVec2VecExpr unroll(RndVecIndexVar l, uint n) {
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
    foreach(var; lhs.idxVars ~ rhs.idxVars) {
      bool add = true;
      foreach(l; _idxVars) {
	if(l is var) add = false;
	break;
      }
      if(add) _idxVars ~= var;
    }
    foreach(arrVar; lhs.arrVars ~ rhs.arrVars) {
      if(op !is CstBinVecOp.IDXINDEX) {
	bool add = true;
	foreach(l; _arrVars) {
	  if(l is arrVar) add = false;
	  break;
	}
	if(add) _arrVars ~= arrVar;
      }
    }
  }

}

class RndVecSliceExpr: RndVecExpr
{
  RndVecExpr _vec;
  RndVecExpr _lhs;
  RndVecExpr _rhs;

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
    auto lvec = _lhs.evaluate();
    auto rvec = lvec;
    if(_rhs is null) {
      rvec = lvec + 1;
    }
    else {
      rvec = _rhs.evaluate();
    }
    return vec[lvec..rvec];
  }

  override public long evaluate() {
    // auto vec  = _vec.evaluate();
    // auto lvec = _lhs.evaluate();
    // auto rvec = _rhs.evaluate();

    assert(false, "Can not evaluate a RndVecSliceExpr!");
  }

  override public RndVecSliceExpr unroll(RndVecIndexVar l, uint n) {
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
    auto arrVars = vec.arrVars ~ lhs.arrVars;
    if(rhs !is null) {
      arrVars ~= rhs.arrVars;
    }
    foreach(arrVar; arrVars) {
      bool add = true;
      foreach(l; _arrVars) {
	if(l is arrVar) add = false;
	break;
      }
      if(add) _arrVars ~= arrVar;
    }
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
  RndVecIndexVar[] _idxVars;

  public RndVecIndexVar[] idxVars() {
    return _idxVars;
  }

  RndVecPrim[] _arrVars;

  public RndVecPrim[] arrVars() {
    return _arrVars;
  }

  // unroll recursively untill no unrolling is possible
  public CstBddExpr[] unroll() {
    CstBddExpr[] retval;
    auto idx = this.unrollable();
    if(idx is null) {
      return [this];
    }
    else {
      foreach(expr; this.unroll(idx)) {
	if(expr.unrollable() is null) retval ~= expr;
	else retval ~= expr.unroll();
      }
    }
    return retval;
  }

  public CstBddExpr[] unroll(RndVecIndexVar l) {
    CstBddExpr[] retval;
    if(! l.isUnrollable()) {
      assert(false, "RndVecIndexVar is not unrollabe yet");
    }
    auto max = l.maxVal();
    for (uint i = 0; i != max; ++i) {
      retval ~= this.unroll(l, i);
    }
    return retval;
  }

  public RndVecIndexVar unrollable() {
    foreach(idx; _idxVars) {
      if(idx.isUnrollable()) return idx;
    }
    return null;
  }

  abstract public CstBddExpr unroll(RndVecIndexVar l, uint n);

  abstract public RndVecPrim[] getPrims();

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

  override public CstBdd2BddExpr unroll(RndVecIndexVar l, uint n) {
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
    foreach(arrVar; lhs.arrVars ~ rhs.arrVars) {
      bool add = true;
      foreach(l; _arrVars) {
	if(l is arrVar) add = false;
	break;
      }
      if(add) _arrVars ~= arrVar;
    }
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

  override public RndVec2BddExpr unroll(RndVecIndexVar l, uint n) {
    bool idx = false;
    foreach(var; idxVars()) {
      if(l is var) {
	idx = true;
	break;
      }
    }
    if(! idx) return this;
    else {
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
    foreach(arrVar; lhs.arrVars ~ rhs.arrVars) {
      bool add = true;
      foreach(l; _arrVars) {
	if(l is arrVar) add = false;
	break;
      }
      if(add) _arrVars ~= arrVar;
    }
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

  override public CstBddConst unroll(RndVecIndexVar l, uint n) {
    return this;
  }

}

class CstNotBddExpr: CstBddExpr
{
  CstBddExpr _expr;

  override public string name() {
    return "( " ~ "!" ~ " " ~ _expr.name ~ " )";
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

  override public CstNotBddExpr unroll(RndVecIndexVar l, uint n) {
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
    _arrVars = expr.arrVars;
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

  public void _esdl__reset() {
    _exprs.length = 0;
  }

  override public RndVecPrim[] getPrims() {
    RndVecPrim[] prims;

    foreach(expr; _exprs) {
      prims ~= expr.getPrims();
    }

    return prims;
  }

  override public CstBlock unroll(RndVecIndexVar l, uint n) {
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
