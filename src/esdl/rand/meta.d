// Written in the D programming language.

// Copyright: Coverify Systems Technology 2012 - 2015
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

module esdl.rand.meta;

import esdl.rand.obdd;

import std.traits: isIntegral, isBoolean, isArray, isStaticArray, isDynamicArray;
import esdl.data.bvec: isBitVector;
import esdl.data.bstr;

import std.exception: enforce;
import std.range: ElementType;

import esdl.rand.misc;
import esdl.rand.expr: CstVal;
import esdl.rand.base: CstBlock, _esdl__Solver, CstVecPrim;
import esdl.rand.vecx: CstVecIdx, CstVecArrIdx;
import esdl.rand.objx: CstObjIdx, CstObjArrIdx;
import esdl.rand.solver;

/// C++ type static_cast for down-casting when you are sure
private import std.typetuple: staticIndexOf, TypeTuple;
private import std.traits: BaseClassesTuple; // required for staticIndexOf


template _esdl__IsRand(alias RAND)
{
  enum bool _esdl__IsRand = ! (__traits (isSame, RAND, _esdl__norand) ||
			       __traits (isSame, RAND, norand) ||
			       __traits (isSame, RAND, rand!false) ||
			       is (RAND == _esdl__rand!false));
}

// static alias Unconst(T) = T;
// static alias Unconst(T: const U, U) = U;

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

// The types that need a proxy for constraint to work
// This is irrespective of whether @rand attribute is present or not
// template _esdl__NeedCstProxy(T, I)
// {
//   alias L = typeof(T.tupleof[I]);
//   static if (is (getRandAttr!(T, I) == rand!false) ||
// 	     is (L: _esdl__Norand)) {
//     alias _esdl__NeedCstProxy = false;
//   }
//   else static if (is (L f == Constraint!(C, F, N),
// 		 immutable (char)[] C, immutable (char)[] F, size_t N)) {
//     alias _esdl__NeedCstProxy = false;
//   }
//   static if (isArray!L) {
//     alias _esdl__NeedCstProxy = _esdl__NeedCstProxy!(ElementType!L);
//   }
//   else {
//     static if (isIntegral!L || isBitVector!L || isBoolean!L ||
// 	       is (L == class) || is (L == struct) ||
// 	       is (L == U*, U) && is (U == struct)) {
//       alias _esdl__NeedCstProxy = true;
//     }
//     else {
//       alias _esdl__NeedCstProxy = false;
//     }
//   }
// }

template _esdl__RandProxyType(T, int I)
{
  import std.traits;
  alias L = typeof(T.tupleof[I]);
  static if (isArray!L) alias E = LeafElementType!L;
  alias RAND = getRandAttr!(T, I);
  static if(isArray!L && (isBitVector!E ||
			  isIntegral!E ||
			  isBoolean!E)) {
    alias _esdl__RandProxyType = CstVecArrIdx!(L, RAND, 0, I);
  }
  else static if(isBitVector!L || isIntegral!L || isBoolean!L) {
    alias _esdl__RandProxyType = CstVecIdx!(L, RAND, 0, I);
  }
  else static if(isArray!L && (is(E == class) || is(E == struct) ||
			       (is(E == U*, U) && is(U == struct)))) {
    alias _esdl__RandProxyType = CstObjArrIdx!(L, RAND, 0, I);
  }
  else static if(is(L == class) || is(L == struct) ||
		 (is(L == U*, U) && is(U == struct))) {
    alias _esdl__RandProxyType = CstObjIdx!(L, RAND, 0, I);
  }
}

void _esdl__doRandomizeElems(P, int I=0)(P p, _esdl__RandGen randGen) {
  static if (I == 0 &&
	     is (P B == super) &&
	     is (B[0] : _esdl__SolverRoot) &&
	     is (B[0] == class)) {
    B[0] b = p;			// super object
    _esdl__doRandomizeElems(b, randGen);
  }
  static if (I == P.tupleof.length) {
    return;
  }
  else {
    alias Q = typeof (P.tupleof[I]);
    static if (is (Q == CstVecIdx!(L, RAND, N, IDX),
		   L, alias RAND, int N, int IDX)) {
      static if (_esdl__IsRand!RAND) {
	p.tupleof[I]._esdl__doRandomize(randGen);
      }
    }
    static if (is (Q == CstVecArrIdx!(L, RAND, N, IDX),
		   L, alias RAND, int N, int IDX)) {
      static if (_esdl__IsRand!RAND) {
	p.tupleof[I]._esdl__doRandomize(randGen);
      }
    }
    static if (is (Q == CstObjIdx!(L, RAND, N, IDX),
		   L, alias RAND, int N, int IDX)) {
      static if (_esdl__IsRand!RAND) {
	p.tupleof[I]._esdl__doRandomize(randGen);
      }
    }
    static if (is (Q == CstObjArrIdx!(L, RAND, N, IDX),
		   L, alias RAND, int N, int IDX)) {
      static if (_esdl__IsRand!RAND) {
	p.tupleof[I]._esdl__doRandomize(randGen);
      }
    }
    _esdl__doRandomizeElems!(P, I+1)(p, randGen);
  }
}

void _esdl__doInitRandsElems(P, int I=0)(P p) {
  static if (I == 0 &&
	     is (P B == super) &&
	     is (B[0] : _esdl__SolverRoot) &&
	     is (B[0] == class)) {
    B[0] b = p;			// super object
    _esdl__doInitRandsElems(b);
  }
  static if (I == P.tupleof.length) {
    return;
  }
  else {
    alias Q = typeof (P.tupleof[I]);
    // pragma(msg, "#" ~ Q.stringof);
    static if (is (Q == CstVecIdx!(L, RAND, N, IDX),
		   L, alias RAND, int N, int IDX)) {
      static if (_esdl__IsRand!RAND) {
	enum NAME = __traits(identifier, p._esdl__outer.tupleof[IDX]);
	p.tupleof[I] = new Q(NAME, p._esdl__outer.tupleof[IDX], p);
	p._esdl__randsList ~= p.tupleof[I];
      }
    }
    static if (is (Q == CstObjIdx!(L, RAND, N, IDX),
		   L, alias RAND, int N, int IDX)) {
      static if (_esdl__IsRand!RAND) {
	enum NAME = __traits(identifier, p._esdl__outer.tupleof[IDX]);
	p.tupleof[I] = new Q(NAME, p._esdl__outer.tupleof[IDX], p);
	p._esdl__randsList ~= p.tupleof[I];
      }
    }
    static if (is (Q == CstVecArrIdx!(L, RAND, N, IDX),
		   L, alias RAND, int N, int IDX)) {
      static if (_esdl__IsRand!RAND) {
	enum NAME = __traits(identifier, p._esdl__outer.tupleof[IDX]);
	p.tupleof[I] = new Q(NAME, p._esdl__outer.tupleof[IDX], p);
	p._esdl__randsList ~= p.tupleof[I];
      }
    }
    static if (is (Q == CstObjArrIdx!(L, RAND, N, IDX),
		   L, alias RAND, int N, int IDX)) {
      static if (_esdl__IsRand!RAND) {
	enum NAME = __traits(identifier, p._esdl__outer.tupleof[IDX]);
	p.tupleof[I] = new Q(NAME, p._esdl__outer.tupleof[IDX], p);
	p._esdl__randsList ~= p.tupleof[I];
      }
    }
    _esdl__doInitRandsElems!(P, I+1)(p);
  }
}

void _esdl__doInitCstsElems(P, int I=0)(P p) {
  static if (I == 0 &&
	     is (P B == super) &&
	     is (B[0] : _esdl__SolverRoot) &&
	     is (B[0] == class)) {
    B[0] b = p;			// super object
    _esdl__doInitCstsElems(b);
  }
  static if (I == P.tupleof.length) {
    return;
  }
  else {
    alias Q = typeof (P.tupleof[I]);
    // pragma(msg, Q.stringof);
    static if (is (Q == Constraint!(C, F, N),
		   immutable (char)[] C, immutable (char)[] F, size_t N)) {
      p.tupleof[I] = p.new p._esdl__Constraint!(C, F, N)
	(p, p.tupleof[I].stringof, p._esdl__cstsList.length);
      p._esdl__cstsList ~= p.tupleof[I];
    }
    _esdl__doInitCstsElems!(P, I+1)(p);
  }
}

void _esdl__doSetOuterElems(P, int I=0)(P p, bool changed) {
  static if (I == 0 &&
	     is (P B == super) &&
	     is (B[0] : _esdl__SolverRoot) &&
	     is (B[0] == class)) {
    B[0] b = p;			// super object
    _esdl__doSetOuterElems(b, changed);
  }
  static if (I == P.tupleof.length) {
    return;
  }
  else {
    alias Q = typeof (P.tupleof[I]);
    static if (is (Q == CstVecIdx!(L, RAND, N, IDX),
		   L, alias RAND, int N, int IDX)) {
      static if (_esdl__IsRand!RAND) {
	p.tupleof[I]._esdl__setValRef(p._esdl__outer.tupleof[IDX]);
      }
    }
    static if (is (Q == CstObjIdx!(L, RAND, N, IDX),
		   L, alias RAND, int N, int IDX)) {
      static if (_esdl__IsRand!RAND) {
	static if (is (L == U*, U) && is(U == struct)) {
	  p.tupleof[I]._esdl__setValRef(* (p._esdl__outer.tupleof[IDX]));
	}
	else {
	  p.tupleof[I]._esdl__setValRef(p._esdl__outer.tupleof[IDX]);
	}
      }
    }
    static if (is (Q == CstVecArrIdx!(L, RAND, N, IDX),
		   L, alias RAND, int N, int IDX)) {
      static if (_esdl__IsRand!RAND) {
	p.tupleof[I]._esdl__setValRef(p._esdl__outer.tupleof[IDX]);
      }
    }
    static if (is (Q == CstObjArrIdx!(L, RAND, N, IDX),
		   L, alias RAND, int N, int IDX)) {
      static if (_esdl__IsRand!RAND) {
	p.tupleof[I]._esdl__setValRef(p._esdl__outer.tupleof[IDX]);
      }
    }
    _esdl__doSetOuterElems!(P, I+1)(p, changed);
  }
}

template _esdl__RandDeclVars(T, int I=0)
{
  static if(I == T.tupleof.length) {
    enum _esdl__RandDeclVars = "";
  }
  else static if (__traits (isSame, getRandAttr!(T, I), _esdl__norand) ||
		  __traits (isSame, getRandAttr!(T, I), rand!false) ||
		  is (getRandAttr!(T, I) == _esdl__rand!false) ||
		  is (typeof(T.tupleof[I]): _esdl__Norand)) {
    enum string _esdl__RandDeclVars = _esdl__RandDeclVars!(T, I+1);
  }
  else {
    // pragma(msg, I);
    // pragma(msg, __traits(identifier, T.tupleof[I]));
    enum string _esdl__RandDeclVars =
      "  _esdl__RandProxyType!(_esdl__T, " ~ I.stringof ~ ") _esdl__" ~
      __traits(identifier, T.tupleof[I]) ~ ";\n" ~
      _esdl__RandDeclVars!(T, I+1);
  }
}

template _esdl__RandDeclFuncs(T, int I=0)
{
  static if(I == T.tupleof.length) {
    enum _esdl__RandDeclFuncs = "";
  }
  else static if (__traits(isSame, getRandAttr!(T, I), rand!false) ||
		  is (getRandAttr!(T, I) == _esdl__rand!false)) {
    enum string _esdl__RandDeclFuncs = _esdl__RandDeclFuncs!(T, I+1);
  }
  else {
    alias L = typeof(T.tupleof[I]);
    enum NAME = __traits(identifier, T.tupleof[I]);
    // skip the constraints and _esdl__ variables
    static if(is(L f == Constraint!(C, F, N),
		 immutable (char)[] C, immutable (char)[] F, size_t N) ||
	      (NAME.length > 8 && NAME[0..7] == "_esdl__")) {
      enum _esdl__RandDeclFuncs = _esdl__RandDeclFuncs!(T, I+1);
    }
    else static if (__traits(isSame, getRandAttr!(T, I), rand!false) ||
		  is (getRandAttr!(T, I) == _esdl__rand!false)) {
      enum _esdl__RandDeclFuncs =
	_esdl__RandDeclFuncs!(T, I+1);
    }
    else static if(__traits(isSame, getRandAttr!(T, I), _esdl__norand) ||
		   is (L: _esdl__Norand)) {
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
	"  const ref auto " ~ NAME ~ "()() { return this._esdl__outer.tupleof[" ~ I.stringof ~ "]; }\n" ~
	_esdl__RandDeclFuncs!(T, I+1);
    }
    else {
      enum _esdl__RandDeclFuncs =
	"  auto " ~ NAME ~ "()() { return _esdl__" ~ NAME ~ "; }\n" ~
	_esdl__RandDeclFuncs!(T, I+1);
    }
  }
}

template _esdl__ConstraintsDefDecl(T)
{
  enum _esdl__ConstraintsDefDecl = "  Constraint!(_esdl__ConstraintDefaults!(_esdl__T, 0), \"#DEFAULT#\", 0) _esdl__defaultConstraint;\n";
}

template _esdl__ConstraintDefaults(T, int I=0)
{
  static if(I == T.tupleof.length) {
    enum _esdl__ConstraintDefaults = "";
  }
  else {
    alias randAttr = getRandAttr!(T, I);
    static if((! __traits (isSame, getRandAttr!(T, I), rand!false)) &&
	      (! is (randAttr == _esdl__rand!false)) &&
	      __traits (compiles, randAttr.RAND) &&
	      is (randAttr.RAND == _esdl__rand!A, A...)) {
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
  else static if (__traits (isSame, getRandAttr!(T, I), rand!false) ||
		  is (getRandAttr!(T, I) == _esdl__rand!false)) {
    enum string _esdl__ConstraintsDecl = _esdl__ConstraintsDecl!(T, I+1);
  }
  else {
    alias L = typeof(T.tupleof[I]);
    enum NAME = __traits(identifier, T.tupleof[I]);
    // skip the constraints and _esdl__ variables
    static if(is(L f == Constraint!(C, F, N),
		 immutable (char)[] C, immutable (char)[] F, size_t N)) {
      enum CONSTRAINT = _esdl__constraintParams!(T, I).CONSTRAINT;
      enum FILE = _esdl__constraintParams!(T, I).FILE;
      enum LINE = _esdl__constraintParams!(T, I).LINE;
      enum _esdl__ConstraintsDecl =
	"  enum string _esdl__CONSTRAINT_" ~ NAME ~
	" = _esdl__constraintParams!(_esdl__T, " ~ I.stringof ~ ").CONSTRAINT;\n" ~
	"  enum string _esdl__FILE_" ~ NAME ~
	" = _esdl__constraintParams!(_esdl__T, " ~ I.stringof ~ ").FILE;\n" ~
	"  enum size_t _esdl__LINE_" ~ NAME ~
	" = _esdl__constraintParams!(_esdl__T, " ~ I.stringof ~ ").LINE;\n" ~
	// "  CstBlock _esdl__cst_block_" ~ NAME ~ ";\n" ~
	// cast(string) constraintXlate("this", CONSTRAINT, FILE, LINE, NAME) ~
	"  Constraint!(_esdl__CONSTRAINT_" ~ NAME ~
	", _esdl__FILE_" ~ NAME ~ ", _esdl__LINE_" ~ NAME ~ ") " ~
	NAME ~ ";\n" ~ _esdl__ConstraintsDecl!(T, I+1);
    }
    else {
      enum _esdl__ConstraintsDecl = _esdl__ConstraintsDecl!(T, I+1);
    }
  }
}

template _esdl__constraintParams(T, int I)
{
  alias L = typeof(T.tupleof[I]);
  static if(is(L f == Constraint!(C, F, N),
	       immutable (char)[] C, immutable (char)[] F, size_t N)) {
    enum string CONSTRAINT = C;
    enum string FILE = F;
    enum size_t LINE = N;
  }
  else {
    static assert(false);
  }
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

  t._esdl__solverInst.solve();

  t._esdl__solverInst._esdl__doRandomize(t._esdl__solverInst._esdl__getRandGen);

  // _esdl__setRands(t, t._esdl__solverInst._esdl__randsList,
  //		  t._esdl__solverInst._esdl__rGen);

  static if(__traits(compiles, t.postRandomize())) {
    t.postRandomize();
  }
}


// generates the code for rand structure inside the class object getting
// randomized
string _esdl__randsMixin(T)() {
  T t;
  string randsMixin;

  string rand_decls = _esdl__RandDeclFuncs!T ~ _esdl__RandDeclVars!T;
  string cst_decls = _esdl__ConstraintsDefDecl!T ~ _esdl__ConstraintsDecl!T;

  randsMixin = rand_decls ~ cst_decls;

  return randsMixin;
}

class Randomizable {
  mixin Randomization;
}

mixin template Randomization()
{
  alias _esdl__T = typeof(this);
  
  // While making _esdl__SolverRand class non-static nested class
  // also works as far as dlang compilers are concerned, do not do
  // that since in that case _esdl__outer object would have an
  // implicit pointer to the outer object which can not be changed
  static class _esdl__SolverRand: _esdl__SolverBase!_esdl__T
  {
    _esdl__T _esdl__outer;
    void _esdl__setValRef()(_esdl__T outer) {
      if (_esdl__outer !is outer) {
	_esdl__outer = outer;
	this._esdl__doSetOuter(true);
      }
    }
    this(string name, _esdl__T outer,
	 _esdl__Solver parent) {
      _esdl__outer = outer;
      static if(_esdl__baseHasRandomization!_esdl__T) {
	super(name, outer, parent);
      }
      else {
	super(name, parent);
      }
      _esdl__doInitRands();
      _esdl__doInitCsts();
    }

    mixin _esdl__SolverMixin;
  }

  enum bool _esdl__hasRandomization = true;
  alias _esdl__Type = typeof(this);

  static _esdl__Type _esdl__thisHasRandomization()() {
    return null;
  }

  alias _esdl__SolverType = _esdl__SolverResolve!_esdl__Type;

  // final auto _esdl__randEval(string NAME)() {
  //   return mixin(NAME);
  // }

  static if(// is(_esdl__T: Randomizable) ||
	    __traits(compiles, _esdl__solverInst)) {
    override void _esdl__virtualRandomize(_esdl__ConstraintBase withCst = null) {
      _esdl__randomize(this, withCst);
    }
    override _esdl__SolverType _esdl__getSolver() {
      return _esdl__staticCast!_esdl__SolverType(_esdl__solverInst);
    }
    override void _esdl__initSolver() {
      if (_esdl__solverInst is null) {
	_esdl__solverInst =
	  new _esdl__SolverType(typeid(_esdl__Type).stringof[8..$-1],
				this, null);
	static if(__traits(compiles, _esdl__setupSolver())) {
	  _esdl__setupSolver();
	}
      }
      else {
	_esdl__getSolver()._esdl__doSetOuter(false);
      }
    }
  }
  else {
    @rand!false _esdl__SolverRoot _esdl__solverInst;
    _esdl__SolverType _esdl__getSolver() {
      return _esdl__staticCast!_esdl__SolverType(_esdl__solverInst);
    }
    void _esdl__virtualRandomize(_esdl__ConstraintBase withCst = null) {
      _esdl__randomize(this, withCst);
    }
    void useThisBuddy() {
      import esdl.rand.obdd;
      useBuddy(_esdl__solverInst._esdl__buddy);
    }
    void seedRandom(int seed) {
      if (_esdl__solverInst is null) {
	_esdl__initSolver();
      }
      _esdl__solverInst.seedRandom(seed);
    }
    bool _esdl__isRandSeeded() {
      if (_esdl__solverInst is null) {
	return false;
      }
      else {
	return _esdl__solverInst.isRandomSeeded;
      }
    }
    uint _esdl__getRandomSeed() {
      if (_esdl__solverInst is null) {
	return 0;
      }
      else {
	return _esdl__solverInst.getRandomSeed();
      }
    }
    alias srandom = seedRandom;	// SV names the similar method srandom
    void _esdl__initSolver() {
      if (_esdl__solverInst is null) {
	_esdl__solverInst =
	  new _esdl__SolverType(typeid(_esdl__Type).stringof[8..$-1],
				this, null);
	static if(__traits(compiles, _esdl__setupSolver())) {
	  _esdl__setupSolver();
	}
      }
      else {
	_esdl__getSolver()._esdl__doSetOuter(false);
      }
    }
  }
  // static if(_esdl__baseHasRandomization!_esdl__Type) {
  // }
}

class _esdl__SolverNoRand(_esdl__T) if (is (_esdl__T == class)):
  _esdl__SolverBase!_esdl__T
    {
      _esdl__T _esdl__outer;
      void _esdl__setValRef()(_esdl__T outer) {
	if (_esdl__outer !is outer) {
	  _esdl__outer = outer;
	  this._esdl__doSetOuter(true);
	}
      }
      this(string name, _esdl__T outer,
	   _esdl__SolverRoot parent) {
	_esdl__outer = outer;
	static if(_esdl__baseHasRandomization!_esdl__T) {
	  super(name, outer, parent);
	}
	else {
	  super(name, parent);
	}
	_esdl__doInitRands();
	_esdl__doInitCsts();
      }

      mixin _esdl__SolverMixin;
    }

class _esdl__SolverNoRand(_esdl__T) if (is (_esdl__T == struct)):
  _esdl__SolverBase!_esdl__T
    {
      _esdl__T* _esdl__outer;
      void _esdl__setValRef(ref _esdl__T outer) {
	if (_esdl__outer !is &outer) {
	  _esdl__outer = &outer;
	  this._esdl__doSetOuter(true);
	}
      }
      this(string name, ref _esdl__T outer,
	   _esdl__SolverRoot parent) {
	_esdl__outer = &outer;
	static if(_esdl__baseHasRandomization!_esdl__T) {
	  super(name, outer, parent);
	}
	else {
	  super(name, parent);
	}
	_esdl__doInitRands();
	_esdl__doInitCsts();
      }

      mixin _esdl__SolverMixin;
    }

mixin template _esdl__SolverMixin()
{
  class _esdl__Constraint(string _esdl__CstString, string FILE, size_t LINE):
    Constraint!(_esdl__CstString, FILE, LINE)
  {
    this(_esdl__SolverRoot eng, string name, size_t index) {
      super(eng, name, index);
    }
    // This mixin writes out the bdd functions after parsing the
    // constraint string at compile time
    CstBlock _esdl__cst_block;
    debug(CSTPARSER) {
      pragma(msg, "// constraintXlate! STARTS\n");
      pragma(msg, constraintXlate("this.getSolver()", _esdl__CstString, FILE, LINE));
      pragma(msg, "// constraintXlate! ENDS\n");
    }
    mixin(constraintXlate("this.getSolver()", _esdl__CstString, FILE, LINE));
  }

  class _esdl__ConstraintWith(string _esdl__CstString, string FILE, size_t LINE, ARGS...): // size_t N):
    Constraint!(_esdl__CstString, FILE, LINE)
  {
    import std.typecons;

    Tuple!(ARGS) _withArgs;

    void withArgs(ARGS...)(ARGS values) if(allIntengral!ARGS) {
      // static assert(ARGS.length == N);
      foreach(i, ref v; values) {
    	_withArgs[i] = v;
      }
    }

    this(ARGS...)(string name, ARGS args) {
      super(this.outer, name, cast(uint) this.outer._esdl__cstsList.length);
      // writeln("pointer: ", &(args[0]));
      foreach (i, arg; args) {
	_withArgs[i] = arg;
      }
    }

    ref auto _esdl__arg(size_t VAR)() {
      static assert(VAR < _withArgs.length, "Can not map specified constraint with argument: @" ~
      		    VAR.stringof);
      return _withArgs[VAR];
    }

    // This mixin writes out the bdd functions after parsing the
    // constraint string at compile time
    CstBlock _esdl__cst_block;
    mixin(constraintXlate("this.getSolver()", _esdl__CstString, FILE, LINE));
    debug(CSTPARSER) {
      pragma(msg, "// randomizeWith! STARTS\n");
      pragma(msg, constraintXlate("this.getSolver()", _esdl__CstString, FILE, LINE));
      pragma(msg, "// randomizeWith! ENDS\n");
    }
  }

  void _esdl__with(string _esdl__CstString, string FILE, size_t LINE, ARGS...)(ARGS values) {
    auto cstWith = new _esdl__ConstraintWith!(_esdl__CstString, FILE, LINE, ARGS)("randWith", values);
    // cstWith.withArgs(values);
    _esdl__cstWith = cstWith;
  }

  debug(CSTPARSER) {
    pragma(msg, "// _esdl__randsMixin!" ~ _esdl__T.stringof ~ " STARTS \n");
    pragma(msg, _esdl__randsMixin!_esdl__T);
    pragma(msg, "// _esdl__randsMixin!" ~ _esdl__T.stringof ~ " ENDS \n");
  }

  mixin(_esdl__randsMixin!_esdl__T);


  override void _esdl__doRandomize(_esdl__RandGen randGen) {
    _esdl__doRandomizeElems(this, randGen);
  }

  override void _esdl__doInitRands() {
    _esdl__doInitRandsElems(this);
  }

  override void _esdl__doSetOuter(bool changed) {
    _esdl__doSetOuterElems(this, changed);
  }

  override void _esdl__doInitCsts() {
    _esdl__doInitCstsElems(this);
  }

}

template _esdl__SolverResolve(T) {
  // static if(__traits(compiles, T._esdl__hasRandomization)) {
  static if (is(T == class)) {
    static if (__traits(compiles, T._esdl__SolverRand)) {
      alias _esdl__SolverResolve = T._esdl__SolverRand;
    }
    else {
      alias _esdl__SolverResolve = _esdl__SolverNoRand!T;
    }
  }
  else {
    alias _esdl__SolverResolve = _esdl__SolverNoRand!T;
  }
}

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
      alias _esdl__SolverBase = _esdl__SolverResolve!U;
    }
    else {
      alias _esdl__SolverBase = _esdl__SolverBase!U;
    }
  }
  else {
    alias _esdl__SolverBase = _esdl__SolverRoot;
  }
}

void randomizeWith(string C, string FILE=__FILE__, size_t LINE=__LINE__, T, ARGS...)(ref T t, ARGS values)
  if(is(T == class) && allIntengral!ARGS) {
    t._esdl__initSolver();
    // The idea is that if the end-user has used the randomization
    // mixin then _esdl__RandType would be already available as an
    // alias and we can use virtual randomize method in such an
    // eventuality.
    // static if(is(typeof(t._esdl__RandType) == T)) {
    if(t._esdl__solverInst._esdl__cstWith is null ||
       t._esdl__solverInst._esdl__cstWith._constraint != C) {
      t._esdl__getSolver()._esdl__with!(C, FILE, LINE)(values);
      t._esdl__solverInst._esdl__cstWithChanged = true;
      // auto withCst =
      //	new Constraint!(C, "_esdl__withCst",
      //			T, ARGS.length)(t, "_esdl__withCst");
      // withCst.withArgs(values);
      // t._esdl__solverInst._esdl__cstWith = withCst;
    }
    else {
      alias CONSTRAINT = _esdl__SolverResolve!T._esdl__ConstraintWith!(C, FILE, LINE, ARGS);
      auto cstWith = _esdl__staticCast!CONSTRAINT(t._esdl__solverInst._esdl__cstWith);
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
template allIntengral(ARGS...) {
  static if(ARGS.length == 0) {
    enum bool allIntengral = true;
  }
  else static if (isIntegral!(ARGS[0]) || isBitVector!(ARGS[0]) || isBoolean!(ARGS[0])) {
    enum bool allIntengral = allIntengral!(ARGS[1..$]);
  }
  else enum bool allIntengral = false;
}

// auto _esdl__rand_proxy_arr(V)(ref V v, _esdl__Solver s) if (isArray!V) {
//   alias L = LeafElementType!V;
//   static if (isBitVector!L || isIntegral!L || isBoolean!L) {
//     CstVecArr!(V, _esdl__norand, 0) vec;
//     CstVecPrim p = s.getVecPrime(cast (void*) &v);
//     if (p !is null) {
//       vec = cast(CstVecArr!(V, _esdl__norand, -1, 0)) p;
//       if (vec is null) assert (false);
//       return vec;
//     }
//     else {
//       vec = new CstVecArr!(V, _esdl__norand, -1, 0)("TMP", v, s);
//       s.addVecPrime(cast (void*) &v, vec);
//       return vec;
//     }
//   }
//   else {
//     static assert (false);
//   }
// }

auto ref _esdl__rand_proxy(L)(ref L l, string name,
			      _esdl__Solver parent) {
  import std.traits: isIntegral, isBoolean, isArray;
  static if (isIntegral!L || isBitVector!L || isBoolean!L) {
    // import std.stdio;
    // writeln("Creating VarVec, ", name);
    return new CstVecIdx!(L, _esdl__norand, 0, -1)(name, l, parent);
  }
  else static if (isArray!L) {
    // import std.stdio;
    // writeln("Creating VarVecArr, ", name);
    return new CstVecArrIdx!(L, _esdl__norand, 0, -1)(name, l, parent);
  }
  else {
    return l;
  }
}

// auto const ref _esdl__rand_proxy(L)(const ref L l, string name,
// 				    _esdl__Solver parent) {
//   import std.traits: isIntegral, isBoolean, isArray;
//   static if (isIntegral!L || isBitVector!L || isBoolean!L) {
//     // import std.stdio;
//     // writeln("Creating VarVec, ", name);
//     return new CstVecIdx!(L, _esdl__norand, 0, -1)(name, l, parent);
//   }
//   else static if (isArray!L) {
//     // alias E = LeafElementType!L;
//     // import std.stdio;
//     // writeln("Creating VarVecArr, ", name);
//     return new CstVecArrIdx!(L, _esdl__norand, 0, -1)(name, l, parent);
//   }
//   else {
//     return l;
//   }
// }


auto _esdl__rand_proxy(L)(L l, string name,
			  _esdl__Solver parent) {
  import std.traits: isIntegral, isBoolean;
  import esdl.data.bvec: isBitVector;
  static if (isIntegral!L || isBitVector!L || isBoolean!L) {
    return new CstVal!L(l); // CstVal!L.allocate(l);
  }
  else {
    return l;
  }
}
  
