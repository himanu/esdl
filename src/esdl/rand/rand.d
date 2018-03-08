// Written in the D programming language.

// Copyright: Coverify Systems Technology 2012 - 2015
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

module esdl.rand.rand;

import esdl.rand.obdd;

import std.traits: isIntegral, isBoolean, isArray, isStaticArray, isDynamicArray;
import esdl.data.bvec: isBitVector;
import esdl.data.bstr;

import std.exception: enforce;
import std.range: ElementType;

import esdl.rand.base;
import esdl.rand.expr: CstVarExpr;
import esdl.rand.solver;

/// C++ type static_cast for down-casting when you are sure
private import std.typetuple: staticIndexOf, TypeTuple;
private import std.traits: BaseClassesTuple; // required for staticIndexOf

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
	"    _esdl__" ~ NAME ~ ".doRandomize(randGen);\n" ~
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
	  "    assert(this._esdl__outer." ~ NAME ~
	  " !is null);\n    _esdl__" ~ NAME ~
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

  t._esdl__solverInst.doRandomize(t._esdl__solverInst._esdl__getRandGen);

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

  // _esdl__initRands and _esdl__initCsts are templatized to make sure
  // that it is not overridable
  string rand_randrand =
    "  override void doRandomize(_esdl__RandGen randGen) {\n" ~
    "    super.doRandomize(randGen);\n" ~
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
	  new _esdl__SolverType(_esdl__randSeed, _esdl__randSeeded,
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
	  new _esdl__SolverType(_esdl__randSeed, _esdl__randSeeded,
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
      alias CST = _esdl__SolverResolve!T._esdl__Constraint!(C, ARG.length);
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

