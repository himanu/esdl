// Written in the D programming language.

// Copyright: Coverify Systems Technology 2012 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

module esdl.data.rand;

import esdl.data.randcore;

template _esdl__SolverUpcast(T) {
  static if(is(T B == super)
	    && is(B[0] == class)) {
    alias U = B[0];
    // check if the base class has Randomization
    static if(__traits(compiles, U._esdl__Solver)) {
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

// generates the code for rand structure inside the class object getting
// randomized
string _esdl__randsMixin(T)() {
  T t;
  string randsMixin;

  // _esdl__initRands and _esdl__initCsts are templatized to make sure
  // that it is not overridable
  string rand_inits =
    "  public void _esdl__initRands()() {\n" ~
    _esdl__RandInits!T ~ "  }\n";
  string cst_inits =
    "  public void _esdl__initCsts()() {\n" ~
    _esdl__CstInits!T ~ "  }\n";
  string rand_decls = _esdl__RandDeclFuncs!T ~ _esdl__RandDeclVars!T;
  string rand_set_outer = "  public void _esdl__setObjOuter()() {\n" ~
    _esdl__RandSetOuter!T ~ "  }\n";
  string cst_decls = _esdl__ConstraintsDecl!T;

  randsMixin = rand_decls ~ rand_inits ~ cst_inits ~ cst_decls ~ rand_set_outer;
  return randsMixin;
}

template rand(N...) {
  static assert(false, "You need to mixin Randomization in order to use @rand");
}

template Constraint(string C) {
  static assert(false, "You need to mixin Randomization in order to define Constraints");
}

mixin template Randomization()
{
  private import esdl.data.randcore;
  enum bool _esdl__hasRandomization = true;
  alias _esdl__Type = typeof(this);

  alias _esdl__Solver = _esdl__SolverRand!_esdl__Type;

  static class _esdl__SolverRand(_esdl__T): _esdl__SolverUpcast!_esdl__T
  {
    _esdl__T _esdl__outer;

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

  public void _esdl__initSolver()() {
    if (_esdl__solverInst is null) {
      _esdl__solverInst =
	new _esdl__Solver(_esdl__randSeed, _esdl__Type.stringof, this);
    }
    else {
      _esdl__solverInst._esdl__setObjOuter();
    }
  }


  static if(__traits(compiles, _esdl__solverInst)) {
    override public void _esdl__virtualRandomize(_esdl__ConstraintBase withCst = null) {
      _esdl__randomize(this, withCst);
    }
    override public _esdl__Solver _esdl__getSolver() {
      return staticCast!_esdl__Solver(_esdl__solverInst);
    }
  }
  else {
    _esdl__Solver _esdl__solverInst;
    public uint _esdl__randSeed;
    public _esdl__Solver _esdl__getSolver() {
      return _esdl__solverInst;
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
      _esdl__solverInst._esdl__rGen.seed(seed);
    }
    alias srandom = seedRandom;	// SV names the similar method srandom
  }

  // static if(__traits(compiles,
  // 		     _esdl__upcast!(typeof(this))._esdl__isRandomizable)) {
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
      // 	new Constraint!(C, "_esdl__withCst",
      // 			T, V.length)(t, "_esdl__withCst");
      // withCst.withArgs(values);
      // t._esdl__solverInst._esdl__cstWith = withCst;
    }
    else {
      alias CST = T._esdl__Solver._esdl__Constraint!(C, V.length);
      auto cstWith = staticCast!CST(t._esdl__solverInst._esdl__cstWith);
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

/// C++ type static_cast for down-casting when you are sure
private import std.typetuple: staticIndexOf;
private import std.traits: BaseClassesTuple, ParameterTypeTuple; // required for staticIndexOf

// Coerced casting to help in efficiently downcast when we are sure
// about the given objects type.
public T staticCast(T, F)(const F from)
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
