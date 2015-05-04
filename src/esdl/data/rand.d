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
      alias _esdl__SolverUpcast = _esdl__SolverBase;
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
  // alias RANDS = _esdl__ListRands!(T);
  // alias CONSTRAINTS = _esdl__ListConstraints!(T);
  string randsMixin;
  //   string rand_header = "
  // class _esdl__Solver: _esdl__SolverUpcast!(typeof(this))" ~
  //     " {\n  alias _esdl__T=typeof(this.outer);
  //   public this(uint seed) {\n    super(seed);\n  }\n";

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
  // string rand_trailer = "}\n";
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

    public void _esdl__setOuter()(_esdl__T _outer) {
      _esdl__outer = _outer;
      // assert(_esdl__outer is _outer,
      // 	     "Dynamically changing @rand objects not yet supported: " ~
      // 	     _esdl__T.stringof);
    }
    
    public this(uint seed, string name, _esdl__T _outer, _esdl__SolverBase parent=null) {
      _esdl__outer = _outer;
      static if(_esdl__T._esdl__baseHasRandomization) {
	super(seed, name, _outer, parent);
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
      mixin(constraintFunc(_esdl__CstString));
      debug(CONSTRAINTS) {
	pragma(msg, constraintFunc(_esdl__CstString));
      }
    }
    
    mixin(_esdl__randsMixin!_esdl__T);

    debug(CONSTRAINTS) {
      pragma(msg, _esdl__randsMixin!_esdl__T);
    }
  }

  final auto _esdl__randEval(string NAME)() {
    return mixin(NAME);
  }

  public void _esdl__initSolver()() {
    if (_esdl__solverInst is null) {
      _esdl__solverInst = new _esdl__Solver(_esdl__randSeed,
					    _esdl__Type.stringof, this);
    }
    else {
      _esdl__solverInst._esdl__setObjOuter();
    }
  }

  static if(__traits(compiles,
		     _esdl__upcast!(typeof(this))._esdl__hasRandomization)) {
    enum bool _esdl__baseHasRandomization = true;
    override public void randomize() {
      _esdl__randomize(this);
    }
  }
  else {
    enum bool _esdl__baseHasRandomization = false;
    public void randomize() {
      _esdl__randomize(this);
    }
    _esdl__Solver _esdl__solverInst;
    public uint _esdl__randSeed;
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
}

