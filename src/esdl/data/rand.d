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

template rand(N...) {
  static if(CheckRandParams!N) {
    struct rand
    {
      enum maxBounds = N;
      // this(int N) {
      // }
    }
  }
}

// Make sure that all the parameters are of type size_t
template CheckRandParams(N...) {
  static if(N.length > 0) {
    import std.traits;
    static if(!is(typeof(N[0]) == bool) && // do not confuse bool as size_t
	      is(typeof(N[0]) : size_t)) {
      static assert(N[0] != 0, "Can not have arrays with size 0");
      static assert(N[0] > 0, "Can not have arrays with negative size");
      enum bool CheckRecurse = CheckRandParams!(N[1..$]);
      enum bool CheckRandParams = CheckRecurse;
    }
    else {
      static assert(false, "Only positive integral values are allowed as array dimensions");
      enum bool CheckRandParams = false;
    }
  }
  else {
    enum bool CheckRandParams = true;
  }
}

abstract class _ESDL__ConstraintBase
{
  this(ConstraintEngine eng, string name, string constraint, uint index) {
    _cstEng = eng;
    _name = name;
    _constraint = constraint;
    _index = index;
  }
  immutable string _constraint;
  protected bool _enabled = true;
  protected ConstraintEngine _cstEng;
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
    BDD retval = _cstEng._buddy.one();
    return retval;
  }

  public string name() {
    return _name;
  }

  abstract public CstBlock getCstExpr();

}

abstract class Constraint (string C) : _ESDL__ConstraintBase
{
  this(ConstraintEngine eng, string name, uint index) {
    super(eng, name, C, index);
  }

  // static immutable string _constraint = C;
  // enum _parseTree = CstGrammar.parse(_constraint);
  // pragma(msg, _parseTree.capture);

  // Called by mixin to create functions out of parsed constraints
  static char[] constraintFoo(string CST) {
    import esdl.data.cstx;
    ConstraintParser parser = ConstraintParser(CST);
    return parser.translate();
  }

  debug(CONSTRAINTS) {
    pragma(msg, constraintFoo(C));
  }
};

class Constraint(string C, string NAME, T, S): Constraint!C
{
  T _outer;			// The object being randomized
  S _outerD;			// The most derived object -- the object on which randomize was originally applied

  this(T t, S s, string name) {
    super(t._esdl__cstEng, name, cast(uint) t._esdl__cstEng.cstList.length);
    _outer = t;
    _outerD = s;
  }
  // This mixin writes out the bdd functions after parsing the
  // constraint string at compile time
  mixin(constraintFoo(C));
}

// The inline constraint to be used with randomizeWith
class Constraint(string C, string NAME, T, S, size_t N): Constraint!C
{
  T _outer;
  S _outerD;

  long[N] _withArgs;

  void withArgs(V...)(V values) if(allIntengral!V) {
    foreach(i, v; values) {
      _withArgs[i] = v;
    }
  }

  this(T t, S s, string name) {
    super(t._esdl__cstEng, name, cast(uint) t._esdl__cstEng.cstList.length);
    _outer = t;
    _outerD = s;
  }

  public RndVecConst _esdl__arg(size_t VAR, T)(ref T t) {
    static assert(VAR < N, "Can not map specified constraint with argument: @" ~
		  VAR.stringof);
    return _esdl__rnd(_withArgs[VAR], t);
  }

  // This mixin writes out the bdd functions after parsing the
  // constraint string at compile time
  mixin(constraintFoo(C));
}

struct RandGen
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
    else static if(is(T: RandomizableIntf)) {
	// int seed = uniform!(int)(_gen);
	// t.seedRandom(seed);
	// t.randomize();
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

// Later we will use freelist to allocate CstStage
class CstStage {
  int _id = -1;
  // List of randomized variables associated with this stage. A
  // variable can be associated with only one stage
  RndVecPrim[] _rndVecs;
  // The Bdd expressions that apply to this stage
  CstBddExpr[] _bddExprs;
  // These are unresolved loop variables
  RndVecLoopVar[] _loopVars;
  // These are the length variables that this stage will solve
  RndVecArrVar[] _arrVars;

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

  // returns true if there are loop variables that need solving
  public bool hasLoops() {
    foreach(loop; _loopVars) {
      if(! loop.isUnrollable()) return true;
    }
    return false;
  }

}

public class ConstraintEngine {
  // Keep a list of constraints in the class
  _ESDL__ConstraintBase[] cstList;
  _ESDL__ConstraintBase cstWith;
  bool _cstWithChanged;

  // ParseTree parseList[];
  public RndVecPrim[] _rnds;
  RandGen _rgen;
  Buddy _buddy;

  // BddDomain[] _domains;
  BddDomain* _domains;
  
  ConstraintEngine _parent = null;

  this(uint seed, size_t rnum, ConstraintEngine parent) {
    debug(NOCONSTRAINTS) {
      assert(false, "Constraint engine started");
    }
    _rgen.seed(seed);
    // _buddy = _new!Buddy(400, 400);
    _rnds.length = rnum;
    _parent = parent;
  }

  this(uint seed, size_t rnum) {
    debug(NOCONSTRAINTS) {
      assert(false, "Constraint engine started");
    }
    _rgen.seed(seed);
    version(USE_EMPLACE) {
      _buddy = _new!Buddy(400, 400);
    }
    else {
      _buddy = new Buddy(400, 400);
    }
    _rnds.length = rnum;
  }

  ~this() {
    import core.memory: GC;
    cstList.length   = 0;
    cstWith          = null;
    _cstWithChanged  = true;
    _rnds.length = 0;

    // _domains.length  = 0;
    // GC.collect();
    _buddy.destroyBuddy();
  }

  public void markCstStageLoops(CstBddExpr expr) {
    auto vecs = expr.getPrims();
    foreach(ref vec; vecs) {
      if(vec !is null) {
	auto stage = vec.stage();
	if(stage !is null) {
	  stage._loopVars ~= expr.loopVars;
	}
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
      // import std.stdio;
      // writeln("null prim");
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

  // public size_t _esdl__countRands(size_t I=0, size_t C=0, T)(T t)
  //   if(is(T unused: RandomizableIntf)) {
  //     static if(I == t.tupleof.length) {
  //       static if(is(T B == super)
  // 		&& is(B[0] : RandomizableIntf)
  // 		&& is(B[0] == class)) {
  // 	B[0] b = t;
  // 	return _esdl__countRands!(0, C)(b);
  //       }
  //       else {
  // 	return C;
  //       }
  //     }
  //     else {
  //       import std.traits;
  //       import std.range;
  //       // check for the integral members
  //       alias typeof(t.tupleof[I]) L;
  //       static if((isIntegral!L || isBitVector!L) &&
  // 		findRandElemAttr!(I, t) != -1) {
  // 	return _esdl__countRands!(I+1, C+1)(t);
  //       }
  //       else static if(isStaticArray!L && (isIntegral!(ElementType!L) ||
  // 					 isBitVector!(ElementType!L)) &&
  // 		     findRandElemAttr!(I, t) != -1) {
  // 	  return _esdl__countRands!(I+1, C+1)(t);
  // 	}
  //       else static if(isDynamicArray!L && (isIntegral!(ElementType!L) ||
  // 					  isBitVector!(ElementType!L)) &&
  // 		     findRandArrayAttr!(I, t) != -1) {
  // 	  return _esdl__countRands!(I+1, C+1)(t);
  // 	}
  // 	else {
  // 	  return _esdl__countRands!(I+1, C)(t);
  // 	}
  //     }
  //   }
  // I is the tuple index in the given T
  // CI is the incremental index over the base classes
  // CI is the index in the list of @rand elements

  // domain index sequence has to match with _esdl__setRands and
  // _esdl__randNamedApply

  // For the moment we store the domain index in the RndVecPrim
  // structure. Later we can remove this element altogether since the
  // order of traversal in the three above mentioned functions would
  // be kept same.

  void initDomains(size_t I, size_t C, T)(T t)
    if(is(T unused: RandomizableIntf))  {
      static if(I == t.tupleof.length) {
        static if(is(T B == super)
		  && is(B[0] : RandomizableIntf)
		  && is(B[0] == class)) {
	  B[0] b = t;
	  return initDomains!(0, C)(b);
        }
        else {
	  // no super class and not another element in the current
	  // class -- we are done
	  return;
        }
      }
      else {
        import std.traits;
        import std.range;
        // check for the integral members
        alias typeof(t.tupleof[I]) L;
        static if((isIntegral!L || isBitVector!L) &&
		  findRandElemAttr!(I, t) != -1) {
	  // found an integral element, create a domain and continue
	  initDomains!(I+1, C+1)(t);
        }
        else static if(isStaticArray!L && (isIntegral!(ElementType!L) ||
					   isBitVector!(ElementType!L)) &&
		       findRandElemAttr!(I, t) != -1) {
	    initDomains!(I+1, C+1)(t);
	  }
        else static if(isDynamicArray!L && (isIntegral!(ElementType!L) ||
					    isBitVector!(ElementType!L)) &&
		       findRandArrayAttr!(I, t) != -1) {
	    initDomains!(I+1, C+1)(t);
	  }
	  else {
	    _esdl__countRands!(I+1, C)(t);
	  }
      }
    }

  void initDomains(T)(T t) {
    uint domIndex = 0;
    int[] domList;
    auto cstStmts = new CstBlock();	// start empty

    // take all the constraints -- even if disabled
    foreach(ref _ESDL__ConstraintBase cst; cstList) {
      cstStmts ~= cst.getCstExpr();
    }

    if(cstWith !is null) {
      cstStmts ~= cstWith.getCstExpr();
    }

    foreach(stmt; cstStmts._exprs) {
      foreach(vec; stmt.getPrims()) {
	if(vec.domIndex == uint.max) {
	  import std.stdio;
	  vec.domIndex = domIndex++;
	  domList ~= vec.bitcount;
	}
      }
    }

    _buddy.clearAllDomains();
    _domains = _buddy.extDomain(domList);

  }

  void solve(T)(T t) {
    // import std.stdio;
    // writeln("Solving BDD for number of contraints = ", cstList.length);

    // if(_domains.length == 0 || _cstWithChanged is true) {
    if(_domains is null || _cstWithChanged is true) {
      initDomains(t);
    }

    auto cstStmts = new CstBlock();	// start empty

    CstStage[] cstStages;

    foreach(ref _ESDL__ConstraintBase cst; cstList) {
      if(cst.isEnabled()) {
	cstStmts ~= cst.getCstExpr();
      }
    }
    if(cstWith !is null) {
      cstStmts ~= cstWith.getCstExpr();
    }

    auto cstExprs = cstStmts._exprs;
    auto usExprs = cstExprs;	// unstaged Expressions -- all
    auto usStages = cstStages;	// unresolved stages -- all

    // First we solve the constraint groups that are responsible for
    // setting the length of the rand!n dynamic arrays. After each
    // such constraint group is resolved, we go back and expand the
    // constraint expressions that depend on the LOOP Variables.

    // Once we have unrolled all the LOOPS, we go ahead and resolve
    // everything that remains.

    int stageIdx=0;

    // This variable is true when all the array lengths have been resolved
    bool allArrayLengthsResolved = false;

    // Ok before we start looking at the constraints, we create a
    // stage for each and every @rand that we have at hand
    foreach(rnd; _rnds) {
      if(cast(RndVecArrVar) rnd is null) {
	addCstStage(rnd, usStages);
      }
    }

    while(usExprs.length > 0 || usStages.length > 0) {
      cstExprs = usExprs;
      usExprs.length = 0;
      auto urExprs = usExprs;	// unrolled expressions
      cstStages = usStages;
      usStages.length = 0;


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
	if(expr.loopVars().length is 0) {
	  addCstStage(expr, cstStages);
	}
      }

      foreach(expr; urExprs) {
	if(expr.loopVars().length !is 0) {
	  // We want to mark the stages that are dependent on a
	  // loopVar -- so that when these loops get resolved, we are
	  // able to factor in more constraints into these stages and
	  // then resolve
	  markCstStageLoops(expr);
	  usExprs ~= expr;
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
	    if(stage.hasLoops() is 0 &&
	       stage._arrVars.length !is 0) {
	      solveStage(stage, stageIdx);
	      allArrayLengthsResolved = false;
	    }
	    else {
	      usStages ~= stage;
	    }
	  }
	}
      }
    }
  }

  void solveStage(CstStage stage, ref int stageIdx) {
    import std.conv;
    // initialize the bdd vectors
    BDD solveBDD = _buddy.one();

    foreach(vec; stage._rndVecs) {
      if(vec.stage is stage) {
	if(vec.bddvec.isNull()) {
	  vec.bddvec = _buddy.buildVec(_domains[vec.domIndex], vec.signed);
	}
	BDD primBdd = vec.getPrimBdd(_buddy);
	if(! primBdd.isOne()) {
	  solveBDD = solveBDD & primBdd;
	}
      }
    }

    // make the bdd tree
    auto exprs = stage._bddExprs;

    foreach(expr; exprs) {
      solveBDD = solveBDD & expr.getBDD(stage, _buddy);
    }

    // The idea is that we apply the max length constraint only if
    // there is another constraint on the lenght. If there is no
    // other constraint, then the array is taken care of later at
    // the time of setting the non-constrained random variables


    double[uint] bddDist;
    solveBDD.satDist(bddDist);

    auto solution = solveBDD.randSatOne(this._rgen.get(),
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
	  vec.value = vec.value + ((cast(ulong) _rgen.flip()) << i);
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


template isRandomizable(T) {	// check if T is Randomizable
  import std.traits;
  import std.range;
  import esdl.data.bvec;
  static if(isArray!T) {
    enum bool isRandomizable = isRandomizable!(ElementType!T);
  }
  else
  static if(isIntegral!T || isBitVector!T) {
    enum bool isRandomizable = true;
  }
  else {
    bool isRandomizable = false;
  }
}

public size_t _esdl__countRands(size_t I=0, size_t C=0, T)(T t)
  if(is(T unused: RandomizableIntf)) {
    static if(is(T B == super)
	      && is(B[0] : RandomizableIntf)
	      && is(B[0] == class)) {
      B[0] b = t;
      return _esdl__countRands!(0, C + t.tupleof.length)(b);
    }
    else {
      return C;
    }
  }

private template _esdl__randVar(string var) {
  import std.string;
  enum I = _esdl__randIndexof!(var);
  static if(I == -1) {
    enum string prefix = var;
    enum string suffix = "";
  }
  else {
    enum string prefix = var[0..I];
    enum string suffix = var[I..$];
  }
}

private template _esdl__randIndexof(string var, int index=0) {
  static if(index == var.length) {
    enum _esdl__randIndexof = -1;
  }
  else static if(var[index] == '.' ||
		 var[index] == '[' ||
		 var[index] == '(') {
      enum _esdl__randIndexof = index;
    }
    else {
      enum _esdl__randIndexof = _esdl__randIndexof!(var, index+1);
    }
}

interface RandomizableIntf
{
  static final string randomization() {
    enum string _esdl__vRand =
      q{
      alias typeof(this) _esdl__RandType;
      override public _esdl__RandType _esdl__typeID() {
	return null;
      }
      override public void _esdl__virtualInitCstEng() {
	_esdl__initCstEng!_esdl__RandType(this);
      }
      override public bool _esdl__virtualRandomize() {
	return _esdl__randomize!_esdl__RandType(this);
      }
      final auto _esdl__randEval(string NAME)() {
	return mixin(NAME);
      }
    };
    return _esdl__vRand;
  }

  mixin template Randomization()
  {
    import esdl.data.rand:_esdl__initCstEng, _esdl__randomize;
    alias typeof(this) _esdl__RandType;
    static if(! __traits(compiles, this._esdl__typeID)) {
      public _esdl__RandType _esdl__typeID() {
	return null;
      }

      public void _esdl__virtualInitCstEng() {
	_esdl__initCstEng!_esdl__RandType(this);
      }
      public bool _esdl__virtualRandomize() {
	return _esdl__randomize!_esdl__RandType(this);
      }

      public ConstraintEngine _esdl__cstEng;
      public uint _esdl__randSeed;

      void useThisBuddy() {
	assert(_esdl__cstEng !is null);
	useBuddy(_esdl__cstEng._buddy);
      }

      public void seedRandom(int seed) {
	_esdl__randSeed = seed;
	if (_esdl__cstEng !is null) {
	  _esdl__cstEng._rgen.seed(seed);
	}
      }
      alias seedRandom srandom;	// for sake of SV like names

      public ConstraintEngine getCstEngine() {
	return _esdl__cstEng;
      }

      void preRandomize() {}
      void postRandomize() {}
    }
    else {

      override public _esdl__RandType _esdl__typeID() {
	return null;
      }
      override public void _esdl__virtualInitCstEng() {
	_esdl__initCstEng!_esdl__RandType(this);
      }
      override public bool _esdl__virtualRandomize() {
	return _esdl__randomize!_esdl__RandType(this);
      }
      final auto _esdl__randEval(string NAME)() {
	return mixin(NAME);
      }
    }
  }

  ConstraintEngine getCstEngine();
  void preRandomize();
  void postRandomize();
  void seedRandom(int seed);
}

class Randomizable: RandomizableIntf
{
  mixin Randomization;
}

version(USE_EMPLACE) {
  T _new(T, Args...) (Args args) {
    import std.stdio, std.conv, core.memory;
    size_t objSize = __traits(classInstanceSize, T);
    void* tmp = GC.malloc(objSize);
    if (!tmp) throw new Exception("Memory allocation failed");
    void[] mem = tmp[0..objSize];
    T obj = emplace!(T, Args)(mem, args);
    return obj;
  }
}


// Initialize all random elements, arrays and objects. Do not yet
// initialize the elements of the array. These would be initialized
// only if these are referred to in the constraints.
void _esdl__initRnds(size_t I=0, size_t CI=0, T, S)(T t, S s)
  if(is(T: RandomizableIntf) && is(T == class) &&
     is(S: RandomizableIntf) && is(S == class)) {
    static if (I < t.tupleof.length) {
      _esdl__initRnd!(I, CI)(t, s);
      _esdl__initRnds!(I+1, CI+1) (t, s);
    }
    else static if(is(T B == super)
		   && is(B[0] : RandomizableIntf)
		   && is(B[0] == class)) {
	B[0] b = t;
	_esdl__initRnds!(0, CI) (b, s);
      }
  }


void _esdl__initRnd(size_t I=0, size_t CI=0, T, S) (T t, S s) {
  import std.traits;
  import std.conv;
  import std.string;

  auto l = t.tupleof[I];
  alias typeof(l) L;
  enum string NAME = t.tupleof[I].stringof[2..$];
  // Look for @rand attribute
  
  // static if (is (L f == Constraint!C, immutable (char)[] C)) {
  //   l = new Constraint!(C, NAME, T, S)(t, s, NAME);
  //   t._esdl__cstEng._rnds ~= l;
  // }
  // else {
  //   synchronized (t) {
  //     // Do nothing
  //   }
  // }
}
// I is the index within the class
// CI is the cumulative index -- starts from the most derived class
// and increases as we move up in the class hierarchy
void _esdl__initCsts(size_t I=0, size_t CI=0, T, S)(T t, S s)
  if(is(T: RandomizableIntf) && is(T == class) &&
     is(S: RandomizableIntf) && is(S == class)) {
    static if (I < t.tupleof.length) {
      _esdl__initCst!(I, CI)(t, s);
      _esdl__initCsts!(I+1, CI+1) (t, s);
    }
    else static if(is(T B == super)
		   && is(B[0] : RandomizableIntf)
		   && is(B[0] == class)) {
	B[0] b = t;
	_esdl__initCsts!(0, CI) (b, s);
      }
  }

void _esdl__initCst(size_t I=0, size_t CI=0, T, S) (T t, S s) {
  import std.traits;
  import std.conv;
  import std.string;

  auto l = t.tupleof[I];
  alias typeof(l) L;
  enum string NAME = t.tupleof[I].stringof[2..$];
  static if (is (L f == Constraint!C, immutable (char)[] C)) {
    l = new Constraint!(C, NAME, T, S)(t, s, NAME);
    t._esdl__cstEng.cstList ~= l;
  }
  else {
    synchronized (t) {
      // Do nothing
    }
  }
}

auto _esdl__namedApply(string VAR, alias F, size_t I=0, size_t CI=0, T)(T t)
  if(is(T unused: RandomizableIntf) && is(T == class)) {
    static if (I < t.tupleof.length) {
      static if ("t."~_esdl__randVar!VAR.prefix == t.tupleof[I].stringof) {
	return F!(VAR, I, CI)(t);
      }
      else {
	return _esdl__namedApply!(VAR, F, I+1, CI+1) (t);
      }
    }
    else static if(is(T B == super)
		   && is(B[0] : RandomizableIntf)
		   && is(B[0] == class)) {
	B[0] b = t;
	return _esdl__namedApply!(VAR, F, 0, CI) (b);
      }
      else {
	static assert(false, "Can not map variable: " ~ VAR);
      }
  }

void _esdl__setRands(size_t I=0, size_t CI=0, T)
  (T t, RndVecPrim[] vecVals, ref RandGen rgen)
  if(is(T unused: RandomizableIntf) && is(T == class)) {
    import std.traits;
    import esdl.data.bvec: toBitVec;
    static if (I < t.tupleof.length) {
      alias typeof(t.tupleof[I]) L;
      static if (isDynamicArray!L) {
	enum RLENGTH = findRandArrayAttr!(I, t);
	static if(RLENGTH != -1) { // is @rand
	  // make sure that there is only one dimension passed to @rand
	  static assert(findRandArrayAttr!(I, t, 1) == int.min);
	  // enum ATTRS = __traits(getAttributes, t.tupleof[I]);
	  // alias ATTRS[RLENGTH] ATTR;
	  auto vecVal = cast(RndVecArrVar) vecVals[CI];
	  if(vecVal is null) {
	    t.tupleof[I].length = rgen.gen(0, RLENGTH+1);
	  }
	  else {
	    t.tupleof[I].length = vecVal._arrLen.value;
	  }
	  foreach(idx, ref v; t.tupleof[I]) {
	    import std.range;
	    if(vecVal is null || (! vecVal.built()) || vecVal[idx] is null) {
	      // v = rgen.gen!(ElementType!L);
	      rgen.gen(v);

	    }
	    else {
	      v = cast(ElementType!L) vecVal[idx].value.toBitVec;
	    }
	  }
	  // t.tupleof[I] = rgen.gen!L;
	  // }
	  // else {
	  //   // t.tupleof[I] = cast(L) vecVal.value;
	  // }

	  _esdl__setRands!(I+1, CI+1) (t, vecVals, rgen);
	}
	else {
	  _esdl__setRands!(I+1, CI+1) (t, vecVals, rgen);
	}
      }
      else {
	static if(findRandElemAttr!(I, t) != -1) { // is @rand
	  static if(isStaticArray!L) {
	    auto vecVal = cast(RndVecArrVar) vecVals[CI];
	    if(vecVal is null) {
	      foreach(idx, ref v; t.tupleof[I]) {
		import std.range;
		// v = rgen.gen!(ElementType!L);
		rgen.gen(v);
	      }
	    }
	    else {
	      foreach(idx, ref v; t.tupleof[I]) {
		import std.range;
		auto elemVal = vecVal[idx];
		if(elemVal is null) {
		  // v = rgen.gen!(ElementType!L);
		  rgen.gen(v);
		}
		else {
		  alias ElementType!L R;
		  static if(isBitVector!R) {
		    import esdl.data.bvec;
		    v = cast(ElementType!L) elemVal.value.toBitVec;
		  }
		  else {
		    v = cast(R) elemVal.value;
		  }
		}
	      }
	    }
	  }
	  else static if(is(L: RandomizableIntf)) {
	      rgen.gen(t.tupleof[I]);
	    }
	    else {
	      auto vecVal = vecVals[CI];
	      if(vecVal is null) {
		// t.tupleof[I] = rgen.gen!L;
		rgen.gen(t.tupleof[I]);
	      }
	      else {
		import esdl.data.bvec;
		Bit!64 temp = vecVal.value;
		t.tupleof[I] = cast(L) temp;
	      }
	    }
	  _esdl__setRands!(I+1, CI+1) (t, vecVals, rgen);
	}
	else {
	  _esdl__setRands!(I+1, CI+1) (t, vecVals, rgen);
	}
      }
    }
    else static if(is(T B == super)
		   && is(B[0] : RandomizableIntf)
		   && is(B[0] == class)) {
	B[0] b = t;
	_esdl__setRands!(0, CI) (b, vecVals, rgen);
      }
  }

template findRandAttr(size_t I, alias t) {
  enum int randAttr =
    findRandElemAttrIndexed!(0, -1, __traits(getAttributes, t.tupleof[I]));
  enum int randsAttr =
    findRandArrayAttrIndexed!(0, -1, 0, __traits(getAttributes, t.tupleof[I]));
  enum bool findRandAttr = randAttr != -1 || randsAttr != -1;
}

template findRandElemAttr(size_t I, alias t) {
  enum int randAttr =
    findRandElemAttrIndexed!(0, -1, __traits(getAttributes, t.tupleof[I]));
  enum int randsAttr =
    findRandArrayAttrIndexed!(0, -1, 0, __traits(getAttributes, t.tupleof[I]));
  static assert(randsAttr == -1, "Illegal use of @rand!" ~ randsAttr.stringof);
  enum int findRandElemAttr = randAttr;
}

template findRandArrayAttr(size_t I, alias t, size_t R=0) {
  enum int randAttr =
    findRandElemAttrIndexed!(0, -1, __traits(getAttributes, t.tupleof[I]));
  enum int randsAttr =
    findRandArrayAttrIndexed!(0, -1, R, __traits(getAttributes, t.tupleof[I]));
  static assert(randAttr == -1,	"Illegal use of @rand");
  enum int findRandArrayAttr = randsAttr;
}

template findRandElemAttrIndexed(size_t C, int P, A...) {
  static if(A.length == 0) enum int findRandElemAttrIndexed = P;
  else static if(__traits(isSame, A[0], rand)) {
      static assert(P == -1, "@rand used twice in the same declaration");
      static if(A.length > 1)
	enum int findRandElemAttrIndexed = findRandElemAttrIndexed!(C+1, C, A[1..$]);
      else
	enum int findRandElemAttrIndexed = C;
    }
    else {
      enum int findRandElemAttrIndexed = findRandElemAttrIndexed!(C+1, P, A[1..$]);
    }
}

template findRandArrayAttrIndexed(size_t C, int P, size_t R, A...) {
  static if(A.length == 0) enum int findRandArrayAttrIndexed = P;
  else static if(is(A[0] unused: rand!M, M...)) {
      static assert(P == -1, "@rand used twice in the same declaration");
      static if(A.length > 1) {
	enum int findRandArrayAttrIndexed =
	  findRandArrayAttrIndexed!(C+1, C, R, A[1..$]);
      }
      else {
	static if(R < M.length && R >= 0) {
	  enum int findRandArrayAttrIndexed = M[R];
	}
	else {
	  enum int findRandArrayAttrIndexed = int.min;
	}
      }
    }
    else {
      enum int findRandArrayAttrIndexed =
	findRandArrayAttrIndexed!(C+1, P, R, A[1..$]);
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

public bool randomizeWith(string C, T, V...)(ref T t, V values)
  if(is(T v: RandomizableIntf) &&
     is(T == class) && allIntengral!V) {
    // The idea is that if the end-user has used the randomization
    // mixin then _esdl__RandType would be already available as an
    // alias and we can use virtual randomize method in such an
    // eventuality.
    // static if(is(typeof(t._esdl__RandType) == T)) {
    static if(is(typeof(t._esdl__typeID()) == T)) {
      t._esdl__virtualInitCstEng();
    }
    else {
      t._esdl__initCstEng();
    }
    if(t._esdl__cstEng.cstWith is null ||
       t._esdl__cstEng.cstWith._constraint != C) {
      auto withCst =
	new Constraint!(C, "_esdl__withCst",
			T, T, V.length)(t, t, "_esdl__withCst");
      withCst.withArgs(values);
      t._esdl__cstEng.cstWith = withCst;
      t._esdl__cstEng._cstWithChanged = true;
    }
    else {
      t._esdl__cstEng._cstWithChanged = false;
    }
    static if(is(typeof(t._esdl__typeID()) == T)) {
      return t._esdl__virtualRandomize();
    }
    else {
      return t._esdl__randomize();
    }
  }

public bool randomize(T) (ref T t)
  if(is(T v: RandomizableIntf) &&
     is(T == class)) {
    // The idea is that if the end-user has used the randomization
    // mixin then _esdl__RandType would be already available as an
    // alias and we can use virtual randomize method in such an
    // eventuality.
    // static if(is(typeof(t._esdl__RandType) == T)) {
    static if(is(typeof(t._esdl__typeID()) == T)) {
      t._esdl__virtualInitCstEng();
      if(t._esdl__cstEng.cstWith !is null) {
	t._esdl__cstEng.cstWith = null;
	t._esdl__cstEng._cstWithChanged = true;
      }
      else {
	t._esdl__cstEng._cstWithChanged = false;
      }
      return t._esdl__virtualRandomize();
    }
    else {
      _esdl__initCstEng(t);
      if(t._esdl__cstEng.cstWith !is null) {
	t._esdl__cstEng.cstWith = null;
	t._esdl__cstEng._cstWithChanged = true;
      }
      else {
	t._esdl__cstEng._cstWithChanged = false;
      }
      return _esdl__randomize(t);
    }
  }

public void _esdl__initCstEng(T) (T t)
  if(is(T v: RandomizableIntf) &&
     is(T == class)) {
    // Initialize the constraint database if not already done
    if (t._esdl__cstEng is null) {
      t._esdl__cstEng = new ConstraintEngine(t._esdl__randSeed,
					     _esdl__countRands(t));
      _esdl__initRnds(t, t);
      _esdl__initCsts(t, t);
    }
  }

public bool _esdl__randomize(T) (T t, _ESDL__ConstraintBase withCst = null)
  if(is(T v: RandomizableIntf) &&
     is(T == class)) {
    import std.exception;
    import std.conv;

    t.useThisBuddy();
    // Call the preRandomize hook
    t.preRandomize();

    auto values = t._esdl__cstEng._rnds;

    foreach(rnd; values) {
      if(rnd !is null) {
	// stages would be assigned again from scratch
	rnd.reset();
	// FIXME -- Perhaps some other fields too need to be reinitialized
      }
    }

    t._esdl__cstEng.solve(t);

    _esdl__setRands(t, values, t._esdl__cstEng._rgen);

    // Call the postRandomize hook
    t.postRandomize();
    exitBuddy();
    return true;
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
      LOOPINDEX,
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

  RndVecLoopVar[] _loopVars;

  public RndVecLoopVar[] loopVars() {
    return _loopVars;
  }

  RndVecArrVar[] _arrVars;

  public RndVecArrVar[] arrVars() {
    return _arrVars;
  }

  // get all the primary bdd vectors that constitute a given bdd expression
  abstract public RndVecPrim[] getPrims();

  // get the list of stages this expression should be avaluated in
  abstract public CstStage[] getStages();

  abstract public BddVec getBDD(CstStage stage, Buddy buddy);

  abstract public long evaluate();

  abstract public RndVecExpr unroll(RndVecLoopVar l, uint n);

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

  public RndVec2BddExpr lth(RndVecExpr other) {
    return new RndVec2BddExpr(this, other, CstBinBddOp.LTH);
  }

  public RndVec2BddExpr lte(RndVecExpr other) {
    return new RndVec2BddExpr(this, other, CstBinBddOp.LTE);
  }

  public RndVec2BddExpr gth(RndVecExpr other) {
    return new RndVec2BddExpr(this, other, CstBinBddOp.GTH);
  }

  public RndVec2BddExpr gte(RndVecExpr other) {
    return new RndVec2BddExpr(this, other, CstBinBddOp.GTE);
  }

  public RndVec2BddExpr equ(RndVecExpr other) {
    return new RndVec2BddExpr(this, other, CstBinBddOp.EQU);
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

  public CstBdd2BddExpr implies(RndVecExpr other)
  {
    return new CstBdd2BddExpr(this.toBdd(), other.toBdd(), CstBddOp.LOGICIMP);
  }

  public CstBdd2BddExpr logicOr(CstBddExpr other)
  {
    return new CstBdd2BddExpr(this.toBdd(), other, CstBddOp.LOGICOR);
  }

  public CstBdd2BddExpr logicOr(RndVecExpr other)
  {
    return new CstBdd2BddExpr(this.toBdd(), other.toBdd(), CstBddOp.LOGICOR);
  }

  public CstBdd2BddExpr logicAnd(CstBddExpr other)
  {
    return new CstBdd2BddExpr(this.toBdd(), other, CstBddOp.LOGICAND);
  }

  public CstBdd2BddExpr logicAnd(RndVecExpr other)
  {
    return new CstBdd2BddExpr(this.toBdd(), other.toBdd(), CstBddOp.LOGICAND);
  }

  public string name();
}

abstract class RndVecPrim: RndVecExpr
{
  abstract public bool isRand();
  abstract public long value();
  abstract public void value(long v);
  abstract public CstStage stage();
  abstract public void stage(CstStage s);
  public void reset() {
    stage = null;
  }
  abstract public uint domIndex();
  abstract public void domIndex(uint s);
  abstract public uint bitcount();
  abstract public bool signed();
  abstract public BddVec bddvec();
  abstract public void bddvec(BddVec b);
  abstract override public string name();

  // public RndVecArrLen length() {
  //   assert(false, "length may only be called for a RndVecArrVar");
  // }
  public void loopVar(RndVecLoopVar var) {
    assert(false, "loopVar may only be called for a RndVecArrLen");
  }
  public RndVecLoopVar loopVar() {
    assert(false, "loopVar may only be called for a RndVecArrLen");
  }
  public RndVecLoopVar makeLoopVar() {
    assert(false, "makeLoopVar may only be called for a RndVecArrLen");
  }
  // this method is used for getting implicit constraints that are required for
  // dynamic arrays and for enums
  public BDD getPrimBdd(Buddy buddy) {
    return buddy.one();
  }
  override public RndVecPrim unroll(RndVecLoopVar l, uint n) {
    return this;
  }
}

abstract class RndVecVar: RndVecPrim
{
  BddVec _bddvec;
  uint _domIndex = uint.max;
  CstStage _stage = null;
  bool _isRand;
  string _name;

  override string name() {
    return _name;
  }

  public this(string name, bool isRand) {
    _name = name;
    _isRand = isRand;
  }

  override public RndVecPrim[] getPrims() {
    RndVecPrim[] _prims;
    if(isRand) _prims = [this];
    return _prims;
  }

  override public CstStage[] getStages() {
    CstStage[] stages;
    if(isRand) stages = [this.stage()];
    return stages;
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
      assert(false, "Rand variable " ~ _name[2..$] ~ " evaluation in wrong stage: " ~ _stage._id.to!string);
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

}

class RndVecArrLen: RndVecVar
{
  
  // This bdd has the constraint on the max length of the array
  BDD _primBdd;
  size_t _maxArrLen;
  RndVecLoopVar _loopVar;

  RndVecArrVar _parent;
  
  public this(string name, long maxArrLen, bool isRand, RndVecArrVar parent) {
    super(name, isRand);
    _name = name;
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

  override public void loopVar(RndVecLoopVar var) {
    _loopVar = loopVar;
  }

  override public RndVecLoopVar loopVar() {
    return _loopVar;
  }

  override public RndVecLoopVar makeLoopVar() {
    if(_loopVar is null) {
      _loopVar = new RndVecLoopVar(_parent);
    }
    return _loopVar;
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
    bdd _primBdd;
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

// T represents the type of the Enum
class RndVec(T...): RndVecVar
{
  import esdl.data.bvec;
  alias L=T[0];
  mixin EnumConstraints!L;
  static if(T.length == 1) {
    L* _var;
    public this(string name, bool isRand, L* var) {
      super(name, isRand);
      _var = var;
    }

    override public long value() {
      return cast(long) (*_var);
    }

    override public void value(long v) {
      *_var = cast(L) toBitVec(v);
    }
  }
  else {
    alias P=T[1..$];
    RndVecArr!P _parent;
    ulong _index;
  
    public this(string name, bool isRand, RndVecArr!P parent,
		ulong index) {
      super(name, isRand);
      _parent = parent;
      _index = index;
    }
    
    override long value() {
      return _parent.getVal(_index);
    }

    override void value(long v) {
      _parent.setVal(v, _index);
    }
  }

  override uint bitcount() {
    static if(isIntegral!L)        return L.sizeof * 8;
    else static if(isBitVector!L)  return L.SIZE;
  }

  override bool signed() {
    static if(isVarSigned!L) {
      return true;
    }
    else  {
      return false;
    }
  }
};

class RndVecArr(T...): RndVecArrVar
{
  import std.traits;
  import std.range;
  static assert(T.length > 0);

  static if(T.length == 1) {
    alias L=T[0];
    L* _var;
    public this(string name, long maxArrLen,
		bool isRand, bool elemIsRand, L* var) {
      super(name, maxArrLen, isRand, elemIsRand);
      _var = var;
      _arrLen = new RndVecArrLen(name, maxArrLen, isRand, this);
    }
    override bool built() {
      return _elems.length != 0;
    }
    void build(ref L l) {
      alias ElementType!L E;
      static assert(isIntegral!E || isBitVector!E);
      _elems.length = maxArrLen();
      for (size_t i=0; i!=maxArrLen; ++i) {
	if(this[i] is null) {
	  import std.conv: to;
	  auto init = (E).init;
	  if(i < l.length) {
	    this[i] = new RndVec!(E, T)(_name ~ "[" ~ i.to!string() ~ "]",
					true, this, i);
	  }
	  else {
	    this[i] = new RndVec!(E, T)(_name ~ "[" ~ i.to!string() ~ "]",
					true, this, i);
	  }
	  assert(this[i] !is null);
	}
      }
    }

    static private long getLen_(A, I...)(ref A arr, I idx)
      if(isArray!A) {
	static if(I.length == 0) return arr.length;
	else {
	  return getLen_(arr[idx[0]], idx[1..$]);
	}
      }
    
    static private void setLen_(A, I...)(ref A arr, long v, I idx)
      if(isArray!A) {
	static if(I.length == 0) {
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
    
    static private long getVal(A, I...)(ref A arr, I idx)
      if(isArray!A && I.length > 0) {
	static if(I.length == 1) return arr[idx[0]];
	else {
	  return getVal(arr[idx[0]], idx[1..$]);
	}
      }
    
    static private void setVal(A, I...)(ref A arr, long v, I idx)
      if(isArray!A && I.length > 0) {
	static if(I.length == 1) {
	  alias E = ElementType!A;
	  arr[idx[0]] = cast(E) v;
	}
	else {
	  setVal(arr[idx[0]], v, idx[1..$]);
	}
      }
    
    public long getLen_(I...)(I idx) {
      return getLen_(*_var, idx);
    }

    public void setLen_(I...)(long v, I idx) {
      setLen_(*_var, v, idx);
    }

    override public long getLen() {
      return getLen_(*_var);
    }

    override public void setLen(long v) {
      setLen_(*_var, v);
    }

    public long getVal(I...)(I idx) {
      return getVal(*_var, idx);
    }

    public void setVal(I...)(long v, I idx) {
      setVal(*_var, v, idx);
    }
  }
  else {
    RndVecArr!(T[1..$]) _parent;
    ulong _index;
  
    public this(string name, long maxArrLen, bool isRand, bool elemIsRand,
		RndVecArr!(T[1..$]) parent, ulong index) {
      super(name, maxArrLen, isRand, elemIsRand);
      _parent = parent;
      _index = index;
      _arrLen = new RndVecArrLen(name, maxArrLen, isRand, this);
    }

    public long getLen(I...)(I idx) {
      return _parent.getLen(_index, idx);
    }

    public void setLen(I...)(long v, I idx) {
      _parent.setLen(v, _index, idx);
    }

    public long getVal(I...)(I idx) {
      return _parent.getVal(_index, idx);
    }

    public void setVal(I...)(long v, I idx) {
      _parent.setVal(v, _index, idx);
    }
  }    


};
			
abstract class RndVecArrVar: RndVecPrim
{
  // Base class object shall be used for constraining the length part
  // of the array.

  // Also has an array of RndVecVar to map all the elements of the
  // array
  RndVecPrim[] _elems;
  bool _elemIsRand;

  string _name;

  RndVecArrLen _arrLen;

  override public string name() {
    return _name;
  }

  override public void reset() {
    _arrLen.stage = null;
    foreach(elem; _elems) {
      if(elem !is null) {
	elem.reset();
      }
    }
  }

  override public RndVecPrim[] getPrims() {
    return _elems.dup;
  }

  override public RndVecArrVar[] arrVars() {
    if(_arrLen.isRand()) return [this];
    else return [];
  }

  bool isUnrollable() {
    if(! isRand) return true;
    if(this.stage.solved()) return true;
    else return false;
  }

  override public RndVec2VecExpr opIndex(RndVecExpr idx) {
    return new RndVec2VecExpr(this, idx, CstBinVecOp.LOOPINDEX);
  }

  override public RndVecPrim opIndex(size_t idx) {
    return _elems[idx];
  }

  void opIndexAssign(RndVecVar c, size_t idx) {
    _elems[idx] = c;
  }

  public this(string name, long maxArrLen,
	      bool isRand, bool elemIsRand) {
    // super(name, maxArrLen, signed, bitcount, isRand);
    _name = name;
    _elemIsRand = elemIsRand;
    // _elems.length = maxArrLen;
  }

  bool built();

  size_t maxArrLen() {
    return _arrLen._maxArrLen;
  }

  public RndVecArrLen arrLen() {
    return _arrLen;
  }
  
  abstract long getLen();
  abstract void setLen(long len);

  // override public RndVecPrim[] getPrims() {
  //   return _arrLen.getPrims();
  // }

  override public CstStage[] getStages() {
    assert(false, "getStages not implemented for RndVecArrVar");
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

}

// This class represents an unrolled Foreach loop at vec level
class RndVecLoopVar: RndVecPrim
{
  // _loopVar will point to the array this RndVecLoopVar is tied to
  RndVecArrVar _arrVar;

  RndVecArrVar arrVar() {
    return _arrVar;
  }

  uint maxVal() {
    if(! this.isUnrollable()) {
      assert(false, "Can not find maxVal since the "
	     "Loop Variable is unrollable");
    }
    return cast(uint) arrVar._arrLen.value;
  }

  override RndVecLoopVar[] loopVars() {
    return [this];
  }

  // this will not return the arrVar since the length variable is
  // not getting constraint here
  override RndVecArrVar[] arrVars() {
    return [];
  }

  this(RndVecArrVar arrVar) {
    _arrVar = arrVar;
    arrVar._arrLen.loopVar(this);
  }

  bool isUnrollable(RndVecArrVar arrVar) {
    if(arrVar is _arrVar) {
      return true;
    }
    else {
      return false;
    }
  }

  bool isUnrollable() {
    if(! _arrVar._arrLen.isRand()) return true;
    if(_arrVar._arrLen.stage !is null &&
       _arrVar._arrLen.stage.solved()) return true;
    else return false;
  }

  // get all the primary bdd vectors that constitute a given bdd expression
  override public RndVecPrim[] getPrims() {
    return arrVar._arrLen.getPrims();
  }

  // get the list of stages this expression should be avaluated in
  override public CstStage[] getStages() {
    return arrVar._arrLen.getStages();
  }

  override public BddVec getBDD(CstStage stage, Buddy buddy) {
    assert(false, "Can not getBDD for a Loop Variable without unrolling");
  }

  override public long evaluate() {
    assert(false, "Can not evaluate for a Loop Variable without unrolling");
  }

  override public bool isRand() {
    return arrVar._arrLen.isRand();
  }
  override public long value() {
    return arrVar._arrLen.value();
  }
  override public void value(long v) {
    arrVar._arrLen.value(v);
  }
  override public CstStage stage() {
    return arrVar._arrLen.stage();
  }
  override public void stage(CstStage s) {
    arrVar._arrLen.stage(s);
  }
  override public uint domIndex() {
    return arrVar._arrLen.domIndex;
  }
  override public void domIndex(uint s) {
    arrVar._arrLen.domIndex(s);
  }
  override public uint bitcount() {
    return arrVar._arrLen.bitcount();
  }
  override public bool signed() {
    return arrVar._arrLen.signed();
  }
  override public BddVec bddvec() {
    return arrVar._arrLen.bddvec();
  }
  override public void bddvec(BddVec b) {
    arrVar.bddvec(b);
  }
  override public string name() {
    return arrVar._arrLen.name();
  }
  override public RndVecPrim unroll(RndVecLoopVar l, uint n) {
    if(this !is l) return this;
    else return new RndVecConst(n, false);
  }
}

abstract class RndVecObjVar: RndVecPrim
{
  // Base class object shall be used for constraining the length part
  // of the array.

  // Also has an array of RndVecVar to map all the elements of the
  // array
  RndVecPrim[] _elems;
  bool _elemIsRand;

  string _name;

  // RndVecArrLen _arrLen;

  override public string name() {
    return _name;
  }

  override public void reset() {
    // _arrLen.stage = null;
    foreach(elem; _elems) {
      if(elem !is null) {
	elem.reset();
      }
    }
  }

  override public RndVecPrim[] getPrims() {
    RndVecPrim[] prims;
    foreach(elem; _elems) {
      prims ~= elem.getPrims();
    }
    return prims;
  }

  override public RndVecArrVar[] arrVars() {
    RndVecArrVar[] arrs;
    foreach(elem; _elems) {
      arrs ~= elem.arrVars();
    }
    return arrs;
  }

  public this(string name) {
    _name = name;
  }

  bool built();

  // override public RndVecPrim[] getPrims() {
  //   return _arrLen.getPrims();
  // }

  override public CstStage[] getStages() {
    assert(false, "getStages not implemented for RndVecObjVar");
  }
  
  override public BddVec getBDD(CstStage stage, Buddy buddy) {
    assert(false, "getBDD not implemented for RndVecObjVar");
  }
  
  override public long evaluate() {
    assert(false, "evaluate not implemented for RndVecObjVar");
  }

  override public bool isRand() {
    assert(false, "isRand not implemented for RndVecObjVar");
  }

  override public long value() {
    assert(false, "value not implemented for RndVecObjVar");
  }

  override public void value(long v) {
    assert(false, "value not implemented for RndVecObjVar");
  }

  override public CstStage stage() {
    assert(false, "stage not implemented for RndVecObjVar");
  }

  override public void stage(CstStage s) {
    assert(false, "stage not implemented for RndVecObjVar");
  }

  override public uint domIndex() {
    assert(false, "domIndex not implemented for RndVecObjVar");
  }

  override public void domIndex(uint s) {
    assert(false, "domIndex not implemented for RndVecObjVar");
  }

  override public uint bitcount() {
    assert(false, "bitcount not implemented for RndVecObjVar");
  }

  override public bool signed() {
    assert(false, "signed not implemented for RndVecObjVar");
  }

  override public BddVec bddvec() {
    assert(false, "bddvec not implemented for RndVecObjVar");
  }

  override public void bddvec(BddVec b) {
    assert(false, "bddvec not implemented for RndVecObjVar");
  }

}

class RndVecConst: RndVecPrim
{
  import std.conv;

  long _value;			// the value of the constant
  bool _signed;
  string _name;

  public this(long value, bool signed) {
    _value = value;
    _name = value.to!string();
    _signed = signed;
  }

  override public RndVecPrim[] getPrims() {
    return [];
  }

  override public CstStage[] getStages() {
    return [];
  }

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
    if(_op !is CstBinVecOp.LOOPINDEX) {
      return _lhs.getPrims() ~ _rhs.getPrims();
    }
    else {
      // LOOP
      // first make sure that the _lhs is an array
      auto lhs = cast(RndVecArrVar) _lhs;
      // FIXME -- what if the LOOPINDEX is use with non-rand array?
      assert(lhs !is null, "LOOPINDEX can not work with non-arrays");
      if(_rhs.loopVars.length is 0) {
	return [lhs[_rhs.evaluate()]];
      }
      else {
	return lhs.getPrims();
      }
    }
  }

  override public CstStage[] getStages() {
    import std.exception;

    enforce(_lhs.getStages.length <= 1 &&
	    _rhs.getStages.length <= 1);

    if(_lhs.getStages.length is 0) return _rhs.getStages;
    else if(_rhs.getStages.length is 0) return _lhs.getStages;
    else {
      // import std.algorithm: max;
      // Stages need to be merged
      // uint stage = max(_lhs.getStages[0], _rhs.getStages[0]);
      // return [stage];
      return _lhs.getStages;
    }
  }

  override public BddVec getBDD(CstStage stage, Buddy buddy) {
    if(this.loopVars.length !is 0) {
      assert(false,
	     "RndVec2VecExpr: Need to unroll the loopVars"
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
    case CstBinVecOp.LOOPINDEX:
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
    case CstBinVecOp.LOOPINDEX: return _lhs[rvec].evaluate();
    case CstBinVecOp.BITINDEX:
      assert(false, "BITINDEX is not implemented yet!");
    }
  }

  override public RndVec2VecExpr unroll(RndVecLoopVar l, uint n) {
    bool loop = false;
    foreach(loopVar; loopVars()) {
      if(l is loopVar) {
	loop = true;
	break;
      }
    }
    if(! loop) return this;
    else {
      return new RndVec2VecExpr(_lhs.unroll(l, n), _rhs.unroll(l, n), _op);
    }
  }

  public this(RndVecExpr lhs, RndVecExpr rhs, CstBinVecOp op) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
    foreach(loopVar; lhs.loopVars ~ rhs.loopVars) {
      bool add = true;
      foreach(l; _loopVars) {
	if(l is loopVar) add = false;
	break;
      }
      if(add) _loopVars ~= loopVar;
    }
    foreach(arrVar; lhs.arrVars ~ rhs.arrVars) {
      if(op !is CstBinVecOp.LOOPINDEX) {
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

  override public CstStage[] getStages() {
    import std.exception;

    return _vec.getStages();
    // enforce(_vec.getStages.length <= 1 &&
    //	    _lhs.getStages.length <= 1 &&
    //	    _rhs.getStages.length <= 1);

    // if(_lhs.getStages.length is 0) return _rhs.getStages;
    // else if(_rhs.getStages.length is 0) return _lhs.getStages;
    // else {
    //   // import std.algorithm: max;
    //   // Stages need to be merged
    //   // uint stage = max(_lhs.getStages[0], _rhs.getStages[0]);
    //   // return [stage];
    //   return _lhs.getStages;
    // }
  }

  override public BddVec getBDD(CstStage stage, Buddy buddy) {
    if(this.loopVars.length !is 0) {
      assert(false,
	     "RndVecSliceExpr: Need to unroll the loopVars"
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

  override public RndVecSliceExpr unroll(RndVecLoopVar l, uint n) {
    bool loop = false;
    foreach(loopVar; loopVars()) {
      if(l is loopVar) {
	loop = true;
	break;
      }
    }
    if(! loop) return this;
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
    auto loopVars = vec.loopVars ~ lhs.loopVars;
    if(rhs !is null) {
      loopVars ~= rhs.loopVars;
    }
    foreach(loopVar; loopVars) {
      bool add = true;
      foreach(l; _loopVars) {
	if(l is loopVar) add = false;
	break;
      }
      if(add) _loopVars ~= loopVar;
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

  // In case this expr is unRolled, the _loopVars here would be empty
  RndVecLoopVar[] _loopVars;

  public RndVecLoopVar[] loopVars() {
    return _loopVars;
  }

  RndVecArrVar[] _arrVars;

  public RndVecArrVar[] arrVars() {
    return _arrVars;
  }

  // unroll recursively untill no unrolling is possible
  public CstBddExpr[] unroll() {
    CstBddExpr[] retval;
    auto loop = this.unrollable();
    if(loop is null) {
      return [this];
    }
    else {
      foreach(expr; this.unroll(loop)) {
	if(expr.unrollable() is null) retval ~= expr;
	else retval ~= expr.unroll();
      }
    }
    return retval;
  }

  public CstBddExpr[] unroll(RndVecLoopVar l) {
    CstBddExpr[] retval;
    if(! l.isUnrollable()) {
      assert(false, "RndVecLoopVar is not unrollabe yet");
    }
    auto max = l.maxVal();
    for (uint i = 0; i != max; ++i) {
      retval ~= this.unroll(l, i);
    }
    return retval;
  }

  public RndVecLoopVar unrollable() {
    foreach(loop; _loopVars) {
      if(loop.isUnrollable()) return loop;
    }
    return null;
  }

  abstract public CstBddExpr unroll(RndVecLoopVar l, uint n);

  abstract public RndVecPrim[] getPrims();

  abstract public CstStage[] getStages();

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

  override public CstStage[] getStages() {
    CstStage[] stages;

    foreach(lstage; _lhs.getStages) {
      bool already = false;
      foreach(stage; stages) {
	if(stage is lstage) {
	  already = true;
	}
      }
      if(! already) stages ~= lstage;
    }
    foreach(rstage; _rhs.getStages) {
      bool already = false;
      foreach(stage; stages) {
	if(stage is rstage) {
	  already = true;
	}
      }
      if(! already) stages ~= rstage;
    }

    return stages;
  }

  override public BDD getBDD(CstStage stage, Buddy buddy) {
    if(this.loopVars.length !is 0) {
      assert(false,
	     "CstBdd2BddExpr: Need to unroll the loopVars"
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

  override public CstBdd2BddExpr unroll(RndVecLoopVar l, uint n) {
    bool loop = false;
    foreach(loopVar; loopVars()) {
      if(l is loopVar) {
	loop = true;
	break;
      }
    }
    if(! loop) return this;
    else {
      return new CstBdd2BddExpr(_lhs.unroll(l, n), _rhs.unroll(l, n), _op);
    }
  }

  public this(CstBddExpr lhs, CstBddExpr rhs, CstBddOp op) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
    foreach(loopVar; lhs.loopVars ~ rhs.loopVars) {
      bool add = true;
      foreach(l; _loopVars) {
	if(l is loopVar) add = false;
	break;
      }
      if(add) _loopVars ~= loopVar;
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

  override public CstStage[] getStages() {
    import std.exception;
    enforce(_lhs.getStages.length <= 1 &&
	    _rhs.getStages.length <= 1);

    if(_lhs.getStages.length is 0) return _rhs.getStages;
    else if(_rhs.getStages.length is 0) return _lhs.getStages;
    else {
      // import std.algorithm: max;
      // uint stage = max(_lhs.getStages[0], _rhs.getStages[0]);
      // return [stage];
      return _lhs.getStages;
    }
  }

  override public RndVecPrim[] getPrims() {
    return _lhs.getPrims() ~ _rhs.getPrims();
  }

  override public BDD getBDD(CstStage stage, Buddy buddy) {
    if(this.loopVars.length !is 0) {
      assert(false,
	     "RndVec2BddExpr: Need to unroll the loopVars"
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

  override public RndVec2BddExpr unroll(RndVecLoopVar l, uint n) {
    bool loop = false;
    foreach(loopVar; loopVars()) {
      if(l is loopVar) {
	loop = true;
	break;
      }
    }
    if(! loop) return this;
    else {
      return new RndVec2BddExpr(_lhs.unroll(l, n), _rhs.unroll(l, n), _op);
    }
  }

  public this(RndVecExpr lhs, RndVecExpr rhs, CstBinBddOp op) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
    foreach(loopVar; lhs.loopVars ~ rhs.loopVars) {
      bool add = true;
      foreach(l; _loopVars) {
	if(l is loopVar) add = false;
	break;
      }
      if(add) _loopVars ~= loopVar;
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

class CstNotBddExpr: CstBddExpr
{
  CstBddExpr _expr;

  override public string name() {
    return "( " ~ "!" ~ " " ~ _expr.name ~ " )";
  }

  override public RndVecPrim[] getPrims() {
    return _expr.getPrims();
  }

  override public CstStage[] getStages() {
    return _expr.getStages();
  }

  override public BDD getBDD(CstStage stage, Buddy buddy) {
    if(this.loopVars.length !is 0) {
      assert(false,
	     "CstBdd2BddExpr: Need to unroll the loopVars"
	     " before attempting to solve BDD");
    }
    auto bdd = _expr.getBDD(stage, buddy);
    return (~ bdd);
  }

  override public CstNotBddExpr unroll(RndVecLoopVar l, uint n) {
    bool shouldUnroll = false;
    foreach(loopVar; loopVars()) {
      if(l is loopVar) {
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
    _loopVars = expr.loopVars;
    _arrVars = expr.arrVars;
  }
}

class CstBlock: CstBddExpr
{
  CstBddExpr[] _exprs;

  override public string name() {
    string name_ = "";
    foreach(expr; _exprs) {
      name_ ~= " & " ~ expr.name() ~ "\n";
    }
    return name_;
  }

  override public RndVecPrim[] getPrims() {
    RndVecPrim[] prims;

    foreach(expr; _exprs) {
      prims ~= expr.getPrims();
    }

    return prims;
  }

  override public CstBlock unroll(RndVecLoopVar l, uint n) {
    assert(false, "Can not unroll a CstBlock");
  }

  override public CstStage[] getStages() {
    CstStage[] stages;

    foreach(expr; _exprs) {
      foreach(lstage; expr.getStages) {
	bool already = false;
	foreach(stage; stages) {
	  if(stage is lstage) {
	    already = true;
	  }
	}
	if(! already) stages ~= lstage;
      }
    }

    return stages;
  }

  override public BDD getBDD(CstStage stage, Buddy buddy) {
    assert(false, "getBDD not implemented for CstBlock");
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
      foreach(expr; other._exprs) {
	_exprs ~= expr;
      }
    }

}

auto _esdl__randNamedApply(string VAR, alias F, T)(T t)
  if(is(T unused: RandomizableIntf) && is(T == class)) {
    return _esdl__randNamedApplyExec!(VAR, F, 0, 0, T)(t, t);
  }

auto _esdl__randNamedApplyExec(string VAR, alias F, size_t I=0,
			       size_t CI=0, T, U)(T t, U u)
  if(is(T unused: RandomizableIntf) && is(T == class)) {
    static if (I < t.tupleof.length) {
      static if ("t."~_esdl__randVar!VAR.prefix == t.tupleof[I].stringof) {
	return F!(VAR, I, CI)(t);
      }
      else {
	static if(findRandAttr!(I, t)) {
	  return _esdl__randNamedApplyExec!(VAR, F, I+1, CI+1) (t, u);
	}
	else {
	  return _esdl__randNamedApplyExec!(VAR, F, I+1, CI+1) (t, u);
	}
      }
    }
    else static if(is(T B == super)
		   && is(B[0] : RandomizableIntf)
		   && is(B[0] == class)) {
	B[0] b = t;
	return _esdl__randNamedApplyExec!(VAR, F, 0, CI) (b, u);
      }
      else {
	// Ok so the variable could not be mapped -- now try general
	// evaluation in the scope of the object
	return _esdl__rnd(u._esdl__randEval!VAR(), u);
      }
  }

private size_t _esdl__delim(string name) {
  foreach(i, c; name) {
    if(c is '.' || c is '[') {
      return i;
    }
  }
  return name.length;
}

public RndVecConst _esdl__rnd(INT, T)(INT var, ref T t)
  if((isIntegral!INT || isBitVector!INT) &&
     is(T f: RandomizableIntf) && is(T == class)) {
    ulong val = var;
    return new RndVecConst(val, isVarSigned!INT);
  }


public auto _esdl__rnd(string VAR, T)(ref T t)
  if(is(T f: RandomizableIntf) && is(T == class)) {
    enum IDX = _esdl__delim(VAR);
    enum LOOKUP = VAR[0..IDX];
    static if(IDX == VAR.length) {
      return _esdl__randNamedApply!(LOOKUP, _esdl__rnd)(t);
    }
    else static if(VAR[IDX..$] == ".length") {
	return _esdl__randNamedApply!(LOOKUP, _esdl__rndArrLen)(t);
      }
    else static if(VAR[IDX] == '.') {
	return _esdl__randNamedApply!(VAR, _esdl__rnd)(t);
      }
    else static if(VAR[IDX] == '[') {
	// hmmmm
	// limitation -- the index expression can not have random
	// variable references. Expression consitiing of loop variable
	// and constants should be fine.
	// We should never be required to call getBDD on this
	// expression -- only evaluate.
	// --
	// It makes all the sense to parse this indenxing part in the
	// cstx module itself.
      }
  }

public auto _esdl__rnd(string VAR, size_t I,
		       size_t CI, T)(ref T t) {
  import std.traits;
  import std.range;
  import esdl.data.bvec;

  // need to know the size and sign for creating a bddvec
  alias typeof(t.tupleof[I]) L;
  static if(isArray!L) {
    alias ElementType!L E;
    static assert(isIntegral!E || isBitVector!E);

    static if(isDynamicArray!L) { // @rand!N form
      enum size_t RLENGTH = findRandArrayAttr!(I, t);
      static assert(RLENGTH != -1);
      enum bool DYNAMIC = true;
    }
    else static if(isStaticArray!L) { // @rand with static array
	size_t RLENGTH = t.tupleof[I].length;
	static assert(findRandElemAttr!(I, t) != -1);
	enum bool DYNAMIC = false;
      }
      else {
	static assert("Can not use .length with non-arrays");
      }
  
    auto rndVecPrim = t._esdl__cstEng._rnds[CI];
    if(rndVecPrim is null) {
      rndVecPrim =
	new RndVecArr!L(t.tupleof[I].stringof, RLENGTH,
			DYNAMIC, true, &(t.tupleof[I]));
      t._esdl__cstEng._rnds[CI] = rndVecPrim;
    }
    return rndVecPrim;
  }
  else {
    static assert(isIntegral!L || isBitVector!L,
		  "Unsupported type: " ~ L.stringof);

    // pragma(msg, t.tupleof[I].stringof);
    static if(findRandElemAttr!(I, t) == -1) {
      return _esdl__rnd(t.tupleof[I], t);
    }
    else {
      auto rndVecPrim = t._esdl__cstEng._rnds[CI];
      if(rndVecPrim is null) {
	rndVecPrim = new RndVec!L(t.tupleof[I].stringof,
				  true, &(t.tupleof[I]));
	t._esdl__cstEng._rnds[CI] = rndVecPrim;
      }
      return rndVecPrim;
    }
  }
}

// public RndVecPrim _esdl__rndElem(string VAR, T)(ref T t)
//   if(is(T f: RandomizableIntf) && is(T == class)) {
//     return _esdl__randNamedApply!(VAR, _esdl__rndElem)(t);
//   }

// public RndVecPrim _esdl__rndElem(string VAR, size_t I,
// 				 size_t CI, T)(ref T t) {
//   import std.traits: isIntegral;
//   import std.range: ElementType;

//   static assert(isArray!L);
//   // need to know the size and sign for creating a bddvec
//   alias typeof(t.tupleof[I]) L;
//   alias ElementType!L E;

//   static assert(isIntegral!E || isBitVector!E);

//   static if(isVarSigned!E) bool signed = true;
//   else                     bool signed = false;

//   static if(isIntegral!L)       uint bitcount = L.sizeof * 8;
//   else static if(isBitVector!L) uint bitcount = L.SIZE;
//     else static assert(false, "Only numeric or bitvector expression"
// 		       "are allowed in constraint expressions");

//   static if(findRandElemAttr!(I, t) == -1) {
//     // no @rand attribute -- just create the rndVecPrim and return
//     auto rndVecPrim = new RndVec!E(t.tupleof[I].stringof,
// 				   cast(long) t.tupleof[I],
// 				   signed, bitcount, false, null);
//   }
//   else {
//     auto rndVecPrim = t._esdl__cstEng._rnds[CI];
//     if(rndVecPrim is null) {
//       rndVecPrim = new RndVec!E(t.tupleof[I].stringof,
// 				cast(long) t.tupleof[I],
// 				signed, bitcount, true, null);
//       t._esdl__cstEng._rnds[CI] = rndVecPrim;
//     }
//   }
//   return rndVecPrim;
// }

public auto _esdl__rndArrLen(string VAR, size_t I,
			     size_t CI, T)(ref T t) {
  import std.traits;
  import std.range;

  // need to know the size and sign for creating a bddvec
  alias typeof(t.tupleof[I]) L;
  static assert(isArray!L);
  alias ElementType!L E;
  static assert(isIntegral!E || isBitVector!E);

  static if(! findRandAttr!(I, t)) { // no @rand attr
    return _esdl__rnd(t.tupleof[I].length, t);
  }
  static if(isDynamicArray!L) { // @rand!N form
    enum size_t RLENGTH = findRandArrayAttr!(I, t);
    static assert(RLENGTH != -1);
    enum bool DYNAMIC = true;
  }
  else static if(isStaticArray!L) { // @rand with static array
      size_t RLENGTH = t.tupleof[I].length;
      static assert(findRandElemAttr!(I, t) != -1);
      enum bool DYNAMIC = false;
    }
    else static assert("Can not use .length with non-arrays");
  
  auto rndVecPrim = t._esdl__cstEng._rnds[CI];
  if(rndVecPrim is null) {
    auto rndVecArr =
      new RndVecArr!L(t.tupleof[I].stringof, RLENGTH, DYNAMIC,
		      true, &(t.tupleof[I]));
    t._esdl__cstEng._rnds[CI] = rndVecArr;
    return rndVecArr.arrLen;
  }
  else {
    return (cast(RndVecArr!L) rndVecPrim).arrLen;
  }
}

public auto _esdl__rndArrElem(string VAR, size_t I,
			      size_t CI, T)(ref T t) {
  import std.traits;
  import std.range;

  // need to know the size and sign for creating a bddvec
  alias typeof(t.tupleof[I]) L;
  static assert(isArray!L);
  alias ElementType!L E;
  static assert(isIntegral!E || isBitVector!E);

  static if(! findRandAttr!(I, t)) { // no @rand attr
    static assert(false,
		  "Foreach constraint can be applied only on @rand arrays: " ~
		  t.tupleof[I].stringof);
    // return _esdl__rnd(t.tupleof[I].length, t);
  }
  else {
    auto rndVecPrim = t._esdl__cstEng._rnds[CI];
    auto rndVecArr = cast(RndVecArr!L) rndVecPrim;
    if(rndVecArr is null && rndVecPrim !is null) {
      assert(false, "Non-array RndVecPrim for an Array");
    }
    static if(isDynamicArray!L) { // @rand!N form
      enum size_t RLENGTH = findRandArrayAttr!(I, t);
      enum bool DYNAMIC = true;
      static assert(RLENGTH != -1);
    }
    else static if(isStaticArray!L) { // @rand with static array
	static assert(findRandElemAttr!(I, t) != -1);
	size_t RLENGTH = t.tupleof[I].length;
	enum bool DYNAMIC = true;
      }
      else static assert("Can not use .length with non-arrays");
    if(rndVecArr is null) {
      rndVecArr =
	new RndVecArr!L(t.tupleof[I].stringof, RLENGTH,
			DYNAMIC, true,	&(t.tupleof[I]));
      t._esdl__cstEng._rnds[CI] = rndVecArr;
    }
    rndVecArr.build(t.tupleof[I]);
    return rndVecArr;
  }
}

public RndVecLoopVar _esdl__rndArrIndex(string VAR, T)(ref T t)
  if(is(T f: RandomizableIntf) && is(T == class)) {
    return _esdl__randNamedApply!(VAR, _esdl__rndArrIndex)(t);
  }

public RndVecLoopVar _esdl__rndArrIndex(string VAR, size_t I,
					size_t CI, T)(ref T t) {
  auto lvar = _esdl__rndArrLen!(VAR, I, CI, T)(t);
  return lvar.makeLoopVar();
}

public RndVecExpr _esdl__rndArrElem(string VAR, T)(ref T t)
  if(is(T f: RandomizableIntf) && is(T == class)) {
    auto arr = _esdl__randNamedApply!(VAR, _esdl__rndArrElem)(t);
    auto idx = arr.arrLen.makeLoopVar();
    return arr[idx];
  }
