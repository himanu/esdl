// Written in the D programming language.

// Copyright: Coverify Systems Technology 2012 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

module esdl.data.randcore;

import esdl.data.obdd;

import std.traits: isIntegral, isBoolean;
import esdl.data.bvec: isBitVector;
import std.algorithm : min, max;
import esdl.data.bstr;

import std.exception: enforce;

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

enum _esdl__norand;

template rand(N...) {
  import std.typetuple;
  static if(CheckRandParams!N) {
    struct rand
    {
      static if(N.length > 0) {
	enum maxBounds = TypeTuple!N;
	alias ElementRand = rand!(N[0..$-1]);
      }
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

abstract class _esdl__ConstraintBase
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
  this(ConstraintEngine eng, string name, uint index) {
    super(eng, name, C, index);
  }

  static char[] constraintFunc(string CST) {
    import esdl.data.cstx;
    CstParser parser = CstParser(CST);
    return parser.translate();
  }
};

struct _esdl__RandGen
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
    // 	// int seed = uniform!(int)(_gen);
    // 	// t.seedRandom(seed);
    // 	// t.randomize();
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
  // These are unresolved loop variables
  RndVecLoopVar[] _loopVars;
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
  protected _esdl__ConstraintBase[] _esdl__cstsList;
  protected _esdl__ConstraintBase _esdl__cstWith;
  bool _esdl__cstWithChanged;

  // ParseTree parseList[];
  public RndVecPrim[] _esdl__randsList;
  _esdl__RandGen _esdl__rGen;
  Buddy _esdl__buddy;

  // BddDomain[] _domains;
  BddDomain* _domains;

  ConstraintEngine _parent = null;

  CstBlock _esdl__cstStatements;

  this(uint seed, size_t rnum, ConstraintEngine parent) {
    debug(NOCONSTRAINTS) {
      assert(false, "Constraint engine started");
    }
    _esdl__rGen.seed(seed);
    // _esdl__buddy = _new!Buddy(400, 400);
    _esdl__randsList.length = rnum;
    _esdl__buddy = _parent._esdl__buddy;
    _parent = parent;
    _esdl__cstStatements = new CstBlock();
  }

  this(uint seed, size_t rnum) {
    debug(NOCONSTRAINTS) {
      assert(false, "Constraint engine started");
    }
    _esdl__rGen.seed(seed);
    _esdl__buddy = new Buddy(400, 400);
    _esdl__randsList.length = rnum;
    _esdl__cstStatements = new CstBlock();
  }

  this(uint seed) {
    _esdl__rGen.seed(seed);
    _esdl__buddy = new Buddy(400, 400);
    _esdl__cstStatements = new CstBlock();
  }

  ~this() {
    import core.memory: GC;
    _esdl__cstsList.length   = 0;
    _esdl__cstWith          = null;
    _esdl__cstWithChanged  = true;
    _esdl__randsList.length = 0;

    // _domains.length  = 0;
    // GC.collect();
    _esdl__buddy.destroyBuddy();
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

  void initDomains(T)(T t) {
    uint domIndex = 0;
    int[] domList;

    _esdl__cstStatements.reset(); // start empty

    // take all the constraints -- even if disabled
    foreach(ref _esdl__ConstraintBase cst; _esdl__cstsList) {
      _esdl__cstStatements ~= cst.getCstExpr();
    }

    if(_esdl__cstWith !is null) {
      _esdl__cstStatements ~= _esdl__cstWith.getCstExpr();
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
    // import std.stdio;
    // writeln("Solving BDD for number of constraints = ", _esdl__cstsList.length);

    // if(_domains.length == 0 || _esdl__cstWithChanged is true) {
    if(_domains is null || _esdl__cstWithChanged is true) {
      initDomains(t);
    }

    CstStage[] cstStages;

    _esdl__cstStatements.reset(); // start empty

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
    // constraint expressions that depend on the LOOP Variables.

    // Once we have unrolled all the LOOPS, we go ahead and resolve
    // everything that remains.

    int stageIdx=0;

    // This variable is true when all the array lengths have been resolved
    bool allArrayLengthsResolved = false;

    // Ok before we start looking at the constraints, we create a
    // stage for each and every @rand that we have at hand
    foreach(rnd; _esdl__randsList) {
      if(rnd !is null && cast(RndVecArrVar) rnd is null &&
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
	    if(stage.hasLoops() is 0 &&
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
	BDD primBdd = vec.getPrimBdd(_esdl__buddy);
	if(! primBdd.isOne()) {
	  solveBDD = solveBDD & primBdd;
	}
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

template isRandomizable(T) { // check if T is Randomizable
  import std.traits;
  import std.range;
  import esdl.data.bvec;
  static if(isArray!T) {
    enum bool isRandomizable = isRandomizable!(ElementType!T);
  }
  else
  static if(isIntegral!T || isBitVector!T || is(T == class))
    {
      static if(is(T: _esdl__ConstraintBase)) {
	enum bool isRandomizable = false;
      }
      else {
	enum bool isRandomizable = true;
      }
    }
  else {
    bool isRandomizable = false;
  }
}

template _esdl__RandAttr(T, int N, int I)
{
  import std.typetuple;		// required for Filter
  alias U=_esdl__upcast!(T, N);
  alias _esdl__RandAttr = Filter!(_esdl__isAttrRand,
				   __traits(getAttributes, U.tupleof[I]));
}

template _esdl__isAttrRand(alias R) {
  static if(__traits(isSame, R, rand) || is(R unused: rand!M, M...)) {
    enum _esdl__isAttrRand = true;
  }
  else {
    enum _esdl__isAttrRand = false;
  }
}

// enum bool _esdl__isAttrRand(alias R) =
//   (__traits(isSame, R, rand) || is(R unused: rand!M, M...));

template _esdl__upcast(T, int N=1) {
  static if(N == 0) {
    alias _esdl__upcast=T;
  }
  else static if(is(T B == super)
		 && is(B[0] == class)) {
      alias _esdl__upcast = _esdl__upcast!(B[0], N-1);
    }
    else {
      static assert(false, "Can not upcast " ~ T.stringof);
    }
}

template _esdl__rand_type(T, int N, int I)
{
  alias U=_esdl__upcast!(T, N);
  alias _esdl__rand_type = typeof(U.tupleof[I]);
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

template _esdl__RandInits(T, int I=0)
{
  static if(I == T.tupleof.length) {
    enum _esdl__RandInits = "";
  }
  else {
    alias RAND = getRandAttr!(T, I);
    static if(! __traits(isSame, RAND, _esdl__norand)) {
      enum NAME = T.tupleof[I].stringof;
      enum _esdl__RandInits =
	"    _esdl__" ~ NAME ~ " = new typeof(_esdl__" ~ NAME ~
	")(\"" ~ NAME ~ "\", true, &(this.outer." ~ NAME ~ "));\n" ~
	"    _esdl__randsList ~= _esdl__" ~ NAME ~ ";\n" ~
	_esdl__RandInits!(T, I+1);
    }
    else {
      enum _esdl__RandInits = _esdl__RandInits!(T, I+1);
    }
  }
}


// Returns a tuple consiting of the type of the rand variable and
// also the @rand!() attribute it has been tagged with
template _esdl__RandTypeAttr(T, int I)
{
  import std.typetuple;
  alias _esdl__randAttrList = Filter!(_esdl__isAttrRand,
					__traits(getAttributes, T.tupleof[I]));
  static if(_esdl__randAttrList.length != 1) {
    static assert(false, "Expected exactly one @rand attribute on variable " ~
		  T.tupleof[I].stringof ~ " of class " ~ T.stringof ~
		  ". But found " ~ _esdl__randAttrList.length.stringof);
  }
  else {
    alias _esdl__RandTypeAttr = TypeTuple!(typeof(T.tupleof[I]),
					   _esdl__randAttrList[0]);
  }
}

template _esdl__RandDeclVars(T, int I=0)
{
  static if(I == T.tupleof.length) {
    enum _esdl__RandDeclVars = "";
  }
  else static if(__traits(isSame, getRandAttr!(T, I), _esdl__norand)) {
    enum _esdl__RandDeclVars = _esdl__RandDeclVars!(T, I+1);
    }
    else {
      enum _esdl__RandDeclVars =
	"  _esdl__Rand!(_esdl__RandTypeAttr!(_esdl__T, " ~ I.stringof ~
	")) _esdl__" ~ T.tupleof[I].stringof ~
	";\n" ~ _esdl__RandDeclVars!(T, I+1);
    }
}

template _esdl__RandDeclFuncs(T, int I=0)
{
  static if(I == T.tupleof.length) {
    enum _esdl__RandDeclFuncs = "";
  }
  else {
    alias L = typeof(T.tupleof[I]);
    enum NAME = T.tupleof[I].stringof;
    // skip the constraints and _esdl__ variables
    static if(is(L f == Constraint!C, immutable (char)[] C) ||
	      (NAME.length > 8 && NAME[0..7] == "_esdl__")) {
      enum _esdl__RandDeclFuncs = _esdl__RandDeclFuncs!(T, I+1);
    }
    else static if(__traits(isSame, getRandAttr!(T, I), _esdl__norand)) {
	enum _esdl__RandDeclFuncs =
	  "  const auto " ~ NAME ~ "() { return this.outer." ~ NAME ~ "; }\n" ~
	  _esdl__RandDeclFuncs!(T, I+1);
      }
      else {
	enum _esdl__RandDeclFuncs =
	  "  auto " ~ NAME ~ "() { return _esdl__" ~ NAME ~ "; }\n" ~
	  _esdl__RandDeclFuncs!(T, I+1);
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
    enum _esdl__CstInits = "";
  }
  else {
    alias L = typeof(T.tupleof[I]);
    enum NAME = T.tupleof[I].stringof;
    static if(is(L f == Constraint!C, immutable (char)[] C)) {
      enum _esdl__CstInits =
	"    " ~ NAME ~ " = new _esdl__Constraint!(_esdl__constraintString!(_esdl__T, "
	~ I.stringof ~ "))(\"" ~
	NAME ~ "\");\n    _esdl__cstsList ~= " ~ NAME ~
	// ";\n    this.outer." ~ NAME ~ " = " ~ NAME ~ 
	";\n" ~ _esdl__CstInits!(T, I+1);
    }
    else {
      enum _esdl__CstInits = _esdl__CstInits!(T, I+1);
    }
  }
}

// template _esdl__ConstraintsDecl(T, int I=0)
// {
//   static if(I == T.tupleof.length) {
//     enum _esdl__ConstraintsDecl = "";
//   }
//   else {
//     alias L = typeof(T.tupleof[I]);
//     enum NAME = T.tupleof[I].stringof;
//     static if(is(L f == Constraint!C, immutable (char)[] C)) {
//       enum _esdl__ConstraintsDecl =
// 	"  Constraint!(_esdl__constraintString!(_esdl__T, "
// 	~ I.stringof ~ ")) " ~ NAME ~ ";\n" ~
// 	_esdl__ConstraintsDecl!(T, I+1);
//     }
//     else {
//       enum _esdl__ConstraintsDecl = _esdl__ConstraintsDecl!(T, I+1);
//     }
//   }
// }

// template _esdl__ConstraintsDecl(CSTS...)
// {
//   static if(CSTS.length == 0) {
//     enum _esdl__ConstraintsDecl = "";
//   }
//   else {
//     enum _esdl__ConstraintsDecl =
//       "  Constraint! q{" ~ CSTS[0] ~ "} " ~ CSTS[1] ~ ";\n" ~
//       _esdl__ConstraintsDecl!(CSTS[2..$]);
//   }
// }

// generates the code for rand structure inside the class object getting
// randomized
template _esdl__ListRands(T, int I=0) {
  import std.typetuple;
  static if(I == T.tupleof.length) {
    alias _esdl__ListRands = TypeTuple!();
  }
  else {
    static if(hasRandAttr_!(T, I)) {
    // static if(hasRandAttr!(__traits(getAttributes, T.tupleof[I]))) {
      alias _esdl__ListRands = TypeTuple!(T, I, _esdl__ListRands!(T, I+1));
    }
    else {
      // alias aaaa = getRandAttr!(__traits(getAttributes, T.tupleof[I]));
      // static if(__traits(isSame, aaaa, _esdl__norand)) {
      // 	pragma(msg, getRandAttr!(__traits(getAttributes, T.tupleof[I])));
      // }
	
      alias _esdl__ListRands = _esdl__ListRands!(T, I+1);
    }
  }
}

template hasRandAttr_(T, int I=0) {
  alias hasRandAttr_ = hasRandAttr!(__traits(getAttributes, T.tupleof[I]));
}

template hasRandAttr(A...) {
  static if(A.length == 0) {
    enum bool hasRandAttr = false;
  }
  else static if(__traits(isSame, A[0], rand) ||
		 is(A[0] unused: rand!M, M...)) {
      enum bool hasRandAttr = true;
    }
    else {
      enum bool hasRandAttr = hasRandAttr!(A[1..$]);
    }
}

template getRandAttr(T, int I=0) {
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

// generates the code for rand structure inside the class object getting
// randomized
template _esdl__ListConstraints(T, int I=0) {
  import std.typetuple;
  static if(I == T.tupleof.length) {
    alias _esdl__ListConstraints = TypeTuple!();
  }
  else {
    // check for the integral members
    alias typeof(T.tupleof[I]) L;
    enum NAME = T.tupleof[I].stringof;
    static if (is (L f == Constraint!C, immutable (char)[] C)) {
      alias _esdl__ListConstraints = TypeTuple!(C, NAME, _esdl__ListConstraints!(T, I+1));
    }
    else {
      alias _esdl__ListConstraints = _esdl__ListConstraints!(T, I+1);
    }
  }
}


// Base class for the randoms
public class _esdl__SolverBase: ConstraintEngine
{
  public void _esdl__initRands() {}
  public void _esdl__initCsts() {}
  this(uint seed) {
    super(seed);
  }
}


// void _esdl__setRands(size_t I=0, size_t CI=0, T)
//   (T t, RndVecPrim[] vecVals, ref _esdl__RandGen rgen)
//   if(is(T unused: RandomizableIntf) && is(T == class)) {
//     import std.traits;
//     import esdl.data.bvec: toBitVec;
//     static if (I < t.tupleof.length) {
//       alias typeof(t.tupleof[I]) L;
//       static if (isDynamicArray!L) {
// 	enum RLENGTH = findRandArrayAttr!(I, T);
// 	static if(RLENGTH != -1) { // is @rand
// 	  // make sure that there is only one dimension passed to @rand
// 	  static assert(findRandArrayAttr!(I, T, 1) == int.min);
// 	  // enum ATTRS = __traits(getAttributes, t.tupleof[I]);
// 	  // alias ATTRS[RLENGTH] ATTR;
// 	  auto vecVal = cast(RndVecArrVar) vecVals[CI];
// 	  // if(vecVal is null) {
// 	  //   t.tupleof[I].length = rgen.gen(0, RLENGTH+1);
// 	  // }
// 	  // else {
// 	  //   t.tupleof[I].length = vecVal._arrLen.value;
// 	  // }
// 	  foreach(idx, ref v; t.tupleof[I]) {
// 	    import std.range;
// 	    if(vecVal is null || (! vecVal.built()) || vecVal[idx] is null) {
// 	      // v = rgen.gen!(ElementType!L);
// 	      rgen.gen(v);

// 	    }
// 	    // else {
// 	    //   v = cast(ElementType!L) vecVal[idx].value.toBitVec;
// 	    // }
// 	  }
// 	  // t.tupleof[I] = rgen.gen!L;
// 	  // }
// 	  // else {
// 	  //   // t.tupleof[I] = cast(L) vecVal.value;
// 	  // }

// 	  _esdl__setRands!(I+1, CI+1) (t, vecVals, rgen);
// 	}
// 	else {
// 	  _esdl__setRands!(I+1, CI+1) (t, vecVals, rgen);
// 	}
//       }
//       else {
// 	static if(findRandElemAttr!(I, T) != -1) { // is @rand
// 	  static if(isStaticArray!L) {
// 	    auto vecVal = cast(RndVecArrVar) vecVals[CI];
// 	    if(vecVal is null || ! vecVal.built()) {
// 	      foreach(idx, ref v; t.tupleof[I]) {
// 		// import std.range;
// 		// v = rgen.gen!(ElementType!L);
// 		rgen.gen(v);
// 	      }
// 	    }
// 	    else {
// 	      foreach(idx, ref v; t.tupleof[I]) {
// 		// import std.range;
// 		auto elemVal = vecVal[idx];
// 		if(elemVal is null) {
// 		  // v = rgen.gen!(ElementType!L);
// 		  rgen.gen(v);
// 		}
// 		// else {
// 		//   alias ElementType!L R;
// 		//   static if(isBitVector!R) {
// 		//     import esdl.data.bvec;
// 		//     v = cast(ElementType!L) elemVal.value.toBitVec;
// 		//   }
// 		//   else {
// 		//     v = cast(R) elemVal.value;
// 		//   }
// 		// }
// 	      }
// 	    }
// 	  }
// 	  else static if(is(L: RandomizableIntf)) {
// 	      // TODO -- random class objects
// 	      rgen.gen(t.tupleof[I]);
// 	    }
// 	    else {
// 	      auto vecVal = vecVals[CI];
// 	      if(vecVal is null || vecVal.domIndex == uint.max) {
// 		rgen.gen(t.tupleof[I]);
// 	      }
// 	      // else {
// 	      //	import esdl.data.bvec;
// 	      //	Bit!64 temp = vecVal.value;
// 	      //	t.tupleof[I] = cast(L) temp;
// 	      // }
// 	    }
// 	  _esdl__setRands!(I+1, CI+1) (t, vecVals, rgen);
// 	}
// 	else {
// 	  _esdl__setRands!(I+1, CI+1) (t, vecVals, rgen);
// 	}
//       }
//     }
//     else static if(is(T B == super)
// 		   && is(B[0] : RandomizableIntf)
// 		   && is(B[0] == class)) {
// 	B[0] b = t;
// 	_esdl__setRands!(0, CI) (b, vecVals, rgen);
//       }
//   }

template findRandElemAttr(size_t I, T) {
  enum int randAttr =
    findRandElemAttrIndexed!(0, -1, __traits(getAttributes, T.tupleof[I]));
  enum int randsAttr =
    findRandArrayAttrIndexed!(0, -1, 0, __traits(getAttributes, T.tupleof[I]));
  static assert(randsAttr == -1, "Illegal use of @rand!" ~ randsAttr.stringof);
  enum int findRandElemAttr = randAttr;
}

template findRandArrayAttr(size_t I, T, size_t R=0) {
  enum int randAttr =
    findRandElemAttrIndexed!(0, -1, __traits(getAttributes, T.tupleof[I]));
  enum int randsAttr =
    findRandArrayAttrIndexed!(0, -1, R, __traits(getAttributes, T.tupleof[I]));
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

// public bool randomizeWith(string C, T, V...)(ref T t, V values)
//   if(is(T v: RandomizableIntf) &&
//      is(T == class) && allIntengral!V) {
//     // The idea is that if the end-user has used the randomization
//     // mixin then _esdl__RandType would be already available as an
//     // alias and we can use virtual randomize method in such an
//     // eventuality.
//     // static if(is(typeof(t._esdl__RandType) == T)) {
//     if(t._esdl__solverInst._esdl__cstWith is null ||
//        t._esdl__solverInst._esdl__cstWith._constraint != C) {
//       auto withCst =
// 	new Constraint!(C, "_esdl__withCst",
// 			T, V.length)(t, "_esdl__withCst");
//       withCst.withArgs(values);
//       t._esdl__solverInst._esdl__cstWith = withCst;
//       t._esdl__solverInst._esdl__cstWithChanged = true;
//     }
//     else {
//       t._esdl__solverInst._esdl__cstWithChanged = false;
//     }
//     return t._esdl__randomize();
//   }

public bool _esdl__randomize(T) (T t, _esdl__ConstraintBase withCst = null) {
  t._esdl__initSolver();
  if(t._esdl__solverInst._esdl__cstWith !is null) {
    t._esdl__solverInst._esdl__cstWith = null;
    t._esdl__solverInst._esdl__cstWithChanged = true;
  }
  else {
    t._esdl__solverInst._esdl__cstWithChanged = false;
  }
  useBuddy(t._esdl__solverInst._esdl__buddy);

  static if(__traits(compiles, t.preRandomize())) {
    t.preRandomize();
  }

  foreach(rnd; t._esdl__solverInst._esdl__randsList) {
    rnd.reset();
  }

  t._esdl__solverInst.solve(t);
  
  // _esdl__setRands(t, t._esdl__solverInst._esdl__randsList,
  // 		  t._esdl__solverInst._esdl__rGen);

  static if(__traits(compiles, t.postRandomize())) {
    t.postRandomize();
  }
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

  RndVecPrim[] _arrVars;

  public RndVecPrim[] arrVars() {
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
  string _name;
  override string name() {
    return _name;
  }

  public this(string name) {
    _name = name;
  }

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

  public long getLen() {
    assert(false, "arrLen is available only for RndVecArr type");
  }
  public void setLen(long len) {
    assert(false, "arrLen is available only for RndVecArr type");
  }

  public RndVecArrLen arrLen() {
    assert(false, "arrLen is available only for RndVecArr type");
  }
  public void build() {
    assert(false, "build is available only for RndVecArr type");
  }
  
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

abstract class FxdVecVar: RndVecPrim
{
  // BddVec _bddvec;
  // uint _domIndex = uint.max;
  // CstStage _stage = null;
  // bool _isRand;

  override string name() {
    return _name;
  }

  public this(string name) {
    super(name);
  }

  override public RndVecPrim[] getPrims() {
    RndVecPrim[] _prims;
    // if(isRand) _prims = [this];
    return _prims;
  }

  override public CstStage[] getStages() {
    CstStage[] stages;
    // if(isRand) stages = [this.stage()];
    return stages;
  }

  override public BddVec getBDD(CstStage stage, Buddy buddy) {
    // if(this.isRand && stage is _stage) {
    //   return _bddvec;
    // }
    // else if((! this.isRand) ||
    // 	    this.isRand && _stage.solved()) { // work with the value
    return buddy.buildVec(value());
    // }
    // else {
    //   assert(false, "Constraint evaluation in wrong stage");
    // }
  }

  override public long evaluate() {
    // if(! this.isRand || _stage.solved()) {
      return value();
    // }
    // else {
    //   import std.conv;
    //   assert(false, "Rand variable " ~ _name[2..$] ~ " evaluation in wrong stage: " ~ _stage._id.to!string);
    // }
  }

  override public bool isRand() {
    return false;
  }

  override public CstStage stage() {
    return null;
  }

  // override public void stage(CstStage s) {
  //   // _stage = s;
  // }

  // override public uint domIndex() {
  //   return -1;
  // }

  // override public void domIndex(uint s) {
  //   // _domIndex = s;
  // }

  // override public BddVec bddvec() {
  //   BddVec _bddvec;
  //   return _bddvec;
  // }

  // override public void bddvec(BddVec b) {
  //   // _bddvec = b;
  // }

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

class RndVecArrLen: RndVecPrim
{

  // This bdd has the constraint on the max length of the array
  BDD _primBdd;
  size_t _maxArrLen;
  RndVecLoopVar _loopVar;

  RndVecPrim _parent;

  BddVec _bddvec;
  uint _domIndex = uint.max;
  CstStage _stage = null;
  bool _isRand;

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

  public this(string name, long maxArrLen, bool isRand, RndVecPrim parent) {
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

template _esdl__Rand(T, alias R)
{
  import std.traits;
  static if(isArray!T) {
    alias _esdl__Rand=RndVecArr!(T, R);
  }
  else {
    alias _esdl__Rand=RndVec!(T, R);
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
  
// T represents the type of the declared array/non-array member
// N represents the level of the array-elements we have to traverse
// for the given element
class RndVec(T, alias R, int N=0): RndVecPrim
{
  import esdl.data.bvec;
  alias L=ElementTypeN!(T, N);
  mixin EnumConstraints!L;

  BddVec _bddvec;
  uint _domIndex = uint.max;
  CstStage _stage = null;
  bool _isRand;

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
    L* _var;
    public this(string name, bool isRand, L* var) {
      super(_name);
      _isRand = isRand;
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
    alias P = RndVecArr!(T, R, N-1);
    P _parent;
    ulong _index;

    public this(string name, bool isRand, P parent,
		ulong index) {
      super(_name);
      _isRand = isRand;
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

class RndVecArr(T, alias R, int N=0): RndVecArrVar
{
  import std.traits;
  import std.range;

  static if(N == 0) {
    alias L=T;
    L* _var;

    static if(__traits(isSame, R, rand)) {
      static assert(isStaticArray!T);
      public this(string name, bool elemIsRand, L* var) {
	T t;
	size_t maxLen = T.length;
	super(name, maxLen, false, elemIsRand);
	_var = var;
	_arrLen = new RndVecArrLen(name, maxLen, false, this);
      }
    }

    static if(is(R: rand!M, M...)) {
      enum int maxLen = M[0];
      public this(string name, bool elemIsRand, L* var) {
	super(name, maxLen, true, elemIsRand);
	_var = var;
	_arrLen = new RndVecArrLen(name, maxLen, true, this);
      }
    }

    auto elements() {
      this.build();
      auto idx = arrLen.makeLoopVar();
      return this[idx];
    }
    auto iterator() {
      this.build();
      auto idx = arrLen.makeLoopVar();
      return idx;
    }
      
    // public this(string name, long maxArrLen,
    // 		bool isRand, bool elemIsRand, L* var) {
    //   super(name, maxArrLen, isRand, elemIsRand);
    //   _var = var;
    //   _arrLen = new RndVecArrLen(name, maxArrLen, isRand, this);
    // }
    override bool built() {
      return _elems.length != 0;
    }
    override void build() {
      alias ElementType!L E;
      static assert(isIntegral!E || isBitVector!E);
      _elems.length = maxArrLen();
      // if(! built()) {
      for (size_t i=0; i!=maxArrLen; ++i) {
	if(this[i] is null) {
	  import std.conv: to;
	  auto init = (E).init;
	  if(i < (*_var).length) {
	    this[i] = new RndVec!(T, R, N+1)(_name ~ "[" ~ i.to!string() ~ "]",
					true, this, i);
	  }
	  else {
	    this[i] = new RndVec!(T, R, N+1)(_name ~ "[" ~ i.to!string() ~ "]",
					true, this, i);
	  }
	  assert(this[i] !is null);
	}
      }
      // }
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
    alias P=ElementTypeN!(T, N-1);
    P _parent;
    ulong _index;

    public this(string name, long maxArrLen, bool isRand, bool elemIsRand,
		P parent, ulong index) {
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

  override public RndVecPrim[] arrVars() {
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

  void opIndexAssign(RndVecPrim c, size_t idx) {
    _elems[idx] = c;
  }

  public this(string name, long maxArrLen,
	      bool isRand, bool elemIsRand) {
    // super(name, maxArrLen, signed, bitcount, isRand);
    super(_name);
    _elemIsRand = elemIsRand;
    // _elems.length = maxArrLen;
  }

  bool built();

  size_t maxArrLen() {
    return _arrLen._maxArrLen;
  }

  public RndVecArrLen length() {
    return _arrLen;
  }

  override public RndVecArrLen arrLen() {
    return _arrLen;
  }

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
  RndVecPrim _arrVar;

  RndVecPrim arrVar() {
    return _arrVar;
  }

  uint maxVal() {
    if(! this.isUnrollable()) {
      assert(false, "Can not find maxVal since the "
	     "Loop Variable is unrollable");
    }
    return cast(uint) arrVar.arrLen.value;
  }

  override RndVecLoopVar[] loopVars() {
    return [this];
  }

  // this will not return the arrVar since the length variable is
  // not getting constraint here
  override RndVecPrim[] arrVars() {
    return [];
  }

  this(RndVecPrim arrVar) {
    super("loopVar");
    _arrVar = arrVar;
    arrVar.arrLen.loopVar(this);
  }

  bool isUnrollable(RndVecPrim arrVar) {
    if(arrVar is _arrVar) {
      return true;
    }
    else {
      return false;
    }
  }

  bool isUnrollable() {
    if(! _arrVar.arrLen.isRand()) return true;
    if(_arrVar.arrLen.stage !is null &&
       _arrVar.arrLen.stage.solved()) return true;
    else return false;
  }

  // get all the primary bdd vectors that constitute a given bdd expression
  override public RndVecPrim[] getPrims() {
    return arrVar.arrLen.getPrims();
  }

  // get the list of stages this expression should be avaluated in
  override public CstStage[] getStages() {
    return arrVar.arrLen.getStages();
  }

  override public BddVec getBDD(CstStage stage, Buddy buddy) {
    assert(false, "Can not getBDD for a Loop Variable without unrolling");
  }

  override public long evaluate() {
    assert(false, "Can not evaluate for a Loop Variable without unrolling");
  }

  override public bool isRand() {
    return arrVar.arrLen.isRand();
  }
  override public long value() {
    return arrVar.arrLen.value();
  }
  override public void value(long v) {
    arrVar.arrLen.value(v);
  }
  override public CstStage stage() {
    return arrVar.arrLen.stage();
  }
  override public void stage(CstStage s) {
    arrVar.arrLen.stage(s);
  }
  override public uint domIndex() {
    return arrVar.arrLen.domIndex;
  }
  override public void domIndex(uint s) {
    arrVar.arrLen.domIndex(s);
  }
  override public uint bitcount() {
    return arrVar.arrLen.bitcount();
  }
  override public bool signed() {
    return arrVar.arrLen.signed();
  }
  override public BddVec bddvec() {
    return arrVar.arrLen.bddvec();
  }
  override public void bddvec(BddVec b) {
    arrVar.bddvec(b);
  }
  override public string name() {
    return arrVar.arrLen.name();
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

  override public RndVecPrim[] arrVars() {
    RndVecPrim[] arrs;
    foreach(elem; _elems) {
      arrs ~= elem.arrVars();
    }
    return arrs;
  }

  public this(string name) {
    super(_name);
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

  public this(long value, bool signed) {
    super(value.to!string());
    _value = value;
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
      // FIXME -- what if the LOOPINDEX is used with non-rand array?
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

  RndVecPrim[] _arrVars;

  public RndVecPrim[] arrVars() {
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
  bool[] _booleans;

  override public string name() {
    string name_ = "";
    foreach(expr; _exprs) {
      name_ ~= " & " ~ expr.name() ~ "\n";
    }
    return name_;
  }

  public void reset() {
    _exprs.length = 0;
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
      foreach(expr; other._exprs) {
	_exprs ~= expr;
      }
      foreach(boolean; other._booleans) {
	_booleans ~= boolean;
      }
    }
}
