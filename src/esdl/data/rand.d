// Written in the D programming language.

// Copyright: Coverify Systems Technology 2012 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

module esdl.data.rand;

import esdl.data.obdd;

import std.ascii: whitespace;
import std.traits: isSomeString;
import std.traits: isIntegral;
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
  this(ConstraintEngine eng, string name, uint index) {
    _cstEng = eng;
    _name = name;
    _index = index;
  }

  protected bool _enabled = true;
  protected ConstraintEngine _cstEng;
  protected string _name;
  // index in the constraint Database
  protected uint _index;

  public bool isEnabled() {
    return _enabled;
  }

  public void enable() {
    _enabled = false;
  }

  public void disable() {
    _enabled = true;
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
    super(eng, name, index);
  }

  static immutable string _constraint = C;
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
  T _outer;
  S _outerD;

  this(T t, S s, ConstraintEngine eng, string name, uint index) {
    super(eng, name, index);
    _outer = t;
    _outerD = s;
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

  private bvec!32 _bv;

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
    static if(isIntegral!T) {
      T result = uniform!(T)(_gen);
      return result;
    }
    else static if(isBitVector!T) {
	T result;
	result.randomize(_gen);
	return result;
      }
      else {
	static assert(false);
      }
  }

  @property public auto gen(T1, T2)(T1 a, T2 b)
    if(isIntegral!T1 && isIntegral!T2) {
      return uniform(a, b, _gen);
    }
}

// Later we will use freelist to allocate CstStage
class CstStage {
  int _id = -1;
  // List of randomized variables associated with this stage. Each
  // variable can be associated with only one stage
  CstVecPrim[] _randVecs;
  CstBddExpr[] _bddExprs;
  // These are unresolved loop variables
  CstVecLoopVar[] _loopVars;
  // These are the length variables that this stage will solve
  CstVecRandArr[] _lengthVars;

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

  public bool hasLoops() {
    foreach(loop; _loopVars) {
      if(! loop.isUnrollable()) return true;
    }
    return false;
  }

}

public class ConstraintEngine {
  // Keep a list of constraints in the class
  _ESDL__ConstraintBase cstList[];
  // ParseTree parseList[];
  public CstVecPrim[] _cstRands;
  RandGen _rgen;
  Buddy _buddy;
  
  BddDomain[] _domains;

  this(uint seed, size_t rnum) {
    _rgen.seed(seed);
    _buddy = _new!Buddy(400, 400);
    _cstRands.length = rnum;
  }

  ~this() {
    import core.memory: GC;
    cstList.length = 0;
    _cstRands.length = 0;
    _domains.length = 0;
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
	  stage._randVecs ~= vec;
	  // cstStages[stage]._randVecs ~= vec;
	}
	if(stage !is vec.stage()) { // need to merge stages
	  mergeCstStages(stage, vec.stage(), cstStages);
	  stage = vec.stage();
	}
      }
    }
    stage._bddExprs ~= expr;
    stage._lengthVars ~= expr.lengthVars();
  }

  public void mergeCstStages(CstStage fromStage, CstStage toStage,
			     ref CstStage[] cstStages) {
    if(fromStage is null) {
      // fromStage has not been created yet
      return;
    }
    foreach(ref vec; fromStage._randVecs) {
      vec.stage = toStage;
    }
    toStage._randVecs ~= fromStage._randVecs;
    toStage._bddExprs ~= fromStage._bddExprs;
    if(cstStages[$-1] is fromStage) {
      cstStages.length -= 1;
    }
    else {
      fromStage._randVecs.length = 0;
      fromStage._bddExprs.length = 0;
    }
  }

  void initDomains() {
    uint domIndex = 0;
    int[] domList;
    auto cstStmts = new CstBlock();	// start empty

    // take all the constraints -- even if disabled
    foreach(ref _ESDL__ConstraintBase cst; cstList) {
      cstStmts ~= cst.getCstExpr();
    }

    foreach(stmt; cstStmts._exprs) {
      foreach(vec; stmt.getPrims()) {
	if(vec.domIndex == uint.max) {
	  vec.domIndex = domIndex++;
	  domList ~= vec.bitcount;
	}
      }
    }

    _buddy.clearAllDomains();
    _domains = _buddy.extDomain(domList);

  }

  void solve() {
    // import std.stdio;
    // writeln("Solving BDD for number of contraints = ", cstList.length);

    if(_domains.length is 0) {
      initDomains();
    }

    auto cstStmts = new CstBlock();	// start empty

    CstStage[] cstStages;

    foreach(ref _ESDL__ConstraintBase cst; cstList) {
      if(cst.isEnabled()) {
	cstStmts ~= cst.getCstExpr();
      }
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

    while(usExprs.length > 0 || usStages.length > 0) {
      cstExprs = usExprs;
      usExprs.length = 0;
      auto urExprs = usExprs;	// unrolled expressions
      cstStages = usStages;
      usStages.length = 0;


      if(! allArrayLengthsResolved) {
	allArrayLengthsResolved = true;
	foreach(expr; urExprs) {
	  if(expr._lengthVars.length !is 0) {
	    allArrayLengthsResolved = false;
	  }
	}
	foreach(stage; cstStages) {
	  if(stage._lengthVars.length !is 0) {
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
	   stage._randVecs.length !is 0) {
	  if(allArrayLengthsResolved) {
	    solveStage(stage, stageIdx);
	  }
	  // resolve allArrayLengthsResolved
	  else {
	    allArrayLengthsResolved = true;
	    if(stage.hasLoops() is 0 &&
	       stage._lengthVars.length !is 0) {
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

    foreach(vec; stage._randVecs) {
      if(vec.stage is stage) {
	if(vec.bddvec is null) {
	  vec.bddvec = _buddy.buildVec(_domains[vec.domIndex], vec.signed);
	}
	BDD primBdd = vec.getPrimBdd(_buddy);
	if(! primBdd.isOne()) {
	  // import std.stdio;
	  // writeln("Adding prime BDD");
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
    enforce(solVecs.length == 1,
	    "Expecting exactly one solutions here; got: " ~
	    to!string(solVecs.length));

    auto bits = solVecs[0];

    foreach(vec; stage._randVecs) {
      vec.value = 0;	// init
      foreach(uint i, ref j; solveBDD.getIndices(vec.domIndex)) {
	if(bits[j] == 1) {
	  vec.value = vec.value + (1L << i);
	}
	if(bits[j] == -1) {
	  vec.value = vec.value + ((cast(ulong) _rgen.flip()) << i);
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

// Need to change this function to return only the count of @rand members
public size_t _esdl__countRands(size_t I=0, size_t C=0, T)(T t)
  if(is(T unused: RandomizableIntf)) {
    static if(I == t.tupleof.length) {
      static if(is(T B == super)
		&& is(B[0] : RandomizableIntf)
		&& is(B[0] == class)) {
	B[0] b = t;
	return _esdl__countRands!(0, C)(b);
      }
      else {
	return C;
      }
    }
    else {
      import std.traits;
      import std.range;
      // check for the integral members
      alias typeof(t.tupleof[I]) L;
      static if((isIntegral!L || isBitVector!L) &&
		findRandElemAttr!(I, t) != -1) {
	return _esdl__countRands!(I+1, C+1)(t);
      }
      else static if(isStaticArray!L && (isIntegral!(ElementType!L) ||
					 isBitVector!(ElementType!L)) &&
		     findRandElemAttr!(I, t) != -1) {
	  return _esdl__countRands!(I+1, C+1)(t);
	}
      else static if(isDynamicArray!L && (isIntegral!(ElementType!L) ||
					  isBitVector!(ElementType!L)) &&
		     findRandArrayAttr!(I, t) != -1) {
	  return _esdl__countRands!(I+1, C+1)(t);
	}
      // ToDo -- Fixme -- Add code for array randomization here
	else {
	  return _esdl__countRands!(I+1, C)(t);
	}
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
      override public bool _esdl__virtualRandomize() {
	return _esdl__randomize!_esdl__RandType(this);
      }
    };
    return _esdl__vRand;
  }

  static final string _esdl__randomizable() {
    return q{

      alias typeof(this) _esdl__RandType;
      public _esdl__RandType _esdl__typeID() {
	return null;
      }

      public bool _esdl__virtualRandomize() {
	return _esdl__randomize!_esdl__RandType(this);
      }

      public ConstraintEngine _esdl__cstEng;
      public uint _esdl__randSeed;

      public void seedRandom (int seed) {
	_esdl__randSeed = seed;
	if (_esdl__cstEng !is null) {
	  _esdl__cstEng._rgen.seed(seed);
	}
      }
      alias seedRandom srandom;	// for sake of SV like names

      public ConstraintEngine getCstEngine() {
	return _esdl__cstEng;
      }

      void pre_randomize() {}
      void post_randomize() {}
    };
  }

  ConstraintEngine getCstEngine();
  void pre_randomize();
  void post_randomize();
}

class Randomizable: RandomizableIntf
{
  mixin(_esdl__randomizable());
}

T _new(T, Args...) (Args args) {
  version(NO_EMPLACE) {
    T obj = new T(args);
  }
  else {
    import std.stdio, std.conv, core.memory;
    size_t objSize = __traits(classInstanceSize, T);
    void* tmp = GC.malloc(objSize);
    if (!tmp) throw new Exception("Memory allocation failed");
    void[] mem = tmp[0..objSize];
    T obj = emplace!(T, Args)(mem, args);
  }
  return obj;
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
  enum string NAME = chompPrefix (t.tupleof[I].stringof, "t.");
  static if (is (L f == Constraint!C, immutable (char)[] C)) {
    l = new Constraint!(C, NAME, T, S)(t, s, t._esdl__cstEng, NAME,
				       cast(uint) t._esdl__cstEng.cstList.length);
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

void _esdl__setRands(size_t I=0, size_t CI=0, size_t RI=0, T)
  (T t, CstVecPrim[] vecVals, ref RandGen rgen)
  if(is(T unused: RandomizableIntf) && is(T == class)) {
    import std.traits;
    static if (I < t.tupleof.length) {
      alias typeof(t.tupleof[I]) L;
      static if (isDynamicArray!L) {
	enum RLENGTH = findRandArrayAttr!(I, t);
	static if(RLENGTH != -1) { // is @rand
	  // make sure that there is only one dimension passed to @rand
	  static assert(findRandArrayAttr!(I, t, 1) == int.min);
	  // enum ATTRS = __traits(getAttributes, t.tupleof[I]);
	  // alias ATTRS[RLENGTH] ATTR;
	  auto vecVal = cast(CstVecRandArr) vecVals[RI];
	  if(vecVal is null) {
	    t.tupleof[I].length = rgen.gen(0, RLENGTH+1);
	  }
	  else {
	    t.tupleof[I].length = vecVal.value;
	  }
	  foreach(idx, ref v; t.tupleof[I]) {
	    import std.range;
	    if(vecVal is null || vecVal[idx] is null) {
	      v = rgen.gen!(ElementType!L);
	    }
	    else {
	      v = cast(ElementType!L) vecVal[idx].value;
	    }
	  }
	  // t.tupleof[I] = rgen.gen!L;
	  // }
	  // else {
	  //   // t.tupleof[I] = cast(L) vecVal.value;
	  // }

	  _esdl__setRands!(I+1, CI+1, RI+1) (t, vecVals, rgen);
	}
	else {
	  _esdl__setRands!(I+1, CI+1, RI) (t, vecVals, rgen);
	}
      }
      else {
	static if(findRandElemAttr!(I, t) != -1) { // is @rand
	  static if(isStaticArray!L) {
	    auto vecVal = cast(CstVecRandArr) vecVals[RI];
	    if(vecVal is null) {
	      foreach(idx, ref v; t.tupleof[I]) {
		import std.range;
		v = rgen.gen!(ElementType!L);
	      }
	    }
	    else {
	      foreach(idx, ref v; t.tupleof[I]) {
		import std.range;
		auto elemVal = vecVal[idx];
		if(elemVal is null) {
		  v = rgen.gen!(ElementType!L);
		}
		else {
		  alias ElementType!L R;
		  static if(isBitVector!R) {
		    import esdl.data.bvec;
		    bvec!64 bval = elemVal.value;
		    v = cast(ElementType!L) bval;
		  }
		  else {
		    v = cast(R) elemVal.value;
		  }
		}
	      }
	    }
	  }
	  else {
	    auto vecVal = vecVals[RI];
	    if(vecVal is null) {
	      t.tupleof[I] = rgen.gen!L;
	    }
	    else {
	      import esdl.data.bvec;
	      bvec!64 temp = vecVal.value;
	      t.tupleof[I] = cast(L) temp;
	    }
	  }
	  _esdl__setRands!(I+1, CI+1, RI+1) (t, vecVals, rgen);
	}
	else {
	  _esdl__setRands!(I+1, CI+1, RI) (t, vecVals, rgen);
	}
      }
    }
    else static if(is(T B == super)
		   && is(B[0] : RandomizableIntf)
		   && is(B[0] == class)) {
	B[0] b = t;
	_esdl__setRands!(0, CI, RI) (b, vecVals, rgen);
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

public bool randomize(T) (ref T t)
  if(is(T v: RandomizableIntf) &&
     is(T == class)) {
    // The idea is that if the end-user has used the randomization
    // mixin then _esdl__RandType would be already available as an
    // alias and we can use virtual randomize method in such an
    // eventuality.
    // static if(is(typeof(t._esdl__RandType) == T)) {
    static if(is(typeof(t._esdl__typeID()) == T)) {
      return t._esdl__virtualRandomize();
    }
    else {
      return _esdl__randomize(t);
    }
  }

public bool _esdl__randomize(T) (ref T t)
  if(is(T v: RandomizableIntf) &&
     is(T == class)) {
    import std.exception;
    import std.conv;

    // Initialize the constraint database if not already done
    if (t._esdl__cstEng is null) {
      t._esdl__cstEng = new ConstraintEngine(t._esdl__randSeed,
					     _esdl__countRands(t));
      _esdl__initCsts(t, t);
    }

    // Call the pre_randomize hook
    t.pre_randomize();

    auto values = t._esdl__cstEng._cstRands;

    foreach(rnd; values) {
      if(rnd !is null) {
	// stages would be assigned again from scratch
	rnd.reset();
	// FIXME -- Perhaps some other fields too need to be reinitialized
      }
    }

    t._esdl__cstEng.solve();

    _esdl__setRands(t, values, t._esdl__cstEng._rgen);

    // Call the post_randomize hook
    t.post_randomize();
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
abstract class CstVecExpr
{

  CstVecLoopVar[] _loopVars;

  public CstVecLoopVar[] loopVars() {
    return _loopVars;
  }

  CstVecRandArr[] _lengthVars;

  public CstVecRandArr[] lengthVars() {
    return _lengthVars;
  }

  // get all the primary bdd vectors that constitute a given bdd expression
  abstract public CstVecPrim[] getPrims();

  // get the list of stages this expression should be avaluated in
  abstract public CstStage[] getStages();

  abstract public BddVec getBDD(CstStage stage, Buddy buddy);

  abstract public long evaluate();

  abstract public CstVecExpr unroll(CstVecLoopVar l, uint n);

  public CstVec2VecExpr opBinary(string op)(CstVecExpr other)
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
    static if(op == "<<") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.LSH);
    }
    static if(op == ">>") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.RSH);
    }
  }

  public CstVec2VecExpr opIndex(CstVecExpr other)
  {
    assert(false, "Index operation defined only for Arrays");
  }

  public CstVecRand opIndex(size_t other)
  {
    assert(false, "Index operation defined only for Arrays");
  }

  public CstVec2BddExpr lth(CstVecExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.LTH);
  }

  public CstVec2BddExpr lte(CstVecExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.LTE);
  }

  public CstVec2BddExpr gth(CstVecExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.GTH);
  }

  public CstVec2BddExpr gte(CstVecExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.GTE);
  }

  public CstVec2BddExpr equ(CstVecExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.EQU);
  }

  public CstVec2BddExpr neq(CstVecExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.NEQ);
  }
}

class CstVecPrim: CstVecExpr
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
  abstract public string name();
  public void loopVar(CstVecLoopVar var) {
    assert(false, "loopVar may only be called for a CstVecRandArr");
  }
  public CstVecLoopVar loopVar() {
    assert(false, "loopVar may only be called for a CstVecRandArr");
  }
  public CstVecLoopVar makeLoopVar() {
    assert(false, "makeLoopVar may only be called for a CstVecRandArr");
  }
  // this method is used for getting implicit constraints that are required for
  // dynamic arrays and for enums
  public BDD getPrimBdd(Buddy buddy) {
    return buddy.one();
  }
  override public CstVecPrim unroll(CstVecLoopVar l, uint n) {
    return this;
  }
}

// This class represents an unrolled Foreach loop at vec level
class CstVecLoopVar: CstVecPrim
{
  // _loopVar will point to the array this CstVecLoopVar is tied to
  CstVecPrim _lengthVar;

  CstVecPrim lengthVar() {
    return _lengthVar;
  }

  uint maxVal() {
    if(! this.isUnrollable()) {
      assert(false, "Can not find maxVal since the "
	     "Loop Variable is unrollable");
    }
    return cast(uint) lengthVar().value;
  }
  
  override CstVecLoopVar[] loopVars() {
    return [this];
  }

  // this will not return the lengthVar since the length variable is
  // not getting constraint here
  override CstVecRandArr[] lengthVars() {
    return [];
  }

  this(CstVecPrim lengthVar) {
    _lengthVar = lengthVar;
    lengthVar.loopVar(this);
  }

  bool isUnrollable(CstVecRand lengthVar) {
    if(lengthVar is _lengthVar) {
      return true;
    }
    else {
      return false;
    }
  }

  bool isUnrollable() {
    if(! _lengthVar.isRand()) return true;
    if(_lengthVar.stage !is null &&
       _lengthVar.stage.solved()) return true;
    else return false;
  }

  // get all the primary bdd vectors that constitute a given bdd expression
  override public CstVecPrim[] getPrims() {
    return lengthVar.getPrims();
  }

  // get the list of stages this expression should be avaluated in
  override public CstStage[] getStages() {
    return lengthVar.getStages();
  }

  override public BddVec getBDD(CstStage stage, Buddy buddy) {
    assert(false, "Can not getBDD for a Loop Variable without unrolling");
  }

  override public long evaluate() {
    assert(false, "Can not evaluate for a Loop Variable without unrolling");
  }

  override public bool isRand() {
    return lengthVar.isRand();
  }
  override public long value() {
    return lengthVar.value();
  }
  override public void value(long v) {
    lengthVar.value(v);
  }
  override public CstStage stage() {
    return lengthVar.stage();
  }
  override public void stage(CstStage s) {
    lengthVar.stage(s);
  }
  override public uint domIndex() {
    return lengthVar.domIndex;
  }
  override public void domIndex(uint s) {
    lengthVar.domIndex(s);
  }
  override public uint bitcount() {
    return lengthVar.bitcount();
  }
  override public bool signed() {
    return lengthVar.signed();
  }
  override public BddVec bddvec() {
    return lengthVar.bddvec();
  }
  override public void bddvec(BddVec b) {
    lengthVar.bddvec(b);
  }
  override public string name() {
    return lengthVar.name();
  }
  override public CstVecPrim unroll(CstVecLoopVar l, uint n) {
    if(this !is l) return this;
    else return new CstVecConst(n, false);
  }
}

class CstVecRand: CstVecPrim
{
  BddVec _bddvec;
  uint _domIndex = uint.max;
  long _value;
  uint _bitcount;
  CstStage _stage = null;
  bool _signed;
  bool _isRand;
  string _name;

  override string name() {
    return _name;
  }

  public this(string name, long value, bool signed,
	      uint bitcount, bool isRand) {
    static uint id;
    _name = name;
    _value = value;
    _signed = signed;
    _bitcount = bitcount;
    _isRand = isRand;
  }

  override public CstVecPrim[] getPrims() {
    CstVecPrim[] _prims;
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
      return buddy.buildVec(_value);
    }
    else {
      assert(false, "Constraint evaluation in wrong stage");
    }
  }

  override public long evaluate() {
    if(! this.isRand || _stage.solved()) {
      return _value;
    }
    else {
      assert(false, "Constraint evaluation in wrong stage");
    }
  }

  override public bool isRand() {
    return _isRand;
  }

  override public long value() {
    return _value;
  }

  override public void value(long v) {
    _value = v;
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

  override public uint bitcount() {
    return _bitcount;
  }

  override public bool signed() {
    return _signed;
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
	return "RAND-" ~ "#" ~ _name ~ ":" ~ _value.to!string();
      }
      else {
	return "VAL#" ~ _name ~ ":" ~ _value.to!string();
      }
    }

  override public string toString() {
    return this.to!string();
  }

}

// T represents the type of the Enum
class CstVecRandEnum(T):
  CstVecRand if(is(T == enum))
  {
    bdd _primBdd;
    override public BDD getPrimBdd(Buddy buddy) {
      // return this.bddvec.lte(buddy.buildVec(_maxValue));
      import std.traits;
      if(! _primBdd.isInitialized()) {
	_primBdd = buddy.zero();
	foreach(e; EnumMembers!T) {
	  _primBdd = _primBdd | this.bddvec.equ(buddy.buildVec(e));
	}
      }
      return _primBdd;
    }

    public this(string name, long value, bool signed,
		uint bitcount, bool isRand) {
      super(name, value, signed, bitcount, isRand);
    }
};

class CstVecRandArr: CstVecRand
{
  // Base class object shall be used for constraining the length part
  // of the array.

  // Also has an array of CstVecRand to map all the elements of the
  // array
  CstVecRand[] _elems;
  bool _elemSigned;
  uint _elemBitcount;
  bool _elemIsRand;

  size_t _maxValue = 0;
  CstVecLoopVar _loopVar;

  // This bdd has the constraint on the max length of the array
  bdd _primBdd;
  override public BDD getPrimBdd(Buddy buddy) {
    if(! _primBdd.isInitialized()) {
      _primBdd = this.bddvec.lte(buddy.buildVec(_maxValue));
    }
    return _primBdd;
  }

  override public void reset() {
    stage = null;
    foreach(elem; _elems) {
      if(elem !is null) {
	elem.reset();
      }
    }
  }

  size_t maxValue() {
    return _maxValue;
  }

  public CstVecPrim[] getArrPrims() {
    CstVecPrim[] elems;
    foreach (elem; _elems) {
      elems ~= elem;
    }
    return elems;
  }
  
  override public void loopVar(CstVecLoopVar var) {
    _loopVar = loopVar;
  }

  override public CstVecLoopVar loopVar() {
    return _loopVar;
  }

  override public CstVecLoopVar makeLoopVar() {
    if(_loopVar is null) {
      _loopVar = new CstVecLoopVar(this);
    }
    return _loopVar;
  }

  override public CstVecRandArr[] lengthVars() {
    if(isRand()) return [this];
    else return [];
  }

  bool isUnrollable() {
    if(! isRand) return true;
    if(this.stage.solved()) return true;
    else return false;
  }

  override public CstVec2VecExpr opIndex(CstVecExpr idx) {
    return new CstVec2VecExpr(this, idx, CstBinVecOp.LOOPINDEX);
  }

  override public CstVecRand opIndex(size_t idx) {
    return _elems[idx];
  }

  void opIndexAssign(CstVecRand c, size_t idx) {
    _elems[idx] = c;
  }

  public this(string name, long value,
	      bool signed, uint bitcount, bool isRand,
	      bool elemSigned, uint elemBitcount, bool elemIsRand) {
    super(name, value, signed, bitcount, isRand);
    static uint id;
    _name = name;
    _value = value;
    _maxValue = value;
    _signed = signed;
    _bitcount = bitcount;
    _isRand = isRand;
    _elemSigned = elemSigned;
    _elemBitcount = elemBitcount;
    _elemIsRand = elemIsRand;
    _elems.length = value;
  }
}

class CstVecConst: CstVecPrim
{
  long _value;			// the value of the constant
  bool _signed;

  public this(long value, bool signed) {
    _value = value;
    _signed = signed;
  }

  override public CstVecPrim[] getPrims() {
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
    assert(false, "no stage for CstVecConst");
  }

  override public void stage(CstStage s) {
    assert(false, "no stage for CstVecConst");
  }

  override public uint domIndex() {
    assert(false, "no domIndex for CstVecConst");
  }

  override public void domIndex(uint s) {
    assert(false, "no domIndex for CstVecConst");
  }

  override public uint bitcount() {
    assert(false, "no bitcount for CstVecConst");
  }

  override public bool signed() {
    return _signed;
  }

  override public BddVec bddvec() {
    assert(false, "no bddvec for CstVecConst");
  }

  override public void bddvec(BddVec b) {
    assert(false, "no bddvec for CstVecConst");
  }

  override public string name() {
    return "CstVecConst";
  }
}

// This class would hold two(bin) vector nodes and produces a vector
// only after processing those two nodes
class CstVec2VecExpr: CstVecExpr
{
  CstVecExpr _lhs;
  CstVecExpr _rhs;
  CstBinVecOp _op;

  override public CstVecPrim[] getPrims() {
    if(_op !is CstBinVecOp.LOOPINDEX) {
      return _lhs.getPrims() ~ _rhs.getPrims();
    }
    else {
      // LOOP
      // first make sure that the _lhs is an array
      auto lhs = cast(CstVecRandArr) _lhs;
      // FIXME -- what if the LOOPINDEX is use with non-rand array?
      assert(lhs !is null, "LOOPINDEX can not work with non-arrays");
      if(_rhs.loopVars.length is 0) {
	return [lhs[_rhs.evaluate()]];
      }
      else {
	return lhs.getArrPrims();
      }
    }
  }

  override public CstStage[] getStages() {
    import std.exception;
    import std.algorithm: max;

    enforce(_lhs.getStages.length <= 1 &&
	    _rhs.getStages.length <= 1);

    if(_lhs.getStages.length is 0) return _rhs.getStages;
    else if(_rhs.getStages.length is 0) return _lhs.getStages;
    else {
      // Stages need to be merged
      // uint stage = max(_lhs.getStages[0], _rhs.getStages[0]);
      // return [stage];
      return _lhs.getStages;
    }
  }

  override public BddVec getBDD(CstStage stage, Buddy buddy) {
    if(this.loopVars.length !is 0) {
      assert(false,
	     "CstVec2VecExpr: Need to unroll the loopVars"
	     " before attempting to solve BDD");
    }
    BddVec vec;

    auto lvec = _lhs.getBDD(stage, buddy);
    auto rvec = _rhs.getBDD(stage, buddy);

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
    case CstBinVecOp.LOOPINDEX: return _lhs[_rhs.evaluate()].getBDD(stage, buddy);
    case CstBinVecOp.BITINDEX: {
      assert(false, "BITINDEX is not implemented yet!");
    }
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
    case CstBinVecOp.BITINDEX: {
      assert(false, "BITINDEX is not implemented yet!");
    }
    }
  }

  override public CstVec2VecExpr unroll(CstVecLoopVar l, uint n) {
    bool loop = false;
    foreach(loopVar; loopVars()) {
      if(l is loopVar) {
	loop = true;
	break;
      }
    }
    if(! loop) return this;
    else {
      return new CstVec2VecExpr(_lhs.unroll(l, n), _rhs.unroll(l, n), _op);
    }
  }

  public this(CstVecExpr lhs, CstVecExpr rhs, CstBinVecOp op) {
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
    foreach(lengthVar; lhs.lengthVars ~ rhs.lengthVars) {
      if(op !is CstBinVecOp.LOOPINDEX) {
	bool add = true;
	foreach(l; _lengthVars) {
	  if(l is lengthVar) add = false;
	  break;
	}
	if(add) _lengthVars ~= lengthVar;
      }
    }
  }

}

class CstNotVecExpr: CstVecExpr
{
}

enum CstBddOp: byte
  {   AND,
      OR ,
      IMP,
      }

abstract class CstBddExpr
{

  // In case this expr is unRolled, the _loopVars here would be empty
  CstVecLoopVar[] _loopVars;

  public CstVecLoopVar[] loopVars() {
    return _loopVars;
  }

  CstVecRandArr[] _lengthVars;

  public CstVecRandArr[] lengthVars() {
    return _lengthVars;
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
  
  public CstBddExpr[] unroll(CstVecLoopVar l) {
    CstBddExpr[] retval;
    if(! l.isUnrollable()) {
      assert(false, "CstVecLoopVar is not unrollabe yet");
    }
    for (uint i = 0; i != l.maxVal(); ++i) {
      retval ~= this.unroll(l, i);
    }
    return retval;
  }

  public CstVecLoopVar unrollable() {
    foreach(loop; _loopVars) {
      if(loop.isUnrollable()) return loop;
    }
    return null;
  }

  abstract public CstBddExpr unroll(CstVecLoopVar l, uint n);
  
  abstract public CstVecPrim[] getPrims();

  abstract public CstStage[] getStages();

  abstract public BDD getBDD(CstStage stage, Buddy buddy);

  public CstBdd2BddExpr opBinary(string op)(CstBddExpr other)
  {
    static if(op == "&") {
      return new CstBdd2BddExpr(this, other, CstBddOp.AND);
    }
    static if(op == "|") {
      return new CstBdd2BddExpr(this, other, CstBddOp.OR);
    }
    static if(op == ">>") {
      return new CstBdd2BddExpr(this, other, CstBddOp.IMP);
    }
  }

  public CstNotBddExpr opUnary(string op)()
  {
    static if(op == "*") {	// "!" in cstx is translated as "*"
      return new CstNotBddExpr(this);
    }
  }

  public CstBdd2BddExpr imp(CstBddExpr other)
  {
    return new CstBdd2BddExpr(this, other, CstBddOp.IMP);
  }

}

class CstBdd2BddExpr: CstBddExpr
{
  CstBddExpr _lhs;
  CstBddExpr _rhs;
  CstBddOp _op;

  override public CstVecPrim[] getPrims() {
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
    case CstBddOp.AND: retval = lvec &  rvec; break;
    case CstBddOp.OR:  retval = lvec |  rvec; break;
    case CstBddOp.IMP: retval = lvec >> rvec; break;
    }
    return retval;
  }

  override public CstBdd2BddExpr unroll(CstVecLoopVar l, uint n) {
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
    foreach(lengthVar; lhs.lengthVars ~ rhs.lengthVars) {
      bool add = true;
      foreach(l; _lengthVars) {
	if(l is lengthVar) add = false;
	break;
      }
      if(add) _lengthVars ~= lengthVar;
    }
  }
}


class CstIteBddExpr: CstBddExpr
{
}

class CstVec2BddExpr: CstBddExpr
{
  CstVecExpr _lhs;
  CstVecExpr _rhs;
  CstBinBddOp _op;

  override public CstStage[] getStages() {
    import std.exception;
    import std.algorithm: max;
    enforce(_lhs.getStages.length <= 1 &&
	    _rhs.getStages.length <= 1);

    if(_lhs.getStages.length is 0) return _rhs.getStages;
    else if(_rhs.getStages.length is 0) return _lhs.getStages;
    else {
      // uint stage = max(_lhs.getStages[0], _rhs.getStages[0]);
      // return [stage];
      return _lhs.getStages;
    }
  }

  override public CstVecPrim[] getPrims() {
    return _lhs.getPrims() ~ _rhs.getPrims();
  }

  override public BDD getBDD(CstStage stage, Buddy buddy) {
    if(this.loopVars.length !is 0) {
      assert(false,
	     "CstVec2BddExpr: Need to unroll the loopVars"
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

  override public CstVec2BddExpr unroll(CstVecLoopVar l, uint n) {
    bool loop = false;
    foreach(loopVar; loopVars()) {
      if(l is loopVar) {
	loop = true;
	break;
      }
    }
    if(! loop) return this;
    else {
      return new CstVec2BddExpr(_lhs.unroll(l, n), _rhs.unroll(l, n), _op);
    }
  }

  public this(CstVecExpr lhs, CstVecExpr rhs, CstBinBddOp op) {
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
    foreach(lengthVar; lhs.lengthVars ~ rhs.lengthVars) {
      bool add = true;
      foreach(l; _lengthVars) {
	if(l is lengthVar) add = false;
	break;
      }
      if(add) _lengthVars ~= lengthVar;
    }
  }
}

class CstNotBddExpr: CstBddExpr
{
  CstBddExpr _expr;

  override public CstVecPrim[] getPrims() {
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

  override public CstNotBddExpr unroll(CstVecLoopVar l, uint n) {
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
    _lengthVars = expr.lengthVars;
  }
}

class CstBlock: CstBddExpr
{
  CstBddExpr _exprs[];

  override public CstVecPrim[] getPrims() {
    CstVecPrim[] prims;

    foreach(expr; _exprs) {
      prims ~= expr.getPrims();
    }

    return prims;
  }

  override public CstBlock unroll(CstVecLoopVar l, uint n) {
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

  public void opOpAssign(string op)(CstBlock other)
    if(op == "~") {
      foreach(expr; other._exprs) {
      _exprs ~= expr;
      }
    }
}

auto _esdl__randNamedApply(string VAR, alias F, size_t I=0,
			   size_t CI=0, size_t RI=0, T)(T t)
if(is(T unused: RandomizableIntf) && is(T == class)) {
  static if (I < t.tupleof.length) {
    static if ("t."~_esdl__randVar!VAR.prefix == t.tupleof[I].stringof) {
      return F!(VAR, I, CI, RI)(t);
    }
    else {
      static if(findRandAttr!(I, t)) {
	return _esdl__randNamedApply!(VAR, F, I+1, CI+1, RI+1) (t);
      }
      else {
	return _esdl__randNamedApply!(VAR, F, I+1, CI+1, RI) (t);
      }
    }
  }
  else static if(is(T B == super)
		 && is(B[0] : RandomizableIntf)
		 && is(B[0] == class)) {
      B[0] b = t;
      return _esdl__randNamedApply!(VAR, F, 0, CI, RI) (b);
    }
    else {
      static assert(false, "Can not map variable: " ~ VAR);
    }
 }

private size_t _esdl__cstDelimiter(string name) {
  foreach(i, c; name) {
    if(c is '.' || c is '[') {
      return i;
    }
  }
  return name.length;
}

public CstVecConst _esdl__cstRand(INT, T)(INT var, ref T t)
  if((isIntegral!INT || isBitVector!INT) &&
     is(T f: RandomizableIntf) && is(T == class)) {
    ulong val = var;
    return new CstVecConst(val, isVarSigned!INT);
  }

public CstVecPrim _esdl__cstRand(string VAR, T)(ref T t)
  if(is(T f: RandomizableIntf) && is(T == class)) {
    enum IDX = _esdl__cstDelimiter(VAR);
    enum LOOKUP = VAR[0..IDX];
    static if(IDX == VAR.length) {
      return _esdl__randNamedApply!(LOOKUP, _esdl__cstRand)(t);
    }
    else static if(VAR[IDX..$] == ".length") {
	return _esdl__randNamedApply!(LOOKUP, _esdl__cstRandArrLength)(t);
    }
    else static if(VAR[IDX] == '.') {
      // hierarchical constraints -- not implemented yet
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

public CstVecPrim _esdl__cstRand(string VAR, size_t I,
				size_t CI, size_t RI, T)(ref T t) {
  import std.traits: isIntegral;

  // need to know the size and sign for creating a bddvec
  alias typeof(t.tupleof[I]) L;
  static assert(isIntegral!L || isBitVector!L);

  static if(isVarSigned!L) bool signed = true;
  else                     bool signed = false;

  static if(isIntegral!L)       uint bitcount = L.sizeof * 8;
  else static if(isBitVector!L) uint bitcount = L.SIZE;
    else static assert(false, "Only numeric or bitvector expression"
		       "are allowed in constraint expressions");

  static if(findRandElemAttr!(I, t) == -1) {
    return _esdl__cstRand(t.tupleof[I], t);
  }
  else {
    auto cstVecPrim = t._esdl__cstEng._cstRands[RI];
    if(cstVecPrim is null) {
      static if(is(L == enum)) {
	cstVecPrim = new CstVecRandEnum!L(t.tupleof[I].stringof, cast(long) t.tupleof[I],
					  signed, bitcount, true);
      }
      else {
	cstVecPrim = new CstVecRand(t.tupleof[I].stringof, cast(long) t.tupleof[I],
				    signed, bitcount, true);
      }
      t._esdl__cstEng._cstRands[RI] = cstVecPrim;
    }
    return cstVecPrim;
  }
}

public CstVecPrim _esdl__cstRandElem(string VAR, T)(ref T t)
  if(is(T f: RandomizableIntf) && is(T == class)) {
    return _esdl__randNamedApply!(VAR, _esdl__cstRandElem)(t);
  }

public CstVecPrim _esdl__cstRandElem(string VAR, size_t I,
				     size_t CI, size_t RI, T)(ref T t) {
  import std.traits: isIntegral;
  import std.range: ElementType;

  static assert(isArray!L);
  // need to know the size and sign for creating a bddvec
  alias typeof(t.tupleof[I]) L;
  alias ElementType!L E;

  static assert(isIntegral!E || isBitVector!E);

  static if(isVarSigned!E) bool signed = true;
  else                     bool signed = false;

  static if(isIntegral!L)       uint bitcount = L.sizeof * 8;
  else static if(isBitVector!L) uint bitcount = L.SIZE;
    else static assert(false, "Only numeric or bitvector expression"
		       "are allowed in constraint expressions");

  static if(findRandElemAttr!(I, t) == -1) {
    // no @rand attribute -- just create the cstVecPrim and return
    auto cstVecPrim = new CstVecRand(t.tupleof[I].stringof,
				     cast(long) t.tupleof[I],
				     signed, bitcount, false);
  }
  else {
    auto cstVecPrim = t._esdl__cstEng._cstRands[RI];
    if(cstVecPrim is null) {
      static if(is(E == enum)) {
	cstVecPrim = new CstVecRandEnum!E(t.tupleof[I].stringof,
					  cast(long) t.tupleof[I],
					  signed, bitcount, true);
      }
      else {
	cstVecPrim = new CstVecRand(t.tupleof[I].stringof,
				    cast(long) t.tupleof[I],
				    signed, bitcount, true);
      }
      t._esdl__cstEng._cstRands[RI] = cstVecPrim;
    }
  }
  return cstVecPrim;
}

public CstVecPrim _esdl__cstRandArrLength(string VAR, size_t I,
				    size_t CI, size_t RI, T)(ref T t) {
  import std.traits;
  import std.range;

  // need to know the size and sign for creating a bddvec
  alias typeof(t.tupleof[I]) L;
  static assert(isArray!L);
  alias ElementType!L E;
  static assert(isIntegral!E || isBitVector!E);

  bool signed = isVarSigned!E;
  static if(isIntegral!E)        uint bitcount = E.sizeof * 8;
  else static if(isBitVector!E)  uint bitcount = E.SIZE;


  if(! findRandAttr!(I, t)) { // no @rand attr
    return _esdl__cstRand(t.tupleof[I].length, t);
  }
  else static if(isDynamicArray!L) { // @rand!N form
      enum size_t RLENGTH = findRandArrayAttr!(I, t);
      static assert(RLENGTH != -1);
      auto cstVecPrim = t._esdl__cstEng._cstRands[RI];
      if(cstVecPrim is null) {
	cstVecPrim = new CstVecRandArr(t.tupleof[I].stringof, RLENGTH,
				       false, 32, true, signed, bitcount, true);
	t._esdl__cstEng._cstRands[RI] = cstVecPrim;
      }
      return cstVecPrim;
    }
  else static if(isStaticArray!L) { // @rand with static array
      static assert(findRandElemAttr!(I, t) != -1);
      auto cstVecPrim = t._esdl__cstEng._cstRands[RI];
      size_t RLENGTH = t.tupleof[I].length;
      if(cstVecPrim is null) {
	cstVecPrim = new CstVecRandArr(t.tupleof[I].stringof, RLENGTH,
				       false, 32, false, signed, bitcount, true);
	t._esdl__cstEng._cstRands[RI] = cstVecPrim;
      }
      return cstVecPrim;
    }
    else static assert("Can not use .length with non-arrays");
}

public CstVecPrim _esdl__cstRandArrElem(string VAR, size_t I,
					size_t CI, size_t RI, T)(ref T t) {
  import std.traits;
  import std.range;

  // need to know the size and sign for creating a bddvec
  alias typeof(t.tupleof[I]) L;
  static assert(isArray!L);
  alias ElementType!L E;
  static assert(isIntegral!E || isBitVector!E);

  bool signed = isVarSigned!E;
  static if(isIntegral!E)        uint bitcount = E.sizeof * 8;
  else static if(isBitVector!E)  uint bitcount = E.SIZE;


  if(! findRandAttr!(I, t)) { // no @rand attr
    return _esdl__cstRand(t.tupleof[I].length, t);
  }
  else {
    auto cstVecPrim = t._esdl__cstEng._cstRands[RI];
    auto cstVecRandArr = cast(CstVecRandArr) cstVecPrim;
    if(cstVecRandArr is null && cstVecPrim !is null) {
      assert(false, "Non-array CstVecPrim for an Array");
    }
    static if(isDynamicArray!L) { // @rand!N form
      enum size_t RLENGTH = findRandArrayAttr!(I, t);
      static assert(RLENGTH != -1);
      if(cstVecRandArr is null) {
	cstVecRandArr = new CstVecRandArr(t.tupleof[I].stringof, RLENGTH,
				       false, 32, true, signed, bitcount, true);
	t._esdl__cstEng._cstRands[RI] = cstVecRandArr;
      }
    }
    else static if(isStaticArray!L) { // @rand with static array
	static assert(findRandElemAttr!(I, t) != -1);
	size_t RLENGTH = t.tupleof[I].length;
	if(cstVecRandArr is null) {
	  cstVecRandArr = new CstVecRandArr(t.tupleof[I].stringof, RLENGTH,
					 false, 32, false, signed, bitcount, true);
	  t._esdl__cstEng._cstRands[RI] = cstVecRandArr;
	}
      }
      else static assert("Can not use .length with non-arrays");
    for (size_t i=0; i!=RLENGTH; ++i) {
      if(cstVecRandArr[i] is null) {
	import std.conv: to;
	auto init = (ElementType!(typeof(t.tupleof[I]))).init;
	if(i < t.tupleof[I].length) {
	  static if(is(E == enum)) {
	    cstVecRandArr[i] = new CstVecRandEnum!L(t.tupleof[I].stringof ~ "[" ~ i.to!string() ~ "]",
						    cast(long) t.tupleof[I][i], signed, bitcount, true);
	  }
	  else {
	    cstVecRandArr[i] = new CstVecRand(t.tupleof[I].stringof ~ "[" ~ i.to!string() ~ "]",
					      cast(long) t.tupleof[I][i], signed, bitcount, true);
	  }
	}
	else {
	  static if(is(E == enum)) {
	    cstVecRandArr[i] = new CstVecRandEnum!L(t.tupleof[I].stringof ~ "[" ~ i.to!string() ~ "]",
						    cast(long) init, signed, bitcount, true);
	  }
	  else {
	    cstVecRandArr[i] = new CstVecRand(t.tupleof[I].stringof ~ "[" ~ i.to!string() ~ "]",
					      cast(long) init, signed, bitcount, true);
	  }
	}
      }
    }
    return cstVecRandArr;
  }
}

public CstVecLoopVar _esdl__cstRandArrIndex(string VAR, T)(ref T t)
  if(is(T f: RandomizableIntf) && is(T == class)) {
    return _esdl__randNamedApply!(VAR, _esdl__cstRandArrIndex)(t);
  }

public CstVecLoopVar _esdl__cstRandArrIndex(string VAR, size_t I,
					 size_t CI, size_t RI, T)(ref T t) {
  auto lvar = _esdl__cstRandArrLength!(VAR, I, CI, RI, T)(t);
  
  return lvar.makeLoopVar();
}

public CstVecExpr _esdl__cstRandArrElem(string VAR, T)(ref T t)
  if(is(T f: RandomizableIntf) && is(T == class)) {
    auto arr = _esdl__randNamedApply!(VAR, _esdl__cstRandArrElem)(t);
    auto idx = arr.makeLoopVar();
    return arr[idx];
  }
