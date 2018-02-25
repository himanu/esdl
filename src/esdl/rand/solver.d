module esdl.rand.solver;
import esdl.rand.obdd;

import esdl.rand.expr: CstBlock, CstVarPrim, CstStage,
  CstBddExpr, CstValAllocator;
import esdl.rand.base;


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
  import esdl.rand.cstx;
  CstParser parser = CstParser(CST);
  return parser.translate(NAME);
}

abstract class Constraint(string C): _esdl__ConstraintBase
{
  this(_esdl__SolverRoot eng, string name, uint index) {
    super(eng, name, C, index);
  }
};


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

template _esdl__SolverResolve(T) {
  // static if(__traits(compiles, T._esdl__hasRandomization)) {
  static if(is(T == class)) {
    alias _esdl__SolverResolve = T._esdl__Solver;
  }
  else {
    alias _esdl__SolverResolve = _esdl__SolverStruct!T;
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

abstract class _esdl__SolverRoot {
  // Keep a list of constraints in the class
  _esdl__ConstraintBase[] _esdl__cstsList;
  _esdl__ConstraintBase _esdl__cstWith;
  bool _esdl__cstWithChanged;

  // ParseTree parseList[];
  CstVarPrim[] _esdl__randsList;
  _esdl__RandGen _esdl__rGen;

  _esdl__RandGen _esdl__getRandGen() {
    return _esdl__rGen;
  }

  Buddy _esdl__buddy;

  BddDomain[] _domains;

  // compositional parent -- not inheritance based
  _esdl__SolverRoot _parent = null;

  bool _esdl__isSeeded = false;

  CstBlock _esdl__cstEqns;

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
      _esdl__cstEqns = new CstBlock();
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

  // These get overridden at compile time
  void _esdl__initRands() {}
  void _esdl__initCsts() {}

  void doRandomize(_esdl__RandGen randGen) {}

  void solve() { // (T)(T t) {
    // writeln("Solving BDD for number of constraints = ", _esdl__cstsList.length);

    if (_domains.length is 0 || _esdl__cstWithChanged is true) {
      initDomains();

      // _esdl__cstEqns._esdl__reset(); // start empty
    
      // take all the constraints -- even if disabled
      // if (_esdl__cstEqns.isEmpty()) {
      foreach(ref _esdl__ConstraintBase cst; _esdl__cstsList) {
	_esdl__cstEqns ~= cst.getCstExpr();
      }
      if(_esdl__cstWith !is null) {
	_esdl__cstEqns ~= _esdl__cstWith.getCstExpr();
      }
      //}

    }

    CstValAllocator.mark();

    CstStage[] unsolvedStages;	// unresolved stages -- all

    int stageIdx=0;
    CstBddExpr[] unsolvedExprs = _esdl__cstEqns._exprs;	// unstaged Expressions -- all
    while(unsolvedExprs.length > 0 || unsolvedStages.length > 0) {
      CstBddExpr[] cstExprs = unsolvedExprs;
      unsolvedExprs.length = 0;
      CstStage[] cstStages = unsolvedStages;
      unsolvedStages.length = 0;

      CstBddExpr[] urExprs;	// unrolled expressions
      // unroll all the unrollable expressions
      foreach(expr; cstExprs) {
	// import std.stdio;
	// writeln("Unrolling: ", expr.name());
	// auto unrolled = expr.unroll();
	// for (size_t i=0; i!=unrolled.length; ++i)
	//   {
	//     writeln("Unrolled as: ", unrolled[i].name());
	//   }
	urExprs ~= expr.unroll();
      }

      foreach(expr; urExprs) {
	if(expr.itrVars().length is 0) {
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
	   stage.allReqsMet()) {
	  solveStage(stage, stageIdx);
	}
	else {
	  // assert(stage._rndVars.length !is 0);
	  unsolvedStages ~= stage;
	}
      }
      
    }

    CstValAllocator.reset();
    
  }

  void solveStage(CstStage stage, ref int stageIdx) {
    import std.conv;
    // initialize the bdd vectors
    BDD solveBDD = _esdl__buddy.one();
    foreach(vec; stage._rndVars) {
      if(vec.stage is stage) {
	if(vec.bddvec.isNull()) {
	  vec.bddvec.buildVec(_domains[vec.domIndex], vec.signed);
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

    foreach (vec; stage._rndVars) {
      ulong v;
      enum WORDSIZE = 8 * v.sizeof;
      auto bitvals = solveBDD.getIndices(vec.domIndex);
      foreach (uint i, ref j; bitvals) {
	uint pos = i % WORDSIZE;
	uint word = i / WORDSIZE;
	if (bits.length == 0 || bits[j] == -1) {
	  v = v + ((cast(size_t) _esdl__rGen.flip()) << pos);
	}
	else if (bits[j] == 1) {
	  v = v + ((cast(ulong) 1) << pos);
	}
	if (pos == WORDSIZE - 1 || i == bitvals.length - 1) {
	  vec.collate(v, word);
	  v = 0;
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

  // list of constraint eqns to solve at a given stage
  // void addCstStage(CstVarPrim prim, ref CstStage[] cstStages) {
  //   assert (prim !is null);
  //   if(prim.stage() is null) {
  //     CstStage stage = new CstStage();
  //     cstStages ~= stage;
  //     prim.stage = stage;
  //     stage._rndVars ~= prim;
  //     // cstStages[stage]._rndVars ~= prim;
  //   }
  // }

  void addCstStage(CstBddExpr expr, ref CstStage[] cstStages) {
    // uint stage = cast(uint) _cstStages.length;
    auto vecs = expr.getSolvables();
    CstStage stage;
    foreach (ref vec; vecs) {
      assert(vec !is null);
      if (vec.stage() is null) {
	if (stage is null) {
	  stage = new CstStage();
	  cstStages ~= stage;
	}
	vec.stage = stage;
	stage._rndVars ~= vec;
	// cstStages[stage]._rndVars ~= vec;
      }
      if (stage !is vec.stage()) { // need to merge stages
	mergeCstStages(stage, vec.stage(), cstStages);
	stage = vec.stage();
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

  void initDomains() { // (T)(T t) {
    uint domIndex = 0;
    int[] domList;

    _esdl__cstEqns._esdl__reset(); // start empty

    // take all the constraints -- even if disabled
    foreach(ref _esdl__ConstraintBase cst; _esdl__cstsList) {
      _esdl__cstEqns ~= cst.getCstExpr();
    }

    if(_esdl__cstWith !is null) {
      _esdl__cstEqns ~= _esdl__cstWith.getCstExpr();
    }

    foreach(stmt; _esdl__cstEqns._exprs) {
      foreach(vec; stmt.getRndPrims()) {
	if(vec.domIndex != uint.max) {
	  vec.domIndex = uint.max;
	}
      }
    }

    foreach(stmt; _esdl__cstEqns._exprs) {
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

  void printSolution() {
    // import std.stdio;
    // writeln("There are solutions: ", _theBDD.satCount());
    // writeln("Distribution: ", dist);
    // auto randSol = _theBDD.randSat(randGen, dist);
    // auto solution = _theBDD.fullSatOne();
    // solution.printSetWith_Domains();
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

mixin template _esdl__SolverMixin()
{
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
      return new CstVar!(L, _esdl__norand, 0)(name, l);
    }
    else static if (isArray!L) {
      // import std.stdio;
      // writeln("Creating VarVecArr, ", name);
      return new CstVar!(L, _esdl__norand, 0)(name, l);
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
      return new CstVar!(L, _esdl__norand, 0)(name, l);
    }
    else static if (isArray!L) {
      // import std.stdio;
      // writeln("Creating VarVecArr, ", name);
      return new CstVar!(L, _esdl__norand, 0)(name, l);
    }
    else {
      return l;
    }
   }


  auto _esdl__vec(L)(L l, string name="unnamed") {
    import std.traits: isIntegral;
    import esdl.data.bvec: isBitVector;
    static if (isIntegral!L || isBitVector!L) {
      return CstVal!L.allocate(l);
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

