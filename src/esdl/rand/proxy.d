module esdl.rand.proxy;
import esdl.solver.obdd;
import esdl.solver.base;
import esdl.solver.bdd;

import esdl.rand.base: CstVecPrim, CstStage, CstLogicExpr,
  CstDomain, CstPredicate, CstBlock, _esdl__Proxy, CstPredGroup,
  DomType;

import esdl.rand.misc;
import esdl.data.folder;
import esdl.data.charbuf;
import std.container: Array;
import std.array;

abstract class _esdl__ConstraintBase: _esdl__Norand
{
  this(_esdl__ProxyRoot eng, string name, string constraint) {
    _cstEng = eng;
    _name = name;
    _constraint = constraint;
  }
  immutable @rand(false) string _constraint;
  protected @rand(false) bool _enabled = true;
  protected @rand(false) _esdl__ProxyRoot _cstEng;
  protected @rand(false) string _name;
  protected @rand(false) CstBlock _cstBlock;

  bool isEnabled() {
    return _enabled;
  }

  void enable() {
    _enabled = true;
  }

  void disable() {
    _enabled = false;
  }

  string name() {
    return _name;
  }

  final _esdl__ProxyRoot getProxy() {
    return _cstEng;
  }

  final CstBlock getCstBlock() {
    if (_cstBlock is null) {
      _cstBlock = getParsedCstBlock();
    }
    return _cstBlock;
  }

  abstract CstBlock getParsedCstBlock();
}

static char[] constraintXlate(string PROXY, string CST,
			      string FILE, size_t LINE, string NAME="") {
  import esdl.rand.cstx;
  CstParser parser = CstParser(CST, FILE, LINE);
  return parser.translate(PROXY, NAME);
}

abstract class Constraint(string CONSTRAINT, string FILE=__FILE__, size_t LINE=__LINE__)
  : _esdl__ConstraintBase
{
  this(_esdl__ProxyRoot eng, string name) {
    super(eng, name, CONSTRAINT);
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


abstract class _esdl__ProxyRoot: _esdl__Proxy
{
  // Keep a list of constraints in the class
  _esdl__ConstraintBase _esdl__cstWith;

  bool _esdl__cstWithChanged;

  CstBlock _esdl__cstExprs;

  CstStage[] savedStages;

  Array!ulong _solveValue;
  
  this(string name, Object outer, _esdl__Proxy parent) {
    super(name, parent);
    if (parent is null) {
      _esdl__cstExprs = new CstBlock();
    }
  }

  this(string name, _esdl__Proxy parent) { // for structs
    super(name, parent);
    if (parent is null) {
      _esdl__cstExprs = new CstBlock();
    }
  }

  // overridden by Randomization mixin -- see meta.d
  abstract void _esdl__doRandomize(_esdl__RandGen randGen);
  abstract void _esdl__doConstrain(_esdl__ProxyRoot proxy);

  // void obsoleteSolve() { // (T)(T t) {
  //   // writeln("Solving BDD for number of constraints = ", _esdl__cstsList.length);
  //   uint lap = 0;
  //   // if (_domains.length is 0 // || _esdl__cstWithChanged is true
  //   // 	) {
  //   if (_esdl__cstExprs.isEmpty || _esdl__cstWithChanged is true) {
  //     initPreds();
  //   }

  //   CstStage[] unsolvedStages;

  //   int stageIndx=0;
  //   CstPredicate[] unrolledPreds = _esdl__cstExprs._exprs;	// unstaged Expressions -- all
  //   CstPredicate[] toResolvePreds;   			// need resolution wrt LAP logic
  //   CstPredicate[] unresolvedPreds;			// need resolution wrt LAP logic
    
  //   // import std.stdio;
  //   // writeln("There are ", unrolledPreds.length, " number of unsolved expressions");
  //   // writeln("There are ", _cstRndDomains.length, " number of domains");

  //   while(unrolledPreds.length > 0 || unsolvedStages.length > 0) {
  //     lap += 1;

  //     CstStage[] cstStages = unsolvedStages;
  //     unsolvedStages.length = 0;

  //     CstPredicate[] cstExprs = unrolledPreds;
  //     unrolledPreds.length = 0;
  //     toResolvePreds = unresolvedPreds;
  //     unresolvedPreds.length = 0;

  //     CstLogicExpr[] uwExprs;	// unwound expressions

  //     // unroll all the unrollable expressions
  //     foreach(expr; cstExprs) {
  // 	// import std.stdio;
  // 	// writeln("Unrolling: ", expr.name());
  // 	// auto unwound = expr.unroll();
  // 	// for (size_t i=0; i!=unwound.length; ++i) {
  // 	//   writeln("Unwound as: ", unwound[i].name());
  // 	// }
  // 	expr.unroll(lap, unrolledPreds, unrolledPreds, toResolvePreds// uwExprs
  // 		    );
  //     }

  //     // foreach(expr; uwExprs) {
  //     // 	// if(expr.itrVars().length is 0) {
  //     // 	if(expr.hasUnresolvedIndx()) {
  //     // 	  import std.stdio;
  //     // 	  writeln("Adding expression ", expr.name(), " to unresolved");
  //     // 	  expr.resolveLap(lap);
  //     // 	  unrolledPreds ~= expr;
  //     // 	}
  //     // 	// else {
  //     // 	//   toResolvePreds ~= expr;
  //     // 	// }
  //     // }

  //     foreach (pred; toResolvePreds) {
  // 	// import std.stdio;
  // 	uint elap = pred.getExpr().resolveLap();
  // 	// writefln("Unroll Lap of the pred %s is %s", pred.name() , lap);
  // 	if (elap == lap) {
  // 	  unresolvedPreds ~= pred;
  // 	}
  // 	else {
  // 	  // import std.stdio;
  // 	  // writeln("Adding pred ", pred.name(), " to stage");
  // 	  addCstStage(pred, cstStages);
  // 	}
  //     }

  //     foreach(stage; cstStages) {
  // 	if(stage !is null) {
  // 	  solveStage(stage, stageIndx);
  // 	}
  // 	else {
  // 	  // assert(stage._domVars.length !is 0);
  // 	  unsolvedStages ~= stage;
  // 	}
  //     }
  //   }
  // }

  Folder!(CstStage, "solvedStages") _solveStages;

  void reset() {
    // _solvedDomains is from previous cycle
    foreach (dom; _solvedDomains) {
      dom.reset();
    }

    // reset all bins
    _rolledPreds.reset();
    _toRolledPreds.reset();
    _resolvedPreds.reset();
    _resolvedDynPreds.reset();
    _toSolvePreds.reset();
    _unresolvedPreds.reset();
    _toUnresolvedPreds.reset();

    _resolvedMonoPreds.reset();

    _solvedDomains.reset();

    updateValDomains();
  }

  void solve() {
    assert(_root is this);
    this._cycle += 1;
    
    int stageIndx=0;

    while (_resolvedMonoPreds.length > 0 ||
	   _resolvedDynPreds.length > 0 ||
	   _resolvedPreds.length > 0 ||
	   _unresolvedPreds.length > 0 ||
	   _toRolledPreds.length > 0) {
      // import std.stdio;

      // if (_resolvedMonoPreds.length > 0) {
      // 	writeln("Here for _resolvedMonoPreds: ", _resolvedMonoPreds.length);
      // }
      // if (_resolvedPreds.length > 0) {
      // 	writeln("Here for _resolvedPreds: ", _resolvedPreds.length);
      // }
      // if (_unresolvedPreds.length > 0) {
      // 	writeln("Here for _unresolvedPreds: ", _unresolvedPreds.length);
      // }
      // _lap, like _cycle starts with 1
      // this is to avoid default values
      _lap += 1;
      // writeln("Lap: ", _lap);

      _rolledPreds.reset();
      _rolledPreds.swap(_toRolledPreds);

      foreach (pred; _rolledPreds) {
	if (pred.isRolled()) {
	  pred.markAsUnresolved(_lap);
	  _toRolledPreds ~= pred;
	}
      }

      foreach (pred; _unresolvedPreds) {
	if (pred.isResolved()) {
	  procResolved(pred);
	}
	else {
	  _toUnresolvedPreds ~= pred;
	  pred.markAsUnresolved(_lap);
	}
      }

      _resolvedMonoPreds.swap(_toSolvePreds);

      foreach (pred; _toSolvePreds) {
	if (! procMonoDomain(pred)) {
	  // writeln("Mono Unsolved: ", pred.name());
	  _resolvedPreds ~= pred;
	}
      }
      _toSolvePreds.reset();
      
      // first handle _resolvedDynPreds
      _resolvedDynPreds.swap(_toSolvePreds);

      foreach (pred; _toSolvePreds) {
	if (pred.isMarkedUnresolved(_lap)) {
	  _resolvedDynPreds ~= pred;
	}
	else {
	  if (! procMaybeMonoDomain(pred)) {
	    pred.solve();
	    _solvePreds ~= pred;
	    addCstStage(pred);
	  }
	}
      }
      _toSolvePreds.reset();

      // now the normal _resolvedPreds
      _resolvedPreds.swap(_toSolvePreds);

      foreach (pred; _toSolvePreds) {
	if (pred.isMarkedUnresolved(_lap)) {
	  _resolvedPreds ~= pred;
	}
	else {
	  pred.solve();
	  _solvePreds ~= pred;
	  addCstStage(pred);
	}
      }

      _toSolvePreds.reset();

      // Work on _solvePreds
      foreach (pred; _solvePreds) {
	CstPredGroup group = pred.group();
	if (group is null) {
	  group = new CstPredGroup;
	  pred._group = group;
	  pred.setGroupContext(group);
	}
      }

      _solvePreds.reset();

      
      foreach(stage; _solveStages) {
	if(stage !is null) {
	  solveStage(stage, stageIndx);
	}
      }
      _solveStages.reset();

      _unresolvedPreds.reset();
      _unresolvedPreds.swap(_toUnresolvedPreds);
    }

  }

  bool procMonoDomain(CstPredicate pred) {
    assert (pred._rnds.length > 0);
    auto dom = pred._rnds[0];
    if (! dom.isSolved()) {
      if (dom.solveRange(_esdl__getRandGen())) {
	_solvedDomains ~= dom;
	return true;
      }
      else return false;
    }
    else return true;
  }

  bool procMaybeMonoDomain(CstPredicate pred) {
    assert (pred._rnds.length > 0);
    if (pred._rnds.length > 1) {
      return false;
    }
    auto dom = pred._rnds[0];
    if (! dom.isStatic()) {
      dom = dom.getResolved();
    }
    if (! dom.isSolved()) {
      if (dom.solveRange(_esdl__getRandGen())) {
	_solvedDomains ~= dom;
	return true;
      }
      else return false;
    }
    else return true;
  }

  void procResolved(CstPredicate pred) {
    assert (pred._rnds.length > 0);
    if (pred._rnds.length == 1 &&
	pred._rnds[0]._type <= DomType.LAZYMONO) {
      _resolvedMonoPreds ~= pred;
      // procMonoDomain(pred._rnds[0], pred);
    }
    else if (pred._dynRnds.length > 0) {
      foreach (rnd; pred._dynRnds) {
	auto dom = rnd.getResolved();
	dom._tempPreds ~= pred;
      }
      _resolvedDynPreds ~= pred;
    }
    else {
      _resolvedPreds ~= pred;
    }
  }

  void addPredicate(CstPredicate pred) {
    // import std.stdio;
    // writeln("Adding Predicate: ", pred.name());
    pred.randomizeDeps();
    if (pred._iters.length > 0) {
      _toRolledPreds ~= pred;
    }
    else if (pred._deps.length > 0) {
      _unresolvedPreds ~= pred;
    }
    else {
      procResolved(pred);
    }
  }

  void addUnrolledPredicate(CstPredicate pred) {
    pred.randomizeDeps();
    if (pred._iters.length == 0) {
      if (pred.isResolved(true)) {
	procResolved(pred);
      }
      else {
	_toUnresolvedPreds ~= pred;
      }
    }
    else {
      _toRolledPreds ~= pred;
    }
  }

  void addDomain(CstDomain domain) {
    if (domain.isRand()) {
      // _root._cstRndDomains ~= domain;
      useBuddy(CstBddSolver.buddy());
      domain.domIndex = CstBddSolver.buddy.addDomVec(domain.bitcount, domain.signed());
    }
    else {
      _root._cstValDomains ~= domain;
    }
  }

  void solveStage(CstStage stage, ref int stageIndx) {
    import std.conv;
    CstPredicate[] preds = stage._predicates;
    static Charbuf exprBuf;

    foreach(pred; preds) {
      // pred.getExpr.writeExprString(exprBuf);
      // writeln(exprBuf[]);
    }
    // foreach (pred; preds) {
    //   pred.annotate();
    // }

    // foreach (pred; preds) {
    //   import std.stdio;
    //   writeln("Proxy: ", pred.name(), " update: ", pred.hasUpdate());
    // }
    // make the bdd tree
    bool updated = false;

    foreach (pred; preds) {
      updated |= pred.hasUpdate();
    }
    // import std.stdio;
    // writeln("Saved Stages: ", savedStages.length);
    // writeln("Saved Stages Index: ", stageIndx);
    // if (savedStages.length > stageIndx) {
    //   foreach (pred; savedStages[stageIndx]._predicates) {
    // 	writeln("saved: ", pred.name());
    //   }
    //   writeln("Comparison: ", savedStages[stageIndx]._predicates[0] == stage._predicates[0]);
    // }
    // foreach (pred; stage._predicates) {
    //   writeln("saved: ", pred.name());
    // }
    BDD solveBDD = CstBddSolver.buddy.one();
    
    if (savedStages.length > stageIndx &&
	savedStages[stageIndx]._predicates == stage._predicates) {
      if (! updated) {
	// import std.stdio;
	// writeln("Reusing previous BDD solution");
	stage._solveBDD = savedStages[stageIndx]._solveBDD;
	stage._bddDist = savedStages[stageIndx]._bddDist;
	solveBDD = stage._solveBDD;
      }
      else {
	foreach(pred; preds) {
	  // pred.getExpr.writeExprString(exprBuf);
	  // pred.getExpr().visit(savedStages[stageIndx]._solver);
	  solveBDD = solveBDD & pred.getExpr().getBDD(stage, CstBddSolver.buddy);
	  // writeln(pred.name());
	}
      }
    }
    else {
      // auto solver = new CstBddSolver(stage);
      if (stage._solver is null) {
	// import std.stdio;
	// writeln("new solver");
	stage._solver = new CstBddSolver(stage);
      }
      foreach (vec; stage._domVars) {
	if (vec.stage is stage) {
	  if (vec.domIndex == uint.max) {
	    this.addDomain(vec);
	  }
	  // if (vec.bddvec(_esdl__buddy).isNull()) {
	  //   vec.bddvec(_esdl__buddy).buildVec(vec.domIndex, vec.signed);
	  // }
	}
      }
      // foreach(vec; stage._domVars) {
      // 	BDD primBdd = vec.getPrimBdd(_esdl__buddy);
      // 	if(! primBdd.isOne()) {
      // 	  solveBDD = solveBDD & primBdd;
      // 	}
      // }
      foreach(pred; preds) {
	// import std.stdio;
	// writeln(pred.describe());
	// pred.getExpr().visit(stage._solver);
	// pred.getExpr.writeExprString(exprBuf);
	solveBDD = solveBDD & pred.getExpr().getBDD(stage, CstBddSolver.buddy);
	// writeln(pred.name());
      }
    }

    stage._solveBDD = solveBDD;
    stage._bddDist.clear();
    solveBDD.satDist(stage._bddDist);


    // import std.stdio;
    // writeln("bddDist: ", stage._bddDist);
    
    auto solution = solveBDD.randSatOne(this._esdl__rGen.get(),
					stage._bddDist);
    auto solVecs = solution.toVector();

    byte[] bits;
    if(solVecs.length != 0) {
      bits = solVecs[0];
    }

    foreach (vec; stage._domVars) {
      ulong v;
      enum WORDSIZE = 8 * v.sizeof;
      auto bitvals = solveBDD.getIndices(vec.domIndex);
      auto NUMWORDS = (bitvals.length+WORDSIZE-1)/WORDSIZE;
      
      if (_solveValue.length < NUMWORDS) {
	_solveValue.length = NUMWORDS;
      }
      foreach (i, ref j; bitvals) {
	uint pos = (cast(uint) i) % WORDSIZE;
	uint word = (cast(uint) i) / WORDSIZE;
	if (bits.length == 0 || bits[j] == -1) {
	  v = v + ((cast(size_t) _esdl__rGen.flip()) << pos);
	}
	else if (bits[j] == 1) {
	  v = v + ((cast(ulong) 1) << pos);
	}
	if (pos == WORDSIZE - 1 || i == bitvals.length - 1) {
	  _solveValue[word] = v;
	  v = 0;
	}
      }
      vec.setVal(array(_solveValue[0..NUMWORDS]));
      _solvedDomains ~= vec;
    }
    stage.id(stageIndx);

    // foreach (vec; stage._domVars) {
    //   vec.execCbs();
    // }
    

    // save for future reference
    while (savedStages.length <= stageIndx) {
      savedStages ~= new CstStage();
    }
    assert(savedStages[stageIndx] !is stage);

    savedStages[stageIndx].copyFrom(stage);

    stageIndx += 1;
  }

  // list of constraint preds to solve at a given stage
  // void addCstStage(CstVecPrim prim, ref CstStage[] cstStages) {
  //   assert (prim !is null);
  //   if(prim.stage() is null) {
  //     CstStage stage = new CstStage();
  //     cstStages ~= stage;
  //     prim.stage = stage;
  //     stage._domVars ~= prim;
  //     // cstStages[stage]._domVars ~= prim;
  //   }
  // }

  void addCstStage(CstPredicate pred) {
    // uint stage = cast(uint) _solveStages.length;
    // auto vecs = pred.getExpr().getRndDomains(true);
    auto vecs = pred.getDomains();
    // auto vecs = pred._rnds;
    // import std.stdio;
    // foreach (vec; vecs) writeln(vec.name());
    CstStage stage;
    foreach (ref vec; vecs) {
      if (! vec.isSolved()) {
	assert(vec !is null);
	if (vec.stage() is null) {
	  // import std.stdio;
	  // writeln("new stage for vec: ", vec.name());
	  // writeln("pred: ", pred.name());
	  if (stage is null) {
	    stage = new CstStage();
	    _solveStages ~= stage;
	  }
	  vec.stage = stage;
	  stage._domVars ~= vec;
	  // _solveStages[stage]._domVars ~= vec;
	}
	if (stage !is vec.stage()) { // need to merge stages
	  // import std.stdio;
	  // writeln("merging");
	  mergeCstStages(stage, vec.stage());
	  stage = vec.stage();
	}
      }
    }
    if (stage is null) {
      stage = new CstStage();
      _solveStages ~= stage;
    }
    // import std.stdio;
    // writeln(pred.name());
    // assert (stage !is null);
    // writeln(stage._predicates.length);
    stage._predicates ~= pred;
  }

  void mergeCstStages(CstStage fromStage, CstStage toStage) {
    if(fromStage is null) {
      // fromStage has not been created yet
      return;
    }
    foreach(ref vec; fromStage._domVars) {
      if (vec.stage == fromStage) {
	vec.stage = toStage;
      }
    }
    foreach(ref vec; fromStage._domains) {
      if (vec.stage == fromStage) {
	vec.stage = toStage;
      }
    }
    toStage._domVars ~= fromStage._domVars;
    toStage._domains ~= fromStage._domains;
    toStage._predicates ~= fromStage._predicates;
    if(_solveStages[$-1] is fromStage) {
      _solveStages.length = _solveStages.length - 1;
    }
    else {
      fromStage._domVars.length = 0;
      fromStage._domains.length = 0;
      fromStage._predicates.length = 0;
    }
  }

  void initDomains() { // (T)(T t) {
    assert(_root is this);
    // int[] domList;
    

    // foreach (dom; _cstRndDomains) dom.reset();
    
    // this._domIndex = 0;
    // _domains.length = 0;
    // _cstRndDomains.length = 0;
    
    // _esdl__buddy.clearAllDomains();

    
    // foreach(stmt; _esdl__cstExprs._exprs) {
    //   addDomains(stmt.getRndDomains(false));
    // }
  }

  // void addDomains(CstDomain[] domains) {
  //   uint[] domList;

  //   // foreach (vec; domains) {
  //   //   assert(vec.domIndex == uint.max);
  //   //   // vec.domIndex = uint.max;
  //   // }

  //   foreach(vec; domains) {
  //     if(vec.domIndex == uint.max) {
  // 	vec.domIndex = this._domIndex++;
  // 	domList ~= vec.bitcount;
  //     }
  //   }
  //   _domains ~= _esdl__buddy.extDomVec(domList);
  // }

  void printSolution() {
    // import std.stdio;
    // writeln("There are solutions: ", _theBDD.satCount());
    // writeln("Distribution: ", dist);
    // auto randSol = _theBDD.randSat(randGen, dist);
    // auto solution = _theBDD.fullSatOne();
    // solution.printSetWith_Domains();
  }
}
