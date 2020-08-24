module esdl.rand.proxy;
import esdl.solver.base;

import esdl.rand.base: CstVecPrim, CstLogicExpr,
  CstDomain, CstPredicate, CstBlock, _esdl__Proxy, CstPredGroup,
  DomType, CstVecExpr;

import esdl.rand.misc;
import esdl.data.folder;
import esdl.data.charbuf;
import esdl.rand.dist: DistRangeSetBase;

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
      _cstBlock = makeCstBlock();
    }
    return _cstBlock;
  }

  abstract CstBlock makeCstBlock();
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

  // Array!ulong _solveValue;
  
  this(_esdl__Proxy parent) { // for structs
    super(parent);
    if (parent is null) {
      _esdl__cstExprs = new CstBlock();
    }
  }

  // overridden by Randomization mixin -- see meta.d
  abstract void _esdl__doRandomize(_esdl__RandGen randGen);
  abstract void _esdl__doConstrain(_esdl__ProxyRoot proxy);


  void reset() {
    // _solvedDomains is from previous cycle
    foreach (dom; _solvedDomains) {
      dom.reset();
    }

    // reset all bins
    _newPreds.reset();
    _unrolledPreds.reset();
    
    _rolledPreds.reset();
    _toRolledPreds.reset();
    _resolvedPreds.reset();
    _resolvedDynPreds.reset();
    _toSolvePreds.reset();
    _unresolvedPreds.reset();
    _toUnresolvedPreds.reset();

    _resolvedDistPreds.reset();
    _resolvedMonoPreds.reset();

    _solvedDomains.reset();

    updateValDomains();
  }

  void solve() {
    assert(_root is this);
    this._cycle += 1;
    
    while (_newPreds.length > 0 ||
	   _unrolledPreds.length > 0 ||
	   // _resolvedDistPreds.length > 0 ||
	   // _resolvedMonoPreds.length > 0 ||
	   _resolvedDynPreds.length > 0 ||
	   _resolvedPreds.length > 0 ||
	   _unresolvedPreds.length > 0 ||
	   _toRolledPreds.length > 0) {
      bool solvedSome = false;

      foreach (pred; _newPreds) procNewPredicate(pred);
      _newPreds.reset();

      foreach (pred; _unrolledPreds) procUnrolledPredicate(pred);
      _unrolledPreds.reset();
      
      foreach (pred; _resolvedDistPreds) {
	if (_esdl__debugSolver) {
	  import std.stdio;
	  writeln("Solving Dist Predicate: ", pred.describe());
	}
	CstDomain[] distVars = pred.getRnds();
	assert (distVars.length == 1);
	CstDomain distDom = distVars[0];
	CstPredicate[] distPreds = distDom.getRandPreds();
	// long[] toRemove;
	DistRangeSetBase dist = pred._expr.getDist();
	dist.reset();
	foreach (predicate; distPreds){
	  CstVecExpr ex = predicate._expr.isNot(distDom);
	  // isNot returns rhs if the predicate is of != type,
	  // otherwise it returns null
	  if (predicate.getRnds().length == 1 && ! predicate.isDist()) {
	    if (ex is null) {
	      assert (false, "can only use != operator on distributed domains");
	    }
	    dist.purge(ex.evaluate());
	  }
	}
	dist.uniform(distDom, _esdl__getRandGen());
	solvedSome = true;
      }
      _resolvedDistPreds.reset();
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
	if (_esdl__debugSolver) {
	  import std.stdio;
	  writeln("Solving Mono Predicate: ", pred.describe());
	}
	if (! procMonoDomain(pred)) {
	  // writeln("Mono Unsolved: ", pred.name());
	  _resolvedPreds ~= pred;
	}
	else {
	  solvedSome = true;
	}
      }
      _toSolvePreds.reset();
      
      // first handle _resolvedDynPreds
      _resolvedDynPreds.swap(_toSolvePreds);

      foreach (pred; _toSolvePreds) {
	if (_esdl__debugSolver) {
	  import std.stdio;
	  writeln("Solving Maybe Mono Predicate: ", pred.describe());
	}
	if (pred.isMarkedUnresolved(_lap)) {
	  _resolvedDynPreds ~= pred;
	}
	else {
	  if (! procMaybeMonoDomain(pred)) {
	    _solvePreds ~= pred;
	  }
	  else {
	    solvedSome = true;
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
	  pred.reset();
	  _solvePreds ~= pred;
	}
      }

      _toSolvePreds.reset();

      // Work on _solvePreds
      foreach (pred; _solvePreds) {
	CstPredGroup group = pred.group();
	if (group is null) {
	  group = new CstPredGroup(this);
	}
	if (! group.isSolved()) {
	  group.setGroupContext(pred);
	  group.solve();
	  _solvedGroups ~= group;
	  solvedSome = true;
	}
      }

      if (solvedSome is false) {
	import std.stdio;
	if (_resolvedDistPreds.length > 0) {
	  stderr.writeln("_resolvedDistPreds: ");
	  foreach (pred; _resolvedDistPreds) stderr.writeln(pred.describe());
	}
	if (_resolvedMonoPreds.length > 0) {
	  stderr.writeln("_resolvedMonoPreds: ");
	  foreach (pred; _resolvedMonoPreds) stderr.writeln(pred.describe());
	}
	if (_resolvedDynPreds.length > 0) {
	  stderr.writeln("_resolvedDynPreds: ");
	  foreach (pred; _resolvedDynPreds) stderr.writeln(pred.describe());
	}
	if (_resolvedPreds.length > 0) {
	  stderr.writeln("_resolvedPreds: ");
	  foreach (pred; _resolvedPreds) stderr.writeln(pred.describe());
	}
	if (_unresolvedPreds.length > 0) {
	  stderr.writeln("_unresolvedPreds: ");
	  foreach (pred; _unresolvedPreds) stderr.writeln(pred.describe());
	}
	if (_toRolledPreds.length > 0) {
	  stderr.writeln("_toRolledPreds: ");
	  foreach (pred; _toRolledPreds) stderr.writeln(pred.describe());
	}
	assert (false, "Infinite loop in constraint solver");
      }

      _solvePreds.reset();

      
      foreach (group; _solvedGroups) {
	group.reset();
	_solvedDomains ~= group.domains();
      }
      _solvedGroups.reset();
      
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
    assert (pred._rnds.length > 0, pred.describe());
    if (pred.isDist()) {
      _resolvedDistPreds ~= pred;
    }
    else if (pred._rnds.length == 1 &&
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

  void addNewPredicate(CstPredicate pred) {
    _newPreds ~= pred;
  }

  void addUnrolledPredicate(CstPredicate pred) {
    _unrolledPreds ~= pred;
  }
  
  void procNewPredicate(CstPredicate pred) {
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

  void procUnrolledPredicate(CstPredicate pred) {
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

}
