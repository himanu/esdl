module esdl.rand.base;

import esdl.rand.obdd;
import esdl.rand.intr;
import esdl.rand.misc: _esdl__RandGen, _esdl__norand, isVecSigned;
import esdl.data.bvec: isBitVector;
import esdl.data.bin;
import std.algorithm;
import std.array;
import std.container.array;
import std.traits: isIntegral, isBoolean, isArray, isStaticArray, isDynamicArray;

abstract class _esdl__Solver
{
  Buddy _esdl__buddy;

  Array!uint _domains;		// indexes of domains

  CstDomain[] _cstDomains;

  uint _domIndex = 0;
  
  // compositional parent -- not inheritance based
  _esdl__Solver _parent;
  _esdl__Solver _root;

  Bin!CstPredicate _rolledPreds;
  Bin!CstPredicate _resolvedPreds;
  Bin!CstPredicate _toResolvedPreds;
  Bin!CstPredicate _toSolvePreds;
  Bin!CstPredicate _solvePreds;
  Bin!CstPredicate _unresolvedPreds;
  Bin!CstPredicate _toUnresolvedPreds;

  // the integer variable _lap is incremented everytime a set of @rand
  // variables is made available for constraint solving. This 
  // variable is used for:
  // 1. marking the predicates that have unresolved dependencies
  //    and/or have a relation with such predicates. When some
  //    predicated are resolved it may lead to some of these
  //    predicates becoming available
  uint _lap;
  // _cycle is incremented once everytime randomize function is
  // called. The use of this variable within a @rand variable is to
  // indicate that the variable has been solved. Within a predicate,
  // this variable indicates that the predicate has been solved for
  // the indiated randomization cycle. There are two more places where
  // this variable is used:
  // 1. To indicate that iterator has been unrolled for a vec array in
  //    a given randomization cycle.
  // 2. To indicate that the index expression has been resolved for a
  //    vec/vec-array for the indicated randomization cycle
  uint _cycle;

  void addDomain(CstDomain domain) {
    // import std.stdio;
    // writeln("Adding domain: ", domain.name());
    _root._cstDomains ~= domain;
    useBuddy(_root._esdl__buddy);
    domain.domIndex = _root._domIndex++;
    _root._domains ~= _esdl__buddy.extDomVec(domain.bitcount);
  }

  _esdl__Solver getSolverRoot() {
    return _root;
  }

  CstVecPrim[] _esdl__randsList;
  _esdl__RandGen _esdl__rGen;

  _esdl__RandGen _esdl__getRandGen() {
    assert(_root is this);
    return _root._esdl__rGen;
  }

  bool _esdl__isSeeded = false;
  uint _esdl__seed;

  uint getRandomSeed() {
    assert(_root is this);
    return _esdl__seed;
  }

  void seedRandom(uint seed) {
    assert(_root is this);
    _esdl__seed = seed;
    _esdl__rGen.seed(seed);    
  }
  
  this(uint seed, bool isSeeded, string name,
       _esdl__Solver parent) {
    import std.random: Random, uniform;
    debug(NOCONSTRAINTS) {
      assert(false, "Constraint engine started");
    }
    _esdl__isSeeded = isSeeded;
    if (isSeeded is true) {
      _esdl__seed = seed;
    }
    else {
      import esdl.base.core: Procedure;
      auto proc = Procedure.self;
      if (proc !is null) {
	Random procRgen = proc.getRandGen();
	_esdl__seed = uniform!(uint)(procRgen);
      }
      else {
	// no active simulation -- use global rand generator
	_esdl__seed = uniform!(uint)();
      }
    }
    _esdl__rGen = new _esdl__RandGen(_esdl__seed);

    if(parent is null) {
      _esdl__buddy = new Buddy(400, 400);
      _root = this;
    }
    else {
      _parent = parent;
      _root = _parent.getSolverRoot();
    }
  }

  void addUnrolled(CstPredicate pred) {
    if (pred.isResolved(true)) {
      _resolvedPreds ~= pred;
    }
    else {
      _unresolvedPreds ~= pred;
    }
  }
  
}

class CstStage {
  // List of randomized variables associated with this stage. A
  // variable can be associated with only one stage
  CstDomain[] _domVars;
  // The Bdd expressions that apply to this stage
  CstPredicate[] _predicates;
  // These are the length variables that this stage will solve
  // CstVecPrim[] _preReqs;
  
  BDD _solveBDD;

  double[uint] _bddDist;
  
  int _id = -1;

  ~this() {
    _solveBDD.reset();
  }
  
  void copyFrom(CstStage from) {
    _domVars = from._domVars;
    _predicates = from._predicates;
    _solveBDD = from._solveBDD;
    _bddDist = from._bddDist;
  }

  // return true is _predicates match
  bool compare(CstStage other) {
    return other._predicates == _predicates;
  }
  
  void id(uint i) {
    _id = i;
  }

  uint id() {
    return _id;
  }

  bool solved() {
    if(_id != -1) return true;
    else return false;
  }
}

// abstract class CstValAllocator {
//   static CstValAllocator[] allocators;

//   static void mark() {
//     foreach (allocator; allocators) {
//       allocator.markIndex();
//     }
//   }
  
//   static void reset() {
//     foreach (allocator; allocators) {
//       allocator.resetIndex();
//     }
//   }
  
//   abstract void resetIndex();

//   abstract void markIndex();
// }

abstract class CstDomain
{
  abstract string name();
  abstract ref BddVec bddvec(Buddy buddy);
  // abstract void bddvec(BddVec b);
  abstract void collate(ulong v, int word=0);
  abstract CstStage stage();
  abstract void stage(CstStage s);
  abstract uint domIndex();
  abstract void domIndex(uint s);
  abstract bool signed();
  abstract bool isRand();
  abstract uint bitcount();
  abstract void reset();
  abstract _esdl__Solver getSolverRoot();
  abstract BDD getPrimBdd(Buddy buddy);
  abstract void _esdl__doRandomize(_esdl__RandGen randGen);
  abstract void _esdl__doRandomize(_esdl__RandGen randGen, CstStage stage);
  final bool solved() {
    if(isRand()) {
      return stage() !is null && stage().solved();
    }
    else {
      return true;
    }
  }

  // Callbacks
  CstDepCallback[] _depCbs;
  CstPredicate[] _varPreds;
  CstPredicate[] _valPreds;

  void registerVarPred(CstPredicate varPred) {
    bool flag;
    foreach (pred; _varPreds) {
      if (pred is varPred) {
	flag = true;
	break;
      }
    }
    if (! flag) {
      _varPreds ~= varPred;
    }
  }
  
  void registerValPred(CstPredicate valPred) {
    bool flag;
    foreach (pred; _valPreds) {
      if (pred is valPred) {
	flag = true;
	break;
      }
    }
    if (! flag) {
      _valPreds ~= valPred;
    }
  }
  
  void registerDepPred(CstDepCallback depCb) {
    bool flag;
    foreach (cb; _depCbs) {
      if (cb is depCb) {
	flag = true;
	break;
      }
    }
    if (! flag) {
      _depCbs ~= depCb;
    }
  }

  void markAsUnresolved(uint lap) {
    foreach (pred; _varPreds) {
      pred.markAsUnresolved(lap);
    }
  }

  void execCbs() {
    execIterCbs();
    execDepCbs();
  }

  void execIterCbs() { }
  void execDepCbs() {
    foreach (cb; _depCbs) {
      cb.doResolve();
    }
  }
}

// The client keeps a list of agents that when resolved makes the client happy
interface CstIterCallback
{
  abstract void doUnroll();
}

interface CstDepCallback
{
  abstract void doResolve();
}

interface CstVecPrim
{
  abstract string name();
  abstract void _esdl__doRandomize(_esdl__RandGen randGen);
  abstract void _esdl__doRandomize(_esdl__RandGen randGen, CstStage stage);
  abstract bool isRand();
  // abstract long value();

  abstract void _esdl__reset();
  abstract bool isVarArr();
  abstract CstDomain[] getDomainLens(bool resolved);
  abstract void solveBefore(CstVecPrim other);
  abstract void addPreRequisite(CstVecPrim other);

  // this method is used for getting implicit constraints that are required for
  // dynamic arrays and for enums
  abstract BDD getPrimBdd(Buddy buddy);
  abstract void resetPrimeBdd();
}


interface CstVecExpr
{

  // alias evaluate this;

  abstract string name();
  
  // Array of indexes this expression has to resolve before it can be
  // converted into a BDD
  abstract CstIteratorBase[] iterVars();
  // get the primary (outermost foreach) iterator CstVecExpr
  abstract CstIteratorBase getIterator();
  abstract bool hasUnresolvedIndx();
  abstract CstDomain[] unresolvedIndxs();

  abstract uint resolveLap();
  abstract void resolveLap(uint lap);
  
  // List of Array Variables
  abstract CstVecPrim[] preReqs();

  abstract bool isConst();//  {
  //   return false;
  // }

  // get all the primary bdd vectors that constitute a given bdd
  // expression
  // The idea here is that we need to solve all the bdd vectors of a
  // given constraint equation together. And so, given a constraint
  // equation, we want to list out the elements that need to be
  // grouped together.
  abstract CstDomain[] getRndDomains(bool resolved);

  // get the list of stages this expression should be avaluated in
  // abstract CstStage[] getStages();
  abstract BddVec getBDD(CstStage stage, Buddy buddy);

  // refresh the _valvec if the current value is not the same as previous value
  abstract bool refresh(CstStage stage, Buddy buddy);

  abstract long evaluate();

  abstract bool getVal(ref long val);
  
  abstract CstVecExpr unroll(CstIteratorBase iter, uint n);

  abstract bool isOrderingExpr();//  {
  //   return false;		// only CstVecOrderingExpr return true
  // }

  abstract void setBddContext(CstPredicate pred,
			      ref CstDomain[] vars,
			      ref CstDomain[] vals,
			      ref CstIteratorBase[] iters,
			      ref CstDomain[] deps);

  abstract bool getIntMods(ref IntRangeModSet modSet);
}


// This class represents an unwound Foreach iter at vec level
abstract class CstIteratorBase
{
  CstIterCallback[] _cbs;
  void registerRolled(CstIterCallback cb) {
    _cbs ~= cb;
  }
  void unrollCbs() {
    foreach (cb; _cbs) {
      cb.doUnroll();
    }
  }
  abstract uint maxVal();
  abstract bool isUnrollable();
  abstract string name();
  abstract CstIteratorBase unrollIter(CstIteratorBase iter, uint n);
  abstract CstDomain getLenVec();
}

interface CstBddExpr
{
  string name();

  abstract bool refresh(CstStage stage, Buddy buddy);
  
  abstract CstIteratorBase[] iterVars(); //  {
  abstract CstIteratorBase getIterator(); //  {

  abstract CstVecPrim[] preReqs();

  abstract bool hasUnresolvedIndx();
  abstract CstDomain[] unresolvedIndxs();

  abstract uint resolveLap();
  abstract void resolveLap(uint lap);

  abstract void setBddContext(CstPredicate pred,
			      ref CstDomain[] vars,
			      ref CstDomain[] vals,
			      ref CstIteratorBase[] iters,
			      ref CstDomain[] deps);

  abstract bool getIntRange(ref IntRangeSet!long rangeSet);

  abstract CstBddExpr unroll(CstIteratorBase iter, uint n);

  abstract CstDomain[] getRndDomains(bool resolved);

  // abstract CstStage[] getStages();

  abstract BDD getBDD(CstStage stage, Buddy buddy);

}

class CstPredicate: CstIterCallback, CstDepCallback
{
  string name() {
    return "PREDICATE: " ~ _expr.name();
  }

  // alias _expr this;

  _esdl__Solver _solver;
  CstBddExpr _expr;
  CstPredicate _parent;
  bool _markResolve;
  uint _unrollCycle;

  Bin!CstPredicate _uwPreds;
  
  this(_esdl__Solver solver, CstBddExpr expr, CstPredicate parent, CstIteratorBase[] iters ...) {
    assert(solver !is null);
    _solver = solver;
    _expr = expr;
    _iters = iters;
    _parent = parent;
    this.setBddContext();
    debug(CSTPREDS) {
      import std.stdio;
      stderr.writeln(this.describe());
    }
  }

  // unroll recursively untill no unrolling is possible
  void unroll(uint lap,
	      ref CstPredicate[] unrollable,
	      ref CstPredicate[] unresolved,
	      ref CstPredicate[] resolved) {
    CstPredicate[] unrolled;
    auto iter = this.unrollableIter();
    if (iter is null) {
      unrolled ~= this;
    }
    // else {
    //   unrolled = this.unrollIter(iter);
    // }

    if (_expr.hasUnresolvedIndx()) {
      // TBD -- check that we may need to call resolveLap on each of the unrolled expression
      foreach (expr; unrolled) {
	_expr.resolveLap(lap);
      }
      
      if (_expr.iterVars().length == 1) {
	unresolved ~= unrolled;
      }
      else {
	unresolved ~= unrolled;
      }
    }
    else {
      resolved ~= unrolled;
    }
  }

  CstIteratorBase unrollableIter() {
    foreach(iter; _expr.iterVars()) {
      if(iter.isUnrollable()) return iter;
    }
    return null;
  }

  void unroll(CstIteratorBase iter) {
    assert (iter is _expr.getIterator());

    if(! iter.isUnrollable()) {
      assert(false, "CstIteratorBase is not unrollabe yet: "
	     ~ this.describe());
    }
    auto currLen = iter.maxVal();
    // import std.stdio;
    // writeln("maxVal is ", currLen);

    if (currLen > _uwPreds.length) {
      // import std.stdio;
      // writeln("Need to unroll ", currLen - _uwPreds.length, " times");
      for (uint i = cast(uint) _uwPreds.length;
	   i != currLen; ++i) {
	_uwPreds ~= new CstPredicate(_solver, _expr.unroll(iter, i), this,
				     _iters[1..$].map!(tr => tr.unrollIter(iter, i)).array);
      }
    }

    // Do not use foreach here since we may have more elements than current
    for (size_t i=0; i!=currLen; ++i) {
      if (this._iters.length == 1) {
	_solver.addUnrolled(_uwPreds[i]);
      }
    }
    
    // return _uwPreds[0..currLen];
  }

  void doResolve() {
    if (_iters.length == 0) {
      _markResolve = true;
      return;
    }
    else {
      doUnroll();
    }
  }

  void doUnroll() {
      if (_unrollCycle == _solver._cycle) return;
      // check if all the dependencies are resolved
      foreach (dep; _deps) {
	if (! dep.solved()) {
	  return;
	}
      }
      CstIteratorBase iter = _iters[0];
      if (iter.getLenVec().solved()) {
	this.unroll(iter);
      }
      _unrollCycle = _solver._cycle;
  }
  
  final bool isResolved(bool force=false) {
    if (_markResolve || force) {
      _markResolve = false;
      foreach (dep; _deps) {
	if (! dep.solved()) {
	  return false;
	}
      }
      return true;
    }
    return false;
  }
  
  CstDomain[] _vars;
  CstDomain[] _vals;
  CstDomain[] _deps;
  CstIteratorBase _iter;
  CstIteratorBase[] _iters;

  int _lap;

  final void markAsUnresolved(uint lap) {
    if (_lap != lap) {	 // already marked -- avoid infinite recursion
      _lap = lap;
      foreach (var; _vars) {
	var.markAsUnresolved(lap);
      }
    }
  }

  final bool markIfUnresolved(uint lap) {
    if (_deps.length > 0 || _iter !is null) {
      this.markAsUnresolved(lap);
      return true;
    }
    return false;
  }

  final bool isRolled() {
    return this._iters.length > 0;
  }
  
  final bool hasDeps() {
    return this._deps.length > 0;
  }

  final bool solvable() {
    return _deps.length == 0 && _iters.length == 0;
  }
  
  final void setBddContext() {
    CstIteratorBase[] varIters;
    CstIteratorBase[] pasredIters = _iters;
    
    _expr.setBddContext(this, _vars, _vals, varIters, _deps);

    // foreach (varIter; varIters) {
    //   import std.stdio;
    //   stderr.writeln("Found Iterator: ", varIter.name());
    // }
    // if (_iters.length > 0) {
    //   _len = _iters[0].getLenVec();
    // }
    foreach (var; _vars) var.registerVarPred(this);
    foreach (val; _vals) val.registerValPred(this);

    // Since parent _deps were already resolved when the parent
    // unrolled
    if (_parent !is null) {
      CstDomain[] _foundDeps = _deps;
      _deps = _foundDeps.filter!(dep => (! canFind(_parent._deps, dep))).array;
    }
    
    foreach (dep; _deps) dep.registerDepPred(this);

    // take only the parsed iterators that are found in the expression
    // as well
    // _iters = pasredIters.filter!(itr =>
    // 				 canFind(varIters, itr)).array;
    _iters = pasredIters.filter!(itr =>
    				 canFind!((CstIteratorBase a, CstIteratorBase b) => a == b)
    				 (varIters, itr)).array;
    
    
    if (_iters.length != 0) _iters[0].registerRolled(this);
  }

  CstBddExpr getExpr() {
    return _expr;
  }

  IntRangeSet!long _rangeSet;

  ref IntRangeSet!long getIntRange() {
    _expr.getIntRange(_rangeSet);
    return _rangeSet;
  }

  string describe() {
    string description = name() ~ "\n";
    if (_iters.length > 0) {
      description ~= "    iterators: \n";
      foreach (iter; _iters) {
	description ~= "\t" ~ iter.name() ~ "\n";
      }
    }
    if (_vars.length > 0) {
      description ~= "    variables: \n";
      foreach (var; _vars) {
	description ~= "\t" ~ var.name() ~ "\n";
      }
    }
    if (_vals.length > 0) {
      description ~= "    values: \n";
      foreach (val; _vals) {
	description ~= "\t" ~ val.name() ~ "\n";
      }
    }
    if (_deps.length > 0) {
      description ~= "    depends: \n";
      foreach (dep; _deps) {
	description ~= "\t" ~ dep.name() ~ "\n";
      }
    }
    description ~= "\n";
    return description;
  }
}

class CstBlock
{
  CstPredicate[] _preds;
  bool[] _booleans;

  bool refresh(CstStage stage, Buddy buddy) {
    bool result = false;
    foreach (pred; _preds) {
      result |= pred._expr.refresh(stage, buddy);
    }
    return result;
  }
  
  string name() {
    string name_ = "";
    foreach(pred; _preds) {
      name_ ~= " & " ~ pred._expr.name() ~ "\n";
    }
    return name_;
  }

  void _esdl__reset() {
    _preds.length = 0;
  }

  bool isEmpty() {
    return _preds.length == 0;
  }
  
  void opOpAssign(string op)(bool other)
    if(op == "~") {
      _booleans ~= other;
    }

  void opOpAssign(string op)(CstPredicate other)
    if(op == "~") {
      _preds ~= other;
    }

  void opOpAssign(string op)(CstBlock other)
    if(op == "~") {
      if(other is null) return;
      foreach(pred; other._preds) {
	_preds ~= pred;
      }
      foreach(boolean; other._booleans) {
	_booleans ~= boolean;
      }
    }

}
