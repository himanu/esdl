module esdl.rand.base;

import esdl.solver.obdd;
import esdl.solver.base;
import esdl.solver.bdd;

import esdl.rand.intr;
import esdl.rand.expr: CstValue;
import esdl.rand.proxy: _esdl__ConstraintBase, _esdl__ProxyRoot;
import esdl.rand.misc: _esdl__RandGen, isVecSigned;
import esdl.data.bvec: isBitVector;
import esdl.data.folder;
import esdl.data.charbuf;
import std.algorithm;
import std.array;
import std.container.array;
import std.traits: isIntegral, isBoolean, isArray, isStaticArray, isDynamicArray;

interface CstVarIntf {
  bool isRand();
}

interface CstVecIntf: CstVarIntf {}
interface CstVecArrIntf: CstVarIntf {}

interface CstObjIntf: CstVarIntf {}
interface CstObjArrIntf: CstVarIntf {}


enum DomType: ubyte
{   TRUEMONO = 1,
    LAZYMONO = 2,		// like TRUEMONO with only some vals that need runtime eval
    MAYBEMONO = 3,
    INDEXEDMONO = 4,
    MULTI = 5
    }

abstract class _esdl__Proxy
{
  // static Buddy _esdl__buddy;

  // CstDomain[] _cstRndDomains;
  CstDomain[] _cstValDomains;

  // compositional parent -- not inheritance based
  _esdl__Proxy _parent;
  _esdl__Proxy _root;
  
  Folder!(CstPredicate, "rolledPreds") _rolledPreds;
  Folder!(CstPredicate, "toRolledPreds") _toRolledPreds;
  Folder!(CstPredicate, "resolvedPreds") _resolvedPreds;
  Folder!(CstPredicate, "resolvedDynPreds") _resolvedDynPreds;
  Folder!(CstPredicate, "toSolvePreds") _toSolvePreds;

  Folder!(CstPredicate, "_solvePreds") _solvePreds;

  Folder!(CstPredicate, "unresolvedPreds") _unresolvedPreds;
  Folder!(CstPredicate, "toResolvedPreds") _toUnresolvedPreds;

  Folder!(CstPredicate, "resolvedMonoPreds") _resolvedMonoPreds;

  Folder!(CstDomain, "solvedDomains") _solvedDomains;

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

  Buddy getBuddy() {
    return CstBddSolver.buddy;
  }

  void updateValDomains() {
    foreach (dom; _cstValDomains) {
      dom.updateVal();
    }
  }
  
  _esdl__Proxy getProxyRoot()() {
    return _root;
  }

  _esdl__RandGen _esdl__rGen;

  _esdl__RandGen _esdl__getRandGen() {
    assert(_root is this);
    return _root._esdl__rGen;
  }

  uint _esdl__seed;
  bool _esdl__seeded = false;

  uint getRandomSeed() {
    assert(_root is this);
    return _esdl__seed;
  }

  bool isRandomSeeded() {
    assert(_root is this);
    return _esdl__seeded;
  }

  void seedRandom(uint seed) {
    assert(_root is this);
    _esdl__seeded = true;
    _esdl__seed = seed;
    _esdl__rGen.seed(seed);    
  }
  
  this(string name, _esdl__Proxy parent) {
    import std.random: Random, uniform;
    debug(NOCONSTRAINTS) {
      assert(false, "Constraint engine started");
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
      // if (_esdl__buddy is null) {
      // 	_esdl__buddy = new Buddy(400, 400);
      // }
      _root = this;
    }
    else {
      _parent = parent;
      _root = _parent.getProxyRoot();
      // _esdl__buddy = _root._esdl__buddy;
    }
    // scopes for constraint parsing
    _rootScope = new CstScope(null, null);
    _currentScope = _rootScope;
  }

  
  // void addToUnrolled(CstPredicate pred) {
  //   _toUnrolledPreds ~= pred;
  // }
  
  // Scope for foreach
  CstScope _rootScope;
  CstScope _currentScope;

  void pushScope(CstIterator iter) {
    assert (_currentScope !is null);
    _currentScope = _currentScope.push(iter);
  }

  void popScope() {
    assert (_currentScope !is null);
    assert (_currentScope !is _rootScope);
    _currentScope = _currentScope.pop();
  }

  CstScope currentScope() {
    assert (_currentScope !is null);
    return _currentScope;
  }

  CstVecPrim[void*] _cstVecPrimes;

  CstVecPrim getVecPrime(void* ptr) {
    CstVecPrim* p = ptr in _cstVecPrimes;
    return (p !is null) ? *p : null;
  }
  
  void addVecPrime(void* ptr, CstVecPrim p) {
    _cstVecPrimes[ptr] = p;
  }

}

class CstScope {
  this(CstScope parent, CstIterator iter) {
    _parent = parent;
    _iter = iter;
    if (_parent !is null) parent._children ~= this;
    if (_parent is null) _level = 0;
    else _level = _parent.getLevel() + 1;
  }

  CstScope pop() {
    return _parent;
  }

  CstScope push(CstIterator iter) {
    CstScope childScope;
    foreach (child; _children) {
      if (child._iter is iter) {
	childScope = child;
	break;
      }
    }
    if (childScope is null) {
      childScope = new CstScope(this, iter);
    }
    return childScope;
  }

  uint getLevel() {
    return _level;
  }

  void getIterators(ref CstIterator[] iters, uint level) {
    if (_level == level) return;
    else {
      assert (_iter !is null);
      assert (_parent !is null);
      _parent.getIterators(iters, level);
      iters ~= _iter;
    }
  }

  CstScope _parent;
  CstScope[] _children;
  CstIterator _iter;
  uint _level;

  string describe() {
    import std.string: format;
    string description = format("Scope:\n\tLevel: %s\n\tIter: %s\n",
				_level, (_iter is null) ?
				"NONE" : _iter.name());
    return description;
  }
}

class CstStage {
  CstSolver _solver;
  // List of randomized variables associated with this stage. A
  // variable can be associated with only one stage
  CstDomain[] _domVars;

  CstDomain[] _domains;
  CstValue[]  _values;
  // The Bdd expressions that apply to this stage
  CstPredicate[] _predicates;
  // These are the length variables that this stage will solve
  
  BDD _solveBDD;

  double[uint] _bddDist;
  
  int _id = -1;

  ~this() {
    _solveBDD.reset();
  }
  
  void copyFrom(CstStage from) {
    _domains = from._domains;
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

  bool isSolved() {
    if(_id != -1) return true;
    else return false;
  }


}

// abstract class CstValueAllocator {
//   static CstValueAllocator[] allocators;

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

  public enum State: ubyte
  {   INIT,
      SOLVED
      }

  
  abstract string name();
  abstract ref BddVec bddvec(Buddy buddy);
  // abstract void bddvec(BddVec b);
  // abstract void collate(ulong v, int word=0);
  abstract void setVal(ulong[] v);
  abstract void setVal(ulong v);
  abstract bool solveRange(_esdl__RandGen randGen);
  abstract CstStage stage();
  abstract void stage(CstStage s);
  abstract uint domIndex();
  abstract void domIndex(uint s);
  abstract bool signed();
  abstract bool isRand();
  abstract uint bitcount();
  abstract void reset();
  abstract _esdl__Proxy getProxyRoot();
  // abstract BDD getPrimBdd(Buddy buddy);
  abstract void _esdl__doRandomize(_esdl__RandGen randGen);
  abstract void _esdl__doRandomize(_esdl__RandGen randGen, CstStage stage);
  abstract CstDomain getResolved();
  abstract bool updateVal();
  abstract bool hasChanged();
  abstract bool isStatic();
  abstract void registerRndPred(CstPredicate rndPred);  
  abstract void registerVarPred(CstPredicate varPred);  
  abstract void registerDepPred(CstDepCallback depCb);
  abstract void registerIdxPred(CstDepCallback idxCb);

  abstract long value();
  
  final void _esdl__doRandomize() {
    this._esdl__doRandomize(getProxyRoot()._esdl__getRandGen());
  }

  final void randIfNoCst() {
    if (! isSolved()) {
      if (_rndPreds.length == 0) {
	_esdl__doRandomize();
      }
    }
  }

  void markSolved() {
    debug(CSTDOMAINS) {
      import std.stdio;
      stderr.writeln(this.describe());
    }
    _tempPreds.reset();
    _state = State.SOLVED;
  }

  final bool isSolved() {
    if (isRand()) {
      if (_state == State.SOLVED) return true;
      else return false;
    }
    else return true;
  }

  // Callbacks
  CstDepCallback[] _depCbs;

  CstPredicate[] _rndPreds;
  CstPredicate[] _varPreds;

  IntRS _rangeSet;

  Folder!(CstPredicate, "tempPreds") _tempPreds;

  CstPredGroup _group;

  void setGroupContext(CstPredGroup group) {
    if (_group is null && (! this.isSolved())) {
      _group = group;
      foreach (pred; _rndPreds) {
	pred.setGroupContext(group);
      }
    }
  }

  // init value has to be different from proxy._cycle init value
  uint _cycle = -1;
  State _state;
  uint _unresolveLap;

  DomType _type = DomType.TRUEMONO;

  void markAsUnresolved(uint lap) {
    if (_unresolveLap != lap) {
      _unresolveLap = lap;
      foreach (pred; _rndPreds) {
	pred.markAsUnresolved(lap);
      }
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

  abstract string describe();

  CstSolverDomain _solverDomain;

  CstSolverValue  _solverValue;
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
  abstract void solveBefore(CstVecPrim other);
  abstract void addPreRequisite(CstVecPrim other);
}

interface CstExpr
{
  string describe();

  abstract uint resolveLap();
  abstract void resolveLap(uint lap);

  abstract void setDomainContext(CstPredicate pred,
				 ref CstDomain[] rnds,
				 ref CstDomain[] vars,
				 ref CstValue[] vals,
				 ref CstIterator[] iters,
				 ref CstDomain[] idxs,
				 ref CstDomain[] deps);

  abstract bool isSolved();
  abstract void annotate(ref uint varN);

  abstract void visit(CstSolver solver);


  abstract void writeExprString(ref Charbuf str);
}

interface CstVecExpr: CstExpr
{
  abstract bool isConst();
  abstract bool isIterator();
  
  abstract BddVec getBDD(CstStage stage, Buddy buddy);

  abstract long evaluate();

  abstract CstVecExpr unroll(CstIterator iter, uint n);

  abstract bool isOrderingExpr();

  abstract bool getIntRange(ref IntR iRange);
  abstract bool getUniRange(ref UniRange rs);

  // get the number of bits and the sign information of an expression
  abstract bool getIntType(ref INTTYPE iType);

}

interface CstLogicExpr: CstExpr
{
  abstract bool getIntRangeSet(ref IntRS iRangeSet);

  abstract bool getUniRangeSet(ref IntRS rs);
  abstract bool getUniRangeSet(ref UIntRS rs);
  abstract bool getUniRangeSet(ref LongRS rs);
  abstract bool getUniRangeSet(ref ULongRS rs);

  abstract CstLogicExpr unroll(CstIterator iter, uint n);

  abstract BDD getBDD(CstStage stage, Buddy buddy);

}


// This class represents an unwound Foreach iter at vec level
abstract class CstIterator
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
  abstract uint size();
  abstract string name();
  abstract CstIterator unrollIterator(CstIterator iter, uint n);
  abstract CstDomain getLenVec();
  final bool isUnrollable() {
    return getLenVec().isSolved();
  }
}

class CstPredGroup			// group of related predicates
{
  // solve cycle for which this group is getting processed. If this
  // _cycle matches solver _cycle, that would mean this group is
  // already processed
  uint _cycle;
  
  // List of predicates permanently in this group
  Folder!(CstPredicate, "preds") _preds;
  Folder!(CstPredicate, "dynPreds") _dynPreds;

  // The flag _hasDynamicBinding gets set if there is at least one
  // predicate that has a dynamically resolvable constraint --
  // typically that would mean a random variable dependancy as part of index 
  bool _hasDynamicBinding;

  Folder!(CstDomain, "doms") _doms;
  void addDomain(CstDomain dom) {
    _doms ~= dom;
  }
  
  Folder!(CstDomain, "vars") _vars;
  void addVariable(CstDomain var) {
    _vars ~= var;
  }

  // If there are groups that are related. This will only be true if
  // the _hasDynamicBinding flag is true
  Folder!(CstPredGroup, "boundGroups") _boundGroups;

  public enum State: ubyte
  {   INIT,
      SCHEDULED,
      SOLVED
      }

  State _state;
  
  void reset() {
    _state = State.INIT;
  }
}

class CstPredicate: CstIterCallback, CstDepCallback
{
  string name() {
    import std.conv;
    if (_parent is null) {
      return _constraint.name() ~ '/' ~ _statement.to!string;
    }
    else {
      return _parent.name() ~ '[' ~ _unrollIterVal.to!string ~ ']';
    }
  }

  // alias _expr this;

  _esdl__ConstraintBase _constraint;
  uint _statement;
  _esdl__ProxyRoot _proxy;
  CstScope _scope;
  CstLogicExpr _expr;
  CstPredicate _parent;
  uint _level;
  uint _unrollCycle;
  bool _markResolve;

  Folder!(CstPredicate, "uwPreds") _uwPreds;
  size_t _uwLength;
  
  this(_esdl__ConstraintBase cst, uint stmt, _esdl__ProxyRoot proxy, CstLogicExpr expr,
       CstPredicate parent=null, CstIterator unrollIter=null, uint unrollIterVal=0// ,
       // CstIterator[] iters ...
       ) {
    assert(proxy !is null);
    _constraint = cst;
    _statement = stmt;
    _proxy = proxy;
    _unrollIterVal = unrollIterVal;
    if (parent is null) {
      _scope = _proxy.currentScope();
      _level = 0;
    }
    else {
      _scope = parent._scope;
      _level = parent._level + 1;
    }
    assert(_scope !is null);
    _expr = expr;


    _parent = parent;
    
    if (_parent is null) {
      _scope.getIterators(_parsedIters, _level);
    }
    else {
      _parsedIters =
	_parent._iters[1..$].
	map!(tr => tr.unrollIterator(unrollIter,
				     unrollIterVal)).array;
    }
      
    this.setDomainContext();
    debug(CSTPREDS) {
      import std.stdio;
      stderr.writeln(this.describe());
    }
  }

  _esdl__Proxy getProxy()() {
    assert(_proxy !is null);
    return _proxy;
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
    if (_unrollCycle == _proxy._cycle) { // already executed
      return;
    }
    // check if all the dependencies are resolved
    foreach (dep; _deps) {
      if (! dep.isSolved()) {
	return;
      }
    }
    CstIterator iter = _iters[0];
    if (iter.getLenVec().isSolved()) {
      this.unroll(iter);
      _unrollCycle = _proxy._cycle;
    }
  }
  
  void unroll(CstIterator iter) {
    assert (iter is _iters[0]);

    if (! iter.isUnrollable()) {
      assert(false, "CstIterator is not unrollabe yet: "
	     ~ this.describe());
    }
    auto currLen = iter.size();
    // import std.stdio;
    // writeln("size is ", currLen);

    if (currLen > _uwPreds.length) {
      // import std.stdio;
      // writeln("Need to unroll ", currLen - _uwPreds.length, " times");
      for (uint i = cast(uint) _uwPreds.length;
	   i != currLen; ++i) {
	_uwPreds ~= new CstPredicate(_constraint, _statement, _proxy, _expr.unroll(iter, i),
				     this, iter, i// ,
				     // _iters[1..$].map!(tr => tr.unrollIterator(iter, i)).array
				     );
      }
    }

    // Do not use foreach here since we may have more elements in the
    // array than the current value of currLen
    for (size_t i=0; i!=currLen; ++i) {
      _proxy.addUnrolledPredicate(_uwPreds[i]);
    }

    _uwLength = currLen;
  }

  final bool isResolved(bool force=false) {
    if (_markResolve || force) {
      _markResolve = false;
      foreach (dep; _deps) {
	if (! dep.isSolved()) {
	  return false;
	}
      }
      // All _idxs are rolled into _deps
      // foreach (idx; _idxs) {
      // 	if (! idx.isSolved()) {
      // 	  return false;
      // 	}
      // }
      return true;
    }
    return false;
  }
  
  CstDomain[] _rnds;
  CstDomain[] _dynRnds;
  CstDomain[] _vars;
  CstValue[]  _vals;
  CstDomain[] _deps;
  CstDomain[] _idxs;
  CstIterator[] _iters;
  CstIterator[] _parsedIters;

  CstIterator _unrollIter;
  uint _unrollIterVal;

  uint _unresolveLap;

  final CstDomain[] getRnds() {
    return _rnds;
  }

  final CstDomain[] getVars() {
    return _vars;
  }

  final CstValue[] getVals() {
    return _vals;
  }

  final CstDomain[] getDomains() {
    return _rnds;
  }

  final void randomizeDepsRolled() {
    for (size_t i=0; i!=_uwLength; ++i) {
      _uwPreds[i].randomizeDeps();
    }
  }

  // final void markAsUnresolvedRolled(uint lap) {
  //   if (this.isRolled()) {
  //     this.markAsUnresolved(lap);
  //   }
  //   // else if (_iters.length > 1) {
  //   //   for (size_t i=0; i!=_uwLength; ++i) {
  //   // 	_uwPreds[i].markAsUnresolvedRolled(lap);
  //   //   }
  //   // }
  // }
  
  final void markAsUnresolved(uint lap) {
    if (_unresolveLap != lap) {	 // already marked -- avoid infinite recursion
      _unresolveLap = lap;
      foreach (rnd; _rnds) {
	rnd.markAsUnresolved(lap);
      }
    }
  }

  final bool isMarkedUnresolved(uint lap) {
    if (_parent !is null) {
      if (_parent.isMarkedUnresolved(lap)) {
	return true;
      }
    }
    return (_unresolveLap == lap);
  }

  // final bool markIfUnresolved(uint lap) {
  //   if (_deps.length > 0 || _iter !is null) {
  //     this.markAsUnresolved(lap);
  //     return true;
  //   }
  //   return false;
  // }

  final bool isRolled() {
    if (this._iters.length > 0 &&
	_unrollCycle != _proxy._cycle) {
      return true;
    }
    return false;
  }
  
  final bool hasDeps() {
    return this._deps.length > 0;
  }

  final bool solvable() {
    return _deps.length == 0 && _iters.length == 0;
  }
  
  bool hasDynamicBinding() {
    return _dynRnds.length > 0;
  }

  final void setDomainContext() {
    CstIterator[] varIters;
    
    _expr.setDomainContext(this, _rnds, _vars, _vals, varIters, _idxs, _deps);

    // foreach (varIter; varIters) {
    //   import std.stdio;
    //   stderr.writeln("Found Iterator: ", varIter.name());
    // }
    // if (_iters.length > 0) {
    //   _len = _iters[0].getLenVec();
    // }
    foreach (rnd; _rnds) {
      rnd.registerRndPred(this);
      if (! rnd.isStatic()) {
	_dynRnds ~= rnd;
      }
    }
    foreach (var; _vars) var.registerVarPred(this);

    assert(_rnds.length != 0);
    if (_rnds.length > 1) {
      foreach (rnd; _rnds) {
	rnd._type = DomType.MULTI;
      }
    }
    else {
      assert(_rnds.length == 1);
      auto rnd = _rnds[0];
      if (rnd._type == DomType.TRUEMONO) {
	if (_vars.length > 0) {
	  rnd._type = DomType.LAZYMONO;
	}
	if (_idxs.length > 0) {
	  assert(! rnd.isStatic());
	  rnd._type = DomType.INDEXEDMONO;
	}
      }
    }

    // When the parent unrolls, its dependencies would already be take care of
    // if (_parent !is null) {
    //   CstDomain[] _foundDeps = _deps ~ _idxs;
    //   _deps = _foundDeps.filter!(dep => (! canFind(_parent._deps, dep))).array;
    // }
    _deps = _deps ~ _idxs;
    
    foreach (dep; _deps) dep.registerDepPred(this);

    // For now treat _idxs as _deps since _idxs are merged with _deps
    // foreach (idx; _idxs) idx.registerIdxPred(this);

    // take only the parsed iterators that are found in the expression
    // as well
    // _iters = pasredIters.filter!(itr =>
    // 				 canFind(varIters, itr)).array;
    _iters = _parsedIters.filter!(itr =>
				  canFind!((CstIterator a, CstIterator b) => a == b)
				  (varIters, itr)).array;
    
    if (_iters.length != 0) _iters[0].registerRolled(this);
  }

  CstLogicExpr getExpr() {
    return _expr;
  }

  void randomizeDeps() {
    foreach (dep; _deps ~ _idxs) {
      dep.randIfNoCst();
    }
  }

  bool hasUpdate() {
    foreach (var; _vars) {
      if (var.hasChanged()) {
	return true;
      }
    }
    foreach (idx; _idxs) {
      if (idx.hasChanged()) {
	return true;
      }
    }
    return false;
  }

  void annotate() {
    uint varN = 0;
    _expr.annotate(varN);
  }
  
  string describe() {
    import std.string:format;
    string description = "Expr: " ~ _expr.describe() ~ "\n    ";
    description ~= _scope.describe();
    description ~= format("    Level: %s\n", _level);
    if (_iters.length > 0) {
      description ~= "    Iterators: \n";
      foreach (iter; _iters) {
	description ~= "\t" ~ iter.name() ~ "\n";
      }
    }
    if (_rnds.length > 0) {
      description ~= "    Domains: \n";
      foreach (rnd; _rnds) {
	description ~= "\t" ~ rnd.name() ~ "\n";
      }
    }
    if (_dynRnds.length > 0) {
      description ~= "    Dyn Domains: \n";
      foreach (rnd; _dynRnds) {
	description ~= "\t" ~ rnd.name() ~ "\n";
      }
    }
    if (_vars.length > 0) {
      description ~= "    Variables: \n";
      foreach (var; _vars) {
	description ~= "\t" ~ var.name() ~ "\n";
      }
    }
    if (_vals.length > 0) {
      description ~= "    Values: \n";
      foreach (val; _vals) {
	description ~= "\t" ~ val.describe() ~ "\n";
      }
    }
    if (_idxs.length > 0) {
      description ~= "    Indexes: \n";
      foreach (idx; _idxs) {
	description ~= "\t" ~ idx.name() ~ "\n";
      }
    }
    if (_deps.length > 0) {
      description ~= "    Depends: \n";
      foreach (dep; _deps) {
	description ~= "\t" ~ dep.name() ~ "\n";
      }
    }
    description ~= "\n";
    return description;
  }

  CstPredGroup _group;

  CstPredGroup group() {
    return _group;
  }

  void setGroupContext(CstPredGroup group) {
    if (_group is null) {
      _group = group;
      foreach (dom; _rnds) {
	dom.setGroupContext(group);
      }
    }
  }

  void solve() {
    if (_group is null) {
    }
  }
}

class CstBlock
{
  CstPredicate[] _preds;
  bool[] _booleans;

  string describe() {
    string name_ = "";
    foreach(pred; _preds) {
      name_ ~= " & " ~ pred._expr.describe() ~ "\n";
    }
    return name_;
  }

  void clear() {
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
