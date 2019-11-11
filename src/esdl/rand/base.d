module esdl.rand.base;

import esdl.rand.obdd;
import esdl.rand.intr;
import esdl.rand.misc: _esdl__RandGen, isVecSigned;
import esdl.data.bvec: isBitVector;
import esdl.data.folder;
import std.algorithm;
import std.array;
import std.container.array;
import std.traits: isIntegral, isBoolean, isArray, isStaticArray, isDynamicArray;

interface CstVarIntf {
  bool isRand();
  void _esdl__reset();
}

interface CstVecIntf: CstVarIntf {}
interface CstVecArrIntf: CstVarIntf {}

interface CstObjIntf: CstVarIntf {}
interface CstObjArrIntf: CstVarIntf {}


enum DomType: ubyte
{   MONO = 1,
    LAZYMONO = 2,		// like MONO with only some vals that need runtime eval
    MAYBEMONO = 3,
    INDEXEDMONO = 4,
    MULTI = 5
    }

abstract class _esdl__Proxy
{
  Buddy _esdl__buddy;

  Array!uint _domains;		// indexes of domains

  CstDomain[] _cstRndDomains;
  CstDomain[] _cstValDomains;

  uint _domIndex = 0;
  
  // compositional parent -- not inheritance based
  _esdl__Proxy _parent;
  _esdl__Proxy _root;
  
  Folder!CstPredicate _rolledPreds;
  Folder!CstPredicate _unrolledPreds;
  Folder!CstPredicate _toUnrolledPreds;
  Folder!CstPredicate _resolvedPreds;
  Folder!CstPredicate _toSolvePreds;
  Folder!CstPredicate _solvePreds;
  Folder!CstPredicate _unresolvedPreds;
  Folder!CstPredicate _toUnresolvedPreds;

  Folder!CstPredicate _resolvedMonoPreds;
  Folder!CstPredicate _solveMonoPreds;

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
    assert (_esdl__buddy !is null);
    return _esdl__buddy;
  }

  void addDomain(CstDomain domain, bool isRand) {
    if (isRand) {
      _root._cstRndDomains ~= domain;
      useBuddy(_esdl__buddy);
      domain.domIndex = _root._domIndex++;
      _root._domains ~= _esdl__buddy.extDomVec(domain.bitcount);
    }
    else {
      _root._cstValDomains ~= domain;
    }
  }

  void updateValDomains() {
    foreach (dom; _cstValDomains) {
      dom.updateVal();
    }
  }
  
  _esdl__Proxy getProxyRoot()() {
    return _root;
  }

  CstVarIntf[] _esdl__randsList;
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
      _esdl__buddy = new Buddy(400, 400);
      _root = this;
    }
    else {
      _parent = parent;
      _root = _parent.getProxyRoot();
      _esdl__buddy = _root._esdl__buddy;
    }
    // scopes for constraint parsing
    _rootScope = new CstScope(null, null);
    _currentScope = _rootScope;
  }

  bool procMonoDomain(CstPredicate pred) {
    assert (pred._vars.length > 0);
    auto dom = pred._vars[0];
    if (! dom.isSolved()) {
      return dom.solveRange(_esdl__getRandGen());
    }
    else {
      return true;
    }
  }

  bool procMaybeMonoDomain(CstPredicate pred) {
    assert (pred._vars.length > 0);
    if (pred._vars.length > 1) {
      return false;
    }
    auto dom = pred._vars[0];
    if (! dom.isPhysical()) {
      dom = dom.getResolved();
    }
    if (! dom.isSolved()) {
      return dom.solveRange(_esdl__getRandGen());
    }
    else {
      return true;
    }
  }

  void procResolved(CstPredicate pred) {
    assert (pred._vars.length > 0);
    if (pred._vars[0]._type <= DomType.LAZYMONO) {
      _resolvedMonoPreds ~= pred;
      // procMonoDomain(pred._vars[0], pred);
    }
    else {
      foreach (var; pred._vars) {
	if (! var.isPhysical()) {
	  auto dom = var.getResolved();
	  dom._tempPreds ~= pred;
	}
      }
      _resolvedPreds ~= pred;
    }
  }

  void addUnrolled(CstPredicate pred) {
    if (pred.isResolved(true)) {
      procResolved(pred);
    }
    else {
      _unresolvedPreds ~= pred;
    }
  }
  
  void addToUnrolled(CstPredicate pred) {
    _toUnrolledPreds ~= pred;
  }
  
  // Scope for foreach
  CstScope _rootScope;
  CstScope _currentScope;

  void pushScope(CstIteratorBase iter) {
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
  this(CstScope parent, CstIteratorBase iter) {
    _parent = parent;
    _iter = iter;
    if (_parent !is null) parent._children ~= this;
    if (_parent is null) _level = 0;
    else _level = _parent.getLevel() + 1;
  }

  CstScope pop() {
    return _parent;
  }

  CstScope push(CstIteratorBase iter) {
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

  void getIterators(ref CstIteratorBase[] iters, uint level) {
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
  CstIteratorBase _iter;
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
  // List of randomized variables associated with this stage. A
  // variable can be associated with only one stage
  CstDomain[] _domVars;
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

enum DomainState: ubyte
{   INIT,
    ANNOTATED,
    SOLVED
    }

abstract class CstDomain
{
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
  abstract BDD getPrimBdd(Buddy buddy);
  abstract void _esdl__doRandomize(_esdl__RandGen randGen);
  abstract void _esdl__doRandomize(_esdl__RandGen randGen, CstStage stage);
  abstract CstDomain getResolved();
  abstract void updateVal();
  abstract bool hasChanged();
  abstract bool isPhysical();
  abstract void registerVarPred(CstPredicate varPred);  
  abstract void registerValPred(CstPredicate valPred);  
  abstract void registerDepPred(CstDepCallback depCb);
  abstract void registerIdxPred(CstDepCallback idxCb);


  final void _esdl__doRandomize() {
    this._esdl__doRandomize(getProxyRoot()._esdl__getRandGen());
  }

  final void randIfNoCst() {
    if (! isSolved()) {
      if (_varPreds.length == 0) {
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
    _cycle = getProxyRoot()._cycle;
    _state = DomainState.SOLVED;
  }

  final bool isSolved() {
    if (isRand()) {
      if (_cycle == getProxyRoot()._cycle &&
	  _state == DomainState.SOLVED) {
	return true;
      }
      else if (stage() is null) {
	return false;
      }
      else {
	return stage().isSolved();
      }
    }
    else {
      return true;
    }
  }

  final bool isAnnotated() {
    if (isRand()) {
      if (_cycle == getProxyRoot()._cycle &&
	  _state >= DomainState.ANNOTATED) {
	return true;
      }
      else {
	return false;
      }
    }
    else {
      return true;
    }
  }

  // Callbacks
  CstDepCallback[] _depCbs;

  CstPredicate[] _varPreds;
  CstPredicate[] _valPreds;

  IntRS _rangeSet;

  Folder!CstPredicate _tempPreds;

  // init value has to be different from proxy._cycle init value
  uint _cycle = -1;
  DomainState _state;
  uint _unresolveLap;

  DomType _type = DomType.MONO;

  void markAsUnresolved(uint lap) {
    if (_unresolveLap != lap) {
      _unresolveLap = lap;
      foreach (pred; _varPreds) {
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


interface CstVecExpr
{

  // alias evaluate this;

  abstract string name();
  
  // Array of indexes this expression has to resolve before it can be
  // converted into a BDD
  // get the primary (outermost foreach) iterator CstVecExpr

  abstract uint resolveLap();
  abstract void resolveLap(uint lap);
  
  abstract bool isConst();//  {
  //   return false;
  // }

  abstract bool isIterator();
  
  // get the list of stages this expression should be avaluated in
  // abstract CstStage[] getStages();
  abstract BddVec getBDD(CstStage stage, Buddy buddy);

  abstract long evaluate();

  // abstract bool getVal(ref long val);
  
  abstract CstVecExpr unroll(CstIteratorBase iter, uint n);

  abstract bool isOrderingExpr();//  {
  //   return false;		// only CstVecOrderingExpr return true
  // }

  abstract void setDomainContext(CstPredicate pred,
				 ref CstDomain[] vars,
				 ref CstDomain[] vals,
				 ref CstIteratorBase[] iters,
				 ref CstDomain[] idxs,
				 ref CstDomain[] deps);

  abstract bool getIntRange(ref IntR iRange);
  abstract bool getUniRange(ref UniRange rs);

  // get the number of bits and the sign information of an expression
  abstract bool getIntType(ref INTTYPE iType);

  abstract bool isSolved();
  abstract void annotate(ref uint varN);
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
  abstract string name();
  abstract CstIteratorBase unrollIterator(CstIteratorBase iter, uint n);
  abstract CstDomain getLenVec();
  final bool isUnrollable() {
    return getLenVec().isSolved();
  }
}

interface CstBddExpr
{
  string name();

  abstract uint resolveLap();
  abstract void resolveLap(uint lap);

  abstract void setDomainContext(CstPredicate pred,
				 ref CstDomain[] vars,
				 ref CstDomain[] vals,
				 ref CstIteratorBase[] iters,
				 ref CstDomain[] idxs,
				 ref CstDomain[] deps);

  abstract bool getIntRangeSet(ref IntRS iRangeSet);

  abstract bool getUniRangeSet(ref IntRS rs);
  abstract bool getUniRangeSet(ref UIntRS rs);
  abstract bool getUniRangeSet(ref LongRS rs);
  abstract bool getUniRangeSet(ref ULongRS rs);

  abstract CstBddExpr unroll(CstIteratorBase iter, uint n);

  // abstract CstStage[] getStages();

  abstract BDD getBDD(CstStage stage, Buddy buddy);

  abstract bool isSolved();
  abstract void annotate(ref uint varN);
}

class CstPredicate: CstIterCallback, CstDepCallback
{
  string name() {
    return "PREDICATE: " ~ _expr.name();
  }

  // alias _expr this;

  _esdl__Proxy _proxy;
  CstScope _scope;
  uint _level;
  CstBddExpr _expr;
  CstPredicate _parent;
  bool _markResolve;
  uint _unrollCycle;

  Folder!CstPredicate _uwPreds;
  size_t _uwLength;
  
  this(_esdl__Proxy proxy, CstBddExpr expr,
       CstPredicate parent=null, CstIteratorBase unrollIter=null, uint unrollIterVal=0// ,
       // CstIteratorBase[] iters ...
       ) {
    assert(proxy !is null);
    _proxy = proxy;
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
    CstIteratorBase iter = _iters[0];
    if (iter.getLenVec().isSolved()) {
      this.unroll(iter);
      _unrollCycle = _proxy._cycle;
    }
  }
  
  void unroll(CstIteratorBase iter) {
    assert (iter is _iters[0]);

    _proxy.addToUnrolled(this);

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
	_uwPreds ~= new CstPredicate(_proxy, _expr.unroll(iter, i), this, iter, i// ,
				     // _iters[1..$].map!(tr => tr.unrollIterator(iter, i)).array
				     );
      }
    }

    // Do not use foreach here since we may have more elements in the
    // array than the current value of currLen
    if (this._iters.length == 1) {
      for (size_t i=0; i!=currLen; ++i) {
	_proxy.addUnrolled(_uwPreds[i]);
      }
    }

    _uwLength = currLen;
    // return _uwPreds[0..currLen];
  }

  final bool isResolved(bool force=false) {
    if (_markResolve || force) {
      _markResolve = false;
      foreach (dep; _deps) {
	if (! dep.isSolved()) {
	  return false;
	}
      }
      foreach (idx; _idxs) {
	if (! idx.isSolved()) {
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
  CstDomain[] _idxs;
  CstIteratorBase[] _iters;
  CstIteratorBase[] _parsedIters;

  CstIteratorBase _unrollIter;
  uint _unrollIterVal;

  uint _unresolveLap;

  final CstDomain[] getDomains() {
    return _vars;
  }

  final void randomizeDepsRolled() {
    for (size_t i=0; i!=_uwLength; ++i) {
      _uwPreds[i].randomizeDeps();
    }
  }

  final void markAsUnresolvedRolled(uint lap) {
    if (this.isRolled()) {
      this.markAsUnresolved(lap);
    }
    else if (_iters.length > 1) {
      for (size_t i=0; i!=_uwLength; ++i) {
	_uwPreds[i].markAsUnresolvedRolled(lap);
      }
    }
  }
  
  final void markAsUnresolved(uint lap) {
    if (_unresolveLap != lap) {	 // already marked -- avoid infinite recursion
      _unresolveLap = lap;
      foreach (var; _vars) {
	var.markAsUnresolved(lap);
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
  
  final void setDomainContext() {
    CstIteratorBase[] varIters;
    
    _expr.setDomainContext(this, _vars, _vals, varIters, _idxs, _deps);

    // foreach (varIter; varIters) {
    //   import std.stdio;
    //   stderr.writeln("Found Iterator: ", varIter.name());
    // }
    // if (_iters.length > 0) {
    //   _len = _iters[0].getLenVec();
    // }
    foreach (var; _vars) var.registerVarPred(this);
    foreach (val; _vals) val.registerValPred(this);

    assert(_vars.length != 0);
    if (_vars.length > 1) {
      foreach (var; _vars) {
	var._type = DomType.MULTI;
      }
    }
    else {
      assert(_vars.length == 1);
      auto var = _vars[0];
      if (var._type == DomType.MONO) {
	if (_vals.length > 0) {
	  var._type = DomType.LAZYMONO;
	}
	if (_idxs.length > 0) {
	  assert(! var.isPhysical());
	  var._type = DomType.INDEXEDMONO;
	}
      }
    }

    // When the parent unrolls, its dependencies would already be take care of
    if (_parent !is null) {
      CstDomain[] _foundDeps = _deps ~ _idxs;
      _deps = _foundDeps.filter!(dep => (! canFind(_parent._deps, dep))).array;
    }
    
    foreach (dep; _deps) dep.registerDepPred(this);
    foreach (idx; _idxs) idx.registerIdxPred(this);

    // take only the parsed iterators that are found in the expression
    // as well
    // _iters = pasredIters.filter!(itr =>
    // 				 canFind(varIters, itr)).array;
    _iters = _parsedIters.filter!(itr =>
				  canFind!((CstIteratorBase a, CstIteratorBase b) => a == b)
				  (varIters, itr)).array;
    
    if (_iters.length != 0) _iters[0].registerRolled(this);
  }

  CstBddExpr getExpr() {
    return _expr;
  }

  void randomizeDeps() {
    foreach (dep; _deps ~ _idxs) {
      dep.randIfNoCst();
    }
  }

  bool hasUpdate() {
    foreach (val; _vals) {
      if (val.hasChanged()) {
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
    string description = name() ~ "\n    ";
    description ~= _scope.describe();
    description ~= format("    Level: %s\n", _level);
    if (_iters.length > 0) {
      description ~= "    Iterators: \n";
      foreach (iter; _iters) {
	description ~= "\t" ~ iter.name() ~ "\n";
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
	description ~= "\t" ~ val.name() ~ "\n";
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
}

class CstBlock
{
  CstPredicate[] _preds;
  bool[] _booleans;

  string name() {
    string name_ = "";
    foreach(pred; _preds) {
      name_ ~= " & " ~ pred._expr.name() ~ "\n";
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
