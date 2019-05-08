module esdl.rand.vecx;

import esdl.data.bvec;
import esdl.data.bstr;
import std.traits: isIntegral, isBoolean, isArray, isStaticArray, isDynamicArray;

import esdl.rand.obdd;
import esdl.rand.misc;
import esdl.rand.intr;
import esdl.rand.base: CstVecPrim, CstVecExpr,
  CstIteratorBase, DomType, CstStage, CstDomain,
  CstBddExpr, CstPredicate, _esdl__Solver; // CstValAllocator,
import esdl.rand.expr: CstVecLen, CstVecDomain, _esdl__cstVal,
  CstVecTerm, CstIterator;

import esdl.rand.intr: IntRangeSet;

void addToDomains(CstDomain d, ref CstDomain[] domains) {
  bool listed;
  foreach (domain; domains) {
    if (d is domain) {
      listed = true;
      break;
    }
  }
  if (listed is false) {
    domains ~= d;
  }
}

mixin template CstVecMixin() {
  enum HAS_RAND_ATTRIB = (! __traits(isSame, R, _esdl__norand));

  string _name;

  this(string name) {
    _name = name;
  }

  override string name() {
    return _name;
  }

  long evaluate() {
    static if (HAS_RAND_ATTRIB) {
      if(! this.isRand || this.solved()) {
	return value();
      }
      else {
	assert(false, "Error evaluating " ~ _name);
      }
    }
    else {
      return value();
    }
  }

  S to(S)()
    if (is(S == string)) {
      import std.conv;
      static if (HAS_RAND_ATTRIB) {
	if (isRand) {
	  return "RAND#" ~ _name ~ ":" ~ value().to!string();
	}
	else {
	  return "VAL#" ~ _name ~ ":" ~ value().to!string();
	}
      }
      else {
	return "VAR#" ~ _name ~ ":" ~ value().to!string();
      }
    }

  override string toString() {
    return this.to!string();
  }

  static if (HAS_RAND_ATTRIB) {
    CstVecPrim[] _preReqs;
  }

  void solveBefore(CstVecPrim other) {
    static if (HAS_RAND_ATTRIB) {
      other.addPreRequisite(this);
    }
    else {
      assert(false);
    }
  }

  void addPreRequisite(CstVecPrim domain) {
    static if (HAS_RAND_ATTRIB) {
      _preReqs ~= domain;
    }
    else {
      assert(false);
    }
  }

  // implementation of CstVecDomain API
  static if (HAS_RAND_ATTRIB && is(LEAF == enum)) {
    BDD _primBdd;
    override BDD getPrimBdd(Buddy buddy) {
      import std.traits;
      if(_primBdd.isZero()) {
	foreach(e; EnumMembers!LEAF) {
	  _primBdd = _primBdd | this.bddvec(buddy).equ(buddy.buildVec(e));
	}
      }
      return _primBdd;
    }
    override void resetPrimeBdd() {
      _primBdd.reset();
    }
  }
  else {
    override BDD getPrimBdd(Buddy buddy) {
      return buddy.one();
    }
    override void resetPrimeBdd() { }
  }

  override uint bitcount() {
    static if(isBoolean!LEAF)         return 1;
    else static if(isIntegral!LEAF)   return LEAF.sizeof * 8;
    else static if(isBitVector!LEAF)  return LEAF.SIZE;
    else static assert(false, "bitcount can not operate on: " ~ LEAF.stringof);
  }

  override bool signed() {
    static if(isVecSigned!LEAF) {
      return true;
    }
    else  {
      return false;
    }
  }

  ~this() {
    resetPrimeBdd();
  }

  bool getIntRange(ref IntR rng) {
    return true;
  }

  bool getUniRange(ref UniRange rng) {
    INTTYPE iType;
    if (this.getIntType(iType)) {
      rng.map(iType);
      return true;
    }
    else {
      return false;
    }
  }

  bool getIntType(ref INTTYPE iType) {
    static if (isIntegral!LEAF) {
      import std.traits;
      enum bool signed = isSigned!LEAF;
      enum uint bits = LEAF.sizeof * 8;
    }
    else static if (isBitVector!LEAF) {
      enum bool signed = LEAF.ISSIGNED;
      enum uint bits = LEAF.SIZE;
    }
    else {			// bool
      enum signed = false;
      enum bits = 1;
    }
    static if (bits <= 64) {
      final switch (iType) {
      case INTTYPE.UINT: iType = bits <= 32 ?
	  (signed ? INTTYPE.INT : INTTYPE.UINT) :
	(signed ? INTTYPE.LONG : INTTYPE.ULONG);
	break;
      case INTTYPE.INT: iType = bits <= 32 ?
	  INTTYPE.INT : INTTYPE.LONG;
	break;
      case INTTYPE.ULONG: iType = signed ?
	  INTTYPE.LONG : INTTYPE.ULONG;
	break;
      case INTTYPE.LONG: break;
      }
      return true;
    }
    else {
      return false;
    }
  }

}

// T represents the type of the declared array/non-array member
// N represents the level of the array-elements we have to traverse
// for the elements this CstVec represents

class CstVec(V, LEAF, alias R, int N) if(N == 0 && _esdl__ArrOrder!(V, N) == 0):
  CstVecDomain!(LEAF, R), CstVecPrim
    {
      import std.traits;
      import std.range;
      import esdl.data.bvec;
      
      mixin CstVecMixin;

      alias RV = typeof(this);

      V* _var;
      _esdl__Solver _parent;
      
      this(string name, ref V var, _esdl__Solver parent) {
	// import std.stdio;
	// writeln("New vec ", name);
	super();
	_var = &var;
	_parent = parent;
	_root = _parent.getSolverRoot();
	_root.addDomain(this, HAS_RAND_ATTRIB);
      }

      final override bool isActualDomain() {
	return true;		// N == 0
      }

      void _esdl__setValRef(ref V var) {
	_var = &var;
      }
      
      override _esdl__Solver getSolverRoot() {
	assert(_root !is null);
	return _root;
      }

      override RV getResolved() {
	return this;
      }

      // RV
      CstVecExpr unroll(CstIteratorBase iter, uint n) {
	return this;
      }

      override LEAF* getRef() {
	return _var;
      }

      long value() {
	return cast(long) (*_var);
      }

      bool refresh(CstStage s, Buddy buddy) {
	static if (HAS_RAND_ATTRIB) {
	  assert (stage(), "Stage not set for " ~ this.name());
	  if (this.isRand && s is stage()) {
	    return false;
	  }
	  else if ((! this.isRand) ||
		   this.isRand && stage().solved()) { // work with the value
	    return refreshVal(buddy);
	  }
	  else {
	    assert(false, "Constraint evaluation in wrong stage");
	  }
	}
	else {
	  return refreshVal(buddy);
	}
      }
  
      private bool refreshVal(Buddy buddy) {
	auto val = *(getRef());
	if (_valvec.isNull ||
	    val != _refreshedVal) {
	  _valvec.buildVec(buddy, val);
	  _refreshedVal = val;
	  return true;
	}
	else {
	  return false;
	}
      }
  
      BddVec getBDD(CstStage s, Buddy buddy) {
	static if (HAS_RAND_ATTRIB) {
	  assert(stage(), "Stage not set for " ~ this.name());
	  if(this.isRand && s is stage()) {
	    return bddvec(buddy);
	    // return _domvec;
	  }
	  else if((! this.isRand) ||
		  this.isRand && stage().solved()) { // work with the value
	    return _valvec;
	  }
	  else {
	    assert(false, "Constraint evaluation in wrong stage");
	  }
	}
	else {
	  return _valvec;
	}
      }

      bool isConst() {
	return false;
      }

      bool isIterator() {
	return false;
      }

      bool isOrderingExpr() {
	return false;		// only CstVecOrderingExpr return true
      }

      void setBddContext(CstPredicate pred,
			 ref CstDomain[] vars,
			 ref CstDomain[] vals,
			 ref CstIteratorBase[] iters,
			 ref CstDomain[] idxs,
			 ref CstDomain[] deps) {
	static if (is (R: _esdl__norand)) {
	  addToDomains(this, vals);
	}
	else {
	  // markAbstractVecDomains(false);
	  addToDomains(this, vars);
	}
      }

      final override void markAsUnresolved(uint lap) {
	if (_unresolveLap != lap) {
	  _unresolveLap = lap;
	  foreach (pred; _varPreds) {
	    pred.markAsUnresolved(lap);
	  }
	}
      }
      
      override bool hasAbstractVecDomains() {
	return false;
      }

      override void markAbstractVecDomains(bool len) {
	assert(len is false);
	return;
      }

      void _esdl__reset() {
	static if (HAS_RAND_ATTRIB) {
	  _stage = null;
	  _resolveLap = 0;
	}
      }

    }

class CstVec(V, LEAF, alias R, int N) if(N != 0 && _esdl__ArrOrder!(V, N) == 0):
  CstVecDomain!(LEAF, R), CstVecPrim
    {
      import std.traits;
      import std.range;
      import esdl.data.bvec;

      mixin CstVecMixin;
      
      alias RV = typeof(this);
      alias P = CstVecArr!(V, LEAF, R, N-1);
      P _parent;

      CstVecExpr _indexExpr = null;
      int _pindex = 0;

      uint _resolvedCycle;	// cycle for which indexExpr has been resolved
      RV _resolvedVec;

      this(string name, P parent, CstVecExpr indexExpr) {
	// import std.stdio;
	// writeln("New ", name);
	assert(parent !is null);
	_parent = parent;
	_indexExpr = indexExpr;
	_root = _parent.getSolverRoot();
	// only concrete elements need be added
	// getSolverRoot().addRndDomain(this);
      }

      this(string name, P parent, uint index) {
	// import std.stdio;
	// writeln("New ", name);
	assert(parent !is null);
	_parent = parent;
	// _indexExpr = _esdl__cstVal(index);
	_pindex = index;
	_root = _parent.getSolverRoot();
	
	if (this.isActualDomain()) {
	  _root.addDomain(this, HAS_RAND_ATTRIB);
	}
      }

      override bool opEquals(Object other) {
	auto rhs = cast(RV) other;
	if (rhs is null) return false;
	else return (_parent == rhs._parent && _indexExpr == _indexExpr);
      }
      
      final override bool isActualDomain() {
	return ((_indexExpr is null ||
		 _indexExpr.isIterator ||
		 _indexExpr.isConst) &&
		_parent.isActualDomain());
      }

      override _esdl__Solver getSolverRoot() {
	assert(_root !is null);
	return _root;
      }

      override RV getResolved() {
	if (_resolvedCycle != getSolverRoot()._cycle) {
	  auto parent = _parent.getResolved();
	  if (_indexExpr) {
	    _resolvedVec = parent[cast(size_t) _indexExpr.evaluate()];
	  }
	  else {
	    _resolvedVec = parent[_pindex];
	  }
	  _resolvedCycle = getSolverRoot()._cycle;
	}
	return _resolvedVec;
      }

      // RV
      CstVecExpr unroll(CstIteratorBase iter, uint n) {
	if(_indexExpr) {
	  return _parent.unroll(iter,n)[_indexExpr.unroll(iter,n)];
	}
	else {
	  return _parent.unroll(iter,n)[_pindex];
	}
      }
      
      override LEAF* getRef() {
	if(_indexExpr) {
	  return _parent.getRef(cast(size_t) _indexExpr.evaluate());
	}
	else {
	  return _parent.getRef(this._pindex);
	}
      }

      long value() {
      	if(_indexExpr) {
      	  return *(_parent.getRef(_indexExpr.evaluate()));
      	}
      	else {
      	  return *(_parent.getRef(this._pindex));
      	}
      }

      RV flatten() {
	if (_indexExpr !is null) {
	  return _parent.flatten()[cast(size_t) _indexExpr.evaluate()];
	}
	else {
	  return this;
	}
      }

      bool refresh(CstStage s, Buddy buddy) {
	static if (HAS_RAND_ATTRIB) {
	  if (_indexExpr !is null) {
	    return flatten().refresh(s, buddy);
	  }
	  else {
	    assert(stage(), "Stage not set for " ~ this.name());
	    if(this.isRand && s is stage()) {
	      return false;
	    }
	    else if((! this.isRand) ||
		    this.isRand && stage().solved()) { // work with the value
	      return refreshVal(buddy);
	    }
	    else {
	      assert(false, "Constraint evaluation in wrong stage");
	    }
	  }
	}
	else {
	  return refreshVal(buddy);
	}
      }
  
      private bool refreshVal(Buddy buddy) {
	auto val = *(getRef());
	if (_valvec.isNull ||
	    val != _refreshedVal) {
	  _valvec.buildVec(buddy, val);
	  _refreshedVal = val;
	  return true;
	}
	else {
	  return false;
	}
      }
  
      BddVec getBDD(CstStage s, Buddy buddy) {
	static if (HAS_RAND_ATTRIB) {
	  if (_indexExpr !is null) {
	    auto dvec = _parent[cast(size_t) _indexExpr.evaluate()];
	    return dvec.getBDD(s, buddy);
	  }
	  else {
	    assert(stage(), "Stage not set for " ~ this.name());
	    if(this.isRand && s is stage()) {
	      return bddvec(buddy);
	      // return _domvec;
	    }
	    else if((! this.isRand) ||
		    this.isRand && stage().solved()) { // work with the value
	      return _valvec;
	    }
	    else {
	      assert(false, "Constraint evaluation in wrong stage");
	    }
	  }
	}
	else {
	  return _valvec;
	}
      }

      bool isConst() {
	return false;
      }

      bool isIterator() {
	return false;
      }

      bool isOrderingExpr() {
	return false;		// only CstVecOrderingExpr return true
      }

      void setBddContext(CstPredicate pred,
			 ref CstDomain[] vars,
			 ref CstDomain[] vals,
			 ref CstIteratorBase[] iters,
			 ref CstDomain[] idxs,
			 ref CstDomain[] deps) {
	static if (is (R: _esdl__norand)) {
	  addToDomains(this, vals);
	}
	else {
	  markAbstractVecDomains(false);
	  addToDomains(this, vars);
	}
	if (_parent.isActualDomain()) {
	  deps ~= _parent._arrLen;
	}
	_parent.setBddContext(pred, vars, vals, iters, idxs, deps);

	if (_indexExpr !is null) {
	  // Here we need to put the parent as a dep for the pred
	  // and since this prim needs resolution, the constituents of
	  // the indexExpr need to trigger a function that finds out
	  // whether the _indexExpr has been fully resolved or
	  // not. When the indexExpr gets resolved, it should inform
	  // the parent about resolution which in turn should inform
	  // the pred that it can go ahead
	  _indexExpr.setBddContext(pred, idxs, vals, iters, idxs, deps);
	}
      }

      override void markAsUnresolved(uint lap) {
	if (isActualDomain()) {
	  if (_unresolveLap != lap) {
	    _unresolveLap = lap;
	    foreach (pred; _varPreds) {
	      pred.markAsUnresolved(lap);
	    }
	  }
	}
	else {
	  _parent.markAsUnresolved(lap);
	}
      }

      override bool hasAbstractVecDomains() {
	return _parent.hasAbstractVecDomains();
      }

      override void markAbstractVecDomains(bool len) {
	assert(len is false);
	if (this.isActualDomain()) {
	  return;
	}
	else {
	  _parent.markAbstractVecDomains(len);
	}
      }

      void labelAbstractVecDomains(bool len) {
	assert(len is false);
	if (this._type !is DomType.MULTI) {
	  this._type = DomType.MAYBEMONO;
	}
      }

      void _esdl__reset() {
	static if (HAS_RAND_ATTRIB) {
	  _stage = null;
	  _resolveLap = 0;
	}
      }

    }

// Arrays (Multidimensional arrays as well)
// template CstVecArrBase(T, int I, int N=0)
//   if(_esdl__ArrOrder!(T, I, N) != 0) {
//   alias CstVecArrBase = CstVecArrBase!(typeof(T.tupleof[I]),
// 					   getRandAttr!(T, I), N);
// }

abstract class CstVecArrBase(V, LEAF, alias R, int N)
  if(_esdl__ArrOrder!(V, N) != 0): CstVecPrim
{
  enum HAS_RAND_ATTRIB = (! __traits(isSame, R, _esdl__norand));

  alias L = ElementTypeN!(V, N);
  alias E = ElementTypeN!(V, N+1);

  static if (_esdl__ArrOrder!(V, N+1) == 0) {
    alias EV = CstVec!(V, LEAF, R, N+1);
  }
  else {
    alias EV = CstVecArr!(V, LEAF, R, N+1);
  }

  EV[] _elems;

  string _name;

  bool _hasAbstractVecDomains;
  bool _hasAbstractLenDomains;

  override string name() {
    return _name;
  }

  bool isRand() {
    assert(false, "isRand not implemented for CstVecArrBase");
  }

  static if (HAS_RAND_ATTRIB) {
    CstVecPrim[] _preReqs;
  }

  void solveBefore(CstVecPrim other) {
    static if (HAS_RAND_ATTRIB) {
      other.addPreRequisite(this);
    }
    else {
      assert(false);
    }
  }

  void addPreRequisite(CstVecPrim prim) {
    static if (HAS_RAND_ATTRIB) {
      _preReqs ~= prim;
    }
    else {
      assert(false);
    }
  }

}

// template CstVecArr(T, int I, int N=0)
//   if(_esdl__ArrOrder!(T, I, N) != 0) {
//   alias CstVecArr = CstVecArr!(typeof(T.tupleof[I]), LeafElementType!(T.tupleof[I]),
// 			       getRandAttr!(T, I), N);
// }

// Arrays (Multidimensional arrays as well)
class CstVecArr(V, LEAF, alias R, int N) if(N == 0 && _esdl__ArrOrder!(V, N) != 0):
  CstVecArrBase!(V, LEAF, R, N)
    {
      alias RV = typeof(this);
      CstVecLen!RV _arrLen;

      alias RAND=R;

      V* _var;
      _esdl__Solver _parent;
      _esdl__Solver _root;
    
      void _esdl__setValRef(ref V var) {
	_var = &var;
      }
      
      this(string name, ref V var, _esdl__Solver parent) {
	_name = name;
	_var = &var;
	_parent = parent;
	_root = _parent.getSolverRoot();
	_arrLen = new CstVecLen!RV(name ~ ".len", this);
	if (_hasAbstractLenDomains) {
	  _arrLen.labelAbstractVecDomains(true);
	}
      }

      final bool isActualDomain() {
	return true; 		// N == 0
      }

      _esdl__Solver getSolverRoot() {
	assert(_root !is null);
	return _root;
      }

      RV getResolved() {
	return this;
      }

      RV unroll(CstIteratorBase iter, uint n) {
	return this;
      }

      RV flatten() {
	return this;
      }

      bool parentLenIsUnresolved() {
	return false;		// no parent
      }

      static private auto getRef(A, N...)(ref A arr, N indx)
	if(isArray!A && N.length > 0 && isIntegral!(N[0])) {
	  static if(N.length == 1) return &(arr[indx[0]]);
	  else {
	    return getRef(arr[indx[0]], indx[1..$]);
	  }
	}

      auto getRef(J...)(J indx) if(isIntegral!(J[0])) {
	return getRef(*_var, cast(size_t) indx);
      }

      static if (HAS_RAND_ATTRIB) {
	static private void setLen(A, N...)(ref A arr, size_t v, N indx)
	  if(isArray!A) {
	    static if(N.length == 0) {
	      static if(isDynamicArray!A) {
		arr.length = v;
		// import std.stdio;
		// writeln(arr, " indx: ", N.length);
	      }
	      else {
		assert(false, "Can not set length of a fixed length array");
	      }
	    }
	    else {
	      // import std.stdio;
	      // writeln(arr, " indx: ", N.length);
	      setLen(arr[indx[0]], v, indx[1..$]);
	    }
	  }

	size_t _forcedLength;
	void buildElements(size_t v=size_t.max) {
	  if (v == size_t.max) {
	    v = getLen();
	  }
	  if (! _arrLen.solved()) {
	    if (v > _forcedLength) {
	      _forcedLength = v;
	    }
	  }
	  else {
	    if (v < _forcedLength) {
	      import std.string: format;
	      assert(false,
		     format("%s: Trying to set length %d, while it should be a minimum %d",
			    name(), v, _forcedLength));
	    }
	  }
	  size_t currLen = _elems.length;
	  // import std.stdio;
	  // writeln("Length was ", currLen, " new ", v);
	  if (currLen < v) {
	    _elems.length = v;
	    for (size_t i=currLen; i!=v; ++i) {
	      import std.conv: to;
	      _elems[i] = new EV(_name ~ "[#" ~ i.to!string() ~ "]",
				 this, cast(uint) i);
	      if (_hasAbstractVecDomains) {
		_elems[i].labelAbstractVecDomains(false);
	      }
	    }
	  }
	}
	
	void setLen(N...)(size_t v, N indx) {
	  static if (N.length == 0) {
	    buildElements(v);
	  }
	  setLen(*_var, v, indx);
	}
      }
      else {
	void setLen(N...)(size_t v, N indx) {
	  // setLen(*_var, v, indx);
	  assert(false, "Can not set value for VarVecArr");
	}
      }

      static private size_t getLen(A, N...)(ref A arr, N indx) if(isArray!A) {
	static if(N.length == 0) return arr.length;
	else {
	  if (arr.length == 0) return 0;
	  else return getLen(arr[indx[0]], indx[1..$]);
	}
      }

      size_t getLen(N...)(N indx) {
	return getLen(*_var, indx);
      }

      EV opIndex(CstVecExpr indexExpr) {
	if (indexExpr.isConst()) {
	  size_t index = cast(size_t) indexExpr.evaluate();
	  if (_arrLen.solved()) {
	    if (_arrLen.evaluate() <= index) {
	      assert (false, "Index Out of Range");
	    }
	  }
	  else {
	    buildElements(index+1);
	  }
	  // resolve only if the length has been resolved yet
	  // And when the length is resolved, we must make sure that
	  // unrolling of iterator takes care of such elements as
	  // well which are not explicitly onvoking iterators
	  // assert(_elems.length > 0, "_elems for expr " ~ this.name() ~
	  // 	 " have not been built");
	  return _elems[index];
	}
	else {
	  return new EV(name ~ "[" ~ indexExpr.name() ~ "]", this, indexExpr);
	}
      }

      EV opIndex(size_t index) {
	assert(_elems[index]._indexExpr is null);
	if (_arrLen.solved()) {
	  if (_arrLen.evaluate() <= index) {
	    assert (false, "Index Out of Range");
	  }
	}
	else {
	  buildElements(index+1);
	}
	return _elems[index];
      }

      void _esdl__doRandomize(_esdl__RandGen randGen) {
	static if (HAS_RAND_ATTRIB) {
	  assert(arrLen !is null);
	  buildElements();
	  for(size_t i=0; i != arrLen.evaluate(); ++i) {
	    this[i]._esdl__doRandomize(randGen);
	  }
	}
	else {
	  assert(false);
	}
      }

      void _esdl__doRandomize(_esdl__RandGen randGen, CstStage s) {
	static if (HAS_RAND_ATTRIB) {
	  // assert (stage is s);
	  assert (arrLen !is null);
	  for(size_t i=0; i != arrLen.evaluate(); ++i) {
	    this[i]._esdl__doRandomize(randGen);
	  }
	}
	else {
	  assert(false);
	}
      }

      EV elements() {
	CstIterator!RV iter = arrLen.makeIterVar();
	return this[iter];
      }

      CstIterator!RV iterator() {
	CstIterator!RV iter = arrLen.makeIterVar();
	return iter;
      }

      CstVecLen!RV length() {
	return _arrLen;
      }

      CstVecLen!RV arrLen() {
	return _arrLen;
      }

      void _esdl__reset() {
	_arrLen._esdl__reset();
	foreach(elem; _elems) {
	  if(elem !is null) {
	    elem._esdl__reset();
	  }
	}
      }

      size_t maxArrLen() {
	static if (HAS_RAND_ATTRIB) {
	  static if(isStaticArray!L) {
	    return L.length;
	  }
	  else {
	    return getRandAttrN!(R, N);
	  }
	}
	else {
	  return L.length;
	}
      }

      void setBddContext(CstPredicate pred,
			 ref CstDomain[] vars,
			 ref CstDomain[] vals,
			 ref CstIteratorBase[] iters,
			 ref CstDomain[] idxs,
			 ref CstDomain[] deps) {
	// arrlen should not be handled here. It is handled as part
	// of the indexExpr in the elements when required (that is
	// when indexExpr is not contant, but an expression)
	  
	// auto iter = arrLen.makeIterVar();
	// iters ~= iter;

	// no parent
      }

      void markAsUnresolved(uint lap) {
	foreach(elem; _elems) {
	  elem.markAsUnresolved(lap);
	}
      }

      bool hasAbstractVecDomains() {
	return _hasAbstractVecDomains;
      }

      void markAbstractVecDomains(bool len) {
	labelAbstractVecDomains(len);
      }

      void labelAbstractVecDomains(bool len) {
	if (len is true) {
	  if (_hasAbstractLenDomains is false) {
	    _hasAbstractLenDomains = true;
	    _arrLen.labelAbstractVecDomains(len);
	    foreach(elem; _elems) {
	      elem.labelAbstractVecDomains(len);
	    }
	  }
	}
	else {
	  if (_hasAbstractVecDomains is false) {
	    _hasAbstractVecDomains = true;
	    foreach(elem; _elems) {
	      elem.labelAbstractVecDomains(len);
	    }
	  }
	}
      }
    }

class CstVecArr(V, LEAF, alias R, int N) if(N != 0 && _esdl__ArrOrder!(V, N) != 0):
  CstVecArrBase!(V, LEAF, R, N)
    {
      import std.traits;
      import std.range;
      import esdl.data.bvec;
      alias P = CstVecArr!(V, LEAF, R, N-1);
      P _parent;
      _esdl__Solver _root;
      CstVecExpr _indexExpr = null;
      int _pindex = 0;

      alias RV = typeof(this);
      CstVecLen!RV _arrLen;

      alias RAND=R;
      
      uint _resolvedCycle;	// cycle for which indexExpr has been resolved
      RV _resolvedVec;

      this(string name, P parent, CstVecExpr indexExpr) {
	// import std.stdio;
	// writeln("New ", name);
	assert(parent !is null);
	_name = name;
	_parent = parent;
	_indexExpr = indexExpr;
	_root = _parent.getSolverRoot();
	_arrLen = new CstVecLen!RV(name ~ ".len", this);
	if (_hasAbstractLenDomains) {
	  _arrLen.labelAbstractVecDomains(true);
	}
      }

      this(string name, P parent, uint index) {
	// import std.stdio;
	// writeln("New ", name);
	assert(parent !is null);
	_name = name;
	_parent = parent;
	// _indexExpr = _esdl__cstVal(index);
	_pindex = index;
	_root = _parent.getSolverRoot();
	_arrLen = new CstVecLen!RV(name ~ ".len", this);
	if (_hasAbstractLenDomains) {
	  _arrLen.labelAbstractVecDomains(true);
	}
      }

      override bool opEquals(Object other) {
	auto rhs = cast(RV) other;
	if (rhs is null) return false;
	else return (_parent == rhs._parent && _indexExpr == _indexExpr);
      }
      
      final bool isActualDomain() {
	return ((_indexExpr is null  ||
		 _indexExpr.isIterator ||
		 _indexExpr.isConst) &&
		_parent.isActualDomain());
      }

      _esdl__Solver getSolverRoot() {
	assert(_root !is null);
	return _root;
      }

      bool parentLenIsUnresolved() {
	return (! _parent._arrLen.solved());
      }

      RV getResolved() {
	if (_resolvedCycle != getSolverRoot()._cycle) {
	  auto parent = _parent.getResolved();
	  if (_indexExpr) {
	    _resolvedVec = parent[_indexExpr.evaluate()];
	  }
	  else {
	    _resolvedVec = parent[_pindex];
	  }
	  _resolvedCycle = getSolverRoot()._cycle;
	}
	return _resolvedVec;
      }

      RV flatten() {
	if (_indexExpr !is null) {
	  return _parent.flatten()[cast(size_t)_indexExpr.evaluate()];
	}
	else {
	  return this;
	}
      }

      RV unroll(CstIteratorBase iter, uint n) {
	if(_indexExpr) {
	  return _parent.unroll(iter,n)[_indexExpr.unroll(iter,n)];
	}
	else {
	  return _parent.unroll(iter,n)[_pindex];
	}
      }

      static private size_t getLen(A, N...)(ref A arr, N indx) if(isArray!A) {
	static if(N.length == 0) return arr.length;
	else {
	  return getLen(arr[indx[0]], indx[1..$]);
	}
      }

      static if (HAS_RAND_ATTRIB) {
	static private void setLen(A, N...)(ref A arr, size_t v, N indx)
	  if(isArray!A) {
	    static if(N.length == 0) {
	      static if(isDynamicArray!A) {
		arr.length = v;
		// import std.stdio;
		// writeln(arr, " indx: ", N.length);
	      }
	      else {
		assert(false, "Can not set length of a fixed length array");
	      }
	    }
	    else {
	      // import std.stdio;
	      // writeln(arr, " indx: ", N.length);
	      setLen(arr[indx[0]], v, indx[1..$]);
	    }
	  }

	size_t _forcedLength;
	void buildElements(size_t v=size_t.max) {
	  if (v == size_t.max) {
	    v = getLen();
	  }
	  if (! _arrLen.solved()) {
	    if (v > _forcedLength) {
	      _forcedLength = v;
	    }
	  }
	  else {
	    if (v < _forcedLength) {
	      import std.string: format;
	      assert(false,
		     format("Trying to set length %d, while it should be a minimum %d",
			    v, _forcedLength));
	    }
	  }
	  size_t currLen = _elems.length;
	  if (currLen < v) {
	    _elems.length = v;
	    for (size_t i=currLen; i!=v; ++i) {
	      import std.conv: to;
	      _elems[i] = new EV(_name ~ "[#" ~ i.to!string() ~ "]",
				 this, cast(uint) i);
	      if (_hasAbstractVecDomains) {
		_elems[i].labelAbstractVecDomains(false);
	      }
	    }
	  }
	}

	void setLen(N...)(size_t v, N indx) {
	  static if (N.length == 0) {
	    buildElements(v);
	  }
	  _parent.setLen(v, _pindex, indx);
	}
      }

      size_t getLen(N...)(N indx) {
	return _parent.getLen(_pindex, indx);
      }

      static private auto getRef(A, N...)(ref A arr, N indx)
	if(isArray!A && N.length > 0 && isIntegral!(N[0])) {
	  static if(N.length == 1) return &(arr[indx[0]]);
	  else {
	    return getRef(arr[indx[0]], indx[1..$]);
	  }
	}

      auto getRef(N...)(N indx) if(isIntegral!(N[0])) {
	if(_indexExpr) {
	  assert(_indexExpr.isConst());
	  return _parent.getRef(cast(size_t) _indexExpr.evaluate(), indx);
	}
	else {
	  return _parent.getRef(this._pindex, indx);
	}
      }

      EV opIndex(CstVecExpr indexExpr) {
	if (indexExpr.isConst()) {
	  size_t index = cast(size_t) indexExpr.evaluate();
	  if (_arrLen.solved()) {
	    if (_arrLen.evaluate() <= index) {
	      assert (false, "Index Out of Range");
	    }
	  }
	  else {
	    buildElements(index+1);
	  }
	  return _elems[index];
	}
	else {
	  return new EV(name ~ "[" ~ indexExpr.name() ~ "]", this, indexExpr);
	}
      }

      EV opIndex(size_t index) {
	assert(_elems[index]._indexExpr is null);
	if (_arrLen.solved()) {
	  if (_arrLen.evaluate() <= index) {
	    assert (false, "Index Out of Range");
	  }
	}
	else {
	  buildElements(index+1);
	}
	return _elems[index];
      }

      void _esdl__doRandomize(_esdl__RandGen randGen) {
	static if (HAS_RAND_ATTRIB) {
	  assert(arrLen !is null);
	  for(size_t i=0; i != arrLen.evaluate(); ++i) {
	    this[i]._esdl__doRandomize(randGen);
	  }
	}
	else {
	  assert(false);
	}
      }

      void _esdl__doRandomize(_esdl__RandGen randGen, CstStage s) {
	static if (HAS_RAND_ATTRIB) {
	  // assert (stage is s);
	  assert (arrLen !is null);
	  for (size_t i=0; i != arrLen.evaluate(); ++i) {
	    this[i]._esdl__doRandomize(randGen);
	  }
	}
	else {
	  assert(false);
	}
      }

      EV elements() {
	CstIterator!RV iter = arrLen.makeIterVar();
	return this[iter];
      }

      CstIterator!RV iterator() {
	CstIterator!RV iter = arrLen.makeIterVar();
	return iter;
      }

      CstVecLen!RV length() {
	return _arrLen;
      }

      CstVecLen!RV arrLen() {
	return _arrLen;
      }

      void _esdl__reset() {
	_arrLen._esdl__reset();
	foreach(elem; _elems) {
	  if(elem !is null) {
	    elem._esdl__reset();
	  }
	}
      }

      size_t maxArrLen() {
	static if (HAS_RAND_ATTRIB) {
	  static if(isStaticArray!L) {
	    return L.length;
	  }
	  else {
	    return getRandAttrN!(R, N);
	  }
	}
	else {
	  return L.length;
	}
      }

      void setBddContext(CstPredicate pred,
			 ref CstDomain[] vars,
			 ref CstDomain[] vals,
			 ref CstIteratorBase[] iters,
			 ref CstDomain[] idxs,
			 ref CstDomain[] deps) {
	// arrlen should not be handled here. It is handled as part
	// of the indexExpr in the elements when required (that is
	// when indexExpr is not contant, but an expression)
	  
	// auto iter = arrLen.makeIterVar();
	// iters ~= iter;
	if (_parent.isActualDomain()) {
	  deps ~= _parent._arrLen;
	}
	_parent.setBddContext(pred, vals, vals, iters, idxs, deps);
	if (_indexExpr !is null) {
	  _indexExpr.setBddContext(pred, idxs, vals, iters, idxs, deps);
	}
      }

      void markAsUnresolved(uint lap) {
	if (isActualDomain()) {
	  foreach(elem; _elems) {
	    elem.markAsUnresolved(lap);
	  }
	}
	else {
	  _parent.markAsUnresolved(lap);
	}
      }

      bool hasAbstractVecDomains() {
	return _hasAbstractVecDomains;
      }

      void markAbstractVecDomains(bool len) {
	if (this.isActualDomain()) {
	  labelAbstractVecDomains(len);
	}
	else {
	  _parent.markAbstractVecDomains(len);
	}
      }

      void labelAbstractVecDomains(bool len) {
	if (len is true) {
	  if (_hasAbstractLenDomains is false) {
	    _hasAbstractLenDomains = true;
	    _arrLen.labelAbstractVecDomains(len);
	    foreach(elem; _elems) {
	      elem.labelAbstractVecDomains(len);
	    }
	  }
	}
	else {
	  if (_hasAbstractVecDomains is false) {
	    _hasAbstractVecDomains = true;
	    foreach(elem; _elems) {
	      elem.labelAbstractVecDomains(len);
	    }
	  }
	}
      }
    }
