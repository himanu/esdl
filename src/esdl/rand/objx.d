module esdl.rand.objx;

import esdl.data.bvec;
import esdl.data.bstr;
import std.traits: isIntegral, isBoolean, isArray, isStaticArray, isDynamicArray;

import esdl.rand.obdd;
import esdl.rand.misc;
import esdl.rand.base: CstVarPrim, CstVarExpr, CstVarIterBase,
  CstStage, CstDomain; // CstValAllocator,
import esdl.rand.expr: CstVarLen, CstVecDomain, _esdl__cstVal;


// Consolidated Proxy Class
// template CstObjBase(T, int I, int N=0) {
//   alias CstObjBase = CstObjBase!(typeof(T.tupleof[I]),
// 				     getRandAttr!(T, I), N);
// }

abstract class CstObjBase(V, alias R, int N)
  if(_esdl__ArrOrder!(V, N) == 0)
{
  enum HAS_RAND_ATTRIB = (! __traits(isSame, R, _esdl__norand));

  alias E = ElementTypeN!(V, N);
  alias RJ = typeof(this);

  string _name;

  ~this() { }

  override string name() {
    return _name;
  }

  void _esdl__reset() {}

  bool isVarArr() {
    return false;
  }

  S to(S)()
    if (is(S == string)) {
      import std.conv;
      static if (HAS_RAND_ATTRIB) {
	if (isRand) {
	  return "RAND#" ~ _name;
	}
	else {
	  return "VAL#" ~ _name;
	}
      }
      else {
	return "VAR#" ~ _name;
      }
    }

  override string toString() {
    return this.to!string();
  }

  void build() {}

  abstract E* getRef();
  
}

// T represents the type of the declared array/non-array member
// N represents the level of the array-elements we have to traverse
// for the elements this CstObj represents
template CstObj(T, int I, int N=0)
  if(_esdl__ArrOrder!(T, I, N) == 0) {
  alias CstObj = CstObj!(typeof(T.tupleof[I]),
			 getRandAttr!(T, I), N);
}

class CstObj(V, alias R, int N) if(N == 0 && _esdl__ArrOrder!(V, N) == 0):
  CstObjBase!(V, R, N)
    {
      import std.traits;
      import std.range;
      import esdl.data.bvec;

      V* _var;

    
      this(string name, ref V var) {
	// import std.stdio;
	// writeln("New ", name);
	_name = name;
	_var = &var;
      }

      override CstVarIterBase[] itrVars() {
	return [];
      }

      override bool hasUnresolvedIdx() {
	return false;
      }
      
      override CstDomain[] getRndDomains(bool resolved) {
	static if (HAS_RAND_ATTRIB) {
	  if(isRand) return [this];
	  else return [];
	}
	else {
	  return [];
	}
      }

      CstDomain[] getDomainLens(bool resolved) {
	assert(false);
      }

      override RJ unroll(CstVarIterBase itr, uint n) {
	// itrVars is always empty
	return this;
      }

      void _esdl__doRandomize(_esdl__RandGen randGen) {
	static if (HAS_RAND_ATTRIB) {
	  if(stage is null) {
	    randGen.gen(*_var);
	  }
	}
	else {
	  assert(false);
	}
      }

      void _esdl__doRandomize(_esdl__RandGen randGen, CstStage s) {
	static if (HAS_RAND_ATTRIB) {
	  assert (stage is s);
	  randGen.gen(*_var);
	}
	else {
	  assert(false);
	}
      }

      override E* getRef() {
	return _var;
      }
    }

class CstObj(V, alias R, int N=0) if(N != 0 && _esdl__ArrOrder!(V, N) == 0):
  CstObjBase!(V, R, N)
    {
      import std.traits;
      import std.range;
      import esdl.data.bvec;

      alias P = CstObjArr!(V, R, N-1);
      P _parent;

      CstVarExpr _indexExpr = null;
      int _index = 0;

      this(string name, P parent, CstVarExpr indexExpr) {
	assert(parent !is null);
	_name = name;
	_parent = parent;
	_indexExpr = indexExpr;
      }

      this(string name, P parent, uint index) {
	assert(parent !is null);
	_name = name;
	_parent = parent;
	_index = index;
      }

      override CstVarIterBase[] itrVars() {
	if(_indexExpr) {
	  return _parent.itrVars() ~ _indexExpr.itrVars();
	}
	else {
	  return _parent.itrVars();
	}
      }

      override bool hasUnresolvedIdx() {
	return _parent.hasUnresolvedIdx(); // no _relatedIdxs for this instance
      }
      
      CstDomain[] getDomainLens(bool resolved) {
	assert(false);
      }

      // What is required here
      // we could be dealing with an _index or an _indexExpr. Further
      // the indexExpr could be either a solvable constraint expression
      // or an array length iterator that iterates over the length of
      // the given array. To add to the complexity here, we could have a
      // case where the given element has a parent that too a
      // non-elementary one (involving non-integer indexes). In such
      // cases we need to list all the elements of the array that could
      // finally represent the given element that we are currently
      // dealing with.
      override CstDomain[] getRndDomains(bool resolved) {
	CstDomain[] domains;
	static if (HAS_RAND_ATTRIB) {
	  if(_indexExpr) {
	    // FIXME -- if the expression has been solved
	    // return _parent.getRndDomains(_indexExpr.evaluate()) ;
	    domains = _indexExpr.getRndDomains(resolved);
	    foreach(pp; _parent.getRndDomainsAtIdx(-1)) {
	      domains ~= pp;
	    }
	  }
	  else {
	    foreach(pp; _parent.getRndDomainsAtIdx(_index)) {
	      domains ~= pp;
	    }
	  }
	  return domains;
	}
	else {
	  if(_indexExpr) {
	    // FIXME -- if the expression has been solved
	    // return _parent.getRndDomainsAtIdx(_indexExpr.evaluate()) ;
	    domains = _indexExpr.getRndDomains(resolved) ~ _parent.getRndDomains(resolved);
	  }
	  else {
	    // foreach(pp; _parent.getRndDomainsAtIdx(_index)) {
	    //   domains ~= pp;
	    // }
	    domains = _parent.getRndDomains(resolved);
	  }
	  return domains;
	}
      }

      override RJ unroll(CstVarIterBase itr, uint n) {
	bool found = false;
	foreach(var; itrVars()) {
	  if(itr is var) {
	    found = true;
	    break;
	  }
	}
	if(! found) return this;
	if(_indexExpr) {
	  return _parent.unroll(itr,n)[_indexExpr.unroll(itr,n)];
	}
	return _parent.unroll(itr,n)[_index];
      }

      void _esdl__doRandomize(_esdl__RandGen randGen, CstStage s) {
	static if (HAS_RAND_ATTRIB) {
	  assert (stage is s);
	  E val;
	  randGen.gen(val);
	  collate(val);
	}
	else {
	  assert(false, name());
	}
      }

      override E* getRef() {
	if(_indexExpr) {
	  return _parent.getRef(cast(size_t) _indexExpr.evaluate());
	}
	else {
	  return _parent.getRef(this._index);
	}
      }
    }

// Arrays (Multidimensional arrays as well)
// template CstObjArrBase(T, int I, int N=0)
//   if(_esdl__ArrOrder!(T, I, N) != 0) {
//   alias CstObjArrBase = CstObjArrBase!(typeof(T.tupleof[I]),
// 					   getRandAttr!(T, I), N);
// }

abstract class CstObjArrBase(V, alias R, int N=0)
  if(_esdl__ArrOrder!(V, N) != 0): CstVarPrim
{
  enum HAS_RAND_ATTRIB = (! __traits(isSame, R, _esdl__norand));

  alias L = ElementTypeN!(V, N);
  alias E = ElementTypeN!(V, N+1);

  static if (_esdl__ArrOrder!(V, N+1) == 0) {
    alias EV = CstObj!(V, R, N+1);
  }
  else {
    alias EV = CstObjArr!(V, R, N+1);
  }

  EV[] _elems;

  string _name;
  override string name() {
    return _name;
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

  bool isVarArr() {
    return true;
  }

  // override CstObj2VecExpr opIndex(CstVarExpr idx) {
  //   return new CstObj2VecExpr(this, idx, CstBinVecOp.IDXINDEX);
  // }

  bool isRand() {
    assert(false, "isRand not implemented for CstObjArrBase");
  }

}

template CstObjArr(T, int I, int N=0)
  if(_esdl__ArrOrder!(T, I, N) != 0) {
  alias CstObjArr = CstObjArr!(typeof(T.tupleof[I]),
			       getRandAttr!(T, I), N);
}

// Arrays (Multidimensional arrays as well)
class CstObjArr(V, alias R, int N=0)
  if(N == 0 && _esdl__ArrOrder!(V, N) != 0):
    CstObjArrBase!(V, R, N)
      {
	alias RJ = typeof(this);
	CstVarLen!RJ _arrLen;

	alias RAND=R;

	V* _var;

	static if (HAS_RAND_ATTRIB) {
	  static if(isStaticArray!V) {
	    static assert(__traits(isSame, R, rand));
	    enum int maxLen = V.length;
	    this(string name, ref V var) {
	      _name = name;
	      _var = &var;
	      _arrLen = new CstVarLen!RJ(name ~ ".len", this);
	      _relatedIdxs ~= _arrLen;
	    }
	  }

	  static if(isDynamicArray!V) {
	    enum int maxLen = getRandAttrN!(R, N);
	    this(string name, ref V var) {
	      _name = name;
	      _var = &var;
	      _arrLen = new CstVarLen!RJ(name ~ ".len", this);
	      _relatedIdxs ~= _arrLen;
	    }
	  }
	}
	else {
	  this(string name, ref V var) {
	    _name = name;
	    _var = &var;
	    _arrLen = new CstVarLen!RJ(name ~ ".len", this);
	    _relatedIdxs ~= _arrLen;
	  }
	}

	CstVarIterBase[] itrVars() {
	  return [];		// N = 0 -- no _parent
	}

	static if (HAS_RAND_ATTRIB) {
	  EV[] getRndDomainsAtIdx(int idx) {
	    if(idx < 0) return _elems;
	    else return [_elems[idx]];
	  }
	}

	CstDomain[] getRndDomains(bool resolved) {
	  static if (HAS_RAND_ATTRIB) {
	    CstDomain[] domains;
	    foreach(elem; _elems) {
	      domains ~= elem;
	    }
	    return domains;
	  }
	  else {
	    return [];
	  }
	}

	CstDomain[] getDomainLens(bool resolved) {
	  static if (HAS_RAND_ATTRIB) {
	    CstDomain[] domains;
	    if(_arrLen.isRand) domains ~= _arrLen;
	    return domains;
	  }
	  else {
	    return [];
	  }
	}
    
	RJ unroll(CstVarIterBase itr, uint n) {
	  return this;
	}

	bool parentLenIsUnresolved() {
	  return false;		// no parent
	}

	bool hasUnresolvedIdx() {
	  foreach (domain; _relatedIdxs) {
	    if (! domain.solved()) {
	      return true;
	    }
	  }
	  return false;		// N=0 -- no _parent
	}
      
	static private auto getRef(A, N...)(ref A arr, N idx)
	  if(isArray!A && N.length > 0 && isIntegral!(N[0])) {
	    static if(N.length == 1) return &(arr[idx[0]]);
	    else {
	      return getRef(arr[idx[0]], idx[1..$]);
	    }
	  }

	auto getRef(J...)(J idx) if(isIntegral!(J[0])) {
	  return getRef(*_var, cast(size_t) idx);
	}

	static if (HAS_RAND_ATTRIB) {

	  static private void setLen(A, N...)(ref A arr, size_t v, N idx)
	    if(isArray!A) {
	      static if(N.length == 0) {
		static if(isDynamicArray!A) {
		  arr.length = v;
		  // import std.stdio;
		  // writeln(arr, " idx: ", N.length);
		}
		else {
		  assert(false, "Can not set length of a fixed length array");
		}
	      }
	      else {
		// import std.stdio;
		// writeln(arr, " idx: ", N.length);
		setLen(arr[idx[0]], v, idx[1..$]);
	      }
	    }
	}
	else {
	  void setLen(N...)(size_t v, N idx) {
	    // setLen(*_var, v, idx);
	    assert(false, "Can not set value for VarVecArr");
	  }
	}

	static private size_t getLen(A, N...)(ref A arr, N idx) if(isArray!A) {
	  static if(N.length == 0) return arr.length;
	  else {
	    return getLen(arr[idx[0]], idx[1..$]);
	  }
	}

	size_t getLen(N...)(N idx) {
	  return getLen(*_var, idx);
	}

	void setLen(N...)(size_t v, N idx) {
	  static if (HAS_RAND_ATTRIB) {
	    setLen(*_var, v, idx);
	  }
	  else {
	    assert(false);
	  }
	}

	bool isUnrollable() {
	  foreach (var; _relatedIdxs) {
	    if (! var.solved()) return false;
	  }
	  return true;
	}

	CstDomain[] _relatedIdxs;
	void addRelatedIdx(CstDomain domain) {
	  foreach (var; _relatedIdxs) {
	    if (domain is var) {
	      return;
	    }
	  }
	  _relatedIdxs ~= domain;
	}
      
	EV opIndex(CstVarExpr idx) {
	  foreach (domain; idx.getRndDomains(resolved)) {
	    addRelatedIdx(domain);
	  }
	  if(idx.isConst()) {
	    assert(_elems.length > 0, "_elems for expr " ~ this.name() ~
		   " have not been built");
	    return _elems[cast(size_t) idx.evaluate()];
	  }
	  else {
	    // static if(isStaticArray!E) {
	    //   // static array
	    return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	    // }
	    // else static if(isDynamicArray!E) {
	    //   // dynamic array
	    //   return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	    // }
	    // else {
	    //   return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	    // }
	  }
	}

	EV opIndex(size_t idx) {
	  build();
	  assert(_elems[idx]._indexExpr is null);
	  return _elems[idx];
	}

	void _esdl__doRandomize(_esdl__RandGen randGen) {
	  static if (HAS_RAND_ATTRIB) {
	    if(_elems.length == 0) this.build();
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
	    if(_elems.length == 0) this.build();
	    assert (stage is s);
	    assert (arrLen !is null);
	    for (size_t i=0; i != arrLen.evaluate(); ++i) {
	      this[i]._esdl__doRandomize(randGen);
	    }
	  }
	  else {
	    assert(false);
	  }
	}

	auto elements() {
	  this.build();
	  auto itr = arrLen.makeItrVar();
	  return this[itr];
	}

	bool built() {
	  return (_elems.length == maxArrLen() &&
		  _elems[0] !is null);
	}
    
	void build() {
	  if(built()) return;
	  _elems.length = maxArrLen();
	  // static if(isIntegral!E || isBitVector!E) {
	  // if(! built()) {
	  for (uint i=0; i!=maxArrLen; ++i) {
	    if(_elems[i] is null) {
	      import std.conv: to;
	      _elems[i] = new EV(_name ~ "[" ~ i.to!string() ~ "]", this, i);
	      if(_elems[i].isVarArr()) {
		_elems[i].build();
	      }
	    }
	  }
	}
	
	auto iterator() {
	  this.build();
	  auto itr = arrLen.makeItrVar();
	  return itr;
	}

	CstVarLen!RJ length() {
	  return _arrLen;
	}

	CstVarLen!RJ arrLen() {
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

	CstStage stage() {
	  return arrLen().stage();
	}

      }

class CstObjArr(V, alias R, int N=0) if(N != 0 && _esdl__ArrOrder!(V, N) != 0):
  CstObjArrBase!(V, R, N)
    {
      import std.traits;
      import std.range;
      import esdl.data.bvec;
      alias P = CstObjArr!(V, R, N-1);
      P _parent;
      CstVarExpr _indexExpr = null;
      int _index = 0;

      alias RJ = typeof(this);
      CstVarLen!RJ _arrLen;

      alias RAND=R;
      
      this(string name, P parent, CstVarExpr indexExpr) {
	assert(parent !is null);
	_name = name;
	_parent = parent;
	_indexExpr = indexExpr;
	_arrLen = new CstVarLen!RJ(name ~ ".len", this);
	_relatedIdxs ~= _arrLen;
      }

      this(string name, P parent, uint index) {
	assert(parent !is null);
	_name = name;
	_parent = parent;
	_index = index;
	_arrLen = new CstVarLen!RJ(name ~ ".len", this);
	_relatedIdxs ~= _arrLen;
      }

      CstDomain[] preReqs() {
	CstDomain[] req;
	static if (HAS_RAND_ATTRIB) {
	  req = _preReqs ~ _parent.arrLen();
	}
	if(_indexExpr) {
	  return req ~ _indexExpr.preReqs() ~ _parent.preReqs();
	}
	else {
	  return req ~ _parent.preReqs();
	}
      }

      CstVarIterBase[] itrVars() {
	if(_indexExpr) {
	  return _parent.itrVars() ~ _indexExpr.itrVars();
	}
	else {
	  return _parent.itrVars();
	}
      }

      bool parentLenIsUnresolved() {
	return (! _parent._arrLen.solved());
      }

      bool hasUnresolvedIdx() {
	foreach (domain; _relatedIdxs) {
	  if (! domain.solved()) {
	    return true;
	  }
	}
	return _parent.hasUnresolvedIdx();
      }

      EV[] getRndDomainsAtIdx(int idx) {
	EV[] domains;
	static if (HAS_RAND_ATTRIB) {
	  if(_indexExpr) {
	    foreach(pp; _parent.getRndDomainsAtIdx(-1)) {
	      if(idx < 0) domains ~= pp._elems;
	      else domains ~= pp._elems[idx];
	    }
	  }
	  else {
	    foreach(pp; _parent.getRndDomainsAtIdx(_index)) {
	      if(idx < 0) domains ~= pp._elems;
	      else domains ~= pp._elems[idx];
	    }
	  }
	}
	return domains;
      }
    
      // This is slightly tricky in case we are pointing directly to
      // just one element of the array, this function should return just
      // that element. But it could be that the index or an upper
      // hierarchy index is not a constant, but an iterator or a
      // randomized epression. In that case, we shall have to return
      // more primary elements
      CstDomain[] getRndDomains(bool resolved) {
	CstDomain[] domains;
	static if (HAS_RAND_ATTRIB) {
	  if(_indexExpr) {
	    domains = _indexExpr.getRndDomains(resolved);
	    foreach(pp; _parent.getRndDomainsAtIdx(-1)) {
	      domains ~= pp;
	    }
	  }
	  else {
	    foreach(pp; _parent.getRndDomainsAtIdx(_index)) {
	      domains ~= pp;
	    }
	  }
	}
	else {
	  domains ~= _parent.getDomainLens(resolved);
	  if(_indexExpr) {
	    domains ~= _indexExpr.getRndDomains(resolved) ~ _parent.getDomainLens(resolved);
	  }
	}
	return domains;
      }

      CstDomain[] getDomainLens(bool resolved) {
	// if(_index.itrVars.length is 0)
	if(_indexExpr is null) {
	  return [_parent[_index].arrLen()];
	  // return domains ~ _parent[_index].getDomainLens();
	}
	if(_indexExpr.isConst()) {
	  CstDomain[] domains;
	  domains ~= _parent[cast(size_t) _indexExpr.evaluate()].getDomainLens(resolved);
	  return domains;
	}
	else {
	  CstDomain[] domains;
	  foreach(p; getRndDomains(resolved)) {
	    // import std.stdio;
	    // writeln(_parent.name(), " ", p.name());
	    domains ~= p.getDomainLens(resolved);
	  }
	  return domains;
	}
      }

      RJ unroll(CstVarIterBase itr, uint n) {
	bool found = false;
	foreach(var; itrVars()) {
	  if(itr is var) {
	    found = true;
	    break;
	  }
	}
	if(! found) return this;
	if(_indexExpr) {
	  return _parent.unroll(itr,n)[_indexExpr.unroll(itr,n)];
	}
	return _parent.unroll(itr,n)[_index];
      }

      static private size_t getLen(A, N...)(ref A arr, N idx) if(isArray!A) {
	static if(N.length == 0) return arr.length;
	else {
	  return getLen(arr[idx[0]], idx[1..$]);
	}
      }

      static if (HAS_RAND_ATTRIB) {
	static private void setLen(A, N...)(ref A arr, size_t v, N idx)
	  if(isArray!A) {
	    static if(N.length == 0) {
	      static if(isDynamicArray!A) {
		arr.length = v;
		// import std.stdio;
		// writeln(arr, " idx: ", N.length);
	      }
	      else {
		assert(false, "Can not set length of a fixed length array");
	      }
	    }
	    else {
	      // import std.stdio;
	      // writeln(arr, " idx: ", N.length);
	      setLen(arr[idx[0]], v, idx[1..$]);
	    }
	  }

	void setLen(N...)(size_t v, N idx) {
	  _parent.setLen(v, _index, idx);
	}
      }

      size_t getLen(N...)(N idx) {
	return _parent.getLen(_index, idx);
      }

      static private auto getRef(A, N...)(ref A arr, N idx)
	if(isArray!A && N.length > 0 && isIntegral!(N[0])) {
	  static if(N.length == 1) return &(arr[idx[0]]);
	  else {
	    return getRef(arr[idx[0]], idx[1..$]);
	  }
	}

      auto getRef(N...)(N idx) if(isIntegral!(N[0])) {
	if(_indexExpr) {
	  assert(_indexExpr.isConst());
	  return _parent.getRef(cast(size_t) _indexExpr.evaluate(), idx);
	}
	else {
	  return _parent.getRef(this._index, idx);
	}
      }

      bool isUnrollable() {
	foreach (var; _relatedIdxs) {
	  if (! var.solved()) return false;
	}
	return true;
      }

      CstDomain[] _relatedIdxs;
      void addRelatedIdx(CstDomain domain) {
	foreach (var; _relatedIdxs) {
	  if (domain is var) {
	    return;
	  }
	}
	_relatedIdxs ~= domain;
      }
      
      EV opIndex(CstVarExpr idx) {
	foreach (domain; idx.getRndDomains(resolved)) {
	  addRelatedIdx(domain);
	}
	if(idx.isConst()) {
	  assert(_elems.length > 0, "_elems for expr " ~ this.name() ~
		 " have not been built");
	  // if(idx.evaluate() >= _elems.length || idx.evaluate() < 0 || _elems is null) {
	  // 	import std.stdio;
	  // 	writeln(this.name(), ":", idx.evaluate());
	  // }
	  // import std.stdio;
	  // writeln(idx.evaluate());
	  return _elems[cast(size_t) idx.evaluate()];
	}
	else {
	  // static if(isStaticArray!E) {
	  //   // static array
	  return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	  // }
	  // else static if(isDynamicArray!E) {
	  //   // dynamic array
	  //   return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	  // }
	  // else {
	  //   return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	  // }
	}
      }

      EV opIndex(size_t idx) {
	build();
	assert(_elems[idx]._indexExpr is null);
	return _elems[idx];
      }

      void _esdl__doRandomize(_esdl__RandGen randGen) {
	static if (HAS_RAND_ATTRIB) {
	  if(_elems.length == 0) this.build();
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
	  if (_elems.length == 0) this.build();
	  assert (stage is s);
	  assert (arrLen !is null);
	  for(size_t i=0; i != arrLen.evaluate(); ++i) {
	    this[i]._esdl__doRandomize(randGen);
	  }
	}
	else {
	  assert(false);
	}
      }

      auto elements() {
	this.build();
	auto idx = arrLen.makeItrVar();
	return this[idx];
      }

      bool built() {
	return (_elems.length == maxArrLen() &&
		_elems[0] !is null);
      }
    
      void build() {
	if(built()) return;
	_elems.length = maxArrLen();
	// static if(isIntegral!E || isBitVector!E) {
	// if(! built()) {
	for (uint i=0; i!=maxArrLen; ++i) {
	  if(_elems[i] is null) {
	    import std.conv: to;
	    _elems[i] = new EV(_name ~ "[" ~ i.to!string() ~ "]", this, i);
	    if(_elems[i].isVarArr()) {
	      _elems[i].build();
	    }
	  }
	}
      }
	
      auto iterator() {
	this.build();
	auto itr = arrLen.makeItrVar();
	return itr;
      }

      CstVarLen!RJ length() {
	return _arrLen;
      }

      CstVarLen!RJ arrLen() {
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

    }
