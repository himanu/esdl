module esdl.rand.objx;

import esdl.data.bvec;
import esdl.data.bstr;
import std.traits: isIntegral, isBoolean, isArray, isStaticArray, isDynamicArray;

import esdl.rand.misc;
import esdl.rand.intr;
import esdl.rand.base: CstVecPrim, CstVecExpr,
  CstIterator, DomType, CstDomain,
  CstLogicExpr, CstPredicate, _esdl__Proxy, CstObjIntf, CstObjArrIntf;
import esdl.rand.proxy: _esdl__ProxyRoot;
import esdl.rand.expr: CstVecLen, CstVecDomain, _esdl__cstVal,
  CstVecTerm, CstVecIterator;

import esdl.rand.intr: IntRangeSet;
import esdl.rand.meta: _esdl__ProxyResolve;

import std.algorithm.searching: canFind;

mixin template CstObjMixin() {
  enum HAS_RAND_ATTRIB = R.isRand();
  alias LEAF = LeafElementType!V;

  string _name;

  string name() {
    return _name;
  }

  S to(S)()
    if (is (S == string)) {
      import std.conv;
      return "";
      // static if (HAS_RAND_ATTRIB) {
      // 	if (isRand) {
      // 	  return "RAND#" ~ _name ~ ":" ~ value().to!string();
      // 	}
      // 	else {
      // 	  return "VAL#" ~ _name ~ ":" ~ value().to!string();
      // 	}
      // }
      // else {
      // 	return "VAR#" ~ _name ~ ":" ~ value().to!string();
      // }
    }

  override string toString() {
    return this.to!string();
  }

  bool isRand() {
    static if (HAS_RAND_ATTRIB) {
      return true;
    }
    else {
      return false;
    }
  }
}

// T represents the type of the declared array/non-array member
// N represents the level of the array-elements we have to traverse
// for the elements this CstObj represents

class CstObjIdx(V, rand R, int N, P, int IDX): CstObj!(V, R, N)
{
  enum _esdl__ISRAND = R.isRand();
  enum _esdl__HASPROXY = R.hasProxy();
  alias _esdl__PROXYT = P;
  enum int _esdl__INDEX = IDX;
  this(string name, ref V var, _esdl__ProxyRoot parent) {
    super(name, var, parent);
  }
}

class CstObj(V, rand R, int N) if (N == 0 && _esdl__ArrOrder!(V, N) == 0):
  _esdl__ProxyResolve!V, CstObjIntf
    {
      import std.traits;
      import std.range;
      import esdl.data.bvec;
      
      mixin CstObjMixin;

      alias RV = typeof(this);

      _esdl__Proxy _root;
      
      static if (is (V == struct)) {
	this(string name, ref V var, _esdl__ProxyRoot parent) {
	  // import std.stdio;
	  // writeln("New obj ", name);
	  super(name, var, parent);
	  // _var = &var;
	  // _parent = parent;
	}
      }
      else {
	this(string name, V var, _esdl__ProxyRoot parent) {
	  // import std.stdio;
	  // writeln("New obj ", name);
	  super(name, var, parent);
	  // _var = &var;
	  // _parent = parent;
	}
      }


      auto _esdl__rand_term_chain(S ...)(CstVecTerm[] indx ...)
      {
	static if (S.length == 0) return this;
	else static if (S[0] == "") {
	  return this.opIndex(indx[0])._esdl__rand_term_chain!(S[1..$])(indx[1..$]);
	}
	else {
	  static assert (S.length == 1);
	  return __traits(getMember, this, S[0]);
	}
      }

      // void _esdl__setValRef(ref V var) {
      // 	_var = &var;
      // }
      
      _esdl__Proxy getProxyRoot()() {
	assert (_root !is null);
	return _root;
      }

      // override RV getResolved() {
      // 	return this;
      // }

      // // RV
      // CstVecExpr unroll(CstIterator iter, uint n) {
      // 	return this;
      // }

      // override LEAF* getRef() {
      // 	return _var;
      // }

      // void setDomainContext(CstPredicate pred,
      // 			 ref CstDomain[] rnds,
      // 			 ref CstDomain[] vars,
      // 			 ref CstValue[] vals,
      // 			 ref CstIterator[] iters,
      // 			 ref CstDomain[] idxs,
      // 			 ref CstDomain[] deps) {
      // 	static if (is (R: _esdl__norand)) {
      // 	  if (! canFind(vars, this)) vars ~= this;
      // 	}
      // 	else {
      // 	  if (! canFind(rnds, this)) rnds ~= this;
      // 	}
      // }

      // final override void markAsUnresolved(uint lap) {
      // 	if (_unresolveLap != lap) {
      // 	  _unresolveLap = lap;
      // 	  foreach (pred; _rndPreds) {
      // 	    pred.markAsUnresolved(lap);
      // 	  }
      // 	}
      // }
      
    }

class CstObj(V, rand R, int N) if (N != 0 && _esdl__ArrOrder!(V, N) == 0): CstObjIntf
  {
    import std.traits;
    import std.range;
    import esdl.data.bvec;

    mixin CstObjMixin;
      
    alias RV = typeof(this);
    alias P = CstObjArr!(V, R, N-1);
    P _parent;

    _esdl__Proxy _root;

    CstVecExpr _indexExpr = null;
    int _pindex = 0;

    uint _resolvedCycle;	// cycle for which indexExpr has been resolved
    RV _resolvedObj;

    this(string name, P parent, CstVecExpr indexExpr) {
      // import std.stdio;
      // writeln("New ", name);
      assert (parent !is null);
      // super(parent.getProxyRoot());
      _parent = parent;
      _indexExpr = indexExpr;
      // _root = _parent.getProxyRoot();
      // only concrete elements need be added
      // getProxyRoot().addRndDomain(this);
    }

    this(string name, P parent, uint index) {
      // import std.stdio;
      // writeln("New ", name);
      assert (parent !is null);
      // super(parent.getProxyRoot());
      _parent = parent;
      // _indexExpr = _esdl__cstVal(index);
      _pindex = index;
    }

    override bool opEquals(Object other) {
      auto rhs = cast (RV) other;
      if (rhs is null) return false;
      else return (_parent == rhs._parent && _indexExpr == _indexExpr);
    }
      
    final bool isStatic() {
      return ((_indexExpr is null ||
	       _indexExpr.isIterator ||
	       _indexExpr.isConst) &&
	      _parent.isStatic());
    }

    _esdl__Proxy getProxyRoot()() {
      assert (_root !is null);
      return _root;
    }

    RV getResolved() {
      // if (_resolvedCycle != getProxyRoot()._cycle) {
      // 	auto parent = _parent.getResolved();
      // 	if (_indexExpr) {
      // 	  _resolvedObj = parent[cast(size_t) _indexExpr.evaluate()];
      // 	}
      // 	else {
      // 	  _resolvedObj = parent[_pindex];
      // 	}
      // 	_resolvedCycle = getProxyRoot()._cycle;
      // }
      return _resolvedObj;
    }

    // RV
    auto unroll(CstIterator iter, uint n) {
      if (_indexExpr) {
	return _parent.unroll(iter,n)[_indexExpr.unroll(iter,n)];
      }
      else {
	return _parent.unroll(iter,n)[_pindex];
      }
    }
      
    LEAF* getRef() {
      if (_indexExpr) {
	return _parent.getRef(cast(size_t) _indexExpr.evaluate());
      }
      else {
	return _parent.getRef(this._pindex);
      }
    }

    void setDomainContext(CstPredicate pred,
		       ref CstDomain[] rnds,
		       ref CstDomain[] vars,
		       ref CstValue[] vals,
		       ref CstIterator[] iters,
		       ref CstDomain[] idxs,
		       ref CstDomain[] bitIdxs,
		       ref CstDomain[] deps) {
      static if (R.isRand()) {
      // 	if (! canFind(rnds, this)) rnds ~= this;
      // }
      // else {
      // 	if (! canFind(vars, this)) vars ~= this;
      }
      if (_parent.isStatic()) {
	deps ~= _parent._arrLen;
      }
      _parent.setDomainContext(pred, rnds, vars, vals, iters, idxs, bitIdxs, deps);

      if (_indexExpr !is null) {
	// Here we need to put the parent as a dep for the pred
	// and since this prim needs resolution, the constituents of
	// the indexExpr need to trigger a function that finds out
	// whether the _indexExpr has been fully resolved or
	// not. When the indexExpr gets resolved, it should inform
	// the parent about resolution which in turn should inform
	// the pred that it can go ahead
	_indexExpr.setDomainContext(pred, idxs, idxs, vals, iters, idxs, bitIdxs, deps);
      }
    }

    auto _esdl__rand_term_chain(S ...)(CstVecTerm[] indx ...) {
      return new CstObjTerm!(RV, S)(this, indx);
    }
}

// Arrays (Multidimensional arrays as well)
// template CstVecArrMixin(T, int I, int N=0)
//   if(_esdl__ArrOrder!(T, I, N) != 0) {
//   alias CstVecArrMixin = CstVecArrMixin!(typeof(T.tupleof[I]),
// 					   getRandAttr!(T, I), N);
// }

mixin template CstObjArrMixin()
{
  alias RV = typeof(this);
  enum HAS_RAND_ATTRIB = RAND.isRand();
  alias LEAF = LeafElementType!V;

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

  string name() {
    return _name;
  }

  bool isRand() {
    assert(false, "isRand not implemented for CstObjArrBase");
  }

  _esdl__Proxy getProxyRoot()() {
    assert (_root !is null);
    return _root;
  }
  
  size_t _forcedLength;
  void buildElements(size_t v) {
    if (! _arrLen.isSolved()) {
      if (v > _forcedLength) {
	_forcedLength = v;
      }
    }
    else if (v < _forcedLength) {
      import std.string: format;
      assert(false,
	     format("Trying to set length %d, while it should be a minimum %d",
		    v, _forcedLength));
    }
    size_t currLen = _elems.length;
    if (currLen < v) {
      _elems.length = v;
      for (size_t i=currLen; i!=v; ++i) {
	import std.conv: to;
	_elems[i] = new EV(_name ~ "[#" ~ i.to!string() ~ "]",
			   this, cast(uint) i);
      }
    }
  }

  EV opIndex(CstVecExpr indexExpr) {
    if (indexExpr.isConst()) {
      size_t index = cast(size_t) indexExpr.evaluate();
      if (_arrLen.isSolved()) {
	if (_arrLen.evaluate() <= index) {
	  assert (false, "Index Out of Range");
	}
      }
      buildElements(index+1);
      return _elems[index];
    }
    else {
      return new EV(name ~ "[" ~ indexExpr.name() ~ "]", this, indexExpr);
    }
  }

  EV opIndex(size_t index) {
    if (_arrLen.isSolved()) {
      auto len = _arrLen.evaluate();
      if (len <= index) {
	assert (false, "Index Out of Range");
      }
      buildElements(len);
    }
    else {
      buildElements(index+1);
    }
    // assert (_elems[index]._indexExpr is null);
    return _elems[index];
  }

  void _esdl__doRandomize(_esdl__RandGen randGen) {
    static if (HAS_RAND_ATTRIB) {
      assert (arrLen !is null);
      buildElements(getLen());
      // for (size_t i=0; i != arrLen.evaluate(); ++i) {
      // 	this[i]._esdl__doRandomize(randGen);
      // }
    }
    else {
      assert (false);
    }
  }

  EV _esdl__elems() {
    return this[_esdl__iter()];
  }

  CstVecIterator!RV _esdl__iter() {
    CstVecIterator!RV iter = arrLen.makeIterVar();
    return iter;
  }

  CstVecLen!RV length() {
    return _arrLen;
  }

  CstVecLen!RV arrLen() {
    return _arrLen;
  }

  uint maxArrLen() {
    static if (HAS_RAND_ATTRIB) {
      static if(isStaticArray!L) {
	return L.length;
      }
      else {
	return R[N];
      }
    }
    else {
      return uint.max;
    }
  }

}

// template CstObjArr(T, int I, int N=0)
//   if(_esdl__ArrOrder!(T, I, N) != 0) {
//   alias CstObjArr = CstObjArr!(typeof(T.tupleof[I]), LeafElementType!(T.tupleof[I]),
// 			       getRandAttr!(T, I), N);
// }

// Arrays (Multidimensional arrays as well)
class CstObjArrIdx(V, rand R, int N, P, int IDX): CstObjArr!(V, R, N)
{
  enum _esdl__ISRAND = R.isRand();
  enum _esdl__HASPROXY = R.hasProxy();
  alias _esdl__PROXYT = P;
  enum int _esdl__INDEX = IDX;
  this(string name, ref V var, _esdl__Proxy parent) {
    super(name, var, parent);
  }
}

class CstObjArr(V, rand R, int N) if (N == 0 && _esdl__ArrOrder!(V, N) != 0): CstObjArrIntf
{
  mixin CstObjArrMixin;
  CstVecLen!RV _arrLen;

  alias RAND=R;

  V* _var;
  _esdl__Proxy _parent;
  _esdl__Proxy _root;
    
  void _esdl__setValRef(ref V var) {
    _var = &var;
  }
      
  this(string name, ref V var, _esdl__Proxy parent) {
    _name = name;
    _var = &var;
    _parent = parent;
    // _root = _parent.getProxyRoot();
    _arrLen = new CstVecLen!RV(name ~ ".len", this);
  }

  _esdl__Proxy getProxyRoot()() {
    assert (_root !is null);
    return _root;
  }

  final bool isStatic() {
    return true; 		// N == 0
  }

  RV getResolved() {
    return this;
  }

  RV unroll(CstIterator iter, uint n) {
    return this;
  }

  static private auto getRef(A, N...)(ref A arr, N indx)
    if (isArray!A && N.length > 0 && isIntegral!(N[0])) {
      static if (N.length == 1) return &(arr[indx[0]]);
      else {
	return getRef(arr[indx[0]], indx[1..$]);
      }
    }

  auto getRef(J...)(J indx) if(isIntegral!(J[0])) {
    return getRef(*_var, cast(size_t) indx);
  }

  void setLen(N...)(size_t v, N indx) {
    static if (HAS_RAND_ATTRIB) {
      setLenTmp(*_var, v, indx);
    }
  }

  static private size_t getLen(A, N...)(ref A arr, N indx) if(isArray!A) {
    static if (N.length == 0) return arr.length;
    else {
      if (arr.length == 0) return 0;
      else return getLen(arr[indx[0]], indx[1..$]);
    }
  }

  size_t getLen(N...)(N indx) {
    return getLen(*_var, indx);
  }

  void setDomainContext(CstPredicate pred,
			ref CstDomain[] rnds,
			ref CstDomain[] vars,
			ref CstValue[] vals,
			ref CstIterator[] iters,
			ref CstDomain[] idxs,
			ref CstDomain[] bitIdxs,
			ref CstDomain[] deps) {
    // arrlen should not be handled here. It is handled as part
    // of the indexExpr in the elements when required (that is
    // when indexExpr is not contant, but an expression)
	  
    // auto iter = arrLen.makeIterVar();
    // iters ~= iter;

    // no parent
  }

  // void markAsUnresolved(uint lap) {
  //   foreach (elem; _elems) {
  //     elem.markAsUnresolved(lap);
  //   }
  // }

}

class CstObjArr(V, rand R, int N) if(N != 0 && _esdl__ArrOrder!(V, N) != 0): CstVecPrim, CstObjArrIntf
{
  mixin CstObjArrMixin;
  import std.traits;
  import std.range;
  import esdl.data.bvec;
  alias P = CstObjArr!(V, R, N-1);
  P _parent;
  _esdl__Proxy _root;
  CstVecExpr _indexExpr = null;
  int _pindex = 0;

  CstVecLen!RV _arrLen;

  alias RAND=R;
      
  uint _resolvedCycle;	// cycle for which indexExpr has been resolved
  RV _resolvedObj;

  this(string name, P parent, CstVecExpr indexExpr) {
    // import std.stdio;
    // writeln("New ", name);
    assert (parent !is null);
    _name = name;
    _parent = parent;
    _indexExpr = indexExpr;
    // _root = _parent.getProxyRoot();
    _arrLen = new CstVecLen!RV(name ~ ".len", this);
  }

  this(string name, P parent, uint index) {
    // import std.stdio;
    // writeln("New ", name);
    assert (parent !is null);
    _name = name;
    _parent = parent;
    // _indexExpr = _esdl__cstVal(index);
    _pindex = index;
    // _root = _parent.getProxyRoot();
    _arrLen = new CstVecLen!RV(name ~ ".len", this);
  }

  _esdl__Proxy getProxyRoot() {
    assert (_root !is null);
    return _root;
  }

  override bool opEquals(Object other) {
    auto rhs = cast (RV) other;
    if (rhs is null) return false;
    else return (_parent == rhs._parent && _indexExpr == _indexExpr);
  }
      
  final bool isStatic() {
    return ((_indexExpr is null  ||
	     _indexExpr.isIterator ||
	     _indexExpr.isConst) &&
	    _parent.isStatic());
  }

  RV getResolved() {
    // if (_resolvedCycle != getProxyRoot()._cycle) {
    //   auto parent = _parent.getResolved();
    //   if (_indexExpr) {
    // 	_resolvedObj = parent[_indexExpr.evaluate()];
    //   }
    //   else {
    // 	_resolvedObj = parent[_pindex];
    //   }
    //   _resolvedCycle = getProxyRoot()._cycle;
    // }
    return _resolvedObj;
  }

  RV unroll(CstIterator iter, uint n) {
    if (_indexExpr) {
      return _parent.unroll(iter,n)[_indexExpr.unroll(iter,n)];
    }
    else {
      return _parent.unroll(iter,n)[_pindex];
    }
  }

  void setLen(N...)(size_t v, N indx) {
    static if (HAS_RAND_ATTRIB) {
      _parent.setLen(v, _pindex, indx);
    }
  }

  static private size_t getLen(A, N...)(ref A arr, N indx) if(isArray!A) {
    static if (N.length == 0) return arr.length;
    else {
      return getLen(arr[indx[0]], indx[1..$]);
    }
  }

  size_t getLen(N...)(N indx) {
    return _parent.getLen(_pindex, indx);
  }

  static private auto getRef(A, N...)(ref A arr, N indx)
    if (isArray!A && N.length > 0 && isIntegral!(N[0])) {
      static if (N.length == 1) return &(arr[indx[0]]);
      else {
	return getRef(arr[indx[0]], indx[1..$]);
      }
    }

  auto getRef(N...)(N indx) if (isIntegral!(N[0])) {
    if (_indexExpr) {
      assert (_indexExpr.isConst());
      return _parent.getRef(cast (size_t) _indexExpr.evaluate(), indx);
    }
    else {
      return _parent.getRef(this._pindex, indx);
    }
  }

  void setDomainContext(CstPredicate pred,
		     ref CstDomain[] rnds,
		     ref CstDomain[] vars,
		     ref CstValue[] vals,
		     ref CstIterator[] iters,
		     ref CstDomain[] idxs,
		     ref CstDomain[] bitIdxs,
		     ref CstDomain[] deps) {
    // arrlen should not be handled here. It is handled as part
    // of the indexExpr in the elements when required (that is
    // when indexExpr is not contant, but an expression)
	  
    // auto iter = arrLen.makeIterVar();
    // iters ~= iter;
    if (_parent.isStatic()) {
      deps ~= _parent._arrLen;
    }
    _parent.setDomainContext(pred, rnds, vars, vals, iters, idxs, bitIdxs, deps);
    if (_indexExpr !is null) {
      _indexExpr.setDomainContext(pred, idxs, idxs, vals, iters, idxs, bitIdxs, deps);
    }
  }

  // void markAsUnresolved(uint lap) {
  //   if (isStatic()) {
  //     foreach (elem; _elems) {
  // 	elem.markAsUnresolved(lap);
  //     }
  //   }
  //   else {
  //     _parent.markAsUnresolved(lap);
  //   }
  // }

}

// T is any of the above defines classes
// S is a series of strings identifying the member object
class CstObjTerm(T, S ...): CstVecTerm
{
  T _objArr;

  this(T, S ...)(T objArr, CstVecTerm[] indx ...) {
  }
}
