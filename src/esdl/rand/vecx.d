module esdl.rand.vecx;

import esdl.data.bvec;
import esdl.data.bstr;
import std.traits: isIntegral, isBoolean, isArray, isStaticArray, isDynamicArray;

import esdl.rand.obdd;
import esdl.rand.base;
import esdl.rand.expr: CstVarPrim, CstVarExpr, CstVarIterBase,
  CstStage, EnumConstraints, CstValAllocator, CstVarLen;

// Consolidated Proxy Class
// template CstVarBase(T, int I, int N=0) {
//   alias CstVarBase = CstVarBase!(typeof(T.tupleof[I]),
// 				     getRandAttr!(T, I), N);
// }

abstract class CstVarBase(V, alias R, int N)
  if(_esdl__ArrOrder!(V, N) == 0): CstVarExpr, CstVarPrim
{
  enum HAS_RAND_ATTRIB = (! __traits(isSame, R, _esdl__norand));

  alias E = ElementTypeN!(V, N);
  alias RV = typeof(this);

  string _name;
  BddVec _valvec;
  

  static if (HAS_RAND_ATTRIB) {
    mixin EnumConstraints!E;

    CstVarPrim[] _preReqs;
    BddVec       _domvec;

    uint         _domIndex = uint.max;
    CstStage     _stage = null;
    uint         _unwindLap = 0;
  }

  ~this() {
    resetPrimeBdd();
    static if (HAS_RAND_ATTRIB) {
      _domvec.reset();
    }
    _valvec.reset();
  }

  override string name() {
    return _name;
  }

  void _esdl__reset() {
    static if (HAS_RAND_ATTRIB) {
      _stage = null;
      _unwindLap = 0;
    }
  }

  bool isVarArr() {
    return false;
  }

  BDD getPrimBdd(Buddy buddy) {
    return buddy.one();
  }

  void resetPrimeBdd() { }
  
  abstract ulong value();
  
  override long evaluate() {
    static if (HAS_RAND_ATTRIB) {
      if(! this.isRand || stage().solved()) {
	return value();
      }
      else {
	import std.conv;
	assert(false, "Rand variable " ~ _name ~
	       " evaluation in wrong stage: " ~ stage()._id.to!string);
      }
    }
    else {
      return value();
    }
  }

  bool isRand() {
    static if (HAS_RAND_ATTRIB) {
      return true;
    }
    else {
      return false;
    }
  }

  CstStage stage() {
    static if (HAS_RAND_ATTRIB) {
      return _stage;
    }
    else {
      assert(false);
    }
  }

  void stage(CstStage s) {
    static if (HAS_RAND_ATTRIB) {
      _stage = s;
    }
    else {
      assert(false);
    }
  }

  override uint unwindLap() {
    static if (HAS_RAND_ATTRIB) {
      if (_stage !is null && _stage.solved()) {
	return 0;
      }
      else {
	static if (HAS_RAND_ATTRIB) {
	  return _unwindLap;
	}
	else {
	  return 0;
	}
      }
    }
    else return 0;
  }

  override void unwindLap(uint lap) {
    static if (HAS_RAND_ATTRIB) {
      if (_stage !is null && _stage.solved()) {
	_unwindLap = 0;
      }
      else {
	static if (HAS_RAND_ATTRIB) {
	  _unwindLap = lap;
	}
      }
    }
  }

  uint domIndex() {
    static if (HAS_RAND_ATTRIB) {
      return _domIndex;
    }
    else {
      assert(false);
    }
  }

  void domIndex(uint s) {
    static if (HAS_RAND_ATTRIB) {
      _domIndex = s;
    }
    else {
      assert(false);
    }
  }

  ref BddVec bddvec() {
    static if (HAS_RAND_ATTRIB) {
      return _domvec;
    }
    else {
      return _valvec;
    }
  }

  void bddvec(BddVec b) {
    static if (HAS_RAND_ATTRIB) {
      _domvec = b;
    }
    else {
      assert(false);
    }
  }

  uint bitcount() {
    static if(isIntegral!E)        return E.sizeof * 8;
    else static if(isBitVector!E)  return E.SIZE;
    else static assert(false, "bitcount can not operate on: " ~ E.stringof);
  }

  bool signed() {
    static if(isVarSigned!E) {
      return true;
    }
    else  {
      return false;
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

  void solveBefore(CstVarPrim other) {
    static if (HAS_RAND_ATTRIB) {
      other.addPreRequisite(this);
    }
    else {
      assert(false);
    }
  }

  void addPreRequisite(CstVarPrim prim) {
    static if (HAS_RAND_ATTRIB) {
      _preReqs ~= prim;
    }
    else {
      assert(false);
    }
  }

  void build() {}

  abstract E* getRef();
  
  private bool refreshVal(Buddy buddy) {
    auto val = *(getRef());
    if (! _valvec.isNull) {
      return false;
    }
    else {
      _valvec.buildVec(buddy, val);
      return true;
    }
  }
  
  override bool refresh(CstStage s, Buddy buddy) {
    static if (HAS_RAND_ATTRIB) {
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
    else {
      return refreshVal(buddy);
    }
  }
  
  override BddVec getBDD(CstStage s, Buddy buddy) {
    static if (HAS_RAND_ATTRIB) {
      assert(stage(), "Stage not set for " ~ this.name());
      if(this.isRand && s is stage()) {
	return _domvec;
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
}

// T represents the type of the declared array/non-array member
// N represents the level of the array-elements we have to traverse
// for the elements this CstVar represents
template CstVar(T, int I, int N=0)
  if(_esdl__ArrOrder!(T, I, N) == 0) {
  alias CstVar = CstVar!(typeof(T.tupleof[I]),
			 getRandAttr!(T, I), N);
}

class CstVar(V, alias R, int N) if(N == 0 && _esdl__ArrOrder!(V, N) == 0):
  CstVarBase!(V, R, N)
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

      void _esdl__setValRef(ref V var) {
	_var = &var;
      }
      
      override CstVarPrim[] preReqs() {
	static if (HAS_RAND_ATTRIB) {
	  return _preReqs;
	}
	else {
	  return [];
	}
      }

      override CstVarIterBase[] itrVars() {
	return [];
      }

      override bool hasUnresolvedIdx() {
	return false;
      }
      
      override CstVarPrim[] getRndPrims() {
	static if (HAS_RAND_ATTRIB) {
	  if(isRand) return [this];
	  else return [];
	}
	else {
	  return [];
	}
      }

      CstVarPrim[] getPrimLens() {
	assert(false);
      }

      override RV unwind(CstVarIterBase itr, uint n) {
	// itrVars is always empty
	return this;
      }

      void doRandomize(_esdl__RandGen randGen) {
	static if (HAS_RAND_ATTRIB) {
	  if(stage is null) {
	    randGen.gen(*_var);
	  }
	}
	else {
	  assert(false);
	}
      }

      override E* getRef() {
	return _var;
      }

      override ulong value() {
	return cast(long) (*_var);
      }

      void collate(ulong v, int word = 0) {
	static if (HAS_RAND_ATTRIB) {
	  static if(isIntegral!V) {
	    if(word == 0) {
	      *_var = cast(V) v; // = cast(V) toBitVec(v      }
	    }
	    else {
	      static if(size_t.sizeof == 4 && (is(V == long) || is(V == ulong))) {
		assert(word == 1);	// 32 bit machine with long integral
		V val = v;
		val = val << (8 * size_t.sizeof);
		*_var += val;
	      }
	      else {
		assert(false, "word has to be 0 for integrals");
	      }
	    }
	  }
	  else {
	    _var._setNthWord(v, word); // = cast(V) toBitVec(v);
	  }
	}
	else {
	  assert(false);
	}
      }
    }

class CstVar(V, alias R, int N=0) if(N != 0 && _esdl__ArrOrder!(V, N) == 0):
  CstVarBase!(V, R, N)
    {
      import std.traits;
      import std.range;
      import esdl.data.bvec;

      alias P = CstVarArr!(V, R, N-1);
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

      override CstVarPrim[] preReqs() {
	if(_indexExpr) {
	  static if (HAS_RAND_ATTRIB) {
	    return _preReqs ~ _parent.arrLen() ~
	      _parent.preReqs() ~ _indexExpr.preReqs();
	  }
	  else {
	    return _parent.preReqs() ~ _indexExpr.preReqs();
	  }
	}
	else {
	  static if (HAS_RAND_ATTRIB) {
	    return _preReqs ~ _parent.arrLen() ~ _parent.preReqs();
	  }
	  else {
	    return _parent.preReqs();
	  }
	}
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
      
      CstVarPrim[] getPrimLens() {
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
      override CstVarPrim[] getRndPrims() {
	CstVarPrim[] prims;
	static if (HAS_RAND_ATTRIB) {
	  if(_indexExpr) {
	    // FIXME -- if the expression has been solved
	    // return _parent.getRndPrims(_indexExpr.evaluate()) ;
	    prims = _indexExpr.getRndPrims();
	    foreach(pp; _parent.getRndPrims(-1)) {
	      prims ~= pp;
	    }
	  }
	  else {
	    foreach(pp; _parent.getRndPrims(_index)) {
	      prims ~= pp;
	    }
	  }
	  return prims;
	}
	else {
	  if(_indexExpr) {
	    // FIXME -- if the expression has been solved
	    // return _parent.getRndPrims(_indexExpr.evaluate()) ;
	    prims = _indexExpr.getRndPrims() ~ _parent.getRndPrims();
	  }
	  else {
	    // foreach(pp; _parent.getRndPrims(_index)) {
	    //   prims ~= pp;
	    // }
	    prims = _parent.getRndPrims();
	  }
	  return prims;
	}
      }

      override RV unwind(CstVarIterBase itr, uint n) {
	bool found = false;
	foreach(var; itrVars()) {
	  if(itr is var) {
	    found = true;
	    break;
	  }
	}
	if(! found) return this;
	if(_indexExpr) {
	  return _parent.unwind(itr,n)[_indexExpr.unwind(itr,n)];
	}
	return _parent.unwind(itr,n)[_index];
      }

      void doRandomize(_esdl__RandGen randGen) {
	static if (HAS_RAND_ATTRIB) {
	  if(stage is null) {
	    E val;
	    randGen.gen(val);
	    collate(val);
	  }
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

      override ulong value() {
	if(_indexExpr) {
	  return *(_parent.getRef(_indexExpr.evaluate()));
	}
	else {
	  return *(_parent.getRef(this._index));
	}
      }

      void collate(ulong v, int word = 0) {
	static if (HAS_RAND_ATTRIB) {
	  E* var = getRef();
	  static if(isIntegral!E) {
	    if(word == 0) {
	      *var = cast(E) v;
	    }
	    else {
	      assert(false, "word has to be 0 for integrals");
	    }
	  }
	  else {
	    (*var)._setNthWord(v, word);
	  }
	}
	else {
	  assert(false);
	}
      }
    }

// Arrays (Multidimensional arrays as well)
// template CstVarArrBase(T, int I, int N=0)
//   if(_esdl__ArrOrder!(T, I, N) != 0) {
//   alias CstVarArrBase = CstVarArrBase!(typeof(T.tupleof[I]),
// 					   getRandAttr!(T, I), N);
// }

abstract class CstVarArrBase(V, alias R, int N=0)
  if(_esdl__ArrOrder!(V, N) != 0): CstVarPrim
{
  enum HAS_RAND_ATTRIB = (! __traits(isSame, R, _esdl__norand));

  alias L = ElementTypeN!(V, N);
  alias E = ElementTypeN!(V, N+1);

  static if (_esdl__ArrOrder!(V, N+1) == 0) {
    alias EV = CstVar!(V, R, N+1);
  }
  else {
    alias EV = CstVarArr!(V, R, N+1);
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

  static if (HAS_RAND_ATTRIB) {
    CstVarPrim[] _preReqs;

    void opIndexAssign(EV c, size_t idx) {
      _elems[idx] = c;
    }
  }

  bool isVarArr() {
    return true;
  }

  BDD getPrimBdd(Buddy buddy) {
    return buddy.one();
  }

  void resetPrimeBdd() { }

  // override CstVec2VecExpr opIndex(CstVarExpr idx) {
  //   return new CstVec2VecExpr(this, idx, CstBinVecOp.IDXINDEX);
  // }

  bool isRand() {
    assert(false, "isRand not implemented for CstVarArrBase");
  }

  ulong value() {
    assert(false, "value not implemented for CstVarArrBase");
  }

  void collate(ulong v, int word = 0) {
    assert(false, "value not implemented for CstVarArrBase");
  }

  void stage(CstStage s) {
    assert(false, "stage not implemented for CstVarArrBase");
  }

  uint domIndex() {
    assert(false, "domIndex not implemented for CstVarArrBase");
  }

  void domIndex(uint s) {
    assert(false, "domIndex not implemented for CstVarArrBase");
  }

  uint bitcount() {
    assert(false, "bitcount not implemented for CstVarArrBase");
  }

  bool signed() {
    assert(false, "signed not implemented for CstVarArrBase");
  }

  ref BddVec bddvec() {
    assert(false, "bddvec not implemented for CstVarArrBase");
  }

  void bddvec(BddVec b) {
    assert(false, "bddvec not implemented for CstVarArrBase");
  }

  void solveBefore(CstVarPrim other) {
    static if (HAS_RAND_ATTRIB) {
      other.addPreRequisite(this);
    }
    else {
      assert(false);
    }
  }

  void addPreRequisite(CstVarPrim prim) {
    static if (HAS_RAND_ATTRIB) {
      _preReqs ~= prim;
    }
    else {
      assert(false);
    }
  }

}

template CstVarArr(T, int I, int N=0)
  if(_esdl__ArrOrder!(T, I, N) != 0) {
  alias CstVarArr = CstVarArr!(typeof(T.tupleof[I]),
			       getRandAttr!(T, I), N);
}

// Arrays (Multidimensional arrays as well)
class CstVarArr(V, alias R, int N=0)
  if(N == 0 && _esdl__ArrOrder!(V, N) != 0):
    CstVarArrBase!(V, R, N)
      {
	alias RV = typeof(this);
	CstVarLen!RV _arrLen;

	alias RAND=R;

	V* _var;

	void _esdl__setValRef(ref V var) {
	  _var = &var;
	}
      
	static if (HAS_RAND_ATTRIB) {
	  static if(isStaticArray!V) {
	    static assert(__traits(isSame, R, rand));
	    enum int maxLen = V.length;
	    this(string name, ref V var) {
	      _name = name;
	      _var = &var;
	      _arrLen = new CstVarLen!RV(name ~ ".len", this);
	      _relatedIdxs ~= _arrLen;
	    }
	  }

	  static if(isDynamicArray!V) {
	    enum int maxLen = getRandAttrN!(R, N);
	    this(string name, ref V var) {
	      _name = name;
	      _var = &var;
	      _arrLen = new CstVarLen!RV(name ~ ".len", this);
	      _relatedIdxs ~= _arrLen;
	    }
	  }
	}
	else {
	  this(string name, ref V var) {
	    _name = name;
	    _var = &var;
	    _arrLen = new CstVarLen!RV(name ~ ".len", this);
	    _relatedIdxs ~= _arrLen;
	  }
	}

	CstVarPrim[] preReqs() {
	  static if (HAS_RAND_ATTRIB) {
	    return _preReqs;		// N = 0 -- no _parent
	  }
	  else {
	    return [];
	  }
	}

	CstVarIterBase[] itrVars() {
	  return [];		// N = 0 -- no _parent
	}

	static if (HAS_RAND_ATTRIB) {
	  EV[] getRndPrims(int idx) {
	    if(idx < 0) return _elems;
	    else return [_elems[idx]];
	  }
	}

	CstVarPrim[] getRndPrims() {
	  static if (HAS_RAND_ATTRIB) {
	    CstVarPrim[] prims;
	    foreach(elem; _elems) {
	      prims ~= elem;
	    }
	    return prims;
	  }
	  else {
	    return [];
	  }
	}

	CstVarPrim[] getPrimLens() {
	  static if (HAS_RAND_ATTRIB) {
	    CstVarPrim[] prims;
	    if(_arrLen.isRand) prims ~= _arrLen;
	    return prims;
	  }
	  else {
	    return [];
	  }
	}
    
	RV unwind(CstVarIterBase itr, uint n) {
	  return this;
	}

	bool parentLenIsUnresolved() {
	  return false;		// no parent
	}

	bool hasUnresolvedIdx() {
	  foreach (prim; _relatedIdxs) {
	    if (! prim.solved()) {
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

	bool isUnwindable() {
	  foreach (var; _relatedIdxs) {
	    if (! var.solved()) return false;
	  }
	  return true;
	}

	CstVarPrim[] _relatedIdxs;
	void addRelatedIdx(CstVarPrim prim) {
	  foreach (var; _relatedIdxs) {
	    if (prim is var) {
	      return;
	    }
	  }
	  _relatedIdxs ~= prim;
	}
      
	EV opIndex(CstVarExpr idx) {
	  foreach (prim; idx.getRndPrims()) {
	    addRelatedIdx(prim);
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

	void doRandomize(_esdl__RandGen randGen) {
	  static if (HAS_RAND_ATTRIB) {
	    if(_elems.length == 0) this.build();
	    assert(arrLen !is null);
	    for(size_t i=0; i != arrLen.evaluate(); ++i) {
	      this[i].doRandomize(randGen);
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

	CstVarLen!RV length() {
	  return _arrLen;
	}

	CstVarLen!RV arrLen() {
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

class CstVarArr(V, alias R, int N=0) if(N != 0 && _esdl__ArrOrder!(V, N) != 0):
  CstVarArrBase!(V, R, N)
    {
      import std.traits;
      import std.range;
      import esdl.data.bvec;
      alias P = CstVarArr!(V, R, N-1);
      P _parent;
      CstVarExpr _indexExpr = null;
      int _index = 0;

      alias RV = typeof(this);
      CstVarLen!RV _arrLen;

      alias RAND=R;
      
      this(string name, P parent, CstVarExpr indexExpr) {
	assert(parent !is null);
	_name = name;
	_parent = parent;
	_indexExpr = indexExpr;
	_arrLen = new CstVarLen!RV(name ~ ".len", this);
	_relatedIdxs ~= _arrLen;
      }

      this(string name, P parent, uint index) {
	assert(parent !is null);
	_name = name;
	_parent = parent;
	_index = index;
	_arrLen = new CstVarLen!RV(name ~ ".len", this);
	_relatedIdxs ~= _arrLen;
      }

      CstVarPrim[] preReqs() {
	CstVarPrim[] req;
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
	foreach (prim; _relatedIdxs) {
	  if (! prim.solved()) {
	    return true;
	  }
	}
	return _parent.hasUnresolvedIdx();
      }

      EV[] getRndPrims(int idx) {
	EV[] prims;
	static if (HAS_RAND_ATTRIB) {
	  if(_indexExpr) {
	    foreach(pp; _parent.getRndPrims(-1)) {
	      if(idx < 0) prims ~= pp._elems;
	      else prims ~= pp._elems[idx];
	    }
	  }
	  else {
	    foreach(pp; _parent.getRndPrims(_index)) {
	      if(idx < 0) prims ~= pp._elems;
	      else prims ~= pp._elems[idx];
	    }
	  }
	}
	return prims;
      }
    
      // This is slightly tricky in case we are pointing directly to
      // just one element of the array, this function should return just
      // that element. But it could be that the index or an upper
      // hierarchy index is not a constant, but an iterator or a
      // randomized epression. In that case, we shall have to return
      // more primary elements
      CstVarPrim[] getRndPrims() {
	CstVarPrim[] prims;
	static if (HAS_RAND_ATTRIB) {
	  if(_indexExpr) {
	    prims = _indexExpr.getRndPrims();
	    foreach(pp; _parent.getRndPrims(-1)) {
	      prims ~= pp;
	    }
	  }
	  else {
	    foreach(pp; _parent.getRndPrims(_index)) {
	      prims ~= pp;
	    }
	  }
	}
	else {
	  prims ~= _parent.getPrimLens();
	  if(_indexExpr) {
	    prims ~= _indexExpr.getRndPrims() ~ _parent.getPrimLens();
	  }
	}
	return prims;
      }

      CstVarPrim[] getPrimLens() {
	// if(_index.itrVars.length is 0)
	if(_indexExpr is null) {
	  return [_parent[_index].arrLen()];
	  // return prims ~ _parent[_index].getPrimLens();
	}
	if(_indexExpr.isConst()) {
	  CstVarPrim[] prims;
	  prims ~= _parent[cast(size_t) _indexExpr.evaluate()].getPrimLens();
	  return prims;
	}
	else {
	  CstVarPrim[] prims;
	  foreach(p; getRndPrims()) {
	    // import std.stdio;
	    // writeln(_parent.name(), " ", p.name());
	    prims ~= p.getPrimLens();
	  }
	  return prims;
	}
      }

      RV unwind(CstVarIterBase itr, uint n) {
	bool found = false;
	foreach(var; itrVars()) {
	  if(itr is var) {
	    found = true;
	    break;
	  }
	}
	if(! found) return this;
	if(_indexExpr) {
	  return _parent.unwind(itr,n)[_indexExpr.unwind(itr,n)];
	}
	return _parent.unwind(itr,n)[_index];
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

      bool isUnwindable() {
	foreach (var; _relatedIdxs) {
	  if (! var.solved()) return false;
	}
	return true;
      }

      CstVarPrim[] _relatedIdxs;
      void addRelatedIdx(CstVarPrim prim) {
	foreach (var; _relatedIdxs) {
	  if (prim is var) {
	    return;
	  }
	}
	_relatedIdxs ~= prim;
      }
      
      EV opIndex(CstVarExpr idx) {
	foreach (prim; idx.getRndPrims()) {
	  addRelatedIdx(prim);
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

      void doRandomize(_esdl__RandGen randGen) {
	static if (HAS_RAND_ATTRIB) {
	  if(_elems.length == 0) this.build();
	  assert(arrLen !is null);
	  for(size_t i=0; i != arrLen.evaluate(); ++i) {
	    this[i].doRandomize(randGen);
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

      CstVarLen!RV length() {
	return _arrLen;
      }

      CstVarLen!RV arrLen() {
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
