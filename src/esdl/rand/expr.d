module esdl.rand.expr;

import esdl.rand.obdd;
import esdl.rand.misc: _esdl__RandGen, _esdl__norand, isVecSigned;
import esdl.rand.base;
import esdl.data.bvec: isBitVector;
import std.traits: isIntegral, isBoolean, isArray, isStaticArray, isDynamicArray;

abstract class CstVarTerm: CstVarExpr
{

  CstBddTerm toBdd() {
    auto zero = new CstVal!int(0); // CstVal!int.allocate(0);
    return new CstVec2BddExpr(this, zero, CstBinBddOp.NEQ);
  }

  override abstract CstVarTerm unroll(CstVarIterBase itr, uint n);

  CstVec2VecExpr opBinary(string op)(CstVarTerm other)
  {
    static if(op == "&") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.AND);
    }
    static if(op == "|") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.OR);
    }
    static if(op == "^") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.XOR);
    }
    static if(op == "+") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.ADD);
    }
    static if(op == "-") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.SUB);
    }
    static if(op == "*") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.MUL);
    }
    static if(op == "/") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.DIV);
    }
    static if(op == "%") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.REM);
    }
    static if(op == "<<") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.LSH);
    }
    static if(op == ">>") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.RSH);
    }
  }

  CstVec2VecExpr opBinary(string op, Q)(Q q)
    if(isBitVector!Q || isIntegral!Q)
      {
  	auto qq = new CstVal!Q(q); // CstVal!Q.allocate(q);
  	static if(op == "&") {
  	  return new CstVec2VecExpr(this, qq, CstBinVecOp.AND);
  	}
  	static if(op == "|") {
  	  return new CstVec2VecExpr(this, qq, CstBinVecOp.OR);
  	}
  	static if(op == "^") {
  	  return new CstVec2VecExpr(this, qq, CstBinVecOp.XOR);
  	}
  	static if(op == "+") {
  	  return new CstVec2VecExpr(this, qq, CstBinVecOp.ADD);
  	}
  	static if(op == "-") {
  	  return new CstVec2VecExpr(this, qq, CstBinVecOp.SUB);
  	}
  	static if(op == "*") {
  	  return new CstVec2VecExpr(this, qq, CstBinVecOp.MUL);
  	}
  	static if(op == "/") {
  	  return new CstVec2VecExpr(this, qq, CstBinVecOp.DIV);
  	}
  	static if(op == "%") {
  	  return new CstVec2VecExpr(this, qq, CstBinVecOp.REM);
  	}
  	static if(op == "<<") {
  	  return new CstVec2VecExpr(this, qq, CstBinVecOp.LSH);
  	}
  	static if(op == ">>") {
  	  return new CstVec2VecExpr(this, qq, CstBinVecOp.RSH);
  	}
      }

  CstVec2VecExpr opBinaryRight(string op, Q)(Q q)
    if(isBitVector!Q || isIntegral!Q)
      {
	auto qq = new CstVal!Q(q); // CstVal!Q.allocate(q);
	static if(op == "&") {
	  return new CstVec2VecExpr(qq, this, CstBinVecOp.AND);
	}
	static if(op == "|") {
	  return new CstVec2VecExpr(qq, this, CstBinVecOp.OR);
	}
	static if(op == "^") {
	  return new CstVec2VecExpr(qq, this, CstBinVecOp.XOR);
	}
	static if(op == "+") {
	  return new CstVec2VecExpr(qq, this, CstBinVecOp.ADD);
	}
	static if(op == "-") {
	  return new CstVec2VecExpr(qq, this, CstBinVecOp.SUB);
	}
	static if(op == "*") {
	  return new CstVec2VecExpr(qq, this, CstBinVecOp.MUL);
	}
	static if(op == "/") {
	  return new CstVec2VecExpr(qq, this, CstBinVecOp.DIV);
	}
	static if(op == "%") {
	  return new CstVec2VecExpr(qq, this, CstBinVecOp.REM);
	}
	static if(op == "<<") {
	  return new CstVec2VecExpr(qq, this, CstBinVecOp.LSH);
	}
	static if(op == ">>") {
	  return new CstVec2VecExpr(qq, this, CstBinVecOp.RSH);
	}
      }

  CstVarTerm opIndex(CstVarExpr index)
  {
    // assert(false, "Index operation defined only for Arrays");
    return new CstVecSliceExpr(this, index);
  }

  CstVarTerm opSlice(P)(P p)
    if(isIntegral!P || isBitVector!P) {
      return new CstVecSliceExpr(this, new CstVal!P(p)); // CstVal!P.allocate(p));
    }

  CstVarTerm opSlice(CstVarExpr lhs, CstVarExpr rhs)
  {
    return new CstVecSliceExpr(this, lhs, rhs);
  }

  CstVarTerm opSlice(P, Q)(P p, Q q)
    if((isIntegral!P || isBitVector!P) && (isIntegral!Q || isBitVector!Q)) {
      return new CstVecSliceExpr(this, new CstVal!P(p), // CstVal!P.allocate(p),
				 new CstVal!Q(q)); // CstVal!Q.allocate(q));
    }
  CstNotBddExpr opUnary(string op)() if(op == "*") {
    // static if(op == "*") {	// "!" in cstx is translated as "*"
    return new CstNotBddExpr(this.toBdd());
    // }
    // else {
    //   static assert(false);
    // }
  }
  CstNotVecExpr opUnary(string op)() if(op == "~") {
    // static if(op == "*") {	// "!" in cstx is translated as "*"
    return new CstNotVecExpr(this);
    // }
    // else {
    //   static assert(false);
    // }
  }
  CstNegVecExpr opUnary(string op)() if(op == "-") {
    // static if(op == "*") {	// "!" in cstx is translated as "*"
    return new CstNegVecExpr(this);
    // }
    // else {
    //   static assert(false);
    // }
  }
}

abstract class CstVecDomain(alias R): CstVarTerm, CstDomain
{
  enum HAS_RAND_ATTRIB = (! __traits(isSame, R, _esdl__norand));

  BddVec _valvec;

  static if (HAS_RAND_ATTRIB) {
    // BddVec       _domvec;
    uint         _domIndex = uint.max;
    CstStage     _stage = null;
    uint         _resolveLap = 0;
  }
  
  ~this() {
    // static if (HAS_RAND_ATTRIB) {
    //   _domvec.reset();
    // }
    _valvec.reset();
  }    

  ref BddVec bddvec(Buddy buddy) {
    static if (HAS_RAND_ATTRIB) {
      return buddy.getVec(_domIndex);
      // return _domvec;
    }
    else {
      return _valvec;
    }
  }

  // void bddvec(BddVec b) {
  //   static if (HAS_RAND_ATTRIB) {
  //     _domvec = b;
  //   }
  //   else {
  //     assert(false);
  //   }
  // }

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

  void reset() {
    static if (HAS_RAND_ATTRIB) {
      _domIndex = uint.max;
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

  override uint resolveLap() {
    static if (HAS_RAND_ATTRIB) {
      if (_stage !is null && _stage.solved()) {
	return 0;
      }
      else {
	static if (HAS_RAND_ATTRIB) {
	  return _resolveLap;
	}
	else {
	  return 0;
	}
      }
    }
    else return 0;
  }

  override void resolveLap(uint lap) {
    static if (HAS_RAND_ATTRIB) {
      if (_stage !is null && _stage.solved()) {
	_resolveLap = 0;
      }
      else {
	static if (HAS_RAND_ATTRIB) {
	  _resolveLap = lap;
	}
      }
    }
  }

  CstBddExpr getNopBddExpr() {
    return new CstNopBddExpr(this);
  }
  
}

abstract class CstBddTerm: CstBddExpr
{
  abstract override CstBddTerm unroll(CstVarIterBase itr, uint n);

  CstBddTerm opBinary(string op)(CstBddTerm other)
  {
    static if(op == "&") {
      if (this.cstExprIsNop()) {
	return other;
      }
      if (other.cstExprIsNop()) {
	return this;
      }
      return new CstBdd2BddExpr(this, other, CstBddOp.LOGICAND);
    }
    static if(op == "|") {
      return new CstBdd2BddExpr(this, other, CstBddOp.LOGICOR);
    }
    static if(op == ">>") {
      return new CstBdd2BddExpr(this, other, CstBddOp.LOGICIMP);
    }
  }

  CstBddTerm opUnary(string op)()
  {
    static if(op == "*") {	// "!" in cstx is translated as "*"
      return new CstNotBddExpr(this);
    }
  }

  CstBddTerm implies(CstBddTerm other)
  {
    return new CstBdd2BddExpr(this, other, CstBddOp.LOGICIMP);
  }

  CstBddTerm implies(CstVarTerm other)
  {
    return new CstBdd2BddExpr(this, other.toBdd(), CstBddOp.LOGICIMP);
  }

  CstBddTerm logicOr(CstBddTerm other)
  {
    return new CstBdd2BddExpr(this, other, CstBddOp.LOGICOR);
  }

  CstBddTerm logicOr(CstVarTerm other)
  {
    return new CstBdd2BddExpr(this, other.toBdd(), CstBddOp.LOGICOR);
  }

  CstBddTerm logicAnd(CstBddTerm other)
  {
    if (this.cstExprIsNop()) {
      return other;
    }
    if (other.cstExprIsNop()) {
      return this;
    }
    return new CstBdd2BddExpr(this, other, CstBddOp.LOGICAND);
  }

  CstBddTerm logicAnd(CstVarTerm other)
  {
    if (this.cstExprIsNop()) {
      return other.toBdd();
    }
    return new CstBdd2BddExpr(this, other.toBdd(), CstBddOp.LOGICAND);
  }

}

class CstVarIter(RV): CstVarTerm, CstVarIterBase
{
  RV _arrVar;

  RV arrVar() {
    return _arrVar;
  }

  string _name;

  this(RV arrVar) {
    _name = "itrVar";
    _arrVar = arrVar;
    _arrVar._arrLen.itrVar(this);
  }

  override CstVarIterBase[] itrVars() {
    return _arrVar.itrVars() ~ this;
  }

  override CstVarIterBase getIterator() {
    CstVarIterBase pitr = _arrVar.getIterator();
    if (pitr !is null) return pitr;
    else return this;
  }

  override bool hasUnresolvedIdx() {
    return true;
  }
      
  override CstDomain[] unresolvedIdxs() {
    return [_arrVar.arrLen()];
  }
  
  override uint maxVal() {
    if(! this.isUnrollable()) {
      assert(false, "Can not find maxVal since the " ~
	     "Itr Variable is unrollable");
    }
    // import std.stdio;
    // writeln("maxVal for arrVar: ", _arrVar.name(), " is ",
    // 	    _arrVar.arrLen.value);
    return cast(uint) _arrVar.arrLen.value;
  }

  override bool isUnrollable() {
    return _arrVar.isUnrollable();
  }

  // get all the primary bdd vectors that constitute a given bdd expression
  override CstDomain[] getRndDomains(bool resolved) {
    return [_arrVar._arrLen]; // _arrVar.arrLen.getRndDomains(resolved);
  }

  override string name() {
    string n = _arrVar.arrLen.name();
    return n[0..$-3] ~ "iter";
  }
  override CstVarTerm unroll(CstVarIterBase itr, uint n) {
    if(this !is itr) {
      return _arrVar.unroll(itr,n).arrLen().makeItrVar();
    }
    else {
      return new CstVal!size_t(n); // CstVal!size_t.allocate(n);
    }
  }

  override uint resolveLap() {
    assert (false, "resolveLap should never be called on CstVarIter");
  }

  override void resolveLap(uint lap) {}

  override bool isConst() {
    return false;
  }

  override bool isOrderingExpr() {
    return false;		// only CstVecOrderingExpr return true
  }

  // this will not return the arrVar since the length variable is
  // not getting constrained here
  override CstVarPrim[] preReqs() {
    return [];
  }

  // get the list of stages this expression should be avaluated in
  // override CstStage[] getStages() {
  //   return arrVar.arrLen.getStages();
  // }

  override bool refresh(CstStage s, Buddy buddy) {
    assert(false, "Can not refresh for a Itr Variable without unrolling");
  }
  
  override BddVec getBDD(CstStage stage, Buddy buddy) {
    assert(false, "Can not getBDD for a Itr Variable without unrolling");
  }

  override long evaluate() {
    assert(false, "Can not evaluate a Itr Variable without unrolling");
  }

  override void setBddContext(CstBddExpr expr,
			      ref CstVarPrim[] vars,
			      ref CstVarIterBase iter,
			      ref CstVarPrim[] deps) {
  }
}

class CstVarLen(RV): CstVecDomain!(RV.RAND), CstVarPrim
{

  enum HAS_RAND_ATTRIB = (! __traits(isSame, RV.RAND, _esdl__norand));

  // This bdd has the constraint on the max length of the array
  BDD _primBdd;
  
  CstVarIter!RV _itrVar;

  RV _parent;

  BddVec _valvec;
  
  string _name;

  CstVarPrim[] _preReqs;

  override string name() {
    return _name;
  }

  this(string name, RV parent) {
    assert(parent !is null);
    _name = name;
    _parent = parent;
  }

  ~this() {
    // _domvec.reset();
    _valvec.reset();
    _primBdd.reset();
  }

  override CstVarPrim[] preReqs() {
    return _preReqs ~ _parent.preReqs();
  }

  override CstVarIterBase[] itrVars() {
    return _parent.itrVars();
  }

  override CstVarIterBase getIterator() {
    return _parent.getIterator();
  }

  override CstDomain[] unresolvedIdxs() {
    return _parent.parentUnresolvedIdxs();
  }

  override bool hasUnresolvedIdx() {
    // just must make sure that the
    // array -- if it has a parent array -- is not an abstract element of the parent array
    return _parent.parentLenIsUnresolved();
  }
      
  override CstDomain[] getRndDomains(bool resolved) {
    return _parent.getDomainLens(resolved);
  }

  CstDomain[] getDomainLens(bool resolved) {
    return [this];
  }
  
  private bool refreshNoRand(Buddy buddy) {
    auto val = _parent.getLen();
    if (! _valvec.isNull()) {
      return false;
    }
    else {
      _valvec.buildVec(buddy, val);
      return true;
    }
  }

  // override CstStage[] getStages() {
  //   CstStage[] stages;
  //   if(isRand) stages = [this.stage()];
  //   return stages;
  // }
  override bool refresh(CstStage s, Buddy buddy) {
    static if (HAS_RAND_ATTRIB) {
      auto fparent = _parent.flatten();
      if (fparent !is _parent) {
	return _parent.flatten().arrLen.refresh(s, buddy);
      }
      else {
	assert(stage() !is null, "stage null for: " ~ name());
	if(this.isRand && stage() is s) {
	  return false;
	}
	else if((! this.isRand) ||
		this.isRand && stage().solved()) { // work with the value
	  return refreshNoRand(buddy);
	}
	else {
	  assert(false, "Constraint evaluation in wrong stage");
	}
      }
    }
    else {
      return refreshNoRand(buddy);
    }
  }

  override BddVec getBDD(CstStage s, Buddy buddy) {
    static if (HAS_RAND_ATTRIB) {
      auto fparent = _parent.flatten();
      if (fparent !is _parent) {
	return _parent.flatten().arrLen.getBDD(s, buddy);
      }
      else {
	assert(stage() !is null, "stage null for: " ~ name());
	if(this.isRand && stage() is s) {
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

  override long evaluate() {
    static if (HAS_RAND_ATTRIB) {
      assert(stage() !is null);
      if(! this.isRand || stage().solved()) {
	return value();
      }
      else {
	import std.conv;
	assert(false, "Rand variable " ~ _name ~ " evaluation in wrong stage: " ~
	       stage()._id.to!string);
      }
    }
    else {
      return value();
    }
  }

  bool isResolved() {
    return stage().solved();
  }
  
  void _esdl__doRandomize(_esdl__RandGen randGen) {
    assert(false);
  }
  
  void _esdl__doRandomize(_esdl__RandGen randGen, CstStage s) {
    assert(s is stage);
    _parent.buildElements();
  }
  
  bool isRand() {
    static if (HAS_RAND_ATTRIB) {
      import std.traits;
      if (isStaticArray!(RV.L)) return false;
      else return true;
    }
    else {
      return false;
    }
  }

  T to(T)()
    if(is(T == string)) {
      import std.conv;
      if(isRand) {
	return "RAND-" ~ "#" ~ _name ~ ":" ~ value().to!string();
      }
      else {
	return "VAL#" ~ _name ~ ":" ~ value().to!string();
      }
    }

  override string toString() {
    return this.to!string();
  }

  BDD getPrimBdd(Buddy buddy) {
    if(_primBdd.isZero()) {
      _primBdd = this.bddvec(buddy).lte(buddy.buildVec(_parent.maxArrLen));
    }
    return _primBdd;
  }
  
  void resetPrimeBdd() {
    _primBdd.reset();
  }

  void itrVar(CstVarIter!RV var) {
    _itrVar = var;
  }

  CstVarIter!RV itrVar() {
    return _itrVar;
  }

  CstVarIter!RV makeItrVar() {
    if(_itrVar is null) {
      _itrVar = new CstVarIter!RV(_parent);
    }
    return _itrVar;
  }

  uint bitcount() {
    if (_parent.maxArrLen == -1) {
      return 32;
    }
    uint i = 1;
    for (size_t c=1; c < _parent.maxArrLen; c *= 2) {
      i++;
    }
    return i;
  }

  bool signed() {
    return false;
  }

  ulong value() {
    return _parent.getLen();
  }

  void collate(ulong v, int word = 0) {
    assert(word == 0);
    // import std.stdio;
    // writeln("Setting value for arrlen for ", _parent.name, " to ", v);
    // import std.stdio;
    // writeln("Setting length for array: ", _parent.name(), " to ", v);
    _parent.setLen(cast(size_t) v);
    // writeln("Getting length for array: ", _parent.name(), " as ", _parent.getLen());
    
  }

  override CstVarLen!RV unroll(CstVarIterBase itr, uint n) {
    return _parent.unroll(itr,n).arrLen();
  }

  void _esdl__reset() {
    _stage = null;
    _resolveLap = 0;
  }

  bool isVarArr() {
    return false;
  }

  void solveBefore(CstVarPrim other) {
    other.addPreRequisite(this);
  }

  void addPreRequisite(CstVarPrim domain) {
    _preReqs ~= domain;
  }

  override bool isConst() {
    return false;
  }
  
  override bool isOrderingExpr() {
    return false;		// only CstVecOrderingExpr return true
  }

  override void setBddContext(CstBddExpr expr,
			      ref CstVarPrim[] vars,
			      ref CstVarIterBase iter,
			      ref CstVarPrim[] deps) {
  }
}

abstract class CstValBase: CstVarTerm
{
  CstBddExpr _cstExpr;
  
  override CstVarPrim[] preReqs() {
    return [];
  }

  override CstVarIterBase[] itrVars() {
    return [];
  }

  override CstVarIterBase getIterator() {
    return null;
  }

  override CstDomain[] unresolvedIdxs() {
    return [];
  }
      
  override bool hasUnresolvedIdx() {
    return false;
  }
      
  override bool isConst() {
    return true;
  }

  override CstDomain[] getRndDomains(bool resolved) {
    return [];
  }

  override CstVarTerm unroll(CstVarIterBase l, uint n) {
    return this;
  }

  override void setBddContext(CstBddExpr expr,
			      ref CstVarPrim[] vars,
			      ref CstVarIterBase iter,
			      ref CstVarPrim[] deps) {
  }
}

auto _esdl__cstVal(T)(T val) {
  return new CstVal!(T)(val); // CstVal!(T).allocate(val);
}

class CstVal(T = int): CstValBase
{
  // static class Allocator: CstValAllocator {
  //   CstVal!T[] container;
  //   uint _index = 0;

  //   uint _mark;

  //   override void markIndex() {
  //     _mark = _index;
  //   }

  //   override void resetIndex() {
  //     for (uint i = _mark; i != _index; ++i) {
  // 	container[i]._valvec.reset();
  //     }
  //     _index = _mark;
      
  //   }


  //   CstVal!T allocate(T val) {
  //     // return new CstVal!T(val);
  //     if (_index >= container.length) {
  //   	container.length += 1;
  //   	container[$-1] = new CstVal!T(val);
  //     }
      
  //     auto cstVal = container[_index];
  //     cstVal._val = val;
  //     _index++;
  //     return cstVal;
  //   }
  // }

  import std.conv;

  // static Allocator _allocator;

  // static this() {
  //   CstVal!T._allocator = new Allocator;
  //   CstValAllocator.allocators ~= CstVal!T._allocator;
  // }

  T _val;			// the value of the constant
  BddVec _valvec;

  override string name() {
    return _val.to!string();
  }

  // static CstVal!T allocate(T value) {
  //   Allocator allocator = _allocator;
  //   if (allocator is null) {
  //     allocator = new Allocator;
  //     _allocator = allocator;
  //     CstValAllocator.allocators ~= allocator;
  //   }

  //   // return new CstVal!T(value);
  //   return allocator.allocate(value);
  // }

  this(T value) {
    _val = value;
  }

  ~this() {
    _valvec.reset();
  }

  override bool refresh(CstStage stage, Buddy buddy) {
    if (_valvec.isNull()) {
      _valvec.buildVec(buddy, _val);
      return true;
    }
    else {
      return false;
    }
  }
  
  override BddVec getBDD(CstStage stage, Buddy buddy) {
    return _valvec;
  }

  const(T)* getRef() {
    return &_val;
  }

  override long evaluate() {
    return _val;
  }

  override CstDomain[] unresolvedIdxs() {
    return [];
  }

  override bool hasUnresolvedIdx() {
    return false;
  }

  override uint resolveLap() {
    return 0;			// const
  }

  override void resolveLap(uint lap) {}

  override bool isOrderingExpr() {
    return false;		// only CstVecOrderingExpr return true
  }
}

// All the operations that produce a BddVec
enum CstBinVecOp: byte
  {   AND,
      OR ,
      XOR,
      ADD,
      SUB,
      MUL,
      DIV,
      REM,
      LSH,
      RSH,
      BITINDEX,
      }

// All the operations that produce a Bdd
enum CstBinBddOp: byte
  {   LTH,
      LTE,
      GTH,
      GTE,
      EQU,
      NEQ,
      }


enum CstBddOp: byte
  {   LOGICAND,
      LOGICOR ,
      LOGICIMP,
      }

// This class would hold two(bin) vector nodes and produces a vector
// only after processing those two nodes
class CstVec2VecExpr: CstVarTerm
{
  import std.conv;

  CstVarExpr _lhs;
  CstVarExpr _rhs;
  CstBinVecOp _op;

  // CstDomain[] _preReqs;
  override CstVarPrim[] preReqs() {
    return _lhs.preReqs() ~ _rhs.preReqs();
  }

  CstVarIterBase[] _itrVars;
  override CstVarIterBase[] itrVars() {
    return _itrVars;
  }

  override CstVarIterBase getIterator() {
    auto litr = _lhs.getIterator();
    auto ritr = _rhs.getIterator();
    if (litr !is null) {
      assert (ritr is null || ritr is litr);
      return litr;
    }
    else {
      return ritr;
    }
  }

  CstDomain[] _unresolvedIdxs;
  
  override CstDomain[] unresolvedIdxs() {
    return _unresolvedIdxs;
  }
  
  override bool hasUnresolvedIdx() {
    return _lhs.hasUnresolvedIdx() || _rhs.hasUnresolvedIdx();
  }
      
  override string name() {
    return "( " ~ _lhs.name ~ " " ~ _op.to!string() ~ " " ~ _rhs.name ~ " )";
  }

  override CstDomain[] getRndDomains(bool resolved) {
    return _lhs.getRndDomains(resolved) ~ _rhs.getRndDomains(resolved);
  }

  override bool refresh(CstStage stage, Buddy buddy) {
    auto l = _lhs.refresh(stage, buddy);
    auto r = _rhs.refresh(stage, buddy);
    return r || l;
  }
  
  override BddVec getBDD(CstStage stage, Buddy buddy) {
    if(this.itrVars.length !is 0) {
      assert(false,
	     "CstVec2VecExpr: Need to unroll the itrVars" ~
	     " before attempting to solve BDD");
    }

    // auto lvec = _lhs.getBDD(stage, buddy);
    // auto rvec = _rhs.getBDD(stage, buddy);

    final switch(_op) {
    case CstBinVecOp.AND: return _lhs.getBDD(stage, buddy) &
	_rhs.getBDD(stage, buddy);
    case CstBinVecOp.OR:  return _lhs.getBDD(stage, buddy) |
	_rhs.getBDD(stage, buddy);
    case CstBinVecOp.XOR: return _lhs.getBDD(stage, buddy) ^
	_rhs.getBDD(stage, buddy);
    case CstBinVecOp.ADD: return _lhs.getBDD(stage, buddy) +
	_rhs.getBDD(stage, buddy);
    case CstBinVecOp.SUB: return _lhs.getBDD(stage, buddy) -
	_rhs.getBDD(stage, buddy);
    case CstBinVecOp.MUL:
      if(_rhs.isConst()) return _lhs.getBDD(stage, buddy) *
			   _rhs.evaluate();
      if(_lhs.isConst()) return _lhs.evaluate() *
			   _rhs.getBDD(stage, buddy);
      return _lhs.getBDD(stage, buddy) * _rhs.getBDD(stage, buddy);
    case CstBinVecOp.DIV:
      if(_rhs.isConst()) return _lhs.getBDD(stage, buddy) /
			   _rhs.evaluate();
      return _lhs.getBDD(stage, buddy) / _rhs.getBDD(stage, buddy);
    case CstBinVecOp.REM:
      if(_rhs.isConst()) return _lhs.getBDD(stage, buddy) %
			   _rhs.evaluate();
      return _lhs.getBDD(stage, buddy) % _rhs.getBDD(stage, buddy);
    case CstBinVecOp.LSH:
      if(_rhs.isConst()) return _lhs.getBDD(stage, buddy) <<
			   _rhs.evaluate();
      return _lhs.getBDD(stage, buddy) << _rhs.getBDD(stage, buddy);
    case CstBinVecOp.RSH:
      if(_rhs.isConst()) return _lhs.getBDD(stage, buddy) >>
			   _rhs.evaluate();
      return _lhs.getBDD(stage, buddy) >> _rhs.getBDD(stage, buddy);
    case CstBinVecOp.BITINDEX:
      assert(false, "BITINDEX is not implemented yet!");
    }
  }

  override long evaluate() {
    auto lvec = _lhs.evaluate();
    auto rvec = _rhs.evaluate();

    final switch(_op) {
    case CstBinVecOp.AND: return lvec &  rvec;
    case CstBinVecOp.OR:  return lvec |  rvec;
    case CstBinVecOp.XOR: return lvec ^  rvec;
    case CstBinVecOp.ADD: return lvec +  rvec;
    case CstBinVecOp.SUB: return lvec -  rvec;
    case CstBinVecOp.MUL: return lvec *  rvec;
    case CstBinVecOp.DIV: return lvec /  rvec;
    case CstBinVecOp.REM: return lvec %  rvec;
    case CstBinVecOp.LSH: return lvec << rvec;
    case CstBinVecOp.RSH: return lvec >> rvec;
    case CstBinVecOp.BITINDEX:
      assert(false, "BITINDEX is not implemented yet!");
    }
  }

  override CstVec2VecExpr unroll(CstVarIterBase itr, uint n) {
    bool found = false;
    foreach(var; itrVars()) {
      if(itr is var) {
	found = true;
	break;
      }
    }
    if(! found) return this;
    else {
      return new CstVec2VecExpr(_lhs.unroll(itr, n), _rhs.unroll(itr, n), _op);
    }
  }

  this(CstVarExpr lhs, CstVarExpr rhs, CstBinVecOp op) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
    foreach (var; lhs.itrVars ~ rhs.itrVars) {
      bool add = true;
      foreach (l; _itrVars) {
    	if (l is var) add = false;
    	break;
      }
      if (add) _itrVars ~= var;
    }

    foreach (var; lhs.unresolvedIdxs ~ rhs.unresolvedIdxs) {
      bool add = true;
      foreach (l; _unresolvedIdxs) {
    	if (l is var) add = false;
    	break;
      }
      if (add) _unresolvedIdxs ~= var;
    }
  }

  override uint resolveLap() {
    auto lhs = _lhs.resolveLap();
    auto rhs = _rhs.resolveLap();
    if (rhs > lhs) return rhs;
    else return lhs;
  }

  override void resolveLap(uint lap) {
    _lhs.resolveLap(lap);
    _rhs.resolveLap(lap);
  }
  
  override bool isConst() {
    return false;
  }

  override bool isOrderingExpr() {
    return false;		// only CstVecOrderingExpr return true
  }

  override void setBddContext(CstBddExpr expr,
			      ref CstVarPrim[] vars,
			      ref CstVarIterBase iter,
			      ref CstVarPrim[] deps) {
  }
}

class CstVecSliceExpr: CstVarTerm
{
  CstVarExpr _vec;
  CstVarExpr _lhs;
  CstVarExpr _rhs;

  // CstDomain[] _preReqs;
  override CstVarPrim[] preReqs() {
    // CstVarPrim[] reqs;
    if(_rhs is null) {
      return _vec.preReqs() ~ _lhs.preReqs();
      // foreach(req; _vec.preReqs() ~ _lhs.preReqs()) {
      // 	if(! req.solved()) {
      // 	  reqs ~= req;
      // 	}
      // }
    }
    else {
      return _vec.preReqs() ~ _lhs.preReqs() ~ _rhs.preReqs();
      // foreach(req; _vec.preReqs() ~ _lhs.preReqs() ~ _rhs.preReqs()) {
      // 	if(! req.solved()) {
      // 	  reqs ~= req;
      // 	}
      // }
    }
    // return reqs;
  }
  
  CstVarIterBase[] _itrVars;
  override CstVarIterBase[] itrVars() {
    return _itrVars;
  }

  override CstVarIterBase getIterator() {
    auto litr = _lhs.getIterator();
    auto ritr = _rhs.getIterator();
    if (litr !is null) {
      assert (ritr is null || ritr is litr);
      return litr;
    }
    else {
      return ritr;
    }
  }

  CstDomain[] _unresolvedIdxs;
  override CstDomain[] unresolvedIdxs() {
    return _unresolvedIdxs;
  }

  override bool hasUnresolvedIdx() {
    return
      _lhs.hasUnresolvedIdx() ||
      _rhs.hasUnresolvedIdx() ||
      _vec.hasUnresolvedIdx();
  }

  override string name() {
    return _vec.name() ~ "[ " ~ _lhs.name() ~ " .. " ~ _rhs.name() ~ " ]";
  }

  override CstDomain[] getRndDomains(bool resolved) {
    if(_rhs is null) {
      return _vec.getRndDomains(resolved) ~ _lhs.getRndDomains(resolved);
    }
    else {
      return _vec.getRndDomains(resolved) ~ _lhs.getRndDomains(resolved) ~ _rhs.getRndDomains(resolved);
    }
  }

  override bool refresh(CstStage stage, Buddy buddy) {
    auto l = _lhs.refresh(stage, buddy);
    auto r = _rhs.refresh(stage, buddy);
    return r || l;
  }
  
  override BddVec getBDD(CstStage stage, Buddy buddy) {
    if(this.itrVars.length !is 0) {
      assert(false,
	     "CstVecSliceExpr: Need to unroll the itrVars" ~
	     " before attempting to solve BDD");
    }

    auto vec  = _vec.getBDD(stage, buddy);
    size_t lvec = cast(size_t) _lhs.evaluate();
    size_t rvec = lvec;
    if(_rhs is null) {
      rvec = lvec + 1;
    }
    else {
      rvec = cast(size_t) _rhs.evaluate();
    }
    return vec[lvec..rvec];
  }

  override long evaluate() {
    // auto vec  = _vec.evaluate();
    // auto lvec = _lhs.evaluate();
    // auto rvec = _rhs.evaluate();

    assert(false, "Can not evaluate a CstVecSliceExpr!");
  }

  override CstVecSliceExpr unroll(CstVarIterBase itr, uint n) {
    bool found = false;
    foreach(var; itrVars()) {
      if(itr is var) {
	found = true;
	break;
      }
    }
    if(! found) return this;
    else {
      if(_rhs is null) {
	return new CstVecSliceExpr(_vec.unroll(itr, n), _lhs.unroll(itr, n));
      }
      else {
	return new CstVecSliceExpr(_vec.unroll(itr, n),
				   _lhs.unroll(itr, n), _rhs.unroll(itr, n));
      }
    }
  }

  this(CstVarExpr vec, CstVarExpr lhs, CstVarExpr rhs=null) {
    _vec = vec;
    _lhs = lhs;
    _rhs = rhs;
    auto itrVars = vec.itrVars ~ lhs.itrVars;
    if(rhs !is null) {
      itrVars ~= rhs.itrVars;
    }
    foreach(var; itrVars) {
      bool add = true;
      foreach(l; _itrVars) {
	if(l is var) add = false;
	break;
      }
      if(add) _itrVars ~= var;
    }

    auto unresolvedIdxs = vec.unresolvedIdxs() ~ lhs.unresolvedIdxs();
    if(rhs !is null) {
      unresolvedIdxs ~= rhs.unresolvedIdxs();
    }

    foreach(idx; unresolvedIdxs) {
      bool add = true;
      foreach(l; _unresolvedIdxs) {
	if(l is idx) add = false;
	break;
      }
      if(add) _unresolvedIdxs ~= idx;
    }

  }

  override uint resolveLap() {
    return _vec.resolveLap();
  }

  override void resolveLap(uint lap) {
    _vec.resolveLap(lap);
  }

  override bool isConst() {
    return false;
  }

  override bool isOrderingExpr() {
    return false;		// only CstVecOrderingExpr return true
  }

  override void setBddContext(CstBddExpr expr,
			      ref CstVarPrim[] vars,
			      ref CstVarIterBase iter,
			      ref CstVarPrim[] deps) {
  }
}

class CstNotVecExpr: CstVarTerm
{
  import std.conv;

  CstVarExpr _expr;

  // CstDomain[] _preReqs;
  override CstVarPrim[] preReqs() {
    return _expr.preReqs();
  }

  CstVarIterBase[] _itrVars;
  override CstVarIterBase[] itrVars() {
    return _itrVars;
  }

  override CstVarIterBase getIterator() {
    return _expr.getIterator();
  }

  override CstDomain[] unresolvedIdxs() {
    return _expr.unresolvedIdxs();
  }

  override bool hasUnresolvedIdx() {
    return _expr.hasUnresolvedIdx();
  }
      
  override string name() {
    return "( ~ " ~ _expr.name ~ " )";
  }

  override CstDomain[] getRndDomains(bool resolved) {
    return _expr.getRndDomains(resolved);
  }

  override bool refresh(CstStage stage, Buddy buddy) {
    return _expr.refresh(stage, buddy);
  }
  
  override BddVec getBDD(CstStage stage, Buddy buddy) {
    if(this.itrVars.length !is 0) {
      assert(false,
	     "CstNotVecExpr: Need to unroll the itrVars" ~
	     " before attempting to solve BDD");
    }

    return ~(_expr.getBDD(stage, buddy));
  }

  override long evaluate() {
    return ~(_expr.evaluate());
  }

  override CstNotVecExpr unroll(CstVarIterBase itr, uint n) {
    bool found = false;
    foreach(var; itrVars()) {
      if(itr is var) {
	found = true;
	break;
      }
    }
    if(! found) return this;
    else {
      return new CstNotVecExpr(_expr.unroll(itr, n));
    }
  }

  this(CstVarExpr expr) {
    _expr = expr;
    _itrVars = _expr.itrVars;
  }

  override uint resolveLap() {
    return _expr.resolveLap();
  }

  override void resolveLap(uint lap) {
    _expr.resolveLap(lap);
  }
  
  override bool isConst() {
    return false;
  }

  override bool isOrderingExpr() {
    return false;		// only CstVecOrderingExpr return true
  }

  override void setBddContext(CstBddExpr expr,
			      ref CstVarPrim[] vars,
			      ref CstVarIterBase iter,
			      ref CstVarPrim[] deps) {
  }
}

class CstNegVecExpr: CstVarTerm
{
  import std.conv;

  CstVarExpr _expr;

  // CstDomain[] _preReqs;
  override CstVarPrim[] preReqs() {
    return _expr.preReqs();
  }

  CstVarIterBase[] _itrVars;
  override CstVarIterBase[] itrVars() {
    return _itrVars;
  }

  override CstVarIterBase getIterator() {
    return _expr.getIterator();
  }

  override CstDomain[] unresolvedIdxs() {
    return _expr.unresolvedIdxs();
  }

  override bool hasUnresolvedIdx() {
    return _expr.hasUnresolvedIdx();
  }
      
  override string name() {
    return "( - " ~ _expr.name ~ " )";
  }

  override CstDomain[] getRndDomains(bool resolved) {
    return _expr.getRndDomains(resolved);
  }

  override bool refresh(CstStage stage, Buddy buddy) {
    return _expr.refresh(stage, buddy);
  }
  
  override BddVec getBDD(CstStage stage, Buddy buddy) {
    if(this.itrVars.length !is 0) {
      assert(false,
	     "CstNegVecExpr: Need to unroll the itrVars" ~
	     " before attempting to solve BDD");
    }

    return -(_expr.getBDD(stage, buddy));
  }

  override long evaluate() {
    return -(_expr.evaluate());
  }

  override CstNegVecExpr unroll(CstVarIterBase itr, uint n) {
    bool found = false;
    foreach(var; itrVars()) {
      if(itr is var) {
	found = true;
	break;
      }
    }
    if(! found) return this;
    else {
      return new CstNegVecExpr(_expr.unroll(itr, n));
    }
  }

  this(CstVarExpr expr) {
    _expr = expr;
    _itrVars = _expr.itrVars;
  }

  override uint resolveLap() {
    return _expr.resolveLap();
  }

  override void resolveLap(uint lap) {
    _expr.resolveLap(lap);
  }
  
  override bool isConst() {
    return false;
  }

  override bool isOrderingExpr() {
    return false;		// only CstVecOrderingExpr return true
  }

  override void setBddContext(CstBddExpr expr,
			      ref CstVarPrim[] vars,
			      ref CstVarIterBase iter,
			      ref CstVarPrim[] deps) {
  }
}


class CstBdd2BddExpr: CstBddTerm
{
  import std.conv;

  CstBddExpr _lhs;
  CstBddExpr _rhs;
  CstBddOp _op;

  CstVarIterBase[] _itrVars;

  CstDomain[] _unresolvedIdxs;
  
  override CstVarIterBase[] itrVars() {
       return _itrVars;
  }

  override CstVarIterBase getIterator() {
    auto litr = _lhs.getIterator();
    auto ritr = _rhs.getIterator();
    if (litr !is null) {
      assert (ritr is null || ritr is litr);
      return litr;
    }
    else {
      return ritr;
    }
  }

  this(CstBddExpr lhs, CstBddExpr rhs, CstBddOp op) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;

    foreach (var; lhs.itrVars ~ rhs.itrVars) {
      bool add = true;
      foreach (l; _itrVars) {
	if (l is var) add = false;
	break;
      }
      if (add) _itrVars ~= var;
    }

    foreach (var; lhs.unresolvedIdxs ~ rhs.unresolvedIdxs) {
      bool add = true;
      foreach (l; _unresolvedIdxs) {
	if (l is var) add = false;
	break;
      }
      if (add) _unresolvedIdxs ~= var;
    }
  }

  override bool refresh(CstStage stage, Buddy buddy) {
    auto l = _lhs.refresh(stage, buddy);
    auto r = _rhs.refresh(stage, buddy);
    return r || l;
  }
  
  override CstDomain[] unresolvedIdxs() {
    return _unresolvedIdxs;
  }

  override bool hasUnresolvedIdx() {
    return _lhs.hasUnresolvedIdx() || _rhs.hasUnresolvedIdx();
  }

  override string name() {
    return "( " ~ _lhs.name ~ " " ~ _op.to!string ~ " " ~ _rhs.name ~ " )";
  }

  override CstVarPrim[] preReqs() {
    return _lhs.preReqs() ~ _rhs.preReqs();
    // CstVarPrim[] reqs;
    // foreach(req; _lhs.preReqs() ~ _rhs.preReqs()) {
    //   if(! req.solved()) {
    // 	reqs ~= req;
    //   }
    // }
    // return reqs;
  }

  override CstDomain[] getRndDomains(bool resolved) {
    return _lhs.getRndDomains(resolved) ~ _rhs.getRndDomains(resolved);
  }


  override BDD getBDD(CstStage stage, Buddy buddy) {
    if(this.itrVars.length !is 0) {
      assert(false,
	     "CstBdd2BddExpr: Need to unroll the itrVars" ~
	     " before attempting to solve BDD");
    }
    auto lvec = _lhs.getBDD(stage, buddy);
    auto rvec = _rhs.getBDD(stage, buddy);

    BDD retval;
    final switch(_op) {
    case CstBddOp.LOGICAND: retval = lvec &  rvec; break;
    case CstBddOp.LOGICOR:  retval = lvec |  rvec; break;
    case CstBddOp.LOGICIMP: retval = lvec >> rvec; break;
    }
    return retval;
  }

  override CstBdd2BddExpr unroll(CstVarIterBase itr, uint n) {
    bool found = false;
    foreach(var; itrVars()) {
      if(itr is var) {
	found = true;
	break;
      }
    }
    if(! found) return this;
    else {
      return new CstBdd2BddExpr(_lhs.unroll(itr, n), _rhs.unroll(itr, n), _op);
    }
  }

  override uint resolveLap() {
    uint lhs = _lhs.resolveLap();
    uint rhs = _rhs.resolveLap();
    if (lhs > rhs) return lhs;
    else return rhs;
  }
  override void resolveLap(uint lap) {
    _lhs.resolveLap(lap);
    _rhs.resolveLap(lap);
  }

  bool getIntRangeSet(T)(ref IntRangeSet rset) {
    if (op == CstBddOp.LOGICAND) {
      return false;
    }
    if (op == CstBddOp.LOGICOR) {
      return false;
    }
    else return false;
  }

  override void setBddContext(CstBddExpr expr,
			      ref CstVarPrim[] vars,
			      ref CstVarIterBase iter,
			      ref CstVarPrim[] deps) {
  }
}

// TBD
class CstIteBddExpr: CstBddTerm
{
  CstVarIterBase[] _itrVars;

  override CstVarIterBase[] itrVars() {
    assert(false, "TBD");
  }

  override CstVarIterBase getIterator() {
    assert(false, "TBD");
  }

  override CstDomain[] unresolvedIdxs() {
    assert(false, "TBD");
  }

  override bool hasUnresolvedIdx() {
    assert(false, "TBD");
  }

  override string name() {
    return "CstIteBddExpr";
  }

  override bool refresh(CstStage stage, Buddy buddy) {
    assert(false);
  }

  bool getIntRangeSet(T)(ref IntRangeSet rset) {
    return false;
  }

  override void setBddContext(CstBddExpr expr,
			      ref CstVarPrim[] vars,
			      ref CstVarIterBase iter,
			      ref CstVarPrim[] deps) {
    assert(false, "TBD");
  }
}

class CstNopBddExpr: CstBddTerm
{
  CstVarExpr _vec;

  override CstVarIterBase[] itrVars() {
    return _vec.itrVars();
  }

  override CstVarIterBase getIterator() {
    return _vec.getIterator();
  }
  
  this(CstVarExpr vec) {
    _vec = vec;
  }

  override bool cstExprIsNop() {
    return true;
  }

  override string name() {
    return "( NOP: " ~ _vec.name ~ " )";
  }

  override bool refresh(CstStage stage, Buddy buddy) {
    return _vec.refresh(stage, buddy);
  }
  
  override CstVarPrim[] preReqs() {
    return _vec.preReqs();
  }
    
  override CstDomain[] getRndDomains(bool resolved) {
    return _vec.getRndDomains(resolved);
  }

  override BDD getBDD(CstStage stage, Buddy buddy) {
    return buddy.one();
  }

  override CstNopBddExpr unroll(CstVarIterBase itr, uint n) {
    bool shouldUnroll = false;
    foreach(var; itrVars()) {
      if(itr is var) {
	shouldUnroll = true;
	break;
      }
    }
    if(! shouldUnroll) return this;
    else {
      return new CstNopBddExpr(_vec.unroll(itr, n));
    }
  }

  override CstDomain[] unresolvedIdxs() {
    return _vec.unresolvedIdxs();
  }

  override bool hasUnresolvedIdx() {
    return _vec.hasUnresolvedIdx();
  }

  override uint resolveLap() {
    return _vec.resolveLap();
  }
  
  override void resolveLap(uint lap) {
    _vec.resolveLap(lap);
  }

  bool getIntRangeSet(T)(ref IntRangeSet rset) {
    return true;
  }

  override void setBddContext(CstBddExpr expr,
			      ref CstVarPrim[] vars,
			      ref CstVarIterBase iter,
			      ref CstVarPrim[] deps) {
  }
}

class CstVec2BddExpr: CstBddTerm
{
  import std.conv;

  CstVarExpr _lhs;
  CstVarExpr _rhs;
  CstBinBddOp _op;

  CstVarIterBase[] _itrVars;
  CstDomain[] _unresolvedIdxs;

  override CstVarIterBase[] itrVars() {
       return _itrVars;
  }

  override CstVarIterBase getIterator() {
    auto litr = _lhs.getIterator();
    auto ritr = _rhs.getIterator();
    if (litr !is null) {
      assert (ritr is null || ritr is litr);
      return litr;
    }
    else {
      return ritr;
    }
  }

  override CstDomain[] unresolvedIdxs() {
       return _unresolvedIdxs;
  }

  this(CstVarExpr lhs, CstVarExpr rhs, CstBinBddOp op) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;

    foreach (var; lhs.itrVars ~ rhs.itrVars) {
      bool add = true;
      foreach (l; _itrVars) {
	if (l is var) add = false;
	break;
      }
      if (add) _itrVars ~= var;
    }

    foreach (var; lhs.unresolvedIdxs ~ rhs.unresolvedIdxs) {
      bool add = true;
      foreach (l; _unresolvedIdxs) {
	if (l is var) add = false;
	break;
      }
      if (add) _unresolvedIdxs ~= var;
    }
  }

  override string name() {
    return "( " ~ _lhs.name ~ " " ~ _op.to!string ~ " " ~ _rhs.name ~ " )";
  }

  override bool refresh(CstStage stage, Buddy buddy) {
    auto l = _lhs.refresh(stage, buddy);
    auto r = _rhs.refresh(stage, buddy);
    return r || l;
  }
  
  override CstVarPrim[] preReqs() {
    return _lhs.preReqs() ~ _rhs.preReqs();
    // CstVarPrim[] reqs;
    // foreach(req; _lhs.preReqs() ~ _rhs.preReqs()) {
    //   if(! req.solved()) {
    // 	reqs ~= req;
    //   }
    // }
    // return reqs;
  }
    
  override CstDomain[] getRndDomains(bool resolved) {
    return _lhs.getRndDomains(resolved) ~ _rhs.getRndDomains(resolved);
  }

  override BDD getBDD(CstStage stage, Buddy buddy) {
    if (this.itrVars.length !is 0) {
      // foreach (itr; itrVars) {
      // 	import std.stdio;
      // 	writeln(name(), " : ", itr.name());
      // }
      assert(false,
	     "CstVec2BddExpr: Need to unroll the itrVars" ~
	     " before attempting to solve BDD");
    }
    auto lvec = _lhs.getBDD(stage, buddy);
    auto rvec = _rhs.getBDD(stage, buddy);

    BDD retval;
    final switch(_op) {
    case CstBinBddOp.LTH: retval = lvec.lth(rvec); break;
    case CstBinBddOp.LTE: retval = lvec.lte(rvec); break;
    case CstBinBddOp.GTH: retval = lvec.gth(rvec); break;
    case CstBinBddOp.GTE: retval = lvec.gte(rvec); break;
    case CstBinBddOp.EQU: retval = lvec.equ(rvec); break;
    case CstBinBddOp.NEQ: retval = lvec.neq(rvec); break;
    }
    return retval;
  }

  override CstVec2BddExpr unroll(CstVarIterBase itr, uint n) {
    // import std.stdio;
    // writeln(_lhs.name() ~ " " ~ _op.to!string ~ " " ~ _rhs.name() ~ " Getting unwound!");
    bool found = false;
    foreach(var; itrVars()) {
      if(itr is var) {
	found = true;
	break;
      }
    }
    if(! found) return this;
    else {
      // writeln("RHS: ", _rhs.unroll(itr, n).name());
      // writeln("LHS: ", _lhs.unroll(itr, n).name());
      return new CstVec2BddExpr(_lhs.unroll(itr, n), _rhs.unroll(itr, n), _op);
    }
  }

  override bool hasUnresolvedIdx() {
    return _lhs.hasUnresolvedIdx() || _rhs.hasUnresolvedIdx();
  }

  override uint resolveLap() {
    uint lhs = _lhs.resolveLap();
    uint rhs = _rhs.resolveLap();
    if (lhs > rhs) return lhs;
    else return rhs;
  }
  
  override void resolveLap(uint lap) {
    _lhs.resolveLap(lap);
    _rhs.resolveLap(lap);
  }


  bool getIntRangeSet(T)(ref IntRangeSet rset) {
    auto lrnd = _lhs.getRndDomains(true);
    auto rrnd = _rhs.getRndDomains(true);

    if (rrnd.length == 0) {
      
    }
  }

  override void setBddContext(CstBddExpr expr,
			      ref CstVarPrim[] vars,
			      ref CstVarIterBase iter,
			      ref CstVarPrim[] deps) {
  }
}

class CstBddConst: CstBddTerm
{
  immutable bool _expr;

  override CstVarIterBase[] itrVars() {
       return [];
  }

  override CstVarIterBase getIterator() {
    return null;
  }
  
  this(bool expr) {
    _expr = expr;
  }

  override bool refresh(CstStage stage, Buddy buddy) {
    return false;
  }
  
  override BDD getBDD(CstStage stage, Buddy buddy) {
    if(_expr) return buddy.one();
    else return buddy.zero();
  }

  override string name() {
    if(_expr) return "TRUE";
    else return "FALSE";
  }

  override CstDomain[] getRndDomains(bool resolved) {
    return [];
  }

  override CstVarPrim[] preReqs() {
    return [];
  }

  override CstBddConst unroll(CstVarIterBase itr, uint n) {
    return this;
  }

  override CstDomain[] unresolvedIdxs() {
    return [];
  }

  override bool hasUnresolvedIdx() {
    return false;
  }

  override uint resolveLap() {
    return 0;
  }
  override void resolveLap(uint lap) {}

  override void setBddContext(CstBddExpr expr,
			      ref CstVarPrim[] vars,
			      ref CstVarIterBase iter,
			      ref CstVarPrim[] deps) {
  }
}

class CstNotBddExpr: CstBddTerm
{
  CstBddExpr _expr;

  this(CstBddExpr expr) {
    _expr = expr;
  }

  override CstVarIterBase[] itrVars() {
    return _expr.itrVars();
  }

  override CstVarIterBase getIterator() {
    return _expr.getIterator();
  }

  override string name() {
    return "( " ~ "!" ~ " " ~ _expr.name ~ " )";
  }

  override bool refresh(CstStage stage, Buddy buddy) {
    return _expr.refresh(stage, buddy);
  }
  
  override CstVarPrim[] preReqs() {
    return _expr.preReqs();
  }

  override CstDomain[] getRndDomains(bool resolved) {
    return _expr.getRndDomains(resolved);
  }

  override BDD getBDD(CstStage stage, Buddy buddy) {
    if(this.itrVars.length !is 0) {
      assert(false,
	     "CstBdd2BddExpr: Need to unroll the itrVars" ~
	     " before attempting to solve BDD");
    }
    auto bdd = _expr.getBDD(stage, buddy);
    return (~ bdd);
  }

  override CstNotBddExpr unroll(CstVarIterBase itr, uint n) {
    bool shouldUnroll = false;
    foreach(var; itrVars()) {
      if(itr is var) {
	shouldUnroll = true;
	break;
      }
    }
    if(! shouldUnroll) return this;
    else {
      return new CstNotBddExpr(_expr.unroll(itr, n));
    }
  }

  override CstDomain[] unresolvedIdxs() {
    return _expr.unresolvedIdxs();
  }

  override bool hasUnresolvedIdx() {
    return _expr.hasUnresolvedIdx();
  }

  override uint resolveLap() {
    return _expr.resolveLap();
  }
  override void resolveLap(uint lap) {
    _expr.resolveLap(lap);
  }

  override void setBddContext(CstBddExpr expr,
			      ref CstVarPrim[] vars,
			      ref CstVarIterBase iter,
			      ref CstVarPrim[] deps) {
  }
}

// CstBdd2BddExpr logicOr(CstVarExpr other)
// {
//   return new CstBdd2BddExpr(toBdd(this), toBdd(other), CstBddOp.LOGICOR);
// }

auto _esdl__logicOr(P, Q)(P p, Q q) {
  CstBddTerm _p;
  CstBddTerm _q;
  static if (is (P == bool)) {
    _p = new CstBddConst(p);
  }
  else static if (is (P: CstVarExpr)) {
    _p = toBdd(p);
  }
  else {
    _p = p;
  }
  static if(is(Q == bool)) {
    _q = new CstBddConst(q);
  }
  else static if (is (Q: CstVarExpr)) {
    _q = toBdd(q);
  }
  else {
    _q = q;
  }
  return _p.logicOr(_q);
}

auto _esdl__logicAnd(P, Q)(P p, Q q) {
  CstBddTerm _p;
  CstBddTerm _q;
  static if(is(P == bool)) {
    _p = new CstBddConst(p);
  }
  else static if (is (P: CstVarExpr)) {
    _p = toBdd(p);
  }
  else {
    _p = p;
  }
  static if(is(Q == bool)) {
    _q = new CstBddConst(q);
  }
  else static if (is (Q: CstVarExpr)) {
    _q = toBdd(q);
  }
  else {
    _q = q;
  }
  return _p.logicAnd(_q);
}


CstVec2BddExpr _esdl__lth(Q)(CstVarExpr left, Q q)
  if(isBitVector!Q || isIntegral!Q) {
    auto qq = new CstVal!Q(q); // CstVal!Q.allocate(q);
    return _esdl__lth(left, qq);
  }

CstVec2BddExpr _esdl__lth(CstVarExpr p, CstVarExpr q) {
  return new CstVec2BddExpr(p, q, CstBinBddOp.LTH);
}

auto _esdl__lth(P, Q)(P p, Q q) {
  static if(is(P: CstVarExpr)) {
    return _esdl__lth(p, q);
  }
  else static if(is(Q: CstVarExpr)) {
    return _esdl__gte(q, p);
  }
  else static if((isBitVector!P || isIntegral!P) &&
		 (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p < q);
  }
}

CstVec2BddExpr _esdl__lte(Q)(CstVarExpr p, Q q)
  if(isBitVector!Q || isIntegral!Q) {
    auto qq = new CstVal!Q(q); // CstVal!Q.allocate(q);
    return _esdl__lte(p, qq);
  }

CstVec2BddExpr _esdl__lte(CstVarExpr p, CstVarExpr q) {
  return new CstVec2BddExpr(p, q, CstBinBddOp.LTE);
}

auto _esdl__lte(P, Q)(P p, Q q) {
  static if(is(P: CstVarExpr)) {
    return _esdl__lte(p, q);
  }
  else static if(is(Q: CstVarExpr)) {
    return _esdl__gth(q, p);
  }
  else static if((isBitVector!P || isIntegral!P) &&
		 (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p <= q);
  }
}

CstVec2BddExpr _esdl__gth(Q)(CstVarExpr p, Q q)
  if(isBitVector!Q || isIntegral!Q) {
    auto qq = new CstVal!Q(q); // CstVal!Q.allocate(q);
    return _esdl__gth(p, qq);
  }

CstVec2BddExpr _esdl__gth(CstVarExpr p, CstVarExpr q) {
  return new CstVec2BddExpr(p, q, CstBinBddOp.GTH);
}

auto _esdl__gth(P, Q)(P p, Q q) {
  static if(is(P: CstVarExpr)) {
    return _esdl__gth(p, q);
  }
  else static if(is(Q: CstVarExpr)) {
    return _esdl__lte(q, p);
  }
  else static if((isBitVector!P || isIntegral!P) &&
		 (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p > q);
  }
}

CstVec2BddExpr _esdl__gte(Q)(CstVarExpr p, Q q)
  if(isBitVector!Q || isIntegral!Q) {
    auto qq = new CstVal!Q(q); // CstVal!Q.allocate(q);
    return _esdl__gte(p, qq);
  }

CstVec2BddExpr _esdl__gte(CstVarExpr p, CstVarExpr q) {
  return new CstVec2BddExpr(p, q, CstBinBddOp.GTE);
}

auto _esdl__gte(P, Q)(P p, Q q) {
  static if(is(P: CstVarExpr)) {
    return _esdl__gte(p, q);
  }
  else static if(is(Q: CstVarExpr)) {
    return _esdl__lth(q, p);
  }
  else static if((isBitVector!P || isIntegral!P) &&
		 (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p >= q);
  }
}

CstVec2BddExpr _esdl__equ(Q)(CstVarExpr p, Q q)
  if(isBitVector!Q || isIntegral!Q) {
    auto qq = new CstVal!Q(q); // CstVal!Q.allocate(q);
    return _esdl__equ(p, qq);
  }

CstVec2BddExpr _esdl__equ(CstVarExpr p, CstVarExpr q) {
  return new CstVec2BddExpr(p, q, CstBinBddOp.EQU);
}

auto _esdl__equ(P, Q)(P p, Q q) {
  static if(is(P: CstVarExpr)) {
    return _esdl__equ(p, q);
  }
  else static if(is(Q: CstVarExpr)) {
    return _esdl__equ(q, p);
  }
  else static if((isBitVector!P || isIntegral!P) &&
		 (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p == q);
  }
}

CstVec2BddExpr _esdl__neq(Q)(CstVarExpr p, Q q)
  if(isBitVector!Q || isIntegral!Q) {
    auto qq = new CstVal!Q(q); // CstVal!Q.allocate(q);
    return _esdl__neq(p, qq);
  }

CstVec2BddExpr _esdl__neq(CstVarExpr p, CstVarExpr q) {
  return new CstVec2BddExpr(p, q, CstBinBddOp.NEQ);
}

auto _esdl__neq(P, Q)(P p, Q q) {
  static if(is(P: CstVarExpr)) {
    return _esdl__neq(p, q);
  }
  else static if(is(Q: CstVarExpr)) {
    return _esdl__neq(q, p);
  }
  else static if((isBitVector!P || isIntegral!P) &&
		 (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p != q);
  }
}
