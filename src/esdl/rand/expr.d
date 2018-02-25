module esdl.rand.expr;

import esdl.data.bvec: isBitVector;
import esdl.data.bstr;
import std.traits: isIntegral, isBoolean, isArray, isStaticArray, isDynamicArray;

import esdl.rand.obdd;
import esdl.rand.base: _esdl__RandGen, _esdl__norand,
  getRandAttr, getRandAttrN;


template isVarSigned(L) {
  import std.traits: isIntegral, isSigned;
  static if(isBitVector!L)
    enum bool isVarSigned = L.ISSIGNED;
  else static if(isIntegral!L)
    enum bool isVarSigned = isSigned!L;
  else
    static assert(false, "isVarSigned: Can not determine sign of type " ~
		  typeid(L));
}

// ToDo -- create a freelist of CstStage's
class CstStage {
  int _id = -1;
  // List of randomized variables associated with this stage. A
  // variable can be associated with only one stage
  CstVarPrim[] _rndVars;
  // The Bdd expressions that apply to this stage
  CstBddExpr[] _bddExprs;
  // These are unresolved itr variables
  CstVarIterBase[] _itrVars;
  // These are the length variables that this stage will solve
  // CstVarPrim[] _preReqs;
  CstBddExpr[] _bddExprsWithUnmetReqs;
  
  BDD _solveBDD;
  ~this() {
    _solveBDD.reset();
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

  bool allReqsMet() {
    CstBddExpr[] bddExprsWithUnmetReqs;
    foreach(expr; _bddExprsWithUnmetReqs) {
      if(expr.preReqs().length !=0) {
	// import std.stdio;
	// writeln(expr.name ~ " needs preReqs resolution, ", expr.preReqs());
	bddExprsWithUnmetReqs ~= expr;
      }
    }
    _bddExprsWithUnmetReqs = bddExprsWithUnmetReqs;
    return _bddExprsWithUnmetReqs.length == 0;
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


interface CstVarPrim
{
  abstract string name();
  abstract void doRandomize(_esdl__RandGen randGen);
  abstract bool isRand();
  // abstract ulong value();

  abstract void collate(ulong v, int word=0);
  abstract CstStage stage();
  abstract void stage(CstStage s);
  abstract void _esdl__reset();
  abstract bool isVarArr();
  abstract uint domIndex();
  abstract void domIndex(uint s);
  abstract uint bitcount();
  abstract bool signed();
  abstract ref BddVec bddvec();
  abstract void bddvec(BddVec b);
  abstract CstVarPrim[] getPrimLens();
  abstract void solveBefore(CstVarPrim other);
  abstract void addPreRequisite(CstVarPrim other);

  // this method is used for getting implicit constraints that are required for
  // dynamic arrays and for enums
  abstract BDD getPrimBdd(Buddy buddy);
  abstract void resetPrimeBdd();
  final bool solved() {
    if(isRand()) {
      return stage() !is null && stage().solved();
    }
    else {
      return true;
    }
  }
}


// proxy class for reading in the constraints lazily
// An abstract class that returns a vector on evaluation
abstract class CstVarExpr
{
  // alias toBdd this;

  // alias evaluate this;

  abstract string name();
  
  CstBddExpr toBdd() {
    auto zero = CstVal!int.allocate(0);
    return new CstVec2BddExpr(this, zero, CstBinBddOp.NEQ);
  }

  // Array of indexes this expression has to resolve before it can be
  // convertted into an BDD
  abstract CstVarIterBase[] itrVars();

  // List of Array Variables
  abstract CstVarPrim[] preReqs();

  bool isConst() {
    return false;
  }

  // get all the primary bdd vectors that constitute a given bdd
  // expression
  // The idea here is that we need to solve all the bdd vectors of a
  // given constraint equation together. And so, given a constraint
  // equation, we want to list out the elements that need to be
  // grouped together.
  abstract CstVarPrim[] getRndPrims();

  // get all the primary bdd vectors that would be solved together
  CstVarPrim[] getSolvables() {
    return getRndPrims();
  }
  
  // get the list of stages this expression should be avaluated in
  // abstract CstStage[] getStages();
  abstract BddVec getBDD(CstStage stage, Buddy buddy);

  // refresh the _valvec if the current value is not the same as previous value
  abstract bool refresh(CstStage stage, Buddy buddy);

  abstract long evaluate();

  abstract CstVarExpr unroll(CstVarIterBase l, uint n);

  CstVec2VecExpr opBinary(string op)(CstVarExpr other)
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
  	auto qq = CstVal!Q.allocate(q);
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
	auto qq = CstVal!Q.allocate(q);
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

  CstVarExpr opIndex(CstVarExpr index)
  {
    // assert(false, "Index operation defined only for Arrays");
    return new CstVecSliceExpr(this, index);
  }

  CstVarExpr opSlice(P)(P p)
    if(isIntegral!P || isBitVector!P) {
      return new CstVecSliceExpr(this, CstVal!P.allocate(p));
    }

  CstVarExpr opSlice(CstVarExpr lhs, CstVarExpr rhs)
  {
    return new CstVecSliceExpr(this, lhs, rhs);
  }

  CstVarExpr opSlice(P, Q)(P p, Q q)
    if((isIntegral!P || isBitVector!P) && (isIntegral!Q || isBitVector!Q)) {
      return new CstVecSliceExpr(this, CstVal!P.allocate(p),
				 CstVal!Q.allocate(q));
    }

  CstVec2BddExpr lth(Q)(Q q)
    if(isBitVector!Q || isIntegral!Q) {
      auto qq = CstVal!Q.allocate(q);
      return this.lth(qq);
    }

  CstVec2BddExpr lth(CstVarExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.LTH);
  }

  CstVec2BddExpr lte(Q)(Q q)
    if(isBitVector!Q || isIntegral!Q) {
      auto qq = CstVal!Q.allocate(q);
      return this.lte(qq);
    }

  CstVec2BddExpr lte(CstVarExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.LTE);
  }

  CstVec2BddExpr gth(Q)(Q q)
    if(isBitVector!Q || isIntegral!Q) {
      auto qq = CstVal!Q.allocate(q);
      return this.gth(qq);
    }

  CstVec2BddExpr gth(CstVarExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.GTH);
  }

  CstVec2BddExpr gte(Q)(Q q)
    if(isBitVector!Q || isIntegral!Q) {
      auto qq = CstVal!Q.allocate(q);
      return this.gte(qq);
    }

  CstVec2BddExpr gte(CstVarExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.GTE);
  }

  CstVec2BddExpr equ(Q)(Q q)
    if(isBitVector!Q || isIntegral!Q) {
      auto qq = CstVal!Q.allocate(q);
      return this.equ(qq);
    }

  CstVec2BddExpr equ(CstVarExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.EQU);
  }

  CstVec2BddExpr neq(Q)(Q q)
    if(isBitVector!Q || isIntegral!Q) {
      auto qq = CstVal!Q.allocate(q);
      return this.neq(qq);
    }

  CstVec2BddExpr neq(CstVarExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.NEQ);
  }

  CstNotBddExpr opUnary(string op)() {
    static if(op == "*") {	// "!" in cstx is translated as "*"
      return new CstNotBddExpr(this.toBdd());
    }
  }

  CstBdd2BddExpr implies(CstBddExpr other) {
    return new CstBdd2BddExpr(this.toBdd(), other, CstBddOp.LOGICIMP);
  }

  // CstBdd2BddExpr implies(CstVarExpr other)
  // {
  //   return new CstBdd2BddExpr(this.toBdd(), other.toBdd(), CstBddOp.LOGICIMP);
  // }

  CstBdd2BddExpr logicOr(CstBddExpr other) {
    return new CstBdd2BddExpr(this.toBdd(), other, CstBddOp.LOGICOR);
  }

  // CstBdd2BddExpr logicOr(CstVarExpr other)
  // {
  //   return new CstBdd2BddExpr(this.toBdd(), other.toBdd(), CstBddOp.LOGICOR);
  // }

  CstBdd2BddExpr logicAnd(CstBddExpr other) {
    return new CstBdd2BddExpr(this.toBdd(), other, CstBddOp.LOGICAND);
  }

  // CstBdd2BddExpr logicAnd(CstVarExpr other)
  // {
  //   return new CstBdd2BddExpr(this.toBdd(), other.toBdd(), CstBddOp.LOGICAND);
  // }

  bool isOrderingExpr() {
    return false;		// only CstVecOrderingExpr return true
  }

}


mixin template EnumConstraints(T) {
  static if(is(T == enum)) {
    BDD _primBdd;
    override BDD getPrimBdd(Buddy buddy) {
      // return this.bddvec.lte(buddy.buildVec(_maxValue));
      import std.traits;
      if(_primBdd.isZero()) {
	_primBdd = buddy.zero();
	foreach(e; EnumMembers!T) {
	  _primBdd = _primBdd | this.bddvec.equ(buddy.buildVec(e));
	}
      }
      return _primBdd;
    }
    override void resetPrimeBdd() {
      _primBdd.reset();
    }
  }
}

template _esdl__Rand(T, int I)
{
  import std.traits;
  alias L = typeof(T.tupleof[I]);
  alias RAND = getRandAttr!(T, I);
  static if(__traits(isSame, RAND, _esdl__norand)) {
    static if(isArray!L) {
      alias _esdl__Rand = CstVar!(L, _esdl__norand, 0);
    }
    else static if(isBitVector!L || isIntegral!L) {
      alias _esdl__Rand = CstVar!(L, _esdl__norand, 0);
    }
    else static if(is(L == class) || is(L == struct)) {
      alias _esdl__Rand = _esdl__SolverResolve!L;
    }
    else static if(is(L == U*, U) && is(U == struct)) {
      alias _esdl__Rand = _esdl__SolverResolve!U;
    }
  }
  else {
    static if(isArray!L) {
      alias _esdl__Rand = CstVar!(L, RAND, 0);
    }
    else static if(isBitVector!L || isIntegral!L) {
      alias _esdl__Rand = CstVar!(L, RAND, 0);
    }
    else static if(is(L == class) || is(L == struct)) {
      alias _esdl__Rand = _esdl__SolverResolve!L;
    }
    else static if(is(L == U*, U) && is(U == struct)) {
      alias _esdl__Rand = _esdl__SolverResolve!U;
    }
  }
}

template ElementTypeN(T, int N=0)
{
  import std.range;		// ElementType
  static if(N==0) {
    alias ElementTypeN = T;
  }
  else {
    alias ElementTypeN = ElementTypeN!(ElementType!T, N-1);
  }
}

template _esdl__ArrOrder(T, int N=0) {
  import std.traits;
  import std.range;
  static if(isArray!T) {
    enum int _esdl__ArrOrder = 1 + _esdl__ArrOrder!(ElementType!T) - N;
  }
  else {
    enum int _esdl__ArrOrder = 0;
  }
}

template _esdl__ArrOrder(T, int I, int N=0) {
  enum int _esdl__ArrOrder = _esdl__ArrOrder!(typeof(T.tupleof[I])) - N;
}

// This class represents an unrolled Foreach itr at vec level
abstract class CstVarIterBase: CstVarExpr
{
  string _name;

  override string name() {
    return name;
  }

  this(string name) {
    _name = name;
  }

  // _itrVar will point to the array this CstVarIterBase is tied to

  uint maxVal();

  // this will not return the arrVar since the length variable is
  // not getting constrained here
  override CstVarPrim[] preReqs() {
    return [];
  }

  bool isUnrollable();

  // get all the primary bdd vectors that constitute a given bdd expression
  override CstVarPrim[] getRndPrims();

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

}

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
      stage = null;
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

      override RV unroll(CstVarIterBase l, uint n) {
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

      alias P = CstVar!(V, R, N-1);
      P _parent;

      CstVarExpr _indexExpr = null;
      int _index = 0;

      this(string name, P parent, CstVarExpr indexExpr) {
	_name = name;
	_parent = parent;
	_indexExpr = indexExpr;
      }

      this(string name, P parent, uint index) {
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

      override RV unroll(CstVarIterBase itr, uint n) {
	bool found = false;
	foreach(var; itrVars()) {
	  if(itr is var) {
	    found = true;
	    break;
	  }
	}
	if(! found) return this;
	else {
	  if(_indexExpr) {
	    auto vec = _parent.unroll(itr,n)[_indexExpr.unroll(itr,n)];
	    // import std.stdio;
	    // writeln(_indexExpr.name(), " has been unrolled as: ",
	    // 	  _indexExpr.unroll(itr,n).name());
	    return vec;
	    // return _parent.unroll(itr,n)[_indexExpr.unroll(itr,n)];
	  }
	  else {
	    return _parent.unroll(itr,n)[_index];
	  }
	}
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
// template CstVarBase(T, int I, int N=0)
//   if(_esdl__ArrOrder!(T, I, N) != 0) {
//   alias CstVarBase = CstVarBase!(typeof(T.tupleof[I]),
// 					   getRandAttr!(T, I), N);
// }

abstract class CstVarBase(V, alias R, int N=0)
  if(_esdl__ArrOrder!(V, N) != 0): CstVarPrim
{
  enum HAS_RAND_ATTRIB = (! __traits(isSame, R, _esdl__norand));

  alias L = ElementTypeN!(V, N);
  alias E = ElementTypeN!(V, N+1);

  alias EV = CstVar!(V, R, N+1);

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
    assert(false, "isRand not implemented for CstVarBase");
  }

  ulong value() {
    assert(false, "value not implemented for CstVarBase");
  }

  void collate(ulong v, int word = 0) {
    assert(false, "value not implemented for CstVarBase");
  }

  void stage(CstStage s) {
    assert(false, "stage not implemented for CstVarBase");
  }

  uint domIndex() {
    assert(false, "domIndex not implemented for CstVarBase");
  }

  void domIndex(uint s) {
    assert(false, "domIndex not implemented for CstVarBase");
  }

  uint bitcount() {
    assert(false, "bitcount not implemented for CstVarBase");
  }

  bool signed() {
    assert(false, "signed not implemented for CstVarBase");
  }

  ref BddVec bddvec() {
    assert(false, "bddvec not implemented for CstVarBase");
  }

  void bddvec(BddVec b) {
    assert(false, "bddvec not implemented for CstVarBase");
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

template CstVar(T, int I, int N=0)
  if(_esdl__ArrOrder!(T, I, N) != 0) {
  alias CstVar = CstVar!(typeof(T.tupleof[I]),
				   getRandAttr!(T, I), N);
}

// Arrays (Multidimensional arrays as well)
class CstVar(V, alias R, int N=0)
  if(N == 0 && _esdl__ArrOrder!(V, N) != 0):
    CstVarBase!(V, R, N)
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
	    }
	  }

	  static if(isDynamicArray!V) {
	    enum int maxLen = getRandAttrN!(R, N);
	    this(string name, ref V var) {
	      _name = name;
	      _var = &var;
	      _arrLen = new CstVarLen!RV(name ~ ".len", this);
	    }
	  }
	}
	else {
	  this(string name, ref V var) {
	    _name = name;
	    _var = &var;
	    _arrLen = new CstVarLen!RV(name ~ ".len", this);
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
	  return [];
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
    
	RV unroll(CstVarIterBase l, uint n) {
	  return this;
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

	EV opIndex(CstVarExpr idx) {
	  if(idx.isConst()) {
	    assert(_elems.length > 0, "_elems for expr " ~ this.name() ~
		   " have not been built");
	    return _elems[cast(size_t) idx.evaluate()];
	  }
	  else {
	    static if(isStaticArray!E) {
	      // static array
	      return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	    }
	    else static if(isDynamicArray!E) {
	      // dynamic array
	      return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	    }
	    else {
	      return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	    }
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
	  _arrLen.stage = null;
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

class CstVar(V, alias R, int N=0) if(N != 0 && _esdl__ArrOrder!(V, N) != 0):
  CstVarBase!(V, R, N)
    {
      import std.traits;
      import std.range;
      import esdl.data.bvec;
      alias P = CstVar!(V, R, N-1);
      P _parent;
      CstVarExpr _indexExpr = null;
      int _index = 0;

      alias RV = typeof(this);
      CstVarLen!RV _arrLen;

      alias RAND=R;
      
      this(string name, P parent, CstVarExpr indexExpr) {
	_name = name;
	_parent = parent;
	_indexExpr = indexExpr;
	_arrLen = new CstVarLen!RV(name ~ ".len", this);
      }

      this(string name, P parent, uint index) {
	_name = name;
	_parent = parent;
	_index = index;
	_arrLen = new CstVarLen!RV(name ~ ".len", this);
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

      RV unroll(CstVarIterBase itr, uint n) {
	bool found = false;
	foreach(var; itrVars()) {
	  if(itr is var) {
	    found = true;
	    break;
	  }
	}
	if(! found) return this;
	else {
	  // return new RV(name ~ "unrolled_" ~ n.to!string,
	  // 	      _parent.unroll(), _index.unroll());
	  if(_indexExpr) {
	    return _parent.unroll(itr,n)[_indexExpr.unroll(itr,n)];
	  }
	  else {
	    return _parent.unroll(itr,n)[_index];
	  }
	}
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

      EV opIndex(CstVarExpr idx) {
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
	  static if(isStaticArray!E) {
	    // static array
	    return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	  }
	  else static if(isDynamicArray!E) {
	    // dynamic array
	    return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	  }
	  else {
	    return new EV(name ~ "[" ~ idx.name() ~ "]", this, idx);
	  }
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
	_arrLen.stage = null;
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


class CstVarIter(RV): CstVarIterBase, CstVarPrim
{
  RV _arrVar;

  RV arrVar() {
    return _arrVar;
  }

  this(RV arrVar) {
    super("itrVar");
    _arrVar = arrVar;
    _arrVar._arrLen.itrVar(this);
  }

  override CstVarIterBase[] itrVars() {
    return _arrVar.itrVars() ~ this;
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
    if(! _arrVar.arrLen.isRand()) return true;
    if(_arrVar.arrLen.stage !is null &&
       _arrVar.arrLen.stage.solved()) return true;
    else return false;
  }

  // get all the primary bdd vectors that constitute a given bdd expression
  override CstVarPrim[] getRndPrims() {
    return []; // _arrVar.arrLen.getRndPrims();
  }

  CstVarPrim[] getPrimLens() {
    return [_arrVar.arrLen];
  }

  bool isRand() {
    return _arrVar.arrLen.isRand();
  }
  ulong value() {
    return _arrVar.arrLen.value();
  }
  void collate(ulong v, int word = 0) {
    // import std.stdio;
    // writeln("Setting value for arrlen for ", _arrVar.name, " to ", v);
    assert(word == 0);
    _arrVar.arrLen.collate(v);
  }
  void doRandomize(_esdl__RandGen randGen) {
    assert(false);
  }
  CstStage stage() {
    return _arrVar.arrLen.stage();
  }
  void stage(CstStage s) {
    _arrVar.arrLen.stage(s);
  }
  uint domIndex() {
    return _arrVar.arrLen.domIndex;
  }
  void domIndex(uint s) {
    _arrVar.arrLen.domIndex(s);
  }
  uint bitcount() {
    return _arrVar.arrLen.bitcount();
  }
  bool signed() {
    // return _arrVar.arrLen.signed();
    return false;
  }
  ref BddVec bddvec() {
    return _arrVar.arrLen.bddvec();
  }
  void bddvec(BddVec b) {
    _arrVar.bddvec(b);
  }
  override string name() {
    string n = _arrVar.arrLen.name();
    return n[0..$-3] ~ "iter";
  }
  override CstVarExpr unroll(CstVarIterBase l, uint n) {
    // import std.stdio;
    // writeln("unrolling: ", arrVar.name());
    if(this !is l) {
      return _arrVar.unroll(l,n).arrLen().makeItrVar();
    }
    else {
      return CstVal!size_t.allocate(n);
    }
  }

  void _esdl__reset() {
    stage = null;
  }

  bool isVarArr() {
    return false;
  }

  BDD getPrimBdd(Buddy buddy) {
    return buddy.one();
  }

  void resetPrimeBdd() { }

  void solveBefore(CstVarPrim other) {
    assert(false);
  }

  void addPreRequisite(CstVarPrim other) {
    assert(false);
  }

}

class CstVarLen(RV): CstVarExpr, CstVarPrim
{

  enum HAS_RAND_ATTRIB = (! __traits(isSame, RV.RAND, _esdl__norand));

  // This bdd has the constraint on the max length of the array
  BDD _primBdd;
  
  CstVarIter!RV _itrVar;

  RV _parent;

  BddVec _domvec;
  BddVec _valvec;
  
  uint _domIndex = uint.max;
  CstStage _stage = null;

  string _name;

  CstVarPrim[] _preReqs;

  override string name() {
    return _name;
  }

  this(string name, RV parent) {
    _name = name;
    _parent = parent;
  }

  ~this() {
    _domvec.reset();
    _valvec.reset();
    _primBdd.reset();
  }

  override CstVarPrim[] preReqs() {
    return _preReqs ~ _parent.preReqs();
  }

  override CstVarIterBase[] itrVars() {
    return _parent.itrVars();
  }

  override CstVarPrim[] getRndPrims() {
    return _parent.getPrimLens();
  }

  CstVarPrim[] getPrimLens() {
    assert(false);
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
    else {
      return refreshNoRand(buddy);
    }
  }

  override BddVec getBDD(CstStage s, Buddy buddy) {
    static if (HAS_RAND_ATTRIB) {
      assert(stage() !is null, "stage null for: " ~ name());
      if(this.isRand && stage() is s) {
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

  override long evaluate() {
    static if (HAS_RAND_ATTRIB) {
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

  void doRandomize(_esdl__RandGen randGen) {
    assert(false);
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
    _domvec = b;
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
      _primBdd = this.bddvec.lte(buddy.buildVec(_parent.maxArrLen));
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

  override CstVarLen!RV unroll(CstVarIterBase l, uint n) {
    return _parent.unroll(l,n).arrLen();
  }

  void _esdl__reset() {
    stage = null;
  }

  bool isVarArr() {
    return false;
  }

  void solveBefore(CstVarPrim other) {
    other.addPreRequisite(this);
  }

  void addPreRequisite(CstVarPrim prim) {
    _preReqs ~= prim;
  }
}


abstract class CstValAllocator {
  static CstValAllocator[] allocators;

  static void mark() {
    foreach (allocator; allocators) {
      allocator.markIndex();
    }
  }
  
  static void reset() {
    foreach (allocator; allocators) {
      allocator.resetIndex();
    }
  }
  
  abstract void resetIndex();

  abstract void markIndex();
}


class CstVal(T = int): ValVec
{
  static class Allocator: CstValAllocator {
    CstVal!T[] container;
    uint _index = 0;

    uint _mark;

    override void markIndex() {
      _mark = _index;
    }

    override void resetIndex() {
      for (uint i = _mark; i != _index; ++i) {
	container[i]._valvec.reset();
      }
      _index = _mark;
      
    }


    CstVal!T allocate(T val) {
      // return new CstVal!T(val);
      if (_index >= container.length) {
    	container.length += 1;
    	container[$-1] = new CstVal!T(val);
      }
      
      auto cstVal = container[_index];
      cstVal._val = val;
      _index++;
      return cstVal;
    }
  }

  import std.conv;

  static Allocator _allocator;

  // static this() {
  //   CstVal!T._allocator = new Allocator;
  //   CstValAllocator.allocators ~= CstVal!T._allocator;
  // }

  T _val;			// the value of the constant
  BddVec _valvec;

  override string name() {
    return _val.to!string();
  }

  static CstVal!T allocate(T value) {
    Allocator allocator = _allocator;
    if (allocator is null) {
      allocator = new Allocator;
      _allocator = allocator;
      CstValAllocator.allocators ~= allocator;
    }

    // return new CstVal!T(value);
    return allocator.allocate(value);
  }

  this(T value) {
    _val = value;
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

  bool signed() {
    return isVarSigned!T;
  }

}

abstract class ValVec: CstVarExpr, CstVarPrim
{
  override CstVarPrim[] preReqs() {
    return [];
  }

  override CstVarIterBase[] itrVars() {
    return [];
  }

  override bool isConst() {
    return true;
  }

  override CstVarPrim[] getRndPrims() {
    return [];
  }

  CstVarPrim[] getPrimLens() {
    assert(false);
  }

  bool isRand() {
    return false;
  }

  ulong value() {
    assert(false);
  }

  void collate(ulong v, int word = 0) {
    assert(false);
  }

  void doRandomize(_esdl__RandGen randGen) {
    assert(false);
  }
  
  CstStage stage() {
    assert(false, "no stage for CstVal");
  }

  void stage(CstStage s) {
    assert(false, "no stage for CstVal");
  }

  uint domIndex() {
    assert(false, "no domIndex for CstVal");
  }

  void domIndex(uint s) {
    assert(false, "no domIndex for CstVal");
  }

  uint bitcount() {
    assert(false, "no bitcount for CstVal");
  }

  ref BddVec bddvec() {
    assert(false, "no bddvec for CstVal");
  }

  void bddvec(BddVec b) {
    assert(false, "no bddvec for CstVal");
  }

  void _esdl__reset() {
    stage = null;
  }

  bool isVarArr() {
    return false;
  }

  BDD getPrimBdd(Buddy buddy) {
    return buddy.one();
  }

  void resetPrimeBdd() { }

  override CstVarExpr unroll(CstVarIterBase l, uint n) {
    return this;
  }
  
  void solveBefore(CstVarPrim other) {
    assert(false);
  }

  void addPreRequisite(CstVarPrim other) {
    assert(false);
  }

}

// This class would hold two(bin) vector nodes and produces a vector
// only after processing those two nodes
class CstVec2VecExpr: CstVarExpr
{
  import std.conv;

  CstVarExpr _lhs;
  CstVarExpr _rhs;
  CstBinVecOp _op;

  // CstVarPrim[] _preReqs;
  override CstVarPrim[] preReqs() {
    CstVarPrim[] reqs;
    foreach(req; _lhs.preReqs() ~ _rhs.preReqs()) {
      if(! req.solved()) {
	reqs ~= req;
      }
    }
    return reqs;
  }
  CstVarIterBase[] _itrVars;
  override CstVarIterBase[] itrVars() {
    return _itrVars;
  }

  override string name() {
    return "( " ~ _lhs.name ~ " " ~ _op.to!string() ~ " " ~ _rhs.name ~ " )";
  }

  override CstVarPrim[] getRndPrims() {
    return _lhs.getRndPrims() ~ _rhs.getRndPrims();
  }

  override CstVarPrim[] getSolvables() {
    CstVarPrim[] solvables;
    foreach(solvable; _lhs.getSolvables() ~ _rhs.getSolvables()) {
      if(! solvable.solved()) {
	bool add = true;
	foreach(req; this.preReqs()) {
	  if(req is solvable) {
	    add = false;
	  }
	}
	if(add) {
	  solvables ~= solvable;
	}
      }
    }
    return solvables;
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
    _itrVars = lhs.itrVars ~ rhs.itrVars;
    // foreach(var; lhs.itrVars ~ rhs.itrVars) {
    //   bool add = true;
    //   foreach(l; _itrVars) {
    // 	if(l is var) add = false;
    // 	break;
    //   }
    //   if(add) _itrVars ~= var;
    // }
  }
}

class CstVecSliceExpr: CstVarExpr
{
  CstVarExpr _vec;
  CstVarExpr _lhs;
  CstVarExpr _rhs;

  // CstVarPrim[] _preReqs;
  override CstVarPrim[] preReqs() {
    CstVarPrim[] reqs;
    if(_rhs is null) {
      foreach(req; _vec.preReqs() ~ _lhs.preReqs()) {
	if(! req.solved()) {
	  reqs ~= req;
	}
      }
    }
    else {
      foreach(req; _vec.preReqs() ~ _lhs.preReqs() ~ _rhs.preReqs()) {
	if(! req.solved()) {
	  reqs ~= req;
	}
      }
    }
    return reqs;
  }
  
  CstVarIterBase[] _itrVars;
  override CstVarIterBase[] itrVars() {
    return _itrVars;
  }

  override string name() {
    return _vec.name() ~ "[ " ~ _lhs.name() ~ " .. " ~ _rhs.name() ~ " ]";
  }

  override CstVarPrim[] getRndPrims() {
    if(_rhs is null) {
      return _vec.getRndPrims() ~ _lhs.getRndPrims();
    }
    else {
      return _vec.getRndPrims() ~ _lhs.getRndPrims() ~ _rhs.getRndPrims();
    }
  }

   override CstVarPrim[] getSolvables() {
    return _vec.getSolvables();
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
  }
}

class CstNotVecExpr: CstVarExpr
{
  override string name() {
    return "CstNotVecExpr";
  }
}

enum CstBddOp: byte
  {   LOGICAND,
      LOGICOR ,
      LOGICIMP,
      }

abstract class CstBddExpr
{
  string name();

  // In case this expr is unRolled, the _itrVars here would be empty
  CstVarIterBase[] _itrVars;

  abstract bool refresh(CstStage stage, Buddy buddy);
  
  CstVarIterBase[] itrVars() {
    return _itrVars;
  }

  // CstVarPrim[] _preReqs;

  abstract CstVarPrim[] preReqs();

  // unroll recursively untill no unrolling is possible
  CstBddExpr[] unroll() {
    CstBddExpr[] retval;
    auto itr = this.unrollableItr();
    if(itr is null) {
      return [this];
    }
    else {
      foreach(expr; this.unroll(itr)) {
	// import std.stdio;
	// writeln(this.name(), " unrolled expr: ", expr.name());
	// writeln(expr.name());
	if(expr.unrollableItr() is null) retval ~= expr;
	else retval ~= expr.unroll();
      }
    }
    return retval;
  }

  CstBddExpr[] unroll(CstVarIterBase itr) {
    CstBddExpr[] retval;
    if(! itr.isUnrollable()) {
      assert(false, "CstVarIterBase is not unrollabe yet");
    }
    auto max = itr.maxVal();
    // import std.stdio;
    // writeln("maxVal is ", max);
    for (uint i = 0; i != max; ++i) {
      retval ~= this.unroll(itr, i);
    }
    return retval;
  }

  CstVarIterBase unrollableItr() {
    foreach(itr; _itrVars) {
      if(itr.isUnrollable()) return itr;
    }
    return null;
  }

  abstract CstBddExpr unroll(CstVarIterBase l, uint n);

  abstract CstVarPrim[] getRndPrims();

  final CstVarPrim[] getSolvables() {
    CstVarPrim[] solvables;
    foreach(prim; getRndPrims()) {
      if(! prim.solved()) {
	bool add = true;
	foreach(req; this.preReqs()) {
	  if(req is prim) {
	    add = false;
	  }
	}
	if(add) {
	  solvables ~= prim;
	}
      }
    }
    return solvables;
  }

  // abstract CstStage[] getStages();

  abstract BDD getBDD(CstStage stage, Buddy buddy);

  CstBdd2BddExpr opBinary(string op)(CstBddExpr other)
  {
    static if(op == "&") {
      return new CstBdd2BddExpr(this, other, CstBddOp.LOGICAND);
    }
    static if(op == "|") {
      return new CstBdd2BddExpr(this, other, CstBddOp.LOGICOR);
    }
    static if(op == ">>") {
      return new CstBdd2BddExpr(this, other, CstBddOp.LOGICIMP);
    }
  }

  CstNotBddExpr opUnary(string op)()
  {
    static if(op == "*") {	// "!" in cstx is translated as "*"
      return new CstNotBddExpr(this);
    }
  }

  CstBdd2BddExpr implies(CstBddExpr other)
  {
    return new CstBdd2BddExpr(this, other, CstBddOp.LOGICIMP);
  }

  CstBdd2BddExpr implies(CstVarExpr other)
  {
    return new CstBdd2BddExpr(this, other.toBdd(), CstBddOp.LOGICIMP);
  }

  CstBdd2BddExpr logicOr(CstBddExpr other)
  {
    return new CstBdd2BddExpr(this, other, CstBddOp.LOGICOR);
  }

  CstBdd2BddExpr logicOr(CstVarExpr other)
  {
    return new CstBdd2BddExpr(this, other.toBdd(), CstBddOp.LOGICOR);
  }

  CstBdd2BddExpr logicAnd(CstBddExpr other)
  {
    return new CstBdd2BddExpr(this, other, CstBddOp.LOGICAND);
  }

  CstBdd2BddExpr logicAnd(CstVarExpr other)
  {
    return new CstBdd2BddExpr(this, other.toBdd(), CstBddOp.LOGICAND);
  }

}

class CstBdd2BddExpr: CstBddExpr
{
  import std.conv;

  CstBddExpr _lhs;
  CstBddExpr _rhs;
  CstBddOp _op;

  override bool refresh(CstStage stage, Buddy buddy) {
    auto l = _lhs.refresh(stage, buddy);
    auto r = _rhs.refresh(stage, buddy);
    return r || l;
  }
  
  
  override string name() {
    return "( " ~ _lhs.name ~ " " ~ _op.to!string ~ " " ~ _rhs.name ~ " )";
  }

  override CstVarPrim[] preReqs() {
    CstVarPrim[] reqs;
    foreach(req; _lhs.preReqs() ~ _rhs.preReqs()) {
      if(! req.solved()) {
	reqs ~= req;
      }
    }
    return reqs;
  }

  override CstVarPrim[] getRndPrims() {
    return _lhs.getRndPrims() ~ _rhs.getRndPrims();
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

  this(CstBddExpr lhs, CstBddExpr rhs, CstBddOp op) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
    foreach(var; lhs.itrVars ~ rhs.itrVars) {
      bool add = true;
      foreach(l; _itrVars) {
	if(l is var) add = false;
	break;
      }
      if(add) _itrVars ~= var;
    }
  }
}

class CstIteBddExpr: CstBddExpr
{
  override string name() {
    return "CstIteBddExpr";
  }

  override bool refresh(CstStage stage, Buddy buddy) {
    assert(false);
  }
}

class CstVec2BddExpr: CstBddExpr
{
  import std.conv;

  CstVarExpr _lhs;
  CstVarExpr _rhs;
  CstBinBddOp _op;

  override string name() {
    return "( " ~ _lhs.name ~ " " ~ _op.to!string ~ " " ~ _rhs.name ~ " )";
  }

  override bool refresh(CstStage stage, Buddy buddy) {
    auto l = _lhs.refresh(stage, buddy);
    auto r = _rhs.refresh(stage, buddy);
    return r || l;
  }
  
  override CstVarPrim[] preReqs() {
    CstVarPrim[] reqs;
    foreach(req; _lhs.preReqs() ~ _rhs.preReqs()) {
      if(! req.solved()) {
	reqs ~= req;
      }
    }
    return reqs;
  }
    
  override CstVarPrim[] getRndPrims() {
    return _lhs.getRndPrims() ~ _rhs.getRndPrims();
  }

  override BDD getBDD(CstStage stage, Buddy buddy) {
    if(this.itrVars.length !is 0) {
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
    // writeln(_lhs.name() ~ " " ~ _op.to!string ~ " " ~ _rhs.name() ~ " Getting unrolled!");
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

  this(CstVarExpr lhs, CstVarExpr rhs, CstBinBddOp op) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
    foreach(var; lhs.itrVars ~ rhs.itrVars) {
      bool add = true;
      foreach(l; _itrVars) {
	if(l is var) add = false;
	break;
      }
      if(add) _itrVars ~= var;
    }
  }
}

class CstBddConst: CstBddExpr
{
  immutable bool _expr;

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

  override CstVarPrim[] getRndPrims() {
    return [];
  }

  override CstVarPrim[] preReqs() {
    return [];
  }

  override CstBddConst unroll(CstVarIterBase l, uint n) {
    return this;
  }

}

class CstNotBddExpr: CstBddExpr
{
  CstBddExpr _expr;

  override string name() {
    return "( " ~ "!" ~ " " ~ _expr.name ~ " )";
  }

  override bool refresh(CstStage stage, Buddy buddy) {
    return _expr.refresh(stage, buddy);
  }
  
  override CstVarPrim[] preReqs() {
    return _expr.preReqs();
  }

  override CstVarPrim[] getRndPrims() {
    return _expr.getRndPrims();
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

  override CstNotBddExpr unroll(CstVarIterBase l, uint n) {
    bool shouldUnroll = false;
    foreach(var; itrVars()) {
      if(l is var) {
	shouldUnroll = true;
	break;
      }
    }
    if(! shouldUnroll) return this;
    else {
      return new CstNotBddExpr(_expr.unroll(l, n));
    }
  }

  this(CstBddExpr expr) {
    _expr = expr;
    _itrVars = expr.itrVars;
    // _preReqs = expr.preReqs;
  }
}

class CstBlock: CstBddExpr
{
  CstBddExpr[] _exprs;
  bool[] _booleans;

  override bool refresh(CstStage stage, Buddy buddy) {
    bool result = false;
    foreach (expr; _exprs) {
      result |= expr.refresh(stage, buddy);
    }
    return result;
  }
  
  
  override string name() {
    string name_ = "";
    foreach(expr; _exprs) {
      name_ ~= " & " ~ expr.name() ~ "\n";
    }
    return name_;
  }

  override CstVarPrim[] preReqs() {
    assert(false);
  }
    
  void _esdl__reset() {
    _exprs.length = 0;
  }

  bool isEmpty() {
    return _exprs.length == 0;
  }
  
  override CstVarPrim[] getRndPrims() {
    assert(false);
  }

  override CstBlock unroll(CstVarIterBase l, uint n) {
    assert(false, "Can not unroll a CstBlock");
  }

  override BDD getBDD(CstStage stage, Buddy buddy) {
    assert(false, "getBDD not implemented for CstBlock");
  }

  void opOpAssign(string op)(bool other)
    if(op == "~") {
      _booleans ~= other;
    }

  void opOpAssign(string op)(CstBddExpr other)
    if(op == "~") {
      _exprs ~= other;
    }

  void opOpAssign(string op)(CstVarExpr other)
    if(op == "~") {
      _exprs ~= other.toBdd();
    }

  void opOpAssign(string op)(CstBlock other)
    if(op == "~") {
      if(other is null) return;
      foreach(expr; other._exprs) {
	_exprs ~= expr;
      }
      foreach(boolean; other._booleans) {
	_booleans ~= boolean;
      }
    }
}
