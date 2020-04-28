module esdl.rand.expr;

import esdl.rand.intr;
import esdl.solver.obdd;
import esdl.solver.base: CstSolver, CstSolverDomain, CstSolverValue;

import esdl.solver.bdd: CstBddSolver, CstBddSolverValue;

import esdl.rand.misc: rand, _esdl__RandGen, isVecSigned, Unconst;
import esdl.rand.misc: CstBinaryOp, CstCompareOp, CstLogicalOp,
  CstUnaryOp, writeHexString;

import esdl.rand.base;
import esdl.data.bvec: isBitVector, toBitVec;
import esdl.data.charbuf;
import std.traits: isIntegral, isBoolean, isArray, isStaticArray,
  isDynamicArray, isSomeChar;

interface CstVecTerm: CstVecExpr
{

  final CstLogicTerm toBdd() {
    auto zero = new CstVecValue!int(0); // CstVecValue!int.allocate(0);
    return new CstVec2LogicExpr(this, zero, CstCompareOp.NEQ);
  }

  // abstract CstVecExpr unroll(CstIterator iter, uint n);

  CstVec2VecExpr opBinary(string op)(CstVecTerm other)
  {
    static if(op == "&") {
      return new CstVec2VecExpr(this, other, CstBinaryOp.AND);
    }
    static if(op == "|") {
      return new CstVec2VecExpr(this, other, CstBinaryOp.OR);
    }
    static if(op == "^") {
      return new CstVec2VecExpr(this, other, CstBinaryOp.XOR);
    }
    static if(op == "+") {
      return new CstVec2VecExpr(this, other, CstBinaryOp.ADD);
    }
    static if(op == "-") {
      return new CstVec2VecExpr(this, other, CstBinaryOp.SUB);
    }
    static if(op == "*") {
      return new CstVec2VecExpr(this, other, CstBinaryOp.MUL);
    }
    static if(op == "/") {
      return new CstVec2VecExpr(this, other, CstBinaryOp.DIV);
    }
    static if(op == "%") {
      return new CstVec2VecExpr(this, other, CstBinaryOp.REM);
    }
    static if(op == "<<") {
      return new CstVec2VecExpr(this, other, CstBinaryOp.LSH);
    }
    static if(op == ">>") {
      return new CstVec2VecExpr(this, other, CstBinaryOp.RSH);
    }
    static if(op == ">>>") {
      return new CstVec2VecExpr(this, other, CstBinaryOp.LRSH);
    }
    static if(op == "~") {
      return new CstVec2VecExpr(this, other, CstBinaryOp.RANGE);
    }
  }

  CstVec2VecExpr opBinary(string op, Q)(Q q)
    if(isBitVector!Q || isIntegral!Q)
      {
  	auto qq = new CstVecValue!Q(q); // CstVecValue!Q.allocate(q);
  	static if(op == "&") {
  	  return new CstVec2VecExpr(this, qq, CstBinaryOp.AND);
  	}
  	static if(op == "|") {
  	  return new CstVec2VecExpr(this, qq, CstBinaryOp.OR);
  	}
  	static if(op == "^") {
  	  return new CstVec2VecExpr(this, qq, CstBinaryOp.XOR);
  	}
  	static if(op == "+") {
  	  return new CstVec2VecExpr(this, qq, CstBinaryOp.ADD);
  	}
  	static if(op == "-") {
  	  return new CstVec2VecExpr(this, qq, CstBinaryOp.SUB);
  	}
  	static if(op == "*") {
  	  return new CstVec2VecExpr(this, qq, CstBinaryOp.MUL);
  	}
  	static if(op == "/") {
  	  return new CstVec2VecExpr(this, qq, CstBinaryOp.DIV);
  	}
  	static if(op == "%") {
  	  return new CstVec2VecExpr(this, qq, CstBinaryOp.REM);
  	}
  	static if(op == "<<") {
  	  return new CstVec2VecExpr(this, qq, CstBinaryOp.LSH);
  	}
  	static if(op == ">>") {
  	  return new CstVec2VecExpr(this, qq, CstBinaryOp.RSH);
  	}
  	static if(op == ">>>") {
  	  return new CstVec2VecExpr(this, qq, CstBinaryOp.LRSH);
  	}
  	static if(op == "~") {
  	  return new CstVec2VecExpr(this, qq, CstBinaryOp.RANGE);
  	}
      }

  CstVec2VecExpr opBinaryRight(string op, Q)(Q q)
    if(isBitVector!Q || isIntegral!Q)
      {
	auto qq = new CstVecValue!Q(q); // CstVecValue!Q.allocate(q);
	static if(op == "&") {
	  return new CstVec2VecExpr(qq, this, CstBinaryOp.AND);
	}
	static if(op == "|") {
	  return new CstVec2VecExpr(qq, this, CstBinaryOp.OR);
	}
	static if(op == "^") {
	  return new CstVec2VecExpr(qq, this, CstBinaryOp.XOR);
	}
	static if(op == "+") {
	  return new CstVec2VecExpr(qq, this, CstBinaryOp.ADD);
	}
	static if(op == "-") {
	  return new CstVec2VecExpr(qq, this, CstBinaryOp.SUB);
	}
	static if(op == "*") {
	  return new CstVec2VecExpr(qq, this, CstBinaryOp.MUL);
	}
	static if(op == "/") {
	  return new CstVec2VecExpr(qq, this, CstBinaryOp.DIV);
	}
	static if(op == "%") {
	  return new CstVec2VecExpr(qq, this, CstBinaryOp.REM);
	}
	static if(op == "<<") {
	  return new CstVec2VecExpr(qq, this, CstBinaryOp.LSH);
	}
	static if(op == ">>") {
	  return new CstVec2VecExpr(qq, this, CstBinaryOp.RSH);
	}
	static if(op == ">>>") {
	  return new CstVec2VecExpr(qq, this, CstBinaryOp.LRSH);
	}
	static if(op == "~") {
	  return new CstVec2VecExpr(qq, this, CstBinaryOp.RANGE);
	}
      }

  final CstVecSliceExpr opIndex(CstVecTerm index) {
    // assert(false, "Index operation defined only for Arrays");
    CstVec2VecExpr range = cast(CstVec2VecExpr) index;
    if (range is null || range._op !is CstBinaryOp.RANGE) {
      range = index ~ (index + new CstVecValue!(int)(1));
    }
    return new CstVecSliceExpr(this, range);
  }

  // CstVecTerm opSlice(P)(P p)
  //   if(isIntegral!P || isBitVector!P) {
  //     return new CstVecSliceExpr(this, new CstVecValue!P(p)); // CstVecValue!P.allocate(p));
  //   }

  // final CstVecTerm opSlice(CstVecExpr lhs, CstVecExpr rhs) {
  //   return new CstVecSliceExpr(this, lhs, rhs);
  // }

  // CstVecTerm opSlice(P, Q)(P p, Q q)
  //   if((isIntegral!P || isBitVector!P) && (isIntegral!Q || isBitVector!Q)) {
  //     return new CstVecSliceExpr(this, new CstVecValue!P(p), // CstVecValue!P.allocate(p),
  // 				 new CstVecValue!Q(q)); // CstVecValue!Q.allocate(q));
  //   }

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

class CstVecDomain(T, rand RAND_ATTR): CstDomain, CstVecTerm
{
  enum HAS_RAND_ATTRIB = RAND_ATTR.isRand();

  BddVec _valvec;
  _esdl__Proxy _root;

  Unconst!T _shadowValue;

  string _name;

  override string name() {
    return _name;
  }

  static if (HAS_RAND_ATTRIB) {
    uint         _domIndex = uint.max;
    uint         _varN = uint.max;
    CstStage     _stage = null;
    uint         _resolveLap = 0;
  }

  void annotate(ref uint varN) {
    static if (HAS_RAND_ATTRIB) {
      if (! this.isSolved()) {
	if (_varN == uint.max) {
	  _varN = varN;
	  varN += 1;
	}
      }
    }
  }

  void writeExprString(ref Charbuf str) {
    if (this.isRand()) {
      str ~= 'R';
      static if (HAS_RAND_ATTRIB) {
	if (_varN < 256) (cast(ubyte) _varN).writeHexString(str);
	else (cast(ushort) _varN).writeHexString(str);
      }
      static if (isBitVector!T) {
	static if (T.ISSIGNED) {
	  str ~= 'S';
	}
	else {
	  str ~= 'U';
	}
	if (T.SIZE < 256) (cast(ubyte) T.SIZE).writeHexString(str);
	else (cast(ushort) T.SIZE).writeHexString(str);
      }
      static if (isIntegral!T) {
	static if (isSigned!T) {
	  str ~= 'S';
	}
	else {
	  str ~= 'U';
	}
	(cast(ubyte) (T.sizeof * 8)).writeHexString(str);
      }
      static if (isBoolean!T) {
	str ~= 'U';
	(cast(ubyte) 1).writeHexString(str);
      }
    }
    else {
      str ~= 'V';
      static if (isBoolean!T) {
	T value = cast(T) this.evaluate;
	str ~= 'U';
	value.writeHexString(str);
      }
      static if (isIntegral!T) {
	T value = cast(T) this.evaluate;
	static if (isSigned!T) {
	  str ~= 'S';
	}
	else {
	  str ~= 'U';
	}
	value.writeHexString(str);
      }
      else {
	T value = cast(T) this.evaluate.toBitVec();
	static if (T.ISSIGNED) {
	  str ~= 'S';
	}
	else {
	  str ~= 'U';
	}
	value.writeHexString(str);
      }
    }
  }

  this(string name, _esdl__Proxy root) {
    _name = name;
    _root = root;
    fixRangeSet();
  }

  ~this() {
    // static if (HAS_RAND_ATTRIB && is(T == enum)) {
    //   _primBdd.reset();
    // }
    _valvec.free();
  }    

  long evaluate() {
    static if (HAS_RAND_ATTRIB) {
      if (! this.isRand || this.isSolved()) {
	return value();
      }
      else {
	assert (false, "Error evaluating " ~ _name);
      }
    }
    else {
      return value();
    }
  }

  S to(S)()
    if (is (S == string)) {
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

  // implementation of CstVecDomain API
  // static if (HAS_RAND_ATTRIB && is(T == enum)) {
  //   // BDD _primBdd;
  //   // override BDD getPrimBdd(Buddy buddy) {
  //   //   import std.traits;
  //   //   if (_primBdd.isZero()) {
  //   // 	foreach(e; EnumMembers!T) {
  //   // 	  _primBdd = _primBdd | this.bddvec(buddy).equ(buddy.buildVec(e));
  //   // 	}
  //   //   }
  //   //   return _primBdd;
  //   // }
  // }
  // else {
  //   override BDD getPrimBdd(Buddy buddy) {
  //     return buddy.one();
  //   }
  // }

  override uint bitcount() {
    static if (isBoolean!T)         return 1;
    else static if (isIntegral!T || isSomeChar!T)   return T.sizeof * 8;
    else static if (isBitVector!T)  return T.SIZE;
    else static assert(false, "bitcount can not operate on: " ~ T.stringof);
  }

  override bool signed() {
    static if (isVecSigned!T) {
      return true;
    }
    else  {
      return false;
    }
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
    static if (isIntegral!T) {
      import std.traits;
      enum bool signed = isSigned!T;
      enum uint bits = T.sizeof * 8;
    }
    else static if (isBitVector!T) {
      enum bool signed = T.ISSIGNED;
      enum uint bits = T.SIZE;
    }
    else {			// bool
      enum signed = false;
      enum bits = 1;
    }
    static if (bits <= 64) {
      // final
      switch (iType) {
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
      default: assert(false);
      }
      return true;
    }
    else {
      return false;
    }
  }


  
  void fixRangeSet() {
    static if (isIntegral!T) {
      static if (RangeT.sizeof == T.sizeof) {
	IntRange!RangeT fixR = IntRange!RangeT(true);
      }
      else {
	IntRange!RangeT fixR =
	  IntRange!RangeT(cast(int) T.min, cast(int) T.max + 1);
      }
      _rs &= fixR;
    }
    static if (isBitVector!T && T.SIZE <= 32) {
      static if (int.sizeof*8 == T.SIZE) {
	IntRange!RangeT fixR = IntRange!RangeT(true);
      }
      else {
	IntRange!RangeT fixR =
	  IntRange!RangeT(cast(int) T.min, cast(int) T.max + 1);
      }
      _rs &= fixR;
    }
  }

  override ref BddVec bddvec(Buddy buddy) {
    static if (HAS_RAND_ATTRIB) {
      assert(_domIndex != uint.max,
	     "BDD Domain not yet created for: " ~ name());
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

  override uint domIndex() {
    static if (HAS_RAND_ATTRIB) {
      return _domIndex;
    }
    else {
      assert(false);
    }
  }

  override void domIndex(uint s) {
    static if (HAS_RAND_ATTRIB) {
      _domIndex = s;
    }
    else {
      assert(false);
    }
  }

  override void reset() {
    static if (HAS_RAND_ATTRIB) {
      _state = DomainState.INIT;
      _stage = null;
      _resolveLap = 0;
    }
  }
  
  override CstStage stage() {
    static if (HAS_RAND_ATTRIB) {
      return _stage;
    }
    else {
      assert(false);
    }
  }

  override void stage(CstStage s) {
    static if (HAS_RAND_ATTRIB) {
      _stage = s;
    }
    else {
      assert(false);
    }
  }

  uint resolveLap() {
    static if (HAS_RAND_ATTRIB) {
      if (_stage !is null && _stage.isSolved()) {
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

  void resolveLap(uint lap) {
    static if (HAS_RAND_ATTRIB) {
      if (_stage !is null && _stage.isSolved()) {
	_resolveLap = 0;
      }
      else {
	static if (HAS_RAND_ATTRIB) {
	  _resolveLap = lap;
	}
      }
    }
  }

  abstract T* getRef();
  
  // override void collate(ulong v, int word = 0) {
  //   static if (HAS_RAND_ATTRIB) {
  //     T* var = getRef();
  //     static if(isIntegral!T) {
  // 	if(word == 0) {
  // 	  *var = cast(T) v;
  // 	}
  // 	else {
  // 	  assert(false, "word has to be 0 for integrals");
  // 	}
  //     }
  //     else {
  // 	(*var)._setNthWord(v, word);
  //     }
  //     markSolved();
  //   }
  //   else {
  //     assert(false);
  //   }
  // }

  override void setVal(ulong[] value) {
    static if (HAS_RAND_ATTRIB) {
      Unconst!T newVal;
      foreach (i, v; value) {
	static if(isIntegral!T || isBoolean!T) {
	  if (i == 0) {
	    newVal = cast(T) v;
	  }
	  else {
	    assert(false, "word has to be 0 for integrals");
	  }
	}
	else {
	  newVal._setNthWord(v, i);
	}
      }
      if (newVal != *(getRef())) {
	_valueChanged = true;
      }
      else {
	_valueChanged = false;
      }
      *(getRef()) = newVal;
      markSolved();
      execCbs();
    }
    else {
      assert(false);
    }
  }

  override void setVal(ulong val) {
    static if (isBitVector!T) {
      assert (T.SIZE <= 64);
    }
    static if (HAS_RAND_ATTRIB) {
      Unconst!T newVal;
      static if(isIntegral!T || isBoolean!T) {
	newVal = cast(T) val;
      }
      else {
	newVal._setNthWord(val, 0);
      }

      if (newVal != *(getRef())) {
	_valueChanged = true;
      }
      else {
	_valueChanged = false;
      }
      *(getRef()) = newVal;
      markSolved();
      execCbs();
    }
    else {
      assert(false);
    }
  }

  override void _esdl__doRandomize(_esdl__RandGen randGen) {
    static if (HAS_RAND_ATTRIB) {
      if (! isSolved()) {
	Unconst!T newVal;
	randGen.gen(newVal);
	if (newVal != *(getRef())) {
	  _valueChanged = true;
	  *(getRef()) = newVal;
	}
	else {
	  _valueChanged = false;
	}
	markSolved();
	execCbs();
      }
    }
    else {
      assert(false);
    }
  }

  override void _esdl__doRandomize(_esdl__RandGen randGen, CstStage s) {
    static if (HAS_RAND_ATTRIB) {
      assert(stage is s);
      _esdl__doRandomize(randGen);
    }
    else {
      assert(false);
    }
  }

  bool _valueChanged = true;

  override bool hasChanged() {
    return _valueChanged;
  }
  
  override bool updateVal() {
    assert(isRand() !is true);
    Unconst!T newVal;
    assert (_root !is null);
    if (! this.isSolved()) {
      newVal = *(getRef());
      this.markSolved();
      if (_valvec.isNull ||
	  newVal != _shadowValue) {
	_shadowValue = newVal;
	_valueChanged = true;
	_valvec.buildVec(CstBddSolver.buddy(), _shadowValue);
	return true;
      }
      else {
	_valueChanged = false;
	return false;
      }
    }
    return true;
  }

  static if (isIntegral!T) {
    import std.traits;
    static if (isSigned!T) {
      enum bool tSigned = true;
    }
    else {
      enum bool tSigned = false;
    }
    enum size_t tSize = T.sizeof * 8;
    enum bool IS_RANGE = true;
  }
  else static if (isBoolean!T) {
    enum bool tSigned = false;
    enum size_t tSize = 1;
    enum bool IS_RANGE = true;
  }
  else static if (isSomeChar!T) {
    enum bool tSigned = false;
    enum size_t tSize = T.sizeof * 8;
    enum bool IS_RANGE = true;
  }
  else static if (isBitVector!T) {
    static if (T.ISSIGNED) {
      enum bool tSigned = true;
    }
    else {
      enum bool tSigned = false;
    }
    static if (T.SIZE <= 64) {
      enum size_t tSize = T.SIZE;
      enum bool IS_RANGE = true;
    }
    else {
      enum bool IS_RANGE = false;
    }
  }
  else {			// boolean
    enum bool IS_RANGE = false;
  }

  static if (IS_RANGE) {
    static if (tSigned) {
      static if (tSize <= 32) {
	alias RangeT = int;
      }
      else static if (tSize <= 64) {
	alias RangeT = long;
      }
    }
    else {
      static if (tSize <= 32) {
	alias RangeT = uint;
      }
      else {
	alias RangeT = ulong;
      }
    }
    IntRangeSet!RangeT _rs;
    // pragma(msg, typeof(_rs).stringof);
  }

  override bool solveRange(_esdl__RandGen randGen) {
    // final
    switch(this._type) {
    case DomType.TRUEMONO:
      if (this._rs.isEmpty()) {
	assert(false, "Constraints on domain " ~ this.name() ~
	       " do not converge");
      }
      this.setVal(this._rs.uniform(randGen));
      break;
    case DomType.LAZYMONO:
      auto rns = this._rs.dup();
      foreach (rp; this._rndPreds) {
	if (rp._vars.length > 0) {
	  IntRangeSet!RangeT tmprns;
	  if (! rp.getExpr().getUniRangeSet(tmprns)) {
	    return false;
	  }
	  rns &= tmprns;
	}
      }
      if (rns.isEmpty()) {
	assert(false, "Constraints on domain " ~ this.name() ~
	       " do not converge");
      }
      this.setVal(rns.uniform(randGen));
      break;
    case DomType.MAYBEMONO:
      auto rns = this._rs.dup();
      foreach (rp; this._rndPreds) {
	if (rp._vars.length > 0) {
	  IntRangeSet!RangeT tmprns;
	  if (! rp.getExpr().getUniRangeSet(tmprns)) {
	    return false;
	  }
	  rns &= tmprns;
	}
      }
      foreach (tp; this._tempPreds) {
	IntRangeSet!RangeT tmprns;
	if (! tp.getExpr().getUniRangeSet(tmprns)) {
	  return false;
	}
	rns &= tmprns;
      }
      if (rns.isEmpty()) {
	assert(false, "Constraints on domain " ~ this.name() ~
	       " do not converge");
      }
      this.setVal(rns.uniform(randGen));
      break;
    case DomType.INDEXEDMONO:
      assert(false);
    case DomType.MULTI:
      return false;
    default:
      assert(false);
    }
    return true;
  }

  override void registerRndPred(CstPredicate rndPred) {
    static if(IS_RANGE) {
      foreach (pred; _rndPreds) {
	if (pred is rndPred) {
	  return;
	}
      }
      if (_type !is DomType.MULTI) {

	IntRangeSet!RangeT rs;
	if (rndPred._vars.length == 0) {
	  if (rndPred.getExpr().getUniRangeSet(rs)) {
	    _rs &= rs;
	  }
	  else {
	    _type = DomType.MULTI;
	  }
	  // import std.stdio;
	  // writeln(this.name());
	  // writeln(_rs);
	}

      }
      _rndPreds ~= rndPred;
    }
  }
  
  override void registerVarPred(CstPredicate varPred) {
    foreach (pred; _varPreds) {
      if (pred is varPred) {
	return;
      }
    }
    _varPreds ~= varPred;
  }
  
  override void registerDepPred(CstDepCallback depCb) {
    foreach (cb; _depCbs) {
      if (cb is depCb) {
	return;
      }
    }
    _depCbs ~= depCb;
  }

  override void registerIdxPred(CstDepCallback idxCb) {
    foreach (cb; _depCbs) {
      if (cb is idxCb) {
	return;
      }
    }
    _depCbs ~= idxCb; // use same callbacks as deps for now
  }


  final override void markSolved() {
    super.markSolved();
    static if (HAS_RAND_ATTRIB) {
      if (this.isRand()) {
	_varN = uint.max;
      }
    }
  }
  
  final override string describe() {
    import std.conv: to;
    string desc = "CstDomain: " ~ name();
    desc ~= "\n	DomType: " ~ _type.to!string();
    if (_type !is DomType.MULTI) {
      desc ~= "\nIntRS: " ~ _rs.toString();
    }
    if (_rndPreds.length > 0) {
      desc ~= "\n	Preds:";
      foreach (pred; _rndPreds) {
	desc ~= "\n		" ~ pred.name();
      }
      desc ~= "\n";
    }
    if (_tempPreds.length > 0) {
      desc ~= "\n	Temporary Preds:";
      foreach (pred; _tempPreds) {
	desc ~= "\n		" ~ pred.name();
      }
      desc ~= "\n";
    }
    return desc;
  }
}

interface CstLogicTerm: CstLogicExpr
{
  abstract override CstLogicTerm unroll(CstIterator iter, uint n);

  CstLogicTerm opBinary(string op)(CstLogicTerm other)
  {
    static if(op == "&") {
      return new CstLogic2LogicExpr(this, other, CstLogicalOp.LOGICAND);
    }
    static if(op == "|") {
      return new CstLogic2LogicExpr(this, other, CstLogicalOp.LOGICOR);
    }
    static if(op == ">>") {
      return new CstLogic2LogicExpr(this, other, CstLogicalOp.LOGICIMP);
    }
  }

  CstLogicTerm opUnary(string op)()
  {
    static if(op == "*") {	// "!" in cstx is translated as "*"
      return new CstNotBddExpr(this);
    }
  }

  final CstLogicTerm implies(CstLogicTerm other)
  {
    return new CstLogic2LogicExpr(this, other, CstLogicalOp.LOGICIMP);
  }

  final CstLogicTerm implies(CstVecTerm other)
  {
    return new CstLogic2LogicExpr(this, other.toBdd(), CstLogicalOp.LOGICIMP);
  }

  final CstLogicTerm logicOr(CstLogicTerm other)
  {
    return new CstLogic2LogicExpr(this, other, CstLogicalOp.LOGICOR);
  }

  final CstLogicTerm logicOr(CstVecTerm other)
  {
    return new CstLogic2LogicExpr(this, other.toBdd(), CstLogicalOp.LOGICOR);
  }

  final CstLogicTerm logicAnd(CstLogicTerm other)
  {
    return new CstLogic2LogicExpr(this, other, CstLogicalOp.LOGICAND);
  }

  final CstLogicTerm logicAnd(CstVecTerm other)
  {
    return new CstLogic2LogicExpr(this, other.toBdd(), CstLogicalOp.LOGICAND);
  }

}

class CstVecIterator(RV): CstIterator, CstVecTerm
{
  RV _arrVar;

  RV arrVar() {
    return _arrVar;
  }

  string _name;

  this(RV arrVar) {
    _name = "iterVar";
    _arrVar = arrVar;
    // _arrVar._arrLen.iterVar(this);
  }

  void annotate(ref uint varN) {
    assert(false);
  }

  final override CstDomain getLenVec() {
    return _arrVar.arrLen();
  }
  
  override uint size() {
    if(! getLenVec().isSolved()) {
      assert(false, "Can not find size since the " ~
	     "Iter Variable is unrollable");
    }
    // import std.stdio;
    // writeln("size for arrVar: ", _arrVar.name(), " is ",
    // 	    _arrVar.arrLen.value);
    return cast(uint) _arrVar.arrLen.value;
  }

  override string name() {
    string n = _arrVar.arrLen.name();
    return n[0..$-3] ~ "iter";
  }

  override string describe() {
    return name();
  }

  // while an iterator is a singleton wrt to an array, the array
  // itself could have multiple object instances in case it is not
  // concrete -- eg foo[foo.iter].iter
  override bool opEquals(Object other) {
    auto rhs = cast(typeof(this)) other;
    if (rhs is null) return false;
    else return (_arrVar == rhs._arrVar);
  }

  override CstVecExpr unroll(CstIterator iter, uint n) {
    if(this !is iter) {
      return _arrVar.unroll(iter,n).arrLen().makeIterVar();
    }
    else {
      return new CstVecValue!size_t(n); // CstVecValue!size_t.allocate(n);
    }
  }

  override CstIterator unrollIterator(CstIterator iter, uint n) {
    assert(this !is iter);
    return _arrVar.unroll(iter,n).arrLen().makeIterVar();
  }

  override uint resolveLap() {
    assert (false, "resolveLap should never be called on CstVecIterator");
  }

  override void resolveLap(uint lap) {}

  override bool isConst() {
    return false;
  }

  override bool isIterator() {
    return true;
  }

  override bool isOrderingExpr() {
    return false;		// only CstVecOrderingExpr return true
  }

  // get the list of stages this expression should be avaluated in
  // override CstStage[] getStages() {
  //   return arrVar.arrLen.getStages();
  // }

  override void visit(CstSolver solver) {
    assert (false, "Can not visit an Iter Variable without unrolling");
  }

  override BddVec getBDD(CstStage stage, Buddy buddy) {
    assert (false, "Can not getBDD for a Iter Variable without unrolling");
  }

  // override bool getVal(ref long val) {
  //   return false;
  // }

  override long evaluate() {
    assert(false, "Can not evaluate an Iterator: " ~ this.name());
  }

  override void setDomainContext(CstPredicate pred,
				 ref CstDomain[] rnds,
				 ref CstDomain[] vars,
				 ref CstValue[] vals,
				 ref CstIterator[] iters,
				 ref CstDomain[] idxs,
				 ref CstDomain[] deps) {
    deps ~= getLenVec();
    iters ~= this;
  }

  override bool getIntRange(ref IntR rng) {
    return false;
  }

  override bool getUniRange(ref UniRange rng) {
    return false;
  }

  override bool getIntType(ref INTTYPE iType) {
    return false;
  }

  override bool isSolved() {
    return _arrVar._arrLen.isSolved();
  }


  void writeExprString(ref Charbuf str) {
    // assert(false);
  }
}

class CstVecLen(RV): CstVecDomain!(uint, RV.RAND), CstVecPrim
{

  enum HAS_RAND_ATTRIB = RV.RAND.isRand();

  // This bdd has the constraint on the max length of the array
  // BDD _primBdd;
  
  CstVecIterator!RV _iterVar;

  RV _parent;

  string _name;

  CstVecPrim[] _preReqs;

  override string name() {
    return _name;
  }

  this(string name, RV parent) {
    assert (parent !is null);
    super(name, parent.getProxyRoot());
    _name = name;
    _parent = parent;
    _iterVar = new CstVecIterator!RV(_parent);
    // if (_parent.isStatic()) {
    //   // _parent.getProxyRoot().addDomain(this, HAS_RAND_ATTRIB);
    // }
  }

  ~this() {
    // _domvec.reset();
    _valvec.free();
    // _primBdd.reset();
  }

  override _esdl__Proxy getProxyRoot() {
    return _parent.getProxyRoot();
  }

  override CstVecLen!RV getResolved() { // always self
    return this;
  }

  void visit(CstSolver solver) {
    assert (stage() !is null, "stage null for: " ~ name());
    solver.pushDomain(this);
  }

  BddVec getBDD(CstStage s, Buddy buddy) {
    static if (HAS_RAND_ATTRIB) {
      // auto fparent = _parent.flatten();
      // if (fparent !is _parent) {
      // 	return _parent.flatten().arrLen.getBDD(s, buddy);
      // }
      // else {
      assert (stage() !is null, "stage null for: " ~ name());
      if (this.isRand && stage() is s) {
	return bddvec(buddy);
	// return _domvec;
      }
      else if ((! this.isRand) ||
	       this.isRand && stage().isSolved()) { // work with the value
	return _valvec;
      }
      else {
	assert (false, "Constraint evaluation in wrong stage");
      }
      // }
    }
    else {
      return _valvec;
    }
  }

  // bool getVal(ref long val) {
  //   static if (HAS_RAND_ATTRIB) {
  //     assert(stage() !is null);
  //     if(! this.isRand || stage().isSolved()) {
  // 	val = value();
  // 	return true;
  //     }
  //     else {
  // 	return false;
  //     }
  //   }
  //   else {
  //     val = value();
  //     return true;
  //   }
  // }

  // bool isResolved() {
  //   return stage().isSolved();
  // }
  
  override void _esdl__doRandomize(_esdl__RandGen randGen) {
    // this function will only be called when array lenght is
    // unconstrainted
    markSolved();
    _parent.buildElements(_parent.getLen());
    execCbs();
  }
  
  override void _esdl__doRandomize(_esdl__RandGen randGen, CstStage s) {
    assert (s is stage);
    _esdl__doRandomize(randGen);
  }
  
  override bool isRand() {
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

  // final override BDD getPrimBdd(Buddy buddy) {
  //   if (_primBdd.isZero()) {
  //     _primBdd = this.bddvec(buddy).lte(buddy.buildVec(_parent.maxArrLen));
  //   }
  //   return _primBdd;
  // }
  
  void iterVar(CstVecIterator!RV var) {
    _iterVar = var;
  }

  CstVecIterator!RV iterVar() {
    return _iterVar;
  }

  CstVecIterator!RV makeIterVar() {
    if(_iterVar is null) {
      _iterVar = new CstVecIterator!RV(_parent);
    }
    return _iterVar;
  }

  override uint bitcount() {
    if (_parent.maxArrLen == -1) {
      return 32;
    }
    uint i = 1;
    for (size_t c=1; c < _parent.maxArrLen; c *= 2) {
      i++;
    }
    return i;
  }

  override bool signed() {
    return false;
  }

  override long value() {
    return _parent.getLen();
  }

  override void setVal(ulong value) {
    markSolved();
    _parent.setLen(cast(size_t) value);
    execCbs();
  }

  override void setVal(ulong[] v) {
    assert(v.length == 1);
    markSolved();
    _parent.setLen(cast(size_t) v[0]);
    execCbs();
  }

  // override void collate(ulong v, int word = 0) {
  //   assert(word == 0);
  //   _parent.setLen(cast(size_t) v);
  // }

  CstVecExpr unroll(CstIterator iter, uint n) {
    return _parent.unroll(iter,n).arrLen();
  }

  void solveBefore(CstVecPrim other) {
    other.addPreRequisite(this);
  }

  void addPreRequisite(CstVecPrim domain) {
    _preReqs ~= domain;
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

  void setDomainContext(CstPredicate pred,
			ref CstDomain[] rnds,
			ref CstDomain[] vars,
			ref CstValue[] vals,
			ref CstIterator[] iters,
			ref CstDomain[] idxs,
			ref CstDomain[] deps) {
    bool listed;
    foreach (rnd; rnds) {
      if (rnd is this) {
	listed = true;
	break;
      }
    }
    if (listed is false) {
      rnds ~= this;
    }
    static if (HAS_RAND_ATTRIB) {
      if (! this.isStatic()) {
	if (_type <= DomType.LAZYMONO) _type = DomType.MAYBEMONO;
      }
    }
    _parent.setDomainContext(pred, rnds, vars, vals, iters, idxs, deps);
  }

  override bool getIntType(ref INTTYPE iType) {
    // INTTYPE defaults to UINT
    return true;
  }

  override void execIterCbs() {
    assert(_iterVar !is null);
    _iterVar.unrollCbs();
  }

  override uint* getRef() {
    assert(false);
  }

  override bool updateVal() {
    assert(isRand() !is true);
    uint newVal;
    assert(_root !is null);
    if (! this.isSolved()) {
      newVal = cast(uint)_parent.getLen();
      this.markSolved();
      if (_valvec.isNull ||
	  newVal != _shadowValue) {
	_shadowValue = cast(uint) newVal;
	_valueChanged = true;
	_valvec.buildVec(CstBddSolver.buddy, _shadowValue);
	return true;
      }
      else {
	_valueChanged = false;
	return false;
      }
    }
    return true;
  }
  
  final override bool isStatic() {
    return _parent.isStatic();
  }

}

abstract class CstValue: CstVecTerm
{
  CstLogicExpr _cstExpr;
  
  bool isConst() {
    return true;
  }

  bool isIterator() {
    return false;
  }

  override CstVecExpr unroll(CstIterator l, uint n) {
    return this;
  }

  CstSolverValue _solverValue;

  abstract long value();
  abstract bool signed();
  abstract uint bitcount();
}

auto _esdl__cstVal(T)(T val) {
  return new CstVecValue!(T)(val); // CstVecValue!(T).allocate(val);
}

class CstVecValue(T = int): CstValue
{
  static if (isIntegral!T) {
    import std.traits;
    enum bool SIGNED = isSigned!T;
    enum uint BITCOUNT = T.sizeof * 8;
  }
  else static if (isBitVector!T) {
    enum bool SIGNED = T.ISSIGNED;
    enum uint BITCOUNT = T.SIZE;
  }

  override bool signed() {
    return SIGNED;
  }

  override uint bitcount() {
    return BITCOUNT;
  }

  override long value() {
    return _val;
  }

  // static class Allocator: CstValueAllocator {
  //   CstVecValue!T[] container;
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


  //   CstVecValue!T allocate(T val) {
  //     // return new CstVecValue!T(val);
  //     if (_index >= container.length) {
  //   	container.length += 1;
  //   	container[$-1] = new CstVecValue!T(val);
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
  //   CstVecValue!T._allocator = new Allocator;
  //   CstValueAllocator.allocators ~= CstVecValue!T._allocator;
  // }

  T _val;			// the value of the constant
  BddVec _valvec;

  string describe() {
    return _val.to!string();
  }

  // static CstVecValue!T allocate(T value) {
  //   Allocator allocator = _allocator;
  //   if (allocator is null) {
  //     allocator = new Allocator;
  //     _allocator = allocator;
  //     CstValueAllocator.allocators ~= allocator;
  //   }

  //   // return new CstVecValue!T(value);
  //   return allocator.allocate(value);
  // }

  this(T value) {
    _val = value;
  }

  ~this() {
    _valvec.free();
  }

  void annotate(ref uint varN) {
    // do nothing for a val
  }

  void visit(CstSolver solver) {
    solver.pushValue(this);
  }

  BddVec getBDD(CstStage stage, Buddy buddy) {
    return _valvec;
  }

  const(T)* getRef() {
    return &_val;
  }

  // bool getVal(ref long val) {
  //   val = _val;
  //   return true;
  // }

  long evaluate() {
    return _val;
  }

  uint resolveLap() {
    return 0;			// const
  }

  void resolveLap(uint lap) {}

  bool isOrderingExpr() {
    return false;		// only CstVecOrderingExpr return true
  }

  bool isSolved() {
    return true;
  }

  void setDomainContext(CstPredicate pred,
			ref CstDomain[] rnds,
			ref CstDomain[] vars,
			ref CstValue[] vals,
			ref CstIterator[] iters,
			ref CstDomain[] idxs,
			ref CstDomain[] deps) {
    vals ~= this;
    if (_valvec.isNull()) {
      _valvec.buildVec(pred.getProxy().getBuddy(), _val);
    }
  }

  bool getIntRange(ref IntR rng) {
    assert(false, "getIntRange should never be called for a CstVecValue");
  }

  bool getUniRange(ref UniRange rng) {
    assert(false, "UniRange should never be called for a CstVecValue");
  }

  bool getIntType(ref INTTYPE iType) {
    static if (BITCOUNT <= 64) {
      // final
      switch (iType) {
      case INTTYPE.UINT: iType = BITCOUNT <= 32 ?
	  (SIGNED ? INTTYPE.INT : INTTYPE.UINT) :
	(SIGNED ? INTTYPE.LONG : INTTYPE.ULONG);
	break;
      case INTTYPE.INT: iType = BITCOUNT <= 32 ?
	  INTTYPE.INT : INTTYPE.LONG;
	break;
      case INTTYPE.ULONG: iType = SIGNED ?
	  INTTYPE.LONG : INTTYPE.ULONG;
	break;
      case INTTYPE.LONG: break;
      default: assert(false);
      }
      return true;
    }
    else {
      return false;
    }
  }


  void writeExprString(ref Charbuf str) {
    // VSxxxxx or VUxxxxx
    str ~= 'V';
    static if (isBoolean!T) {
      str ~= 'U';
    }
    static if (isIntegral!T) {
      static if (isSigned!T) {
	str ~= 'S';
      }
      else {
	str ~= 'U';
      }
    }
    else {
      static if (T.ISSIGNED) {
	str ~= 'S';
      }
      else {
	str ~= 'U';
      }
    }
    _val.writeHexString(str);
  }
}


// This class would hold two(bin) vector nodes and produces a vector
// only after processing those two nodes
class CstVec2VecExpr: CstVecTerm
{
  import std.conv;

  CstVecExpr _lhs;
  CstVecExpr _rhs;
  CstBinaryOp _op;

  string describe() {
    return "( " ~ _lhs.describe ~ " " ~ _op.to!string() ~ " " ~ _rhs.describe ~ " )";
  }

  void annotate(ref uint varN) {
    _lhs.annotate(varN);
    _rhs.annotate(varN);
  }

  void visit(CstSolver solver) {
    _lhs.visit(solver);
    _rhs.visit(solver);
    solver.process(_op);
  }

  BddVec getBDD(CstStage stage, Buddy buddy) {
    final switch(_op) {
    case CstBinaryOp.AND: return _lhs.getBDD(stage, buddy) &
	_rhs.getBDD(stage, buddy);
    case CstBinaryOp.OR:  return _lhs.getBDD(stage, buddy) |
	_rhs.getBDD(stage, buddy);
    case CstBinaryOp.XOR: return _lhs.getBDD(stage, buddy) ^
	_rhs.getBDD(stage, buddy);
    case CstBinaryOp.ADD: return _lhs.getBDD(stage, buddy) +
	_rhs.getBDD(stage, buddy);
    case CstBinaryOp.SUB: return _lhs.getBDD(stage, buddy) -
	_rhs.getBDD(stage, buddy);
    case CstBinaryOp.MUL:
      if(_rhs.isConst()) return _lhs.getBDD(stage, buddy) *
			   _rhs.evaluate();
      if(_lhs.isConst()) return _lhs.evaluate() *
			   _rhs.getBDD(stage, buddy);
      return _lhs.getBDD(stage, buddy) * _rhs.getBDD(stage, buddy);
    case CstBinaryOp.DIV:
      if(_rhs.isConst()) return _lhs.getBDD(stage, buddy) /
			   _rhs.evaluate();
      return _lhs.getBDD(stage, buddy) / _rhs.getBDD(stage, buddy);
    case CstBinaryOp.REM:
      if(_rhs.isConst()) return _lhs.getBDD(stage, buddy) %
			   _rhs.evaluate();
      return _lhs.getBDD(stage, buddy) % _rhs.getBDD(stage, buddy);
    case CstBinaryOp.LSH:
      if(_rhs.isConst()) return _lhs.getBDD(stage, buddy) <<
			   _rhs.evaluate();
      return _lhs.getBDD(stage, buddy) << _rhs.getBDD(stage, buddy);
    case CstBinaryOp.RSH:
      if(_rhs.isConst()) return _lhs.getBDD(stage, buddy) >>
			   _rhs.evaluate();
      return _lhs.getBDD(stage, buddy) >> _rhs.getBDD(stage, buddy);
    case CstBinaryOp.LRSH:
      if(_rhs.isConst()) return _lhs.getBDD(stage, buddy) >>>
			   _rhs.evaluate();
      return _lhs.getBDD(stage, buddy) >>> _rhs.getBDD(stage, buddy);
    case CstBinaryOp.BITINDEX:
      assert(false, "BITINDEX is not implemented yet!");
    case CstBinaryOp.RANGE:
      assert(false, "RANGE is used only in conjunction with Slice expressions!");
    }
  }

  // bool getVal(ref long val) {

  //   long lval;
  //   long rval;
  //   if (! _lhs.getVal(lval)) {
  //     return false;
  //   }
  //   if (! _rhs.getVal(rval)) {
  //     return false;
  //   }

  //   final switch(_op) {
  //   case CstBinaryOp.AND: val = lval &  rval; return true;
  //   case CstBinaryOp.OR:  val = lval |  rval; return true;
  //   case CstBinaryOp.XOR: val = lval ^  rval; return true;
  //   case CstBinaryOp.ADD: val = lval +  rval; return true;
  //   case CstBinaryOp.SUB: val = lval -  rval; return true;
  //   case CstBinaryOp.MUL: val = lval *  rval; return true;
  //   case CstBinaryOp.DIV: val = lval /  rval; return true;
  //   case CstBinaryOp.REM: val = lval %  rval; return true;
  //   case CstBinaryOp.LSH: val = lval << rval; return true;
  //   case CstBinaryOp.RSH: val = lval >> rval; return true;
  //   case CstBinaryOp.BITINDEX:
  //     assert(false, "BITINDEX is not implemented yet!");
  //   }
  // }

  long evaluate() {
    auto lvec = _lhs.evaluate();
    auto rvec = _rhs.evaluate();

    final switch(_op) {
    case CstBinaryOp.AND: return lvec &  rvec;
    case CstBinaryOp.OR:  return lvec |  rvec;
    case CstBinaryOp.XOR: return lvec ^  rvec;
    case CstBinaryOp.ADD: return lvec +  rvec;
    case CstBinaryOp.SUB: return lvec -  rvec;
    case CstBinaryOp.MUL: return lvec *  rvec;
    case CstBinaryOp.DIV: return lvec /  rvec;
    case CstBinaryOp.REM: return lvec %  rvec;
    case CstBinaryOp.LSH: return lvec << rvec;
    case CstBinaryOp.RSH: return lvec >> rvec;
    case CstBinaryOp.LRSH: return lvec >>> rvec;
    case CstBinaryOp.BITINDEX:
      assert (false, "BITINDEX is not implemented yet!");
    case CstBinaryOp.RANGE:
      assert (false, "RANGE is used only in conjunction with Slice expressions!");
    }
  }

  override CstVec2VecExpr unroll(CstIterator iter, uint n) {
    return new CstVec2VecExpr(_lhs.unroll(iter, n), _rhs.unroll(iter, n), _op);
  }

  this(CstVecExpr lhs, CstVecExpr rhs, CstBinaryOp op) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
  }

  uint resolveLap() {
    auto lhs = _lhs.resolveLap();
    auto rhs = _rhs.resolveLap();
    if (rhs > lhs) return rhs;
    else return lhs;
  }

  void resolveLap(uint lap) {
    _lhs.resolveLap(lap);
    _rhs.resolveLap(lap);
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

  void setDomainContext(CstPredicate pred,
			ref CstDomain[] rnds,
			ref CstDomain[] vars,
			ref CstValue[] vals,
			ref CstIterator[] iters,
			ref CstDomain[] idxs,
			ref CstDomain[] deps) {
    _lhs.setDomainContext(pred, rnds, vars, vals, iters, idxs, deps);
    _rhs.setDomainContext(pred, rnds, vars, vals, iters, idxs, deps);
  }

  bool getUniRange(ref UniRange rng) {
    INTTYPE iType;
    if (this.getIntType(iType)) {
      rng.map(iType);
      if (_rhs.isSolved()) {
	assert(! _lhs.isSolved());
	long rhs = _rhs.evaluate();
	switch(_op) {
	case CstBinaryOp.ADD:
	  UniRangeMod(IntRangeModOp.ADD, rhs).apply(rng);
	  return _lhs.getUniRange(rng);
	case CstBinaryOp.SUB: 
	  UniRangeMod(IntRangeModOp.SUB, rhs).apply(rng);
	  return _lhs.getUniRange(rng);
	default:
	  return false;
	}
      }
      else if (_lhs.isSolved()) {
	assert(! _rhs.isSolved());
	long lhs = _lhs.evaluate();
	switch(_op) {
	case CstBinaryOp.ADD:
	  UniRangeMod(IntRangeModOp.ADD, lhs).apply(rng);
	  return _rhs.getUniRange(rng);
	case CstBinaryOp.SUB: 
	  UniRangeMod(IntRangeModOp.SUBD, lhs).apply(rng);
	  return _rhs.getUniRange(rng);
	default:
	  return false;
	}
      }
      else {
	return false;
      }
    }
    else {
      return false;
    }
  }

  bool getIntRange(ref IntR rng) {
    if (_rhs.isSolved()) {
      assert(! _lhs.isSolved());
      auto rhs = cast(int) _rhs.evaluate();
      switch(_op) {
      case CstBinaryOp.ADD:
	IntRangeMod!int(IntRangeModOp.ADD, rhs).apply(rng);
	return _lhs.getIntRange(rng);
      case CstBinaryOp.SUB: 
	IntRangeMod!int(IntRangeModOp.SUB, rhs).apply(rng);
	return _lhs.getIntRange(rng);
      default:
	return false;
      }
    }
    else if (_lhs.isSolved()) {
      assert(! _rhs.isSolved());
      auto lhs = cast(int) _lhs.evaluate();
      switch(_op) {
      case CstBinaryOp.ADD:
	IntRangeMod!int(IntRangeModOp.ADD, lhs).apply(rng);
	return _rhs.getIntRange(rng);
      case CstBinaryOp.SUB: 
	IntRangeMod!int(IntRangeModOp.SUBD, lhs).apply(rng);
	return _rhs.getIntRange(rng);
      default:
	return false;
      }
    }
    else {
      return false;
    }
  }

  bool getIntType(ref INTTYPE iType) {
    bool lvalid, rvalid;
    INTTYPE lType, rType;
    
    lvalid = _lhs.getIntType(lType);
    rvalid = _rhs.getIntType(rType);

    if (lvalid && rvalid) {
      if (lType == INTTYPE.LONG ||
	  rType == INTTYPE.LONG ||
	  iType == INTTYPE.LONG) {
	iType = INTTYPE.LONG;
      }
      else if ((lType == INTTYPE.ULONG ||
		rType == INTTYPE.ULONG ||
		iType == INTTYPE.ULONG) &&
	       (lType == INTTYPE.INT ||
		rType == INTTYPE.INT ||
		iType == INTTYPE.INT))	       {
	iType = INTTYPE.LONG;
      }
      else if (lType == INTTYPE.ULONG ||
	       rType == INTTYPE.ULONG ||
	       iType == INTTYPE.ULONG) {
	iType = INTTYPE.ULONG;
      }
      else if (lType == INTTYPE.INT ||
	       rType == INTTYPE.INT ||
	       iType == INTTYPE.INT) {
	iType = INTTYPE.INT;
      }
      else {
	iType = INTTYPE.UINT;
      }
      return true;
    }
    else {
      return false;
    }
  }

  override bool isSolved() {
    return _lhs.isSolved() && _rhs.isSolved();
  }

  override void writeExprString(ref Charbuf str) {
    str ~= '(';
    str ~= _op.to!string;
    str ~= ' ';
    _lhs.writeExprString(str);
    str ~= ' ';
    _rhs.writeExprString(str);
    str ~= ')';
  }
}

class CstVecSliceExpr: CstVecTerm
{
  CstVecExpr _vec;
  CstVec2VecExpr _range;


  string describe() {
    return _vec.describe() ~ "[ " ~ _range.describe() ~ " ]";
  }

  void annotate(ref uint varN) {
    _vec.annotate(varN);
    _range.annotate(varN);
  }

  void visit(CstSolver solver) {
    _vec.visit(solver);
    _range._lhs.visit(solver);
    _range._rhs.visit(solver);
    solver.process(CstBinaryOp.RANGE);
  }

  BddVec getBDD(CstStage stage, Buddy buddy) {
    auto vec  = _vec.getBDD(stage, buddy);
    size_t lvec = cast(size_t) _range._lhs.evaluate();
    size_t rvec = lvec;
    if(_range._rhs is null) {
      rvec = lvec + 1;
    }
    else {
      rvec = cast(size_t) _range._rhs.evaluate();
    }
    return vec[lvec..rvec];
  }

  // bool getVal(ref long val) {
  //   return false;
  // }

  long evaluate() {
    // auto vec  = _vec.evaluate();
    // auto lvec = _lhs.evaluate();
    // auto rvec = _range._rhs.evaluate();

    assert(false, "Can not evaluate a CstVecSliceExpr!");
  }

  override CstVecSliceExpr unroll(CstIterator iter, uint n) {
    if (_range._rhs is null) {
      return new CstVecSliceExpr(_vec.unroll(iter, n), _range._lhs.unroll(iter, n));
    }
    else {
      return new CstVecSliceExpr(_vec.unroll(iter, n),
				 _range._lhs.unroll(iter, n), _range._rhs.unroll(iter, n));
    }
  }

  this(CstVecExpr vec, CstVecExpr lhs, CstVecExpr rhs=null) {
    _vec = vec;
    _range = new CstVec2VecExpr(lhs, rhs, CstBinaryOp.RANGE);
  }

  this(CstVecExpr vec, CstVec2VecExpr range) {
    assert (range._op is CstBinaryOp.RANGE);
    _vec = vec;
    _range = range;
  }

  uint resolveLap() {
    return _vec.resolveLap();
  }

  void resolveLap(uint lap) {
    _vec.resolveLap(lap);
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

  void setDomainContext(CstPredicate pred,
			ref CstDomain[] rnds,
			ref CstDomain[] vars,
			ref CstValue[] vals,
			ref CstIterator[] iters,
			ref CstDomain[] idxs,
			ref CstDomain[] deps) {
    _vec.setDomainContext(pred, rnds, vars, vals, iters, idxs, deps);
    _range.setDomainContext(pred, idxs, vars, vals, iters, idxs, deps);
  }

  bool getIntRange(ref IntR rng) {
    return false;
  }

  bool getUniRange(ref UniRange rng) {
    return false;
  }

  bool getIntType(ref INTTYPE iType) {
    return false;
  }
  
  override bool isSolved() {
    return _range.isSolved() && _vec.isSolved();
  }
  
  override void writeExprString(ref Charbuf str) {
    _vec.writeExprString(str);
    str ~= '[';
    _range.writeExprString(str);
    str ~= ']';
  }
}

class CstNotVecExpr: CstVecTerm
{
  import std.conv;

  CstVecExpr _expr;

  string describe() {
    return "( ~ " ~ _expr.describe ~ " )";
  }

  void annotate(ref uint varN) {
    _expr.annotate(varN);
  }

  void visit(CstSolver solver) {
    _expr.visit(solver);
    solver.process(CstUnaryOp.NOT);
  }

  BddVec getBDD(CstStage stage, Buddy buddy) {
    return ~(_expr.getBDD(stage, buddy));
  }

  // bool getVal(ref long val) {
  //   auto retval = _expr.getVal(val);
  //   val = ~val;
  //   return retval;
  // }

  long evaluate() {
    return ~(_expr.evaluate());
  }

  override CstNotVecExpr unroll(CstIterator iter, uint n) {
    return new CstNotVecExpr(_expr.unroll(iter, n));
  }

  this(CstVecExpr expr) {
    _expr = expr;
  }

  uint resolveLap() {
    return _expr.resolveLap();
  }

  void resolveLap(uint lap) {
    _expr.resolveLap(lap);
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

  void setDomainContext(CstPredicate pred,
			ref CstDomain[] rnds,
			ref CstDomain[] vars,
			ref CstValue[] vals,
			ref CstIterator[] iters,
			ref CstDomain[] idxs,
			ref CstDomain[] deps) {
    _expr.setDomainContext(pred, rnds, vars, vals, iters, idxs, deps);
  }

  bool getIntRange(ref IntR rng) {
    return false;
  }

  bool getUniRange(ref UniRange rng) {
    return false;
  }

  bool getIntType(ref INTTYPE iType) {
    return false;
  }

  override bool isSolved() {
    return _expr.isSolved();
  }

  override void writeExprString(ref Charbuf str) {
    str ~= "(NOT ";
    _expr.writeExprString(str);
    str ~= ')';
  }
}

class CstNegVecExpr: CstVecTerm
{
  import std.conv;

  CstVecExpr _expr;

  string describe() {
    return "( - " ~ _expr.describe ~ " )";
  }

  void annotate(ref uint varN) {
    _expr.annotate(varN);
  }

  void visit(CstSolver solver) {
    _expr.visit(solver);
    solver.process(CstUnaryOp.NEG);
  }

  BddVec getBDD(CstStage stage, Buddy buddy) {
    return -(_expr.getBDD(stage, buddy));
  }

  // bool getVal(ref long val) {
  //   auto retval = _expr.getVal(val);
  //   val = -val;
  //   return retval;
  // }

  long evaluate() {
    return -(_expr.evaluate());
  }

  override CstNegVecExpr unroll(CstIterator iter, uint n) {
    return new CstNegVecExpr(_expr.unroll(iter, n));
  }

  this(CstVecExpr expr) {
    _expr = expr;
  }

  uint resolveLap() {
    return _expr.resolveLap();
  }

  void resolveLap(uint lap) {
    _expr.resolveLap(lap);
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

  void setDomainContext(CstPredicate pred,
			ref CstDomain[] rnds,
			ref CstDomain[] vars,
			ref CstValue[] vals,
			ref CstIterator[] iters,
			ref CstDomain[] idxs,
			ref CstDomain[] deps) {
    _expr.setDomainContext(pred, rnds, vars, vals, iters, idxs, deps);
  }

  bool getIntRange(ref IntR rng) {
    return false;
  }

  bool getUniRange(ref UniRange rng) {
    return false;
  }

  bool getIntType(ref INTTYPE iType) {
    return false;
  }

  override bool isSolved() {
    return _expr.isSolved();
  }

  override void writeExprString(ref Charbuf str) {
    str ~= "(NEG ";
    _expr.writeExprString(str);
    str ~= ')';
  }
}


class CstLogic2LogicExpr: CstLogicTerm
{
  import std.conv;

  CstLogicExpr _lhs;
  CstLogicExpr _rhs;
  CstLogicalOp _op;

  this(CstLogicExpr lhs, CstLogicExpr rhs, CstLogicalOp op) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
  }

  string describe() {
    return "( " ~ _lhs.describe ~ " " ~ _op.to!string ~ " " ~ _rhs.describe ~ " )";
  }

  void visit(CstSolver solver) {
    _lhs.visit(solver);
    _rhs.visit(solver);
    solver.process(_op);
  }

  BDD getBDD(CstStage stage, Buddy buddy) {
    auto lvec = _lhs.getBDD(stage, buddy);
    auto rvec = _rhs.getBDD(stage, buddy);

    BDD retval;
    final switch(_op) {
    case CstLogicalOp.LOGICAND: retval = lvec &  rvec; break;
    case CstLogicalOp.LOGICOR:  retval = lvec |  rvec; break;
    case CstLogicalOp.LOGICIMP: retval = lvec >> rvec; break;
    case CstLogicalOp.LOGICNOT: assert(false);
    }
    return retval;
  }

  void annotate(ref uint varN) {
    _lhs.annotate(varN);
    _rhs.annotate(varN);
  }

  override CstLogic2LogicExpr unroll(CstIterator iter, uint n) {
    return new CstLogic2LogicExpr(_lhs.unroll(iter, n), _rhs.unroll(iter, n), _op);
  }

  uint resolveLap() {
    uint lhs = _lhs.resolveLap();
    uint rhs = _rhs.resolveLap();
    if (lhs > rhs) return lhs;
    else return rhs;
  }
  void resolveLap(uint lap) {
    _lhs.resolveLap(lap);
    _rhs.resolveLap(lap);
  }

  void setDomainContext(CstPredicate pred,
			ref CstDomain[] rnds,
			ref CstDomain[] vars,
			ref CstValue[] vals,
			ref CstIterator[] iters,
			ref CstDomain[] idxs,
			ref CstDomain[] deps) {
    _lhs.setDomainContext(pred, rnds, vars, vals, iters, idxs, deps);
    _rhs.setDomainContext(pred, rnds, vars, vals, iters, idxs, deps);
  }

  override bool getUniRangeSet(ref IntRS rs) {
    return getUniRangeSetImpl(rs);
  }
    
  override bool getUniRangeSet(ref UIntRS rs) {
    return getUniRangeSetImpl(rs);
  }
    
  override bool getUniRangeSet(ref LongRS rs) {
    return getUniRangeSetImpl(rs);
  }
    
  override bool getUniRangeSet(ref ULongRS rs) {
    return getUniRangeSetImpl(rs);
  }
    
  bool getUniRangeSetImpl(T)(ref T rs) {
    assert(! _lhs.isSolved(), this.describe());
    assert(! _rhs.isSolved(), this.describe());

    T lhs;
    T rhs;

    if (_lhs.getUniRangeSet(lhs) is false) return false;
    if (_rhs.getUniRangeSet(rhs) is false) return false;

    final switch(_op) {
    case CstLogicalOp.LOGICAND: lhs &= rhs; break;
    case CstLogicalOp.LOGICOR:  lhs |= rhs; break;
    case CstLogicalOp.LOGICIMP: return false;
    case CstLogicalOp.LOGICNOT: assert(false);
    }

    rs = lhs;
    return true;
  }

  override bool getIntRangeSet(ref IntRS rs) {
    assert(! _lhs.isSolved());
    assert(! _rhs.isSolved());

    IntRS lhs;
    IntRS rhs;

    if (_lhs.getIntRangeSet(lhs) is false) return false;
    if (_rhs.getIntRangeSet(rhs) is false) return false;

    final switch(_op) {
    case CstLogicalOp.LOGICAND: lhs &= rhs; break;
    case CstLogicalOp.LOGICOR:  lhs |= rhs; break;
    case CstLogicalOp.LOGICIMP: return false;
    case CstLogicalOp.LOGICNOT: assert(false);
    }

    rs = lhs;
    return true;
  }

  bool cstExprIsNop() {
    return false;
  }

  override bool isSolved() {
    return _lhs.isSolved && _rhs.isSolved();
  }

  override void writeExprString(ref Charbuf str) {
    str ~= '(';
    str ~= _op.to!string;
    str ~= ' ';
    _lhs.writeExprString(str);
    str ~= ' ';
    _rhs.writeExprString(str);
    str ~= ")\n";
  }
}

// TBD
class CstIteBddExpr: CstLogicTerm
{
  string describe() {
    return "CstIteBddExpr";
  }

  void setDomainContext(CstPredicate pred,
			ref CstDomain[] rnds,
			ref CstDomain[] vars,
			ref CstValue[] vals,
			ref CstIterator[] iters,
			ref CstDomain[] idxs,
			ref CstDomain[] deps) {
    assert(false, "TBD");
  }

  void annotate(ref uint varN) {
    assert(false, "TBD");
  }

  override bool getUniRangeSet(ref IntRS rs) {
    return false;
  }

  override bool getUniRangeSet(ref UIntRS rs) {
    return false;
  }

  override bool getUniRangeSet(ref LongRS rs) {
    return false;
  }

  override bool getUniRangeSet(ref ULongRS rs) {
    return false;
  }

  override bool getIntRangeSet(ref IntRS rs) {
    return false;
  }

  bool cstExprIsNop() {
    return false;
  }

  uint resolveLap() {
    assert(false, "TBD");
  }    

  void resolveLap(uint lap) {
    assert(false, "TBD");
  }    

  CstLogicTerm unroll(CstIterator iter, uint n) {
    assert(false, "TBD");
  }

  // abstract CstStage[] getStages();

  abstract void visit(CstSolver solver) {
    assert(false, "TBD");
  }

  abstract BDD getBDD(CstStage stage, Buddy buddy) {
    assert(false, "TBD");
  }

  override bool isSolved() {
    assert(false, "TBD");
  }
}

class CstVec2LogicExpr: CstLogicTerm
{
  import std.conv;

  CstVecExpr _lhs;
  CstVecExpr _rhs;
  CstCompareOp _op;

  this(CstVecExpr lhs, CstVecExpr rhs, CstCompareOp op) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
  }

  string describe() {
    return "( " ~ _lhs.describe ~ " " ~ _op.to!string ~ " " ~ _rhs.describe ~ " )";
  }

  void annotate(ref uint varN) {
    _lhs.annotate(varN);
    _rhs.annotate(varN);
  }

  void visit(CstSolver solver) {
    _lhs.visit(solver);
    _rhs.visit(solver);
    solver.process(_op);
  }

  BDD getBDD(CstStage stage, Buddy buddy) {
    auto lvec = _lhs.getBDD(stage, buddy);
    auto rvec = _rhs.getBDD(stage, buddy);

    BDD retval;
    final switch(_op) {
    case CstCompareOp.LTH: retval = lvec.lth(rvec); break;
    case CstCompareOp.LTE: retval = lvec.lte(rvec); break;
    case CstCompareOp.GTH: retval = lvec.gth(rvec); break;
    case CstCompareOp.GTE: retval = lvec.gte(rvec); break;
    case CstCompareOp.EQU: retval = lvec.equ(rvec); break;
    case CstCompareOp.NEQ: retval = lvec.neq(rvec); break;
    }
    return retval;
  }

  override CstVec2LogicExpr unroll(CstIterator iter, uint n) {
    // import std.stdio;
    // writeln(_lhs.describe() ~ " " ~ _op.to!string ~ " " ~ _rhs.describe() ~ " Getting unwound!");
    // writeln("RHS: ", _rhs.unroll(iter, n).describe());
    // writeln("LHS: ", _lhs.unroll(iter, n).describe());
    return new CstVec2LogicExpr(_lhs.unroll(iter, n), _rhs.unroll(iter, n), _op);
  }

  uint resolveLap() {
    uint lhs = _lhs.resolveLap();
    uint rhs = _rhs.resolveLap();
    if (lhs > rhs) return lhs;
    else return rhs;
  }
  
  void resolveLap(uint lap) {
    _lhs.resolveLap(lap);
    _rhs.resolveLap(lap);
  }


  void setDomainContext(CstPredicate pred,
			ref CstDomain[] rnds,
			ref CstDomain[] vars,
			ref CstValue[] vals,
			ref CstIterator[] iters,
			ref CstDomain[] idxs,
			ref CstDomain[] deps) {
    _lhs.setDomainContext(pred, rnds, vars, vals, iters, idxs, deps);
    _rhs.setDomainContext(pred, rnds, vars, vals, iters, idxs, deps);
  }

  bool getIntType(ref INTTYPE iType) {
    bool lvalid, rvalid;
    INTTYPE lType, rType;

    assert(iType == INTTYPE.UINT);

    lvalid = _lhs.getIntType(lType);
    rvalid = _rhs.getIntType(rType);

    if (lvalid && rvalid) {
      if (lType == INTTYPE.ULONG ||
	  rType == INTTYPE.ULONG) {
	iType = INTTYPE.ULONG;
      }
      else if ((lType == INTTYPE.LONG ||
		rType == INTTYPE.LONG) &&
	       (lType == INTTYPE.UINT ||
		rType == INTTYPE.UINT))	       {
	iType = INTTYPE.ULONG;
      }
      else if (lType == INTTYPE.LONG ||
	       rType == INTTYPE.LONG) {
	iType = INTTYPE.LONG;
      }
      else if (lType == INTTYPE.UINT ||
	       rType == INTTYPE.UINT) {
	iType = INTTYPE.UINT;
      }
      else {
	iType = INTTYPE.INT;
      }
      return true;
    }
    else {
      return false;
    }
  }
  
  override bool getUniRangeSet(ref IntRS rs) {
    return getUniRangeSetImpl(rs);
  }
    
  override bool getUniRangeSet(ref UIntRS rs) {
    return getUniRangeSetImpl(rs);
  }
    
  override bool getUniRangeSet(ref LongRS rs) {
    return getUniRangeSetImpl(rs);
  }
    
  override bool getUniRangeSet(ref ULongRS rs) {
    return getUniRangeSetImpl(rs);
  }
    
  bool getUniRangeSetImpl(RS)(ref RS rs) {
    static if (is (RS == IntRangeSet!T, T)) {
      if (_rhs.isSolved()) {
	INTTYPE iType;
	assert(! _lhs.isSolved(), "Expression: " ~ _lhs.describe());
	bool valid = this.getIntType(iType);
	if (valid) {
	  ulong rhs = cast(int) _rhs.evaluate();
	  UniRange rn = UniRange(_op, iType, rhs);
	  valid = _lhs.getUniRange(rn);
	  auto irn = IntRange!T(rn);
	  rs ~= irn;
	}
	return valid;
      }
      else if (_lhs.isSolved()) {
	INTTYPE iType;
	assert(! _rhs.isSolved(), "Expression: " ~ _rhs.describe());
	bool valid = this.getIntType(iType);
	if (valid) {
	  ulong lhs = cast(int) _lhs.evaluate();
	  UniRange rn = UniRange(_op, iType, lhs);
	  valid = _rhs.getUniRange(rn);
	  auto irn = IntRange!T(rn);
	  rs ~= irn;
	}
	return valid;
      }
      else {
	return false;
      }
    }
    else {
      static assert(false);
    }
  }

  override bool getIntRangeSet(ref IntRS rs) {
    if (_rhs.isSolved()) {
      assert(! _lhs.isSolved(), "Expression: " ~ _lhs.describe());
      auto rhs = cast(int) _rhs.evaluate();
      IntR rn = IntR(_op, rhs);
      bool valid = _lhs.getIntRange(rn);
      rs ~= rn;
      return valid;
    }
    else if (_lhs.isSolved()) {
      assert(! _rhs.isSolved(), "Expression: " ~ _rhs.describe());
      auto lhs = cast(int) _lhs.evaluate();
      IntR rn = IntR(_op, lhs, true);
      bool valid = _rhs.getIntRange(rn);
      rs ~= rn;
      return valid;
    }
    else {
      return false;
    }
  }

  override bool isSolved() {
    return _lhs.isSolved && _rhs.isSolved();
  }

  override void writeExprString(ref Charbuf str) {
    str ~= '(';
    str ~= _op.to!string;
    str ~= ' ';
    _lhs.writeExprString(str);
    str ~= ' ';
    _rhs.writeExprString(str);
    str ~= ")\n";
  }

}

class CstBddConst: CstLogicTerm
{
  immutable bool _expr;

  this(bool expr) {
    _expr = expr;
  }

  void annotate(ref uint varN) {
    // _lhs.annotate(varN);
    // _rhs.annotate(varN);
  }

  void visit(CstSolver solver) {
    solver.pushValue(_expr);
  }

  BDD getBDD(CstStage stage, Buddy buddy) {
    if(_expr) return buddy.one();
    else return buddy.zero();
  }

  string describe() {
    if(_expr) return "TRUE";
    else return "FALSE";
  }

  override CstBddConst unroll(CstIterator iter, uint n) {
    return this;
  }

  uint resolveLap() {
    return 0;
  }
  void resolveLap(uint lap) {}

  void setDomainContext(CstPredicate pred,
			ref CstDomain[] rnds,
			ref CstDomain[] vars,
			ref CstValue[] vals,
			ref CstIterator[] iters,
			ref CstDomain[] idxs,
			ref CstDomain[] deps) {
    // nothing for CstBddConst
  }

  override bool getUniRangeSet(ref IntRS rs) {
    return false;
  }

  override bool getUniRangeSet(ref UIntRS rs) {
    return false;
  }

  override bool getUniRangeSet(ref LongRS rs) {
    return false;
  }

  override bool getUniRangeSet(ref ULongRS rs) {
    return false;
  }

  override bool getIntRangeSet(ref IntRS rng) {
    return false;
  }

  override bool isSolved() {
    return true;
  }

  override void writeExprString(ref Charbuf str) {
    if (_expr) str ~= "TRUE";
    else str ~= "FALSE";
  }
}

class CstNotBddExpr: CstLogicTerm
{
  CstLogicExpr _expr;

  this(CstLogicExpr expr) {
    _expr = expr;
  }

  string describe() {
    return "( " ~ "!" ~ " " ~ _expr.describe ~ " )";
  }

  void annotate(ref uint varN) {
    _expr.annotate(varN);
  }

  void visit(CstSolver solver) {
    _expr.visit(solver);
    solver.process(CstLogicalOp.LOGICNOT);
  }

  BDD getBDD(CstStage stage, Buddy buddy) {
    auto bdd = _expr.getBDD(stage, buddy);
    return (~ bdd);
  }

  override CstNotBddExpr unroll(CstIterator iter, uint n) {
    return new CstNotBddExpr(_expr.unroll(iter, n));
  }

  uint resolveLap() {
    return _expr.resolveLap();
  }
  void resolveLap(uint lap) {
    _expr.resolveLap(lap);
  }

  void setDomainContext(CstPredicate pred,
			ref CstDomain[] rnds,
			ref CstDomain[] vars,
			ref CstValue[] vals,
			ref CstIterator[] iters,
			ref CstDomain[] idxs,
			ref CstDomain[] deps) {
    _expr.setDomainContext(pred, rnds, vars, vals, iters, idxs, deps);
  }

  override bool getUniRangeSet(ref IntRS rs) {
    return false;
  }

  override bool getUniRangeSet(ref UIntRS rs) {
    return false;
  }

  override bool getUniRangeSet(ref LongRS rs) {
    return false;
  }

  override bool getUniRangeSet(ref ULongRS rs) {
    return false;
  }

  override bool getIntRangeSet(ref IntRS rng) {
    return false;
  }

  override bool isSolved() {
    return _expr.isSolved();
  }

  override void writeExprString(ref Charbuf str) {
    str ~= "(NOT ";
    _expr.writeExprString(str);
    str ~= ")\n";
  }
  
}

// CstLogic2LogicExpr logicOr(CstVecExpr other)
// {
//   return new CstLogic2LogicExpr(toBdd(this), toBdd(other), CstLogicalOp.LOGICOR);
// }

auto _esdl__logicOr(P, Q)(P p, Q q) {
  CstLogicTerm _p;
  CstLogicTerm _q;
  static if (is (P == bool)) {
    _p = new CstBddConst(p);
  }
  else static if (is (P: CstVecExpr)) {
    _p = toBdd(p);
  }
  else {
    _p = p;
  }
  static if(is(Q == bool)) {
    _q = new CstBddConst(q);
  }
  else static if (is (Q: CstVecExpr)) {
    _q = toBdd(q);
  }
  else {
    _q = q;
  }
  return _p.logicOr(_q);
}

auto _esdl__logicAnd(P, Q)(P p, Q q) {
  CstLogicTerm _p;
  CstLogicTerm _q;
  static if(is(P == bool)) {
    _p = new CstBddConst(p);
  }
  else static if (is (P: CstVecExpr)) {
    _p = toBdd(p);
  }
  else {
    _p = p;
  }
  static if(is(Q == bool)) {
    _q = new CstBddConst(q);
  }
  else static if (is (Q: CstVecExpr)) {
    _q = toBdd(q);
  }
  else {
    _q = q;
  }
  return _p.logicAnd(_q);
}


auto _esdl__lth(P, Q)(P p, Q q) {
  static if(is(P: CstVecExpr)) {
    return _esdl__lth_impl(p, q);
  }
  else static if(is(Q: CstVecExpr)) {
    return _esdl__gte_impl(q, p);
  }
  else static if((isBitVector!P || isIntegral!P) &&
		 (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p < q);
  }
}

CstVec2LogicExpr _esdl__lth_impl(Q)(CstVecExpr left, Q q)
  if(isBitVector!Q || isIntegral!Q) {
    auto qq = new CstVecValue!Q(q); // CstVecValue!Q.allocate(q);
    return _esdl__lth_impl(left, qq);
  }

CstVec2LogicExpr _esdl__lth_impl(CstVecExpr p, CstVecExpr q) {
  return new CstVec2LogicExpr(p, q, CstCompareOp.LTH);
}

auto _esdl__lte(P, Q)(P p, Q q) {
  static if(is(P: CstVecExpr)) {
    return _esdl__lte_impl(p, q);
  }
  else static if(is(Q: CstVecExpr)) {
    return _esdl__gth_impl(q, p);
  }
  else static if((isBitVector!P || isIntegral!P) &&
		 (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p <= q);
  }
}

CstVec2LogicExpr _esdl__lte_impl(Q)(CstVecExpr p, Q q)
  if(isBitVector!Q || isIntegral!Q) {
    auto qq = new CstVecValue!Q(q); // CstVecValue!Q.allocate(q);
    return _esdl__lte_impl(p, qq);
  }

CstVec2LogicExpr _esdl__lte_impl(CstVecExpr p, CstVecExpr q) {
  return new CstVec2LogicExpr(p, q, CstCompareOp.LTE);
}

auto _esdl__gth(P, Q)(P p, Q q) {
  static if(is(P: CstVecExpr)) {
    return _esdl__gth_impl(p, q);
  }
  else static if(is(Q: CstVecExpr)) {
    return _esdl__lte_impl(q, p);
  }
  else static if((isBitVector!P || isIntegral!P) &&
		 (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p > q);
  }
}

CstVec2LogicExpr _esdl__gth_impl(Q)(CstVecExpr p, Q q)
  if(isBitVector!Q || isIntegral!Q) {
    auto qq = new CstVecValue!Q(q); // CstVecValue!Q.allocate(q);
    return _esdl__gth_impl(p, qq);
  }

CstVec2LogicExpr _esdl__gth_impl(CstVecExpr p, CstVecExpr q) {
  return new CstVec2LogicExpr(p, q, CstCompareOp.GTH);
}

auto _esdl__gte(P, Q)(P p, Q q) {
  static if(is(P: CstVecExpr)) {
    return _esdl__gte_impl(p, q);
  }
  else static if(is(Q: CstVecExpr)) {
    return _esdl__lth_impl(q, p);
  }
  else static if((isBitVector!P || isIntegral!P) &&
		 (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p >= q);
  }
}

CstVec2LogicExpr _esdl__gte_impl(Q)(CstVecExpr p, Q q)
  if(isBitVector!Q || isIntegral!Q) {
    auto qq = new CstVecValue!Q(q); // CstVecValue!Q.allocate(q);
    return _esdl__gte_impl(p, qq);
  }

CstVec2LogicExpr _esdl__gte_impl(CstVecExpr p, CstVecExpr q) {
  return new CstVec2LogicExpr(p, q, CstCompareOp.GTE);
}

auto _esdl__equ(P, Q)(P p, Q q) {
  static if(is(P: CstVecExpr)) {
    return _esdl__equ_impl(p, q);
  }
  else static if(is(Q: CstVecExpr)) {
    return _esdl__equ_impl(q, p);
  }
  else static if((isBitVector!P || isIntegral!P) &&
		 (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p == q);
  }
}

CstVec2LogicExpr _esdl__equ_impl(Q)(CstVecExpr p, Q q)
  if(isBitVector!Q || isIntegral!Q) {
    auto qq = new CstVecValue!Q(q); // CstVecValue!Q.allocate(q);
    return _esdl__equ_impl(p, qq);
  }

CstVec2LogicExpr _esdl__equ_impl(CstVecExpr p, CstVecExpr q) {
  return new CstVec2LogicExpr(p, q, CstCompareOp.EQU);
}

auto _esdl__neq(P, Q)(P p, Q q) {
  static if(is(P: CstVecExpr)) {
    return _esdl__neq_impl(p, q);
  }
  else static if(is(Q: CstVecExpr)) {
    return _esdl__neq_impl(q, p);
  }
  else static if((isBitVector!P || isIntegral!P) &&
		 (isBitVector!Q || isIntegral!Q)) {
    return new CstBddConst(p != q);
  }
}
CstVec2LogicExpr _esdl__neq_impl(Q)(CstVecExpr p, Q q)
  if(isBitVector!Q || isIntegral!Q) {
    auto qq = new CstVecValue!Q(q); // CstVecValue!Q.allocate(q);
    return _esdl__neq(p, qq);
  }

CstVec2LogicExpr _esdl__neq_impl(CstVecExpr p, CstVecExpr q) {
  return new CstVec2LogicExpr(p, q, CstCompareOp.NEQ);
}

