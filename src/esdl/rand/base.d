module esdl.rand.base;

import esdl.rand.obdd;
import esdl.rand.misc: _esdl__RandGen, _esdl__norand, isVecSigned;
import esdl.data.bvec: isBitVector;
import std.traits: isIntegral, isBoolean, isArray, isStaticArray, isDynamicArray;

class CstStage {
  // List of randomized variables associated with this stage. A
  // variable can be associated with only one stage
  CstDomain[] _domVars;
  // The Bdd expressions that apply to this stage
  CstBddExpr[] _bddExprs;
  // These are the length variables that this stage will solve
  // CstVarPrim[] _preReqs;
  
  BDD _solveBDD;

  double[uint] _bddDist;
  
  int _id = -1;

  ~this() {
    _solveBDD.reset();
  }
  
  void copyFrom(CstStage from) {
    _domVars = from._domVars;
    _bddExprs = from._bddExprs;
    _solveBDD = from._solveBDD;
    _bddDist = from._bddDist;
  }

  // return true is _bddExprs match
  bool compare(CstStage other) {
    return other._bddExprs == _bddExprs;
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


interface CstDomain
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
  abstract BDD getPrimBdd(Buddy buddy);
  abstract CstBddExpr getNopBddExpr();
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
}

interface CstVarPrim
{
  abstract string name();
  abstract void _esdl__doRandomize(_esdl__RandGen randGen);
  abstract void _esdl__doRandomize(_esdl__RandGen randGen, CstStage stage);
  abstract bool isRand();
  // abstract ulong value();

  abstract void _esdl__reset();
  abstract bool isVarArr();
  abstract CstDomain[] getDomainLens(bool resolved);
  abstract void solveBefore(CstVarPrim other);
  abstract void addPreRequisite(CstVarPrim other);

  // this method is used for getting implicit constraints that are required for
  // dynamic arrays and for enums
  abstract BDD getPrimBdd(Buddy buddy);
  abstract void resetPrimeBdd();
}


abstract class CstVarExpr
{

  // alias evaluate this;

  abstract string name();
  
  // Array of indexes this expression has to resolve before it can be
  // convertted into an BDD
  abstract CstVarIterBase[] itrVars();
  abstract bool hasUnresolvedIdx();
  abstract CstDomain[] unresolvedIdxs();

  abstract uint resolveLap();
  abstract void resolveLap(uint lap);
  
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
  abstract CstDomain[] getRndDomains(bool resolved);

  // get the list of stages this expression should be avaluated in
  // abstract CstStage[] getStages();
  abstract BddVec getBDD(CstStage stage, Buddy buddy);

  // refresh the _valvec if the current value is not the same as previous value
  abstract bool refresh(CstStage stage, Buddy buddy);

  abstract long evaluate();

  abstract CstVarExpr unwind(CstVarIterBase itr, uint n);

  bool isOrderingExpr() {
    return false;		// only CstVecOrderingExpr return true
  }

}


// This class represents an unwound Foreach itr at vec level
interface CstVarIterBase
{
  abstract uint maxVal();

  abstract bool isUnwindable();


}

abstract class CstBddExpr
{
  string name();

  abstract bool refresh(CstStage stage, Buddy buddy);
  
  abstract CstVarIterBase[] itrVars(); //  {

  abstract CstVarPrim[] preReqs();

  abstract bool hasUnresolvedIdx();
  abstract CstDomain[] unresolvedIdxs();

  abstract uint resolveLap();
  abstract void resolveLap(uint lap);

  CstBddExpr[] _uwExprs;
  CstVarIterBase _uwItr;
  
  // unwind recursively untill no unwinding is possible
  CstBddExpr[] unwind() {
    CstBddExpr[] unwound;
    auto itr = this.unwindableItr();
    if(itr is null) {
      return [this];
    }
    else {
      foreach(expr; this.unwind(itr)) {
	// import std.stdio;
	// writeln(this.name(), " unwound expr: ", expr.name());
	// writeln(expr.name());
	if(expr.unwindableItr() is null) unwound ~= expr;
	else unwound ~= expr.unwind();
      }
    }
    return unwound;
  }

  CstBddExpr[] unwind(CstVarIterBase itr) {
    if(! itr.isUnwindable()) {
      assert(false, "CstVarIterBase is not unwindabe yet");
    }
    auto currLen = itr.maxVal();
    // import std.stdio;
    // writeln("maxVal is ", currLen);

    if (_uwItr !is itr) {
      _uwItr = itr;
      _uwExprs.length = 0;
    }
    
    if (currLen > _uwExprs.length) {
      // import std.stdio;
      // writeln("Need to unwind ", currLen - _uwExprs.length, " times");
      for (uint i = cast(uint) _uwExprs.length;
	   i != currLen; ++i) {
	_uwExprs ~= this.unwind(itr, i);
      }
    }
    
    return _uwExprs[0..currLen];
  }

  CstVarIterBase unwindableItr() {
    foreach(itr; itrVars()) {
      if(itr.isUnwindable()) return itr;
    }
    return null;
  }

  abstract CstBddExpr unwind(CstVarIterBase itr, uint n);

  abstract CstDomain[] getRndDomains(bool resolved);

  // abstract CstStage[] getStages();

  abstract BDD getBDD(CstStage stage, Buddy buddy);

  bool cstExprIsNop() {
    return false;
  }
}

class CstBlock: CstBddExpr
{
  CstBddExpr[] _exprs;
  bool[] _booleans;

  // CstVarIterBase[] _itrVars;

  override CstVarIterBase[] itrVars() {
    assert(false, "itrVars() is not implemented for CstBlock");
    // return _itrVars;
  }

  override bool hasUnresolvedIdx() {
    assert(false, "hasUnresolvedIdx() is not implemented for CstBlock");
  }
  
  override CstDomain[] unresolvedIdxs() {
    assert(false, "unresolvedIdxs() is not implemented for CstBlock");
  }
  
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
  
  override CstDomain[] getRndDomains(bool resolved) {
    assert(false);
  }

  override CstBlock unwind(CstVarIterBase itr, uint n) {
    assert(false, "Can not unwind a CstBlock");
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
      _exprs ~= toBdd(other);
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

  override uint resolveLap() {
    assert(false, "resolveLap not callable for CstBlock");
  }
  override void resolveLap(uint lap) {
    assert(false, "resolveLap not callable for CstBlock");
  }
}
