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
  CstEquation[] _bddEqns;
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
    _bddEqns = from._bddEqns;
    _solveBDD = from._solveBDD;
    _bddDist = from._bddDist;
  }

  // return true is _bddEqns match
  bool compare(CstStage other) {
    return other._bddEqns == _bddEqns;
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

// The agent keeps a list of clients that are dependent on the agent
interface CstAgent
{
  abstract void post(CstClient client);
}

// The client keeps a list of agents that when resolved makes the client happy
interface CstClient
{
  abstract void trigger(CstAgent agent);
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
  // converted into a BDD
  abstract CstVarIterBase[] itrVars();
  // get the primary (outermost foreach) iterator CstVarExpr
  abstract CstVarIterBase getIterator();
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

  abstract CstVarExpr unroll(CstVarIterBase itr, uint n);

  bool isOrderingExpr() {
    return false;		// only CstVecOrderingExpr return true
  }

  abstract void setBddContext(CstEquation eqn,
			      ref CstVarPrim[] vars,
			      ref CstVarIterBase iter,
			      ref CstVarPrim[] deps);
}


// This class represents an unwound Foreach itr at vec level
interface CstVarIterBase
{
  abstract uint maxVal();

  abstract bool isUnrollable();

  abstract string name();
}

abstract class CstBddExpr
{
  string name();

  abstract bool refresh(CstStage stage, Buddy buddy);
  
  abstract CstVarIterBase[] itrVars(); //  {
  abstract CstVarIterBase getIterator(); //  {

  abstract CstVarPrim[] preReqs();

  abstract bool hasUnresolvedIdx();
  abstract CstDomain[] unresolvedIdxs();

  abstract uint resolveLap();
  abstract void resolveLap(uint lap);

  abstract void setBddContext(CstEquation eqn,
			      ref CstVarPrim[] vars,
			      ref CstVarIterBase iter,
			      ref CstVarPrim[] deps);

  abstract CstBddExpr unroll(CstVarIterBase itr, uint n);

  abstract CstDomain[] getRndDomains(bool resolved);

  // abstract CstStage[] getStages();

  abstract BDD getBDD(CstStage stage, Buddy buddy);

  bool cstExprIsNop() {
    return false;
  }
}

class CstEquation
{
  string name() {
    return "EQN:" ~ _expr.name();
  }

  string describe() {
    string description = name() ~ "\n";
    if (_iter !is null) {
      description ~= "    iterator: \n";
      description ~= "\t" ~ _iter.name() ~ "\n";
    }
    if (_vars.length > 0) {
      description ~= "    variables: \n";
      foreach (var; _vars) {
	description ~= "\t" ~ var.name() ~ "\n";
      }
    }
    if (_deps.length > 0) {
      description ~= "    depends: \n";
      foreach (dep; _deps) {
	description ~= "\t" ~ dep.name() ~ "\n";
      }
    }
    description ~= "\n";
    return description;
  }
  // alias _expr this;

  CstBddExpr _expr;

  CstVarIterBase _uwItr;
  CstEquation[] _uwEqns;
  
  this(CstBddExpr expr) {
    _expr = expr;
    this.setBddContext();
    debug(CSTEQNS) {
      import std.stdio;
      stderr.writeln(this.describe());
    }
  }

  // unroll recursively untill no unrolling is possible
  void unroll(uint lap,
	      ref CstEquation[] unrollable,
	      ref CstEquation[] unresolved,
	      ref CstEquation[] resolved) {
    CstEquation[] unrolled;
    auto itr = this.unrollableItr();
    if (itr is null) {
      unrolled ~= this;
    }
    else {
      unrolled = this.unroll(itr);
    }

    if (_expr.hasUnresolvedIdx()) {
      // TBD -- check that we may need to call resolveLap on each of the unrolled expression
      foreach (expr; unrolled) {
	_expr.resolveLap(lap);
      }
      
      if (_expr.itrVars().length == 1) {
	unresolved ~= unrolled;
      }
      else {
	unresolved ~= unrolled;
      }
    }
    else {
      resolved ~= unrolled;
    }
  }

  CstVarIterBase unrollableItr() {
    foreach(itr; _expr.itrVars()) {
      if(itr.isUnrollable()) return itr;
    }
    return null;
  }

  CstEquation[] unroll(CstVarIterBase itr) {
    assert (itr is _expr.getIterator());

    if(! itr.isUnrollable()) {
      assert(false, "CstVarIterBase is not unrollabe yet");
    }
    auto currLen = itr.maxVal();
    // import std.stdio;
    // writeln("maxVal is ", currLen);

    if (_uwItr !is itr) {
      _uwItr = itr;
      _uwEqns.length = 0;
    }
    
    if (currLen > _uwEqns.length) {
      // import std.stdio;
      // writeln("Need to unroll ", currLen - _uwEqns.length, " times");
      for (uint i = cast(uint) _uwEqns.length;
	   i != currLen; ++i) {
	_uwEqns ~= new CstEquation(_expr.unroll(itr, i));
      }
    }
    
    return _uwEqns[0..currLen];
  }

  CstVarPrim[] _vars;
  CstVarPrim[] _deps;
  CstVarIterBase _iter;

  final void setBddContext() {
    _expr.setBddContext(this, _vars, _iter, _deps);
  }

  CstBddExpr getExpr() {
    return _expr;
  }
}

class CstBlock
{
  CstEquation[] _eqns;
  bool[] _booleans;

  bool refresh(CstStage stage, Buddy buddy) {
    bool result = false;
    foreach (eqn; _eqns) {
      result |= eqn.getExpr().refresh(stage, buddy);
    }
    return result;
  }
  
  string name() {
    string name_ = "";
    foreach(eqn; _eqns) {
      name_ ~= " & " ~ eqn.name() ~ "\n";
    }
    return name_;
  }

  void _esdl__reset() {
    _eqns.length = 0;
  }

  bool isEmpty() {
    return _eqns.length == 0;
  }
  
  void opOpAssign(string op)(bool other)
    if(op == "~") {
      _booleans ~= other;
    }

  void opOpAssign(string op)(CstEquation other)
    if(op == "~") {
      _eqns ~= other;
    }

  void opOpAssign(string op)(CstBlock other)
    if(op == "~") {
      if(other is null) return;
      foreach(eqn; other._eqns) {
	_eqns ~= eqn;
      }
      foreach(boolean; other._booleans) {
	_booleans ~= boolean;
      }
    }

}
