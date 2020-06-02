module esdl.solver.bdd;


import std.container: Array;
// import std.array;

import esdl.solver.base;
import esdl.rand.expr;
import esdl.rand.base;
import esdl.rand.misc;
import esdl.solver.obdd;

import std.algorithm.searching: canFind;

private import std.typetuple: staticIndexOf, TypeTuple;
private import std.traits: BaseClassesTuple; // required for staticIndexOf

T StaticCast(T, F)(const F from)
  if (is (F == class) && is (T == class)
     // make sure that F is indeed amongst the base classes of T
     && staticIndexOf!(F, BaseClassesTuple!T) != -1
     )
    in {
      // assert statement will not be compiled for production release
      assert((from is null) || cast(T)from !is null);
    }
body {
  return cast(T) cast(void*) from;
 }

struct BddTerm
{
  bool   _isVec;
  BDD    _bdd;
  BddVec _vec;

  this(BddVec vec) {
    // import std.stdio;
    // writeln("Adding a vec");
    _isVec = true;
    _vec = vec;
  }

  this(BDD bdd) {
    // import std.stdio;
    // writeln("Adding a bdd");
    _isVec = false;
    _bdd = bdd;
  }
}

class CstBddSolver: CstSolver
{
  static Buddy _esdl__buddy;

  static Buddy buddy() {
    if (_esdl__buddy is null) {
      _esdl__buddy = new Buddy(400, 400);
    }
    return _esdl__buddy;
  }

  Array!BddTerm _evalStack;


  this(string signature, CstPredGroup group) {
    super(signature);
    if (_esdl__buddy is null) {
      _esdl__buddy = new Buddy(400, 400);
    }
    // foreach (CstDomain; group._doms) {
    //   foreach (rnd; pred.getRnds) this.registerDomain(rnd);
    //   foreach (var; pred.getVars) this.registerDomain(var);
    //   foreach (val; pred.getVals) this.registerValue(val);
    // }
  }

  override void solve(CstPredGroup group) { }
  
  override void pushToEvalStack(CstDomain domain) { }
  override void pushToEvalStack(CstValue value) { }
  override void pushToEvalStack(bool value) { }

  override void processEvalStack(CstUnaryOp op) { }
  override void processEvalStack(CstBinaryOp op) { }
  override void processEvalStack(CstCompareOp op) { }
  override void processEvalStack(CstLogicalOp op) { }
}

