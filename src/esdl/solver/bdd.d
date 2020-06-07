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

class Context
{
  BDD _bdd;
  Buddy _buddy;

  this(Buddy buddy) {
    _buddy = buddy;
    _bdd = _buddy.one();
  }

  BDD[] _bddStack;

  void push() {
    _bddStack ~= _bdd;
  }

  void pop() {
    _bdd = _bddStack[$-1];
    _bddStack.length = _bddStack.length - 1;
  }
}

struct BddTerm
{
  import std.conv: to;

  enum Type: ubyte { BOOLEXPR, BVEXPR, ULONG }

  BDD    _boolExpr;
  BddVec _bvExpr;
  ulong  _ulong;

  Type _type;

  ref BddVec toBv() {
    if (_type != Type.BVEXPR) assert(false, "Expected a BVEXPR, got "
				     ~ _type.to!string);
    return _bvExpr;
  }

  ref BDD toBool() {
    if (_type != Type.BOOLEXPR) assert(false, "Expected a BOOLEXPR, got "
				       ~ _type.to!string);
    return _boolExpr;
  }

  ulong toUlong() {
    if (_type != Type.ULONG) assert(false, "Expected a ULONG, got "
				    ~ _type.to!string);
    return _ulong;
  }

  // workaround for https://issues.dlang.org/show_bug.cgi?id=20876
  this(ref BddTerm other) {
    _boolExpr = other._boolExpr;
    _bvExpr = other._bvExpr;
    _ulong = other._ulong;
    _type = other._type;
  }

  this(ref BddVec expr) {
    _bvExpr = expr;
    _type = Type.BVEXPR;
  }

  this(BddVec expr) {
    _bvExpr = expr;
    _type = Type.BVEXPR;
  }

  this(ref BDD expr) {
    _boolExpr = expr;
    _type = Type.BOOLEXPR;
  }

  this(BDD expr) {
    _boolExpr = expr;
    _type = Type.BOOLEXPR;
  }

  this(ulong expr) {
    _ulong = expr;
    _type = Type.ULONG;
  }

}

struct BvVar
{
  enum State: ubyte {INIT, CONST, VAR}
  BddVec _dom;
  long   _val;
  State  _state;

  alias _dom this;
  
  this(BddVec dom) {
    _dom = dom;
    _val = 0;
    _state = State.INIT;
  }

  ref BvVar opAssign(ref BddVec dom) {
    _dom = dom;
    _val = 0;
    _state = State.INIT;
    return this;
  }

  BddVec getValExpr() {
    size_t size = _dom.length;
    return getBuddy().buildVec(size, _val, _dom.signed());
  }

  BDD getRule() {
    return _dom.equ(getValExpr());
  }
  
  void update(CstDomain dom, CstBddSolver solver) {
    assert (dom.isSolved());
    long val = dom.value();
    if (_val != val) {
      _val = val;
      final switch (_state) {
      case State.INIT:
	_state = State.CONST;
	solver._count0 += 1;
	break;
      case State.CONST:
	_state = State.VAR;
	solver._count0 -= 1;
	solver._count1 += 1;
	break;
      case State.VAR:
	solver._refreshVar = true;
	break;
      }
    }
    else {
      final switch (_state) {
      case State.INIT:
	_state = State.CONST;
	solver._count0 += 1;
	break;
      case State.CONST:
	break;
      case State.VAR:
	break;
      }
    }
  }
}

class CstBddSolver: CstSolver
{
  static Buddy _esdl__buddy;

  BddTerm[] _evalStack;

  BddVec[] _domains;

  BvVar[] _variables;

  Context _context;

  _esdl__Proxy _proxy;

  uint _count0;
  uint _count1;

  // whether some variables have changed and a refresh required
  bool _refreshVar;	    // whether the variable values need refresh
  ubyte _pushCount;	    // balance number of pushed z3 context has


  static Buddy buddy() {
    if (_esdl__buddy is null) {
      _esdl__buddy = new Buddy(400, 400);
    }
    return _esdl__buddy;
  }


  this(string signature, CstPredGroup group) {
    super(signature);

    _proxy = group.getProxy();

    if (_esdl__buddy is null) {
      _esdl__buddy = new Buddy(400, 400);
    }

    _context = new Context(_esdl__buddy);

    useBuddy(_esdl__buddy);

    CstDomain[] doms = group.domains();

    foreach (dom; doms) {
      _domains ~= _esdl__buddy.createDomVec(dom.bitcount, dom.signed());
    }
      
    // import std.stdio;
    // writeln("Adding Z3 Domain for @rand ", doms[i].name());
    // auto d = BvExpr(_context, format("_dom%s", i), doms[i].bitcount, doms[i].signed());
    // dom = d;

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
  override void pushToEvalStack(ulong value) { }

  override void processEvalStack(CstUnaryOp op) { }
  override void processEvalStack(CstBinaryOp op) { }
  override void processEvalStack(CstCompareOp op) { }
  override void processEvalStack(CstLogicalOp op) { }
  override void processEvalStack(CstSliceOp op) { }
}

