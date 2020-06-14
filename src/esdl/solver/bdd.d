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
    import std.stdio;
    writeln("Adding a bdd");
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

  BddTerm[] _evalStack;

  // Array!uint _domains;		// indexes of domains
  CstDomain[] _cstRnds;
  CstDomain[] _cstVars;

  // Array!CstValue  _cstVals;

  this(CstStage stage) {
    super(stage);
    if (_esdl__buddy is null) {
      _esdl__buddy = new Buddy(400, 400);
    }
    foreach (pred; stage._predicates) {
      foreach (rnd; pred.getRnds) this.registerDomain(rnd);
      foreach (var; pred.getVars) this.registerDomain(var);
      foreach (val; pred.getVals) this.registerValue(val);
    }
  }

  override void registerDomain(CstDomain domain) {
    if (domain.isRand()) {
      if (! canFind(_cstRnds[], domain)) {
	_cstRnds ~= domain;
	useBuddy(_esdl__buddy);
	if (domain._solverDomain is null) {
	  uint domIndex = _esdl__buddy.addDomVec(domain.bitcount, domain.signed);
	  domain._solverDomain = new CstBddSolverDomain(this, domain, domIndex);
	}
      }
    }
    else {
      if (! canFind(_cstVars[], domain)) {
	_cstVars ~= domain;
	auto solverValue = new CstBddSolverValue(this, domain);
	solverValue._valvec.buildVec(_esdl__buddy, domain.value,
				     domain.bitcount, domain.signed);
	domain._solverValue = solverValue;
      }
    }
  }


  override void registerValue(CstValue value) {
    // import std.stdio;
    // writeln("registering value: ", value.name());
    useBuddy(_esdl__buddy);
    if (value._solverValue is null) {
      auto solverValue = new CstBddSolverValue(this, value);
      solverValue._valvec.buildVec(_esdl__buddy, value.value,
				   value.bitcount, value.signed);
      value._solverValue = solverValue;
    }
  }

  override void pushDomain(CstDomain domain) {
    if (domain.isRand()) {
      assert (domain._solverDomain !is null);
      _evalStack ~= BddTerm((cast(CstBddSolverDomain) domain._solverDomain).getBdd());
    }
    else {
      assert (domain._solverValue !is null);
      _evalStack ~= BddTerm((cast(CstBddSolverValue) domain._solverValue).getBdd());
    }
  }

  override void pushValue(CstValue value) {
    assert (value._solverValue !is null);
    _evalStack ~= BddTerm((cast(CstBddSolverValue) value._solverValue).getBdd());
  }

  override void pushValue(bool value) {
    if (value is true)
      _evalStack ~= BddTerm(_esdl__buddy.one());
    else
      _evalStack ~= BddTerm(_esdl__buddy.zero());
  }

  override void process(CstUnaryOp op) {
  }

  override void process(CstBinaryOp op) {
  }

  override void process(CstCompareOp op) {
  }

  override void process(CstLogicalOp op) {
  }
}

class CstBddSolverDomain: CstSolverDomain
{
  uint _index;
  BddVec _valvec;
  CstBddSolver _solver;

  this(CstBddSolver solver, CstDomain domain, uint index) {
    super(domain);
    _solver = solver;
    _index = index;
  }

  ~this() {
    _valvec.free();
  }

  final ref BddVec getBdd() {
    if (domain.stage() is null) {
      assert(false, domain.name());
    }
    if (domain.isRand()) {
      import std.stdio;
      if (domain.stage() is _solver._stage) {
	writeln("Solving: ", domain.name());
	return CstBddSolver.buddy.getVec(_index);
      }
      else if (domain.stage().isSolved()) {
	writeln("Solved: ", domain.name());
	_valvec.buildVec(CstBddSolver.buddy(), domain.value(),
			 domain.bitcount, domain.signed);
      }
      else {
	assert(false, domain.name());
      }
    }
    else {
      if (domain.hasChanged()) {
	_valvec.buildVec(CstBddSolver.buddy(), domain.value(),
			 domain.bitcount, domain.signed);
      }
    }
    return _valvec;
  }
}

class CstBddSolverValue: CstSolverValue
{
  BddVec _valvec;
  CstBddSolver _solver;

  this(CstBddSolver solver, CstValue value) {
    super(value);
    _solver = solver;
  }

  this(CstBddSolver solver, CstDomain domain) {
    super(domain);
    _solver = solver;
  }

  ~this() {
    _valvec.free();
  }

  final ref BddVec getBdd() {
    if (_valvec.isNull) {
      if (value() is null) {
      _valvec.buildVec(CstBddSolver.buddy(), domain.value(),
		       domain.bitcount, domain.signed);
      }
      else {
	_valvec.buildVec(CstBddSolver.buddy(), value.value(),
			 value.bitcount, value.signed);
      }
    }
    return _valvec;
  }
}
