module esdl.solver.z3;
import std.stdio;


import std.container: Array;
// import std.array;

import esdl.solver.base;
import esdl.solver.z3expr;
import esdl.rand.expr;
import esdl.rand.base;
import esdl.rand.misc;
import esdl.intf.z3.z3;
import esdl.intf.z3.api.z3_types: Z3_ast;
import esdl.intf.z3.api.z3_api: Z3_mk_int64, Z3_mk_unsigned_int64;

import std.algorithm.searching: canFind;

private import std.typetuple: staticIndexOf, TypeTuple;
private import std.traits: BaseClassesTuple; // required for staticIndexOf


struct Z3Term
{
  import std.conv: to;
  
  enum Type: ubyte { BOOLEXPR, BVEXPR, ULONG }

  BoolExpr _boolExpr;
  BvExpr _bvExpr;
  ulong   _ulong;

  Type _type;

  bool opEquals()(const ref Z3Term other) const {
    // this is placeholder so that Array!Z3Term can compile
    assert (false);
  }

  ref BvExpr toBv() {
    if (_type != Type.BVEXPR) assert(false, "Expected a BVEXPR, got "
				     ~ _type.to!string);
    return _bvExpr;
  }

  ref BoolExpr toBool() {
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
  this(ref Z3Term other) {
    _boolExpr = other._boolExpr;
    _bvExpr = other._bvExpr;
    _ulong = other._ulong;
    _type = other._type;
  }

  this(ref BvExpr expr) {
    _bvExpr = expr;
    _type = Type.BVEXPR;
  }

  this(BvExpr expr) {
    _bvExpr = expr;
    _type = Type.BVEXPR;
  }

  this(ref BoolExpr expr) {
    _boolExpr = expr;
    _type = Type.BOOLEXPR;
  }

  this(BoolExpr expr) {
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
  BvExpr _dom;
  long   _val;
  State  _state;

  alias _dom this;
  
  this(BvExpr dom) {
    _dom = dom;
    _val = 0;
    _state = State.INIT;
  }

  ref BvVar opAssign(ref BvExpr dom) {
    assert (_dom.isNull());
    _dom = dom;
    _val = 0;
    _state = State.INIT;
    return this;
  }

  BvExpr getValExpr() {
    Sort sort = _dom.getSort();
    Context context = _dom.context();
    Z3_ast r;
    if (_dom.isSigned()) r = Z3_mk_int64(context, _val, sort);
    else        r = Z3_mk_unsigned_int64(context, _val, sort);
    return BvExpr(context, r, _dom.isSigned());
  }

  BoolExpr getRule() {
    return eq(_dom, getValExpr());
  }
  
  void update(CstDomain dom, CstZ3Solver solver) {
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

class CstZ3Solver: CstSolver
{
  
  Z3Term[] _evalStack;

  BvExpr[] _domains;
  BvVar[] _variables;

  Context _context;

  Solver _solver;

  _esdl__Proxy _proxy;

  uint _count0;
  uint _count1;

  // whether some variables have changed and a refresh required
  bool _refreshVar;	    // whether the variable values need refresh
  ubyte _pushCount;	    // balance number of pushed z3 context has

  // the group is used only for the purpose of constructing the Z3 solver
  // otherwise the solver identifies with the signature only
  this(string signature, CstPredGroup group) {
    import std.stdio;
    super(signature);

    _proxy = group.getProxy();

    setParam("auto_config", false);
    setParam("smt.phase_selection", 5);
    Config cfg = new Config();
    _context = new Context(cfg);

    CstDomain[] doms = group.domains();
    _domains.length = doms.length;

    foreach (i, ref dom; _domains) {
      import std.string: format;
      // import std.stdio;
      // writeln("Adding Z3 Domain for @rand ", doms[i].name());
      auto d = BvExpr(_context, format("_dom%s", i), doms[i].bitcount, doms[i].signed());
      dom = d;
    }

    CstDomain[] vars = group.variables();
    _variables.length = vars.length;

    foreach (i, ref var; _variables) {
      import std.string: format;
      // import std.stdio;
      // writeln("Adding Z3 Domain for variable ", vars[i].name());
      auto d = BvExpr(_context, format("_var%s", i), vars[i].bitcount, vars[i].signed());
      var = d;
    }

    Solver s = Solver(_context);
    _solver = s;

    foreach (pred; group.predicates()) {
      // import std.stdio;
      // writeln("Working on: ", pred.name());
      if (pred.group() !is group) {
	assert (false, "Group Violation " ~ pred.name());
      }
      pred.visit(this);
      assert(_evalStack.length == 1);
      uint softWeight = pred.getSoftWeight();
      if (softWeight == 0) {
	addRule(_solver, _evalStack[0].toBool());
      }
      else {
	addRule(_solver, _evalStack[0].toBool(), softWeight);
      }
      _evalStack.length = 0;
    }


    this.push();

    // writeln("auto_config: ", getParam("auto_config"));
    // writeln("smt.phase_selection: ", getParam("smt.phase_selection"));
    // writeln("smt.arith.random_initial_value: ", getParam("smt.arith.random_initial_value"));
    // writeln("smt.random_seed: ", getParam("smt.random_seed"));
    // writeln("sat.phase: ", getParam("sat.phase"));
    // writeln("sat.random_seed: ", getParam("sat.random_seed"));
  }

  BvVar.State varState;

  void push() {
    assert(_pushCount <= 2);
    _pushCount += 1;
    _solver.push();
  }

  void pop() {
    assert(_pushCount >= 0);
    _pushCount -= 1;
    _solver.pop();
  }

  override void solve(CstPredGroup group) {
    updateVars(group);

    CstDomain[] doms = group.domains();

    debug (Z3SOLVER) {
      import std.stdio;
      writeln(_solver);
    }
    // writeln(_solver.check());
    // writeln(_solver.getModel());

    _solver.check();
    auto model = _solver.getModel();
    foreach (i, ref dom; _domains) {
      // import std.string: format;
      // string value;
      BvExpr vdom = dom.mapTo(model, true);
      ulong vlong = vdom.getNumeralInt64();
      doms[i].setVal(vlong);
      // writeln("Value for Domain ", doms[i].name(), ": ",
      // 	      vdom.getNumeralInt64());
      // writeln(vdom.getNumeralInt64());
      // vdom.isNumeral(value);
      // writeln(value);
    }

  }

  void updateVars(CstPredGroup group) {
    CstDomain[] vars = group.variables();
    _refreshVar = false;
    uint pcount0 = _count0;
    uint pcount1 = _count1;
    foreach (i, ref var; _variables) var.update(vars[i], this);
    assert (_count0 + _count1 == _variables.length);
    // import std.stdio;
    // writeln("refresh: ", _refreshVar,
    // 	    " prev counts: ", pcount0, ", ", pcount1,
    // 	    " now counts: ", _count0, ", ", _count1);

    if (_refreshVar || (pcount1 != 0 && pcount1 != _count1))
      pop();			// for variables
    if (pcount0 != 0 && pcount0 != _count0)
      pop();			// for constants

    // process constants -- if required
    if (pcount0 != _count0 && _count0 > 0) {
      push();
      foreach (i, ref var; _variables) {
	if (var._state == BvVar.State.CONST)
	  addRule(_solver, var.getRule());
      }
    }
    if (_refreshVar || pcount1 != _count1) {
      push();
      foreach (i, ref var; _variables) {
	if (var._state == BvVar.State.VAR)
	  addRule(_solver, var.getRule());
      }
    }
  }

  override void pushToEvalStack(CstDomain domain) {
    uint n = domain.annotation();
    // writeln("push: ", domain.name(), " annotation: ", n);
    // writeln("_domains has a length: ", _domains.length);
    if (domain.isSolved()) { // is a variable
      _evalStack ~= Z3Term(_variables[n]);
    }
    else {
      _evalStack ~= Z3Term(_domains[n]);
    }
  }

  override void pushToEvalStack(CstValue value) {
    // writeln("push: value ", value.value());
    _evalStack ~= Z3Term(bvNumVal(_context, value.value(), value.bitcount()));
  }

  override void pushToEvalStack(ulong value) {
    // writeln("push: ", value);
    _evalStack ~= Z3Term(value);
  }

  override void pushToEvalStack(bool value) {
    // writeln("push: ", value);
    assert(false);
  }

  override void processEvalStack(CstUnaryOp op) {
    // writeln("eval: CstUnaryOp ", op);
    final switch (op) {
    case CstUnaryOp.NOT:
      BvExpr e = compliment(_evalStack[$-1].toBv());
      _evalStack.length = _evalStack.length - 1;
      _evalStack ~= Z3Term(e);
      break;
    case CstUnaryOp.NEG:
      BvExpr e = neg(_evalStack[$-1].toBv());
      _evalStack.length = _evalStack.length - 1;
      _evalStack ~= Z3Term(e);
      break;
    }
  }

  override void processEvalStack(CstBinaryOp op) {
    // writeln("eval: CstBinaryOp ", op);
    final switch (op) {
    case CstBinaryOp.AND:
      BvExpr e = bvand(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length = _evalStack.length - 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstBinaryOp.OR:
      BvExpr e = bvor(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length = _evalStack.length - 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstBinaryOp.XOR:
      BvExpr e = bvxor(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length = _evalStack.length - 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstBinaryOp.ADD:
      BvExpr e = add(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length = _evalStack.length - 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstBinaryOp.SUB:
      BvExpr e = sub(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length = _evalStack.length - 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstBinaryOp.MUL:
      BvExpr e = mul(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length = _evalStack.length - 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstBinaryOp.DIV:
      BvExpr e = div(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length = _evalStack.length - 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstBinaryOp.REM:
      BvExpr e = rem(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length = _evalStack.length - 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstBinaryOp.LSH:
      BvExpr e = lsh(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length = _evalStack.length - 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstBinaryOp.RSH:			// Arith shift right ">>"
      BvExpr e = rsh(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length = _evalStack.length - 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstBinaryOp.LRSH:			// Logic shift right ">>>"
      BvExpr e = lrsh(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length = _evalStack.length - 2;
      _evalStack ~= Z3Term(e);
      break;
    }
  }

  override void processEvalStack(CstCompareOp op) {
    // writeln("eval: CstCompareOp ", op);
    final switch (op) {
    case CstCompareOp.LTH:
      BoolExpr e = lt(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length = _evalStack.length - 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstCompareOp.LTE:
      BoolExpr e = le(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length = _evalStack.length - 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstCompareOp.GTH:
      BoolExpr e = gt(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length = _evalStack.length - 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstCompareOp.GTE:
      BoolExpr e = ge(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length = _evalStack.length - 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstCompareOp.EQU:
      BoolExpr e = eq(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length = _evalStack.length - 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstCompareOp.NEQ:
      BoolExpr e = neq(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length = _evalStack.length - 2;
      _evalStack ~= Z3Term(e);
      break;
    }
  }

  override void processEvalStack(CstLogicOp op) {
    // writeln("eval: CstLogicOp ", op);
    final switch (op) {
    case CstLogicOp.LOGICAND:
      BoolExpr e = and(_evalStack[$-2].toBool(), _evalStack[$-1].toBool());
      _evalStack.length = _evalStack.length - 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstLogicOp.LOGICOR:
      BoolExpr e = or(_evalStack[$-2].toBool(), _evalStack[$-1].toBool());
      _evalStack.length = _evalStack.length - 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstLogicOp.LOGICIMP:
      BoolExpr e = implies(_evalStack[$-2].toBool(), _evalStack[$-1].toBool());
      _evalStack.length = _evalStack.length - 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstLogicOp.LOGICNOT:
      BoolExpr e = not(_evalStack[$-1].toBool());
      _evalStack.length = _evalStack.length - 1;
      _evalStack ~= Z3Term(e);
      break;
    }
  }

  override void processEvalStack(CstSliceOp op) {
    assert (op == CstSliceOp.SLICE);
    BvExpr e = _evalStack[$-3].toBv().extract(cast(uint) _evalStack[$-1].toUlong() - 1,
					      cast(uint) _evalStack[$-2].toUlong());
    _evalStack.length = _evalStack.length - 3;
    _evalStack ~= Z3Term(e);
  }
  
}

