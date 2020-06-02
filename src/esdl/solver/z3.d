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

import std.algorithm.searching: canFind;

private import std.typetuple: staticIndexOf, TypeTuple;
private import std.traits: BaseClassesTuple; // required for staticIndexOf


struct Z3Term
{
  enum Type: ubyte { BOOL, BV }

  BoolExpr _bool;
  BvExpr _bv;

  Type _type;

  bool opEquals()(const ref Z3Term other) const {
    // this is placeholder so that Array!Z3Term can compile
    assert (false);
  }

  ref BvExpr toBv() {
    if (_type == Type.BOOL) assert(false, "Expected a BV expr, got BOOL");
    return _bv;
  }

  ref BoolExpr toBool() {
    if (_type == Type.BV) assert(false, "Expected a BOOL expr, got BV");
    return _bool;
  }

  // workaround for https://issues.dlang.org/show_bug.cgi?id=20876
  this(ref Z3Term other) {
    _bool = other._bool;
    _bv = other._bv;
    _bool = other._bool;
    _type = other._type;
  }

  this(ref BvExpr expr) {
    _bv = expr;
    _type = Type.BV;
  }

  this(BvExpr expr) {
    _bv = expr;
    _type = Type.BV;
  }

  this(ref BoolExpr expr) {
    _bool = expr;
    _type = Type.BOOL;
  }

  this(BoolExpr expr) {
    _bool = expr;
    _type = Type.BOOL;
  }

}

struct BvVar
{
  enum State: ubyte {INIT, CONST, VAR, VARCHANGED, CONSTCHANGED}
  BvExpr _dom;
  ulong  _val;
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

  State update(CstDomain dom) {
    assert (dom.isSolved());
    ulong val = dom.value();
    if (_val != val) {
      final switch (_state) {
      case State.INIT: _state = State.CONST; break;
      case State.CONST: _state = State.CONSTCHANGED; break;
      case State.VAR: _state = State.VARCHANGED; break;
      case State.VARCHANGED: _state = State.VARCHANGED; break;
      case State.CONSTCHANGED: _state = State.VARCHANGED; break;
      }
      _val = val;
    }
    else {
      final switch (_state) {
      case State.INIT: _state = State.CONST; break;
      case State.CONST: _state = State.CONST; break;
      case State.VAR: _state = State.VAR; break;
      case State.VARCHANGED: _state = State.VAR; break;
      case State.CONSTCHANGED: _state = State.VAR; break;
      }
    }
    return _state;
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

  // whether a push is required to make
  bool push0;
  bool push1;

  // true if we need to pop SMT
  bool pop0;
  bool pop1;

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
      import std.stdio;
      writeln("Adding Z3 Domain for @rand ", doms[i].name());
      auto d = BvExpr(_context, format("_dom%s", i), doms[i].bitcount, doms[i].signed());
      dom = d;
    }

    CstDomain[] vars = group.variables();
    _variables.length = vars.length;

    foreach (i, ref var; _variables) {
      import std.string: format;
      import std.stdio;
      writeln("Adding Z3 Domain for variable ", vars[i].name());
      auto d = BvExpr(_context, format("_var%s", i), vars[i].bitcount, vars[i].signed());
      var = d;
      var.update(vars[i]);
    }

    Solver s = Solver(_context);
    _solver = s;

    foreach (pred; group.predicates()) {
      import std.stdio;
      writeln("Working on: ", pred.name());
      if (pred.group() !is group) {
	writeln (pred.name(), " Group Violation");
      }
      pred.visit(this);
      assert(_evalStack.length == 1);
      addRule(_solver, _evalStack[0].toBool());
      _evalStack.length = 0;
    }


    _solver.push();

    // writeln("auto_config: ", getParam("auto_config"));
    // writeln("smt.phase_selection: ", getParam("smt.phase_selection"));
    // writeln("smt.arith.random_initial_value: ", getParam("smt.arith.random_initial_value"));
    // writeln("smt.random_seed: ", getParam("smt.random_seed"));
    // writeln("sat.phase: ", getParam("sat.phase"));
    // writeln("sat.random_seed: ", getParam("sat.random_seed"));
  }

  BvVar.State varState;

  override void solve(CstPredGroup group) {
    CstDomain[] doms = group.domains();
    // writeln(_solver);
    // writeln(_solver.check());
    // writeln(_solver.getModel());

    _solver.check();
    auto model = _solver.getModel();
    foreach (i, ref dom; _domains) {
      import std.string: format;
      string value;
      BvExpr vdom = dom.mapTo(model, true);
      writeln("Value for Domain ", doms[i].name(), ": ",
	      vdom.getNumeralInt64());
      // writeln(vdom.getNumeralInt64());
      // vdom.isNumeral(value);
      // writeln(value);
    }

    CstDomain[] vars = group.variables();
    
    BvVar.State newState = BvVar.State.INIT;
    foreach (i, ref var; _variables) {
      BvVar.State state = var.update(vars[i]);
      if (state > newState) newState = state;
    }

    import std.stdio;
    writeln ("Variables state: ", varState, " new: ", newState);
    
    varState = newState;
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

  override void pushToEvalStack(bool value) {
    // writeln("push: ", value);
    assert(false);
  }

  void pop() {
    _evalStack.length -= 1;
  }

  override void processEvalStack(CstUnaryOp op) {
    // writeln("eval: CstUnaryOp ", op);
    final switch (op) {
    case CstUnaryOp.NOT:
      BvExpr e = compliment(_evalStack[$-1].toBv());
      _evalStack.length -= 1;
      _evalStack ~= Z3Term(e);
      break;
    case CstUnaryOp.NEG:
      BvExpr e = neg(_evalStack[$-1].toBv());
      _evalStack.length -= 1;
      _evalStack ~= Z3Term(e);
      break;
    }
  }

  override void processEvalStack(CstBinaryOp op) {
    // writeln("eval: CstBinaryOp ", op);
    final switch (op) {
    case CstBinaryOp.AND:
      BvExpr e = bvand(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length -= 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstBinaryOp.OR:
      BvExpr e = bvor(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length -= 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstBinaryOp.XOR:
      BvExpr e = bvxor(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length -= 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstBinaryOp.ADD:
      BvExpr e = add(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length -= 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstBinaryOp.SUB:
      BvExpr e = sub(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length -= 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstBinaryOp.MUL:
      BvExpr e = mul(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length -= 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstBinaryOp.DIV:
      BvExpr e = div(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length -= 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstBinaryOp.REM:
      BvExpr e = rem(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length -= 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstBinaryOp.LSH:
      BvExpr e = lsh(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length -= 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstBinaryOp.RSH:			// Arith shift right ">>"
      BvExpr e = rsh(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length -= 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstBinaryOp.LRSH:			// Logic shift right ">>>"
      BvExpr e = lrsh(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length -= 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstBinaryOp.RANGE: 			// for bitvec slice
      assert(false);
    case CstBinaryOp.BITINDEX:
      assert(false);
    }
  }

  override void processEvalStack(CstCompareOp op) {
    // writeln("eval: CstCompareOp ", op);
    final switch (op) {
    case CstCompareOp.LTH:
      BoolExpr e = lt(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length -= 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstCompareOp.LTE:
      BoolExpr e = le(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length -= 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstCompareOp.GTH:
      BoolExpr e = gt(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length -= 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstCompareOp.GTE:
      BoolExpr e = ge(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length -= 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstCompareOp.EQU:
      BoolExpr e = eq(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length -= 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstCompareOp.NEQ:
      BoolExpr e = neq(_evalStack[$-2].toBv(), _evalStack[$-1].toBv());
      _evalStack.length -= 2;
      _evalStack ~= Z3Term(e);
      break;
    }
  }

  override void processEvalStack(CstLogicalOp op) {
    // writeln("eval: CstLogicalOp ", op);
    final switch (op) {
    case CstLogicalOp.LOGICAND:
      BoolExpr e = and(_evalStack[$-2].toBool(), _evalStack[$-1].toBool());
      _evalStack.length -= 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstLogicalOp.LOGICOR:
      BoolExpr e = or(_evalStack[$-2].toBool(), _evalStack[$-1].toBool());
      _evalStack.length -= 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstLogicalOp.LOGICIMP:
      BoolExpr e = implies(_evalStack[$-2].toBool(), _evalStack[$-1].toBool());
      _evalStack.length -= 2;
      _evalStack ~= Z3Term(e);
      break;
    case CstLogicalOp.LOGICNOT:
      BoolExpr e = not(_evalStack[$-1].toBool());
      _evalStack.length -= 1;
      _evalStack ~= Z3Term(e);
      break;
    }
  }
}

