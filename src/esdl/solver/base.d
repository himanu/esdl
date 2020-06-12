module esdl.solver.base;
import esdl.rand.base;
import esdl.rand.expr;
import esdl.rand.misc;

abstract class CstSolver
{
  CstStage _stage;

  this(CstStage stage) {
    _stage = stage;
  }

  CstStage stage() {
    return _stage;
  }

  abstract void registerDomain(CstDomain domain);
  abstract void pushDomain(CstDomain domain);
  abstract void registerValue(CstValue value);
  abstract void pushValue(CstValue value);
  abstract void pushValue(bool value);
  // abstract void pushValue(ulong value, uint bitcount, bool signed);
  abstract void process(CstUnaryOp op);
  abstract void process(CstBinaryOp op);
  abstract void process(CstCompareOp op);
  abstract void process(CstLogicalOp op);
}

abstract class CstSolverDomain
{
  CstDomain _domain;
  
  this(CstDomain domain) {
    _domain = domain;
  }

  final CstDomain domain() {
    return _domain;
  }

}

abstract class CstSolverValue
{
  CstValue _value;
  CstDomain _domain;
  
  this(CstValue value) {
    _value = value;
  }

  this(CstDomain domain) {
    _domain = domain;
  }

  final CstValue value() {
    return _value;
  }

  final CstDomain domain() {
    return _domain;
  }
}
