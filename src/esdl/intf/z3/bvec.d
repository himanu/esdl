module esdl.intf.z3.bvec;

import esdl.intf.z3.api;
import esdl.intf.z3.z3;
import std.string: toStringz;
import core.stdc.string: strlen;

import std.traits: isIntegral, isBoolean;
import std.stdio;

alias BvExprVector = AstVectorTpl!BvExpr;
alias BoolExprVector = AstVectorTpl!BoolExpr;

struct BvExpr
{
  mixin RvalueRef;
  
  AST _ast;
  bool _signed;

  this(Context c, string name, uint size, bool signed=true) {
    _signed = signed;
    Z3_ast r = Z3_mk_const(c, c.strSymbol(name), c.bvSort(size));
    c.checkError();
    _ast = AST(c, r);
  }
  
  this(Context c, Z3_ast n, bool signed=true) {
    _signed = signed;
    _ast = AST(c, n);
  }

  this(ref return scope BvExpr rhs) {
    _signed = rhs._signed;
    _ast = rhs._ast;
  }

  BvExpr opAssign(ref return scope BvExpr rhs) {
    _ast = rhs._ast;
    _signed = rhs._signed;
    return this;
  }


  void setContext(Context ctx) {
    _ast.setContext(ctx);
  }
  Context context() {
    return _ast.context();
  }
  Z3_error_code checkError()  {
    return _ast.checkError();
  }
  // alias _ast this;

  T opCast(t)() if (is (T == Z3_ast)) {
    return _ast.m_ast;
  }
  Z3_ast getAST() {
    return _ast.m_ast;
  }
  
  alias getAST this;

  Z3_ast_kind kind() {
    return _ast.kind();
  }

  Z3_ast opCast(T)() if (is (T == Z3_ast)) {
    return _ast._m_ast;
  }
  
  /**
     \brief Return the sort of this expression.
  */
  Sort getSort() {
    Z3_sort s = Z3_get_sort(context(), _ast.m_ast);
    checkError();
    return Sort(context(), s);
  }

  /**
     \brief Return true if this expression is an application.
  */
  bool isApp() {
    return kind() == Z3_ast_kind.Z3_APP_AST || kind() == Z3_ast_kind.Z3_NUMERAL_AST;
  }
  /**
     \brief Return true if this expression is a constant (i.e., an application with 0 arguments).
  */
  bool isConst() {
    return isApp() && numArgs() == 0;
  }
  /**
     \brief Return true if this expression is a lambda expression.
  */
  bool isLambda() {
    return Z3_is_lambda(context(), getAST);
  }
  /**

     \brief Return true if this expression is a variable.
  */
  bool isVar() {
    return kind() == Z3_ast_kind.Z3_VAR_AST;
  }
  /**
     \brief Return true if expression is an algebraic number.
  */
  bool isAlgebraic() {
    return Z3_is_algebraic_number(context(), getAST);
  }

  /**
     \brief Return true if this expression is well sorted (aka type correct).
  */
  bool isWellSorted() {
    bool r = Z3_is_well_sorted(context(), getAST);
    checkError();
    return r;
  }

  /**
     \brief Return string representation of numeral or algebraic number
     This method assumes the expression is numeral or algebraic

     \pre is_numeral() || is_algebraic()
  */
  string get_decimal_string(int precision) {
    auto r = (Z3_get_numeral_decimal_string(context(), getAST, precision));
    return cast(string) r[0..r.strlen];
  }
        

  /**
     \brief retrieve unique identifier for expression.
  */
  uint id() {
    uint r = Z3_get_ast_id(context(), getAST);
    checkError();
    return r;
  }

  /**
     \brief Return true if this expression is a string literal. 
     The string can be accessed using \c get_string() and \c get_escaped_string()
  */
  bool isStringValue() {
    return Z3_is_string(context(), getAST);
  }

  /**
     \brief for a string value expression return an escaped or unescaped string value.
     \pre expression is for a string value.
  */

  string getEscapedString() {            
    assert (isStringValue());
    auto s = Z3_get_string(context(), getAST);
    checkError();
    return cast(string) s[0..s.strlen];
  }

  string getString() {
    assert (isStringValue());
    uint n;
    auto s = Z3_get_lstring(context(), getAST, &n);
    checkError();
    return cast(string) s[0..n];
  }

  T opCast(T)() if (is (T == Z3_app)) {
    assert (isApp());
    return cast (Z3_app) this;
  }
  
  Z3_app getApp() {
    assert (isApp());
    return cast (Z3_app) this;
  }


  /**
     \brief Return the declaration associated with this application.
     This method assumes the expression is an application.

     \pre is_app()
  */
  FuncDecl decl() {
    Z3_func_decl f = Z3_get_app_decl(context(), cast(Z3_app) this);
    checkError();
    return FuncDecl(context(), f);
  }
  /**
     \brief Return the number of arguments in this application.
     This method assumes the expression is an application.

     \pre is_app()
  */
  uint numArgs() {
    uint r = Z3_get_app_num_args(context(), cast(Z3_app) this);
    checkError();
    return r;
  }
  /**
     \brief Return the i-th argument of this application.
     This method assumes the expression is an application.

     \pre is_app()
     \pre i < num_args()
  */
  BvExpr arg(uint i) {
    Z3_ast r = Z3_get_app_arg(context(), cast(Z3_app) this, i);
    checkError();
    return BvExpr(context(), r, _signed);
  }

  // expr rotate_left(uint i) { Z3_ast r = Z3_mk_rotate_left(context(), i, *this); context().checkError(); return expr(context(), r); }
  BvExpr rotateLeft(uint i) {
    Z3_ast r = Z3_mk_rotate_left(context(), i, getAST);
    context().checkError();
    return BvExpr(context(), r, _signed);
  }
  // expr rotate_right(uint i) { Z3_ast r = Z3_mk_rotate_right(context(), i, *this); context().checkError(); return expr(context(), r); }
  BvExpr rotateRight(uint i) {
    Z3_ast r = Z3_mk_rotate_right(context(), i, getAST);
    context().checkError();
    return BvExpr(context(), r, _signed);
  }
  // expr repeat(uint i) { Z3_ast r = Z3_mk_repeat(context(), i, *this); context().checkError(); return expr(context(), r); }
  BvExpr repeat(uint i) {
    Z3_ast r = Z3_mk_repeat(context(), i, getAST);
    context().checkError();
    return BvExpr(context(), r, _signed);
  }

  // expr extract(uint hi, uint lo) { Z3_ast r = Z3_mk_extract(context(), hi, lo, *this); context().checkError(); return expr(context(), r); }
  BvExpr extract(uint hi, uint lo) {
    Z3_ast r = Z3_mk_extract(context(), hi, lo, getAST);
    context().checkError();
    return BvExpr(context(), r, _signed);
  }
  
  // uint lo() { assert (is_app() && Z3_get_decl_num_parameters(context(), decl()) == 2); return static_cast<uint>(Z3_get_decl_int_parameter(context(), decl(), 1)); }
  uint lo() {
    assert (isApp() && Z3_get_decl_num_parameters(context(), decl()) == 2);
    return cast(uint) Z3_get_decl_int_parameter(context(), decl(), 1);
  }

  // uint hi() { assert (is_app() && Z3_get_decl_num_parameters(context(), decl()) == 2); return static_cast<uint>(Z3_get_decl_int_parameter(context(), decl(), 0)); }
  uint hi() {
    assert (isApp() && Z3_get_decl_num_parameters(context(), decl()) == 2);
    return cast(uint) Z3_get_decl_int_parameter(context(), decl(), 0);
  }

  // friend expr fma(expr const& a, expr const& b, expr const& c, expr const& rm);

  // /**
  //    \brief sequence and regular expression operations.
  //    + is overloaded as sequence concatenation and regular expression union.
  //    concat is overloaded to handle sequences and regular expressions
  // */
  BvExpr extract()(auto ref BvExpr offset, auto ref BvExpr length) {
    checkContext(this, offset); checkContext(offset, length);
    Z3_ast r = Z3_mk_seq_extract(context(), getAST, offset.getAST, length.getAST);
    checkError();
    return BvExpr(context(), r, _signed);
  }
  BvExpr replace()(auto ref BvExpr src, auto ref BvExpr dst) {
    checkContext(this, src); checkContext(src, dst);
    Z3_ast r = Z3_mk_seq_replace(context(), getAST, src.getAST, dst.getAST);
    checkError();
    return BvExpr(context(), r, _signed);
  }
  BvExpr unit() {
    Z3_ast r = Z3_mk_seq_unit(context(), getAST);
    checkError();
    return BvExpr(context(), r, _signed);
  }
  BvExpr contains()(auto ref BvExpr s) {
    checkContext(this, s);
    Z3_ast r = Z3_mk_seq_contains(context(), getAST, s.getAST);
    checkError();
    return BvExpr(context(), r, _signed);
  }
  BvExpr at()(auto ref BvExpr index) {
    checkContext(this, index);
    Z3_ast r = Z3_mk_seq_at(context(), getAST, index.getAST);
    checkError();
    return BvExpr(context(), r, _signed);
  }
  BvExpr nth()(auto ref BvExpr index) {
    checkContext(this, index);
    Z3_ast r = Z3_mk_seq_nth(context(), getAST, index.getAST);
    checkError();
    return BvExpr(context(), r, _signed);
  }
  BvExpr length() {
    Z3_ast r = Z3_mk_seq_length(context(), getAST);
    checkError();
    return BvExpr(context(), r, _signed);
  }
  BvExpr stoi() {
    Z3_ast r = Z3_mk_str_to_int(context(), getAST);
    checkError();
    return BvExpr(context(), r, _signed);
  }
  BvExpr itos() {
    Z3_ast r = Z3_mk_int_to_str(context(), getAST);
    checkError();
    return BvExpr(context(), r, _signed);
  }

  // friend expr range(expr const& lo, expr const& hi);


  /**
     \brief create a looping regular expression.
  */
  BvExpr loop(uint lo) {
    Z3_ast r = Z3_mk_re_loop(context(), getAST, lo, 0);
    checkError();
    return BvExpr(context(), r, _signed);
  }

  BvExpr loop(uint lo, uint hi) {
    Z3_ast r = Z3_mk_re_loop(context(), getAST, lo, hi);
    checkError();
    return BvExpr(context(), r, _signed);
  }

  /**
   * index operator defined on arrays and sequences.
   */
  BvExpr opIndex()(auto ref BvExpr index) {
    assert(isArray() || isSeq());
    if (isArray()) {
      return select(this, index);
    }
    return nth(index);            
  }

  BvExpr opIndex()(auto ref BvExprVector index) {
    return select(this, index);
  }

  /**
     \brief Return a simplified version of this expression.
  */
  BvExpr simplify() {
    Z3_ast r = Z3_simplify(context(), getAST);
    checkError();
    return BvExpr(context(), r, _signed);
  }
  /**
     \brief Return a simplified version of this expression. The parameter \c p is a set of parameters for the Z3 simplifier.
  */
  BvExpr simplify()(auto ref Params p) {
    Z3_ast r = Z3_simplify_ex(context(), getAST, p);
    checkError();
    return BvExpr(context(), r, _signed);
  }

  /**
     \brief Apply substitution. Replace src expressions by dst.
  */

  BvExpr substitute()(auto ref BvExprVector src, auto ref BvExprVector dst) {
    assert (src.size() == dst.size());
    Z3_ast[] _src = new Z3_ast[](src.size());
    Z3_ast[] _dst = new Z3_ast[](dst.size());
    for (uint i = 0; i < src.size(); ++i) {
      _src[i] = src[i].getAST;
      _dst[i] = dst[i].getAST;
    }
    Z3_ast r = Z3_substitute(context(), getAST, src.size(), _src.ptr, _dst.ptr);
    checkError();
    return BvExpr(context(), r, _signed);
  }

  /**
     \brief Apply substitution. Replace bound variables by expressions.
  */

  BvExpr substitute()(auto ref BvExprVector dst) {
    Z3_ast[] _dst = new Z3_ast[](dst.size());
    for (uint i = 0; i < dst.size(); ++i) {
      _dst[i] = dst[i].getAST;
    }
    Z3_ast r = Z3_substitute_vars(context(), getAST, dst.size(), _dst.ptr);
    checkError();
    return BvExpr(context(), r, _signed);
  }

  BvExpr opUnary(string op)() if(op == "~") {
    return BvExpr(context(), Z3_mk_bvnot(this.context, getAST), _signed);
  }

  BvExpr opUnary(string op)() if(op == "-") {
    Z3_ast r = 0;
    r = Z3_mk_bvneg(this.context(), getAST);
    this.checkError();
    return BvExpr(context(), r, _signed);
  }

  BvExpr zext(uint i) {
    return BvExpr(context(), Z3_mk_zero_ext(context(), i, getAST), _signed);
  }

  BvExpr sext(uint i) {
    return BvExpr(context(), Z3_mk_sign_ext(context(), i, getAST), _signed);
  }

  BvExpr extendBy(uint i, bool signed) {
    if (signed) {
      return BvExpr(context(), Z3_mk_sign_ext(context(), i, getAST), signed);
    }
    else {
      return BvExpr(context(), Z3_mk_zero_ext(context(), i, getAST), signed);
    }
  }

  BvExpr extendTo(uint newSize, bool signed) {
    assert (newSize >= size());
    if (signed) {
      return BvExpr(context(), Z3_mk_sign_ext(context(), newSize - size(), getAST), signed);
    }
    else {
      return BvExpr(context(), Z3_mk_zero_ext(context(), newSize - size(), getAST), signed);
    }
  }

  uint size() {
    auto sort = getSort();
    assert (sort.isBv());
    return sort.bvSize();
  }

  BvExpr promote() {
    uint sz = size();
    if (sz < 32) {
      if (_signed) {
	return BvExpr(context(), Z3_mk_sign_ext(context(), 32-sz, getAST), true);
      }
      else {
	return BvExpr(context(), Z3_mk_zero_ext(context(), 32-sz, getAST), true);
      }
    }
    if (sz == 32) return this;
    if (sz < 64) {
      if (_signed) {
	return BvExpr(context(), Z3_mk_sign_ext(context(), 64-sz, getAST), true);
      }
      else {
	return BvExpr(context(), Z3_mk_zero_ext(context(), 64-sz, getAST), true);
      }
    }
    else return this;
  }
  
  /**
     \brief Return true if this is a Bit-vector expression.
  */
  bool isBv() {
    return getSort().isBv();
  }

  BvExpr opBinary(string op)(BvExpr other)
    if (op == "&" || op == "|" || op == "^" || op == "+" ||
	op == "-" || op == "*" || op == "/" || op == "%" ||
	op == "<<" || op == ">>" || op == ">>>") {
      import std.algorithm: min, max;
      checkContext(this, other);

      // BvExpr lhs = this;
      // BvExpr rhs = other;
      BvExpr lhs, rhs;

      bool signed = promoteToCommonType(this, other, lhs, rhs);

      assert (lhs.isBv()); assert(rhs.isBv());
      
      Z3_ast r = null;
      static if(op == "&") {
	r = Z3_mk_bvand(this.context(), lhs, rhs);
      }
      static if(op == "|") {
	r = Z3_mk_bvor(this.context(), lhs, rhs);
      }
      static if(op == "^") {
	r = Z3_mk_bvxor(this.context(), lhs, rhs);
      }
      static if(op == "+") {
	r = Z3_mk_bvadd(this.context(), lhs, rhs);
      }
      static if(op == "-") {
	r = Z3_mk_bvsub(this.context(), lhs, rhs);
      }
      static if(op == "*") {
	r = Z3_mk_bvmul(this.context(), lhs, rhs);
      }
      static if(op == "/") {
	if (signed) r = Z3_mk_bvsdiv(a.context(), lhs, rhs);
	else        r = Z3_mk_bvudiv(a.context(), lhs, rhs);
      }
      static if(op == "%") {
	if (signed) r = Z3_mk_bvsrem(a.context(), lhs, rhs);
	else        r = Z3_mk_bvurem(a.context(), lhs, rhs);
      }
      static if(op == "<<") {
	r = Z3_mk_bvshl(a.context(), lhs, rhs);
      }
      static if(op == ">>") {
	if (signed) r = Z3_mk_bvashr(a.context(), lhs, rhs);
	else        r = Z3_mk_bvlshr(a.context(), lhs, rhs);
      }
      static if(op == ">>>") {
	r = Z3_mk_bvlshr(a.context(), lhs, rhs);
      }
      checkError();
      return BvExpr(this.context(), r, signed);
    }

  static BvExpr castAST(Context c, Z3_ast a) {
    assert (Z3_get_ast_kind(c, a) == Z3_ast_kind.Z3_NUMERAL_AST ||
	    Z3_get_ast_kind(c, a) == Z3_ast_kind.Z3_APP_AST ||
	    Z3_get_ast_kind(c, a) == Z3_ast_kind.Z3_QUANTIFIER_AST ||
	    Z3_get_ast_kind(c, a) == Z3_ast_kind.Z3_VAR_AST);
    return BvExpr(c, a);
  }

}


BvExpr intVal(T)(Context c, T n, auto ref Sort s) {
  import std.traits: isSigned;
  Z3_ast r = Z3_mk_int(c, n, s);
  c.checkError();
  enum signed = isSigned!T;
  return BvExpr(c, r, signed);
}

bool promoteToCommonType(ref BvExpr a, ref BvExpr b,
			 ref BvExpr lhs, ref BvExpr rhs) {

  // lhs = a; rhs = b;
  // return true;
  import std.algorithm: max;

  checkContext(a, b);

  BvExpr lexpr = a.promote();
  BvExpr rexpr = b.promote();
      
  uint lsz = lexpr.size();
  uint rsz = rexpr.size();

  uint size = max(lsz, rsz);
  bool signed = lexpr._signed && rexpr._signed;

  if (lsz != size) {
    auto extended = lexpr.extendTo(size, signed);
    lhs = extended;
  }
  else lhs = lexpr;

  if (rsz != size) {
    auto extended = rexpr.extendTo(size, signed);
    rhs = extended;
  }
  else rhs = rexpr;

  return signed;
}

// friend expr concat(expr const& a, expr const& b);
BvExpr concat()(auto ref BvExpr a, auto ref BvExpr b) {
  checkContext(a, b);
  Z3_ast r;
  if (Z3_is_seq_sort(a.context(), a.getSort())) {
    Z3_ast[2] _args = [a.getAST, b.getAST];
    r = Z3_mk_seq_concat(a.context(), 2, _args.ptr);
  }
  else if (Z3_is_re_sort(a.context(), a.getSort())) {
    Z3_ast[2] _args = [a.getAST, b.getAST];
    r = Z3_mk_re_concat(a.context(), 2, _args.ptr);
  }
  else {
    r = Z3_mk_concat(a.context(), a.getAST, b.getAST);
  }
  a.checkError();
  return BvExpr(a.context(), r);
}

// friend expr concat(expr_vector const& args);
BvExpr concat()(auto ref BvExprVector args) {
  Z3_ast r;
  assert (args.size() > 0);
  if (args.size() == 1) {
    return args[0];
  }
  Z3_ast[] _args = args.toArray!Z3_ast();
  if (Z3_is_seq_sort(args.context(), args[0].getSort())) {
    r = Z3_mk_seq_concat(args.context(), cast(uint) _args.length, _args.ptr);
  }
  else if (Z3_is_re_sort(args.context(), args[0].getSort())) {
    r = Z3_mk_re_concat(args.context(), cast(uint) _args.length, _args.ptr);
  }
  else {
    r = _args[_args.length-1];
    for (size_t i = _args.length - 1; i > 0; ) {
      --i;
      r = Z3_mk_concat(args.context(), _args[i], r);
      args.checkError();
    }
  }
  args.checkError();
  return BvExpr(args.context(), r);
}

// friend expr operator==(expr & a, expr & b);
BoolExpr eq()(auto ref BvExpr a, auto ref BvExpr b) {
  BvExpr lhs, rhs;
  bool signed = promoteToCommonType(a, b, lhs, rhs);
  Z3_ast r = Z3_mk_eq(lhs.context(), lhs.getAST, rhs.getAST);
  lhs.checkError();
  return BoolExpr(lhs.context(), r);
}

// friend expr operator==(expr & a, int b);
BoolExpr eq()(auto ref BvExpr a, int b) {
  assert (a.isBv());
  return eq(a, intVal(a.context(), b, a.getSort().byRef).byRef);
}

// friend expr operator==(int a, expr & b);
BoolExpr eq()(int a, auto ref BvExpr b) {
  assert(b.isBv());
  return eq(intVal(b.context(), a, b.getSort().byRef).byRef, b);
}



// friend expr operator!=(expr & a, expr & b);
BoolExpr neq()(auto ref BvExpr a, auto ref BvExpr b) {
  BvExpr lhs, rhs;
  bool signed = promoteToCommonType(a, b, lhs, rhs);
  Z3_ast[2] args = [lhs.getAST, rhs.getAST];
  Z3_ast r = Z3_mk_distinct(lhs.context(), 2, args.ptr);
  lhs.checkError();
  return BoolExpr(lhs.context(), r);
}

// friend expr operator!=(expr & a, int b);
BoolExpr neq()(auto ref BvExpr a, int b) {
  assert (a.isArith() || a.isBv() || a.isFpa());
  return neq(a, intVal(a.context(), b, a.getSort().byRef).byRef);
}

// friend expr operator!=(int a, expr & b);
BoolExpr neq()(int a, auto ref BvExpr b) {
  assert(b.isArith() || b.isBv() || b.isFpa());
  return neq(intVal(b.context(), a, b.getSort().byRef).byRef, b);
}


// friend expr operator+(expr & a, expr & b);
BvExpr add()(auto ref BvExpr a, auto ref BvExpr b) {
  BvExpr lhs, rhs;
  bool signed = promoteToCommonType(a, b, lhs, rhs);
  Z3_ast r = Z3_mk_bvadd(lhs.context(), lhs.getAST, rhs.getAST);
  lhs.checkError();
  return BvExpr(lhs.context(), r);
}

// friend expr operator+(expr & a, int b);
BvExpr add()(auto ref BvExpr a, int b) {
  return add(a, intVal(a.context(), b, a.getSort().byRef).byRef);
}
    
// friend expr operator+(int a, expr & b);
BvExpr add()(int a, auto ref BvExpr b) {
  return add((b.intVa, a, b.getSort().byRef).byRef, b);
}


// friend expr sum(expr_vector const& args);
BvExpr sum()(auto ref BvExprVector args) {
  assert (args.size() > 0);
  Context ctx = args.context();
  Z3_ast[] _args = args.toArray!Z3_ast();
  Z3_ast r = Z3_mk_add(args.context(), cast(uint) _args.length, _args.ptr);
  args.checkError();
  return BvExpr(args.context(), r);
}


// friend expr operator*(expr & a, expr & b);
BvExpr mul()(auto ref BvExpr a, auto ref BvExpr b) {
  BvExpr lhs, rhs;
  bool signed = promoteToCommonType(a, b, lhs, rhs);
  Z3_ast r = null;
  r = Z3_mk_bvmul(lhs.context(), lhs.getAST, rhs.getAST);
  lhs.checkError();
  return BvExpr(lhs.context(), r);
}

// friend expr operator*(expr & a, int b);
BvExpr mul()(auto ref BvExpr a, int b) {
  return mul(a, intVal(a.context(), b, a.getSort().byRef).byRef);
}

// friend expr operator*(int a, expr & b);
BvExpr mul()(int a, auto ref BvExpr b) {
  return mul(intVal(b.context(), a, b.getSort().byRef).byRef, b);
}

// /*  \brief Power operator  */
// friend expr pw(expr & a, expr & b);
BvExpr pw()(auto ref BvExpr a, auto ref BvExpr b) {
  BvExpr lhs, rhs;
  bool signed = promoteToCommonType(a, b, lhs, rhs);
  Z3_ast r = Z3_mk_power(lhs.context(), lhs.getAST, rhs.getAST);
  lhs.checkError();
  return BvExpr(lhs.context(), r);
}

// friend expr pw(expr & a, int b);
BvExpr pw()(auto ref BvExpr a, int b) {
  return pw(a, intVal(a.context(), b, a.getSort().byRef).byRef);
}

// friend expr pw(int a, expr & b);
BvExpr pw()(int a, auto ref BvExpr b) {
  return pw(intVal(b.context(), a, b.getSort().byRef).byRef, b);
}

// /* \brief mod operator */
// friend expr mod(expr const& a, expr const& b);
BvExpr mod()(auto ref BvExpr a, auto ref BvExpr b) {
  BvExpr lhs, rhs;
  bool signed = promoteToCommonType(a, b, lhs, rhs);
  Z3_ast r = Z3_mk_bvsmod(lhs.context(), lhs.getAST, rhs.getAST);
  lhs.checkError();
  return BvExpr(lhs.context(), r);
}

// friend expr mod(expr const& a, int b);
BvExpr mod()(auto ref BvExpr a, int b) {
  return mod(a, intVal(a.context(), b, a.getSort().byRef).byRef);
}

// friend expr mod(int a, expr const& b);
BvExpr mod()(int a, auto ref BvExpr b) {
  return mod(intVal(b.context(), a, b.getSort().byRef).byRef, b);
}


// /* \brief rem operator */
// friend expr rem(expr const& a, expr const& b);
BvExpr rem()(auto ref BvExpr a, auto ref BvExpr b) {
  BvExpr lhs, rhs;
  bool signed = promoteToCommonType(a, b, lhs, rhs);
  Z3_ast r = Z3_mk_rem(lhs.context(), lhs.getAST, rhs.getAST);
  lhs.checkError();
  return BvExpr(lhs.context(), r);
}

// friend expr rem(expr const& a, int b);
BvExpr rem()(auto ref BvExpr a, int b) {
  return rem(a, intVal(a.context(), b, a.getSort().byRef).byRef);
}

// friend expr rem(int a, expr const& b);
BvExpr rem()(int a, auto ref BvExpr b) {
  return rem(intVal(b.context(), a, b.getSort().byRef).byRef, b);
}

// friend expr operator/(expr & a, expr & b);
BvExpr div()(auto ref BvExpr a, auto ref BvExpr b) {
  BvExpr lhs, rhs;
  bool signed = promoteToCommonType(a, b, lhs, rhs);
  Z3_ast r = Z3_mk_bvsdiv(lhs.context(), lhs.getAST, rhs.getAST);
  lhs.checkError();
  return BvExpr(lhs.context(), r);
}

// friend expr operator/(expr & a, int b);
BvExpr div()(auto ref BvExpr a, int b) {
  return div(a, intVal(a.context(), b, a.getSort().byRef).byRef);
}

// friend expr operator/(int a, expr & b);
BvExpr div()(int a, auto ref BvExpr b) {
  return div(intVal(b.context(), a, b.getSort().byRef).byRef, b);
}

// friend expr operator-(expr & a);
BvExpr neg()(auto ref BvExpr a) {
  Z3_ast r = Z3_mk_bvneg(a.context(), a.getAST);
  a.checkError();
  return BvExpr(a.context(), r);
}

// friend expr operator-(expr & a, expr & b);
BvExpr sub()(auto ref BvExpr a, auto ref BvExpr b) {
  BvExpr lhs, rhs;
  bool signed = promoteToCommonType(a, b, lhs, rhs);
  Z3_ast r = Z3_mk_bvsub(lhs.context(), lhs.getAST, rhs.getAST);
  lhs.checkError();
  return BvExpr(lhs.context(), r);
}

// friend expr operator-(expr & a, int b);
BvExpr sub()(auto ref BvExpr a, int b) {
  return sub(a, intVal(a.context(), b, a.getSort().byRef).byRef);
}

// friend expr operator-(int a, expr & b);
BvExpr sub()(int a, auto ref BvExpr b) {
  return sub(intVal(b.context(), a, b.getSort().byRef).byRef, b);
}


// friend expr operator<=(expr & a, expr & b);
BoolExpr le()(auto ref BvExpr a, auto ref BvExpr b) {
  BvExpr lhs, rhs;
  bool signed = promoteToCommonType(a, b, lhs, rhs);
  Z3_ast r = Z3_mk_bvsle(a.context(), lhs.getAST, rhs.getAST);
  a.checkError();
  return BoolExpr(a.context(), r);
}

// friend expr operator<=(expr & a, int b);
BoolExpr le()(auto ref BvExpr a, int b) {
  return le(a, intVal(a.context(), b, a.getSort().byRef).byRef);
}

// friend expr operator<=(int a, expr & b);
BoolExpr le()(int a, auto ref BvExpr b) {
  return le(intVal(b.context(), a, b.getSort().byRef).byRef, b);
}

// friend expr operator>=(expr & a, expr & b);
BoolExpr ge()(auto ref BvExpr a, auto ref BvExpr b) {
  BvExpr lhs, rhs;
  bool signed = promoteToCommonType(a, b, lhs, rhs);
  Z3_ast r = Z3_mk_bvsge(a.context(), lhs.getAST, rhs.getAST);
  a.checkError();
  return BoolExpr(a.context(), r);
}

// friend expr operator>=(expr & a, int b);
BoolExpr ge()(auto ref BvExpr a, int b) {
  return ge(a, intVal(a.context(), b, a.getSort().byRef).byRef);
}

// friend expr operator>=(int a, expr & b);
BoolExpr ge()(int a, auto ref BvExpr b) {
  return ge(intVal(b.context(), a, b.getSort().byRef).byRef, b);
}


// friend expr operator<(expr & a, expr & b);
BoolExpr lt()(auto ref BvExpr a, auto ref BvExpr b) {
  BvExpr lhs, rhs;
  bool signed = promoteToCommonType(a, b, lhs, rhs);
  Z3_ast r = null;
  if (signed) r = Z3_mk_bvslt(a.context(), lhs.getAST, rhs.getAST);
  else        r = Z3_mk_bvult(a.context(), lhs.getAST, rhs.getAST);
  a.checkError();
  return BoolExpr(a.context(), r);
}

// friend expr operator<(expr & a, int b);
BoolExpr lt()(auto ref BvExpr a, int b) {
  return lt(a, intVal(a.context(), b, a.getSort().byRef).byRef);
}

// friend expr operator<(int a, expr & b);
BoolExpr lt()(int a, auto ref BvExpr b) {
  return lt(intVal(b.context(), a, b.getSort().byRef).byRef, b);
}

// friend expr operator>(expr & a, expr & b);
BoolExpr gt()(auto ref BvExpr a, auto ref BvExpr b) {
  BvExpr lhs, rhs;
  bool signed = promoteToCommonType(a, b, lhs, rhs);
  Z3_ast r = null;
  if (signed) r = Z3_mk_bvsgt(a.context(), lhs.getAST, rhs.getAST);
  else        r = Z3_mk_bvugt(a.context(), lhs.getAST, rhs.getAST);
  a.checkError();
  return BoolExpr(a.context(), r);
}

// friend expr operator>(expr & a, int b);
BoolExpr gt()(auto ref BvExpr a, int b) {
  return gt(a, intVal(a.context(), b, a.getSort().byRef).byRef);
}

// friend expr operator>(int a, expr & b);
BoolExpr gt()(int a, auto ref BvExpr b) {
  return gt(intVal(b.context(), a, b.getSort().byRef).byRef, b);
}

// friend expr pble(expr_vector const& es, int * coeffs, int bound);
BoolExpr pble()(auto ref BvExprVector es, int* coeffs, int bound) {
  assert (es.size() > 0);
  Z3_ast[] _es = es.toArray!Z3_ast;
  Z3_ast r = Z3_mk_pble(es.context(), cast(uint) _es.length, _es.ptr, coeffs, bound);
  es.context.checkError();
  return BoolExpr(es.context(), r);
}

// friend expr pbge(expr_vector const& es, int * coeffs, int bound);
BoolExpr pbge()(auto ref BvExprVector es, int* coeffs, int bound) {
  assert (es.size() > 0);
  Z3_ast[] _es = es.toArray!Z3_ast;
  Z3_ast r = Z3_mk_pbge(es.context(), cast(uint) _es.length, _es.ptr, coeffs, bound);
  es.context.checkError();
  return BoolExpr(es.context(), r);
}

// friend expr pbeq(expr_vector const& es, int * coeffs, int bound);
BoolExpr pbeq()(auto ref BvExprVector es, int* coeffs, int bound) {
  assert (es.size() > 0);
  Z3_ast[] _es = es.toArray!Z3_ast;
  Z3_ast r = Z3_mk_pbeq(es.context(), cast(uint) _es.length, _es.ptr, coeffs, bound);
  es.context.checkError();
  return BoolExpr(es.context(), r);
}

// friend expr atmost(expr_vector const& es, uint bound);
BoolExpr atmost()(auto ref BvExprVector es, uint bound) {
  assert (es.size() > 0);
  Z3_ast[] _es = es.toArray!Z3_ast;
  Z3_ast r = Z3_mk_atmost(es.context(), cast(uint) _es.length, _es.ptr, bound);
  es.context.checkError();
  return BoolExpr(es.context(), r);
}

// friend expr atleast(expr_vector const& es, uint bound);
BoolExpr atleast()(auto ref BvExprVector es, uint bound) {
  assert (es.size() > 0);
  Z3_ast[] _es = es.toArray!Z3_ast;
  Z3_ast r = Z3_mk_atleast(es.context(), cast(uint) _es.length, _es.ptr, bound);
  es.context.checkError();
  return BoolExpr(es.context(), r);
}

// friend expr operator&(expr & a, expr & b);
BvExpr bvand()(auto ref BvExpr a, auto ref BvExpr b) {
  checkContext(a, b);
  Z3_ast r = Z3_mk_bvand(a.context(), a.getAST, b.getAST);
  return BvExpr(a.context(), r);
}
// friend expr operator&(expr & a, int b);
BvExpr bvand()(auto ref BvExpr a, int b) {
  return bvand(a, intVal(a.context(), b, a.getSort().byRef).byRef);
}
// friend expr operator&(int a, expr & b);
BvExpr bvand()(int a, auto ref BvExpr b) {
  return bvand(intVal(b.context(), a, b.getSort().byRef).byRef, b);
}

// friend expr operator^(expr & a, expr & b);
BvExpr bvxor()(auto ref BvExpr a, auto ref BvExpr b) {
  checkContext(a, b);
  Z3_ast r = Z3_mk_bvxor(a.context(), a.getAST, b.getAST);
  return BvExpr(a.context(), r);
}
// friend expr operator^(expr & a, int b);
BvExpr bvxor()(auto ref BvExpr a, int b) {
  return bvxor(a, intVal(a.context(), b, a.getSort().byRef).byRef);
}
// friend expr operator^(int a, expr & b);
BvExpr bvXor()(int a, auto ref BvExpr b) {
  return bvxor(intVal(b.context(), a, b.getSort().byRef).byRef, b);
}

// friend expr operator|(expr & a, expr & b);
BvExpr bvor()(auto ref BvExpr a, auto ref BvExpr b) {
  checkContext(a, b);
  Z3_ast r = Z3_mk_bvor(a.context(), a.getAST, b.getAST);
  return BvExpr(a.context(), r);
}
// friend expr operator|(expr & a, int b);
BvExpr bvor()(auto ref BvExpr a, int b) {
  return bvor(a, intVal(a.context(), b, a.getSort().byRef).byRef);
}
// friend expr operator|(int a, expr & b);
BvExpr bvor()(int a, auto ref BvExpr b) {
  return bvor(intVal(b.context(), a, b.getSort().byRef).byRef, b);
}

// friend expr nand(expr const& a, expr const& b);
BvExpr bvnand()(auto ref BvExpr a, auto ref BvExpr b) {
  checkContext(a, b);
  Z3_ast r = Z3_mk_bvnand(a.context(), a.getAST, b.getAST);
  return BvExpr(a.context(), r);
}
// friend expr nor(expr const& a, expr const& b);
BvExpr bvnor()(auto ref BvExpr a, auto ref BvExpr b) {
  checkContext(a, b);
  Z3_ast r = Z3_mk_bvnor(a.context(), a.getAST, b.getAST);
  return BvExpr(a.context(), r);
}
// friend expr xnor(expr const& a, expr const& b);
BvExpr bvxnor()(auto ref BvExpr a, auto ref BvExpr b) {
  checkContext(a, b);
  Z3_ast r = Z3_mk_bvxnor(a.context(), a.getAST, b.getAST);
  return BvExpr(a.context(), r);
}

// friend expr min(expr const& a, expr const& b);
BvExpr min()(auto ref BvExpr a, auto ref BvExpr b) { 
  checkContext(a, b); 
  Z3_ast r = Z3_mk_ite(a.context(), Z3_mk_bvuge(a.context(), a.getAST, b.getAST), b.getAST, a.getAST);
  return BvExpr(a.context(), r); 
}

// friend expr max(expr const& a, expr const& b);
BvExpr max()(auto ref BvExpr a, auto ref BvExpr b) { 
  checkContext(a, b); 
  Z3_ast r = Z3_mk_ite(a.context(), Z3_mk_bvuge(a.context(), a.getAST, b.getAST), a.getAST, b.getAST);
  return BvExpr(a.context(), r); 
}

struct BoolExpr
{
  mixin RvalueRef;
  
  AST _ast;

  this(Context c) {
    _ast = AST(c);
  }

  this(Context c, string name, uint size) {
    Z3_ast r = Z3_mk_const(c, c.strSymbol(name), c.bvSort(size));
    c.checkError();
    _ast = AST(c, r);
  }
  
  this(Context c, Z3_ast n) {
    _ast = AST(c, n);
  }

  this(ref return scope BoolExpr rhs) {
    _ast = AST(rhs._ast);
  }

  BoolExpr opAssign(ref return scope BoolExpr rhs) {
    _ast = rhs._ast;
    return this;
  }


  void setContext(Context ctx) {
    _ast.setContext(ctx);
  }
  Context context() {
    return _ast.context();
  }
  Z3_error_code checkError()  {
    return _ast.checkError();
  }
  // alias _ast this;

  T opCast(t)() if (is (T == Z3_ast)) {
    return _ast.m_ast;
  }
  Z3_ast getAST() {
    return _ast.m_ast;
  }
  
  alias getAST this;

  Z3_ast_kind kind() {
    return _ast.kind();
  }

  Z3_ast opCast(T)() if (is (T == Z3_ast)) {
    return _ast.m_ast;
  }
  
  /**
     \brief Return the sort of this expression.
  */
  Sort getSort() {
    Z3_sort s = Z3_get_sort(context(), _ast.m_ast);
    checkError();
    return Sort(context(), s);
  }

  /**
     \brief Return true if this expression is an application.
  */
  bool isApp() {
    return kind() == Z3_ast_kind.Z3_APP_AST || kind() == Z3_ast_kind.Z3_NUMERAL_AST;
  }
  /**
     \brief Return true if this expression is a constant (i.e., an application with 0 arguments).
  */
  bool isConst() {
    return isApp() && numArgs() == 0;
  }
  /**
     \brief Return true if this expression is a quantifier.
  */
  bool isQuantifier() {
    return kind() == Z3_ast_kind.Z3_QUANTIFIER_AST;
  }

  /**
     \brief Return true if this expression is a universal quantifier.
  */
  bool isForall() {
    return Z3_is_quantifier_forall(context(), getAST);
  }
  /**
     \brief Return true if this expression is an existential quantifier.
  */
  bool isExists() {
    return Z3_is_quantifier_exists(context(), getAST);
  }
  /**
     \brief Return true if this expression is a lambda expression.
  */
  bool isLambda() {
    return Z3_is_lambda(context(), getAST);
  }
  /**

     \brief Return true if this expression is a variable.
  */
  bool isVar() {
    return kind() == Z3_ast_kind.Z3_VAR_AST;
  }
  /**
     \brief Return true if this expression is well sorted (aka type correct).
  */
  bool isWellSorted() {
    bool r = Z3_is_well_sorted(context(), getAST);
    checkError();
    return r;
  }

  /**
     \brief retrieve unique identifier for expression.
  */
  uint id() {
    uint r = Z3_get_ast_id(context(), getAST);
    checkError();
    return r;
  }

  Z3_lbool boolValue() {
    return Z3_get_bool_value(context(), getAST);
  }

  T opCast(T)() if (is (T == Z3_app)) {
    assert (isApp());
    return cast (Z3_app) this;
  }
  
  Z3_app getApp() {
    assert (isApp());
    return cast (Z3_app) this;
  }


  /**
     \brief Return the declaration associated with this application.
     This method assumes the expression is an application.

     \pre is_app()
  */
  FuncDecl decl() {
    Z3_func_decl f = Z3_get_app_decl(context(), cast(Z3_app) this);
    checkError();
    return FuncDecl(context(), f);
  }
  /**
     \brief Return the number of arguments in this application.
     This method assumes the expression is an application.

     \pre is_app()
  */
  uint numArgs() {
    uint r = Z3_get_app_num_args(context(), cast(Z3_app) this);
    checkError();
    return r;
  }
  /**
     \brief Return the i-th argument of this application.
     This method assumes the expression is an application.

     \pre is_app()
     \pre i < num_args()
  */
  BoolExpr arg(uint i) {
    Z3_ast r = Z3_get_app_arg(context(), cast(Z3_app) this, i);
    checkError();
    return BoolExpr(context(), r);
  }

  /**
     \brief Return the 'body' of this quantifier.

     \pre is_quantifier()
  */
  BoolExpr body() {
    assert (isQuantifier());
    Z3_ast r = Z3_get_quantifier_body(context(), getAST);
    checkError();
    return BoolExpr(context(), r);
  }

  bool isTrue() {
    return isApp() && Z3_decl_kind.Z3_OP_TRUE == decl().declKind();
  }
  bool isFalse() {
    return isApp() && Z3_decl_kind.Z3_OP_FALSE == decl().declKind();
  }
  bool isNot() {
    return isApp() && Z3_decl_kind.Z3_OP_NOT == decl().declKind();
  }
  bool isAnd() {
    return isApp() && Z3_decl_kind.Z3_OP_AND == decl().declKind();
  }
  bool isOr()  {
    return isApp() && Z3_decl_kind.Z3_OP_OR  == decl().declKind();
  }
  bool isXor() {
    return isApp() && Z3_decl_kind.Z3_OP_XOR  == decl().declKind();
  }
  bool isImplies() {
    return isApp() && Z3_decl_kind.Z3_OP_IMPLIES  == decl().declKind();
  }
  bool isEq() {
    return isApp() && Z3_decl_kind.Z3_OP_EQ == decl().declKind();
  }
  bool isIte() {
    return isApp() && Z3_decl_kind.Z3_OP_ITE == decl().declKind();
  }
  bool isDistinct() {
    return isApp() && Z3_decl_kind.Z3_OP_DISTINCT == decl().declKind();
  }

        
  /**
     \brief Return a simplified version of this expression.
  */
  BoolExpr simplify() {
    Z3_ast r = Z3_simplify(context(), getAST);
    checkError();
    return BoolExpr(context(), r);
  }
  /**
     \brief Return a simplified version of this expression. The parameter \c p is a set of parameters for the Z3 simplifier.
  */
  BoolExpr simplify()(auto ref Params p) {
    Z3_ast r = Z3_simplify_ex(context(), getAST, p);
    checkError();
    return BoolExpr(context(), r);
  }

  /**
     \brief Apply substitution. Replace src expressions by dst.
  */

  BoolExpr substitute()(auto ref BoolExprVector src, auto ref BoolExprVector dst) {
    assert (src.size() == dst.size());
    Z3_ast[] _src = new Z3_ast[](src.size());
    Z3_ast[] _dst = new Z3_ast[](dst.size());
    for (uint i = 0; i < src.size(); ++i) {
      _src[i] = src[i].getAST;
      _dst[i] = dst[i].getAST;
    }
    Z3_ast r = Z3_substitute(context(), getAST, src.size(), _src.ptr, _dst.ptr);
    checkError();
    return BoolExpr(context(), r);
  }

  /**
     \brief Apply substitution. Replace bound variables by expressions.
  */

  BoolExpr substitute()(auto ref BoolExprVector dst) {
    Z3_ast[] _dst = new Z3_ast[](dst.size());
    for (uint i = 0; i < dst.size(); ++i) {
      _dst[i] = dst[i].getAST;
    }
    Z3_ast r = Z3_substitute_vars(context(), getAST, dst.size(), _dst.ptr);
    checkError();
    return BoolExpr(context(), r);
  }

  BoolExpr opUnary(string op)() if(op == "~") {
    return BoolExpr(this.context(), Z3_mk_bvnot(this.context, getAST));
  }

  static BoolExpr castAST(Context c, Z3_ast a) {
    assert (Z3_get_ast_kind(c, a) == Z3_ast_kind.Z3_NUMERAL_AST ||
	    Z3_get_ast_kind(c, a) == Z3_ast_kind.Z3_APP_AST ||
	    Z3_get_ast_kind(c, a) == Z3_ast_kind.Z3_QUANTIFIER_AST ||
	    Z3_get_ast_kind(c, a) == Z3_ast_kind.Z3_VAR_AST);
    return BoolExpr(c, a);
  }
}

void addRule()(auto ref Solver solver, auto ref BoolExpr e) {
  Z3_solver_assert(solver.context(), solver, e.getAST);
  solver.checkError();
}

void addRule()(auto ref Solver solver, auto ref BoolExpr e, auto ref BoolExpr p) {
  assert (p.isConst());
  Z3_solver_assert_and_track(context(), solver, e.getAST, p.getAST);
  solver.checkError();
}

void addRule()(auto ref Solver solver, auto ref BoolExpr e, string p) {
  addRule(solver, e, solver.context().boolConst(p).byRef);
}

// friend expr ite(expr & c, expr & t, expr & e);
BoolExpr ite()(auto ref BoolExpr c, auto ref BoolExpr t, auto ref BoolExpr e) {
  checkContext(c, t); checkContext(c, e);
  Z3_ast r = Z3_mk_ite(c.context, c.getAST, t.getAST, e.getAST);
  c.checkError();
  return BoolExpr(c.context(), r);
}

