module esdl.rand.intr;

import std.traits: isIntegral;
import std.container.array;
import std.traits: Unsigned;

T larger(T) (T a, T b) {
  if (a > b) return a;
  else return b;
}

T smaller(T) (T a, T b) {
  if (a < b) return a;
  else return b;
}


struct IntRange(T) if (isIntegral!T) {
  T _min = T.min;
  T _max = T.min;

  T min() {
    return _min;
  }
  
  T max() {
    return _max;
  }
  
  Unsigned!T _count;
  
  bool opOpAssign(string op)(IntRange other) {
    static if (op == "|") {
      if (this._max == T.min) {
	if (other._max == T.min || other._max >= this._min) {
	  this._min = smaller(this._min, other._min);
	  return true;
	}
	else return false;
      }
      if (other._max == T.min) {
	if (this._max == T.min || this._max >= other._min) {
	  this._max = T.min;
	  this._min = smaller(this._min, other._min);
	  return true;
	}
	else return false;
      }
      if (other._min > this._max || this._min > other._max) {
	return false;
      }
      else {
	this._min = smaller(this._min, other._min);
	this._max = larger(this._max, other._max);
	return true;
      }
    }
    static if (op == "&") {
      if (this._max == T.min) {
	if (other._max == T.min || other._max > this._min) {
	  this._min = larger(this._min, other._min);
	  this._max = other._max;
	  return true;
	}
	else return false;
      }
      if (other._max == T.min) {
	if (this._max == T.min || this._max > other._min) {
	  this._min = larger(this._min, other._min);
	  return true;
	}
	else return false;
      }
      if (other._min >= this._max || this._min >= other._max) {
	return false;
      }
      else {
	this._min = larger(this._min, other._min);
	this._max = smaller(this._max, other._max);
	return true;
      }
    }    
  }

  string toString() {
    import std.string;
    if (this._max == T.min) {
      return format("[%s, ..)", this._min);
    }
    else {
      return format("[%s, %s)", this._min, this._max);
    }
  }

  Unsigned!T count() {
    if (this == typeof(this).init) {
      assert(false, "Too big to count");
    }
    else {
      return cast(Unsigned!T) (_max - this._min);
    }
  }

  Unsigned!T unsafeCount() {
    return cast(Unsigned!T) (_max - this._min);
  }
}

struct IntRangeSet(T)
{
  import std.random: uniform, rndGen, Random;
  Array!(IntRange!T) ranges;

  Unsigned!T _count;
  
  void optimize() {
    import std.algorithm.sorting;
    IntRange!T r;
    Array!(IntRange!T) optRanges;
    auto sortedRanges = sort!((a, b) => a.min < b.min)(ranges[]);
    bool first = true;
    foreach (ref sr; sortedRanges) {
      if (first) {
      	r = sr;
	first = false;
      	continue;
      }
      if (r |= sr) continue;
      else {
      	optRanges ~= r;
      	r = sr;
      }
    }
    optRanges ~= r;
    this.ranges = optRanges;
    if (! this.isFull()) {
      _count = count();
    }
  }

  void opOpAssign(string op)(IntRange!T r) {
    static if (op == "~") {
      ranges ~= r;
      this.optimize();
    }
  }

  void opOpAssign(string op)(IntRangeSet other) {
    static if (op == "|") {
      ranges ~= other.ranges;
      optRanges();
    }
    static if (op == "&") {
      this.optimize();
      other.optimize();
      Array!(IntRange!T) newRanges;
      foreach (oRange; other.ranges) {
	foreach (tRange; this.ranges) {
	  if (tRange &= oRange) {
	    newRanges ~= tRange;
	  }
	}
      }
      ranges = newRanges;
      this.optimize();
    }
  }

  bool isEmpty() {
    return (ranges.length == 0);
  }

  bool isFull() {
    return (ranges.length == 1 &&
	    ranges[0] == IntRange!T.init);
  }

  string toString() {
    import std.conv;
    return ranges[].to!string();
  }

  Unsigned!T count() {
    Unsigned!T cou;
    foreach(r; ranges) {
      cou += r.count();
    }
    return cou;
  }

  T uniform(ref Random gen = rndGen()) {
    if (this.isFull()) {
      return uniform!T(gen);
    }
    else {
      auto rn = uniform(0, _count, gen);

      foreach (r; ranges) {
	auto cou = r.unsafeCount();
	if (cou > rn) {
	  return cast(T) (r.min + rn);
	}
	else {
	  rn -= cou;
	}
      }
      assert(false, "Random number generated is outside scope");
    }
  }
}

template IntRangeSet(bool S, uint N)
{
  static if (N > 32) {
    static if (S) alias T = long;
    else alias T = ulong;
  }
  else static if (N > 16) {
    static if (S) alias T = int;
    else alias T = uint;
  }
  else static if (N > 8) {
    static if (S) alias T = short;
    else alias T = ushort;
  }
  else {
    static if (S) alias T = byte;
    else alias T = ubyte;
  }
  alias IntRangeSet = IntRangeSet!T;
}

enum IntRangeModOp: byte {ADD, SUB, SUBD, MULT, DIV, DIVD}

struct IntRangeMod(T)
{
  long _arg;
  IntRangeModOp _op;
}

struct IntRangeModSet(T)
{
  Array!(IntRangeMod!T) _mods;

  void opOpAssign(string op)(IntRangeMod!T mod) {
    static if (op == "~") {
      _mods ~= mod;
    }
    else {
      static assert (false);
    }
  }

  IntRange!T apply(IntRange!T range, IntRangeOp op) {
    foreach (mod; _mods) {
      mod.apply(range, op);
    }
    return range;
  }
}
