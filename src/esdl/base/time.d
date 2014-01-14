// Written in the D programming language.

// Copyright: Coverify Systems Technology 2012 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

// This file is part of esdl.

module esdl.base.time;

protected enum TimeUnit: byte
  {   SEC  = 0,
      MSEC = -3,
      USEC = -6,
      NSEC = -9,
      PSEC = -12,
      FSEC = -15
      }

alias TimeUnit.FSEC FSEC;
alias TimeUnit.PSEC PSEC;
alias TimeUnit.NSEC NSEC;
alias TimeUnit.USEC USEC;
alias TimeUnit.MSEC MSEC;
alias TimeUnit.SEC  SEC;

public struct Time
{
  public long _value;
  public TimeUnit _unit;

  public this(long value, TimeUnit unit) {
    _value = value;
    _unit = unit;
  }

  public bool isZero() {
    if(_value is 0) return true;
    else return false;
  }

  public int opCmp(Time other) {
    if(other._unit > this._unit) {
      int p = other._unit - this._unit;
      if(other._value * 10^^p > this._value) return -1;
      if(other._value * 10^^p < this._value) return 1;
      return 0;
    }
    else {
      int p = this._unit - other._unit;
      if(other._value > this._value * 10^^p) return -1;
      if(other._value < this._value * 10^^p) return 1;
      return 0;
    }
  }

  public bool opEquals(Time other) {
    if(other._unit > this._unit) {
      int p = other._unit - this._unit;
      if(other._value * 10^^p == this._value) return true;
      else return false;
    }
    else {
      int p = this._unit - other._unit;
      if(other._value == this._value * 10^^p) return true;
      else return false;
    }
  }
}

public Time fsec(long value) {
  return Time(value, FSEC);
}

public Time psec(long value) {
  return Time(value, PSEC);
}

public Time nsec(long value) {
  return Time(value, NSEC);
}

public Time usec(long value) {
  return Time(value, USEC);
}

public Time msec(long value) {
  return Time(value, MSEC);
}

public Time sec(long value) {
  return Time(value, SEC);
}

unittest {
  assert(10.msec > 10.usec);
  assert(10.usec < 10.msec);
  assert(100.usec < 10.msec);
  assert(10.msec == 10000.usec);
}

interface TimeContext
{
public:
  @property abstract byte timeUnit();
  @property abstract byte timePrecision();
  @property abstract ulong timeScale();
}

interface TimeConfigContext: TimeContext
{
public:
  private enum byte _timePrecisionNull = byte.min;
  private enum byte _timeUnitNull = byte.min;
  @property abstract void timeUnit(Time t);
  @property abstract void timeUnit(byte unit);
  @property abstract byte timeUnit();
  @property abstract byte timePrecision();
  @property abstract void timePrecision(Time t);
  @property abstract void timePrecision(byte unit);
  @property abstract ulong timeScale();
  @property abstract void timeScale(ulong t);
  public final SimTime tu(ulong val) {
    return SimTime(val*timeScale());
  }
  // SimTime Time(ulong val, TimeUnit unit);
  // SimTime Time(string unit="default")(ulong val);
  // SimTime Time(double val);
  // SimTime Time(double val, TimeUnit unit);
  public byte findTimePrecision();
  public void fixTimeParameters(byte precision);

  static final string timedMixin() {
    return q{
      private enum byte _timePrecisionNull = byte.min;
      private enum byte _timeUnitNull = byte.min;
      private byte _timeUnit = _timeUnitNull;
      // Since precision would be same everywhere, let it be stored only
      // in the _esdl__setRoot(simulator).
      protected byte _timePrecision = _timePrecisionNull;
      protected ulong _timeScale = 0;

      @property override public void timeUnit(Time t) {
	synchronized(this) {
	  this._isPowerOf10(t._value) ||
	    assert(false, "timeUnit takes only powers of 10 as arguments");
	  (this.timeUnit == this._timeUnitNull) ||
	    assert(false, "you can use timeUnit only once");
	  this.timeUnit = cast(byte)(t._unit + this._log10(t._value));
	}
      }

      @property override public byte timeUnit() {
	synchronized(this) {
	  return this._timeUnit;
	}
      }

      @property override public void timeUnit(byte unit) {
	synchronized(this) {
	  this._timeUnit = unit;
	}
      }

      @property override public void timePrecision(Time t) {
	synchronized(this) {
	  this._isPowerOf10(t._value) ||
	    assert(false, "timePrecision takes only powers of 10 as arguments");
	  (this._timePrecision == this._timePrecisionNull) ||
	    assert(false, "you can use timePrecision only once");
	  this._timePrecision = cast(byte)(t._unit + this._log10(t._value));
	}
      }

      @property override public void timePrecision(byte u) {
	synchronized(this) {
	  this._timePrecision = u;
	}
      }

      @property override public byte timePrecision() {
	synchronized(this)
	  {
	    return this._timePrecision;
	  }
      }

      @property override public ulong timeScale() {
	synchronized(this)
	  {
	    return this._timeScale;
	  }
      }

      @property override public void timeScale(ulong t) {
	synchronized(this) {
	  this._timeScale = t;
	}
      }

      // returns true if the given number is an exact power of 10
      private static bool _isPowerOf10(ulong n) {
	if(n == 0) return false;
	if(n == 1) return true;
	if(n % 10L == 0) return _isPowerOf10(n/10L);
	else return false;
      }
      private static ubyte _log10(ulong n) {
	if(n == 1) return 0;
	else return cast(ubyte)(1 + _log10(n/10L));
      }

      // unittest {
      //   assert(TimedObject._isPowerOf10(10) == true);
      //   assert(TimedObject._isPowerOf10(20) == false);
      //   assert(TimedObject._isPowerOf10(0) == false);
      //   assert(TimedObject._isPowerOf10(1) == true);
      //   assert(TimedObject._log10(1) == 0);
      //   assert(TimedObject._log10(100) == 2);
      //   assert(TimedObject._log10(10000) == 4);
      // }
    };
  }

}

import std.math;

struct SimTime
{
  import std.traits: isIntegral;
  // Just store simulation time steps
  private long _value;

  public this(long value) {
    this._value = value;
  }

  public long getVal() {
    return _value;
  }

  public void opAssign(long value) {
    this._value = value;
  }

  public void opAssign(SimTime t) {
    this._value = t._value;
  }

  public this(TimeConfigContext context, long val) {
    synchronized(context) {
      this._value = val * context.timeScale;
    }
  }

  public this(TimeConfigContext context, long val, TimeUnit unit) {
    synchronized(context) {
      if(context.timePrecision <= unit) {
	this._value = val * 10L ^^(unit - context.timePrecision);
      }
      else {
	this._value = val / 10L ^^(context.timePrecision - unit);
      }
    }
  }

  public this(TimeConfigContext context, Time t) {
    synchronized(context) {
      if(context.timePrecision <= t._unit) {
	this._value = t._value * 10L ^^(t._unit - context.timePrecision);
      }
      else {
	this._value = t._value / 10L ^^(context.timePrecision - t._unit);
      }
    }
  }

  public int opCmp(SimTime rhs) {
    if(this._value == rhs._value) return 0;
    if(this._value < rhs._value) return -1;
    else return 1;
  }

  public int opCmp(long rhs) {
    if(this._value == rhs) return 0;
    if(this._value < rhs) return -1;
    else return 1;
  }

  public bool opEquals(SimTime rhs) {
    return _value == rhs._value;
  }

  public bool opEquals(long rhs) {
    return _value == rhs;
  }

  public SimTime opBinary(string op)(SimTime rhs)
    if(op == "+") {
      // import std.exception;	// enforce
      auto result = this._value + rhs._value;
      // enforce to make sure that there has not been any long overflow
      // enforce(result >= this._value);
      return SimTime(result);
    }

  public SimTime opBinary(string op)(SimTime rhs)
    if(op == "-") {
      // import std.exception;	// enforce
      // enforce that rhs is not greater
      // -- since a long can not hold a negative number
      // enforce(rhs._value <= this._value);
      auto result = this._value - rhs._value;
      return SimTime(result);
    }

  public SimTime opBinary(string op, T)(T rhs)
    if(isIntegral!T && op == "*") {
      import std.exception;	// enforce
      auto result = this._value * rhs;
      // enforce to make sure that there has not been any long overflow
      // enforce(result >= this._value);
      return SimTime(result);
    }

  public SimTime opBinaryRight(string op, T)(T rhs)
    if(isIntegral!T && op == "*") {
      import std.exception;	// enforce
      auto result = this._value * rhs;
      // enforce to make sure that there has not been any long overflow
      // enforce(result >= this._value);
      return SimTime(result);
    }

  public SimTime opBinary(string op, T)(T rhs)
    if(isIntegral!T && op == "/") {
      import std.exception;	// enforce
      auto result = this._value / rhs;
      // enforce to make sure that there has not been any long overflow
      // enforce(result >= this._value);
      return SimTime(result);
    }

  public SimTime opBinaryRight(string op, T)(T rhs)
    if(isIntegral!T && op == "/") {
      import std.exception;	// enforce
      auto result = this._value / rhs;
      // enforce to make sure that there has not been any long overflow
      // enforce(result >= this._value);
      return SimTime(result);
    }

  public T to(T)()
    if(is(T == string)) {
      import std.conv;
      return "#" ~ to!string(_value);
    }

  public T to(T)()
    if(is(T == long)) {
      return _value;
    }

  public bool isZero() {
    if(_value is 0) return true;
    else return false;
  }

  alias to!string toString;
}

// alias SimTime SimTime;
immutable SimTime DELTA = SimTime(0L);

immutable SimTime MAX_SIMULATION_TIME = SimTime(long.max);
