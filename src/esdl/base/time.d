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

  public Time normalize() {
    if(_unit is 0) return this;
    if(_value % 1000 != 0) return this;
    else {
      Time retval;
      retval._value = this._value / 1000;
      retval._unit  = cast(TimeUnit) (this._unit + 3);
      return retval.normalize();
    }
  }
  
  public bool isZero() {
    if(_value is 0) return true;
    else return false;
  }

  public int opCmp(Time other) {
    if(other._unit > this._unit) {
      int p = other._unit - this._unit;
      if(other._value * 10L^^p > this._value) return -1;
      if(other._value * 10L^^p < this._value) return 1;
      return 0;
    }
    else {
      int p = this._unit - other._unit;
      if(other._value > this._value * 10L^^p) return -1;
      if(other._value < this._value * 10L^^p) return 1;
      return 0;
    }
  }

  public bool opEquals(Time other) {
    if(other._unit > this._unit) {
      int p = other._unit - this._unit;
      if(other._value * 10L^^p == this._value) return true;
      else return false;
    }
    else {
      int p = this._unit - other._unit;
      if(other._value == this._value * 10L^^p) return true;
      else return false;
    }
  }

  public Time opBinary(string OP)(Time rhs) {
    TimeUnit unit;
    long value;
    Time _lhs = this.normalize();
    Time _rhs = rhs.normalize();
    static if(OP = "+") {
      if(_rhs._unit <= _lhs._unit) {
	unit = _rhs._unit;
	value = _lhs._value * 10L^^(_lhs._unit - _rhs._unit) + _rhs._value;
      }
      else {
	unit = _lhs._unit;
	value = _lhs._value + _rhs._value * 10L^^(_rhs._unit - _lhs._unit);
      }
      return Time(value, unit);
    }
    else static if(OP = "-") {
	if(_rhs._unit <= _lhs._unit) {
	  unit = _rhs._unit;
	  value = _lhs._value * 10L^^(_lhs._unit - _rhs._unit) - _rhs._value;
	}
	else {
	  unit = _lhs._unit;
	  value = _lhs._value - _rhs._value * 10L^^(_rhs._unit - _lhs._unit);
	}
	return Time(value, unit);
      }
  }

  public Time opBinary(string OP)(long rhs) {
    Time _lhs = this.normalize();
    static if(OP == "*") {
      _lhs._value *= rhs;
    }
    return _lhs.normalize();
  }

  public Time opBinaryRight(string OP)(long rhs) {
    Time _lhs = this.normalize();
    static if(OP == "*") {
      _lhs._value *= rhs;
    }
    return _lhs.normalize();
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
  public Time timeUnit();
  public ulong timeScale();
}

interface TimeConfigContext: TimeContext
{
  public Time timeUnit();
  protected void timeUnit(Time t);
  protected void timePrecision(Time t);
  public ulong timeScale();
  package void timeScale(ulong t);
  public final SimTime tu(ulong val) {
    return SimTime(val*timeScale());
  }
  // SimTime Time(ulong val, TimeUnit unit);
  // SimTime Time(string unit="default")(ulong val);
  // SimTime Time(double val);
  // SimTime Time(double val, TimeUnit unit);
  public void fixTimeParameters(ulong scale = 0);

  static final string timedMixin() {
    return q{
      protected ulong _timeScale = 0;

      override protected void timeUnit(Time t) {
	synchronized(this) {
	  // do not do anything in the first pass
	  if(getRootEntity.timePrecisionSet()) {
	    if(getSimPhase() !is SimPhase.CONFIGURE) {
	      assert(false,
		     "timeUnit may only be called from"
		     " within doConfig method");
	    }
	    Time prec = getRootEntity.getTimePrecision.normalize();
	    Time tuni = t.normalize();
	    if(prec._unit > tuni._unit ||
	       ((tuni._value * 10L^^(tuni._unit - prec._unit)) %
	    	prec._value) !is 0) {
	      import std.string: format;
	      assert(false,
	    	     format("timeUnit %s incompatible with timePrecision %s",
	    		    tuni, prec));
	    }
	    _timeScale =
	      tuni._value * (10L^^(tuni._unit - prec._unit)) / prec._value;
	  }
	}
      }

      override protected void timePrecision(Time t) {
	import esdl.base.core;
	if(! getRootEntity.timePrecisionSet()) {
	  if(getSimPhase() !is SimPhase.CONFIGURE) {
	    assert(false,
		   "timePrecision should only be called from"
		   " within doConfig method");
	  }
	  setTimePrecision(t.normalize());
	}
      }
      
      override public Time timeUnit() {
	synchronized(this) {
	  return this._timeScale * getTimePrecision;
	}
      }

      override public ulong timeScale() {
	synchronized(this)
	  {
	    return this._timeScale;
	  }
      }

      package void timeScale(ulong t) {
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
      if(getTimePrecisionOrder <= unit) {
	this._value = val * 10L ^^(unit - getTimePrecisionOrder);
      }
      else {
	this._value = val / 10L ^^(getTimePrecisionOrder - unit);
      }
    }
  }

  public this(TimeConfigContext context, Time t) {
    synchronized(context) {
      if(getTimePrecisionOrder <= t._unit) {
	this._value = t._value * 10L ^^(t._unit - getTimePrecisionOrder);
      }
      else {
	this._value = t._value / 10L ^^(getTimePrecisionOrder - t._unit);
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

public void setTimePrecision(Time t) {
  _isPowerOf10(t._value) ||
    assert(false, "timePrecision takes only powers of 10 as arguments");
  import esdl.base.core: getRootEntity;
  getRootEntity.setTimePrecision(t.normalize);
}

public Time getTimePrecision() {
  import esdl.base.core: getRootEntity;
  return getRootEntity.getTimePrecision();
}

public byte getTimePrecisionOrder() {
  import esdl.base.core: getRootEntity;
  auto t = getRootEntity.getTimePrecision();
  auto retval = cast(byte)(t._unit + _log10(t._value));
  return retval;
}

private bool _isPowerOf10(ulong n) {
  if(n == 0) return false;
  if(n == 1) return true;
  if(n % 10L == 0) return _isPowerOf10(n/10L);
  else return false;
}
      
private ubyte _log10(ulong n) {
  if(n == 1) return 0;
  else return cast(ubyte)(1 + _log10(n/10L));
}

