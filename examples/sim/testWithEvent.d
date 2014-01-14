// Copyright: Coverify Systems Technology 2013 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

import std.stdio;
import esdl.base.core;
import esdl.base.comm;
import uvm.meta.misc;
// import core.thread;


class Foo: Entity {
  WithEvent!int wEvent;
  this() {
    synchronized(this) {
      wEvent = new WithEvent!int(this);
    }
  }

  void withEvent()
  {
    // e0.notify();
    // wait(e0);
    import std.stdio;
    wEvent = 5;
    if(wEvent > 4) {
      writeln("Yes, > 4");
    }
    if(wEvent < 6) {
      writeln("Yes, < 6");
    }
    if(wEvent is 5) {
      writeln("Yes, is 5");
    }
    for (int i=0; i!=20; ++i)
      {
	wEvent = i;
	wait(10);
      }
  }

  void waitEvent()
  {
    import std.stdio;
    wEvent.wait;

    while(wEvent < 10) {
      wEvent.wait;
    }
    writeln(getSimTime);

    writeln("Done waiting");
  }

  Task!withEvent test;
  Task!waitEvent wTest;

  override void doConfig() {
    timePrecision = 1.psec;
    timeUnit = 100.psec;
    // writeln("Contructor for Bar ", this.fullName);
  }
}

class Sim: RootEntity {
  this(string name)
    {
      super(name);
    }

  Inst!(Foo) top;

  override void doConfig() {
    timeUnit(100.psec);
    timePrecision(10.psec);
  }

}

void main()
{
  Sim theRootEntity = new Sim("theRootEntity");
  theRootEntity.elaborate();
  theRootEntity.simulate(1000000.nsec);
  // theRootEntity.terminate();
}
