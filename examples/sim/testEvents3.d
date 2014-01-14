// Copyright: Coverify Systems Technology 2013 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

import std.stdio;
import esdl.base.core;
import esdl.base.comm;
// import core.thread;

class Foo: Entity {
  // Event e0;
  NotificationQueue!int e1;

  void testEvent()
  {
    // e0.notify();
    // wait(e0);
    import std.stdio;
    for (size_t i=0; i!=10; ++i)
      {
	e1.post(4);
	e1.post(1);
	e1.post(3);
	e1.post(3);
	e1.post(3);
	e1.post(0, 0);
	e1.post(0);
	e1.post(10.psec, 10);
	e1.post(20.psec, 20);
	e1.post(30.psec, 30);
	e1.post(40.psec, 40);
	e1.post(0.psec, 1);
	e1.post(-4);
	e1.post(-1);
	e1.post(0.psec, 0);
	e1.post(0.psec, 0);
	e1.post(0.psec, 0);
	e1.post(-3);
      }
    auto e2 = e1.delayed(SimTime(130));
    for (size_t i=0; i!=200; ++i)
      {
	auto p = observe(e2);
	writeln("time is: ", getSimTime, " data is: ", p);
      }
}

  Task!testEvent test;

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
  theRootEntity.simulate(10000.nsec);
  // theRootEntity.terminate();
}
