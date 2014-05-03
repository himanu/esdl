// Copyright: Coverify Systems Technology 2013 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

import std.stdio;
import esdl.base.core;
import esdl.base.comm;
// import core.thread;

class Payload
{
  this(int load)
  {
    _load = load;
  }

  int _load;

  int load()
  {
    return _load;
  }
  
}

class Foo: Entity {
  // Event e0;
  Event e1;

  void testEvent() {
    Payload p;
    // e0.notify();
    // wait(e0);
    import std.stdio;
    writeln(getSimTime());
    wait(1);
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
  //   theRootEntity.terminate();
}
