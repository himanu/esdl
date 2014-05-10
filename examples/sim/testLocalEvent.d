// Copyright: Coverify Systems Technology 2013 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

import std.stdio;
import esdl.base.core;
import esdl.base.comm;

class Foo: Entity {
  Event e1, e2;
  
  void hello() {
    for (size_t i=0; i!=1000000; ++i)
      {
	// Event e0, e5;
	// e0.init();
	// e5.init();
	// Event e10 = e0 | e5;
	// Event e4 = e2 | e1;
	// e1.notify(0);
	e2.notify(1);

	wait(e2);
	// Event e3 = e1 | e2;
	// Event e6 = e0 & e5;
      }
  }

  Task!hello helloWorld;
  // Process!hello helloProcess;
      
  override void doConfig() {
    timePrecision = 10.psec;
    timeUnit = 100.psec;
  }

}

class Sim: RootEntity {

  this(string name) {
      super(name);
    }
  Inst!Foo[1] top;
  override void doBuild() {
    // srandom(0);
  }

  override void doConfig() {
    timeUnit = 100.psec;
    timePrecision = 10.psec;
  }
}

void main()
{
  // top level module
  Sim theRootEntity = new Sim("theRootEntity");
  theRootEntity.elaborate();
  theRootEntity.simulateUpto(1000000.nsec);
  // theRootEntity.terminate();
}
