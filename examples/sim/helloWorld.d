// Copyright: Coverify Systems Technology 2013 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

import esdl.base;

class Hello: Entity {
  void sayHello() {
    import std.stdio: writeln;
    writeln("Hello World from: ", getFullName(), ":",
	    getRootEntity.getTimePrecision().normalize());
  }
  Task!sayHello greet[2];
}

class VlangWorld: RootEntity {
  this(string name) {
    super(name);
  }
  override void doConfig() {
    timePrecision = 1.psec;
    timeUnit = 10.nsec;
  }
  Hello hello[2];
}

void main() {
  auto theRoot = new VlangWorld("theRoot");
  theRoot.elaborate();
  theRoot.simulate(100.nsec);
}
