// Copyright: Coverify Systems Technology 2013 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>
import esdl.base;

@parallelize(1)
class Hello: Entity {
  void sayHello() {
    import std.stdio: writeln;
    writeln("Hello World from: ", Process.self.getFullName());
  }
  Task!sayHello greet[2];
  Worker!sayHello greetWorker[2];
}

// @timeUnit(1.nsec)
// @timePrecision(1.psec)
class VlangWorld: RootEntity {
  this(string name) {
    super(name);
  }
  Hello hello[2];
}

void main() {
  auto theRoot = new VlangWorld("theRoot");
  theRoot.elaborate();
  theRoot.simulate(100.nsec);
}
