// Copyright: Coverify Systems Technology 2013 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>
import esdl.base;
import std.stdio;

@timeUnit(1.nsec)
@timePrecision(1.psec)
@parallelize(ParallelPolicy.SINGLE)
class Foo: Entity {
  void hello() {
    writeln("Greetings from: ",
	    Process.self.getFullName());
  }
  Task!hello greet[2];
}
class Top: Entity {
  Foo foo[4];
}
void main() {
  simulate!(Top, "root");
}
