// Copyright: Coverify Systems Technology 2013 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

import std.stdio;
import esdl.base.core;
import esdl.base.comm;

class bore
{
}

// class Dummy: Entity
// {
//   bore bb;
//   Event eeeee;
// }

class TrafficLight: Entity
{
  uint s;
  void put() {
    // srandom(2);
    for (size_t i=0; i!=1000; ++i)
      {
	wait(10.nsec);
	import std.random;
	synchronized(this) s = urandom();
	
      }
  }
  
  void get() {
    wait(1.nsec);
    for (size_t i=0; i!=1000; ++i)
      {
	wait(10.nsec);
	import std.stdio;
	synchronized(this) writeln(s);
      }
  }
  
  Task!put p;
  Task!get g;
  // Task!(light, 0)  tLightTT[2];
  // Dummy dummy;
}


class TrafficRoot: RootEntity {
  Inst!TrafficLight traffic[2][32];
}

void main()
{
  // top level module
  TrafficRoot theRoot = new TrafficRoot;
  theRoot.elaborate("theRoot");
  theRoot.simulate(100.nsec);
  theRoot.terminate();
}

