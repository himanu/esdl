// Copyright: Coverify Systems Technology 2013 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

import std.stdio;
import esdl.base.core;

class RootWrap: RootEntity {
  void proc1() {
    import std.stdio;
    writeln("********** Task from RootEntity **********");
  }
  Task!proc1 taskP;

}

void main()
{
  // top level module
  auto theRootEntity = new RootWrap;
  theRootEntity.elaborate("theRootEntity");
  theRootEntity.simulate();
  // theRootEntity.terminate();
  // import std.stdio;
  // writeln("All Done");
}
