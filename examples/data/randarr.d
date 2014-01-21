// Copyright: Coverify Systems Technology 2013 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

import std.stdio;
import esdl.data.rand;
import esdl.data.obdd;
import esdl.data.bvec;

class Bar: Randomizable
{
  @rand!8 byte[] foo;
  @rand byte[8] bar;

  @rand ubyte baz = 12;

  void display() {
    writeln("foo: ", foo);
    writeln("bar: ", bar);
    writeln("baz: ", baz);
  }

  Constraint! q{
    foo.length > 2;
    baz < 16;
  } cstFooLength;

  Constraint! q{
    foreach(i, f; foo) {
      f < 64;
      f > 16;
    }
    
    foreach(i, f; bar) {
      f <= i;
      f >= 0;
    }
  } cstFoo;

}

void main()
{
  auto foo = new Bar;
  for (size_t i=0; i!=16; ++i)
    {
      foo.randomize();
      foo.display();
    }
}
