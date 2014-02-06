// Copyright: Coverify Systems Technology 2013 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

import std.stdio;
import esdl.data.rand;
import esdl.data.obdd;
import esdl.data.bvec;

class Foo: Randomizable
{
  @rand int roo;
}


enum AhbMode: ubyte
  {   BURST = 0,
      BURST_WRAP = 1,
      SINGLE = 2,
      IDLE = 64,
      NONE,
      }

class Bar: Foo
{
  // mixin(_esdl__randomizable());

  // private @rand!(16) ushort bob[];
  private @rand ubyte pop;
  private @rand ubyte bro;

  private @rand AhbMode mode;
  
  byte foo = 0;

  void display() {
    import std.stdio;
    writeln("bro: ", bro, " pop: ", pop, " foo: ", foo, " mode: ", mode);
  }

  override void pre_randomize() {
    foo++;
  }

  // void post_randomize() {
  //   writeln("Post Randomize Called");
  // }

  Constraint! q{
    pop > bro;
    pop < 8;
    mode < 7;
    foo == 2 -> pop == 4;
    pop <= 4 -> bro == 0;
  } cst01;
}

void main()
{
  auto foo = new Bar;
  for (size_t i=0; i != 10; ++i)
    {
      foo.randomize();
      foo.display();
    }
}
