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
  mixin(randomization);
  @rand int roo;
}


enum AhbMode: ubyte {BURST, BURST_WRAP, SINGLE, IDLE, NONE}

class Bar: Foo
{
   mixin(randomization);

  // private @rand!(16) ushort bob[];
  private @rand ubyte pop;
  private @rand ubyte bro;

  private @rand AhbMode mode;
  
  @rand ubyte foo;

  void display() {
    import std.stdio;
    writeln("bro: ", bro, " pop: ", pop, " foo: ", foo, " mode: ", mode);
  }

  override void pre_randomize() {
    // foo++;
  }

  // void post_randomize() {
  //   writeln("Post Randomize Called");
  // }

  Constraint! q{
    pop > bro;
    pop < 8;
    // foo < 3;
    if(mode == AhbMode.SINGLE) {
      foo[0..3] == 0;
    }
    foo == 2 -> pop == 4;
    pop <= 4 -> bro == 0;
    if(foo[3..5]) {
      bro == 5;
    }
    else {
      bro == 0;
    }
  } cst01;
}

void main()
{
  auto foo = new Bar;
  auto myMode = AhbMode.NONE;
  for (size_t i=0; i != 1000; ++i)
    {
      if(myMode == AhbMode.NONE) {
	myMode = AhbMode.BURST;
      }
      else {
	myMode++;
      }
      import std.stdio;
      writeln("mode is: ", myMode);
      foo.randomizeWith!q{mode == @1;}(0, myMode);
      foo.display();
    }
}
