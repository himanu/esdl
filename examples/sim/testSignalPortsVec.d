// Copyright: Coverify Systems Technology 2013 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

import esdl.base.core;
import esdl.base.comm;
import esdl.data.bvec;

class Sink: Entity
{
  Input!(lvec!12) sigIn;

  void sigSink()
  {
    import std.stdio;
    for(int i=0; i != 4; ++i)
      {
	wait(2.nsec);
	lvec!12 a = sigIn;// .read();
	writeln("Reading from Sig: ", sigIn.getFullName);
      }
  }
  Task!sigSink sink;
}

class Mon: Entity
{
  Input!(lvec!12) sigIn;

  void sigMon()
  {
    import std.stdio;
    wait(1.nsec);
    for(int i=0; i != 10; ++i)
      {
	wait(sigIn);
	// sigIn.wait();
	lvec!12 a = sigIn; // .read();
	writeln("Monitoring Sig: ", sigIn.getFullName);
      }
  }
  Task!sigMon mon;
}

class Source: Entity
{
  Output!(lvec!12) sigOut;

  void sigSource()
  {
    import std.stdio;
    for(int i=0; i != 8; ++i)
      {
	wait(1.nsec);
	sigOut = cast(lvec!12) i.toBitVec;
	writefln("Just wrote: %x", i);
	writeln("Port Value: ", sigOut);
      }
  }

  Task!sigSource source;
}

class Foo: Entity {
  Signal!(lvec!12) sig;

  Inst!Mon mon;
  Inst!Source source;
  Inst!Sink sink;

  override void doConnect()
  {
    // sigOut(sig);
    // sigIn(sig);
    // writeln("Connecting Ports");
    source.sigOut(sig);
    sink.sigIn(sig);
    mon.sigIn(sig);
  }

      
  override void doConfig() {
    timePrecision = 10.psec;
    timeUnit = 100.psec;
  }

}

class TestSignalPorts: RootEntity {

  this(string name)
    {
      super(name);
    }

  Inst!Foo[1] foo;
  override void doConfig() {
    timeUnit = 100.psec;
    timePrecision = 10.psec;
    // writeln("Configure the RootEntity: ", this.getFullName);
  }
}

int main()
{
  // top level module
  // writeln("Size of EventNotice is: ", EventNotice.sizeof);
  // writeln("Size of Time is: ", Time.sizeof);
  
  TestSignalPorts test = new TestSignalPorts("test");
  test.elaborate();
  test.simulate(1000.nsec);
  // test.terminate();
  return 0;
}
