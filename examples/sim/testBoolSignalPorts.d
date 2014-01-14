// Copyright: Coverify Systems Technology 2013 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

import esdl.base.core;
import esdl.base.comm;

class Sink: Entity
{
  Input!bool sigIn;

  void sigSink()
  {
    import std.stdio;
    for(int i=0; i != 4; ++i)
      {
  	wait(2.nsec);
  	bool a = sigIn;// .read();
  	writeln("Reading from Sig: ", sigIn.getFullName, ": ", a);
      }
  }
  Task!sigSink sink;
}

class MonNeg: Entity
{
  Input!bool sigIn;

  void sigMon()
  {
    import std.stdio;
    for(int i=0; i != 10; ++i)
      {
  	wait(sigIn.posedge);
  	// sigIn.wait();
  	bool a = sigIn; // .read();
  	writeln(getSimTime, " Monitoring posedge Sig: ",
		sigIn.getFullName, ": ", a);
      }
  }
  Task!sigMon mon;
}

class MonPos: Entity
{
  Input!bool sigIn;

  void sigMon()
  {
    import std.stdio;
    for(int i=0; i != 10; ++i)
      {
  	wait(sigIn.negedge);
  	// sigIn.wait();
  	bool a = sigIn; // .read();
  	writeln(getSimTime, " Monitoring negedge Sig: ",
		sigIn.getFullName, ": ", a);
      }
  }
  Task!sigMon mon;
}

class Source: Entity
{
  Output!bool sigOut;

  void sigSource()
  {
    import std.stdio;
    for(int i=0; i != 8; ++i)
      {
	bool val;
	if(i % 2 == 0)
	  val = 1;
	else
	  val = 0;
	sigOut = val;
	writeln("Just wrote: ", val);
	writeln("Port Value: ", sigOut);
	wait(1.nsec);
      }
  }

  Task!sigSource source;
}

class Foo: Entity {
  Signal!bool sig;

  Inst!MonPos monpos;
  Inst!MonNeg monneg;
  Inst!Source source;
  Inst!Sink sink;

  override void doConnect()
  {
    // sigOut(sig);
    // sigIn(sig);
    // writeln("Connecting Ports");
    source.sigOut(sig);
    sink.sigIn(sig);
    monpos.sigIn(sig);
    monneg.sigIn(sig);
  }

      
  override void doConfig() {
    timePrecision = 10.psec;
    timeUnit = 100.psec;
  }

}

class Sim: RootEntity {

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
  
  Sim theRootEntity = new Sim("theRootEntity");
  theRootEntity.elaborate();
  theRootEntity.simulate(1000.nsec);
  // theRootEntity.terminate();
  return 0;
}
