// Copyright: Coverify Systems Technology 2013 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

import std.stdio;
import esdl.base.core;
import esdl.base.comm;

@parallelize
class TrafficLight: Entity
{
  // mixin(entityMixin());

  enum POLES = 4;
 private:
  Event red[POLES];
  Event yellow[POLES];
  Event green[POLES];

  int count = 0;

  override public void doConfig()
  {
    timeUnit = 1.nsec;
  }

  void susLight() {
    wait(1000);
    import std.stdio;
    writeln("Suspending Operations");
    foreach(l; tLightTT) {
      l.suspend;
    }
    wait(200);
    import std.stdio;
    writeln("Resuming Operations");
    foreach(l; tLightTT) {
      l.resume;
    }

  }

  void testPhase() {
    writeln("Stage: ", Process.self.stage);
  }

  void light() {
    writeln(getSimTime);
    // wait((cast(Dummy) getParent).e);
    auto index = Process.self.taskIndices[0];
    if(index != 0)
      wait(green[index]);
    // writeln("Time unit is: ", timeUnit());
    while(true)
      {
	lockStage();
	unlockStage();
	writeln(getSimTime, ": Red -> Green ", index, " -- ",
		Process.self.getFullName());
	yellow[index].notify(20);
	// writeln("I am here: ", index);
	red[index].notify(25);
	wait(yellow[index]);
	writeln("Green -> Yellow ", index);
	wait(red[index]);
	writeln("Yellow -> Red ", index);
	green[(index + 1)%POLES].notify();
	wait(green[index]);
	synchronized(this) {
	  ++count;
	}
      }
  }
  public int x;
  // Task!("light()",0)  tLightTT[POLES];
  Task!(light, 5)  tLightTT[POLES];
  Task!(testPhase, -1)  test_1;
  Task!(testPhase, 10)  test2;
  Task!(testPhase, -3)  test3;
  Task!(testPhase, -4)  test4;
  Task!(testPhase, -5)  test5;

  Task!(susLight, 5) suspendLight;
}

@parallelize
class TrafficLightWrapper: TrafficLight {}

@parallelize
class Dummy: Entity
{
  // Event e;
  // void etrigger()
  // {
  //   wait(990.nsec);
  //   e.notify();
  // }
  // Task!(etrigger, 0) trigE;

  private TrafficLightWrapper[1] traffic;
}

class TrafficRoot: RootEntity
{
  @parallelize Inst!Dummy[16] dummy;

  this(string name) {
    super(name);
  }

  override void doConfig() {
    // writeln("Configuring TrafficLight");
    if(this.getName == "theRoot") {
      timeUnit = 100.psec;
      timePrecision = 10.psec;
      // threadCount = 4;
    } else {
      timeUnit = 1000.psec;
      timePrecision = 100.psec;
      // threadCount = 4;
    }
  }
}

void main()
{
  import std.stdio;

  // top level module
  TrafficRoot theRoot = new TrafficRoot("theRoot");
  theRoot.forkElab();
  theRoot.joinElab();
  // theRoot.simulate(100.nsec);
  // theRoot.joinSim();
  // theRoot.simulate(200.nsec);
  // theRoot.joinSim();
  // theRoot.simulate(1000.nsec);
  // theRoot.joinSim();
  // theRoot.simulate(2000.nsec);
  // theRoot.joinSim();

  theRoot.forkSim(2000.nsec);
  theRoot.joinSim();

  // for (size_t i=1; i!=20; ++i) {
  //   theRoot.forkSim((i*100).nsec);
  //   theRoot.joinSim();
  // }
  // theRoot.terminate();
}
