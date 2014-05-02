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
  Event red[POLES];
  Event yellow[POLES];
  Event green[POLES];

  int count = 0;

  override void doConfig() {
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
    // wait(0);
    if(Process.self.stage == 10) {
      wait(0);
    }
    writeln("Stage: ", Process.self.stage);
  }

  void light() {
    // lockStage();
    writeln(getSimTime);
    // wait((cast(Dummy) getParent).e);
    auto index = Process.self.taskIndices[0];
    if(index != 0)
      wait(green[index]);
    // writeln("Time unit is: ", timeUnit());
    while(true)
      {
	// lockStage();
	// unlockStage();
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
	  // if(count == 1) getRoot.abortTree();
	}
      }
  }
  public int x;
  // Task!("light()",0)  tLightTT[POLES];
  Task!(light, 5)  tLightTT[POLES];
  Task!(testPhase, -1)  test_1;
  Task!(testPhase, 10)  test2;
  Task!(testPhase, 11)  test11;
  Task!(testPhase, -3)  test3;
  Task!(testPhase, -4)  test4;
  Task!(testPhase, -5)  test5;

  Task!(susLight, 5) suspendLight;
}

@parallelize
class TrafficLightWrapper: TrafficLight {}

class Dummy: Entity
{
  // Event e;
  // void etrigger()
  // {
  //   wait(990.nsec);
  //   e.notify();
  // }
  // Task!(etrigger, 0) trigE;

  private TrafficLightWrapper traffic;
}

class TrafficRoot: RootEntity
{
  @parallelize Inst!Dummy[10] dummy;

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
  theRoot.elaborate();
  // theRoot.waitElab();
  // theRoot.simulate(100.nsec);
  // theRoot.waitSim();
  // theRoot.simulate(200.nsec);
  // theRoot.waitSim();
  // theRoot.simulate(1000.nsec);
  // theRoot.waitSim();
  // theRoot.simulate(2000.nsec);
  // theRoot.waitSim();

  // theRoot.doSim(25.nsec);
  // theRoot.waitSim();
  theRoot.simulateUpto(25.nsec);
  // theRoot.simulate(2500.nsec);
  // theRoot.simulate(0.nsec);
  theRoot.finish();
  theRoot.simulate();
  // theRoot.terminate();
  // theRoot.simulate(250.nsec);

  // for (size_t i=1; i!=20; ++i) {
  //   theRoot.doSim((i*100).nsec);
  //   theRoot.waitSim();
  // }
  // theRoot.terminate();
}
