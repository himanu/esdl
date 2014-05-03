// Copyright: Coverify Systems Technology 2014 -
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

// SystemC uses namespaces sc_core etc
// Pending DIP61 implementation in dlang we will have to write
// wrappers over some systemc functions to make them accessible from
// dlang. Once DIP61 is implemented, we will refactor this module

module esdl.intf.systemc;

version(COSIM_SYSTEMC) {
  extern(C) byte systemc_time_resolution();
  
  import esdl.base.core: simulateAllRoots;
  import esdl.base.time: Time;

  extern(C) void esdlSimulateFor(long t) {
    Time time_ = Time(t, systemc_time_resolution());
    simulateAllRoots(time_);
  }
}
