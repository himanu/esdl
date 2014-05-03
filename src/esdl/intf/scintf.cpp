#include <systemc>
#include <math.h>
// this function will return an integer representing logarithm of time resolution being used in systemc

// uses memoization
extern "C" char systemc_time_resolution() {
  static char result = 0;
  static bool memoized = false;
  if(memoized) return result;
  double res = log10(sc_core::sc_get_time_resolution().to_seconds());
  // we are adding
  if(sc_core::sc_start_of_simulation_invoked()) {
    memoized = true;
    result = static_cast<char>(floor(res + 0.5));
  }
  return static_cast<char>(floor(res + 0.5));
}
