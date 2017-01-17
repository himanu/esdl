// Written in the D programming language.

// Copyright: Coverify Systems Technology 2016
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

module esdl.data.cover;		// Coverage

// mixin template Coverage {
// }

struct CoverPoint(alias t, string BINS="") {
  import std.traits: isIntegral;
  import esdl.data.bvec: isBitVector;
  pragma(msg, __traits(parent, t));
  alias T = typeof(t);
  static assert(isIntegral!T || isBitVector!T || is(T: bool),
		"Only integral, bitvec, or bool values can be covered."
		~ " Unable to cover a value of type: " ~ T.stringof);
  // the number of bins and which one is hit is made out by the
  // sample function
  int[] bins;    // We keep a count of how many times a bin is hit
}

class CoverGroup {		// Base Class
}

public void sample(G, V...)(G g, V v) if (is(G: CoverGroup)) {
  // navigate through the class elements of G to know the CoverPoint
  // instantiations as well as any Integral/BitVector instances
  sample_args!(0)(g, v);
  // Now look for all the coverpoints
 }

private void sample_args(int I, G, V...)(G g, V v) {
  import std.traits: isAssignable;
  static if (V.length == 0) {
    return;
  }
  else {
    alias VAL_TUPLE = getIntElements!G;
    alias N = VAL_TUPLE[I];
    static assert (isAssignable!(typeof(G.tupleof[N]), V[0]),
		   "Method sample called with argument of type " ~
		   V[0].stringof ~ " at position " ~ I.stringof ~
		   " is not assignable to type " ~
		   typeof(G.tupleof[N]).stringof);
    static if (I == 0) {
      static assert (VAL_TUPLE.length >= V.length,
		     "Method sample called with " ~ V.length.stringof ~
		     " arguments, while it can take only " ~
		     VAL_TUPLE.length.stringof ~
		     " arguments for covergroup of type: " ~ G.stringof);
    }
    g.tupleof[N] = v[0];
    sample_args!(I+1)(g, v[1..$]);
  }
}

// return a tuple of integral elements
private template getIntElements(G, int N=0, I...) {
  import std.traits: isIntegral;
  import esdl.data.bvec: isBitVector;
  static if (N == G.tupleof.length) {
    enum getIntElements = I;
  }
  else {
    alias T = typeof(G.tupleof[N]);
    static if (isBitVector!T || isIntegral!T || is(T == bool)) {
      enum getIntElements = getIntElements!(G, N+1, I, N);
    }
    else {
      enum getIntElements = getIntElements!(G, N+1, I);
    }
  }
}

private template nthIntElement(alias g, int N, int I=0) {
  import std.traits: isIntegral;
  import esdl.data.bvec: isBitVector;
  static assert(I < g.tupleof.length);
  alias T = typeof(g.tupleof[I]);
  static if (isBitVector!T || isIntegral!T || is(T == bool)) {
    static if (N == 0) {
      enum nthIntElement = g.tupleof[I];
    }
    else {
      enum nthIntElement = nthIntElement!(g, N-1, I+1);
    }
  }
  else {
      enum nthIntElement = nthIntElement!(g, N, I+1);
  }
}

private template countIntElements(G, int COUNT=0, int I=0) {
  import std.traits: isIntegral;
  import esdl.data.bvec: isBitVector;
  static if (I != G.tupleof.length) {
    alias T = typeof(G.tupleof[I]);
    static if (isBitVector!T || isIntegral!T || is(T == bool)) {
      enum countIntElements = countIntElements!(G, COUNT+1, I+1);
    }
    else {
      enum countIntElements = countIntElements!(G, COUNT, I+1);
    }
  }
  else {
    enum countIntElements = COUNT;
  }
}
