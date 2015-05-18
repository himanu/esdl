// Written in the D programming language.

// Copyright: Coverify Systems Technology 2012 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

// Coerced casting to help in efficiently downcast when we are sure
// about the given objects type.

// template isRandomizable(T) { // check if T is Randomizable
//   import std.traits;
//   import std.range;
//   import esdl.data.bvec;
//   static if(isArray!T) {
//     enum bool isRandomizable = isRandomizable!(ElementType!T);
//   }
//   else
//   static if(isIntegral!T || isBitVector!T || is(T == class))
//     {
//       static if(is(T: _esdl__ConstraintBase)) {
// 	enum bool isRandomizable = false;
//       }
//       else {
// 	enum bool isRandomizable = true;
//       }
//     }
//   else {
//     bool isRandomizable = false;
//   }
// }

// template _esdl__RandAttr(T, int N, int I)
// {
//   import std.typetuple;		// required for Filter
//   alias U = _esdl__upcast!(T, N);
//   alias _esdl__RandAttr = Filter!(_esdl__isAttrRand,
// 				   __traits(getAttributes, U.tupleof[I]));
// }

