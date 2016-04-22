// Written in the D programming language.

// Copyright: Coverify Systems Technology 2011 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

// This file is part of esdl.

import std.bitmanip;

// A tightly packed fixed width vector of bits
struct VcdVec(size_t N)
{

  static if(N <= 8)                     private alias ubyte  store_t;
  else static if(N <= 16)               private alias ushort store_t;
  else static if(N <= 32)               private alias uint   store_t;
  else static if(size_t.sizeof*8 == 32) private alias uint   store_t;
    else                                private alias ulong  store_t;

  private enum size_t STORESIZE =
    (8*StoreT.sizeof+N-1)/(8*StoreT.sizeof);

  private store_t[STORESIZE] _aval;
  private store_t[STORESIZE] _bval;

  auto setBit(size_t i, char b) {
    static if(STORESIZE == 1) {
      switch(b) {
      case '0':
	this._aval[0] &= ~(1L << i);
	this._bval[0] &= ~(1L << i);
	break;
      case '1':
	this._aval[0] |=  (1L << i);
	this._bval[0] &= ~(1L << i);
	break;
      case 'X':
      case 'x':
	this._aval[0] &= ~(1L << i);
	this._bval[0] |=  (1L << i);
	break;
      case 'Z':
      case 'z':
	this._aval[0] |=  (1L << i);
	this._bval[0] |=  (1L << i);
	break;
      default:
	assert(false, "Illegal character: " ~ b);
      }
    }
    else {
      switch(b) {
      case '0':
	btr((cast(size_t*) _aval.ptr), i);
	btr((cast(size_t*) _bval.ptr), i);
	break;
      case '1':
	bts((cast(size_t*) _aval.ptr), i);
	btr((cast(size_t*) _bval.ptr), i);
	break;
      case 'X':
      case 'x':
	btr((cast(size_t*) _aval.ptr), i);
	bts((cast(size_t*) _bval.ptr), i);
	break;
      case 'Z':
      case 'z':
	bts((cast(size_t*) _aval.ptr), i);
	bts((cast(size_t*) _bval.ptr), i);
	break;
      default:
	assert(false, "Illegal character: " ~ b);
      }
    }
  }
}
