// Written in the D programming language.

// Copyright: Coverify Systems Technology 2012 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

// Synchronized containers
//
module esdl.data.sync;

class SyncAssoc(KEY=Object, VAL=Object)
{

  protected VAL[KEY] _assoc;

  // Allow no aliasing, since aliasing leaks out the original assoc
  // array thus putting concurrency in peril
  // For this aliasing
  // @property public ref auto get_assoc() {
  //   synchronized(this) {
  //     return _assoc;
  //   }
  // }

  // alias get_assoc this;

  // Function: new
  //
  // Creates a new assoc with the given ~name~.

  int opApply(int delegate(ref KEY, ref VAL) dg) {
    synchronized(this) {
      int result = 0;
      auto keys = _assoc.keys;
      for (size_t i = 0; i < keys.length; ++i) {
	result = dg(keys[i], _assoc[keys[i]]);
	if (result) break;
      }
      return result;
    }
  }

  int opApply(int delegate(ref VAL) dg) {
    synchronized(this) {
      int result = 0;
      auto keys = _assoc.keys;
      for (size_t i = 0; i < keys.length; ++i) {
	result = dg(_assoc[keys[i]]);
	if (result) break;
      }
      return result;
    }
  }

  // We delibrately do not return a pointer from opBinaryRight since
  // that pointer can in that case escape the concurrency guards
  bool opBinaryRight(string OP)(KEY key) if(OP == "in") {
    synchronized(this) {
      if(key in _assoc) return true;
      else return false;
    }
  }

  KEY[] keys() {
    synchronized(this) {
      return _assoc.keys;
    }
  }

  VAL[] values() {
    synchronized(this) {
      return _assoc.values;
    }
  }

  VAL get(KEY key, lazy VAL defVal) {
    synchronized(this) {
      return _assoc.get(key, defVal);
    }
  }

  void opIndexAssign(VAL val, KEY key) {
    synchronized(this) {
      _assoc[key] = val;
    }
  }

  void opIndexOpAssign(string OP)(VAL val, KEY key) {
    synchronized(this) {
      // static if(isIntegral!VAL) {
      static if(OP == "+")   {_assoc[key] += val;}
      static if(OP == "-")   {_assoc[key] -= val;}
      static if(OP == "*")   {_assoc[key] *= val;}
      static if(OP == "/")   {_assoc[key] /= val;}
      static if(OP == "%")   {_assoc[key] %= val;}
      static if(OP == "^^")  {_assoc[key] ^^= val;}
      static if(OP == "&")   {_assoc[key] &= val;}
      static if(OP == "|")   {_assoc[key] |= val;}
      static if(OP == "^")   {_assoc[key] ^= val;}
      static if(OP == "<<")  {_assoc[key] <<= val;}
      static if(OP == ">>")  {_assoc[key] >>= val;}
      static if(OP == ">>>") {_assoc[key] >>>= val;}
      // }
      static if(OP == "~")   {_assoc[key] ~= val;}
    }
  }

  VAL[KEY] dup() {
    synchronized(this) {
      VAL[KEY] retval = _assoc.dup;
      return retval;
    }
  }

  void opAssign(VAL[KEY] assoc) {
    synchronized(this) {
      _assoc = assoc;
    }
  }

  VAL opIndex(KEY key) {
    synchronized(this) {
      return _assoc[key];
    }
  }

  public size_t length() const {
    synchronized(this) {
      return _assoc.length;
    }
  }
  alias length num;

  // Function: delete
  //
  // Removes the item with the given ~key~ from the assoc.

  public void remove (KEY key) {
    synchronized(this) {
      _assoc.remove(key);
    }
  }

  public void remove () {
    synchronized(this) {
      _assoc = null;
    }
  }

  public void clear () {
    synchronized(this) {
      _assoc = null;
    }
  }

  public void destroy () {
    synchronized(this) {
      _assoc = null;
    }
  }
}

class SyncQueue (T=Object)
{
  import esdl.data.queue;

  // No this aliasing -- this aliasing is making the queue object
  // escape from the synchronization guards

  // // For this aliasing
  // @property public ref auto get_queue() {
  //   synchronized(this) {
  //     return _queue;
  //   }
  // }

  // // Some DMD bug is not allowing this alias here
  // alias get_queue this;

  protected Queue!T _queue;

  alias length size;

  public T opIndex(size_t index) {
    synchronized(this) {
      return _queue[index];
    }
  }

  public T opIndexAssign(T item, size_t index) {
    synchronized(this) {
      _queue[index] = item;
      return item;
    }
  }

  void opIndexOpAssign(string OP)(T item, size_t index) {
    synchronized(this) {
      // static if(isIntegral!T) {
      static if(OP == "+")   {_queue[index] += item;}
      static if(OP == "-")   {_queue[index] -= item;}
      static if(OP == "*")   {_queue[index] *= item;}
      static if(OP == "/")   {_queue[index] /= item;}
      static if(OP == "%")   {_queue[index] %= item;}
      static if(OP == "^^")  {_queue[index] ^^= item;}
      static if(OP == "&")   {_queue[index] &= item;}
      static if(OP == "|")   {_queue[index] |= item;}
      static if(OP == "^")   {_queue[index] ^= item;}
      static if(OP == "<<")  {_queue[index] <<= item;}
      static if(OP == ">>")  {_queue[index] >>= item;}
      static if(OP == ">>>") {_queue[index] >>>= item;}
      // }
      static if(OP == "~")   {_queue[index] ~= item;}
    }
  }

  public int opApplyReverse(int delegate(ref size_t, ref const T) dg) const {
    synchronized(this) {
      for(size_t idx = this.length-1; idx < ptrdiff_t.max; --idx) {
	if(int r = dg(idx, this._queue[idx])) {
	  return r;
	}
      }
      return 0;
    }
  }

  public int opApplyReverse(int delegate(const ref T) dg) const {
    synchronized(this) {
      for(size_t idx = this.length-1; idx < ptrdiff_t.max; --idx) {
	if(int r = dg(this._queue[idx])) {
	  return r;
	}
      }
      return 0;
    }
  }

  public int opApplyReverse(int delegate(ref size_t, ref T) dg) {
    synchronized(this) {
      for(size_t idx = this.length-1; idx < ptrdiff_t.max; --idx) {
	if(int r = dg(idx, this._queue[idx])) {
	  return r;
	}
      }
      return 0;
    }
  }

  public int opApplyReverse(int delegate(ref T) dg) {
    synchronized(this) {
      for(size_t idx = this.length-1; idx < ptrdiff_t.max; --idx) {
	if(int r = dg(this._queue[idx])) {
	  return r;
	}
      }
      return 0;
    }
  }

  public int opApply(int delegate(ref size_t, ref const T) dg) const {
    synchronized(this) {
      for(size_t idx = 0; idx < this.length; ++idx) {
	if(int r = dg(idx, this._queue[idx])) {
	  return r;
	}
      }
      return 0;
    }
  }

  public int opApply(int delegate(ref size_t, ref T) dg) {
    synchronized(this) {
      for(size_t idx = 0; idx < this.length; ++idx) {
	if(int r = dg(idx, this._queue[idx])) {
	  return r;
	}
      }
      return 0;
    }
  }

  public int opApply(int delegate(const ref T) dg) const {
    synchronized(this) {
      for(size_t idx = 0; idx < this.length; ++idx) {
	if(int r = dg(this._queue[idx])) {
	  return r;
	}
      }
      return 0;
    }
  }

  public int opApply(int delegate(ref T) dg) {
    synchronized(this) {
      for(size_t idx = 0; idx < this.length; ++idx) {
	if(int r = dg(this._queue[idx])) {
	  return r;
	}
      }
      return 0;
    }
  }

  public size_t length () const {
    synchronized(this) {
      return _queue.length();
    }
  }

  public void insert (ptrdiff_t index, T item) {
    synchronized(this) {
      _queue.insert(index, item);
    }
  }

  public void remove (size_t index) {
    synchronized(this) {
      _queue.remove(index);
    }
  }

  public void remove () {
    synchronized(this) {
      _queue.clear();
    }
  }

  public void clear () {
    synchronized(this) {
      _queue.clear();
    }
  }

  public void popFront(ref T pop) {
    synchronized(this) {
      _queue.popFront(pop);
    }
  }

  public void removeFront() {
    synchronized(this) {
      _queue.removeFront();
    }
  }

  public void popBack(ref T pop) {
    synchronized(this) {
      _queue.popBack(pop);
    }
  }

  public void removeBack() {
    synchronized(this) {
      _queue.removeBack();
    }
  }

  public void pushFront(T item) {
    synchronized(this) {
      _queue.pushFront(item);
    }
  }

  public void pushBack(T item) {
    synchronized(this) {
      _queue.pushBack(item);
    }
  }

  public void opOpAssign(string op, R)(R other)
    if(op == "~" && is(R unused: T)) {
      synchronized(this) {
	this.pushBack(other);
      }
    }
}

class SyncArray (T=Object)
{
  // No this aliasing -- this aliasing is making the array object
  // escape from the synchronization guards

  // // For this aliasing
  // @property public ref auto get_array() {
  //   synchronized(this) {
  //     return _array;
  //   }
  // }

  // // Some DMD bug is not allowing this alias here
  // alias get_array this;

  protected T[] _array;

  alias length size;

  public T opIndex(size_t index) {
    synchronized(this) {
      return _array[index];
    }
  }

  public T opIndexAssign(T item, size_t index) {
    synchronized(this) {
      _array[index] = item;
      return item;
    }
  }

  void opIndexOpAssign(string OP)(T item, size_t index) {
    synchronized(this) {
      // static if(isIntegral!T) {
      static if(OP == "+")   {_queue[index] += item;}
      static if(OP == "-")   {_queue[index] -= item;}
      static if(OP == "*")   {_queue[index] *= item;}
      static if(OP == "/")   {_queue[index] /= item;}
      static if(OP == "%")   {_queue[index] %= item;}
      static if(OP == "^^")  {_queue[index] ^^= item;}
      static if(OP == "&")   {_queue[index] &= item;}
      static if(OP == "|")   {_queue[index] |= item;}
      static if(OP == "^")   {_queue[index] ^= item;}
      static if(OP == "<<")  {_queue[index] <<= item;}
      static if(OP == ">>")  {_queue[index] >>= item;}
      static if(OP == ">>>") {_queue[index] >>>= item;}
      // }
      static if(OP == "~")   {_queue[index] ~= item;}
    }
  }

  public size_t length () const {
    synchronized(this) {
      return _array.length;
    }
  }

  public int opApplyReverse(int delegate(ref size_t, ref const T) dg) const {
    synchronized(this) {
      for(size_t idx = this.length-1; idx < ptrdiff_t.max; --idx) {
	if(int r = dg(idx, this._queue[idx])) {
	  return r;
	}
      }
      return 0;
    }
  }

  public int opApplyReverse(int delegate(const ref T) dg) const {
    synchronized(this) {
      for(size_t idx = this.length-1; idx < ptrdiff_t.max; --idx) {
	if(int r = dg(this._queue[idx])) {
	  return r;
	}
      }
      return 0;
    }
  }

  public int opApplyReverse(int delegate(ref size_t, ref T) dg) {
    synchronized(this) {
      for(size_t idx = this.length-1; idx < ptrdiff_t.max; --idx) {
	if(int r = dg(idx, this._queue[idx])) {
	  return r;
	}
      }
      return 0;
    }
  }

  public int opApplyReverse(int delegate(ref T) dg) {
    synchronized(this) {
      for(size_t idx = this.length-1; idx < ptrdiff_t.max; --idx) {
	if(int r = dg(this._queue[idx])) {
	  return r;
	}
      }
      return 0;
    }
  }

  public int opApply(int delegate(ref size_t, ref const T) dg) const {
    synchronized(this) {
      for(size_t idx = 0; idx < this.length; ++idx) {
	if(int r = dg(idx, this._queue[idx])) {
	  return r;
	}
      }
      return 0;
    }
  }

  public int opApply(int delegate(ref size_t, ref T) dg) {
    synchronized(this) {
      for(size_t idx = 0; idx < this.length; ++idx) {
	if(int r = dg(idx, this._queue[idx])) {
	  return r;
	}
      }
      return 0;
    }
  }

  public int opApply(int delegate(const ref T) dg) const {
    synchronized(this) {
      for(size_t idx = 0; idx < this.length; ++idx) {
	if(int r = dg(this._queue[idx])) {
	  return r;
	}
      }
      return 0;
    }
  }

  public int opApply(int delegate(ref T) dg) {
    synchronized(this) {
      for(size_t idx = 0; idx < this.length; ++idx) {
	if(int r = dg(this._queue[idx])) {
	  return r;
	}
      }
      return 0;
    }
  }

  public void insert (ptrdiff_t index, T item) {
    synchronized(this) {
      auto pre = _array[0..index];
      auto post = _array[index..$];
      _array =  pre ~ item ~ post;
    }
  }

  public void remove (size_t index) {
    synchronized(this) {
      auto pre = _array[0..index];
      auto post = _array[index+1..$];
      _array = pre ~ post;
    }
  }

  public void remove () {
    synchronized(this) {
      _array = [];
    }
  }

  public void clear () {
    synchronized(this) {
      _array = [];
    }
  }

  public void popFront(ref T pop) {
    synchronized(this) {
      pop = _array[0];
      _array = _array[1..$];
    }
  }

  public void removeFront() {
    synchronized(this) {
      _array = _array[1..$];
    }
  }

  public void popBack(ref T pop) {
    synchronized(this) {
      pop = _array[$-1];
      _array = _array[0..$-1];
    }
  }

  public void removeBack() {
    synchronized(this) {
      _array = _array[0..$-1];
    }
  }

  public void pushBack(T item) {
    synchronized(this) {
      _array ~= item;
    }
  }

  public void opOpAssign(string op, R)(R other)
    if(op == "~" && is(R unused: T)) {
      synchronized(this) {
	_array ~= other;
      }
    }
}
