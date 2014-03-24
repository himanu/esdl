// Written in the D programming language.

// Copyright: Coverify Systems Technology 2012 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

// This file is part of esdl.

module esdl.base.core;

// import std.concurrency: Thread;
import core.thread;

public import esdl.base.time;
public import esdl.base.comm;

// use atomicStore and atomicLoad
// This would get redundant later when share construct gets functional
// in the D compiler.

import std.traits: isArray, isIntegral;
import std.random: Random, uniform;


alias void delegate() DelegateThunk;
alias void function() FunctionThunk;

private alias void delegate(Object) DEvent;
private extern (C) void rt_attachDisposeEvent(Object h, DEvent e);
private extern (C) void rt_detachDisposeEvent(Object h, DEvent e);

/// C++ type static_cast for down-casting when you are sure
private import std.typetuple: staticIndexOf;
private import std.traits: BaseClassesTuple, ParameterTypeTuple; // required for staticIndexOf

// Coerced casting to help in efficiently downcast when we are sure
// about the given objects type.
public T staticCast(T, F)(const F from)
  if(is(F == class) && is(T == class)
     // make sure that F is indeed amongst the base classes of T
     && staticIndexOf!(F, BaseClassesTuple!T) != -1
     )
    in {
      // assert statement will not be compiled for production release
      assert((from is null) || cast(T)from !is null);
    }
body {
  return cast(T) cast(void*) from;
 }

// Base class for ESDL -- all ESDL classes will have this common parent
private interface EsdlObj {}

struct _esdl__ignore {};
struct _esdl__component {};

// This struct is used to control homw many threads can be
// simultaneously active. A value of 0 here would mean maximum
// constraint -- only one thread active down the hierarchy. Default
// value is 1 and that means that per Entity, there is one thread
// active. If a user just puts @parallelize (no arguments), that would
// result in unbounded number of threads (limited by the number of
// processors available) getting simultaneoulsy active. The UDA would
// be activated hierarchically unless the user provides another UDA
// down the hierarchy.
struct parallelize
{
  byte _parallel;
}

// All ESDL designs are pseudo static. This basically means that any
// ESDL model would be first elaborated and during the simulation, the
// elaborated design will remain static. To make it possible to
// identify different components (hierarchical as well as leaf) of the
// design, all the components should have an instance name associated
// with them. Since for any given class object in C++ as well as D
// also has symbolic names associated with them, this symbolic naming
// hierarchy should possibly be reused for maintaining instance names
// for simulation purposes. A NamedObj and associated functionality
// provide a way to reflect the compile time symbolic object names
// during the simulation. This base class also makes it possible for
// the objects to identify their parent (enclosing) objects as well as
// the root and simulator objects.

// At NamedObj level, we store only the name as a string and the
// parent object handle. The root and the simulator objects are
// identified by traversing the hierarchy up and reach a HierComp that
// keeps a reference to the root and simulator components. Since a
// plain NamedObj may not have any child objects associated with it

// Now something about the nameing conventions -- We use _esdl__ as
// prefix with the methods that need to be exposed (declared public),
// since all the metaprogramming calls are made from external template
// functions. Ideally these methods should be hidden from the end-user
// by making these private, but since that does not seem possible in
// the current scheme of things, we use such prefix.

// NamedObj is an interface and not a class. This is a design decision
// to allow a user declared entity to inherit from another class and
// to just use EntityIntf to along with entityMixin. Similarly if one
// wants to create a NamedObj, he needs to implement NamedObj
// interface and for that purpose, the namedMixin is provided.

package interface NamedObj: EsdlObj, TimeContext
{
  // Elaboration and Hierarchy related interface functions
  public void _esdl__setParent(NamedObj parent);
  public void _esdl__setName(string name);
  public NamedObj getParent();
  public string getName();
  public RootEntityIntf getRoot();
  public EsdlSimulator getSimulator();

  public final string getFullName() {
    synchronized(this) {
      if(this.getParent is this) return(this.getName); // RootEntity
      else return this.getParent.getFullName ~ "." ~ this.getName;
    }
  }

  public void _esdl__nomenclate()(string name);
  public void _esdl__nomenclate(size_t I, T)(T t, uint[] indices);

  // get the simulation phase
  // Assumes that the getRoot has been set
  public final SimPhase simPhase() {
    if(this.getSimulator) {
      return this.getSimulator.phase();
    }
    assert(false, "Not possible to know the simulation phase "
	   "without connection to the getSimulator simulator");
  }

  public static string namedMixin() {
    return q{
      static if(!__traits(compiles, _esdl__name)) {
	protected string _esdl__name;
      }

      // The default getParent will be null, but on elaboration every
      // (pseudo static) NamedObj is going to have this value set
      // after elaboration. The RootEntityIntf would have its self as
      // parent. Dynamically spawned processes with identify their
      // parent by looking at the thread that spawns the given
      // thread.
      static if(!__traits(compiles, _esdl__parent)) {
	@_esdl__ignore protected NamedObj _esdl__parent;
      }

      // Returns null if the process is not a child process of another
      // process or of a routine. Otherwise return the Routine or the
      // Process
      private static NamedObj _esdl__getParentProc() {
	auto p = Process.self;
	if(p !is null) {return p;}
	else return Routine.self;
      }

      // Look for the root object in the parent and return it
      static if(!__traits(compiles, _esdl__root)) {
	public final RootEntityIntf getRoot() {
	  auto parent = this.getParent();
	  if(parent !is null) {
	    return parent.getRoot;
	  }
	  // This is retuired for the cases where the object does not
	  // have a parent. But since we decided that the parent must
	  // be set either during the elaboration or during inside the
	  // sontructor, this part is not retuired.
	  // else {
	  //   auto rootProc = RootProcess.self;
	  //   if(rootProc !is null)
	  //     return rootProc.getRoot;
	  // }
	  assert(false,
		 "Can not determine root of a NamedObj with null Parent");
	}
      }

      // Look for the simulator object in the root, and return it
      public final override EsdlSimulator getSimulator() {
	return getRoot().simulator();
      }

      // This function is called only during the elaboration phase
      public final override void _esdl__setParent(NamedObj parent) {
	synchronized(this) {
	  if(this._esdl__parent !is null && this._esdl__parent !is parent) {
	    assert(false, "Attempt to modify parent object for object: " ~ _esdl__name);
	  }
	  else {
	    this._esdl__parent = parent;
	  }
	}
      }

      // This function is required only during the elaboration phase
      public final override void _esdl__setName(string name) {
	synchronized(this) {
	  if((_esdl__name != "") &&(_esdl__name != name)) {
	    assert(false, "Attempt to modify name of the object!");
	  }
	  this._esdl__name = name;
	}
      }

      public final string getName() {
	// name is always set in elaboration phase and therefor is
	// effectively immutable during simulation run
	return this._esdl__name;
      }

      // Return the parent object and if it not set, signal an error
      public final override NamedObj getParent() {
	if(_esdl__parent !is null) return _esdl__parent;
	assert(false, "Tried to seek parent for a NamedObj "
	       "which does not have it set");
	// synchronized(this) {
	//   _esdl__parent = _esdl__getParentProc();
	//   return _esdl__parent;
	// }
      }

      // Called during the elaboration phase to help determine the
      // name of an (array) object
      public final void _esdl__nomenclate(size_t I, T)(T t, uint[] indices) {
	synchronized(this) {
	  import std.string: chompPrefix;
	  import std.conv: to;
	  string name = chompPrefix(t.tupleof[I].stringof, "t.");
	  foreach(i; indices) {
	    name ~= "[" ~ to!string(i) ~ "]";
	  }
	  _esdl__nomenclate(name);
	}
      }

      // Sets the name of an object during elaboration. Name is set
      // only if it is not already set
      public final void _esdl__nomenclate()(string name) {
	synchronized(this) {
	  if(this._esdl__name == "") this._esdl__name = name;
	}
      }

      /////////////////////////////////////////////////////////////
      // TimeContext
      /////////////////////////////////////////////////////////////
      // Parent is fixed either during the elaboration or inside the
      // (guarded) constructor of the object, hence the parent is
      // immutable


      // a plain NamedObj does not have timeUnit, and so
      // seek the timeUnit from the parent and return it.
      static if(!__traits(compiles, _timeScale)) {
	public final override ulong timeScale() {
	  return this.getParent.timeScale();
	}
	// And timeUnit
	public final override Time timeUnit() {
	  return this.getParent.timeUnit();
	}
      }
    };
  }
}


public interface HierComp: NamedObj
{
  import core.sync.semaphore: CoreSemaphore = Semaphore;

  // Elaboration and Hierarchy related interface functions

  // UI methods for traversing through the hierarchy -- All these
  // methods return object hierarchy that is set in stone during the
  // elaboration -- as a result the returned array does not include
  // any dynamic process or routine
  public NamedObj[] getChildObjs();
  public NamedObj[] getChildObjsHier();
  public Process[]  getChildTasks();
  public Process[]  getChildTasksHier();
  public HierComp[] getChildComps();
  public HierComp[] getChildCompsHier();

  // If the given object is an element of a (possibly
  // multidimensional) array of objects, this function can be used to
  // determine the position of the given object inside the array
  public uint[] getIndices();

  // The next couple of functions are useful to determine the list of
  // dynamic processes launched by another process in addition to all
  // the static processes (tasks). These functions are not part of the
  // user interface, since the implementation is not guarded. These
  // functions are called internally only during the scheduling
  // phase. The first function is sometimes called (see abortForks and
  // waitForks) even during the run-phase, but only on the current
  // thread -- and hence does not require any guards.
  protected Process[] _esdl__getChildProcs();
  protected Process[] _esdl__getChildProcsHier();

  // The next set of functions are used only during the elaboration
  // phase
  public void _esdl__addChildObj(NamedObj child);
  public void _esdl__addChildTask(Process child);
  public void _esdl__addChildComp(HierComp child);
  public void _esdl__setIndices(uint[] indices);;

  // Process interface, While Process class has its own implementation
  // of these functions, HierComp interface implementation applies
  // these operations to immediate child processes
  public void suspend();
  public void suspendRec();
  public void disable();
  public void disableRec();
  public void resume();
  public void enable();
  public void abort();
  public void abortRec();

  // Interface functions for enabling parallelize UDP
  public CoreSemaphore _esdl__getLockMutex();
  public void _esdl__setEntityMutex(CoreSemaphore parentLock,
					     byte linfo, byte plinfo);
  static byte _esdl__get_parallelism(size_t I, T, L)(T t, ref L l) {
    enum int P = _esdl__attr!(parallelize, t, I);
    enum int Q = _esdl__attr!(parallelize, L);
    static if(P is -1) {
      // check if the entity definition has a parallelize tag
      static if(Q is -1) {
	enum byte par = -1; // inherit from parent
      }
      else {
	static if(__traits(isSame, __traits(getAttributes, L)[Q],
			   parallelize)) {
	  enum byte par = byte.min;
	}
	else {
	  enum attr = __traits(getAttributes, L)[Q];
	  enum byte par = attr._parallel;
	}
      }
    }
    else {
      // first check if the attribute is just @parallelize
      static if(__traits(isSame, __traits(getAttributes, t.tupleof[I])[P],
			 parallelize)) {
	enum byte par = byte.min;
      }
      else {
	enum attr = __traits(getAttributes, t.tupleof[I])[P];
	enum byte par = attr._parallel;
      }
    }
    return par;
  }

  static string hierMixin() {
    return q{
      //////////////////////////////////////////////////////
      // Implementation of the parallelize UDP functionality
      //////////////////////////////////////////////////////

      // Lock to inhibit parallel threads in the same Entity
      // This valriable is set in elaboration phase and in the later
      // phases it is only accessed. Therefor this variable can be
      // treated as effectively immutable.

      // In practice, the end-user does not need to bother about
      // this Lock. It is automatically picked up by the simulator
      // when a process wakes up and subsequently, when the process
      // deactivates (starts waiting for an event), the simulator
      // gives away the Lock
      static if(!__traits(compiles, _esdl__parLock)) {
	import core.sync.semaphore: CoreSemaphore = Semaphore;
	CoreSemaphore _esdl__parLock;
      }

      // This variable keeps the information provided by the user as
      // an argument to the parallelize UDP -- For the hierarchy
      // levels where the parallelize UDP is not specified, the value
      // for this variable is copied from the uppper hierarchy
      // This variable is set and used only during the elaboration
      // phase
      static if(!__traits(compiles, _esdl__lockInfo)) {
	byte _esdl__lockInfo = 1; // default value
      }

      // Effectively immutable in the run phase since the variable is
      // set durin gthe elaboration
      public final override CoreSemaphore _esdl__getLockMutex() {
	return _esdl__parLock;
      }

      // Create a new Semaphore for a given HierComp when required,
      // based on the UDP option @parallelize. This function is called
      // during the elaboration phase.
      public final override void _esdl__setEntityMutex(CoreSemaphore parentLock,
						       byte linfo, byte plinfo) {
	if(linfo == -1) {	// take hier information
	  _esdl__lockInfo = plinfo;
	  if(plinfo == byte.min) { // UDP @parallelize without argument
	    _esdl__parLock = null;
	  }
	  else if(plinfo == 0) {
	    _esdl__parLock = parentLock;
	  }
	  else {
	    _esdl__parLock = new CoreSemaphore(plinfo);
	  }
	}
	else {
	  if(plinfo == 0) {
	    // FIXME -- give out warning
	  }
	  _esdl__lockInfo = linfo;
	  if(linfo == byte.min) {	// parallelize
	    _esdl__parLock = null;
	  }
	  else if(linfo == 0) {
	    _esdl__parLock = new CoreSemaphore(1);
	  }
	  else {
	    _esdl__parLock = new CoreSemaphore(linfo);
	  }
	}
      }

      //////////////////////////////////////////////////////
      // Implementation of the hierarchy/elaboration methods
      //////////////////////////////////////////////////////

      // _esdl__root is set in the elaboration phase and are used (but
      // not modified) in the simulation phase, as a result these
      // varables can be treated as effectively immutable
      static if(!__traits(compiles, _esdl__root)) {
	@_esdl__ignore protected RootEntityIntf _esdl__root;
      }

      public final void _esdl__setRoot(RootEntityIntf root) {
	synchronized(this) {
	  if(this._esdl__root !is null &&
	     this._esdl__root !is root) {
	    assert(false, "Attempt to modify root object!");
	  }
	  else {
	    this._esdl__root = root;
	  }
	}
      }


      public final override RootEntityIntf getRoot() {
	if(_esdl__root) return _esdl__root;
	assert(false, "Root not set for HierComp " ~ getName());
	// synchronized(this) {
	//   if(_esdl__root) {
	//     return _esdl__root;
	//   }
	//   else if(_esdl__parent) {
	//     _esdl__root = _esdl__parent.getRoot();
	//     return _esdl__root;
	//   }
	//   else {
	//     RootEntityIntf root =(cast(RootEntityIntf) this);
	//     if(root !is null) return root;
	//     else {
	//       assert(false, "getRoot: ElabContext Inst " ~ getName() ~ ":" ~
	//	     typeof(this).stringof ~
	//	     " does not have a parent!");
	//     }
	//   }
	// }
      }

      // The Process and Routine classes would have only child
      // processes and not other hierarchy to track. There may be
      // Events and Channels etc declared inside a process' body but
      // a process will not keep track of these objects as it child
      // objects -- no useful purpose is served .
      static if(__traits(compiles, _esdl__childProcs)) {}
      else {
	static if(!__traits(compiles, _esdl__childObjs)) {
	  @_esdl__ignore protected NamedObj[] _esdl__childObjs;
	}

	static if(!__traits(compiles, _esdl__childTasks)) {
	  @_esdl__ignore protected Process[] _esdl__childTasks;
	}

	static if(!__traits(compiles, _esdl__childComps)) {
	  @_esdl__ignore protected HierComp[] _esdl__childComps;
	}

	static if(__traits(isAbstractFunction, getChildObjs)) {
	  public final override NamedObj[] getChildObjs() {
	    // _esdl__childObjs is effectively immutable
	    return this._esdl__childObjs;
	  }
	}
      }

      // Returns only the static(frozen during elaboration)
      // hierarchical objects
      static if(__traits(isAbstractFunction, getChildObjsHier)) {
	public final override NamedObj[] getChildObjsHier() {
	  NamedObj[] children = getChildObjs();
	  foreach(child; getChildComps()) {
	    children ~= child.getChildObjsHier();
	  }
	  return children;
	}
      }

      static if(__traits(isAbstractFunction, getChildTasks)) {
	public final override Process[] getChildTasks() {
	  // Though the scheduler does modify _esdl__childTasks as and
	  // when processes get spawned or die out, the variable can
	  // be treated as effectively immutable since the scheduler
	  // works with single thread
	  return this._esdl__childTasks;
	}
      }

      // Returns only the static tasks. Only Entities are traversed as
      // hierarchy
      static if(__traits(isAbstractFunction, getChildTasksHier)) {
	public final override Process[] getChildTasksHier() {
	  Process[] children = getChildTasks();
	  foreach(child; getChildComps()) {
	    children ~= child.getChildTasksHier();
	  }
	  return children;
	}
      }

      static if(__traits(isAbstractFunction, _esdl__getChildProcs)) {
	// When called on an entity, returns the  static tasks. Same
	// behaviour as getChildTasks
	protected final override Process[] _esdl__getChildProcs() {
	  // Though the scheduler does modify _esdl__childProcs as and
	  // when processes get spawned or die out, the variable can
	  // be treated as effectively immutable since the scheduler
	  // works with single thread
	  return _esdl__childTasks; // this._esdl__childProcs;
	}
      }

      static if(__traits(isAbstractFunction, _esdl__getChildProcsHier)) {
	// When called for an entity, returns the static tasks and the
	// dynamic child hierarchy thereof. This function may be
	// called only in the scheduling phase
	protected final override Process[] _esdl__getChildProcsHier() {
	  Process[] children = _esdl__getChildProcs();
	  foreach(child; getChildComps()) {
	    children ~= child._esdl__getChildProcsHier();
	  }
	  return children;
	}
      }

      static if(__traits(isAbstractFunction, getChildComps)) {
	// Return the child components.
	public final override HierComp[] getChildComps() {
	  // _esdl__childComps is effectively immutable
	  return this._esdl__childComps;
	}
      }

      static if(__traits(isAbstractFunction, getChildCompsHier)) {
	// Return all the components in the static hierarchy.
	public final override HierComp[] getChildCompsHier() {
	  HierComp[] children = getChildComps();
	  foreach(child; getChildComps()) {
	    children ~= child.getChildCompsHier();
	  }
	  return children;
	}
      }

      static if(__traits(isAbstractFunction, _esdl__addChildObj)) {
	// Add object as child of this parent. This function is called
	// only during the elaboration phase for the purpose of
	// creation of object hierarchy.
	public final override void _esdl__addChildObj(NamedObj child) {
	  synchronized(this) {
	    bool add = true;
	    debug(DUPLICATE_CHILD) {
	      foreach(ref _child; this._esdl__childObjs) {
		if(child is _child) {
		  add = false;
		  break;
		}
	      }
	    }
	    if(add) this._esdl__childObjs ~= child;
	  }
	}
      }

      static if(__traits(isAbstractFunction, _esdl__addChildTask)) {
	// We maintain a list of static tasks in the entities in order
	// to avoid incurring efficiency loss because of any dynamic
	// casting.
	public final override void _esdl__addChildTask(Process child) {
	  synchronized(this) {
	    bool add = true;
	    debug(DUPLICATE_CHILD) {
	      foreach(ref _child; this._esdl__childTasks) {
		if(child is _child) {
		  add = false;
		  break;
		}
	      }
	    }
	    if(add) this._esdl__childTasks ~= child;
	  }
	}
      }

      static if(__traits(isAbstractFunction, _esdl__addChildComp)) {
	// To help build a list of hierarchical components.
	public final override void _esdl__addChildComp(HierComp child) {
	  synchronized(this) {
	    bool add = true;
	    debug(DUPLICATE_CHILD) {
	      foreach(ref _child; this._esdl__childComps) {
		if(child is _child) {
		  add = false;
		  break;
		}
	      }
	    }
	    if(add) this._esdl__childComps ~= child;
	  }
	}
      }

      // The indeces is useful to find out the position of a given
      // component inside an array of components. For example, the
      // end-user might be interested in tweeking the functionality of
      // a task based on this parameter.
      protected uint[] _esdl__indices;
      public final override void _esdl__setIndices(uint[] indices) {
	_esdl__indices = indices;
      }

      public final override uint[] getIndices() {
	return _esdl__indices;
      }

      // Used by the Task and Method templates
      public final void _esdl__thunk(string THUNK)() {
	mixin(THUNK);
      }


      static if(__traits(isAbstractFunction, suspend)) {
	// suspend all the tasks of an entity
	public final void suspend() {
	  foreach(task; getChildTasks()) {
	    task.suspend();
	  }
	}
      }

      static if(__traits(isAbstractFunction, suspendRec)) {
	// suspend all the tasks and processes hierarchically for an
	// entity.
	public final void suspendRec() {
	  foreach(task; getChildTasks()) {
	    task.suspendRec();
	  }
	  foreach(comp; getChildComps()) {
	    comp.suspendRec();
	  }
	}
      }

      static if(__traits(isAbstractFunction, disable)) {
	// disable all the tasks of an entity.
	public final void disable() {
	  foreach(task; getChildTasks()) {
	    task.disable();
	  }
	}
      }

      static if(__traits(isAbstractFunction, disableRec)) {
	// disable all tasks and processes hierarchically for an
	// entity.
	public final void disableRec() {
	  foreach(task; getChildTasks()) {
	    task.disableRec();
	  }
	  foreach(comp; getChildComps()) {
	    comp.disableRec();
	  }
	}
      }

      static if(__traits(isAbstractFunction, abort)) {
	// abort all tasks
	public final void abort() {
	  foreach(task; getChildTasks()) {
	    task.abort();
	  }
	}
      }

      static if(__traits(isAbstractFunction, abortRec)) {
	// abort all tasks and ptocesses thereof hierarchically
	public final void abortRec() {
	  foreach(task; getChildTasks()) {
	    task.abortRec();
	  }
	  foreach(comp; getChildComps()) {
	    comp.abortRec();
	  }
	}
      }

      static if(__traits(isAbstractFunction, resume)) {
	// resume a suspended entity
	public final void resume() {
	  foreach(task; getChildTasks()) {
	    task.resume();
	  }
	  foreach(comp; getChildComps()) {
	    comp.resume();
	  }
	}
      }

      static if(__traits(isAbstractFunction, enable)) {
	// enable a disabled an entity
	public final void enable() {
	  foreach(task; getChildTasks()) {
	    task.enable();
	  }
	  foreach(comp; getChildComps()) {
	    comp.enable();
	  }
	}
      }

      mixin(namedMixin());

    };

  }
}

// template BaseCompType(T)
// {
//   static if(isArray!(T)) {
//     alias BaseCompType!(CompType!(T)) BaseCompType;
//   }
//   else {
//     alias T BaseCompType;
//   }
// }

// template Search(T, TL...)
// {
//   static if(TL.length == 0)
//     enum bool Search = false;
//   else static if(is(T == TL[0]))
//	 enum bool Search = true;
//     else
//       enum bool Search = Search!(T, TL[1 .. $]);
// }

// Defined in std.typecons
// template Tuple(E...)
// {
//   alias E Tuple;
// }

// template UniqueModules(size_t n, M, SL...)
// {
//   static if(n == FieldTypeTuple!(M).length)
//     alias SL UniqueModules;
//   else static if(is(BaseCompType!(FieldTypeTuple!(M)[n]) : Module))
//	 static if(Search!(BaseCompType!(FieldTypeTuple!(M)[n]), SL))
//	   alias Tuple!(UniqueModules!(n+1, M, SL)) UniqueModules;
//	 else
//	   alias Tuple!(UniqueModules!(n+1, M,
//				       Tuple!(BaseCompType!(FieldTypeTuple!(M)[n]), SL)))
//	     UniqueModules;
//     else Tuple!(UniqueModules!(n+1, M, SL)) UniqueModules;
// }

// template CheckEsdlTemplateParams (alias F, size_t I, T, L)
// {
//   alias F!(I, T, L) _unused;
//   enum bool CheckEsdlTemplateParams = true;
// }

// Template to check during elaboration whether an instance found
// during the hierarchy navigation is itself an ESDL Object that needs
// to be elaborated too.
// An ESDL object could be ..
// 1. any class object of type NamedObj
// 2. a struct instnace that is tacgged with _esdl__component UDP
// 3. an array of objects with elements of either of the two types as
//    listed above.
template CheckEsdlObj(L)
{
  static if(is(L == class) && is(L unused: NamedObj)) {
    enum bool CheckEsdlObj = true;
  }
  else static if(is(L == struct) &&
		 _esdl__attr!(_esdl__component, L) != -1) {
      enum bool CheckEsdlObj = true;
    }
  else static if(isArray!L) {
      import std.range: ElementType;
      enum bool CheckEsdlObj = CheckEsdlObj!(ElementType!L);
    }
    else {
      enum bool CheckEsdlObj = false;
    }
}

void _esdl__elabMems(size_t I=0, size_t CI=0, T)(T t)
  if(is(T : NamedObj) && is(T == class)) {
    static if(I < t.tupleof.length) {
      import std.string;
      // ignore the ESDL that have been tagged as _esdl__ignore
      static if(_esdl__attr!(_esdl__ignore, t, I) == -1) {
	static if(CheckEsdlObj!(typeof(t.tupleof[I])))
	  {
	    // multidimensional arrays would need special (recursive)
	    // handling, therefor arrays are handled by a separate
	    // function
	    static if(isArray!(typeof(t.tupleof[I]))) {
	      _esdl__elabArray!I(t, t.tupleof[I]);
	    }
	    else {
	      // neither ignored, nor an array, so elaborate the object
	      t.tupleof[I]._esdl__elab!I(t, t.tupleof[I]);
	      debug(ELABORATE) {
		// debug message for listing the elaborated objects
		import std.stdio;
		writeln("Elaborated " ~ typeof(t.tupleof[I]).stringof ~ " : ",
			I, "/", t.tupleof.length);
	      }
	    }
	  }
	else {
	  debug(ELABORATE) {
	    // debug message for listing the objects, which do not
	    // seem to be ESDL objects
	    import std.stdio;
	    writeln("Ignoring " ~ typeof(t.tupleof[I]).stringof ~ " : ",
		    I, "/", t.tupleof.length);
	  }
	}
      }
      else {
	debug(ELABORATE) {
	  // debug message for listing the objects that have been
	  // explicitly tagged to be ignored
	  import std.stdio;
	  writeln("Ignoring(@_esdl__ignore) ", t.tupleof[I].stringof ~ " : ",
		  I, "/", t.tupleof.length);
	}
      }
      // iterate through the next element in the object tuple
      _esdl__elabMems!(I+1, CI+1)(t);
    }
    // recurse through the objects found in the base classes
    else static if(is(T B == super)
		   && is(B[0] : ElabContext)
		   && is(B[0] == class)) {
	B[0] b = t;
	debug(ELABORATE) {
	  // debug message for listing the base classes being
	  // recursed
	  import std.stdio;
	  writeln("*** Elaborating " ~ typeof(b).stringof);
	}
	_esdl__elabMems!(0, CI)(b);
      }
  }

void _esdl__elabArray(size_t I, T, L)(T t, ref L l, uint[] indices=null)
{
  for(size_t j = 0; j < l.length; ++j) {
    // pragma(msg, "Adding: ", typeof(l[j]));
    static if(isArray!(typeof(l[j]))) {
      _esdl__elabArray!I(t, l[j],
			 indices ~ cast(uint) j);
    }
    else {
      l[j]._esdl__elab!I(t, l[j],
			 indices ~ cast(uint) j);
    }
  }
}

// For timing and other configuration
void _esdl__config(T)(T t)
  if(is(T : ElabContext)) {
    synchronized(t) {
      t.doConfig();
      foreach(child; t.getChildComps()) {
	ElabContext hChild = cast(ElabContext) child;
	if(hChild !is null) {
	  _esdl__config(hChild);
	}
      }
    }
  }

void _esdl__postCfg(T)(T t)
  if(is(T : ElabContext)) {
    synchronized(t) {
      t._esdl__postConfig();
      foreach(child; t.getChildComps()) {
	ElabContext hChild = cast(ElabContext) child;
	if(hChild !is null) {
	  _esdl__postCfg(hChild);
	}
      }
    }
  }

// Hook provided for the user to call any code he wishes to just
// before starting the simulation
void _esdl__endElab(T)(T t)
  if(is(T : ElabContext)) {
    synchronized(t) {
      t.endElab();
      foreach(child; t.getChildComps()) {
	ElabContext hChild = cast(ElabContext) child;
	if(hChild !is null) {
	  _esdl__endElab(hChild);
	}
      }
    }
  }

// To facillitate connections between ports etc. Also check if all the
// ports are bound sanely by the user.
void _esdl__connect(T)(T t) {
  synchronized(t) {
    // t._phase = SimPhase.CONFIGURE;
    // static if(__traits(compiles, t.doConnect()))
    // {
    t.doConnect();
    // }
    foreach(ref port; t.getPorts) {
      // import std.stdio: writeln;
      // writeln("Checking connectivity for port", port);
      if(t.simPhase == SimPhase.BINDPORTS) {
	port._esdl__portIsBound();
      }
    }
    foreach(ref exeport; t.getExePorts) {
      // import std.stdio: writeln;
      // writeln("Checking connectivity for exeport", exeport);
      if(t.simPhase == SimPhase.BINDEXEPORTS) {
	exeport._esdl__exeportIsBound();
      }
    }
    // _esdl__connectIterSuper(t);
    foreach(child; t.getChildObjs()) {
      ElabContext hChild = cast(ElabContext) child;
      if(hChild !is null) {
	_esdl__connect(hChild);
      }
    }
  }
}


// Register all the routines are tasks with the simulator during the
// elaboration phase. The Dynamic processes and routines are handled
// separately(in the process/routine constructor)
void _esdl__register(T, L)(T t, ref L l)
  if(is(T : NamedObj) && is(T == class)) {
    static if((is(L : Process)) &&(is(L == class))) {
      synchronized(l) {
	// Dynamic tasks get registered by the constructor --
	// static tasks get registered during Elaboration.
	t.getSimulator.reqRegisterProcess(l, l.stage);
      }
    }

    static if(is(L : Routine)) {
      synchronized(l) {
	// Dynamic tasks get registered by the constructor --
	// static tasks get registered during Elaboration.
	t.getSimulator.reqRegisterRoutine(l);
      }
    }

  }


// void _esdl__inst(size_t I=0, T, L)(T t, ref L l)
//   if(is(L f : Task!(F, S), alias F, size_t S)) {
//     synchronized(t) {
//       // import std.functional; // used for toDelegate
//       // import std.traits;

//       alias L._FUNCTION F;
//       alias L._STACKSIZE S;

//       l = t.new t.TaskT!(T, F, S)(t);
//       // string getFuncName()
//       // {
//       //   return "t." ~ __traits(identifier, F);
//       // }

//       // import std.stdio;
//       // writeln("New thread for: ", t.getFullName);
//       // // does not work for functions with default arguments
//       // // typeof(toDelegate(&F)) dg;
//       // // pragma(msg, getFuncName());
//       // typeof(& mixin(getFuncName())) dg;

//       // dg.funcptr = &F;
//       // dg.ptr = cast(void *)t;

//       // static if((ParameterTypeTuple!dg).length > 0)
//       //   {
//       //     auto fun = delegate()
//       //	 {
//       //	   dg(ARGS);
//       //	 };
//       //     l = new Task!(F, A)(fun, 0);
//       //     // l = new Process(fun, 0);
//       //   }
//       // else
//       //   {
//       //     l = new Task!(F, A)(dg, 0);
//       //     // l = new Process(dg, 0);
//       //   }
//     }
//   }

// {

//   // Tasks
//   // else static if(is(L f == Task!(T, N), immutable(char)[] T, ulong N))
//   //	     {
//   //	       synchronized(t)
//   //		 {
//   //		   l = new Process(&t._esdl__thunk!(L._THUNK), L._STACKSIZE);
//   //		 }
//   //	     }


//   // handle ports events mutexes semaphores channels etc.
//   static
//	{
//	  pragma(msg, "MeInstantiating: " ~ L.stringof);
//	  if(l is null) l = new L();
//	}

//   // else static if(is(L == class) &&
//   //		     is(L c: NamedObj) &&
//   //		     ! is(L: RootProcess))
//   //	     {
//   //	       // static if(is(L f == Task!(S, T), alias S, T...) ||
//   //	       //	    is(L f == Task!(T, N), immutable(char)[] T, ulong N) ||
//   //	       //	    is(L g == Inst!E, E : ElabContext))
//   //	       pragma(msg, "Instantiating: " ~ L.stringof);
//   //	       if(l is null) l = new L();
//   //	     }

// }


public interface ElabContext: HierComp
{
  // hooks for end user.
  // FIXME -- Right now these do not seemed to be called anywhere.
  protected void preSimulation();
  protected void postSimulation();

  // elaboration methods for end-user. Typically doBuild is called
  // before the elaboration tries to build the various objects
  // automatically. And in case a component is instantiated by the
  // user himself as part of the doBuild method, the automatic
  // elaboration process will not try to instantiate that object.
  public void doBuild();
  public void doConfig();
  public void doConnect();
  public void endElab();

  // these methods are hooks that are not exposed to the end user
  protected void _esdl__postBuild();
  protected void _esdl__postElab();
  protected void _esdl__postConfig();


  public void _esdl__addPort(BasePort port);
  public void _esdl__addExePort(BaseExePort port);

  public BasePort[] getPorts();
  public BaseExePort[] getExePorts();

  static void _esdl__inst(size_t I=0, T, L)(T t, ref L l)
  {
    synchronized(t) {
      static if(is(L unused: EsdlSimulator)) {
	// Simulator is handled separately as part of the RootEntity
      }
      else {
	if(l is null) {
	  static if(__traits(compiles, new L())) {
	    l = new L();
	  }
	  else
	    // handle nested entities
	    static if(__traits(compiles, t.new L())) {
	      l = t.new L();
	    }
	    else {
	      static assert(false,
			    "Unable to instantiate " ~
			    t.tupleof[I].stringof);
	    }
	}
      }
    }
  }

  static void _esdl__elab(size_t I, T, L)
    (T t, ref L l, uint[] indices=null) {
    debug(ELABORATE) {
      import std.stdio;
      writeln("** ElabContext: Elaborating " ~ t.tupleof[I].stringof ~ ":" ~
	      typeof(l).stringof);
    }
    l._esdl__inst!I(t, l);
    synchronized(l) {
      import core.sync.semaphore: Semaphore;
      static assert(is(T unused: ElabContext),
		    "Only ElabContext components are allowed to instantiate "
		    "other ElabContext components");
      static if(is(T unused: ElabContext)) {
	t._esdl__addChildObj(l);
	t._esdl__addChildComp(l);
      }
      l._esdl__nomenclate!I(t, indices);
      l._esdl__setParent(t);
      l._esdl__setRoot(t.getRoot);
      l._esdl__setIndices(indices);
      auto linfo = _esdl__get_parallelism!I(t, l);
      l._esdl__setEntityMutex(t._esdl__getLockMutex, linfo,
			      t._esdl__lockInfo);
      l.doBuild();
      l._esdl__postBuild();
      _esdl__elabMems(l);
      l._esdl__postElab();
    }
  }

  final static string hierContextMixin() {
    return q{
      // Arrays for ports and exports. These variables are effectively
      // immutable since (ex)ports are added to them only during
      // elaboration.
      static if(!__traits(compiles, _esdl__ports)) {
	@_esdl__ignore public BasePort[] _esdl__ports;
      }

      static if(!__traits(compiles, _esdl__exePorts)) {
	@_esdl__ignore public BaseExePort[] _esdl__exePorts;
      }


      // default function bodies(mostly empty) for the user defined
      // methods.
      protected override void preSimulation() {}
      protected override void postSimulation() {}

      static if(__traits(isAbstractFunction, doConfig)) {
	public override void doConfig() {}
      }
      static if(__traits(isAbstractFunction, doConnect)) {
	public override void doConnect() {}
      }
      static if(__traits(isAbstractFunction, endElab)) {
	public override void endElab() {}
      }
      static if(__traits(isAbstractFunction, doBuild)) {
	public override void doBuild() {}
      }
      static if(__traits(isAbstractFunction, _esdl__postBuild)) {
	public override void _esdl__postBuild() {}
      }
      static if(__traits(isAbstractFunction, _esdl__postElab)) {
	public override void _esdl__postElab() {}
      }
      static if(__traits(isAbstractFunction, _esdl__postConfig)) {
	public override void _esdl__postConfig() {}
      }


      // _esdl__ports and _esdl__exePorts are effectively immutable
      // and therefor no sync guards are required.
      public final BasePort[] getPorts() {
	return this._esdl__ports;
      }
      public final BaseExePort[] getExePorts() {
	return this._esdl__exePorts;
      }

      public final override void _esdl__addPort(BasePort port) {
	synchronized(this) {
	  bool add = true;
	  debug(DUPLICATE_CHILD) {
	    foreach(ref _port; this._esdl__ports) {
	      if(port is _port) {
		add = false;
		break;
	      }
	    }
	  }
	  if(add) this._esdl__ports ~= port;
	}
      }

      public final override void _esdl__addExePort(BaseExePort exeport) {
	synchronized(this) {
	  bool add = true;
	  debug(DUPLICATE_CHILD) {
	    foreach(ref _exeport; this._esdl__exePorts) {
	      if(exeport is _exeport) {
		add = false;
		break;
	      }
	    }
	  }
	  if(add) this._esdl__exePorts ~= exeport;
	}
      }
      // _Random_ Generator
      // public Random _r;
    };
  }
}


// Any object that can poke/trigger an event
interface EventAgent
{
  // trigger will poke a list of events that need a poke
  protected void trigger(EsdlSimulator sim);
  protected void addClientEvent(SimEvent client, size_t index);
  // A Process(Process or Routine) can be client to only one Event Agent
  // protected void addClientProc(EventClient client);
  protected void addClientProcess(Process client);
  protected void addClientRoutine(Routine client);
}

// Any object that is able to get notified by an event
interface EventClient
{
  // notify that an event being waited for has been triggered
  protected void poke(EsdlSimulator sim, EventObj agent, size_t index);
  // The addAgent method returns the index at which this agent gets hooked
  protected void addAgent(EventObj agent);
  // protected string getFullName();
  public bool notify();
  public bool notify(SimTime steps);
}

// nested struct
final class IndexedSimEvent
{
  import core.atomic;
  import core.memory;

  private size_t _client;
  private size_t _ptr;

  // SimEvent client = void;
  size_t index = void;

  this(SimEvent event, size_t i) {
    this.index = i;
    auto ptr = cast(size_t)*cast(void**)&event;

    // We use atomics because not all architectures may guarantee
    // atomic store and load of these values.
    atomicStore(*cast(shared)&_client, ptr);

    // Only assigned once, so no atomics.
    _ptr = ptr;

    rt_attachDisposeEvent(event, &unhook);
    // GC.setAttr(cast(void*)this, GC.BlkAttr.NO_SCAN);
    GC.setAttr(cast(void*)this, GC.BlkAttr.NO_MOVE);
  }

  private final SimEvent client() {
    auto obj = cast(SimEvent)cast(void*)atomicLoad(*cast(shared)&_client);

    // We've moved obj into the GC-scanned stack space, so it's now
    // safe to ask the GC whether the object is still alive. Note
    // that even if the cast and assignment of the obj local
    // doesn't put the object on the stack, this call will. So,
    // either way, this is safe.
    if (GC.addrOf(cast(void*)obj)) return obj;
    return null;
  }

  private final void unhook(Object object) {
    rt_detachDisposeEvent(object, &unhook);

    // This assignment is important. If we don't null _client when
    // it is collected, the check in object could return false
    // positives where the GC has reused the memory for a new
    // object.
    // writeln("Event is nulled");
    atomicStore(*cast(shared)&_client, cast(size_t)0);
  }

}

// FIXME -- create a freelist for notifications
public class NotificationObj(T): EventObj
  // , private EventClient
{
  // _data is set when a data is provided while making a notification
  private T _data;
  // data is copied over to _dataTriggered when an event is triggered
  private T _dataTriggered;

  public this(NamedObj parent=null) {
    super(parent);
  }

  public this(SimEvent simEvent, NamedObj parent=null) {
    super(simEvent, parent);
  }

  // This function is always called in the schedule phase, when only a
  // single thread is active -- do we need synchronization guards
  // here?
  protected override void trigger(EsdlSimulator sim) {
    _dataTriggered = _data;
    super.trigger(sim);
  }

  // public final ProxyEvent proxy() {
  //   ProxyEvent event = new ProxyEvent();
  //   event.addAgent(this);
  //   return event;
  // }

  //////////////////////////////////////////////////////////////
  // TimedNotice methods
  // These methods are applied only for Timed Events, otherwise
  // a runtime assertion error would occur
  // Any object can be passed as a data while notifying. The
  // object becomes available to the waiting threads.
  /////////////////////////////////////////////////////////////

  // Immediate notification
  // The thread is *not* stopped to activate immediate
  // notifications, instead the scheduler has a loop looking for
  // immediate notifications before it looks for any delta
  // notifications. As a result if you raise two immediate
  // notifications one-after-another, the result would be same
  // as having just one notification
  public void post(T data) {
    if (this.getTimed().notify()) {
      synchronized(this)
	_data = data;
    }
  }

  // Timed notification -- takes a ulong as an argument and
  // applies the timescale of this event
  public void post(ulong t, T data) {
    this.post(SimTime(t * getTimeScale()), data);
  }

  // Timed notification with Time as argument
  public void post(Time t, T data) {
    this.post(SimTime(this.getSimulator, t), data);
  }

  // Timed notification with simulation time steps as argument
  public void post(SimTime t, T data) {
    if (this.getTimed().notify(t)) {
      synchronized(this) {
	_data = data;
      }
    }
  }

  // method to set the data on an event independent of notification
  public final void set(T data) {
    synchronized(this) {
      _data = data;
    }
  }

  // getter method for getting the data from an event notification
  // -- this method would be usually called right after wait
  public final T get() {
    synchronized(this) {
      return _dataTriggered;
    }
  }

  public final NotificationQueue!T delayed(D)(D delay)
    if(is(D == SimTime) || is(D == Time) || isIntegral!D) {
      auto delN = new NotificationQueueObj!T();
      cron(this,
	   {
	     delN.post(delay, this.get());
	   });
      // fork({
      // 	  while(true) {
      // 	    delN.post(delay, observe(this));
      // 	  }
      // 	});
      NotificationQueue!T n = delN;
      return n;
    }

  public final Notification!T clocked(ref Event clk) {
    auto clked = new NotificationObj!T();
    cron(clk,
	 {
	   clked.post(this.get());
	 });
    // fork({
    // 	while(true) {
    // 	  clk.wait();
    // 	  clked.post(this.get());
    // 	}
    //   });
    Notification!T n = clked;
    return n;
  }

  // internal function for event instantiation -- called during the
  // elaboration phase
  static void _esdl__inst(size_t I=0, U, L)(U u, ref L l)
  {
    synchronized(u) {
      if(l is null) l = new L();
    }
  }
};

// A struct wrapper for EventObj class -- this is basically to make
// local instantiation more user-friendly. Also since an EventObj
// object is copied by refence, named events become more difficult to
// handle with EventObj.
// Effectively an Event wrapper is exposed for the API.
@_esdl__component struct Notification(T)
{
  // The wrapped object
  package NotificationObj!T _notificationObj = null;

  // seek the object by reference. This function is used during the
  // elaboration phase. Do not use it otherwise.
  package ref NotificationObj!T _esdl__objRef() {
    return _notificationObj;
  }

  NotificationObj!T _esdl__obj() {
    // Use double-locked checking -- assumption Intel Processor Architecture
    // All the pointer read writes are atomic
    if(this._notificationObj is null) {
      synchronized(typeid(Notification!T)) {
	if(this._notificationObj is null) {
	  NotificationObj!T be = new NotificationObj!T();
	  this._notificationObj = be;
	}
      }
    }
    return this._notificationObj;
  }


  alias _esdl__obj this;
  alias _esdl__obj getNotification;

  // postblit
  this(this) {
    import std.exception: enforce;	// enforce
    enforce((this._notificationObj !is null),
	    "Attempt to copy un-initialized notification");
  }

  this(SimEvent e) {
    this.init;
    synchronized(getNotification, e) {
      import std.exception: enforce;
      enforce((e._eventObj is null));
      e._eventObj = this.getNotification;
      this.getNotification._simEvent = e;
    }
  }


  // Disallow Event assignment
  @disable private void opAssign(Notification!T e);

  // User API

  public final void opAssign(SimEvent e) {
    this.init();
    synchronized(getNotification, e) {
      import std.exception: enforce;	// enforce
      enforce(e._eventObj is null);
      enforce(this.getNotification._simEvent is null);
      e._eventObj = this.getNotification;
      this.getNotification._simEvent = e;
    }
  }


  // public static Notification!T opCall() {
  //   Notification!T notification;
  //   notification.init();
  //   return notification;
  // }

  this(string name) {
    this.init(name);
  }

  public static Notification!T[] opIndex(size_t n) {
    Notification!T notifications[] = new Notification!T[n];
    foreach(ref notification;notifications) {
      synchronized {
	notification.init();
      }
    }
    return notifications;
  }

  public final void init(NamedObj parent=null) {
    init(null, parent);
  }

  public final void init(string name, NamedObj parent=null) {
    synchronized {
      if(RootProcess.self !is null && parent is null) {
	assert(false, "Must provide parent for NotificationObj being "
	       "\"init\" during elaboration");
      }
      if(_notificationObj is null) {
	_notificationObj = new NotificationObj!T(parent);
      }
      if(name !is null) {
	_notificationObj._esdl__nomenclate(name);
      }
    }
  }
  // alias init init;

  public final void opAssign()(NotificationObj!T e) {
    import std.exception: enforce;	// enforce
    enforce(this._notificationObj is null);
    this._eventObj = e;
  }

  public this(NotificationObj!T e) {
    this._notificationObj = e;
  }

  static void _esdl__inst(size_t I=0, T, L)(T t, ref L l) {
    l._esdl__objRef._esdl__inst!I(t, l._esdl__objRef);
  }

  static void _esdl__elab(size_t I, T, L)(T t, ref L l, uint[] indices=null)
  {
    debug(ELABORATE) {
      import std.stdio;
      writeln("** Notification: Elaborating " ~ t.tupleof[I].stringof ~ ":" ~
	      typeof(l).stringof);
    }
    l._esdl__inst!I(t, l);
    synchronized(l._esdl__obj) {
      static if(is(T unused: ElabContext)) {
	t._esdl__addChildObj(l._esdl__obj);
      }
      l._esdl__obj._esdl__nomenclate!I(t, indices);
      l._esdl__obj._esdl__setParent(t);
    }
  }
}


// Notification Queue -- just like event queues in SystemC.
// FIXME -- review later since this is not required for UVM --
// actually does not even have an equivalent functionality in
// SystemVerilog
public class NotificationQueueObj(T): NotificationObj!T
{
  import std.container: BinaryHeap;
  private TimedNotice[] _nQueue;
  private TimedNotice _currentNotice;

  static int less(T)(T a, T b) {
    return a < b;
  }

  static int greater(T)(T a, T b) {
    return a > b;
  }

  alias BinaryHeap!(TimedNotice[], greater) Heap;
  private Heap _nHeap;

  struct TimedNotice
  {
    enum SimTime invalid = SimTime(long.min);
    
    SimTime _time = invalid;
    T _data;

    public final bool isValid() {
      return _time != invalid;
    }

    public final void invalidate() {
      _time = invalid;
    }

    public final int opCmp(TimedNotice rhs) {
      if(this._time == rhs._time) return 0;
      if(this._time < rhs._time) return -1;
      else return 1;
    }
  }


  protected this(SimEvent simEvent=null) {
    super(simEvent);
    synchronized(this) {
      this._nQueue = new TimedNotice[4];
      this._nHeap = Heap(_nQueue, 0);
    }
  }

  protected final override void trigger(EsdlSimulator sim) {
    super.trigger(sim);
    assert((_currentNotice.isValid),
	   "Heap can not be empty when an NotificationQueueObj triggered");
    debug(EVENT_HEAP) {
      import std.stdio;
      writeln(getSimulator.simTime, "::", _currentNotice._time);
      assert(getSimulator.simTime == _currentNotice._time ||
	     // cover the immediate notifications
	     getSimulator.simTime == (_currentNotice._time + SimTime(1)));
    }

    if(_nHeap.empty()) {
      _currentNotice.invalidate();
    }
    else {
      _currentNotice = _nHeap.front();
      _nHeap.removeFront();
      auto t = _currentNotice._time - getSimulator.simTime;
      if(t == SimTime(-1)) {
	debug(EVENT_HEAP) {
	  import std.stdio;
	  writeln("Immediate Notification: ", _nHeap.length);
	}
	super.post(_currentNotice._data);
      }
      else {
	super.post(t, _currentNotice._data);
      }
      debug(EVENT_HEAP) {
	import std.stdio;
	writeln("Next TimedNotice at time: ",
		_currentNotice._time - getSimulator.simTime);
      }
    }
  }

  public final override void cancel() {
    synchronized(this) {
      _currentNotice.invalidate();
      _nHeap.release();
      super.cancel();
    }
  }

  public final override void post(T data) {
    // since we want to store the immediate notifications too in the
    // same event heap, we shall represent immediate events as
    // occuring at SimTime -1
    this._post(SimTime(-1), data);
  }

  public final override void post(ulong t, T data) {
    this.post(SimTime(t * getTimeScale()), data);
  }

  public final override void post(SimTime t, T data) {
    if(t == SimTime(-1)) {
      assert(false, "Negative SimTime provided with notify");
    }
    this._post(t, data);
  }

  private final void _post(SimTime t, T data) {
    auto nTime = t + getSimulator.simTime;
    synchronized(this) {
      if(this._nQueue.length == this._nHeap.length) {
	// double the size of underlying array
	debug(EVENT_HEAP) {
	  import std.stdio: writeln;
	  writeln("Increasing the heap store to: ",
		  this._nQueue.length * 2);
	}
	this._nQueue.length *= 2;
	this._nHeap.assume(this._nQueue,
			   this._nHeap.length);
      }
      if(_currentNotice.isValid) {
	if(_currentNotice._time <= nTime) {
	  this._nHeap.insert(TimedNotice(nTime, data));
	  return;
	}
	else {
	  super.cancel();
	  this._nHeap.insert(_currentNotice);
	  _currentNotice = TimedNotice(nTime, data);
	  if(t == SimTime(-1)) super.post(_currentNotice._data);
	  else super.post(t, _currentNotice._data);
	}
      }
      else {
	_currentNotice = TimedNotice(nTime, data);
	if(t == SimTime(-1)) super.post(_currentNotice._data);
	else super.post(t, _currentNotice._data);
      }
    }
  }

  static void _esdl__inst(size_t I=0, U, L)(U u, ref L l)
  {
    synchronized(u) {
      if(l is null) l = new L();
    }
  }
}


// Wrapper struct for EventQueueObj
// Fixme -- review later -- but should be very similar to the Event
// struct
@_esdl__component struct NotificationQueue(T)
{
  package NotificationQueueObj!T _notificationQueueObj = void;

  package final ref NotificationQueueObj!T _esdl__objRef() {
    return _notificationQueueObj;
  }

  final NotificationObj!T _esdl__obj() {
    // Use double-locked checking -- assumption Intel Processor Architecture
    // All the pointer read writes are atomic
    if(this._notificationQueueObj is null) {
      synchronized(typeid(NotificationQueue!T)) {
	if(this._notificationQueueObj is null) {
	  NotificationQueueObj!T be = new NotificationQueueObj!T();
	  this._notificationQueueObj = be;
	}
      }
    }
    return this._notificationQueueObj;
  }

  alias _esdl__obj this;
  alias _esdl__obj getNotificationQueue;

  // postblit
  this(this) {
    import std.exception: enforce;	// enforce
    enforce((this._notificationQueueObj !is null),
	    "Attempt to copy un-initialized notificationQueue");
  }

  this(SimEvent e) {
    synchronized(getNotificationQueue, e) {
      import std.exception: enforce;	// enforce
      enforce((e._eventObj is null));
      e._eventObj = this.getNotificationQueue;
      this.getNotificationQueue._simEvent = e;
    }
  }


  // Disallow NotificationQueue assignment
  @disable private void opAssign(NotificationQueue!T e);

  // User API

  public final void opAssign(SimEvent e) {
    synchronized(getNotificationQueue, e) {
      import std.exception: enforce;	// enforce
      enforce(e._eventObj is null);
      enforce(this.getNotificationQueue._simEvent is null);
      e._eventObj = this.getNotificationQueue;
      this.getNotificationQueue._simEvent = e;
    }
  }


  // public static NotificationQueue!T opCall() {
  //   NotificationQueue!T notification;
  //   notificationQueue.init();
  //   return notification;
  // }

  this(string name) {
    this.init(name);
  }

  public final void init() {
    synchronized {
      if(_notificationQueueObj is null) {
	_notificationQueueObj = new NotificationQueueObj!T();
      }
    }
  }

  public final void init(string name) {
    synchronized {
      if(_notificationQueueObj is null) {
	_notificationQueueObj = new NotificationQueueObj!T();
      }
      _notificationQueueObj._esdl__nomenclate(name);
    }
  }
  // alias init init;

  public final void opAssign()(NotificationQueueObj!T e) {
    import std.exception: enforce;	// enforce
    enforce(this._notificationQueueObj is null);
    this._eventObj = e;
  }

  public this(NotificationQueueObj!T e) {
    this._notificationQueueObj = e;
  }

  static void _esdl__inst(size_t I=0, T, L)(T t, ref L l) {
    l._esdl__objRef._esdl__inst!I(t, l._esdl__objRef);
  }

  static void _esdl__elab(size_t I, T, L)(T t, ref L l, uint[] indices=null)
  {
    debug(ELABORATE) {
      import std.stdio;
      writeln("** NotificationQueue: Elaborating " ~ t.tupleof[I].stringof ~ ":" ~
	      typeof(l).stringof);
    }
    l._esdl__inst!I(t, l);
    synchronized(l._esdl__obj) {
      static if(is(T unused: ElabContext)) {
	t._esdl__addChildObj(l._esdl__obj);
      }
      l._esdl__obj._esdl__nomenclate!I(t, indices);
      l._esdl__obj._esdl__setParent(t);
    }
  }
}

// FIXME -- create a freelist for events
public class EventObj: EventAgent, NamedObj
  // , private EventClient
{
  private SimEvent _simEvent;

  private long _triggeredAt = 0;

  // _triggeredAt is only read during the run phase -- therefor no
  // synchronization guards required for reading
  public final bool triggered() {
    return _triggeredAt == getSimulator.updateCount;
  }

  // Clients that need to be notified when this event triggers
  private IndexedSimEvent[] _clientEvents;

  // Processes that are waiting for this event to trigger
  private Process[] _clientProcs;
  // Routines that would be triggered by this event
  private Routine[] _clientRoutines;

  protected this(NamedObj parent=null) {
    this(null, parent);
  }

  protected this(SimEvent simEvent, NamedObj parent=null) {
    synchronized(this) {
      if(parent is null) parent = _esdl__getParentProc();
      if(parent !is null) this._esdl__parent = parent;
      if(simEvent !is null) {
	this._simEvent = simEvent;
      }
    }
    if(simEvent !is null) {
      synchronized(simEvent) {
	simEvent._eventObj = this;
      }
    }
  }

  // An event could be responsible for poking other events -- see
  // EventExpr and derived classes
  protected final void addClientEvent(SimEvent client, size_t index) {
    synchronized(this) {
      this._clientEvents ~= new IndexedSimEvent(client, index);
      debug(CLIENTS) {
	import std.stdio;
	writeln("there are a number of clients: ", _clientEvents.length);
      }
    }
  }

  // called when a process starts waiting for this event
  protected final void addClientProcess(Process client) {
    synchronized(this) {
      this._clientProcs ~= client;
    }

    // just to make sure that the event a thread is waiting for, is
    // not GCed.

    // At this point DMD's GC has a stop-the-world
    // implementation. Synchronization guards are required if the
    // implementation is changed to parallel GC thread type in future.
    // synchronized(client) {
    //   client._waitingFor = this;
    // }
  }

  // FIXME -- save the event with the routine, so that the event is
  // not possibly garbage collected
  protected final void addClientRoutine(Routine client) {
    synchronized(this) {
      this._clientRoutines ~= client;
    }
  }

  public final void wait() {
    if(Process.self is null) {
      assert(false, "Wait can be called only from a Process");
    }
    else {
      Process.self.waitSensitive(this);
    }
  }

  // This function is always called in the schedule phase, when only a
  // single thread is active -- do we need synchronization guards
  // here?
  protected void trigger(EsdlSimulator sim) {
    _triggeredAt = sim.updateCount;
    debug(EVENT) {
      import std.stdio: writeln;
      writeln("Event Triggered: ", this.getFullName);
    }

    foreach(c; _clientProcs) {
      sim._executor.addRunnableProcess(c);
      // c._waitingFor = null;
    }

    _clientProcs.length = 0;

    foreach(ref c; _clientRoutines) {
      sim._executor.addRunnableRoutine(c);
    }

    _clientRoutines.length = 0;

    auto clientEvents = _clientEvents;
    _clientEvents.length = 0;

    foreach(c; clientEvents) {
      if(c.client !is null) {
	_clientEvents ~= c;
	debug(EVENT) {
	  import std.stdio: writeln;
	  writeln("Event Triggered: ", this.getFullName, " client ", index);
	}
	c.client.poke(sim, this, c.index);
	debug(EVENT) {
	  import std.stdio: writeln;
	  writeln("Event Triggered: ", this.getFullName, " poked client ", index);
	}
      }
    }
  }

  public final ProxyEvent proxy() {
    ProxyEvent event = new ProxyEvent();
    event.addAgent(this);
    return event;
  }

  public final OrSimEvent opBinary(string op)(EventObj rhs)
    if(op == "|" || op == "||") {
      OrSimEvent event = new OrSimEvent();
      event.addAgent(this);
      event.addAgent(rhs);
      return event;
    }

  public final OrSimEvent opBinary(string op, T)(T[] rhs)
    if((op == "|" || op == "||") && is(T unused: EventObj)) {
      OrSimEvent event = new OrSimEvent();
      event.addAgent(this);
      foreach(e; rhs) {
	event.addAgent(e);
      }
      return event;
    }

  public final AndSimEvent opBinary(string op)(EventObj rhs)
    if(op == "&" || op == "&&") {
      AndSimEvent event = new AndSimEvent();
      event.addAgent(this);
      event.addAgent(rhs);
      return event;
    }

  public final AndSimEvent opBinary(string op, T)(T[] rhs)
    if((op == "&" || op == "&&") && is(T unused: EventObj)) {
      AndSimEvent event = new AndSimEvent();
      event.addAgent(this);
      foreach(e; rhs) {
	event.addAgent(e);
      }
      return event;
    }

  public final OrSimEvent opBinary(string op)(Time rhs)
    if(op == "|" || op == "||") {
      return this.opBinary!op(SimTime(this.getSimulator, rhs));
    }

  public final OrSimEvent opBinary(string op)(SimTime rhs)
    if(op == "|" || op == "||") {
      Event event = Event();
      event.notify(rhs);
      return this.opBinary!op(event);
    }

  public final AndSimEvent opBinary(string op)(Time rhs)
    if(op == "&" || op == "&&") {
      return this.opBinary!op(SimTime(this.getSimulator, rhs));
    }

  public final AndSimEvent opBinary(string op)(SimTime rhs)
    if(op == "&" || op == "&&") {
      Event event = Event();
      event.notify(rhs);
      return this.opBinary!op(event);
    }

  public final OrSimEvent opBinaryRight(string op)(Time lhs)
    if(op == "|" || op == "||") {
      return this.opBinary!op(SimTime(this.getSimulator, lhs));
    }

  public final OrSimEvent opBinaryRight(string op)(SimTime lhs)
    if(op == "|" || op == "||") {
      Event event = Event();
      event.notify(lhs);
      return this.opBinary!op(event);
    }

  public final AndSimEvent opBinaryRight(string op)(Time lhs)
    if(op == "&" || op == "&&") {
      return this.opBinary!op(SimTime(this.getSimulator, lhs));
    }

  public final AndSimEvent opBinaryRight(string op)(SimTime lhs)
    if(op == "&" || op == "&&") {
      Event event = Event();
      event.notify(lhs);
      return this.opBinary!op(event);
    }

  // Return the timed SimEvent, and if this is not a TimedEvent,
  // assert error
  public final SimEvent getTimed() {
    synchronized(this) {
      if(this._simEvent is null) {
	this._simEvent = new TimedEvent(this);
	return this._simEvent;
      }
      if(this._simEvent.isTimed()) {
	return this._simEvent;
      } else {
	assert(false, "Not a timed event: " ~ this.getFullName());
      }
    }
  }

  // Resetting an event would abandon all the Task and Routine clients
  // that are waiting on the event -- it also cancels all the scheduled
  // triggers -- reset is applicable only to TimedEvent's.
  // If the argument wakeup is true, notify the event immediately, but
  // no routines or processes would be triggered.
  public final void reset(bool wakeup = false) {
    if(wakeup) {
      this.notify();
    }
    synchronized(this) {
      _clientRoutines.length = 0;
      _clientProcs.length = 0;
      this.cancel();
    }
  }

  // Cancel the scheduled notification
  public void cancel() {
    synchronized(this) {
      this.getTimed().cancel();
    }
  }

  //////////////////////////////////////////////////////////////
  // TimedNotice methods
  // These methods are applied only for Timed Events, otherwise
  // a runtime assertion error would occur
  // Any object can be passed as a payload while notifying. The
  // object becomes available to the waiting threads.
  /////////////////////////////////////////////////////////////

  // Immediate notification
  // The thread is *not* stopped to activate immediate
  // notifications, instead the scheduler has a loop looking for
  // immediate notifications before it looks for any delta
  // notifications. As a result if you raise two immediate
  // notifications one-after-another, the result would be same
  // as having just one notification
  public void notify() {
    this.getTimed().notify();
  }

  // Timed notification -- takes a ulong as an argument and
  // applies the timescale of this event
  public void notify(ulong t) {
    this.notify(SimTime(t * getTimeScale()));
  }

  // Timed notification with Time as argument
  public void notify(Time t) {
    this.notify(SimTime(this.getSimulator, t));
  }


  // Timed notification with simulation time steps as argument
  public void notify(SimTime t) {
    this.getTimed().notify(t);
  }

  // Hierarchy
  mixin(namedMixin());

  // internal function for event instantiation -- called during the
  // elaboration phase
  static void _esdl__inst(size_t I=0, U, L)(U u, ref L l)
  {
    synchronized(u) {
      if(l is null) l = new L();
    }
  }
}

// A struct wrapper for EventObj class -- this is basically to make
// local instantiation more user-friendly. Also since an EventObj
// object is copied by refence, named events become more difficult to
// handle with EventObj.
// Effectively an Event wrapper is exposed for the API.
@_esdl__component struct Event
{
  // The wrapped object
  package EventObj _eventObj = null;

  // seek the object by reference. This function is used during the
  // elaboration phase. Do not use it otherwise.
  package final ref EventObj _esdl__objRef() {
    return _eventObj;
  }

  final EventObj _esdl__obj() {
    // Use double-locked checking -- assumption Intel Processor Architecture
    // All the pointer read writes are atomic
    if(this._eventObj is null) {
      synchronized(typeid(Event)) {
	if(this._eventObj is null) {
	  EventObj be = new EventObj();
	  this._eventObj = be;
	}
      }
    }
    return this._eventObj;
  }


  alias _esdl__obj this;
  alias _esdl__obj getEvent;

  // postblit
  this(this) {
    import std.exception: enforce;	// enforce
    enforce((this._eventObj !is null),
	    "Attempt to copy un-initialized event");
  }

  this(SimEvent e) {
    this.init;
    synchronized(getEvent, e) {
      import std.exception: enforce;
      enforce((e._eventObj is null));
      e._eventObj = this.getEvent;
      this.getEvent._simEvent = e;
    }
  }


  // Disallow Event assignment
  @disable private void opAssign(Event e);

  // User API

  public final void opAssign(SimEvent e) {
    this.init();
    synchronized(getEvent, e) {
      import std.exception: enforce;	// enforce
      enforce(e._eventObj is null);
      enforce(this.getEvent._simEvent is null);
      e._eventObj = this.getEvent;
      this.getEvent._simEvent = e;
    }
  }

  public final void opAssign(EventObj e) {
    import std.exception: enforce;	// enforce
    enforce(this._eventObj is null);
    this._eventObj = e;
  }

  public this(EventObj e) {
    this._eventObj = e;
  }

  // public static Event opCall() {
  //   Event event;
  //   event.init();
  //   return event;
  // }

  this(string name) {
    this.init(name);
  }

  public static Event[] opIndex(size_t n) {
    Event events[] = new Event[n];
    foreach(ref event;events) {
      synchronized {
	event.init();
      }
    }
    return events;
  }

  public final void init(NamedObj parent=null) {
    init(null, parent);
  }

  public final void init(string name, NamedObj parent=null) {
    synchronized {
      if(RootProcess.self !is null && parent is null) {
	assert(false, "Must provide parent for EventObj being "
	       "\"init\" during elaboration");
      }
      if(_eventObj is null) {
	_eventObj = new EventObj(parent);
      }
      if(name !is null) {
	_eventObj._esdl__nomenclate(name);
      }
    }
  }
  // alias init init;

  static void _esdl__inst(size_t I=0, T, L)(T t, ref L l) {
    l._esdl__objRef._esdl__inst!I(t, l._esdl__objRef);
  }

  static void _esdl__elab(size_t I, T, L)(T t, ref L l, uint[] indices=null)
  {
    debug(ELABORATE) {
      import std.stdio;
      writeln("** Event: Elaborating " ~ t.tupleof[I].stringof ~ ":" ~
	      typeof(l).stringof);
    }
    l._esdl__inst!I(t, l);
    synchronized(l._esdl__obj) {
      static if(is(T unused: ElabContext)) {
	t._esdl__addChildObj(l._esdl__obj);
      }
      l._esdl__obj._esdl__nomenclate!I(t, indices);
      l._esdl__obj._esdl__setParent(t);
    }
  }
}


// Event Queue -- just like event queues in SystemC.
// FIXME -- review later since this is not required for UVM --
// actually does not even have an equivalent functionality in
// SystemVerilog
public class EventQueueObj: EventObj
{
  import std.container: BinaryHeap;
  private TimedNotice[] _nQueue;
  private TimedNotice _currentNotice;

  static int less(T)(T a, T b) {
    return a < b;
  }

  static int greater(T)(T a, T b) {
    return a > b;
  }

  alias BinaryHeap!(TimedNotice[], greater) Heap;
  private Heap _nHeap;

  struct TimedNotice
  {
    enum SimTime invalid = SimTime(long.min);
    
    SimTime _time = invalid;

    public final bool isValid() {
      return _time != invalid;
    }

    public final void invalidate() {
      _time = invalid;
    }

    public final int opCmp(TimedNotice rhs) {
      if(this._time == rhs._time) return 0;
      if(this._time < rhs._time) return -1;
      else return 1;
    }
  }


  protected this(SimEvent simEvent=null) {
    super(simEvent);
    synchronized(this) {
      this._nQueue = new TimedNotice[4];
      this._nHeap = Heap(_nQueue, 0);
    }
  }

  protected final override void trigger(EsdlSimulator sim) {
    super.trigger(sim);
    assert((_currentNotice.isValid),
	   "Heap can not be empty when an EventQueueObj triggered");
    debug(EVENT_HEAP) {
      import std.stdio;
      writeln(getSimulator.simTime, "::", _currentNotice._time);
      assert(getSimulator.simTime == _currentNotice._time ||
	     // cover the immediate events
	     getSimulator.simTime == (_currentNotice._time + SimTime(1)));
    }

    if(_nHeap.empty()) {
      _currentNotice.invalidate();
    }
    else {
      _currentNotice = _nHeap.front();
      _nHeap.removeFront();
      auto t = _currentNotice._time - getSimulator.simTime;
      if(t == SimTime(-1)) {
	debug(EVENT_HEAP) {
	  import std.stdio;
	  writeln("Immediate Event: ", _nHeap.length);
	}
	super.notify();
      }
      else {
	super.notify(t);
      }
      debug(EVENT_HEAP) {
	import std.stdio;
	writeln("Next TimedNotice at time: ",
		_currentNotice._time - getSimulator.simTime);
      }
    }
  }

  public final override void cancel() {
    synchronized(this) {
      _currentNotice.invalidate();
      _nHeap.release();
      super.cancel();
    }
  }

  public final override void notify() {
    // since we want to store the immediate notifications too in the
    // same event heap, we shall represent immediate events as
    // occuring at SimTime -1
    this._notify(SimTime(-1));
  }

  public final override void notify(ulong t) {
    this.notify(SimTime(t * getTimeScale));
  }

  public final override void notify(SimTime t) {
    if(t == SimTime(-1)) {
      assert(false, "Negative Time provided with notify");
    }
    this._notify(t);
  }

  private final void _notify(SimTime t) {
    auto nTime = t + getSimulator.simTime;
    synchronized(this) {
      if(this._nQueue.length == this._nHeap.length) {
	// double the size of underlying array
	debug(EVENT_HEAP) {
	  import std.stdio: writeln;
	  writeln("Increasing the heap store to: ",
		  this._nQueue.length * 2);
	}
	this._nQueue.length *= 2;
	this._nHeap.assume(this._nQueue,
			   this._nHeap.length);
      }
      if(_currentNotice.isValid) {
	if(_currentNotice._time <= nTime) {
	  this._nHeap.insert(TimedNotice(nTime));
	  return;
	}
	else {
	  super.cancel();
	  this._nHeap.insert(_currentNotice);
	  _currentNotice = TimedNotice(nTime);
	  if(t == SimTime(-1)) super.notify();
	  else super.notify(t);
	}
      }
      else {
	_currentNotice = TimedNotice(nTime);
	if(t == SimTime(-1)) super.notify();
	else super.notify(t);
      }
    }
  }

  static void _esdl__inst(size_t I=0, U, L)(U u, ref L l)
  {
    synchronized(u) {
      if(l is null) l = new L();
    }
  }
}


// Wrapper struct for EventQueueObj
// Fixme -- review later -- but should be very similar to the Event
// struct
@_esdl__component struct EventQueue
{
  package EventQueueObj _eventObj = void;

  package final ref EventQueueObj _esdl__objRef() {
    return _eventObj;
  }

  final EventObj _esdl__obj() {
    // Use double-locked checking -- assumption Intel Processor Architecture
    // All the pointer read writes are atomic
    if(this._eventObj is null) {
      synchronized(typeid(EventQueue)) {
	if(this._eventObj is null) {
	  EventQueueObj be = new EventQueueObj();
	  this._eventObj = be;
	}
      }
    }
    return this._eventObj;
  }

  alias _esdl__obj this;
  alias _esdl__obj getEvent;

  // postblit
  this(this) {
    import std.exception: enforce;	// enforce
    enforce((this._eventObj !is null),
	    "Attempt to copy un-initialized event");
  }

  this(SimEvent e) {
    synchronized(getEvent, e) {
      import std.exception: enforce;	// enforce
      enforce((e._eventObj is null));
      e._eventObj = this.getEvent;
      this.getEvent._simEvent = e;
    }
  }


  // Disallow Event assignment
  @disable private void opAssign(Event e);

  // User API

  public final void opAssign(SimEvent e) {
    synchronized(getEvent, e) {
      import std.exception: enforce;	// enforce
      enforce(e._eventObj is null);
      enforce(this.getEvent._simEvent is null);
      e._eventObj = this.getEvent;
      this.getEvent._simEvent = e;
    }
  }


  // public static EventQueue opCall() {
  //   EventQueue event;
  //   event.init();
  //   return event;
  // }

  this(string name) {
    this.init(name);
  }

  public final void init() {
    synchronized {
      if(_eventObj is null) {
	_eventObj = new EventQueueObj();
      }
    }
  }

  public final void init(string name) {
    synchronized {
      if(_eventObj is null) {
	_eventObj = new EventQueueObj();
      }
      _eventObj._esdl__nomenclate(name);
    }
  }
  // alias init init;

  static void _esdl__inst(size_t I=0, T, L)(T t, ref L l) {
    l._esdl__objRef._esdl__inst!I(t, l._esdl__objRef);
  }

  static void _esdl__elab(size_t I, T, L)(T t, ref L l, uint[] indices=null)
  {
    debug(ELABORATE) {
      import std.stdio;
      writeln("** EventQueue: Elaborating " ~ t.tupleof[I].stringof ~ ":" ~
	      typeof(l).stringof);
    }
    l._esdl__inst!I(t, l);
    synchronized(l._esdl__obj) {
      static if(is(T unused: ElabContext)) {
	t._esdl__addChildObj(l._esdl__obj);
      }
      l._esdl__obj._esdl__nomenclate!I(t, indices);
      l._esdl__obj._esdl__setParent(t);
    }
  }
}

// helper function -- creates an event given any type of wait argument
// and returns that
private EventObj getEventObj(E)(ref E e) {
  static if(is(E unused: SimTime) ||
	    is(E unused: Time)) {
    Event event = Event();
    event.notify(e);
    return event;
  }
  else static if(is(E unused: EventObj)) {
      return e;
    }
  else static if(is(E unused: SimEvent)) {
      Event event = e;
      return event;
    }
  else static if(is(E unused: Process)) {
      // if the task is already terminated, return null
      if(e.isTerminated) {
	// return null;
	assert(false, "getEventObj called for a terminated task");
      }
      else {
	return e.getEvent;
      }
    }
  else static if(isIntegral!E) {
      // if the task is already terminated, return null
      // wait(SimTime(n * this.timeScale));
      Event event = Event();
      event.notify(e);
      return event.getEvent();
    }
  else static if(is(E unused: Port!(IF, N, M), IF, size_t N, size_t M) ||
		 is(E unused: PortObj!(IF, N, M), IF, size_t N, size_t M)) {
      static if(hasEventMethod!(IF, "defaultEvent")) {
	return e.defaultEvent();
      }
      else {
	static assert(false, "No defaultEvent associated with Port");
      }
    }
  else static if(is(E unused: Signal!(T, M), T, bool M) ||
		 is(E unused: SignalObj!(T, M), T, bool M)) {
      return e.defaultEvent();
    }
  else static if(is(E unused: IF, IF)) {
      static if(hasEventMethod!(IF, "defaultEvent")) {
	return e.defaultEvent();
      }
      else {
	static assert(false, "No defaultEvent associated with Interface: " ~
		      E.stringof);
      }
    }
    else {
      static assert(false, "Unable to convert to EventObj from type " ~
		    E.stringof);
    }
}

public SimEvent andEvents(E...)(ref E events) {
  AndSimEvent simE = new AndSimEvent();
  if(simEventsAdd(simE, 0, events) == 0) {
    Event dummy = Event();
    simEventsAdd(simE, 0, dummy);
    dummy.notify();
  }
}

public SimEvent orEvents(E...)(ref E events) {
  OrSimEvent simE = new OrSimEvent();
  simEventsAdd(simE, 0, events);
  return simE;
}

private size_t simEventsAdd(E...)
  (SimEvent simE, size_t count, ref E events) {
  static if(events.length == 0) {
    return count;
  }
  else {
    import std.range: ElementType;
    import std.traits: isIterable;
    static if(isIterable!(E[0])) {
      alias ElementType!(E[0]) eType;
      // static if(is(eType unused: EventObj)) {
      foreach(ref e; events[0]) {
	// terminated tasks would return a null
	auto event = getEventObj(e);
	if(event is null) {
	  assert(false, "Null event");
	}
	else {
	  ++count;
	  simE.addAgent(event);
	}
      }
    }
    else {
      ++count;
      simE.addAgent(getEventObj(events[0]));
    }
    return simEventsAdd(simE, count, events[1..$]);
  }
}

abstract private class SimEvent: EventClient
{
  private EventObj _eventObj;

  public this() {}

  // No need to synchronize these functions. In most cases it is the
  // EventObj which gets constructed first. Only in case of immediate
  // anded and ored events, the base might get constructed later --
  // and in those cases, the constrcuted event would be accessible
  // only to a single thread
  public this(EventObj event) {
    synchronized(this) {
      if(this._eventObj) {
	assert(false, "You are not allowed to re-initialize an Event");
      }
      this._eventObj = event;
    }
  }

  final public EventObj getObj() {
    synchronized(this) {
      if(this._eventObj is null) {
	assert(false, "No eventObj associated with SimEvent");
      }
      return this._eventObj;
    }
  }


  protected abstract void addAgent(EventObj agent);

  // return true if the given event is timed event
  public bool isTimed() {return false;}

  public void cancel() {assert(false, "Can cancel only a timed event");}

  public bool notify() {assert(false, "Can notify only a timed event");}

  public bool notify(SimTime steps) {assert(false, "Can notify only a timed event");}

  public bool notify(Time steps) {assert(false, "Can notify only a timed event");}

  // Called only during the schedule phase
  protected abstract void poke(EsdlSimulator sim, EventObj agent, size_t index);
  // Only during the schedule phase
  public void trigger(EsdlSimulator sim) {assert(false, "Can trigger only a timed event");}

  public final RootEntityIntf getRoot() {
    return this.getObj.getRoot;
  }
  public final EsdlSimulator getSimulator() {
    return getRoot().simulator();
  }

}

abstract class EventExpr: SimEvent
{
  // list of events that are in the expression
  private EventObj[] _agents;
}


// Fixme -- unlike EventExpr, a list creates a new event only when you
// wait on it -- implementation TBD
abstract class EventList: SimEvent
{
  // list of events that are in the expression
  private EventObj[] _agents;
}

private enum NotifyPolicy: bool
  {   EARLIER = false,
      OVERRIDE = true
      }

final private class TimedEvent: SimEvent
{
  public this(EventObj event) {
    super(event);
  }

  enum Schedule: ubyte
    {   NONE,
	NOW,
	DELTA,
	TIMED
	}

  private Schedule _schedule;
  private NotifyPolicy _notifyPolicy = NotifyPolicy.EARLIER;
  private EventNotice _notice;
  private size_t _eventQueueIndex;

  public final override bool isTimed() {
    return true;
  }

  public final override void cancel() {
    synchronized(this) {
      final switch(this._schedule) {
      case Schedule.NONE:
	// Not scheduled -- do nothing
	break;
      case Schedule.NOW:
	this.getObj.getSimulator._scheduler.cancelImmediateEvent(this);
	this._schedule = Schedule.NONE;
	break;
      case Schedule.DELTA:
	this.getObj.getSimulator._scheduler.cancelDeltaEvent(this);
	this._schedule = Schedule.NONE;
	break;
      case Schedule.TIMED:
	if(this._notice is null) {
	  assert(false, "TimedEvent is schedules as TIMED, "
		 "but associated notice is null");
	}
	this._notice.annul();
	this._notice = null;
	this._schedule = Schedule.NONE;
	break;
      }
    }
  }

  // called in the single threaded schedule phase, hence no need for
  // the synchronization
  public final override void trigger(EsdlSimulator sim) {
    if(_schedule == Schedule.DELTA ||
       _schedule == Schedule.NOW) {
      this._schedule = Schedule.NONE;
    }
    else {
      this.cancel();
    }
    this.getObj.trigger(sim);
  }

  // Immediate notification -- TBD
  public final override bool notify() {
    // Cancel the already scheduled notifications
    synchronized(this) {
      if(this._schedule != Schedule.NOW) {
	cancel();
	this._schedule = Schedule.NOW;
	this._eventQueueIndex =
	  this.getSimulator._scheduler.insertImmediate(this);
      }
      return true;
    }

    // auto thread = staticCast!SimThread(Thread.getThis());
    // if(Process.self is null) {
    //   assert(false, "Immediate notification does not work in Routines!");
    // }
    // else {
    //   Process.self.wait(this.getObj);
    // }
  }

  public final override bool notify(SimTime steps) {
    synchronized(this) {
      if(_notifyPolicy is NotifyPolicy.OVERRIDE) {
	if(this._schedule != Schedule.NONE) {
	  this.cancel();
	}
	if(steps == DELTA)	{ // delta event
	  this._schedule = Schedule.DELTA;
	  this._eventQueueIndex =
	    this.getSimulator._scheduler.insertDelta(this);
	  return true;
	}
 	else {	// request for scheduling timed event
	  // Create a new timed event
	  _notice = EventNotice.alloc(this.getSimulator.simTime
				      + steps, this);
	  // register the timed event with the simulator
	  this._schedule = Schedule.TIMED;
	  this.getSimulator._scheduler.insertTimed(_notice);
	  return true;
	}
      }
      else { // ! if(_notifyPolicy is NotifyPolicy.OVERRIDE) {
	if(this._schedule == Schedule.NOW) {
	  return false;
	}
	if(this._schedule == Schedule.DELTA) {
	  return true;
	}
	if(steps == DELTA)	{ // delta event
	  if(this._schedule == Schedule.TIMED) {
	    debug {
	      import std.stdio: writeln;
	      writeln("Cancelling scheduled event");
	    }
	    this.cancel();
	  }
	  this._schedule = Schedule.DELTA;
	  this._eventQueueIndex =
	    this.getSimulator._scheduler.insertDelta(this);
	  return true;
	}
	else {	// request for scheduling timed event
	  // Check if the event is already scheduled and at what time
	  if(this._schedule == Schedule.TIMED) {
	    // Compare if the new schedule is sooner than the old
	    if((this.getSimulator.simTime() + steps) >= _notice.atTime) {
	      // Event already scheduled for an earlier time
	      return false;
	    }
	    else {
	      // Event needs rescheduling
	      this.cancel();
	    }
	  }
	  // Create a new timed event
	  _notice = EventNotice.alloc(this.getSimulator.simTime
				      + steps, this);
	  // register the timed event with the simulator
	  this._schedule = Schedule.TIMED;
	  this.getSimulator._scheduler.insertTimed(_notice);
	  return true;
	}
      }
    }
  }

  // called in the single threaded schedule phase, hence no need for
  // the synchronization
  protected final override void poke(EsdlSimulator sim, EventObj agent, size_t index) {
    assert(false, "TimedEvent not dependent on any other event: "
	   ~ this._eventObj.getFullName() ~ " -- poked");
  }


  protected final override void addAgent(EventObj agent) {
    assert(false, "TimedEvent not dependent on any other event: "
	   ~ this._eventObj.getFullName() ~ " -- addAgent");
  }
}

// Mostly like the OrSimEvent -- the only difference is that a
// ProxyEvent can have only one agent poking it
final private class ProxyEvent: SimEvent
{
  public this() {
    super();
  }

  private EventObj _agent = void;

  protected final override void addAgent(EventObj agent) {
    agent.addClientEvent(this, 0);
    synchronized(this) {
      if(_agent) {
	assert(false, "ProxyEvent can have only one agent");
      }
      _agent = agent;
    }
  }

  // called in the single threaded schedule phase, hence no need for
  // the synchronization
  protected final override void poke(EsdlSimulator sim,
				     EventObj agent, size_t index) {
    if(this._eventObj !is null) {
      this._eventObj.trigger(sim);
    }
  }
}

final private class OrSimEvent: EventExpr
{
  public this() {
    super();
  }

  protected final override void addAgent(EventObj agent) {
    agent.addClientEvent(this, 0);
    synchronized(this) {
      _agents ~= agent;
    }
  }

  // called in the single threaded schedule phase, hence no need for
  // the synchronization
  protected final override void poke(EsdlSimulator sim,
				     EventObj agent, size_t index) {
    if(this._eventObj !is null) {
      this._eventObj.trigger(sim);
    }
  }

}

private class AndSimEvent: EventExpr
{
  public this() {
    synchronized(this) {
      super();
    }
  }

  struct Objections
  {
    byte flags[];
    uint num;

    size_t length() {
      return this.num;
    }

    void reset() {
      foreach(i, ref flag; flags) {
	if(i < num/8) {
	  flags[i] = -1;
	} else {
	  flags[i] = (1 << (num % 8)) - 1;
	}
      }
    }

    // regiter a poke -- return true if all the flags have fallen
    bool unflag(size_t index) {
      bool retval = true;
      this.flags[index/8] &= ~(1 << (index % 8));
      foreach(flag; flags) {
	if(flag != 0) retval = false;
	debug(ANDEDEVENT) {
	  import std.stdio: writefln;
	  writefln("objections: %08b", flag);
	}
      }
      return retval;
    }

    size_t addFlag() {
      if((num % 8) == 0) flags.length += 1;
      this.flags[num/8] |= 1 << (num % 8);
      this.num += 1;
      return this.num - 1;
    }
  }

  private Objections _objections;

  protected final override void addAgent(EventObj agent) {
    size_t index;
    synchronized(this) {
      index = this._objections.addFlag();
      _agents ~= agent;
    }
    agent.addClientEvent(this, index);
  }

  // called in the single threaded schedule phase, hence no need for
  // the synchronization
  protected final override void poke(EsdlSimulator sim,
				     EventObj agent, size_t index) {
    if(this._objections.unflag(index)) {
      this.getObj.trigger(sim);
    }
  }
}

final class EventNotice
{
  // Since _event is used for checking if an event has been cancelled
  // (and thus sent back to freelist), _event can not be made a member
  // of a union

  private union TimeNoticeUnion {
    private EventNotice _next;		// for use by FreeList
    private SimTime _time;
  }

  private TimeNoticeUnion unionTimeNext;
  alias unionTimeNext this;

  private TimedEvent _event;

  __gshared private EventNotice freelist;

  static EventNotice alloc(SimTime t, TimedEvent event) {
    synchronized(typeid(EventNotice)) {
      // assert(t > SimTime(0));
      debug {
	import std.stdio: writeln;
	writeln("Creating Timed Event at time ", t.getVal);
      }
      EventNotice f;
      if(freelist !is null) {
	f = freelist;
	freelist = f._next;
	f._time = t;
	f._event = event;
      }
      else {
	f = new EventNotice(t, event);
      }
      return f;
    }
  }

  // called in the single threaded schedule phase, hence no need for
  // the synchronization
  public final void trigger(EsdlSimulator sim) {
    if(this._event is null) {
      debug {
	import std.stdio: writeln;
	writeln("An annulled event notice got triggered");
      }
    }
    else {
      this._event.trigger(sim);
    }
    // once notified, let the event go back to freelist.
    dealloc(this);
  }

  static void dealloc(EventNotice f) {
    f._event = null;
    f._next = freelist;
    freelist = f;
  }

  // _time is effectively immutable
  package final SimTime atTime() {
    // synchronized(this) {
    return this._time;
    // }
  }

  // Use annull or dealloc instead
  // final private void cancel()
  // {
  //   _event = null;
  //   this._next = freelist;
  //   this.freelist = this;
  // }

  public final void annul() {
    synchronized(this) {
      this._event = null;
    }
  }

  // make new not callable directly -- private
  private this(SimTime t, TimedEvent event) {
    synchronized(this) {
      this._time = t;
      this._event = event;
    }
  }

  private final int opCmp(EventNotice rhs) {
    // if(rhs is null) return -1;
    // if(this is null) return 1;
    synchronized(this, rhs) {
      return this._time.opCmp(rhs._time);
    }
  }
}



interface Procedure: HierComp
{
  // returns true if the process was spawned at run time
  // on the other hand if the process was created at time of
  // elaboration, return false.
  public bool isDynamic();
  // helper functions to keep track of dynamic child processes
  protected void addProcess(Process t);
  protected void removeProcess(Process t);

  // find out what procedure is running now
  public static Procedure self() {
    if(Process.self !is null) return Process.self;
    if(Routine.self !is null) return Routine.self;
    return RootProcess.self;
  }

  // Functions for Random Stability
  protected ref Random randGen();

  public final void srandom(uint _seed) {
    synchronized(this) {
      randGen().seed(_seed);
    }
  }

  public final Random getRandState() {
    synchronized(this) {
      return randGen.save();
    }
  }

  public final void setRandState(Random state) {
    synchronized(this) {
      randGen = state;
    }
  }

}

public void waitAll(E...)(E events) {
  AndSimEvent simE = new AndSimEvent();
  auto count = simEventsAdd(simE, 0, events);
  if(count == 0) {
    Event dummy = Event();
    simEventsAdd(simE, 0, dummy);
    dummy.notify();
  }
  wait(simE);
}

public void waitAny(E...)(E events) {
  OrSimEvent simE = new OrSimEvent();
  simEventsAdd(simE, 0, events);
  wait(simE);
}

public auto observe(E)(E e)
  if(is(E: NotificationObj!T, T) ||
     is(E: Notification!T, T) ||
     is(E: NotificationQueueObj!T, T) ||
     is(E: NotificationQueue!T, T)) {
    waitForEvent(e);
    return e.get();
  }

public void wait(E)(E e)
  if(!is(E == struct) &&
     !isIntegral!E) {
  auto event = getEventObj(e);
  waitForEvent(event);
}

public void wait(I)(I n)
  if(isIntegral!I &&
     !is(I: bool)) {
    wait(SimTime(n * getTimeScale()));
  }

public void wait(E)(E e)
  if(is(E: SimTime) || is(E: Time)) {
      auto event = Process.self._timed;
      event.notify(e);
      waitForEvent(event);
    }

public void sleep(E)(E e)
  if(is(E: SimTime) || is(E: Time)) {
      auto event = Process.self._timed;
      event.notify(e);
      waitForEvent(event);
    }

public void sleep(I)(I n)
  if(isIntegral!I &&
     !is(I: bool)) {
    wait(SimTime(n * getTimeScale()));
  }

public void sleep()() { // sleep forever
  auto event = Process.self._timed;
  waitForEvent(event);
}

public void wait(ref Event e) {
  e.init();
  auto event = getEventObj(e);
  waitForEvent(event);
}

public void wait(Event e) {
  auto event = getEventObj(e);
  waitForEvent(event);
}

public void wait(ref EventQueue e) {
  e.init();
  auto event = getEventObj(e);
  waitForEvent(event);
}

public void wait(EventQueue e) {
  auto event = getEventObj(e);
  waitForEvent(event);
}

public void wait(T)(ref Notification!T e) {
  e.init();
  auto event = getEventObj(e);
  waitForEvent(event);
}

public void wait(T)(Notification!T e) {
  auto event = getEventObj(e);
  waitForEvent(event);
}

public void wait(T)(ref NotificationQueue!T e) {
  e.init();
  auto event = getEventObj(e);
  waitForEvent(event);
}

public void wait(T)(NotificationQueue!T e) {
  auto event = getEventObj(e);
  waitForEvent(event);
}

public void wait(IF, size_t N, size_t M)(ref Port!(IF, N, M) e) {
  e.init();
  auto event = getEventObj(e);
  waitForEvent(event);
}

public void wait(IF, size_t N, size_t M)(Port!(IF, N, M) e) {
  auto event = getEventObj(e);
  waitForEvent(event);
}

public void wait(T, bool M)(ref Signal!(T, M) e) {
  e.init();
  auto event = getEventObj(e);
  waitForEvent(event);
}

public void wait(T, bool M)(Signal!(T, M) e) {
  auto event = getEventObj(e);
  waitForEvent(event);
}

public void waitp(E)(E e)
  if(!is(E == struct)) {
  auto event = getEventObj(e);
  waitForEventP(event);
}

public void waitp(E)(E e)
  if(is(E: SimTime) || is(E: Time))
    {
      auto event = Process.self._timed;
      event.notify(e);
      waitForEventP(event);
    }

public void waitp(ref Event e) {
  e.init();
  auto event = getEventObj(e);
  waitForEventP(event);
}

public void waitp(Event e) {
  auto event = getEventObj(e);
  waitForEventP(event);
}

public void waitp(ref EventQueue e) {
  e.init();
  auto event = getEventObj(e);
  waitForEventP(event);
}

public void waitp(EventQueue e) {
  auto event = getEventObj(e);
  waitForEventP(event);
}

public void waitp(T)(ref Notification!T e) {
  e.init();
  auto event = getEventObj(e);
  waitForEventP(event);
}

public void waitp(T)(Notification!T e) {
  auto event = getEventObj(e);
  waitForEventP(event);
}

public void waitp(T)(ref NotificationQueue!T e) {
  e.init();
  auto event = getEventObj(e);
  waitForEventP(event);
}

public void waitp(T)(NotificationQueue!T e) {
  auto event = getEventObj(e);
  waitForEventP(event);
}

public void waitp(IF, size_t N, size_t M)(ref Port!(IF, N, M) e) {
  e.init();
  auto event = getEventObj(e);
  waitForEventP(event);
}

public void waitp(IF, size_t N, size_t M)(Port!(IF, N, M) e) {
  auto event = getEventObj(e);
  waitForEventP(event);
}

public void waitp(T, bool M)(ref Signal!(T, M) e) {
  e.init();
  auto event = getEventObj(e);
  waitForEventP(event);
}

public void waitp(T, bool M)(Signal!(T, M) e) {
  auto event = getEventObj(e);
  waitForEventP(event);
}


public void wait() {
  if(Process.self is null) {
    assert(false, "Wait can be called only from a process");
  }
  Process.self.wait();
}

public void waitp() {
  if(Process.self is null) {
    assert(false, "Wait can be called only from a process");
  }
  Process.self.waitp();
}

// public void wait(IF, int N, int M) (Port!(IF, N, M) port)
// {
//   static if(hasEventMethod!(IF, "defaultEvent")) {
//     port.wait();
//   }
//   else {
//     static assert("Error: No 'defaultEvent' method for channel type");
//   }
// }

private void waitForEventP(EventObj event) {
  if(Process.self is null) {
    assert(false, "Wait can be called only from a process");
  }
  Process.self.waitSensitiveP(event);
}

private void waitForEvent()(EventObj event) {
  if(Process.self is null) {
    assert(false, "Wait can be called only from a process");
  }
  Process.self.waitSensitive(event);
}

// public void wait(U, bool MULTI=1)(SignalObj!(U, MULTI) sig) {
//   wait(sig.defaultEvent());
// }

public void waitDelta() {
  Event event = Event();
  event.notify(DELTA);
  wait(event);
}


public void waitForks() {
  // get the thread this call is made from
  if(Process.self is null) {
    assert(false, "waitRec can only be called from inside a Process");
  }
  Process[] procs = Process.self._esdl__getChildProcsHier();
  EventObj[] events;
  foreach(proc; procs) {
    events ~= proc.getRecEvent();
  }
  waitAll(events);
}

public void abortForks() {
  // get the thread this call is made from
  if(Process.self is null) {
    assert(false, "abortForks can only be called from inside a Process");
  }
  foreach(t; Process.self._esdl__getChildProcs()) {
    t.abortRec();
  }
}

public void nextTrigger(E)(E e)
  if(!is(E == struct) &&
     !isIntegral!E) {
    auto event = getEventObj(e);
    _nextTriggerEvent(event);
  }

public void nextTrigger(I)(I n)
  if(isIntegral!I &&
     !is(I: bool)) {
    nextTrigger(SimTime(n * getTimeScale()));
  }

public void nextTrigger(E)(E e)
  if(is(E: SimTime) || is(E: Time)) {
      auto event = Routine.self._timed;
      event.notify(e);
      _nextTriggerEvent(event);
    }

private void _nextTriggerEvent(EventObj event) {
  if(Routine.self is null) {
    assert(false, "nextTrigger may be called only from a Routine");
  }
  Routine.self.nextTrigger(event);
}

public Process process(DelegateThunk dg, int stage = 0, size_t sz = 0) {
  Process f = new Process(dg, stage, sz);
  return f;
}

public Process process(FunctionThunk fn, int stage = 0, size_t sz = 0) {
  Process f = new Process(fn, stage, sz);
  return f;
}

public Routine routine(DelegateThunk dg) {
  Routine f = new Routine(dg);
  return f;
}

public Routine routine(FunctionThunk fn) {
  Routine f = new Routine(fn);
  return f;
}

private void forkHelper(F...)(ref Process[] procs, F thunks) {
  static if(F.length == 0) {
    return;
  }
  else static if(is(F[0]: DelegateThunk) ||
		 is(F[0]: FunctionThunk)) {
      procs ~= process(thunks[0], getStage);
      forkHelper(procs, thunks[1..$]);
    }
  else static if(is(F[0]: Process)) {
      procs ~= thunks[0];
      forkHelper(procs, thunks[1..$]);
    }
    else {
      static assert(false, "join can take only functions, delegates "
		    "or processes as arguments");
    }
}

// public Fork joinAll(F...)(F thunks) {
//   Process[] procs;
//   forkHelper(procs, thunks);
//   Fork retval = new Fork(procs);
//   waitAll(procs);
//   return retval;
// }

// public Fork joinAny(F...)(F thunks) {
//   Process[] procs;
//   forkHelper(procs, thunks);
//   Fork retval = new Fork(procs);
//   waitAny(procs);
//   return retval;
// }

// public Fork joinNone(F...)(F thunks) {
//   Process[] procs;
//   forkHelper(procs, thunks);
//   Fork retval = new Fork(procs);
//   return retval;
// }

public Fork fork(F...)(F thunks) if(F.length > 1) {
  Process[] procs;
  forkHelper(procs, thunks);
  Fork retval = new Fork(procs);
  return retval;
}

public ForkMono fork(F)(F thunk) {
  Process proc;
  static if(is(F: DelegateThunk) ||
	    is(F: FunctionThunk)) {
    proc = process(thunk, getStage);
  }
  else static if(is(F: Process)) {
      proc = thunk;
    }
    else {
      static assert(false, "fork can take only functions, delegates "
		    "or processes as arguments");
    }

  return new ForkMono(proc);
 }

public Routine cron(E, F)(E event, F thunk) {
  Routine rtn;
  auto e = getEventObj(event);
  static if(is(F: DelegateThunk) ||
	    is(F: FunctionThunk)) {
    rtn = new Routine(thunk);
  }
  else static if(is(F: Routine)) {
      rtn = thunk;
    }
    else {
      static assert(false, "cron can take only a function, a delegate "
		    "or a Routine as arguments");
    }
  rtn.sensitiveTo(e);
  return rtn;
}

public int getStage() {
  if(Process.self is null) {
    assert(false,
	   "You can call getStage from only within a process");
  }
  return Process.self.stage();
}

public void lockStage() {
  if(Process.self is null) {
    assert(false,
	   "You can call lockStage from only within a process");
  }
  Process.self.raisePersistFlag();
}

public void unlockStage() {
  if(Process.self is null) {
    assert(false,
	   "You can call unlockStage from only within a process");
  }
  Process.self.dropPersistFlag();
}

// Simulation related methods are made available by this
// interface. The interface is implemented by EsdlSimulator and by
// Entity. The Entity just passes the messages to the
// EsdlSimulator via the 'getSimulator'.
interface SimContext: NamedObj { }

// Each process, routine and the root process have their own random
// generator. This is done to enable random stability.
private ref Random getRandGen() {
  Procedure proc;
  proc = Process.self;
  if(proc is null) {
    proc = RootProcess.self;
  }
  if(proc !is null) {
    return proc.randGen();
  }
  auto routine = Routine.self;
  if(routine !is null) {
    return routine.randGen();
  }
  else {
    assert(false, "randGen can be accessed only from a Process,"
	   " RootProcess, or Routine");
  }
}

public T urandom(T=uint)() {
  return uniform!T(getRandGen());
}

public T urandom(string BOUNDARY="[]", T=uint)(T min, T max) {
  return uniform!(BOUNDARY, T)(min, max, getRandGen());
}

public T urandom_range(string BOUNDARY="[]", T=uint)(T min, T max) {
  return uniform!(BOUNDARY, T)(min, max, getRandGen());
}

public void srandom(uint _seed) {
  getRandGen().seed(_seed);
}

@_esdl__component struct Inst(M : EntityIntf)
{
  alias M EntityType;
  private M _esdl__instance = void;

  package ref M _esdl__objRef() {
    return this._esdl__instance;
  }

  M _esdl__obj() {
    // _esdl__instance is effectively immutable since the
    // initialization is done during the elaboration phase.
    // synchronized(typeid(M)) {
    if(_esdl__instance is null) {
      assert(false, "Uninitialized Inst");
    }
    return this._esdl__instance;
    // }
  }

  alias _esdl__obj this;

  // Disallow Inst assignment
  @disable void opAssign(Inst);
  // Allow assigning from module handle once
  public final void opAssign(M m) {
    synchronized(typeid(M)) {
      // Allow it only once
      this._esdl__instance &&
	assert(false, "Inst re-initialization not allowed");

      this._esdl__instance = m;
    }
  }

  // Next two methods help with elaboration
  static void _esdl__inst(size_t I=0, T, L)(T t, ref L l) {
    l._esdl__objRef._esdl__inst!I(t, l._esdl__objRef);
  }
  static void _esdl__elab(size_t I, T, L)(T t, ref L l, uint[] indices=null)
  {
    debug(ELABORATE) {
      import std.stdio;
      writeln("** Inst: Elaborating " ~ t.tupleof[I].stringof ~ ":" ~
	      typeof(l).stringof);
    }
    l._esdl__objRef._esdl__elab!I(t, l._esdl__objRef, indices);
  }
}

interface EntityIntf: ElabContext, SimContext, TimeConfigContext
{

  static string elabTasks() {
    return
      q{

      class TaskT(T, alias F, int R=0, size_t S=0): Task!(F, R, S)
      {
	this(T t) {
	  auto dg = recreateDelegate!F(t);

	  static assert((ParameterTypeTuple!dg).length == 0);
	  super(dg, R, S);
	  //	{
	  //	  auto ARGS = adjustArgs!(T, A)(t);
	  //	  auto fn = delegate()
	  //	    {
	  //	      dg(ARGS.expand);
	  //	    };
	  //	  super(fn, 0);
	  //	}
	  // else
	  // {
	  // }
	}

      }
    };
  }

  static string configTime() {
    return q{

      // Fix the various time configuration parameters for an entity
      // once the time precision has been fixed
      public final override void fixTimeParameters(ulong scale = 0) {
	synchronized(this) {
	  // this._phase = SimPhase.CONFIGURE;
	  if(this._timeScale is 0 && scale is 0) {
	    import std.stdio;
	    writeln("No default timeUnit specified; "
		    "setting timeUnit to 1.nsec");
	    this.timeUnit = 1.nsec;
	  }

	  if(this._timeScale is 0) {
	    // if timeScale not defined, take it from the parent
	    this._timeScale = scale;
	  }
	  
	  foreach(ref c; this._esdl__childObjs) {
	    if(TimeConfigContext m = cast(TimeConfigContext) c) {
	      synchronized(m) {
		// propagate the scale downwards
		m.fixTimeParameters(this._timeScale);
	      }
	    }
	  }
	}
      }
    };
  }

  // EntityIntf Constructor
  static string entityMixin() {
    return
      // "import std.stdio: writeln;" ~
      hierMixin() ~
      hierContextMixin() ~
      configTime() ~
      timedMixin() ~
      elabTasks();

  }
}

// Entity class -- alternate is to inherit from EntityIntf and use the
// entityMixin
class Entity: EntityIntf
{
  mixin(entityMixin);
}



template Task(alias F, int R=0, size_t S=0)
{
  static if(__traits(compiles, F())) {
    // pragma(msg, F.stringof);
    class Task: Process
    {
      alias F _FUNCTION;
      enum ulong _STACKSIZE = S;

      this() {
	// import std.stdio;
	// writeln("New Dynamic Task");
	super(F, S);
      }
    }
  }
  else {
    // Normally during elaboration of the tasks, this branch would be taken
    class Task: Process
    {
      alias F _FUNCTION;
      enum ulong _STACKSIZE = S;

      protected this(void delegate() dg, int stage, size_t stackSize) {
	super(dg, stage, stackSize);
      }

      static void _esdl__inst(size_t I=0, T, L)(T t, ref L l)
      {
	synchronized(t) {
	  // import std.functional; // used for toDelegate
	  // import std.traits;

	  l = t.new t.TaskT!(T, F, R, S)(t);
	  // string getFuncName()
	  // {
	  //   return "t." ~ __traits(identifier, F);
	  // }

	  // import std.stdio;
	  // writeln("New thread for: ", t.getFullName);
	  // // does not work for functions with default arguments
	  // // typeof(toDelegate(&F)) dg;
	  // // pragma(msg, getFuncName());
	  // typeof(& mixin(getFuncName())) dg;

	  // dg.funcptr = &F;
	  // dg.ptr = cast(void *)t;

	  // static if((ParameterTypeTuple!dg).length > 0)
	  //   {
	  //     auto fun = delegate()
	  //	 {
	  //	   dg(ARGS);
	  //	 };
	  //     l = new Task!(F, A)(fun, 0);
	  //     // l = new Process(fun, 0);
	  //   }
	  // else
	  //   {
	  //     l = new Task!(F, A)(dg, 0);
	  //     // l = new Process(dg, 0);
	  //   }
	}
      }

      static void _esdl__elab(size_t I, T, L)
	(T t, ref L l, uint[] indices=null) {
	debug(ELABORATE) {
	  import std.stdio;
	  writeln("** Task: Elaborating " ~ t.tupleof[I].stringof ~ ":" ~
		  typeof(l).stringof);
	}
	l._esdl__inst!I(t, l);
	synchronized(l) {
	  import core.sync.semaphore: Semaphore;
	  static if(is(T unused: ElabContext)) {
	    t._esdl__addChildObj(l);
	    t._esdl__addChildTask(l);
	    t._esdl__addChildComp(l);
	  }
	  l._dynamic = false;
	  l._esdl__setIndices(indices);
	  l._esdl__nomenclate!I(t, indices);
	  l._esdl__setRoot(t.getRoot);
	  l._esdl__setParent(t);
	  t._esdl__register(l);
	  auto linfo = _esdl__get_parallelism!I(t, l);
	  l._esdl__parLock = t._esdl__getLockMutex;
	}
      }

      // this()
      //   {
      //     static assert((ParameterTypeTuple!F).length == 0);
      //     auto dg =
      //       recreateDelegateFromVoidPtr!F(cast(void *) this.outer);
      //     super(dg, _STACKSIZE);
      //   }

    }
    // }
  }
}

// private auto recreateDelegateFromVoidPtr(alias F)(void* _entity)
// {
//   void delegate() dg;
//   dg.funcptr = &F;
//   // Need to add dilligence here
//   // But how to make sure that the function does not take arguments
//   dg.ptr = _entity;
//   return dg;
// }

private auto recreateDelegate(alias F, T)(T _entity)
{
  final string getFuncName() {
    return "_entity." ~ __traits(identifier, F);
  }

  // does not work for functions with default arguments
  // typeof(toDelegate(&F)) dg;
  // pragma(msg, getFuncName());
  typeof(& mixin(getFuncName())) dg;

  dg.funcptr = &F;
  dg.ptr = cast(void *) _entity;

  return dg;
}

// auto adjustArgs(T, A...)(T t)
// {
//   import std.typecons;
//   static if(A.length == 0) return tuple();
//   else
//     {
//       auto p = adjustArgs!(T, A[0..$-1])(t);
//       static if(__traits(compiles, t.A[$-1]))
//	{
//	  pragma(msg, "Adding Entity Scope Resolution");
//	  return tuple(p.expand, t.A[$-1]);
//	}
//       else
//	{
//	  return tuple(p.expand, A[$-1]);
//	}
//     }
// }



// Alternative task definition using struct -- we are using

// struct Task(string THUNK, size_t STACKSIZE=0L)
// {
//   static immutable string _THUNK = THUNK;
//   enum ulong _STACKSIZE = STACKSIZE;

//   private Process _proc;

//   package ref Process _esdl__objRef()
//   {
//     return this._proc;
//   }

//   Process _esdl__obj()
//   {
//     synchronized {
//       import std.exception: enforce;
//       enforce(this._proc, "Uninitialized Task");
//       return this._proc;
//     }
//   }

//   alias _esdl__obj this;

//   @disable void opAssign(Task);
//   // Allow assigning from module handle once
//   public void opAssign(Process task)
//   {
//     synchronized {
//       // Allow it only once
//       this._proc &&
//	assert(false, "Task re-initialization not allowed");

//       this._proc = task;
//     }
//   }

// PersistFlag object will always work with a single thread and so no
// need for synchronisation guards.
class PersistFlag
{
  private bool _registered;
  private bool _raised;

  final public void raise(EsdlSimulator root) {
    // register if necessary
    if(! _registered) root.registerPersistFlag(this);
    _raised = true;
  }

  final public bool raised() {
    return _raised;
  }

  final public bool dropped() {
    return (! _raised);
  }

  final public void drop(EsdlSimulator root) {
    _raised = false;
  }

  final public void deregister() {
    assert(! _raised);
    _registered = false;
  }
}

abstract class SimThread: Thread
{
  this( void function() fn, size_t sz = 0 ) {
    super(fn, sz);
    _persist = new PersistFlag;
  }

  this( void delegate() dg, size_t sz = 0 ) {
    super(dg, sz);
    _persist = new PersistFlag;
  }

  protected PersistFlag _persist;

  public final void raisePersistFlag() {
    _persist.raise(getSimulator);
  }

  public final void dropPersistFlag() {
    _persist.drop(getSimulator);
  }

  public abstract void waitSensitiveP(EventObj event);
  public abstract void waitSensitive(EventObj event);
  public abstract void waitSensitiveP();
  public abstract void waitSensitive();
  public abstract void nextTrigger(EventObj event);
  public abstract EventObj nextTrigger();

  public abstract RootEntityIntf getRoot();
  public abstract EsdlSimulator getSimulator();

}

abstract class SimProcess: SimThread
{
  import core.sync.semaphore: Semaphore;

  this( void function() fn, size_t sz = 0 ) {
    synchronized(this) {
      super(fn, sz);
      _waitLock = new Semaphore(0);
      state = ProcState.STARTING;
    }
  }

  this( void delegate() dg, size_t sz = 0 ) {
    synchronized(this) {
      super(dg, sz);
      _waitLock = new Semaphore(0);
      state = ProcState.STARTING;
    }
  }

  final void caughtException() {
    state = ProcState.EXCEPTION;
    // _execLock.notify();
  }

  static if(!__traits(compiles, _esdl__root)) {
    @_esdl__ignore protected RootEntityIntf _esdl__root;	// The EsdlSimulator
  }


  protected override RootEntityIntf getRoot() {
    synchronized(this) {
      return _esdl__root;
    }
  }

  protected override EsdlSimulator getSimulator() {
    return getRoot().simulator();
  }

  // final protected void waitBarrier() {
  //   // synchronized(this)
  //   //	{
  //   this.getSimulator.freeLock();
  //   // }
  // }

  // final protected void freeThreadSlot() {
  //   // synchronized(this)
  //   //	{
  //   this.getSimulator.freeThreadSlot();
  //   // }
  // }

  private Semaphore _waitLock;
  // D Mutexes are recursive -- therefor the same thread can lock a
  // Mutex multiple times. This behaviour is not useful for us.
  // private Semaphore _execLock;

  // class State {};
  //  private State _stateMonitor;

  private ProcState _state = ProcState.STARTING;
  private bool _dontInit      = false;
  private ProcState _origState;
  private bool _hasStarted = false;

  private final  bool hasStarted() {
    return _hasStarted;
  }

  private final void start(){
    synchronized(this) {
      _hasStarted = true;
    }
    super.start();
  }

  private final ProcState state() {
    synchronized(this /*_stateMonitor*/) {
      debug(THREAD) {
	import std.stdio;
	Process t = cast(Process) this;
	if(t !is null)
	  writeln(t.getFullName, ": Thread is ", _state);
      }
      return _state;
    }
  }

  protected ProcStateReq reqState() {
    assert(false, "This function is overriden in the derived function");
  }

  private final void state(ProcState s) {
    synchronized(this /*_stateMonitor*/) {
      debug(THREAD) {
	import std.stdio;
	Process t = cast(Process) this;
	if(t !is null)
	  writeln(t.getFullName, ": Setting Thread to ", s);
      }
      _state = s;
    }
  }

  private final bool isWaiting() {
    synchronized(this /*_stateMonitor*/) {
      return(_state == ProcState.WAITING ||
	     _state == ProcState.SUSPENDED ||
	     _state == ProcState.DISABLED);
    }
  }

  private final void requestSuspension(bool REC) {
    synchronized(this) {
      final switch(_state) {
      case ProcState.STARTING:
      case ProcState.WAITING:
	this._origState = this._state;
	if(REC) {
	  this._state = ProcState.SUSPENDED;
	}
	else {
	  this._state = ProcState.SUSPENDED_REC;
	}
	break;
      case ProcState.FINISHED:
      case ProcState.EXCEPTION:
      case ProcState.ABORTED:
      case ProcState.ABORTED_REC:
      case ProcState.SUSPENDED:
      case ProcState.SUSPENDED_REC:
      case ProcState.DISABLED:
      case ProcState.DISABLED_REC:
      case ProcState.TERMINATED:
	assert(false, "Cannot suspend a process in state " ~ _state);
      case ProcState.RUNNING:
	assert(false, "Impossible Thread State");
      }
    }
  }

  private final void requestDisable(bool REC) {
    synchronized(this) {
      final switch(_state) {
      case ProcState.STARTING:
      case ProcState.WAITING:
	this._origState = this._state;
	if(REC) {
	  this._state = ProcState.DISABLED_REC;
	}
	else {
	  this._state = ProcState.DISABLED;
	}
	break;
      case ProcState.FINISHED:
      case ProcState.EXCEPTION:
      case ProcState.ABORTED:
      case ProcState.ABORTED_REC:
      case ProcState.SUSPENDED:
      case ProcState.SUSPENDED_REC:
      case ProcState.DISABLED:
      case ProcState.DISABLED_REC:
      case ProcState.TERMINATED:
	assert(false, "Cannot disable a process in state " ~ _state);
      case ProcState.RUNNING:
	assert(false, "Impossible Thread State");
      }
    }
  }

  private final bool requestResume() {
    final switch(_state) {
    case ProcState.STARTING:
    case ProcState.WAITING:
    case ProcState.FINISHED:
    case ProcState.EXCEPTION:
    case ProcState.DISABLED:
    case ProcState.DISABLED_REC:
    case ProcState.RUNNING:
      assert(false, "Only suspended processes can be resumed");
    case ProcState.ABORTED:
    case ProcState.ABORTED_REC:
    case ProcState.TERMINATED:
      return false;
    case ProcState.SUSPENDED:
    case ProcState.SUSPENDED_REC:
      if(_origState == ProcState.RUNNING) {
	_state = ProcState.WAITING;
	return true;
      } else {
	_state = _origState;
	return false;
      }
    }
  }

  private final void requestEnable() {
    final switch(_state) {
    case ProcState.STARTING:
    case ProcState.WAITING:
    case ProcState.FINISHED:
    case ProcState.EXCEPTION:
    case ProcState.SUSPENDED:
    case ProcState.SUSPENDED_REC:
    case ProcState.RUNNING:
      assert(false, "Only disabled processes can be enabled");
    case ProcState.ABORTED:
    case ProcState.ABORTED_REC:
    case ProcState.TERMINATED:
      return;
    case ProcState.DISABLED:
    case ProcState.DISABLED_REC:
      // if(_origState == ProcState.RUNNING) {
      //	_state = ProcState.WAITING;
      //	return true;
      // } else {
      _state = _origState;
      return;
      // }
    }
  }

  private final bool requestAbort(bool REC) {
    synchronized(this) {
      final switch(_state) {
      case ProcState.WAITING:
      case ProcState.SUSPENDED:
      case ProcState.SUSPENDED_REC:
      case ProcState.DISABLED:
      case ProcState.DISABLED_REC:
	if(REC) {
	  _state = ProcState.ABORTED_REC;
	}
	else {
	  _state = ProcState.ABORTED;
	}
	return true;
      case ProcState.STARTING:
	if(REC) {
	  _state = ProcState.ABORTED_REC;
	}
	else {
	  _state = ProcState.ABORTED;
	}
	return false;
      case ProcState.FINISHED:
      case ProcState.EXCEPTION:
      case ProcState.ABORTED:
      case ProcState.ABORTED_REC:
	return false;
      case ProcState.RUNNING:
      case ProcState.TERMINATED:
	assert(false, "Impossible Thread State");
      }
    }
  }

  private final bool requestTerminate() {
    synchronized(this) {
      final switch(_state) {
      case ProcState.WAITING:
      case ProcState.SUSPENDED:
      case ProcState.SUSPENDED_REC:
      case ProcState.DISABLED:
      case ProcState.DISABLED_REC:
	_state = ProcState.TERMINATED;
	return true;
      case ProcState.STARTING:
	_state = ProcState.TERMINATED;
	return false;
      case ProcState.FINISHED:
      case ProcState.EXCEPTION:
      case ProcState.ABORTED:
      case ProcState.ABORTED_REC:
	return false;
      case ProcState.TERMINATED:
      case ProcState.RUNNING:
	assert(false, "Impossible Thread State");
      }
    }
  }

  public final bool isRunnable() {
    synchronized(this) {
      return (_state != ProcState.FINISHED &&
	      _state != ProcState.TERMINATED &&
	      _state != ProcState.ABORTED &&
	      _state != ProcState.ABORTED_REC &&
	      _state != ProcState.EXCEPTION);
    }
  }

  public final bool isTerminated() {
    synchronized(this) {
      return(_state == ProcState.FINISHED ||
	     _state == ProcState.ABORTED ||
	     _state == ProcState.ABORTED_REC ||
	     _state == ProcState.EXCEPTION ||
	     _state == ProcState.TERMINATED);
    }
  }

  class TermException: Throwable
  {
    this() {
      super("Process Terminated");
    }
  }

}

// This flag (whenever raised) get registered with the simulator. If a
// flag is raised, the simulator shall not automatically die even if
// it has no pending events or runnable threads. Note that this is
// useful since in such a scenario, the user is expected to increase
// the simulation time.

// A flag can be raised or dropped. When raised for the first time,
// the flag will register itself with the simulator. When dropped, the
// flag state will indicate that the flag has been dropped. The
// simulator, when it does not have any pending events or runnable
// threads, will look for all the registered flags. It will
// remove/unregister the flags that have been dropped and if no flag
// is active, the simulation will come to an end. On the other hand if
// there are one or more raised flags, the simulator will pause and
// wait for an external actor to activate the simulation again.


class Process: SimProcess, EventClient, Procedure
{
  __gshared size_t _procCount;
  private size_t _procID;
  public final size_t procID() {
    return _procID;
  }

  private int _stage=0;
  public final int stage() {
    synchronized(this) {
      return _stage;
    }
  }

  private EventObj _sensitiveTo = null;
  private bool _dynamic = true;
  private Process[] _esdl__childProcs;

  // When waiting for an event, this variable will have the event
  // object
  // private EventObj _waitingFor = null;

  // Return true if the event is dynamically spawned
  public final bool isDynamic() {
    synchronized(this) {
      return _dynamic;
    }
  }

  // Add a newly launched process to the list
  protected final void addProcess(Process t) {
    synchronized(this) {
      _esdl__childProcs ~= t;
      debug(FORK) {
	import std.stdio;
	writeln("Adding Fork to active forks list: ",
		&t, "/", _esdl__childProcs.length);
      }
    }
  }

  // remove process from the list of child processes
  protected final void removeProcess(Process t) {
    // trigger the _endedRec event
    t._endedRec.notify();
    ptrdiff_t i = -1;
    foreach(j, f; _esdl__childProcs) {
      if(f is t) {
	i = j;
	break;
      }
    }
    debug(FORK) {
      import std.stdio;
      writeln("Removing fork from active forks list: ",
	      &t, "/", _esdl__childProcs.length, "/", t.getFullName());
    }
    if(i is -1) {
      assert(false, "removeProcess: Fork not found on the list ");
    }
    else {
      for(size_t j=i; j != _esdl__childProcs.length - 1; ++j) {
	_esdl__childProcs[j] = _esdl__childProcs[j+1];
      }
      _esdl__childProcs.length -= 1;
    }
    if(this._dynamic && this._esdl__childProcs.length is 0 &&
       this._isZombieProc) {
      auto parent = cast(Procedure) getParent();
      parent.removeProcess(this);
    }
  }

  bool _registered = false;
  private final bool registered() {
    synchronized(this) {
      return _registered;
    }
  }
  private final void register() {
    synchronized(this) {
      _registered = true;
    }
  }

  private final void cleanup() {
    if(state is ProcState.RUNNING) {
      state = ProcState.FINISHED;
    }
    if(this.state is ProcState.ABORTED_REC ||
       this.state is ProcState.ABORTED) {
      this.getSimulator.freeLock(this, true);
    }
    else {
      this.getSimulator.freeLock(this, false);
    }
  }

  // prepare to execute
  // called in the sched phase -- no guards
  // preExecute phase is used to filter out the processes that
  // actually need to be run. This is done because execute is
  // completed using a barrier and the barrier count has to be
  // available before we start.
  protected final bool preExecute() {
    final switch(_state) {
    case ProcState.STARTING:
    case ProcState.WAITING:
      _state = ProcState.RUNNING;
      return true;
    case ProcState.RUNNING:
      assert(false, this.getFullName ~
	     " Unexpected Process State -- RUNNING");
    case ProcState.FINISHED:
    case ProcState.ABORTED:
    case ProcState.ABORTED_REC:
    case ProcState.EXCEPTION:
    case ProcState.TERMINATED:
      return false;
    case ProcState.SUSPENDED:
    case ProcState.SUSPENDED_REC:
      // _origState may have been STARTING or WAITING
      // extra bool variable _hasStarted is used to decide that
      _origState = ProcState.RUNNING;
      return false;
    case ProcState.DISABLED:
    case ProcState.DISABLED_REC:
      return false;
    }
  }

  protected final void execute() {
    final switch(_state) {
    case ProcState.RUNNING:
      if(_hasStarted) {
	_waitLock.notify();
      }
      else {
	super.start();
      }
      break;

      // The rest of the case switches would have been taken care of
      // by preExecute
    case ProcState.FINISHED:
    case ProcState.ABORTED:
    case ProcState.ABORTED_REC:
    case ProcState.EXCEPTION:
    case ProcState.TERMINATED:
    case ProcState.DISABLED:
    case ProcState.DISABLED_REC:
    case ProcState.SUSPENDED:
    case ProcState.SUSPENDED_REC:
    case ProcState.STARTING:
    case ProcState.WAITING:
      assert(false, "Unexpected Process State: " ~ _state);
    }
  }
  // make sure that a user can not directly "start" the underlying
  // thread -- somehow it seems @disable does not work alone, the
  // private tag seems to make the effect though
  @disable private final void start();

  final protected void abortProcess() {
    debug {
      import std.stdio: writeln;
      writeln("Aborting thread in state:", state);
    }
    final switch(state) {
    case ProcState.STARTING:
      state = ProcState.ABORTED;
      this.getSimulator.freeLock(this, true);
      break;
    case ProcState.WAITING:
    case ProcState.SUSPENDED:
    case ProcState.SUSPENDED_REC:
    case ProcState.DISABLED:
    case ProcState.DISABLED_REC:
      state = ProcState.ABORTED;
      _waitLock.notify();
      break;
    case ProcState.RUNNING:
      assert(false, "Unexpected thread state!");
      // break;
    case ProcState.FINISHED:
    case ProcState.EXCEPTION:
    case ProcState.ABORTED:
    case ProcState.ABORTED_REC:
    case ProcState.TERMINATED:
      break;
    }
  }

  final protected void terminateProcess() {
    debug {
      import std.stdio: writeln;
      writeln("Terminating thread in state:", state);
    }
    final switch(state) {
    case ProcState.STARTING:
      state = ProcState.TERMINATED;
      this.getSimulator.freeLock(this);
      break;
    case ProcState.WAITING:
    case ProcState.SUSPENDED:
    case ProcState.SUSPENDED_REC:
    case ProcState.DISABLED:
    case ProcState.DISABLED_REC:
      state = ProcState.TERMINATED;
      _waitLock.notify();
      break;
    case ProcState.RUNNING:
      assert(false, "Unexpected thread state!");
      // break;
    case ProcState.FINISHED:
    case ProcState.EXCEPTION:
    case ProcState.ABORTED:
    case ProcState.ABORTED_REC:
    case ProcState.TERMINATED:
      break;
    }
  }


  public final void dontInitialize() {
    synchronized(this) {
      if(_state !is ProcState.STARTING) {
	assert(false, "A process can be tagged dontInitialize "
	       "only before it has started running");
      }
      _dontInit = true;
    }
  }

  // Called only in schedule phase -- no need for syn guards
  public final bool isDontInitialize() {
    return _dontInit;
  }


  public final void sensitiveTo(EventObj event) {
    synchronized(this) {
      _sensitiveTo = event;
    }
  }

  public final void sensitiveTo(SimEvent simEvent) {
    auto event = new EventObj(simEvent);
    sensitiveTo(event);
  }

  public final EventObj sensitiveTo() {
    synchronized(this) {
      return _sensitiveTo;
    }
  }

  protected final void poke(EsdlSimulator sim, EventObj agent, size_t index) {
    assert(false, "Cannot poke a Process: " ~ this.getFullName);
  }

  protected final void poke(EsdlSimulator sim) {
    assert(false, "Cannot poke a Process: " ~ this.getFullName);
  }

  public final bool notify() {
    assert(false, "Cannot notify a Process: " ~ this.getFullName);
  }

  public final bool notify(SimTime steps) {
    assert(false, "Cannot notify a Process: " ~ this.getFullName);
  }

  protected final void addAgent(EventObj agent) {
    assert(false, "Process not dependent on any event: "
	   ~ this.getFullName() ~ " -- addAgent");
  }

  public final uint[] taskIndices() {
    // import std.stdio: writeln;
    // writeln("Adding indices: ", indices);
    return _esdl__indices;
  }

  private final void _esdl__fixParent() {
    synchronized(this) {
      if(Process.self || Routine.self) { // only dynamic procedures
	Procedure _parent = Procedure.self;
	this._esdl__setParent(_parent);
	this._esdl__setRoot(_parent.getRoot());
	// if(_parent._esdl__getLockMutex is null) {
	//   // For the time being all the dynamic tasks would run one-at-a-time
	//   this._esdl__parLock = new Semaphore(1);
	// }
	// else {
	this._esdl__parLock = _parent._esdl__getLockMutex;
	// }
      }
    }
  }

  mixin(hierMixin());

  public final void _esdl__addChildObj(NamedObj child) {
    assert(false, "A task can have only processes as childObjs");
  }

  Random _randGen;

  protected final override ref Random randGen() {
    synchronized(this) {
      return _randGen;
    }
  }

  // For all the timed-waits during the execution of the process, use
  // this event
  // Effectively Immutable
  private EventObj _timed;

  // Triggered when a process ends execution
  private Event _ended;

  // Trigger this when the process as well as all the child processes
  // have ended.
  private Event _endedRec;

  // A process is marked zombie if it has completed, but can not be
  // removed from the parent process' active list since it have
  // childObjs which have not completed yet.
  private bool _isZombieProc = false;
  private final bool isZombieProc() {
    synchronized(this) {
      return _isZombieProc;
    }
  }
  private final void markZombie() {
    synchronized(this) {
      _isZombieProc = true;
    }
  }
  private static Process _self;
  public final static Process self() {
    return _self;
  }

  // events are effectively immutable
  public final void wait() {
    this._ended.wait();
  }

  public final void waitRec() {
    this._endedRec.wait();
  }

  public final EventObj getEvent() {
    return _ended;
  }

  public final EventObj getRecEvent() {
    return _endedRec;
  }

  this(void function() fn, int stage = 0, size_t sz = 0 ) {
    synchronized(typeid(Process)) {
      _procID = _procCount++;
    }
    synchronized(this) {
      _ended.init(this);
      _endedRec.init(this);
      _stage = stage;

      _timed = new EventObj(this);

      super(() {fn_wrap(fn);}, sz);
      this.setRandomSeed();
      if(Process.self || Routine.self) {
	this._esdl__fixParent();
	this.reqRegisterProcess(stage);
      }
    }
  }

  this(void delegate() dg, int stage = 0, size_t sz = 0 ) {
    synchronized (typeid(Process)) {
      _procID = _procCount++;
    }
    synchronized(this) {
      _ended.init(this);
      _endedRec.init(this);
      _stage = stage;

      _timed = new EventObj(this);

      super(() {dg_wrap(dg);}, sz);
      this.setRandomSeed();
      if(Process.self || Routine.self) {
	this._esdl__fixParent();
	this.reqRegisterProcess(stage);
      }
    }
  }

  private final void fn_wrap(void function() fn) {
    try {
      Process._self = this;
      scope(exit) {
	// drop persistence
	dropPersistFlag();

	if(_esdl__getChildProcs().length is 0) {
	  // If there are no active child processes, remove the process
	  getSimulator().reqPurgeProc(this);
	}
	else {
	  // mark the process as zombie
	  this.markZombie();
	}
	_ended.notify();
      }
      getRoot().initProcess();
      fn();
    }
    catch(TermException e) {
      // Process got terminated at end of simulation
      debug(THREAD) {
	import std.stdio: writeln;
	writeln(this.getFullName, " Thread terminated");
      }
    }
    catch(Throwable e) {
      import std.stdio: writeln;
      writeln("Thread threw exception: ", e);
      // writeln(e);
      debug(PROC) {
	import std.stdio;
	writeln("Process ending with exception   : ", Process.procID);
      }
      this.getSimulator.freeLock(this);
      this.caughtException();
      throw(e);
    }
    // cleanup is defined in TaskProcess
    this.cleanup();
  }

  private final void dg_wrap(void delegate() dg) {
    try {
      Process._self = this;
      scope(exit) {
	// drop persistence
	dropPersistFlag();

	if(_esdl__getChildProcs().length is 0) {
	  // If there are no active child processes, remove the process
	  getSimulator().reqPurgeProc(this);
	}
	else {
	  // mark the process as zombie
	  this.markZombie();
	}
	_ended.notify();
      }
      getRoot().initProcess();
      dg();
    }
    catch(TermException e) {
      // Process got terminated at end of simulation
      debug(THREAD) {
	import std.stdio: writeln;
	writeln(this.getFullName, " Thread terminated");
      }
    }
    catch(Throwable e) {
      import std.stdio: writeln;
      writeln("Thread threw exception: ", e);
      // writeln(e);
      debug(PROC) {
	import std.stdio;
	writeln("Process ending with exception   : ", Process.procID);
      }
      this.getSimulator.freeLock(this);
      this.caughtException();
      throw(e);
    }
    // cleanup is defined in TaskProcess
    this.cleanup();
  }

  public final override void waitSensitive() {
    EventObj event = this.sensitiveTo();
    if(event is null) {
      assert(false,
	     "wait() called when no event sensitivity is specified");
    }
    this.waitSensitive(event);
  }

  public final override void waitSensitiveP() {
    EventObj event = this.sensitiveTo();
    if(event is null) {
      assert(false,
	     "wait() called when no event sensitivity is specified");
    }
    this.waitSensitiveP(event);
  }

  public final override void waitSensitiveP(EventObj event) {
    if(! event.triggered) {
      waitSensitive(event);
    }
  }

  public final override void waitSensitive(EventObj event) {
    event.addClientProcess(this);
    state = ProcState.WAITING;
    this.getSimulator.freeLock(this);
    _waitLock.wait();
    if(this.state == ProcState.ABORTED ||
       this.state == ProcState.ABORTED_REC ||
       this.state == ProcState.TERMINATED) {
      throw new TermException();
    }
  }

  private final void setRandomSeed() {
    synchronized(this) {
      uint seed = urandom();
      this._randGen.seed(seed);
    }
  }

  private final void reqRegisterProcess(int stage) {
    this.getSimulator.reqRegisterProcess(this, stage);
    auto parent = Procedure.self;
    parent.addProcess(this);
  }

  Fork _fork = null;

  private final void setFork(Fork fork) {
    synchronized(this) {
      _fork = fork;
    }
  }

  public final Fork thisFork() {
    synchronized(this) {
      return _fork;
    }
  }

  private final void terminateWaiting() {
    synchronized(this) {
      _waitLock.notify();
    }
  }

  // this is looked at the time thread enters waiting state
  private ProcStateReq _reqState; // requested state

  protected final override ProcStateReq reqState() {
    synchronized(this) {
      debug(PROC) {
	import std.stdio;
	writeln(this.procID, " Thread has request for ", _reqState);
      }
      return _reqState;
    }
  }

  private final void reqState(ProcStateReq s) {
    synchronized(this) {
      debug(PROC) {
	import std.stdio;
	writeln(this.procID, " Requesting Thread to ", s);
      }
      _reqState = s;
    }
    getSimulator().reqUpdateProc(this);
  }

  public final ProcState status() {
    return this.state();
  }

  public final void suspend() {
    synchronized(this) {
      this.reqState(ProcStateReq.SUSPEND);
    }
  }

  public final void suspendRec() {
    synchronized(this) {
      this.reqState(ProcStateReq.SUSPEND_REC);
    }
  }

  public final void disable() {
    synchronized(this) {
      this.reqState(ProcStateReq.DISABLE);
    }
  }

  public final void disableRec() {
    synchronized(this) {
      this.reqState(ProcStateReq.DISABLE_REC);
    }
  }


  public final void resume() {
    synchronized(this) {
      this.reqState(ProcStateReq.RESUME);
    }
  }

  public final void enable() {
    synchronized(this) {
      this.reqState(ProcStateReq.ENABLE);
    }
  }

  public final void abortRec() {
    synchronized(this) {
      this.reqState(ProcStateReq.ABORT_REC);
    }
  }

  public final void abort() {
    synchronized(this) {
      this.reqState(ProcStateReq.ABORT);
    }
  }

  public final override void nextTrigger(EventObj event) {
    assert(false, "Can not call nextTrigger from a Process");
  }

  public final override EventObj nextTrigger() {
    assert(false, "Can not call nextTrigger from a Process");
  }

  public final override Process[] getChildTasks() {
    return [];
  }

  public final override HierComp[] getChildComps() {
    return [];
  }

  public final override NamedObj[] getChildObjs() {
    return [];
  }

  protected final override Process[] _esdl__getChildProcs() {
    // Though the scheduler does modify _esdl__childProcs as and
    // when processes get spawned or die out, the variable can
    // be treated as effectively immutable since the scheduler
    // works with single thread
    return _esdl__childProcs; // this._esdl__childProcs;
  }

  protected final override Process[] _esdl__getChildProcsHier() {
    Process[] children = _esdl__getChildProcs();
    foreach(child; _esdl__getChildProcs()) {
      children ~= child._esdl__getChildProcsHier();
    }
    return children;
  }

  public final override void _esdl__addChildTask(Process child) {}
  public final override void _esdl__addChildComp(HierComp child) {}
}

// A specialized class for single process forks -- this class exists
// to make it convenient to use function 'fork' to return the process
// being forked -- courtsey alias 'this'
class ForkMono: Fork
{
  private Process _proc;

  private this(Process proc) {
    synchronized(this) {
      _proc = proc;
      Process[] procs;
      procs ~= proc;
      super(procs);
    }
  }
  
  public final Process getProcess() {
    synchronized(this) {
      return _proc;
    }
  }

  alias getProcess this;
}

class Fork
{
  protected Process[] _procs;

  Event _allDone;
  Event _anyDone;

  private this(Process[] tasks) {
    synchronized(this) {
      _procs = tasks;
      AndSimEvent allDone = new AndSimEvent();
      OrSimEvent  anyDone = new OrSimEvent();
      foreach(_proc; _procs) {
	auto event = _proc.getEvent;
	simEventsAdd(allDone, 0, event);
	simEventsAdd(anyDone, 0, event);
	_proc.setFork(this);
      }
      _allDone = allDone;
      _anyDone = anyDone;
    }
  }

  public final Process[] _esdl__getChildProcs() {
    synchronized(this) {
      return _procs;
    }
  }

  public final void joinAll() {
    // wait for all tasks that are not terminated
    EventObj[] events;
    foreach(task; _esdl__getChildProcs()) {
      if(task.isRunnable) {
	events ~= task.getEvent();
      }
    }
    waitAll(events);
  }
  alias joinAll join;

  public final void joinAny() {
    // wait for all tasks that are not terminated
    EventObj[] events;
    foreach(task; _esdl__getChildProcs()) {
      if(task.isRunnable) {
	events ~= task.getEvent();
      }
      if(task.isTerminated) {
	return;
      }
    }
    waitAny(events);
  }

  public final void joinNone() {}

  public final void wait() {
    // wait for all tasks that are not terminated
    EventObj[] events;
    foreach(task; _esdl__getChildProcs()) {
      if(task.isRunnable) {
	events ~= task.getEvent();
      }
    }
    waitAll(events);
  }

  public final void waitRec() {
    // wait for all tasks that are not terminated
    Process[] tasks;
    foreach(task; _esdl__getChildProcs()) {
      tasks ~= task;
      tasks ~= task._esdl__getChildProcsHier();
    }
    EventObj[] events;
    foreach(task; tasks) {
      if(task.isRunnable) {
	events ~= task.getEvent();
      }
    }
    waitAll(events);
  }

  public final void abort() {
    foreach(task; _esdl__getChildProcs()) {
      task.abort();
    }
  }

  public final void abortRec() {
    foreach(task; _esdl__getChildProcs()) {
      task.abortRec();
    }
  }

  public final void suspend() {
    foreach(task; _esdl__getChildProcs()) {
      task.suspend();
    }
  }

  public final void suspendRec() {
    foreach(task; _esdl__getChildProcs()) {
      task.suspendRec();
    }
  }

  public final void resume() {
    foreach(task; _esdl__getChildProcs()) {
      task.resume();
    }
  }

  public final void disable() {
    foreach(task; _esdl__getChildProcs()) {
      task.disable();
    }
  }

  public final void disableRec() {
    foreach(task; _esdl__getChildProcs()) {
      task.disableRec();
    }
  }

  public final void enable() {
    foreach(task; _esdl__getChildProcs()) {
      task.enable();
    }
  }
}

class Routine: EventClient, Procedure
{
  private EventObj _sensitiveTo = null;
  private EventObj _nextTrigger;
  private bool _dynamic = true;
  private EventObj _timed;

  public final bool isDynamic() {
    synchronized(this) {
      return _dynamic;
    }
  }

  protected final void addProcess(Process t) {
    synchronized(this) {
      _esdl__childTasks ~= t;
    }
  }

  // called only in the sched phase
  protected final void removeProcess(Process t) {
    ptrdiff_t i = -1;
    foreach(j, f; _esdl__childTasks) {
      if(f is t) {
	i = j;
	break;
      }
    }
    debug(FORK) {
      import std.stdio;
      writeln("Removing fork from active forks list: ",
	      i, "/", _esdl__childTasks.length);
    }
    if(i is -1) {
      assert(false, "removeProcess: Fork not found on the list ");
    }
    else {
      for(size_t j=i; j != _esdl__childTasks.length - 1; ++j) {
	_esdl__childTasks[j] = _esdl__childTasks[j+1];
      }
      _esdl__childTasks.length -= 1;
    }
  }

  ProcState _state = ProcState.STARTING;
  bool _dontInit      = false;

  public final void dontInitialize() {
    synchronized(this) {
      import std.exception;
      enforce(_state == ProcState.STARTING,
	      "A process can be tagged dontInitialize "
	      "only before it has started running");
      _dontInit = true;
    }
  }

  // Called only in schedule phase -- no need for syn guards
  public final bool isDontInitialize() {
    return _dontInit;
  }


  public final void sensitiveTo(EventObj event) {
    synchronized(this) {
      _sensitiveTo = event;
    }
  }

  public final void sensitiveTo(SimEvent simEvent) {
    auto event = new EventObj(simEvent);
    sensitiveTo(event);
  }

  public final EventObj sensitiveTo() {
    synchronized(this) {
      return _sensitiveTo;
    }
  }

  protected final void poke(EsdlSimulator sim, EventObj agent, size_t index) {}

  protected final void poke(EsdlSimulator sim) {}

  public final bool notify() {
    assert(false, "Cannot notify an Routine: " ~ this.getFullName);
  }

  public final bool notify(SimTime steps) {
    assert(false, "Cannot notify an Routine: " ~ this.getFullName);
  }


  protected final void addAgent(EventObj agent) {
    assert(false, "Routine not dependent on any event: "
	   ~ this.getFullName() ~ " -- addAgent");
  }

  public final EventObj nextTrigger() {
    synchronized(this) {
      auto event = _nextTrigger;
      _nextTrigger = null;
      return event;
    }
  }

  public final void nextTrigger(EventObj event) {
    synchronized(this) {
      _nextTrigger = event;
    }
  }

  // interface HierComp

  mixin(hierMixin());

  public final uint[] routineIndices() {
    // import std.stdio: writeln;
    // writeln("Adding indices: ", indices);
    return _esdl__indices;
  }

  private final void _esdl__fixParent() {
    synchronized(this) {
      if(Process.self || Routine.self) {
	Procedure _parent = Procedure.self;
	this._esdl__setParent(_parent);
	this._esdl__setRoot(_parent.getRoot());
	// if(_parent._esdl__getLockMutex is null) {
	//   import core.sync.semaphore: Semaphore;
	//   // For the time being all the dynamic tasks would run one-at-a-time
	//   this._esdl__parLock = new Semaphore(1);
	// }
	// else {
	this._esdl__parLock = _parent._esdl__getLockMutex;
	// }
      }
    }
  }

  Random _randGen;

   protected final override ref Random randGen() {
    synchronized(this) {
      return _randGen;
    }
  }

  private void delegate() _dg = null;
  private static Routine _self;
  public static Routine self() {
    return _self;
  }

  this( void function() fn ) {
    synchronized(this) {
      _timed = new EventObj(this);
      _dg =() {fn_wrap(fn);};
    }
    this.setRandomSeed();
    if(Process.self || Routine.self) {
      this._esdl__fixParent();
      this.reqRegisterRoutine();
    }
  }

  this( void delegate() dg ) {
    synchronized(this) {
      _timed = new EventObj(this);
      _dg =() {dg_wrap(dg);};
    }
    this.setRandomSeed();
    if(Process.self || Routine.self) {
      this._esdl__fixParent();
      this.reqRegisterRoutine();
    }
  }

  private final void fn_wrap(void function() fn) {
    try {
      fn();

      // set the next trigger
      EventObj trigger = this.nextTrigger();
      if(trigger is null) trigger = this.sensitiveTo();
      if(trigger !is null) {
	trigger.addClientRoutine(this);
      }
    }
    catch(Throwable e) {
      import std.stdio: writeln;
      writeln("Thread threw exception: ", e);
    }
  }

  private final void dg_wrap(void delegate() dg) {
    try {
      dg();

      // set the next trigger
      EventObj trigger = this.nextTrigger();
      if(trigger is null) trigger = this.sensitiveTo();
      if(trigger !is null) {
	trigger.addClientRoutine(this);
      }
    }
    catch(Throwable e) {
      import std.stdio: writeln;
      writeln("Thread threw exception: ", e);
    }
  }

  private final void setRandomSeed() {
    synchronized(this) {
      uint seed = urandom();
      this._randGen.seed(seed);
    }
  }

  private final void reqRegisterRoutine() {
    import std.exception: enforce;
    enforce(this.getSimulator !is null);
    this.getSimulator.reqRegisterRoutine(this);
  }

  static void _esdl__elab(size_t I, T, L)
    (T t, ref L l, uint[] indices=null) {
    debug(ELABORATE) {
      import std.stdio;
      writeln("** Routine: Elaborating " ~ t.tupleof[I].stringof ~ ":" ~
	      typeof(l).stringof);
    }
    l._esdl__inst!I(t, l);
    synchronized(l) {
      import core.sync.semaphore: Semaphore;
      static if(is(T unused: ElabContext)) {
	t._esdl__addChildObj(l);
	t._esdl__addChildComp(l);
      }
      l._dynamic = false;
      l._esdl__setIndices(indices);
      l._esdl__nomenclate!I(t, indices);
      l._esdl__setRoot(t.getRoot);
      l._esdl__setParent(t);
      t._esdl__register(l);
      auto linfo = _esdl__get_parallelism!I(t, l);
      l._esdl__parLock = t._esdl__getLockMutex;
    }
  }
}

interface ChannelIF
{
  // update would be called only during the schedule phase
  public void update();
  // requestUpdate would be called during the eval phase
  protected void requestUpdate(UpdateReason reason=UpdateReason.VLANG);
}

enum UpdateReason: byte
  {   NONE,
      VLANG,
      VERILOG,
      VHDL,
      SYSTEMC
  }

class Channel: ChannelIF, NamedObj // Primitive Channel
{
  UpdateReason _updateReason = UpdateReason.NONE;
  
  this(string name="", EntityIntf parent=null) {
    synchronized(this) {
      // _esdl__getParentProc returns null during elaboration
      this._esdl__parent = _esdl__getParentProc();
    }
  }

  mixin(namedMixin());

  public void update() {
    assert(false, "method 'update' undefined for " ~
	   this.getFullName);
  }

  // no locking required since this operation is triggered by the scheduler
  protected final void updateDone() {
    _updateReason = UpdateReason.NONE;
  }

  protected final void requestUpdate(UpdateReason reason=UpdateReason.VLANG) {
    synchronized(this) {
      // The channel should get registered with the scheduler for
      // update only if it has not been registered yet
      if(_updateReason is UpdateReason.NONE) {
	this.getSimulator.requestUpdate(this);
      }
      _updateReason = reason;
    }
  }

}

class RootProcess: SimThread, Procedure
{
  private static RootProcess _self;
  public static RootProcess self() {return _self;}

  private final void fn_wrap(void function() fn) {
    _self = this;
    fn();
  }

  private final void dg_wrap(void delegate() dg) {
    _self = this;
    dg();
  }

  this( void function() fn, size_t sz = 0 ) {
    super(() {fn_wrap(fn);}, sz);
  }

  this( void delegate() dg, size_t sz = 0 ) {
    super(() {dg_wrap(dg);}, sz);
  }

  public final override void waitSensitiveP(EventObj event) {
    assert(false, "Can not call wait from RootProcess");
  }
  public final override void waitSensitive(EventObj event) {
    assert(false, "Can not call wait from RootProcess");
  }
  public final override void waitSensitive() {
    assert(false, "Can not call wait from RootProcess");
  }
  public final override void waitSensitiveP() {
    assert(false, "Can not call wait from RootProcess");
  }
  public final override void nextTrigger(EventObj event) {
    assert(false, "Can not call nextTrigger from RootProcess");
  }
  public final override EventObj nextTrigger() {
    assert(false, "Can not call nextTrigger from RootProcess");
  }

  @_esdl__ignore Random _randGen;

  protected final override ref Random randGen() {
    synchronized(this) {
      return _randGen;
    }
  }

  public final bool isDynamic() {
    return false;
  }

  protected final void addProcess(Process t) {}

  protected final void removeProcess(Process t) {}

  public final Process[] getChildTasks() {
    return [];
  }

  // interface HierComp
  static void _esdl__elab(size_t I, T, L)
    (T t, ref L l, uint[] indices=null) {
    debug(ELABORATE) {
      import std.stdio;
      writeln("** RootProcess: Elaborating " ~ t.tupleof[I].stringof ~ ":" ~
	      typeof(l).stringof);
    }
    synchronized(l) {
      static if(is(T unused: ElabContext)) {
	t._esdl__addChildObj(l);
	t._esdl__addChildComp(l);
      }
      l._esdl__nomenclate!I(t, indices);
      l._esdl__setParent(t);
      _esdl__elabMems(l);
    }
  }

  mixin(hierMixin());
}


private enum ProcState: byte
  {   STARTING,			// yet to start
      RUNNING,			// runnning
      WAITING,			// waiting for an event
      SUSPENDED,		// user suspended
      SUSPENDED_REC,		// user suspended recursively
      DISABLED,			// user disabled
      DISABLED_REC,		// user disabled resursively
      ABORTED,			// user aborted
      ABORTED_REC,		// user aborted recursively
      FINISHED,			// naturally ended run
      EXCEPTION,		// thread faced an exception
      TERMINATED,		// end of simulation
      }

private enum ProcStateReq: byte
  {   SUSPEND,
      SUSPEND_REC,
      DISABLE,
      DISABLE_REC,
      ABORT,
      ABORT_REC,
      TERMINATE, // At the end of simulation, terminate recursively
      ENABLE,
      RESUME,
      NONE,
      }


class RoutineThread: SimProcess
{

  static if(!__traits(compiles, _esdl__root)) {
    @_esdl__ignore protected RootEntityIntf _esdl__root;
  }


  this(EsdlSimulator sim, size_t sz = 0 ) {
    synchronized(this) {
      this._esdl__root = sim.getRoot();
      super(&execRoutineProcess, sz);
    }
  }

  private final void execRoutineProcess() {
    Routine routine = null;
    _esdl__root.simulator()._executor._routineThreadStartBarrier.wait();
    while(true) {
      // wait for next cycle
      this._waitLock.wait();
      if(this._isTerminated()) {
	_esdl__root.simulator()._executor._routineThreadBarrier.wait();
	break;
      }

      routine = this._esdl__root.simulator()._executor.nextRoutine();
      while(routine !is null) {
	Routine._self = routine;
	_esdl__root.initRoutine();
	routine._dg();
	routine = this._esdl__root.simulator()._executor.nextRoutine();
      }
      _esdl__root.simulator()._executor._routineThreadBarrier.wait();

    }
  }

  private bool _terminate = false;

  private final bool _isTerminated() {
    synchronized(this) {
      return _terminate;
    }
  }

  private final void terminate() {
    synchronized(this) {
      _terminate = true;
    }
  }

  // make sure that a user can not directly "start" the underlying
  // thread -- somehow it seems @disable does not work alone, the
  // private tag seems to make the effect though
  @disable private final void start();
  private final initialize() {
    super.start();
  }

  public final override EventObj nextTrigger() {
    synchronized(this) {
      return Routine.self.nextTrigger();
    }
  }

  public final override void nextTrigger(EventObj event) {
    synchronized(this) {
      Routine.self.nextTrigger(event);
    }
  }

  public final override void waitSensitiveP(EventObj event) {
    assert(false, "Can not call wait from a Routine");
  }
  public final override void waitSensitive(EventObj event) {
    assert(false, "Can not call wait from a Routine");
  }
  public final override void waitSensitive() {
    assert(false, "Can not call wait from a Routine");
  }
  public final override void waitSensitiveP() {
    assert(false, "Can not call wait from a Routine");
  }

  public final override RootEntityIntf getRoot() {
    assert(false, "getRoot not available in RoutineThread");
  }
}

enum SimPhase : byte
  {   NONE = 0,
      BUILD,
      ELABORATE,
      CONFIGURE,
      BINDEXEPORTS,
      BINDPORTS,
      SIMULATE,
      PAUSE,
      TERMINATED
      }

void simulateAllRoots(T)(T t)
  if(is(T == Time) || is(T == SimTime)) {
    RootEntityIntf.simulateAll(t);
  }

void terminateAllRoots() {
  RootEntityIntf.terminateAll();
}

void forkElab(T)(T t)
{
  t.getSimulator.elabRootThread(t);
}

void elaborate(T)(T t)
{
  t.forkElab();
  t.joinElab();
}

void doElab(T)(T t)
  if(is(T unused: RootEntityIntf))
    {
      synchronized(t) {
	import std.stdio: writeln;
	import std.exception: enforce;

	// So that getRootEntity returns a legal value even during elaboration
	_esdl__rootEntity = t;
	
	// The BUILD Phase
	// Instantiated modules and events are identified and constructed
	// (using an explicit call to new operator) if these are not already
	// instantiated

	// The ELABORATE Phase
	// All the instantiated modules/events get automatically named.
	// Information regarding the parent/childObjs
	// modules/events is also added as part of t phase
	// At some stage we would like to include Tasks too.
	writeln(">>>>>>>>>> Starting Phase: ELABORATE");
	t.getSimulator.setPhase = SimPhase.ELABORATE;
	t.doBuild();
	t._esdl__postBuild();
	// _esdl__elabIterSuper(t);
	_esdl__elabMems(t);
	t._esdl__postElab();

	// Each module is allowed to override the config() method
	// which is declared in the Entity class. The config methods
	// if defined are read in during the call to the respective
	// constructors of the modules. In the CONFIGURE phase, all t
	// information read in during the configurarion is consolidated and
	// reflected at the EsdlSimulator level.
	writeln(">>>>>>>>>> starting Phase: CONFIGURE");
	t.getSimulator.setPhase = SimPhase.CONFIGURE;
	_esdl__config(t);

	// Precision is now set call _esdl__config again to fix timeScale
	getRootEntity.timePrecisionSet = true;
	_esdl__config(t);
	// Propagate the timeScale to the hierarchy, whereever required
	t.fixTimeParameters();
	_esdl__postCfg(t);

	// Each module is allowed to override the doConnect() method
	// which is declared in the Entity class. The doConnect methods
	// if defined are read in during the call to the respective
	// constructors of the modules. In the CONFIGURE phase, all t
	// information read in during the configurarion is consolidated and
	// reflected at the EsdlSimulator level.
	writeln(">>>>>>>>>> Starting Phase: BIND");
	t.getSimulator.setPhase = SimPhase.BINDEXEPORTS;
	// _esdl__connect!0(t);
	_esdl__connect(t);
	enforce(t._esdl__noUnboundExePorts, "Error: There are unbound exeports");
	t.getSimulator.setPhase = SimPhase.BINDPORTS;
	// _esdl__connect!0(t);
	_esdl__connect(t);
	enforce(t._esdl__noUnboundPorts, "Error: There are unbound ports");

	// In t Phase the EsdlSimulator invokes the endElab() method(if defined
	// by the user) for all the modules.
	// This phase may be used for creating Tasks and Scheduling Timed
	// Events
	writeln(">>>>>>>>>> Calling \"endElab\" for all module instances");
	// _esdl__done!0(t);
	_esdl__endElab(t);

	t.getSimulator._executor.initRoutineThreads();
	t.getSimulator._executor._routineThreadStartBarrier.wait();

	t.getSimulator.setPhase = SimPhase.PAUSE;
	t.getSimulator._executor.resetStage();
	t.getSimulator.elabDoneLock.notify();
	version(MULTICORE) {
	  writeln(">>>>>>>>>> Start of Simulation (multicore enabled)");
	}
	else {
	  writeln(">>>>>>>>>> Start of Simulation");
	}
      }
    }

interface EsdlExecutorIf
{
  public void addRunnableProcess(Process task);
  public void addRunnableRoutine(Routine routine);
  public void reqRegisterProcess(Process task, int reqStage=0);
  public void reqUpdateProcess(Process task);
  public void reqPurgeProcess(Process task);
  public void reqRegisterRoutine(Routine routine, int reqStage=0);
  // public ref Process[] getRunnableProcs();
  public size_t runnableProcsCount();
  public size_t runnableThreadsCount();
  public size_t runnableRoutinesCount();
  public void processRegisteredProcs();
}

class EsdlExecutor: EsdlExecutorIf
{
  private EsdlSimulator _simulator;
  // class ProcessMonitor {}
  import core.sync.semaphore: Semaphore;
  import core.sync.barrier: Barrier;
  private Semaphore _procSemaphore;
  private Barrier _procBarrier;
  private Barrier _routineThreadBarrier;
  private Barrier _routineThreadStartBarrier;
  // private size_t _numThreads;
  // private ProcessMonitor _monitor;
  // private Semaphore _termSemaphore;
  // private Barrier _termBarrier;

  public this(EsdlSimulator simulator) {
    synchronized(this)
      {
	_simulator = simulator;
      }
  }

  private Process[] _runnableProcs;
  private Routine[] _runnableRoutines;
  // Before adding them to _runnableProcs, make a check whether
  // these tasks are dontInit
  private Process[][] _registeredProcs;
  private Process[] _updateProcs;
  private Process[] _purgeProcs;

  // Before adding them to _runnableRoutines, make a check whether
  // these routines are dontInit
  private Routine[][] _registeredRoutines;

  private RoutineThread[] _routineThreads = null;

  private int _minStage = int.max;
  private int _stage;
  private int _stageIndex = -1; // basically _stage - _minStage
  public final int stage() {
    synchronized(this) {
      return _stage;
    }
  }

  private final void resetStage() {
    if(_stageIndex is -1) {
      _stage = _minStage;
      _stageIndex = 0;
    }
  }

  private final bool incrStage() {
    if(_stageIndex == _registeredProcs.length - 1) {
      return false;
    }
    else {
      ++_stage;
      ++_stageIndex;
      return true;
    }
  }

  private final void createRoutineThreads(size_t numThreads,
				    size_t stackSize) {
    _routineThreads.length = numThreads;
    for(size_t i=0; i!=numThreads; ++i) {
      debug {
	import std.stdio;
	writeln("Creating Routine Threads: ", i);
      }
      _routineThreads[i] = new RoutineThread(_simulator, stackSize);
    }
  }

  private final void initRoutineThreads() {
    foreach(rt; _routineThreads) {
      rt.initialize();
    }
  }


  private final Routine nextRoutine() {
    synchronized(this) {
      if(_runnableRoutines.length == 0) return null;
      auto routine = _runnableRoutines[0];
      _runnableRoutines = _runnableRoutines[1..$];
      return routine;
    }
  }

  public final size_t runnableThreadsCount() {
    return(_runnableProcs.length +
	   _runnableRoutines.length);
  }

  public final size_t runnableProcsCount() {
    return _runnableProcs.length;
  }

  public final size_t runnableRoutinesCount() {
    return _runnableRoutines.length;
  }

  // private size_t _processedProcs = 0;
  // private size_t _processedRoutines = 0;

  public final void processRegisteredProcs() {
    foreach(ref task; _registeredProcs[_stageIndex]) {
      synchronized(task) {
	if(task.isDontInitialize()) {
	  EventObj event = task.sensitiveTo();
	  event.addClientProcess(task);
	}
	else {
	  if(task.isRunnable) {
	    this._runnableProcs ~= task;
	  }
	}
      }
    }
    // _processedProcs = _registeredProcs.length;
    _registeredProcs[_stageIndex].length = 0;
    foreach(ref routine; _registeredRoutines[_stageIndex]) {
      synchronized(routine) {
	if(routine.isDontInitialize()) {
	  EventObj event = routine.sensitiveTo();
	  event.addClientRoutine(routine);
	}
	else {
	  this._runnableRoutines ~= routine;
	}
      }
    }
    // _processedRoutines = _registeredRoutines.length;
    _registeredRoutines[_stageIndex].length = 0;
  }

  public final void addRunnableProcess(Process task) {
    if(task.isRunnable) {
      this._runnableProcs ~= task;
    }
  }

  public final void addRunnableRoutine(Routine routine) {
    this._runnableRoutines ~= routine;
  }

  private final void addPhaseIfNeeded(int reqStage) {
    synchronized(this) {
      // if we are not yet running allow to add phase on lower side
      if(_stageIndex is -1) { // run phase has not started yet
	if(_minStage is int.max) {
	  _minStage = reqStage;
	  _registeredRoutines.length = 1;
	  _registeredProcs.length = 1;
	}
  	else if(reqStage < _minStage) {
  	  auto delta = _minStage - reqStage;
  	  _minStage = reqStage;
  	  _registeredProcs.length += delta;
  	  _registeredRoutines.length += delta;
  	  auto len = _registeredProcs.length;
  	  for (size_t i=len; i!=0; --i) {
  	    if(i > delta) {
  	      _registeredProcs[i-1] = _registeredProcs[i-1-delta];
  	      _registeredRoutines[i-1] = _registeredRoutines[i-1-delta];
  	    } else {
  	      _registeredProcs[i-1] = [];
  	      _registeredRoutines[i-1] = [];
  	    }
  	  }
  	}
      }
      else {
  	if(reqStage - _minStage < _stageIndex) {
	  import std.conv: to;
  	  assert(false, "Can not add a process to a phase which"
  		 " is already over: " ~ reqStage.to!string());
  	}
      }
      if(reqStage - _minStage >= _registeredProcs.length) {
  	auto delta = reqStage - _minStage - _registeredProcs.length + 1;
  	_registeredProcs.length += delta;
  	_registeredRoutines.length += delta;
      }
    }
  }

  public final void reqRegisterProcess(Process task, int reqStage=0) {
    synchronized(this) {
      addPhaseIfNeeded(reqStage);
      this._registeredProcs[reqStage-_minStage] ~= task;
    }
  }

  public final void reqRegisterRoutine(Routine routine, int reqStage=0) {
    synchronized(this) {
      addPhaseIfNeeded(reqStage);
      this._registeredRoutines[reqStage-_minStage] ~= routine;
    }
  }

  public final void reqUpdateProcess(Process task) {
    synchronized(this) {
      this._updateProcs ~= task;
    }
  }

  public final void reqPurgeProcess(Process task) {
    synchronized(this) {
      this._purgeProcs ~= task;
    }
  }

  public final void updateProcs() {
    // expand the list by recursion
    Process[] expandedList;
    foreach(proc; _purgeProcs) {
      if(proc.isDynamic()) {
	auto parent = cast(Process) proc.getParent();
	parent.removeProcess(proc);
      }
      else {
	// For dynamic processes _endedRec is triggered by the
	// removeProcess function
	proc._endedRec.notify();
      }
    }

    _purgeProcs.length = 0;

    foreach(proc; _updateProcs) {
      final switch(proc._reqState) {
      case ProcStateReq.SUSPEND_REC:
      case ProcStateReq.DISABLE_REC:
      case ProcStateReq.ABORT_REC:
      case ProcStateReq.TERMINATE:
	expandedList ~= proc;
	auto childProcs = proc._esdl__getChildProcsHier();
	foreach(p; childProcs) {
	  p._reqState = proc._reqState;
	}
	expandedList ~= childProcs;
	break;
      case ProcStateReq.RESUME:
	expandedList ~= proc;
	if(proc._state is ProcState.SUSPENDED_REC) {
	  auto childProcs = proc._esdl__getChildProcsHier();
	  foreach(p; childProcs) {
	    p._reqState = proc._reqState;
	  }
	  expandedList ~= childProcs;
	}
	break;
      case ProcStateReq.ENABLE:
	expandedList ~= proc;
	if(proc._state is ProcState.DISABLED_REC) {
	  auto childProcs = proc._esdl__getChildProcsHier();
	  foreach(p; childProcs) {
	    p._reqState = proc._reqState;
	  }
	  expandedList ~= childProcs;
	}
	break;
      case ProcStateReq.SUSPEND:
      case ProcStateReq.DISABLE:
      case ProcStateReq.ABORT:
	expandedList ~= proc;
	break;
      case ProcStateReq.NONE:
	assert(false, "Illegal request state for process");
      }
    }

    debug(PROC) {
      import std.stdio;
      writeln("Updating ", expandedList.length, " processes....");
      foreach(proc; expandedList) {
	writeln(proc._reqState, "/", proc.procID);
      }
    }

    Process[] _termProcs;
    Barrier _termBarrier;

    // first create a list of tasks that require temination
    foreach(proc; expandedList) {
      final switch(proc.reqState) {
      case ProcStateReq.RESUME:
	if(proc.requestResume()) {
	  addRunnableProcess(proc);
	}
	break;
      case ProcStateReq.ENABLE:
	proc.requestEnable();
	break;
      case ProcStateReq.SUSPEND:
	proc.requestSuspension(false);
	break;
      case ProcStateReq.SUSPEND_REC:
	proc.requestSuspension(true);
	break;
      case ProcStateReq.DISABLE:
	proc.requestDisable(false);
	break;
      case ProcStateReq.DISABLE_REC:
	proc.requestDisable(true);
	break;
      case ProcStateReq.ABORT:
	if(proc.requestAbort(false)) {
	  _termProcs ~= proc;
	}
	break;
      case ProcStateReq.ABORT_REC:
	if(proc.requestAbort(true)) {
	  _termProcs ~= proc;
	}
	break;
      case ProcStateReq.TERMINATE:
	if(proc.requestTerminate()) {
	  _termProcs ~= proc;
	}
	break;
      case ProcStateReq.NONE:
	assert(false, "Illegal Process Requested State -- NONE");
      }
      // If I uncomment the next line, I get a crash :-(
      // proc.reqState = ProcStateReq.NONE;
    }
    foreach(proc; expandedList) {
      proc.reqState = ProcStateReq.NONE;
    }
    _updateProcs.length = 0;
    if(_termProcs.length > 0) {
      // create a barrier and make the threads wait on that
      _termBarrier = new Barrier(cast(uint)_termProcs.length + 1);
      _procBarrier = _termBarrier;
      // right now only handles terminate requests
      foreach(proc; _termProcs) {
	proc.terminateWaiting();
      }
      _termBarrier.wait();
    }
  }

  static class DebugBarrier: Barrier
  {
    private int _num;
    private int _size;
    public this(uint num) {
      synchronized(this) {
	_num = num;
	_size = num;
	debug(BARRIER_TRACE) {
	  import std.stdio;
	  writeln("Creating a Barrier of size: ", _num);
	}
      }
      super(num);
    }
    final override void wait() {
      synchronized(this) {
	assert(--_num >= 0);
	debug(BARRIER_TRACE) {
	  import std.stdio;
	  writeln("Waiting a Barrier of size: ", _size);
	}
      }
      super.wait();
      synchronized(this) {
	assert(_num is 0);
	debug(BARRIER_TRACE) {
	  import std.stdio;
	  writeln("Coming out of Barrier of size: ", _size);
	}
      }
    }
  }

  static class DebugSemaphore: Semaphore
  {
    private int _num;
    private int _size;
    public this(uint num) {
      synchronized(this) {
	_num = num;
	_size = num;
	debug(SEMAPHORE_TRACE) {
	  import std.stdio;
	  writeln("Creating a Semaphore of size: ", _num);
	}
      }
      super(num);
    }
    final override void wait() {
      super.wait();
      synchronized(this) {
	assert(--_num >= 0);
	debug(SEMAPHORE_TRACE) {
	  import std.stdio;
	  writeln("Waiting a Semaphore of size: ", _num);
	}
      }
    }
    final override void notify() {
      synchronized(this) {
	assert(++_num <= _size);
	debug(SEMAPHORE_TRACE) {
	  import std.stdio;
	  writeln("Notifying a Semaphore of size: ", _num);
	}
      }
      super.notify();
    }
  }

  private final void threadCount(size_t numThreads=1) {
    synchronized(this) {
      debug {
	import std.stdio: writeln;
	writeln("Creating an Executor with ", numThreads, " active threads.");
      }
      debug(SEMAPHORE) {
	_procSemaphore = new DebugSemaphore(cast(uint)numThreads);
      }
      else {
	_procSemaphore = new Semaphore(cast(uint)numThreads);
      }
      debug(BARRIER) {
	_routineThreadBarrier = new DebugBarrier(cast(uint)numThreads + 1);
	_routineThreadStartBarrier = new DebugBarrier(cast(uint)numThreads + 1);
      }
      else {
	_routineThreadBarrier = new Barrier(cast(uint)numThreads + 1);
	_routineThreadStartBarrier = new Barrier(cast(uint)numThreads + 1);
      }
      // _termBarrier = new Barrier(// cast(uint)tasks.length +
      //			 1);
      // _termSemaphore = new Semaphore(// cast(uint)numThreads
      //			     1);
    }
  }

  public final void execRoutines() {
    foreach(ref _routineThread; this._routineThreads) {
      _routineThread._waitLock.notify();
    }
    _routineThreadBarrier.wait();
  }

  public final void termRoutines() {
    import std.stdio: writeln;
    writeln(" > Shutting down all the Routine Threads ....");
    foreach(ref _routineThread; this._routineThreads) {
      _routineThread.terminate();
      _routineThread._waitLock.notify();
    }
    _routineThreadBarrier.wait();
  }

  public final void execProcs() {
    Process[] runProcs;
    debug(EXECUTOR) {
      import std.stdio: writeln;
      writeln("Creating a barrier of size: ",
	      _runnableProcs.length);
    }
    debug(EXECUTOR) {
      import std.stdio: writeln;
      writeln("******* About to execute ",
	      _runnableProcs.length, " tasks");
    }


    foreach(ref task; this._runnableProcs) {
      debug(EXECUTOR) {
	import std.stdio: writeln;
	writeln("******* About to execute ",
		task.procID, " (ID) ", task.state, "(status)");
      }
      if(task.preExecute()) {
	runProcs ~= task;
      }
    }

    this._runnableProcs.length = 0;

    debug(BARRIER) {
    _procBarrier = new DebugBarrier(cast(uint)runProcs.length + 1);
    }
    else {
      _procBarrier = new Barrier(cast(uint)runProcs.length + 1);
    }

    while(runProcs.length != 0) {
      Process[] tasks = runProcs;
      runProcs.length = 0;
      foreach(ref task; tasks) {
	this._procSemaphore.wait();
	if(task._esdl__parLock is null ||
	   task._esdl__parLock.tryWait) {
	  task.execute();
	}
	else {			// postpone
	  runProcs ~= task;
	  this._procSemaphore.notify();
	  debug(EXECUTOR) {
	    import std.stdio: writeln;
	    writeln("######## Could not get lock -- Postponing Process ",
		    runProcs.length, " tasks");
	  }
	}
      }
    }

    debug(EXECUTOR) {
      import std.stdio: writeln;
      writeln("All tasks executing");
    }

    this._procBarrier.wait();

    debug(EXECUTOR) {
      import std.stdio: writeln;
      writeln("All tasks done with executing");
    }
  }


  public final void terminateProcs(Process[] procs) {
    import std.algorithm: filter, count;	// filter
    auto waitingProcs = filter!(function bool(Process t) { return t.isWaiting();})(procs);

    debug {
      import std.stdio: writeln;
      writeln("******* About to terminate ",
	      count(waitingProcs), " waiting procs");
    }
    debug(BARRIER) {
    _procBarrier = new DebugBarrier(cast(uint)(count(waitingProcs) + 1));
    }
    else {
      _procBarrier = new Barrier(cast(uint)(count(waitingProcs) + 1));
    }
    foreach(ref proc; waitingProcs) {
      debug {
	import std.stdio: writeln;
	writeln("Terminating Process");
      }
      _procSemaphore.wait();
      proc.terminateProcess();
    }
    debug {
      import std.stdio: writeln;
      writeln("All processes terminating");
    }
    _procBarrier.wait();
    debug {
      import std.stdio: writeln;
      writeln("All processes done with terminating");
    }
  }
}

interface EsdlScheduler
{
public:
  // void cancel(EventNotice e);
  // An EventObj can be queued up
  void cancelDeltaEvent(TimedEvent e);
  void cancelImmediateEvent(TimedEvent e);
  void insertTimed(EventNotice e);
  size_t insertDelta(TimedEvent e);
  size_t insertImmediate(TimedEvent e);
  public void triggerDeltaEvents();
  public void triggerImmediateEvents();
  public SimPhase triggerNextEventNotices(SimTime maxTime);
  public SimTime simTime();
}

class EsdlHeapScheduler : EsdlScheduler
{
  private EsdlSimulator _simulator;

  import std.container: BinaryHeap;
  private EventNotice[] _noticeQueue;
  private TimedEvent[] _deltaQueue;
  private TimedEvent[] _deltaQueueAlt;
  private TimedEvent[] _immediateQueue;
  private TimedEvent[] _immediateQueueAlt;

  private SimTime _simTime = SimTime(0);

  final public SimTime simTime() {
    synchronized(this) {
      return _simTime;
    }
  }

  static int less(T)(T a, T b) {
    return a < b;
  }

  static int greater(T)(T a, T b) {
    return a > b;
  }

  alias BinaryHeap!(EventNotice[], greater) Heap;
  private Heap _noticeHeap;

  this(EsdlSimulator simulator) {
    synchronized(this) {
      this._noticeQueue = new EventNotice[8];
      this._noticeHeap = Heap(_noticeQueue, 0);
      this._simulator = simulator;
    }
  }

  public final void insertTimed(EventNotice e) {
    // combining these synchronized guards results in segfault
    synchronized(this) {
      // load(e);
      if(this._noticeQueue.length == this._noticeHeap.length) {
	// double the underlying array
	debug(SCHEDULER_HEAP) {
	  import std.stdio: writeln;
	  writeln("Increasing the heap store to: ",
		  this._noticeQueue.length * 2);
	}
	this._noticeQueue.length *= 2;
	this._noticeHeap.assume(this._noticeQueue,
				this._noticeHeap.length);
      }
      this._noticeHeap.insert(e);
    }
  }

  public final size_t insertDelta(TimedEvent e) {
    synchronized(this) {
      // synchronized(e.getObj)
      debug {
	import std.stdio: writeln;
	writeln("======== Adding delta event at ",
		_deltaQueue.length);
      }
      this._deltaQueue ~= e;
      return this._deltaQueue.length - 1;
    }
  }

  public final size_t insertImmediate(TimedEvent e) {
    synchronized(this) { // synchronized(e.getObj)
      debug {
	import std.stdio: writeln;
	writeln("======== Adding Immediate TimedNotice ",
		_immediateQueue.length);
      }
      this._immediateQueue ~= e;
      return this._immediateQueue.length - 1;
    }
  }

  public final void cancelImmediateEvent(TimedEvent e) {
    synchronized(e) {
      synchronized(this) {
	import std.exception: enforce;
	enforce((this._immediateQueue[e._eventQueueIndex] is e),
		"Received event with wrong _eventQueueIndex");
	// move the last event in the queue to occupy cancelled events place
	synchronized(this._immediateQueue[$-1]) {
	  this._immediateQueue[$-1]._eventQueueIndex = e._eventQueueIndex;
	}
	this._immediateQueue[e._eventQueueIndex] = this._immediateQueue[$-1];
	// reduce the event queue
	this._immediateQueue.length -= 1;
      }
    }
  }

  public final void cancelDeltaEvent(TimedEvent e) {
    synchronized(e) {
      synchronized(this) {
	import std.exception: enforce;
	enforce((this._deltaQueue[e._eventQueueIndex] is e),
		"Received event with wrong _eventQueueIndex");
	// move the last event in the queue to occupy cancelled events place
	synchronized(this._deltaQueue[$-1]) {
	  this._deltaQueue[$-1]._eventQueueIndex = e._eventQueueIndex;
	}
	this._deltaQueue[e._eventQueueIndex] = this._deltaQueue[$-1];
	// reduce the event queue
	this._deltaQueue.length -= 1;
      }
    }
  }

  public final void triggerDeltaEvents() {
    debug(SCHEDULER) {
      import std.stdio: writeln;
      writeln("There are ", this._deltaQueue.length,
	      " delta events to trigger");
    }

    auto _exec = _deltaQueue;
    _deltaQueue = _deltaQueueAlt;
    _deltaQueueAlt = _exec;

    _deltaQueue.length = 0;

    foreach(ref event; this._deltaQueueAlt) {
      event.trigger(_simulator);
    }
  }

  public final void triggerImmediateEvents() {
    debug(SCHEDULER) {
      import std.stdio: writeln;
      writeln("There are ", this._immediateQueue.length,
	      " events to trigger");
    }
    auto _exec = _immediateQueue;
    _immediateQueue = _immediateQueueAlt;
    _immediateQueueAlt = _exec;

    _immediateQueue.length = 0;

    foreach(ref event; this._immediateQueueAlt) {
      event.trigger(_simulator);
    }
  }

  public final SimPhase triggerNextEventNotices(SimTime maxTime) {
    if(this._noticeHeap.empty()) {
      return SimPhase.PAUSE;
    }

    EventNotice firstEvent = this._noticeHeap.front();

    // Since the scheduler runs as a single task this
    // no synchronization guards would be needed
    if(firstEvent.atTime >= maxTime) {
      this._simTime = maxTime;
      // import std.stdio: writeln;
      // writeln("Max Simulation SimTime reached, Terminating Simulation");
      return SimPhase.PAUSE;
    }

    debug {
      import std.stdio: writeln;
      writeln(" > Advancing simulation time to #",
	      firstEvent.atTime.getVal);
    }
    debug(TIME) {
      import std.stdio: writeln;
      writeln(" > Advancing simulation time to #",
	      firstEvent.atTime.getVal);
    }
    this._simTime = firstEvent.atTime;
    debug(EVENTS) {size_t numTriggered = 0;}
    while(firstEvent.atTime == this._simTime) {
      debug {
	import std.stdio: writeln;
	writeln("Notifying Timed Event");
      }
      firstEvent.trigger(_simulator);
      debug(EVENTS) {++numTriggered;}
      this._noticeHeap.removeFront();
      if(this._noticeHeap.empty()) {
	break;
      }
      else {
	firstEvent = this._noticeHeap.front();
      }
    }
    debug(EVENTS) {
      import std.stdio: writeln;
      writeln("Triggered Timed Events: ",
	      numTriggered);
    }
    return SimPhase.SIMULATE;
  }
}

interface RootEntityIntf: EntityIntf
{
  private __gshared RootEntityIntf[] _roots;
  static void addRoot(RootEntityIntf root) {
    _roots ~= root;
  }
  static RootEntityIntf[] allRoots() {
    return _roots;
  }

  public void _esdl__unboundPorts();
  public void _esdl__unboundExePorts();

  // This function is called just before a thread is started by the simulator
  // Every thread calls this function. Also the function is called after the
  // elaboration phase is over. Since all the threads call it, it is useful
  // to set thread static variables.
  public void initProcess();
  public void initRoutine();

  static void forkAllRootSim(T)(T t)
    if(is(T == Time) || is(T == SimTime)) {
      foreach(root; allRoots()) {
	root.forkSim(t);
      }
    }
  static void joinAllRootSim() {
    foreach(root; allRoots()) {
      root.joinSim();
    }
  }
  static void simulateAll(T)(T t)
    if(is(T == Time) || is(T == SimTime)) {
      forkAllRootSim(t);
      joinAllRootSim();
    }
  static void terminateAll() {
    foreach(root; allRoots()) {
      root.terminate();
    }
  }

  public EsdlSimulator simulator();

  public SimTime getSimTime();

  public void setTimePrecision(Time precision);
  public Time getTimePrecision();
  public bool timePrecisionSet();
  public void timePrecisionSet(bool s);
  
  final void simulate(Time simTime) {
    forkSim(simTime);
    joinSim();
  }
  final void simulate(SimTime maxTime = MAX_SIMULATION_TIME) {
    forkSim(maxTime);
    joinSim();
  }
  final public void joinSim() {
    this.getSimulator.joinSim();
  }
  final public void joinElab() {
    this.getSimulator.joinElab();
    addRoot(this);
  }
  final void forkSim(Time simTime) {
    // So that the simulation root thread too returns a legal value for
    // getRootEntity
    _esdl__rootEntity = this;
    getSimulator.forkSim(simTime);
  }
  final void forkSim(SimTime maxTime = MAX_SIMULATION_TIME) {
    // So that the simulation root thread too returns a legal value for
    // getRootEntity
    _esdl__rootEntity = this;
    getSimulator.forkSim(maxTime);
  }
  final public void joinTerm() {
    this.getSimulator.joinTerm();
  }
  final public void terminate() {
    this.getSimulator.terminate();
  }
}

abstract class RootEntity: RootEntityIntf
{
  EsdlSimulator _simulator;
  public final EsdlSimulator simulator() {
    return _simulator;
  }

  mixin(entityMixin());

  this(string name) {
    synchronized(this) {
      if(name == "") assert(false,
			    "Must provide a valid name to the Root Entiry");
      this._esdl__setName(name);
      _simulator = new EsdlSimulator(this);
      _esdl__root = this;
      _esdl__parent = this;
      _simulator._esdl__setRoot(this);
      _simulator._esdl__setParent(this);
      _simulator._esdl__setName("_simulator");
    }
  }

  protected bool _esdl__noUnboundPorts = true;
  protected bool _esdl__noUnboundExePorts = true;

  private Time _timingPrecision;
  private bool _timingPrecisionSet = false;

  public void setTimePrecision(Time precision) {
    synchronized(this) {
      if(_timingPrecision is Time.init ||
	 _timingPrecision > precision) {
	_timingPrecision = precision;
      }
    }
  }

  public Time getTimePrecision() {
    synchronized(this) {
      return _timingPrecision;
    }
  }

  public bool timePrecisionSet() {
    synchronized(this) {
      return _timingPrecisionSet;
    }
  }

  public void timePrecisionSet(bool s) {
    synchronized(this) {
      _timingPrecisionSet = s;
    }
  }

  public final override void _esdl__unboundPorts() {
    _esdl__noUnboundPorts = false;
  }

  public final override void _esdl__unboundExePorts() {
    _esdl__noUnboundExePorts = false;
  }

  public override void initRoutine() {
    _esdl__rootEntity = this;
    _esdl__timeScale = Routine.self.timeScale();
    _esdl__simPhase = SimPhase.SIMULATE;
  }

  public override void initProcess() {
    _esdl__rootEntity = this;
    _esdl__timeScale = Process.self.timeScale();
    _esdl__simPhase = SimPhase.SIMULATE;
  }

  public final override SimTime getSimTime() {
    return _esdl__root.simulator().simTime();
  }
}

class EsdlSimulator: EntityIntf
{
  mixin(entityMixin());

  enum SchedPhase: byte
  {   IMMEDIATE,
      DELTA,
      TIMED,
      UPDATE,
      EXEC
      }

  SchedPhase _sched = SchedPhase.IMMEDIATE;
  

  public final SchedPhase schedPhase() {
    synchronized(this)
      return _sched;
  }

  public final void schedPhase(SchedPhase sched) {
    synchronized(this)
      _sched = sched;
  }

  // Phase is defined in the SimContext interface class
  // enum SimPhase : byte {BUILD, ELABORATE, CONFIGURE, BINDEXEPORTS, BINDPORTS, SIMULATE}
  @_esdl__ignore private long _updateCount = 0;	// increments each time update happens
  @_esdl__ignore private size_t _threadCount = 1;

  private RootProcess _rootThread;
  public final RootProcess rootThread() {
    synchronized(this) {
      return _rootThread;
    }
  }

  private SimTime _runUntil;

  private final SimTime runUntil() {
    synchronized(this) {
      return _runUntil;
    }
  }

  private final void runUntil(SimTime val) {
    synchronized(this) {
      _runUntil = val;
    }
  }

  @_esdl__ignore private Channel[] _channelUpdateReqs;

  private final Channel[] channelUpdateReqs() {
    synchronized(this) {
      return _channelUpdateReqs;
    }
  }

  private final void clearChannelUpdateReqs() {
    synchronized(this) {
      _channelUpdateReqs.length = 0;
    }
  }

  // private EventObj[]  _triggeredEvents;

  protected SimPhase _phase = SimPhase.BUILD;
  public final SimPhase phase() {
    synchronized(this) {
      return this._phase;
    }
  }

  public final void setPhase(SimPhase _phase) {
    synchronized(this) {
      this._phase = _phase;
      _esdl__simPhase = _phase;
    }
  }

  protected bool _terminationRequested = false;

  public final void terminateSim() {
    synchronized(this) {
      _terminationRequested = true;
    }
  }

  public final void threadCount(size_t count) {
    synchronized(this) {
      import std.exception: enforce;
      enforce(this._phase != SimPhase.SIMULATE);
      this._threadCount = count;
    }
  }

  public final long updateCount() {
    synchronized(this) {
      return _updateCount;
    }
  }

  public final void incrUpdateCount() {
    synchronized(this) {
      _updateCount += 1;
    }
  }

  final void triggerElab() {
    rootThread.start();
  }
  // private Process[] tasks;
  final void forkSim(Time simTime) {
    // elabDoneLock.wait();
    // elabDoneLock.notify();		// notified so that forkSim(maxTime) gets going
    SimTime maxTime = SimTime(this, simTime);
    this.forkSim(maxTime);
  }

  final void forkSim(SimTime maxTime = MAX_SIMULATION_TIME) {
    synchronized(this) {
      import std.conv: to;
      // elabDoneLock.wait();
      switch(phase) {
      case SimPhase.PAUSE:
	runUntil(maxTime);
	this.setPhase(SimPhase.SIMULATE);
	simStartLock.notify();
	break;
      default:
	assert(false, "Can not start a simulatot in state: " ~ phase.to!string);
      // super.join();
      }
    }
  }

  final void terminate() {
    synchronized(this) {
      if(phase !is SimPhase.PAUSE) {
	import std.conv: to;
	assert(false, "Asked to terminate a simulation in phase: " ~
	       phase.to!string);
      }
      setPhase(SimPhase.TERMINATED);
      // Get the doSim move ahead
      simStartLock.notify();
      import std.stdio: writeln;
      writeln(" > Shutting down all the active tasks ....");
      this._executor.terminateProcs(getRoot()._esdl__getChildProcsHier());
      this._executor.termRoutines();
      writeln(" > Simulation Complete....");
      simTermLock.notify();
    }
  }

  // always defined in the derived simulator
  final void doSim() {
    simStartLock.wait();
    runSimulation();
    simDoneLock.notify();
  }

  final void runSimulation() {
    while(this.phase == SimPhase.SIMULATE) {
      schedPhase = SchedPhase.IMMEDIATE;
      // bool channelUpdatePending = false;
      // tasks = this._executor.getRunnableProcs();

      // Look at all the requests for thread terminations/suspensions etc
      _executor.updateProcs();

      _executor.processRegisteredProcs();

      debug(SCHEDULER) {
	import std.stdio: writeln;
	writeln(" > Looking for Immediate tasks/routines");
      }

      _scheduler.triggerImmediateEvents();
      // tasks = _executor.getRunnableProcs();
      debug(SCHEDULER) {
	import std.stdio: writeln;
	if(_executor.runnableProcsCount)
	  writeln(" > Got Immediate tasks: ", _executor.runnableProcsCount);
      }
      debug(SCHEDULER) {
	import std.stdio: writeln;
	if(_executor.runnableRoutinesCount)
	  writeln(" > Got Immediate routines: ", _executor.runnableRoutinesCount);
      }
      if(_executor.runnableThreadsCount is 0) {
	schedPhase = SchedPhase.UPDATE;
	// No immediate task -- update channels if required
	// channelUpdatePending = true;
	incrUpdateCount();
	foreach(ref chan; this.channelUpdateReqs) {
	  chan.update();
	  chan.updateDone();
	}
	clearChannelUpdateReqs();
	// Also reset the _triggered flag on triggered events
	// foreach(ref e; _triggeredEvents) {
	//   e.resetTriggered();
	// }
	// _triggeredEvents.length = 0;

	schedPhase = SchedPhase.DELTA;
	debug(SCHEDULER) {
	  import std.stdio: writeln;
	  writeln(" > Looking for Delta tasks/routines");
	}
	debug(TIME) {
	  import std.stdio: writeln;
	  writeln(" > Incrementing Delta SimTime");
	}
	_scheduler.triggerDeltaEvents();
	debug(SCHEDULER) {
	  if(_executor.runnableProcsCount) {
	    import std.stdio: writeln;
	    writeln(" > Got Delta tasks: ", _executor.runnableProcsCount);
	  }
	}
	debug(SCHEDULER) {
	  if(_executor.runnableRoutinesCount) {
	    import std.stdio: writeln;
	    writeln(" > Got Delta routines: ", _executor.runnableRoutinesCount);
	  }
	}
	bool nextStage = false;
	while(this.phase is SimPhase.SIMULATE &&
	      _executor.runnableThreadsCount is 0 &&
	      nextStage is false) {
	  schedPhase = SchedPhase.TIMED;
	  debug(SCHEDULER) {
	    import std.stdio: writeln;
	    writeln(" > Looking for Timed tasks/routines");
	  }
	  switch(_scheduler.triggerNextEventNotices(runUntil())) {
	  case SimPhase.SIMULATE:
	    debug(SCHEDULER) {
	      if(_executor.runnableProcsCount) {
		import std.stdio: writeln;
		writeln(" > Got Timed tasks: ",
			_executor.runnableProcsCount);
	      }
	    }
	    debug(SCHEDULER) {
	      if(_executor.runnableRoutinesCount) {
		import std.stdio: writeln;
		writeln(" > Got Timed routines: ",
			_executor.runnableRoutinesCount);
	      }
	    }
	    break;
	  case SimPhase.TERMINATED:
	    this.setPhase(SimPhase.TERMINATED);
	    break;
	  case SimPhase.PAUSE:
	    // import std.stdio: writeln;
	    // writeln("No Events in the Scheduler, Terminating Simulation");
	    // FIXME -- make it depend on whether this simulator is the only actor
	    if(checkPersistFlags()) {
	      this.setPhase(SimPhase.PAUSE);
	    }
	    else {
	      if(_executor.incrStage()) {
		// Make it go back to the beginning of runSimulation
		nextStage = true;
	      }
	      else {
		this.setPhase(SimPhase.PAUSE);
		this.terminate();
	      }
	    }
	    break;
	  default:
	    assert(false, "Illegal Phase!");
	  }
	}
      }

      schedPhase = SchedPhase.EXEC;
      debug(SCHEDULER) {
	import std.stdio: writeln;
	writeln(" > Executing tasks: ");
      }
      if(_executor.runnableProcsCount)
	_executor.execProcs();
      debug(SCHEDULER) {
	import std.stdio: writeln;
	writeln(" > Done executing tasks");
      }
      if(_executor.runnableRoutinesCount)
	_executor.execRoutines();
    }
  }

  final void joinElab() {
    elabDoneLock.wait();
  }

  final void joinSim() {
    if(phase is SimPhase.SIMULATE) {
      simDoneLock.wait();
    }
    else {
      import std.conv: to;
      assert(false, "Asked to wait on a simulation that is in phase: " ~
	     phase.to!string);
    }
  }

  final void joinTerm() {
    simTermLock.wait();
  }

  private PersistFlag[] _persistFlags;
  public final void registerPersistFlag(PersistFlag flag) {
    synchronized(this) {
      _persistFlags ~= flag;
    }
  }
  private final bool checkPersistFlags() {
    PersistFlag[] flags;
    foreach(flag; _persistFlags) {
      if(flag.raised) {
	flags ~= flag;
      }
      else {
	flag.deregister();
      }
    }
    _persistFlags = flags;
    if(_persistFlags.length !is 0) return true;
    else return false;
  }


  import core.sync.semaphore: Semaphore;
  Semaphore _elabDoneLock;
  Semaphore _simStartLock;
  Semaphore _simDoneLock;
  Semaphore _simTermLock;

  public final Semaphore elabDoneLock() {
    synchronized(this)
      return _elabDoneLock;
  }
  public final Semaphore simStartLock() {
    synchronized(this)
      return _simStartLock;
  }
  public final Semaphore simDoneLock() {
    synchronized(this)
      return _simDoneLock;
  }
  public final Semaphore simTermLock() {
    synchronized(this)
      return _simTermLock;
  }

  public final void reqUpdateProc(Process task) {
    this._executor.reqUpdateProcess(task);
  }

  public final void reqPurgeProc(Process task) {
    this._executor.reqPurgeProcess(task);
  }

  public final void reqRegisterProcess(Process task, int stage=0) {
    if(task.registered) {
      assert(false, "Can not register same task twice");
    }
    task.register();
    this._executor.reqRegisterProcess(task, stage);
  }

  public final void reqRegisterRoutine(Routine routine) {
    this._executor.reqRegisterRoutine(routine);
  }

  public final void requestUpdate(Channel channel) {
    synchronized(this) {
      this._channelUpdateReqs ~= channel;
    }
  }


  final void freeLock(Process proc, bool onlyBarrier=false) {
    if(onlyBarrier is false) {
      if(proc._esdl__parLock !is null) {
	proc._esdl__parLock.notify();
      }
      this._executor._procSemaphore.notify();
    }
    this._executor._procBarrier.wait();
  }
  // The class Executor is responsible for executing the runnable
  // tasks and routines

  public EsdlScheduler _scheduler;
  public EsdlExecutor _executor;

  final SimTime simTime() {
    return this._scheduler.simTime();
  }

  this(RootEntityIntf _root) {
    synchronized(this) {
      this._esdl__root = _root;
      this._esdl__parent = _root;
      try {
	import std.exception: enforce;
	import std.stdio: writeln;
	this._scheduler = new EsdlHeapScheduler(this);
	this._executor = new EsdlExecutor(this);

	import core.cpuid: threadsPerCPU;
	_elabDoneLock = new Semaphore(0);
	_simStartLock = new Semaphore(0);
	_simDoneLock = new Semaphore(0);
	_simTermLock = new Semaphore(0);

	version(MULTICORE) {
	  import core.cpuid: threadsPerCPU;
	  _executor.threadCount(2*threadsPerCPU());
	  _executor.createRoutineThreads(2*threadsPerCPU(), 0);
	}
	else {
	  _executor.threadCount(1);
	  _executor.createRoutineThreads(1, 0);
	}
	// We do this to make dure that all the routine threads are up
	// and running before we attempt to create other threads. For
	// some reason the simulation sometimes gets into a deadlock
	// if we do not take care of this
      }
      catch(Throwable e) {
	import std.conv: to;
	import std.stdio: writeln;
	writeln("Exiting with Errors!\n" ~ to!string(e));
	// writeln("Simulation ended with Errors");
	// exit(1);
	throw(e);
      }
    }
  }

  public final void elabRootThread(T)(T t) {
    synchronized(this) {
      _rootThread = new RootProcess({
	  _esdl__rootEntity = t;
	  import std.stdio;
	  writeln("Root thread started");
	  try {
	    runSim(t);
	  }
	  catch(Throwable e) {
	    import std.stdio: writeln;
	    writeln("Simulation Root Thread threw exception: ", e);
	    // throw(e);
	  }
	});
      _rootThread._esdl__setParent(this);
      _rootThread._esdl__setName("root");
    }
    this.triggerElab();
  }

  final void runSim(T)(T t) {
    _esdl__rootEntity = t;
    t.doElab();
    // inclrementally run simulation
    while(this.phase !is SimPhase.TERMINATED) {
      t.getSimulator.doSim();
    }
  }
}

private static RootEntityIntf _esdl__rootEntity;
private static ulong _esdl__timeScale;
private static SimPhase _esdl__simPhase;

public SimTime getSimTime() {
  return _esdl__rootEntity.getSimTime();
}

public RootEntityIntf getRootEntity() {
  return _esdl__rootEntity;
}

public ulong getTimeScale() {
  return _esdl__timeScale;
}

public SimPhase getSimPhase() {
  return _esdl__simPhase;
}

template _esdl__attr(alias A, T)
{
  enum int _esdl__attr = _esdl__attrIndexed!(A, 0, -1,
					     __traits(getAttributes, T));
}

template _esdl__attr(alias A, alias t, size_t I)
{
  enum int _esdl__attr = _esdl__attrIndexed!(A, 0, -1,
					     __traits(getAttributes,
						      t.tupleof[I]));
}

template _esdl__attrIndexed(alias A, size_t C, int P, AA...)
{
  static if(AA.length == 0) {
    enum int _esdl__attrIndexed = P;
  }
  else {
    static if(__traits(isSame, AA[0], A) ||
	      is(typeof(AA[0]) == A)) {
      static assert(P == -1, "@" ~ A.stringof ~
		    " used twice in the same declaration");
      static if(AA.length > 1) {
	enum int _esdl__attrIndexed =
	  _esdl__attrIndexed!(A, C+1, C, AA[1..$]);
      }
      else {
	enum int _esdl__attrIndexed = C;
      }
    }
    else {
      enum int _esdl__attrIndexed =
	_esdl__attrIndexed!(A, C+1, P, AA[1..$]);
    }
  }
}


public void finish() {
  debug(FINISH) {
    import std.stdio;
    writeln("Somebody called finish");
  }
  getRootEntity.abortRec();
  version(COSIM_VERILOG) {
    // pragma(msg, "Compiling COSIM_VERILOG version!");
    import esdl.intf.vpi;
    vpi_control(vpiFinish, 1);
  }
}
