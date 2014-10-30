/**
Type casting functions.

Functions from this module are intended to be used instead of
build-in $(HTTP dlang.org/expression.html#CastExpression, $(D cast))
as they provide safer and richer interface.

Copyright: Denis Shelomovskij 2013

License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors: Denis Shelomovskij
*/
module unstd.casts;


import std.string;


@safe pure nothrow:

/**
Functions for $(D class)/$(D interface) dynamic casting.

These functions don't change type qualifier.

$(D toRawPtr)/$(D fromRawPtr) should be used to safely convert class instances
to raw $(D void*) instead of $(MREF viewAs) because $(D interface) variables
don't point to a beginning of a class instance.

Preconditions:
Passed object (pointer for $(D fromRawPtr)) is not $(D null).

Note:
$(HTTP dlang.org/class.html#AliasThis, $(D alias this)) isn't considered while casting
in contrary to build-in $(HTTP dlang.org/expression.html#CastExpression, $(D cast)).
*/
enum typesDynamicCastable(To, From) =
	(is(From == class) || is(From == interface)) && (is(To == class) || is(To == interface))
		&& (is(From == interface) || is(To == interface) || is(To : From) || is(From : To));

/// ditto
@property inout(To) tryDynamicCast(To, From)(inout(From) o) @trusted @nogc
if(typesDynamicCastable!(To, From))
{
	return o._dynamicCastImpl!To;
}

/// ditto
@property bool dynamicCastable(To, From)(inout(From) o) @trusted @nogc
if(typesDynamicCastable!(To, From))
{
	return !!o._dynamicCastImpl!To;
}

/// ditto
@property inout(To) dynamicCast(To, From)(inout(From) o) @trusted @nogc
if(typesDynamicCastable!(To, From))
in { assert(o.dynamicCastable!To); }
body
{
	return o._dynamicCastImpl!(To, true);
}


/// ditto
enum typesDowncastable(To, From) =
	(is(From == class) || is(From == interface)) && (is(To : From) || is(From == Object));

/// ditto
@property inout(To) tryDowncast(To, From)(inout(From) o) @trusted @nogc
if(typesDowncastable!(To, From))
{
	return o._dynamicCastImpl!To;
}

/// ditto
@property bool downcastable(To, From)(inout(From) o) @trusted @nogc
if(typesDowncastable!(To, From))
{
	return !!o._dynamicCastImpl!To;
}

/// ditto
@property inout(To) downcast(To, From)(inout(From) o) @trusted @nogc
if(typesDowncastable!(To, From))
in { assert(o.downcastable!To); }
body
{
	return o._dynamicCastImpl!(To, true);
}


/// ditto
enum typesUpcastable(To, From) =
	(is(To == class) || is(To == interface)) && (is(From : To) || is(To == Object));

/// ditto
@property inout(To) upcast(To, From)(inout(From) o) @trusted @nogc
if(typesUpcastable!(To, From))
{
	return o._dynamicCastImpl!(To, true);
}


/// ditto
@property inout(void)* toRawPtr(From)(inout(From) o) @trusted @nogc
if(is(From == class) || is(From == interface))
{
	return o.upcast!Object.viewAs!(void*);
}

/// ditto
@property inout(To) fromRawPtr(To)(inout(void)* p) @system @nogc
if(is(To == class) || is(To == interface))
{
	return p.viewAs!Object.downcast!To;
}


///
unittest
{
	class A { }
	class B { }

	A a = new A;
	Object o = a;
	assert(o.downcast!A is a);
	assert(!o.downcastable!B);
	static assert(!__traits(compiles, a.dynamicCast!B)); // cast impossible
}

///
@trusted unittest
{
	interface I { }
	class A : I { }

	A a = new A;
	I i = a;
	void* p = a.toRawPtr;
	// `i` doesn't point to a beginning of a class instance:
	assert(i.viewAs!(void*) != p);
	assert(i.toRawPtr == p);
	assert(p.fromRawPtr!I is a); // `fromRawPtr` is `@system`
}

unittest
{
	class A { }
	class B: A { }

	B b = new B;
	A a = b;
	assert(a.downcast!B is b);
	assert(b.upcast!A is a);
	static assert(!__traits(compiles, b.downcast!A));
	static assert(!__traits(compiles, a.upcast!B));
	const ca = a;
	static assert(is(typeof(ca.downcast!B) == const B));
	const cb = b;
	static assert(is(typeof(cb.upcast!A) == const A));


	class X { }
	X x;
	static assert(!__traits(compiles, x.downcast!A));
	static assert(!__traits(compiles, x.dynamicCast!A));
}

unittest
{
	interface I { }
	class C: I { }
	C c = new C;
	I i = c;
	assert(c.upcast!I is i);
	Object o = c;
	assert(o.downcast!I is c);
	assert(i.upcast!Object is c);

	assert(i.dynamicCast!C is c);
	assert(o.dynamicCast!C is c);
	assert(o.dynamicCast!I is i);

	void* p = o.toRawPtr;
	assert(c.toRawPtr == p);
	assert(i.toRawPtr == p);
	() @trusted
	{
		assert(p.fromRawPtr!C is c);
		assert(p.fromRawPtr!I is i);
	} ();
}

unittest
{
	interface I1 { }
	interface I2 { }
	class C: I1, I2 { int n; alias n this; }
	class D: C {}
	D d = new D;
	C c = d;
	I1 i1 = c;
	I2 i2 = c;
	Object o = c;
	assert(o.downcast!I1 is c);
	assert(i2.dynamicCast!I1 is c);

	assert(c.upcast!I1() is c);
	assert(c.dynamicCast!I2 is c);

	assert(c.downcast!D() is d);
	assert(!new C().downcastable!D());
}


private @property inout(To) _dynamicCastImpl(To, bool castExists = false, From)(inout(From) o) @system @nogc
if(typesDynamicCastable!(To, From))
//in { assert(o, format("Attempt to perform dynamic cast of `null` from `%s` to `%s`.", From.stringof, To.stringof)); }
body
{
	static if(is(From == class) && is(To == class) && castExists)
	{
		return o.viewAs!To;
	}
	else
	{
		static if(is(From == class)) alias rtCast = _d_dynamic_cast;
		else alias rtCast = _d_interface_cast;
		return rtCast(o.viewAs!(void*), To.classinfo).viewAs!To;
	}
}

extern(C) @system @nogc
{
	inout(void)* _d_dynamic_cast(inout(void)* p, in ClassInfo c);
	inout(void)* _d_interface_cast(inout(void)* p, in ClassInfo c);
}


/**
Function allowing "blind" casting to any type of the same size.

No operations on actial value performed.
It is just treated as having different type.

These functions don't change type qualifier.

Note:
$(HTTP dlang.org/class.html#AliasThis, $(D alias this)) isn't considered while casting
in contrary to build-in $(HTTP dlang.org/expression.html#CastExpression, $(D cast)).

Warning:
Resulting value may depend on target architecture.
*/
@property inout(To) viewAs(To, From)(inout(From) val) @system @nogc
{
	return val.viewAs!To;
}

/// ditto
@property ref inout(To) viewAs(To, From)(ref inout(From) val) @system @nogc
{
	static assert(To.sizeof == From.sizeof,
		format("Type size mismatch in `viewAs`: %s.sizeof(%s) != %s.sizeof(%s)",
		To.stringof, To.sizeof, From.stringof, From.sizeof));
	return *cast(inout(To)*) &val;
}

///
@trusted @nogc unittest
{
	int i = 0;
	i.viewAs!(ubyte[4]) = 3;
	assert(i == 0x03030303);
}

@trusted @nogc unittest
{
	assert(1.viewAs!uint == 1);
	static assert(!__traits(compiles, 1.viewAs!ulong));
	assert(!Object.init.viewAs!size_t);

	int i = 0;
	++i.viewAs!uint;
	assert(i == 1);
	i.viewAs!(ushort[2]) = 1;
	assert(i == 0x00010001);

	const int ci = i;
	static assert(!__traits(compiles, ++ci.viewAs!uint));
	static assert(is(typeof(ci.viewAs!(ushort[2])) == const(ushort[2])));
}
