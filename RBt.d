module RBt;

//import std.typecons : Rebindable;

//alias Rebindable!(const C) CC;
// todo: opAssign(Rebindable!(T)) is missing so c4 = c3
// todo: T opCast() is missing for explicit cast(T)

// todo: opdot needs const

// because of this here is own Rebindable

template RBMixin(T, RetT) {
static if (is(T X == const(U), U) || is(T X == invariant(U), U)) {
	//pragma(msg, "Original: "~T.stringof);
	//pragma(msg, "Stripped: "~U.stringof);
	private union {
		T original;
		U stripped;
	}

	RetT opAssign(T another) {
		stripped = cast(U)another;
		return this;
	}

	RetT opAssign(RetT another) {
		stripped = another.stripped;
		return this;
	}

	static RetT opCall(T initializer) {
		//return RetT(initializer);
		RetT result;
		result = initializer;
		return result;
	}

	T opDot() const {
		return original;
	}

	static if (!is(T.opCast)) {
	//pragma(msg, "adding opCast");
	T opCast() {
		return original;
	}
	}
}
}

template RB(T) {
	static if (!is(T X == const(U), U) && !is(T X == invariant(U), U)) {
		alias T RB;
	} else {
		struct RB {
			mixin RBMixin!(T, RB);
			alias original get;
		}
	}
}


unittest {
	alias RB Rebindable;

	class C { int foo() const { return 42; } }
	Rebindable!(C) obj0;
	static assert(is(typeof(obj0) == C));

	Rebindable!(const(C)) obj1;
	static assert(is(typeof(obj1.get) == const(C)), typeof(obj1.get).stringof);
	static assert(is(typeof(obj1.stripped) == C));
	obj1 = new C;
	assert(obj1.get !is null);
	obj1 = new const(C);
	assert(obj1.get !is null);

	Rebindable!(invariant(C)) obj2;
	static assert(is(typeof(obj2.get) == invariant(C)));
	static assert(is(typeof(obj2.stripped) == C));
	obj2 = new invariant(C);
	assert(obj1.get !is null);

	// test opDot
	assert(obj2.foo == 42);
}
