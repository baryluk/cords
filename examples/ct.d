module ct;

import RBt : RB;

class C {
	const int a;
	private const C b;

	this(int a_) {
		a = a_;
		b = null;
	}

	this(int a_, in const(C) b_) {
		a = a_;
		b = b_;
	}
	const(C) m() const {
		return b;
	}

//	const(C) opCast() {
//	    return this;
//	}

	const(C) merge(in const(C) b2) const {
	    return new C(a, b2);
	}
}

import std.stdio;

alias RB!(const C) CC;

CC CCC(int i) {
    return CC(new C(i));
}
CC CCC(int i, CC b) {
    return CC(new C(i, b.opDot()));
}

int something(in C a) {
    return a.a;
}

int something(CC a) {
    return something(a.opDot());
}

const(C) something2(in C a) {
	return a;
}

CC something2(CC a) {
	//return CC(something2(a.opDot()));
	return CC(something2(cast(const(C))a));
}

void main() {
	auto c1 = CCC(1);
	auto c2 = CCC(2, c1);
	CC c3 = CCC(2, c1);
	writefln("%d", c2.a);
	c2 = c2.m();
	writefln("%d", c2.a);
	writefln("%d", something(c2));
	CC c4 = something2(c3);
	c4 = c3.opDot();
	c4 = c3 = c2;
	c3 = c1.merge(c2.opDot());
}
