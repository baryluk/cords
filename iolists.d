module iolists;

/** Immutable list based implementation, simple and fast for small strings, or for append/prepend only
 * Sometime calles IoList
 *
 * Actually this is just binary tree, without rebalancing
 *
 * Few methods which is known to be slow (like opIndexAssign) are removed, because if you really need such functionality,
 * you should use another structure. Other mutating methods and slicing (which are also slow), are there for some usages.
 */
final class cord_l2(T) {
	final class node {
		T[] v;
		node next;
		this() {
			v = null;
		}
		this(in T[] s) {
			v = s;
		}
		this(in T[] s, node n) {
			v = s;
			next = n;
		}
	}

	node* head;

	this() {
		first = last = new node(); // guard
	}

	this(in T[] s) {
		first = last = new node(s);
	}

	// inplace
	void opCatAssign(in T[] s) {
		last = (last.next = new node(s));
	}

	cord_l2!(T) opCat(in T[] s)
	out (res) {
		assert(res.length == this.length + s.length);
	}
	body {
		auto r = this.clone();
		r ~= s;
		return r;
	}

	cord_l2!(T) opCat_r(in T[] s)
	out (res) {
		assert(res.length == this.length + s.length);
	}
	body {
		auto r = this.clone();
		r.prepend(s);
		return r;
	}


	// inplace
	void prepend(in T[] s) {
		first = new node(s, first);
	}

	int opApply(int delegate(inout T[] chunk) dg) {
		int ret;
		node current = first;
		while (current) {
			auto s = cast(T[])current.v;
			if ((ret = dg(s)) != 0) {
				break;
			}
			current = current.next;
		}
		return ret;
	}

	size_t length() {
		size_t l = 0;
		foreach (chunk; this) {
			l += chunk.length;
		}
		return l;
	}

	override invariant(T)[] toString()
	out (res) {
		assert(res.length == this.length);
	}
	body {
		T[] ret;
		ret.length = this.length;
		size_t i = 0;
		foreach (chunk; this) {
			ret[i .. i+chunk.length] = chunk;
			i += chunk.length;
		}
		return cast(invariant)ret;
	}

	T opIndex(size_t i) {
		foreach (chunk; this) {
			if (i < chunk.length) {
				return chunk[i];
			}
			i -= chunk.length;
		}
		throw new Exception("CordBoundsException");
	}

	cord_l2!(T) clone()
	out (res) {
		assert(res.length == this.length);
	}
	body {
		auto r = new cord_l2!(T)();
		foreach (chunk; this) {
			r ~= chunk;
		}
		return r;
	}

	cord_l2!(T) opSlice(size_t j1, size_t j2)
	in {
		assert(j1 <= j2);
		assert(j2 <= this.length);
	}
	out (res) {
		assert(res.length == j2-j1);
		assert(res.toString() == (this.toString())[j1..j2]);
	}
	body {
		auto r = new cord_l2!(T)();
		size_t i = 0;
		foreach (chunk; this) {
			auto i_end = i+chunk.length;
			if (i_end <= j1) {
				;
			} else if (i > j2) {
				break;
			} else if (i <= j1) {
				r ~= chunk[j1-i .. $];
			} else if (i_end > j2) {
				r ~= chunk[0 .. j2-i];
				break;
			} else {
				r ~= chunk;
			}
			i = i_end;
		}
		return r;
	}

	cord_l2!(T) opCat(cord_l2!(T) b)
	out (res) {
		assert(res.length == this.length + b.length);
	}
	body {
		auto r = this.clone();
		foreach (chunk; b) {
			r ~= chunk;
		}
		return r;
	}

	// inplace
	void opCatAssign(cord_l2!(T) b) {
		foreach (chunk; b) {
			this ~= chunk;
		}
	}

	cord_l2!(T) change(size_t j1, size_t j2, cord_l2!(T) b)
	in {
		assert(j1 <= j2);
		assert(j2 <= this.length);
	}
	out (res) {
		assert(res.length == this.length - (j2-j1) + b.length);
	}
	body {
		auto r = this[0..j1];
		r ~= b;
		r ~= this[j2..this.length];
		return r;
	}

	cord_l2!(T) change(size_t j1, size_t j2, in T[] b)
	in {
		assert(j1 <= j2);
		assert(j2 <= this.length);
	}
	out (res) {
		assert(res.length == this.length - (j2-j1) + b.length);
	}
	body {
		auto r = this[0..j1];
		r ~= b;
		r ~= this[j2..this.length];
		return r;
	}

	cord_l2!(T) remove(size_t j1, size_t j2)
	in {
		assert(j1 <= j2);
		assert(j2 <= this.length);
	}
	out (res) {
		assert(res.length == this.length - (j2-j1));
	}
	body {
		auto r = this[0..j1];
		r ~= this[j2..this.length];
		return r;
	}
}

alias cord_l2!(char) cord;

unittest {
	auto r = new cord("a");
	r ~= "xx";
	assert(r.length == 3);
	r ~= "yyy";
	assert(r.length == 6);
	r ~= "zzz";
	assert(r.length == 9);
	assert(r.toString() == "axxyyyzzz");

	r.prepend("aaxa");

	r ~= "zazz";
	r ~= "zaadz";
	r ~= "agnz";
	r ~= "cbzz";
	r ~= "zasdz";

	auto r0 = r[5..26];
	assert(r0.length == 21);
	for (int i = 0; i < r0.length; i++) {
		assert(r0[i] == r[5+i]);
	}

	auto x = new cord("ala") ~ new cord(" ma ") ~ new cord("kota");
	assert(x.length == 11);
	assert(x.toString() == "ala ma kota");
}
