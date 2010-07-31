/** Cord structure is used for fast manipulation of large strings.
 *
 * Large -- more than 10k characters. 1 GB is practical
 * limit of it usage.
 *
 * Fast -- in most usage it is O(1), for very big strings it is O(log n)
 *
 * Manipulations -- substring (slice), concatenation, append, search,
 * replace, index access, lazy reading of files are supported to be fast.
 *
 * Other: iterating, writing to files/sockets, converting to standard string
 * are O(n).
 *
 * Note: On 32-bit systems, cord_t is practically limited to 4GB, because length and indexes will overflow.
 *
 * TODO: Optimise append only mode.
 *
 * Note: Currently only char, dchar and wchar are supported. toString for non-char types are limited.
 *       If you need other typed (ie. numbers) tell me.
 *
 * Example:
 * ---
 * auto p = new CordI("The quick brown fox jumps over the lazy dog");
 * auto f = p[16 .. 19];
 * assert(f == "fox");
 * auto p2 = p[0 .. 16] ~ "cat" ~ p[19 .. p.length];
 * auto p3 = (p[16 .. 19] = "cat");
 * assert(p2 == p3 && p2[16 .. 19] == "cat");
 * auto p4 = (p[16] = 'b');
 * assert(p4[16 .. 19] == "bat");
 * 
 * auto pp = p ~ p;
 * auto ppp = pp ~ pp;
 * auto pppp = ppp ~ ppp;
 * auto ppppp = pppp ~ pppp;
 * auto pppppp = ppppp ~ ppppp;
 * auto ppppppp = pppppp ~ pppppp;
 * auto pppppppp = ppppppp ~ ppppppp;
 * auto ppppppppp = pppppppp ~ pppppppp;
 * 
 * auto x = ppppppppp[ 876123 ... 915123 ];
 * ---
 *
 * Note: UTF8 (with varying number of bytes) isn't well supported. Use dchar for simpler indexing and interating
 *
 * Note: There is few op*Assign methods, but its semantic is quite different. Actually just like op*, with possible
 *       optimalisation. They don't change underlaying data (like op*Assign), but returns new cord.
 *       (See 5th line of exampe above). Use RBCord_t for your convinience.
 *
 * Example:
 * ---
 * CordI a = new CordI();
 * CordI b = new CordI("Sun");
 * //a = "Earth"; // not allowed
 * a = new CordI("Earth");
 * //a = a ~ " " ~ b; // not allowed
 * auto a2 = a ~ " " ~ b;
 * //b = "Moon"; // not allowed
 * b = new CordI("Moon");
 * //a = a ~ " " ~ b;
 * auto a3 = a ~ " " ~ b;
 * ---
 *
 * Note: Documentation is displayed correctly in Opera and Links. Firefox and Epiphany badly interpret few parts.
 *
 * Authors: Witold Baryluk <baryluk@smp.if.uj.edu.pl
 * License: BSD
 */
module cords;

import std.stdio : writef, writefln;

/// Number of bytes in small arrays. Small arrays will be copied if not filled on append and replace and concatenation of strings operations.
static invariant size_t SHORT_LIMIT = 8;
/// ditto
static invariant size_t MAX_LEFT_LEN = 255;

/// Maximal number of tree levels befor forcing rebalancing. 48 is sufficient for 4 billion elements
static invariant size_t MAX_DEPTH = 48;

static assert(MAX_DEPTH > 2);

/// Fibonacci numbers
static invariant(size_t[MAX_DEPTH]) min_len;
/// Largest cord possible in ballanced form
static invariant size_t cord_max_len;

static this() {
	size_t prev, last;

	min_len[0] = prev = 1;
	min_len[1] = last = 2;

	for (size_t i = 2; i < MAX_DEPTH; i++) {
		size_t current = prev + last;
		if (current < last) { // overflow
			current = last;
		}
		min_len[i] = current;
		prev = last;
		last = current;
	}

	cord_max_len = last - 1;
}

template cord_t_fn(T) {
	alias T delegate(size_t i) cord_t_fn;
}

/** Binary tree based implementation, like orginal Boehm's 'cords',
 *
 * Fast for large strings, lazy reading of large files and procedural strings.
 *
 * Also known as 'ropes' in C++'s STL.
 *
 * Note: Use this type only if you want to write functional style programing, otherwise RBCord_t is easier to use.
 */
abstract class cord_t(T) {
	alias cord_t!(T) CT;

	/** Length of whole string */
	const size_t length;

	/** Depth of tree */
	protected const size_t depth;

	/// Only for informative purpose
	size_t get_depth() const {
		return depth;
	}

	/// For inherited classes usage.
	private this(const size_t length_, const size_t depth_) {
		length = length_;
		depth = depth_;
	}

	/** Returns char at position _pos. */
	abstract T opIndex(const size_t _pos) const;

	/** Returns new string which is concatenation of _this and _s.
	 *
	 * Note: If used strings isn't invariant, it is copied.
	 */
	abstract const(CT) opCat(in const(CT) s) const;
	/// ditto
	const(CT) opCat(in T s) const {
	    return this ~ cast(invariant(T)[])[s];
	}
	/// ditto
	abstract const(CT) opCat(in invariant(T)[] s) const;
	/// ditto
	const(CT) opCat_r(in T s) const {
	    return cast(invariant(T)[])[s] ~ this;
	}
	/// Also
	/// TODO: also abstract, so prepending also will use SHORT_LIMIT optimialisation
	const(CT) opCat_r(in invariant(T)[] s) const
	{
		return (new cord_t_string!(T)(s) ~ this);
	}

version(alsoconsts) {
	/// ditto
	const(CT) opCat(const(T)[] s) const {
		return opCat(s.idup);
	}
}
	// ditto
	//abstract CT opCat(invariant(T[]) s) const;

	/// Returns whole cord
	const(CT) opSlice() const {
		return this;
	}

	/** Returns substring.
	 *
	 * ---
	 * auto p = new cord_t("The quick brown fox jumps over the lazy dog");
	 * auto f = p[16 .. 19];
	 * assert(f == "fox");
	 * ---
	 */
	abstract const(CT) opSlice(const size_t _begin, const size_t _end) const;

	/** Returns string with replaced substring _mid: */
	const(CT) opSliceAssign(const T mid, const size_t _begin, const size_t _end) const
	{
		return opSliceAssign([mid], _begin, _end);
	}

	/** Returns string with replaced substring _mid:
	 *
	 * ---
	 * auto p = new cord_t("The quick brown fox jumps over the lazy dog");
	 * auto r = (p[16 .. 19] = "firefox");
	 * assert(r == "The quick brown firefox jumps over the lazy dog");
	 * ---
	 *
	 * Note: Orginal string isn't changed! If assigned string isn't invariant,
	 * it is copied.
	 */
	const(CT) opSliceAssign(in invariant(T)[] mid, const size_t _begin, const size_t _end) const
	in {
		assert(_begin <= _end);
		assert(_end <= this.length);
	}
//	out (res) {
//		version (paranoid) {
//		assert(res.length == mid.length + _begin + (this.length-_end));
//		}
//	}
	body {
		return this[0 .. _begin] ~ mid ~ this[_end .. this.length];
	}
	unittest {
		auto a = new cord_t_string!(char)("The quick brown fox jumps over the lazy dog\n");

		const(cord_t!(char)) t(const(cord_t!(char)) a, size_t l,
			size_t t1, size_t t2, in char[] t,
			size_t r1, size_t r2, in char[] r,
			size_t nl,
			in char[] c
			)
		{
			assert(t2-t1 == t.length);
			assert(nl == l - t.length + r.length);

			assert(a[t1 .. t2] == t);
			assert(a.length == l);
			auto b = (a[r1 .. r2] = r.idup);
			assert(b.length == nl);
			assert(c.length == nl);
			if (r.length) {
				assert(b[r1 .. r1 + r.length] == r);
			}
			assert(a[t1 .. t2] == t);
			assert(b == c);
			return b;
		}

		// 21 tests = 7 (prepend, start, middle, end, append, whole, middle empty) with 3 (small, large, empty)

		// middle, replace nonempty with same length
		t(a, 44,
			16, 19, "fox",
			16, 19, "cat",
			44,
			"The quick brown cat jumps over the lazy dog\n");

		// middle, replace nonempty with larger
		t(a, 44,
			16, 19, "fox",
			16, 19, "girraffe",
			49,
			"The quick brown girraffe jumps over the lazy dog\n");

		// middle, replace nonempty with smaller
		t(a, 44,
			16, 19, "fox",
			16, 19, "X",
			42,
			"The quick brown X jumps over the lazy dog\n");

		// middle, replace nonempty with empty
		auto b1 = t(a, 44,
			16, 19, "fox",
			16, 19, "",
			41,
			"The quick brown  jumps over the lazy dog\n");
		assert(b1[14 .. 18] == "n  j");

		// start, replace nonepty with longer
		t(a, 44,
			0, 3, "The",
			0, 3, "This",
			45,
			"This quick brown fox jumps over the lazy dog\n");

		// start, replace nonepty with smaller
		t(a, 44,
			0, 3, "The",
			0, 3, "A",
			42,
			"A quick brown fox jumps over the lazy dog\n");

		// start, replace nonepty with empty
		t(a, 44,
			0, 4, "The ",
			0, 4, "",
			40,
			"quick brown fox jumps over the lazy dog\n");

		// degenerated start, prepend
		auto b2 = t(a, 44,
			0, 0, "",
			0, 0, "Now ",
			48,
			"Now The quick brown fox jumps over the lazy dog\n");
		assert(b2[0 .. 7] == "Now The");

		// end, replace nonepty with longer
		t(a, 44,
			40, 44, "dog\n",
			40, 44, "dogs and cats\n",
			54,
			"The quick brown fox jumps over the lazy dogs and cats\n");

		// end, replace nonepty with empty
		t(a, 44,
			40, 44, "dog\n",
			40, 44, "",
			40,
			"The quick brown fox jumps over the lazy ");

		// degenerated end, append
		t(a, 44,
			44, 44, "",
			44, 44, " and cats\n",
			54,
			"The quick brown fox jumps over the lazy dog\n and cats\n");
	}

	/// ditto
	const(CT) opSliceAssign(in const(CT) mid, const size_t _begin, const size_t _end) const
	in {
		assert(_begin <= _end);
		assert(_end <= this.length);
	}
//version(outbug) {
//	out (res) {
//		version (paranoid) {
//		assert(res.length == mid.length + _begin + (this.length-_end));
//		}
//	}
//}
	body {
		return this[0 .. _begin] ~ mid ~ this[_end .. this.length];
	}


	/** Returns string with replaced character _pos by character mid
	 *
	 * TODO: More optimal (change only single leaf, preserve structure of tree)
	 * Bugs: Not tested well (edge cases can fail)
	 */
	const(CT) opIndexAssign(const T mid, const size_t _pos) const
	in {
		assert(_pos < this.length);
	}
//version(outbut) {
//	out (res) {
//		version (paranoid) {
//		assert(res.length == this.length);
//		}
//	}
//}
	body {
		return this[0 .. _pos] ~ mid ~ this[_pos+1 .. this.length];
	}

	/** Returns string with replaced character _pos by mid
	 *
	 * TODO: More optimal
	 * Bugs: Not tested
	 */
	const(CT) opIndexAssign(in invariant(T)[] mid, const size_t _pos) const
	in {
		assert(_pos < this.length);
	}
//version(outbut) {
//	out (res) {
//		version (paranoid) {
//		assert(res.length == mid.length + this.length - 1);
//		}
//	}
//}
	body {
		return this[0 .. _pos] ~ mid ~ this[_pos .. this.length];
	}

	/// ditto
	const(CT) opIndexAssign(in const(CT) mid, const size_t _pos) const
	in {
		assert(_pos < this.length);
	}
//version(outbug) {
//	out (res) {
//		version (paranoid) {
//		assert(res.length == mid.length + this.length - 1);
//		}
//	}
//}
	body {
		return this[0 .. _pos] ~ mid ~ this[_pos .. this.length];
	}


version(alsoconsts) {
	/// ditto
	CT opSliceAssign(in const(T)[] mid, size_t _begin, size_t _end) const {
		opSliceAssign(mid.idup, _begin, _end);
	}
}

	/** Iterates over string in chunks of some (variable) size.
	 *
	 * ---
	 * auto p = new cord_t("The quick brown fox jumps over the lazy dog");
	 * auto reqest = "SEND: " + p + "\n";
	 * foreach (chunk; request) {
	 *     socket.write(chunk);
	 * }
	 * ---
	 *
	 * Note: You MUST NOT change chunk variable.
	 */
	int opApply(int delegate(inout invariant(T[])) dg) const;
	//int opApply_r(int delegate(inout invariant(T[])) dg) const;

	//int opApply(int delegate(inout T) dg) const;
	//int opApply_r(int delegate(inout T) dg) const;

	/** Converts to standard D string.
	 *
	 * ---
	 * auto p = new cord_t("The quick brown fox jumps");
	 * auto p2 = p + " over the lazy dog";
	 * writefln("%s", p2.toString());
	 * ---
	 *
	 * Note: Usage of opApply (using foreach) is recommended, because this function uses lots of memory and copying, still it is O(n).
	 * Note: If string is a pure string (single leaf), coping isn't necassary. You MUST NOT change content of it.
	 * Note: Should be sufficient
	 * Note: We are using here some not very safe casts. I think they are reasonable
	 */
	final invariant(T)[] toArray() const
	out (res) {
		assert(res.length == this.length);
	}
	body {
		auto t = cast(cord_t_string!(T))this;
		if (t !is null) {
			return cast(invariant(T)[])t.str;
		}
		T[] ret;
		ret.length = this.length;
		toBuffer!(T)(ret);
		return cast(invariant(T)[])ret;
	}

	static if (is(T == char)) {
	/// ditto
	final override invariant(char)[] toString() const
	out (res) {
		assert(res.length == this.length);
	}
	body {
		auto t = cast(cord_t_string!(T))this;
		if (t !is null) {
			return cast(invariant(char)[])t.str;
		}
		T[] ret;
		ret.length = this.length;
		toBuffer!(T)(ret);
		return cast(invariant(char)[])ret;
	}
	}

	/** Converts to standard D string, writing to supplied buffer
	 *
	 * Note: See toString
	 * Note: caller need to ensure that buffer is big enaugh
	 */
	final void toBuffer(ref T[] buf) const {
		size_t i = 0;
		foreach (chunk; this) {
			buf[i .. i+chunk.length] = chunk;
			i += chunk.length;
		}
	}

	/** Prints string to standard output using writef. Prints line end.*/
	void print() const {
		foreach (leaf_chunk; this) {
			writef("%s", leaf_chunk);
		}
		writefln();
	}

	/** Prints internal structure of string */
	//protected
	abstract void print_structure(const size_t n = 0) const;

// use murmur

	/** We are using MurmurHash 2.0 scheme here
	 *
	 * Hash value is computed only based on value of string,
	 * it is independed of the structure of the tree.
	 *
	 * This version is quite fast on Intel processors,
	 *
	 * It is based on endian and alligment neutral version of MurmurHash 2.0
	 * because handling of allignment of multiple buffers in cords
	 * is very hard. Mayby some day I will implement Bob Jenkins' lookup3.
	 *
	 * Do NOT use for cryptographic puposes.
	 *
	 * Note: Hash value will be different than hash of this.toString(),
	 *       because we are using different hash function (better :D)
	 *       Use (cord_t_string!(T)(x)).toHash(), or Cord(x).toHash()
	 *       to calculate hash value of standard string.
	 *
	 * TODO: only char is currently supported.
	 *
	 * URL: http://murmurhash.googlepages.com/
	 */
/+	final hash_t toHash() const {
		const uint m = 0x5bd1e995; // magic number
		const uint r = 24; // magic number
		const uint seed = 0xd728bc41; // just a random number

		uint h = seed ^ length;

		size_t len = length;

		auto it = this.getIterator();

		// the most important point is that mix2{a,b,c} can start calculating
		// without waiting for mix1{a,b} from previous step
		// allowing for paralllel multiplcation and mixing
		// this leads to double speed on Intel Core
		while (len >= 4) {
			uint k = (cast(ubyte)(it.fetch_and_inc()));
			k |= (cast(ubyte)(it.fetch_and_inc())) << 8;
			k |= (cast(ubyte)(it.fetch_and_inc())) << 16;
			k |= (cast(ubyte)(it.fetch_and_inc())) << 24;

			k *= m; // mix2a
			k ^= k >> r; // mix2b
			k *= m; // mix2c

			h *= m; // mix1a
			h ^= k; // mix1b

			len -= 4;
		}

		ubyte d0, d1, d2;

		if (len >= 1) {
			d0 = (cast(ubyte)(it.fetch_and_inc()));
			if (len >= 2) {
				d1 = (cast(ubyte)(it.fetch_and_inc()));
				if (len >= 3) {
					d2 = (cast(ubyte)(it.fetch_and_inc()));
				}
			}
		}

		switch (len) {
			case 3:
				h ^= d2 << 16;
			case 2:
				h ^= d1 << 8;
			case 1:
				h ^= d0;
				h *= m;
		}

		h ^= h >> 13;
		h *= m;
		h ^= h >> 15;

		return cast(hash_t)h;
	}
+/

/+
	/** We are using Bob Jenkin's lookup3.c scheme.
	 *
	 * Hash value is computed only based on value of string,
	 * it is independed of the structure of the tree.
	 *
	 * We are using here function hashlittle() (it is fast on little-endian machines,
	 * like Intel and AMD) which returns 32-bit hash.
	 *
	 * For big-endian or 64bit machines (or if you just want 64 bit hash)
	 * this need to be changed.
	 *
	 * Do NOT use for cryptographic puposes.
	 *
	 * lookup3 is very hackish and tricky, this code is complete hardcore
	 * bit and aligment hack. Managing multiple unaligned and discontinued
	 * buffers, and keep this fast is really hard. Hope there is no errors.
	 *
	 * TODO: Investigate other hashes. MurmurHash is faster, but needs fast multiplication
	 *
	 * URL: http://burtleburtle.net/bob/c/lookup3.c
	 */
	hash_t toHash() const {
		uint rot(uint x, int k) { return (x<<k) | (x>>(32-k)); }
		void mix(ref uint a, ref uint b, ref uint c) {
			a -= c;  a ^= rot(c,  4);  c += b;
			b -= a;  b ^= rot(a,  6);  a += c;
			c -= b;  c ^= rot(b,  8);  b += a;
			a -= c;  a ^= rot(c, 16);  c += b;
			b -= a;  b ^= rot(a, 19);  a += c;
			c -= b;  c ^= rot(b,  4);  b += a;
		}
		byte fin(ref uint a, ref uint b, ref uint c) {
			c ^= b; c -= rot(b, 14);
			a ^= c; a -= rot(c, 11);
			b ^= a; b -= rot(a, 25);
			c ^= b; c -= rot(b, 16);
			a ^= c; a -= rot(c,  4);
			b ^= a; b -= rot(a, 14);
			c ^= b; c -= rot(b, 24);
		}
		const uint initval = 0x98a7d3acu; // any number
		uint a, b, c;
		a = b = c = 0xdeadbeefu + cast(uint)len + initval;
		const(uint)* k;

		opApplyThreeUInts(int (nexta, nextb, nextc, l) {
			if (l == 12) {
				a += nexta;
				b += nextb;
				c += nextc;
				mix(a, b, c);
				return 0;
			} else { // last 3 bytes
				// we are relaying here on iterator
				c += nextc;
				b += nextb;
				a += nexta;

				/*
				switch (l) {
					case 12:
						c += nextc;
						b += nextb;
						a += nexta;
						break;
					case 11:
						c += nextc & 0xffffff;
						b += nextb;
						a += nexta;
						break;
					case 10:
						c += nextc & 0xffff;
						b += nextb;
						a += nexta;
						break;
					case 9:
						c += nextc & 0xff;
						b += nextb;
						a += nexta;
						break;
					case 8:
						b += nextb;
						a += nexta;
						break;
					case 7:
						b += nextb & 0xffffff;
						a += nexta;
						break;
					case 6:
						b += nextb & 0xffff;
						a += nexta;
						break;
					case 5:
						b += nextb & 0xff;
						a += nexta;
						break;
					case 4:
						a += nexta;
						break;
					case 3:
						a += nexta & 0xffffff;
						break;
					case 2:
						a += nexta & 0xffff;
						break;
					case 1:
						a += nexta & 0xff;
						break;
					case 0:
						break; // zero length strings require no mixing
				}
				*/
				
				return 1;
			}
		}

		// final
		fin(a, b, c);
		
		return cast(hash_t)c; /// TODO: c have only 32bis, also on 64bit machines
	}


	/** Iterator which tries to quickly iterate over leafs, giving set of 3 32-bit integers
	 *
	 * It's used for hashing.
	 *
	 * It can be slow for unaligned read.
	 *
	 * On little endian machines, layout of characters in a, b, c is as follows:
	 *
	 * a  4  3  2  1
	 * b  8  7  6  5
	 * c 12 11 10  9
	 *
	 * On big endian
	 *
	 * a  1  2  3  4
	 * b  5  6  7  8
	 * c  9 10 11 12
	 *
	 * Last dg call will be with bytes != 12, and relevant parts of a, b, c will be zero.
	 */
	int opApplyThreeUInts(int delegate(ubyte offset, uint a_, uint b_, uint c_, ubyte bytes_) dg) const {
		const(uint)* k;
		foreach (chunk; this) {
			k = chunk.ptr;
			len = chunk.length;
			while (len > 12) { // read 12 bytes if possible
				dg(0, k[0], k[1], k[2], 12);
				len -= 12;
				k += 3;
			}
			
		}
	}
+/

	/** Returns possibly new string with ballanced internal structure if it is not ballanced enaught.
	 *
	 * You dont need to call this function manually, because all functions (especially opCat) already call it.
	 */
	protected const(CT) balance() const
	{
		//if (depth < MAX_DEPTH) {
			return this;
		//} else {
		//	return this.force_balance();
		//}
	}

	/** Returns reblanced tree.
	 *
	 * Use it only if you know that after now you will need frequent reading of cord (for example after reading whole file from disk).
	 */
	protected const(CT) force_balance() const
	{
		static int c;
		c++;
		if (c % 100 == 0) {
			writefln("Balancing %d", c);
		}
		size_t len = length;
		Forest!(T) forest = new Forest!(T)(len);
		forest.insert(this);
		return forest.concat(len);
	}

	/** Returns iterator object, which can be used in foreach or in manual traversing of string. */
	cord_t_iterator!(T) getIterator(const size_t _start = 0) const
	in {
		assert(_start <= length);
	}
	body {
		return new cord_t_iterator!(T)(this, _start);
	}

	/** Comparision functions */
	abstract override bool opEquals(Object) const;
	/// ditto
	abstract override int opCmp(Object) const;
	/// ditto
	abstract bool opEquals(in T[]) const;
	/// ditto
	abstract int opCmp(in T[]) const;

	/** Comparission function opEquals
	 *
	 * Note: It works be iterating concurently two cords
	 * TODO: There are still possible some optimalisations (like comparring for common subtrees)
	 */
	static bool equals(const(CT) l, const(CT) r, const bool ignorecases = false) {
		if (l is r) {
			return true;
		}

		if (l.length != r.length) {
			return false;
		}

		auto li = l.getIterator();
		auto ri = r.getIterator();

		size_t curpos;
		size_t length = l.length;

		while (curpos < length) {
			if (li.fetch_and_move() != ri.fetch_and_move()) {
				return false;
			}
			curpos++;
		}
/+
		assert(length == l.length && length == r.length);
		size_t lleafi, rleafi;
		invariant(T)[] lleaf = li.fetch_leaf(lleafi);
		assert(lleafi == 0);
		invariant(T)[] rleaf = li.fetch_leaf(rleafi);
		assert(rleafi == 0);

		while(curpos < length) {
			if (lleaf.length < rleaf.length) {
				return false;
			}
		}
+/
		return true;
	}

	/// Same as above, but for opCmp
	static int cmp(const(CT) l, const(CT) r, const bool ignorecases = false) {
		return 0;
	}

	/* str.file functions implementations */
	//int write(T c);

	/* std.string functions implementations */
	//int strchr(T c);
	//int strrchr(T c);
	//int find(T[] s);
	//int kmp_find(T[] s);
	//bool opCmp(T2)(in T1[] s2) const;
	//bool opCmp(in CT s2) const;
	//CT[] split(string s) const;
	//CT join(CT[], string s) const;
	//CT join(CT[], CT s) const;
	//CT strip() const;
	//CT stripl() const;
	//CT stripr() const;
	//CT chomp() const;
	//CT chop() const;
	//CT replace(CT, CT s) const;
	//CT replace(string s, CT s) const;
	//CT replace(string s, string s) const;
	//CT replace(CT s, string s) const;
	//int count(string s) const;
	//int count(CT s) const;

	/* str.regexp */
}


import RBt : RB, RBMixin;

/** Private: Forest element is used internally by Forest */
private class ForestElement(T) {
	alias cord_t!(T) CT;

	const(CT) c; /// tree
	const size_t len; /// actual length of c
	/// trivial constuctor
	this(const(CT) c_, const size_t len_) {
		c = c_;
		len = len_;
	}
}

/** Private: Forest class is used when ballancing ocurs */
private final class Forest(T) {
	alias cord_t!(T) CT;
	alias cord_t_empty!(T) CTE;
	alias cord_t_concatenation!(T) CTC;

	alias RB!(const(ForestElement!(T))) FET;

	/// Internal member
	FET[MAX_DEPTH] forest = void; /// list of trees

	/// Internal function
	this(const size_t max_len) {
		for (size_t i = 0; i < MAX_DEPTH; i++) {
			forest[i] = FET(new ForestElement!(T)(null, 0));
			if (min_len[i] > max_len) {
				return;
			}
		}
		writefln(MAX_DEPTH, " ", max_len);
		throw new Exception("Cord to big");
	}

	/// Internal function
	private void add(const(CT) x, const size_t len)
	in {
		assert(x.length == len);
	}
	body {
		size_t i = 0;
		alias RB!(const(CT)) RCT;
		auto sum = RCT(new CTE());
		size_t sum_len = 0;

		while (len > min_len[i+1]) {
			if (forest[i].c) {
				sum = forest[i].c ~ sum.opDot();
				sum_len += forest[i].len;
				forest[i] = FET(new ForestElement!(T)(null, forest[i].len));
				//forest[i].c = null;
			}
			i++;
		}

		sum = sum ~ x;
		sum_len += len;

		while (sum_len >= min_len[i]) {
			if (forest[i].c) {
				sum = forest[i].c ~ sum.opDot();
				sum_len += forest[i].len;
				forest[i] = FET(new ForestElement!(T)(null, forest[i].len));
				//forest[i].c = null;
			}
			i++;
		}
		assert(i != 0);
		i--;

		forest[i] = FET(new ForestElement!(T)(sum.opDot(), sum_len));
	}

	/// Internal function
	private const(CT) concat(size_t expected_size) const
//version(outbug) {
//	out (res) {
//		assert(res.length == expected_size);
//	}
//}
	body {
		size_t i = 0;
		alias RB!(const(CT)) RCT;
		auto sum = RCT(new CTE());
		size_t sum_len = 0;

		while (sum_len != expected_size) {
			if (forest[i].c !is null) {
				sum = forest[i].c ~ sum.opDot();
				sum_len += forest[i].len;
			}
			i++;
		}
		return sum.opDot();
	}

	/// Internal function
	void insert(const(CT) x) {
		size_t depth;
		auto x2 = cast(CTC)x;
		if (x2 && ((depth = x2.depth) >= MAX_DEPTH || x2.length < min_len[depth])) {
			insert(x2.left);
			insert(x2.right);
		} else { // string, function or balanced
			add(x, x.length);
		}
	}
}

/** Structure for storing leafs as real (standard D arrays) strings. They are limited in size to SHORT_LIMIT (with
 * exception to the strings which was passed bigger as standard D strings on creation).
 */
final class cord_t_string(T) : cord_t!(T) {
	alias cord_t!(T) CT;
	alias cord_t_string!(T) CTS;
	alias cord_t_concatenation!(T) CTC;
	alias cord_t_empty!(T) CTE;

	private invariant(T)[] str;

// tu tez jest bug z
// cord_t.d(990): Error: variable cord_t.cord_t_string!(char).cord_t_string.opCat.__result cannot modify const
// bo invarianty sa wstawiene przed i po
//	invariant() {
//		assert(this.depth == 0);
//		assert(this.length == str.length);
//	}

	/** Create empty string. Usefull when starting creation of string.
	 *
	 * Note: Use cord_t_empty instead of this
	 */
	this() {
		super(0, 0);
		str.length = 0;
	}

	/** Create string with content of string _s.
	 *
	 * Note: Because _s is invariant it is not copied.
	 */
	this(in invariant(T)[] s) {
		super(s.length, 0);
		str = s;
	}

	/** Create string with concatenated content of strings _s1 and _s2.
	 *
	 * Note: Should be used only internally (when s1.length + s2.length is small),
	 * for normal operation use ~ operators!
	 * Note: Implementation uses trick for not doing additionall recopying of string (s1~s2) when changing to invariant.
	 */
	this(in const(T)[] s1, in const(T)[] s2) {
		super(s1.length + s2.length, 0);
		version(old) {
			str = (s1 ~ s2).idup;
		}
		T[] temp = new T[s1.length + s2.length]; /// how to not initialize data here?, where to put void?
		temp[0 .. s1.length] = s1;
		temp[s1.length .. $] = s2;
		str = cast(invariant(T)[])temp;
	}

	/** See cord_t interface for description.
	 *
	 * Note: Returns tree if lenght of resulting target array (and coping operation)
	 * will be larger that SHORT_LIMIT.
	 *
	 * TODO: Doen't triger balancing
	 */

	override const(CT) opCat(in invariant(T)[] s) const
//version(outbug) {
//	out (res) {
//		version(paranoid) {
//		assert(res.length == this.length + s.length);
//		}
//	}
//}
	body {
		if (this.length == 0) {
			return new CTS(s);
		}
		if (s.length == 0) {
			return this;
		}

		size_t result_len = this.length + s.length;
		if (result_len <= SHORT_LIMIT) {
			return new CTS(this.str, s);
		} else {
			auto ss = new CTS(s);
			return (new CTC(this, ss)).balance();
		}
	}

	/+
	override CT opCat_r(in T[] s) const {
		return (new CTS(s) ~ this);
	}
	+/

	/** See cord_t interface for description.
	 *
	 * Note: If concatenated cord is real string (leaf) it above function will be used.
	 * Else internal node will be created, and optional balancing started.
	 */
	override const(CT) opCat(const(CT) s) const
//	out (res) {
//		version(paranoid) {
//		assert(res.length == this.length + s.length);
//		}
//	}
	body {
		if (this.length == 0) {
			return s;
		}
		if (s.length == 0) {
			return this;
		}

		auto ss = cast(CTS)s;
		if (ss) {
			return this.opCat(ss.str); /// Optimise: above two checks, are rechecked here
		} else {
			return (new CTC(this, s)).balance();
		}
	}

	/** See cord_t interface for description. */
	override T opIndex(const size_t pos_) const
	in {
		assert(pos_ < this.length);
	}
	body {
		return str[pos_];
	}

	/** Returns substring from string cord. D slicing is used, so it is very fast.
	 * It is also safe because strings are invariant any way.
	 */
	override const(CT) opSlice(const size_t _begin, const size_t _end) const
	in {
		assert(_begin <= _end);
		assert(_end <= this.length);
	}
//	out (res) {
//		version(paranoid) {
//		assert(res.length == _end - _begin);
//		}
//	}
	body {
		if (_end != _begin) {
			return new CTS(str[_begin .. _end]);
		} else {
			return new CTE();
		}
	}

	/** See cord_t interface for description. */
	override int opApply(int delegate(inout invariant(T[])) dg) const {
		return dg(str);
	}

	/** See cord_t interface for description. */
	override void print_structure(const size_t n = 0) const {
		for (int i = 0; i < n; i++) writef("  ");
		auto l = (length < 20 ? length : 20);
		writefln("Str: %s%s (len: %d)", str[0 .. l], (l == length ? "" : "..."), length);
	}

	/** Comparissions */
	override bool opEquals(Object o) const {
		auto c = cast(CT)o;
		if (c) {
			return equals(this, c);
		}
		throw new Exception("not comparable");
	}

	/// ditto
	override int opCmp(Object o) const {
		auto c = cast(CT)o;
		if (c) {
			return cmp(this, c);
		}
		throw new Exception("not comparable");
	}

	/// ditto
	override bool opEquals(in T[] c) const {
		return equals(this, new CTS(cast(invariant(T)[])c));
	}

	/// ditto
	override int opCmp(in T[] c) const {
		return cmp(this, new CTS(cast(invariant(T)[])c));
	}
}

/** Helper function */
size_t max(const size_t a, const size_t b) {
	return (a > b ? a : b);
}


import core.exception : RangeError;

/** Empty cord.
 *
 * All operations are trivial.
 */
final class cord_t_empty(T) : cord_t!(T) {
	alias cord_t!(T) CT;
	alias cord_t_empty!(T) CTE;
	alias cord_t_string!(T) CTS;

	/** Empty cord */
	this() {
		super(0, 0);
	}

// bug compilator
//	invariant() {
//		assert(length == 0);
//		assert(depth == 0);
//	}
//
	/** Returns cord for s */
	override const(CT) opCat(in invariant(T)[] s) const
//	out (res) {
//		version(paranoid) {
//		assert(res.length == this.length + s.length);
//		assert(res.length == s.length);
//		}
//	}
	body {
		return new CTS(s);
	}

	/// ditto
	//override
	const(CT) opCat(CTS y) const
//	out (res) {
//		version(paranoid) {
//		assert(this.length + y.length == res.length);
//		assert(y.length == res.length);
//		}
//	}
	body {
		return y;
	}

	/// ditto
	override const(CT) opCat(const(CT) y) const
//	out (res) {
//		version(paranoid) {
//		assert(this.length + y.length == res.length);
//		assert(y.length == res.length);
//		}
//	}
	body {
		return y;
	}

	/** See cord_t interface for description.
	 *
	 * Note: Throws error
	 */
	override T opIndex(const size_t _pod) const
	{
		throw new RangeError(__FILE__, __LINE__);
	}

	/** See cord_t interface for description.
	 *
	 * Note: Only c[0 .. 0]; is valid call
	 */
	override const(CT) opSlice(const size_t _begin, const size_t _end) const
	in {
		assert(_begin <= _end);
		assert(_end <= this.length);
	}
//	out (res) {
//		version(paranoid) {
//		assert(res.length == _end - _begin);
//		}
//	}
	body {
		if (_begin == 0 && _end == 0) {
			return this;
		}
		throw new RangeError(__FILE__, __LINE__);
	}

	/** See cord_t interface for description.
	 *
	 * Note: Does nothing
	 */
	override int opApply(int delegate(inout invariant(T[])) dg) const {
		return 0;
	}

	/** See cord_t interface for description. */
	override void print_structure(const size_t n = 0) const {
		for (int i = 0; i < n; i++) writef("  ");
		writefln("Empty: (len: %d, depth: %d)", length, depth);
	}	

	/** Comparissions */
	override bool opEquals(Object o) const {
		auto c = cast(cord_t!(char))o;
		if (c) {
			return (c.length == 0);
		}
		throw new Exception("not comparable");
	}

	/// ditto
	override int opCmp(Object o) const {
		auto c = cast(cord_t!(char))o;
		if (c) {
			return (c.length == 0 ? 0 : -1);
		}
		throw new Exception("not comparable");
	}

	/// ditto
	override bool opEquals(in T[] c) const {
		return (c.length == 0);
	}

	/// ditto
	override int opCmp(in T[] c) const {
		return 1; /// TODO: value is correct?
	}
}


/** Structure for storing internal nodes of tree.
 *
 * Note: Don't use this manually. Use ~ operator for concatenation, and foreach for iterations!
 * Implementation can change without notice.
 */
final class cord_t_concatenation(T) : cord_t!(T) {
	alias cord_t!(T) CT;
	alias cord_t_concatenation!(T) CTC;
	alias cord_t_string!(T) CTS;
	alias cord_t_empty!(T) CTE;

	private const(CT) left;
	private const(CT) right;

version (costlyinvariants) {
	bool inv = true;
	invariant() {
		if (inv) {
			inv = false;
			assert(left !is null);
			assert(right !is null);
			assert(left.length + right.length == length); 
			assert(depth == 1 + max(left.depth, right.depth));
			inv = true;
		}
	}
}

	/** Concatenation of two cord nodes
	 *
	 * Note: Subject to change. Don't use this manually. Use ~ operator.
	 */
	this(in const(CT) left_, in const(CT) right_) {
		super(left_.length + right_.length, max(left_.depth, right_.depth) + 1);
		left = left_;
		right = right_;
	}

	/** See cord_t interface for description.
	 *
	 * Note: Triggers optional balancing.
	 */
	override const(CT) opCat(in invariant(T)[] s) const
//	out (res) {
//		version(paranoid) {
//		assert(res.length == this.length + s.length);
//		}
//	}
	body {
		auto this_len = this.length;
		if (this_len == 0) {
			return new CTS(s);
		}
		if (s.length == 0) {
			return this;
		}


		// if right subtree is real array, and it and new string are short, then create new array and substitute right subtree by it
		auto rs = cast(CTS)right;
		if (rs && s.length <= SHORT_LIMIT/2 && rs.length + s.length <= SHORT_LIMIT) {
			CT temp = new CTS(rs.str, s);
			return (new CTC(left, temp)).balance();
		}
		// for large catenated strings, or when right subtree is real tree, do lazy
		auto ss = new CTS(s);
		return (new CTC(this, ss)).balance();
	}

	/// ditto
	//override
	const(CT) opCat(const(CTS) y) const
//	out (res) {
//		version(paranoid) {
//		assert(this.length + y.length == res.length);
//		}
//	}
	body {
		auto this_len = this.length;
		if (this_len == 0) {
			return y;
		}
		if (y.length == 0) {
			return this;
		}
		return this.opCat(y.str);
	}

	/// ditto
	override const(CT) opCat(const(CT) y) const
//	out (res) {
//		version(paranoid) {
//		assert(this.length + y.length == res.length);
//		}
//	}
	body {
		auto this_len = this.length;
		if (this_len == 0) {
			return y;
		}
		if (y.length == 0) {
			return this;
		}
		return (new CTC(this, y)).balance();
	}

	/** See cord_t interface for description.
	 *
	 * Note: Uses tail recursion.
	 */
	override T opIndex(const size_t pos_) const
	in {
		assert(pos_ < this.length);
	}
	body {
		if (pos_ < left.length) {
			return left.opIndex(pos_);
		}
		return right.opIndex(pos_ - left.length);
	}

	/** See cord_t interface for description.
	 *
	 * Note: Uses recursion for slicing of substrees.
	 *
	 * Note: Doesn't trigger balancing. Slicing whole string ( s[0..$] )
	 * will create duplicated object, but other edge cases are optimal.
	 */
	override const(CT) opSlice(const size_t _begin, const size_t _end) const
	in {
		assert(_begin <= _end);
		assert(_end <= this.length);
	}
//	out (res) {
//		version(paranoid) {
//		assert(res.length == _end - _begin);
//		}
//	}
	body {
		if (_begin >= left.length) {
			if (_end-_begin == right.length) {
				return right;
			} else {
				return right[_begin-left.length .. _end-left.length];
			}
		} else if (_end <= left.length) {
			if (_end-_begin == left.length) {
				return left;
			} else {
				return left[_begin .. _end];
			}
		} else {
			return
				(_begin == 0 ? left : left[_begin .. left.length])
				~
				(_end == this.length ? right : right[0 .. _end-left.length]);
		}
	}

	/** See cord_t interface for description.
	 */
	override int opApply(int delegate(inout invariant(T[])) dg) const {
		int res = left.opApply(dg);
		if (res) return res;
		res = right.opApply(dg);
		return res;
	}

	/** See cord_t interface for description. */
	override void print_structure(const size_t n = 0) const {
		for (int i = 0; i < n; i++) writef("  ");
		writefln("Concat: (len: %d, depth: %d)", length, depth);
		left.print_structure(n+1);
		right.print_structure(n+1);
	}

	/** Comparissions */
	override bool opEquals(Object o) const {
		auto c = cast(CT)o;
		if (c) {
			return equals(this, c);
		}
		throw new Exception("not comparable");
	}

	/// ditto
	override int opCmp(Object o) const {
		auto c = cast(CT)o;
		if (c) {
			return cmp(this, c);
		}
		throw new Exception("not comparable");
	}

	/// ditto
	override bool opEquals(in T[] c) const {
		return equals(this, new CTS(cast(invariant(T)[])c));
	}

	/// ditto
	override int opCmp(in T[] c) const {
		return cmp(this, new CTS(cast(invariant(T)[])c));
	}
}

/** Structure for storing lazy transformations of data. */
final class cord_t_function(T) : cord_t!(T) {
	private const(cord_t_fn) fn;
	private void* client_data;
//assert(c.func.fn !is null);
}

/** Structure for storing lazy content of big files. */
final class cord_t_file(T) : cord_t!(T) {
	private const int fd;
}



/** Iterator over cord
 *
 * TODO: this is big! very big (about 1KB) use inteligent caching like c++ stl ropes
 *
 * You can iterate char by char, or leaf by leaf
 *
 * Bugs: You can't intermix currently iteration modes
 */
final class cord_t_iterator(T) {
	alias cord_t!(T) CT;
	alias cord_t_concatenation!(T) CTC;
	alias cord_t_string!(T) CTS;

	private size_t cur_pos;
	private byte path_len;
	private invariant(T)[] cur_leaf; // current leaf, or pointer to some valid buffer
	private size_t cur_start, cur_end; // start and end position of cur_leaf

	struct pe {
		const(CT) pe_cord;
		size_t pe_start_pos;
	};
	private pe[] path;

	this(const(CT) _c, size_t _start = 0) {
		path.length = _c.depth + 1;
		path[0] = pe(_c, 0);
		set_index(_start);
	}

	/** Returns current position of iterator. Position is 0-based. */
	size_t get_index() const {
		return cur_pos;
	}

	/** Change current position of iterator.
	 *
	 * Note: Changing beyond of string lenght is undefined.
	 */
	void set_index(const size_t _newstart) {
		path_len = 0;
		cur_pos = _newstart;
		extend_path();
	}

	private const(CT) to_cord() const
	{
		return path[0].pe_cord;
	}

	// start: pe contains a prefix of the path to cur_pos
	// end: extended to full path, and setup leaf info
	// throw exception if path the end
	private void extend_path() {
		//cord_t_string!(T) s = cast(cord_t_string!(T))leaf;
		pe* cur_pe = &(path[path_len]);
		alias RB!(const(CT)) RCT;
		RCT top = cur_pe.pe_cord;
		size_t pos = cur_pos;
		size_t top_pos = cur_pe.pe_start_pos;
		size_t top_len = top.length;

		auto top_c = cast(CTC)top;

		/* Fill in the rest of path */
		while (top_c) { // while it is concatenation
			size_t left_len = top_c.left.length;
			cur_pe++;
			if (pos >= top_pos + left_len) {
				cur_pe.pe_cord = pe((top = top_c.right), (top_pos = top_pos + left_len));
				top_len -= left_len;
			} else {
				cur_pe.pe_cord = pe((top = top_c.left), top_pos);
				top_len = left_len;
			}
			path_len++;

			top_c = cast(CTC)top;
		}

		auto top_s = cast(CTS)top;
		if (top_s) {
			cur_leaf = top_s.str;
			cur_start = top_pos;
			cur_end = top_pos + top_len;
		} else {
			throw new Exception("Function leaf not implemented");
			//cur_end = 0;
		}
		if (pos >= top_pos + top_len) {
			throw new Exception("Cord bound exception");
		}
	}

	/** Returns lenght of string */
	size_t length() const {
		return path[0].pe_cord.length;
	}

	/** Tests if it is valid position. */
	bool isValid() const {
		return (cur_pos < length);
	}

	/** Tests if it is last position. */
	bool isLast() const {
		return (cur_pos == length-1);
	}


	/** Returns char from current position of iterator */
	T fetch() const
	in {
		assert(cur_pos < length);
	}
	body {
		return (cur_end != 0 ? cast(T)cur_leaf[cur_pos - cur_start] : fullfetch()); // FIXIT: why cast(T) ?
	}

	/// Internal function
	private T fullfetch() const {
		throw new Exception("imposible: function fullfetch not implemented");
	}

	/** Returns array of chars in current leaf, and set parameter to position of this array in cord
	 *
	 * If there is no more leafs returns null.
	 *
	 * Bugs: You can't intermix currently fetch, fetch_leaf, next_leaf, and opInc
	 * TODO: Return only slice from cur_pos to end of cur_leaf
	 */
	invariant(T)[] fetch_leaf(out size_t cur_leaf_start_pos) const
	in {
		assert(cur_pos < length);
	}
	body {
		cur_leaf_start_pos = 0;
		return cur_leaf;
	}


	/** Moves to next leaf. */
	void next_leaf() {
		
	}

	/** Moves position by one character.
	 *
	 * Note: UTF-8 characters are not supported yet.
	 */
	void opInc()
	in {
		assert(cur_pos < length-1);
	}
	out {
		assert(cur_pos < length);
	}
	body {
		if (cur_pos + 1 < cur_end) {
			cur_pos++;
		} else {
			fullInc();
		}
	}

	/// Moves position by one character, returns current value of iterator
	T fetch_and_inc() {
		auto r = fetch();
		opInc();
		return r;
	}

	/// Internal testing function
	private void print_path() const {
		for (int i = 0; i < path_len; i++) {
			writef("level_%d : ", i);
			path[i].pe_cord.print();
		}
	}

	/// Internal function
	private void fullInc() {
		pe* cur_pe = &(path[path_len]);
		CT leaf = cur_pe.pe_cord; /// TODO: constnes
		cur_pos++;
		auto s = cast(cord_t_string!(T))leaf; /// TODO: constnes
		if (!s) {
			// function leaf
			throw new Exception("function leafs not implemented");
			/*
			if (cur_pos < end_pos) {
				// do caching
				return;
			}
			*/
		}
		// end of leaf
		// pop the stack until we find two concatenation nodes with the
		// same start position: this implies we were in left part.
		{
			while (path_len > 0 && cur_pe[0].pe_start_pos != cur_pe[-1].pe_start_pos) {
				path_len--;
				cur_pe--;
			}
			if (path_len == 0) {
				//path_len = invalid;
				//return;
				throw new Exception("Cord bounds exception");
			}
		}
		path_len--;
		extend_path();
	}

	/** Move by n characters.
	 *
	 * Bugs: Not implemented efficiently, as it can be (multiple call's to opInc).
	 * TODO: calculate quickly which subtree had given position, and skip by whole leafs.
	 */
	void opAdd(int n)
	in {
		assert(cur_pos < length-n);
	}
	out {
		assert(cur_pos < length);
	}
	body {
		for (int i = 0; i < n; i++) {
			opInc();
		}
	}

/+
	void opDec()
	in {
		assert(cur_pos > 0);
	}
	out {
		assert(cur_pos >= 0);
	}
	body {
		if (cur_end != 0 && cur_pos > cur_start) {
			cur_pos--;
		} else {
			fullDec()
		}
	}
+/
}



/** Structure for storing repeating strings.
 *
 * In this example:
 * ---
 * auto s = new cord_t("Some very long (1000 chars) line"); // 1 KB
 * auto big = s.repeat(1000, "\n"); // 1000 lines
 * ---
 * big will be about 2 kB, and all string operations still be very fast.
 */
final class cord_t_repeat(T) : cord_t!(T) {
	private const int times;
}

/// Aliases for immutable cords
alias const(cord_t!(char)) CordI;
/// ditto
alias const(cord_t!(dchar)) CordID;
/// ditto
alias const(cord_t!(wchar)) CordIW;

/// Aliases for immutable cords
alias const(cord_t_string!(char)) CordIString;
/// ditto
alias const(cord_t_string!(dchar)) CordIDString;
/// ditto
alias const(cord_t_string!(wchar)) CordIWString;

/// Aliases for immutable cords
alias const(cord_t_empty!(char)) CordIEmpty;
/// ditto
alias const(cord_t_empty!(dchar)) CordIDEmpty;
/// ditto
alias const(cord_t_empty!(wchar)) CordIWEmpty;

/** Rebindable cords.
 *
 * Because of immutability of cord_t they are passed everywhere as consts and returned, this
 * implies that variables need to be const. Such variables can be written to only once, on initialisation.
 * To hide this constness this wrapper is provided, it allows changing of references in local functions.
 *
 * If you don't like Single Assigment or don't write functional style programing, this is for you.
 *
 * Note: op*Assign are still not supported with proper meaning. Use cord_wrap (aka Cord), for this.
 *
 * Note: For few function (which doesn't returns cord_t, which we should convert) we relay on opDot)
 *
 * Example:
 * ---
 * RBCord a = RBCord();
 * RBCord b = RBCord("Sun");
 * a = "Earth";
 * a = a ~ " " ~ b;
 * b = "Moon";
 * a = a ~ " " ~ b;
 * ---
 */
struct RBCord_t(T) {
	alias cord_t!(T) CT;
	alias cord_t_empty!(T) CTE;
	alias cord_t_string!(T) CTS;
	alias RBCord_t!(T) RBC;

	/// mixin Rebindable template
	mixin RBMixin!(const(CT), RBC) RBM;

	/// allow overloding of opCall from template with our
	alias RBM.opCall opCall;

	/// create empty string
	static RBC opCall() {
		RBC r;
		r = new CTE();
		return r;
	}

	/// create pure strings
	static RBC opCall(invariant(T)[] t) {
		RBC r;
		r = new CTS(t);
		return r;
	}

	/// allow overloding of opAssign from template with our
	alias RBM.opAssign opAssign;

	/// allow assigning pure strings
	RBC opAssign(invariant(T)[] t) {
		stripped = cast(CT)(new CTS(t));
		return this;
	}

	// comented out methods are not implemented,
	// because they are forwarded to CT using opDot, //
	// and don't need conversion of return value

	//const size_t length;
	//size_t get_depth() const {

	/// i don't know why this is needed. opDot should suffiece, but it isn't
	T opIndex(const size_t _pos) const {
		return original[_pos];
	}

	/// opCat
	RBC opCat(in T s) const {
		return RBC(original.opCat(s));
	}

	/// ditto
	RBC opCat(in const(CT) s) const {
		return RBC(original.opCat(s));
	}

	// added
	RBC opCat(in RBC s) const {
		return RBC(original.opCat(s.original));
	}

	/// opCat string
	RBC opCat(in invariant(T)[] s) const {
		return RBC(original.opCat(s));
	}
	RBC opCat_r(in invariant(T)[] s) const {
		return RBC(original.opCat_r(s));
	}

	// added. needed because ~ isn't commutative
	//RBC opCat_r(in RBC s) const {
	//	return RBC(s.original.opCat(original));
	//}
	// tests.d(320): Error: overloads const RBCord_t!(char)(in const(RBCord_t!(char)) s) and const RBCord_t!(char)(in const(RBCord_t!(char)) s) both match argument list for opCat
	// but it is not commutive!!!
	// aha. it's not needed, because RBC a.opCat(RBC b), and RBC b.opCat_r(RBC a) are both good


	/// opSlice
	RBC opSlice(const size_t _begin, const size_t _end) const {
		return RBC(original.opSlice(_begin, _end));
	}

	/// opSliceAssign
	RBC opSliceAssign(const T mid, const size_t _begin, const size_t _end) const {
		return RBC(original.opSliceAssign(mid, _begin, _end));
	}
	/// ditto
	RBC opSliceAssign(in invariant(T)[] mid, const size_t _begin, const size_t _end) const {
		return RBC(original.opSliceAssign(mid, _begin, _end));
	}
	/// ditto
	RBC opSliceAssign(in const(CT) mid, const size_t _begin, const size_t _end) const {
		return RBC(original.opSliceAssign(mid, _begin, _end));
	}
	// added
	RBC opSliceAssign(in RBC mid, const size_t _begin, const size_t _end) const {
		return RBC(original.opSliceAssign(mid.original, _begin, _end));
	}

	/// opIndexAssign
	RBC opIndexAssign(const T mid, const size_t _pos) const {
		return RBC(original.opIndexAssign(mid, _pos));
	}
	/// ditto
	RBC opIndexAssign(in invariant(T)[] mid, const size_t _pos) const {
		return RBC(original.opIndexAssign(mid, _pos));
	}
	/// ditto
	RBC opIndexAssign(in const(CT) mid, const size_t _pos) const {
		return RBC(original.opIndexAssign(mid, _pos));
	}
	/// added
	RBC opIndexAssign(in RBC mid, const size_t _pos) const {
		return RBC(original.opIndexAssign(mid.original, _pos));
	}

	/// needed because compiler  can't infer type for foreach using opDot
	int opApply(int delegate(inout invariant(T[])) dg) const {
		return original.opApply(dg);
	}
	//final invariant(T)[] toArray() const
	//static if (is(T == char)) {
	//final override invariant(char)[] toString() const
	//}
	//final void toBuffer(ref T[] buf) const {
	//void print() const {
	//void print_structure(const size_t n = 0) const;
	/// balance
	protected RBC balance() const {
	    return RBC(original.balance());
	}
	/// force ballance
	protected RBC force_balance() const {
	    return RBC(original.force_balance());
	}
	//cord_t_iterator!(T) getIterator(const size_t _start = 0) const
	/** Comparission functions */
	bool opEquals(Object o) const {
		return original.opEquals(o);
	}
	/// ditto
	int opCmp(Object o) const {
		return original.opCmp(o);
	}
	/// added
	int opEquals(RBC r) const {
		return original.opEquals(r.original);
	}
	/// added
	int opCmp(RBC r) const {
		return original.opCmp(r.original);
	}
	//bool opEquals(in T[]) const;
	//int opCmp(in T[]) const;

	//hash_t opHash() const {
}

/// aliases
alias RBCord_t!(char) RBCord;
/// ditto
alias RBCord_t!(dchar) RBCordD;
/// ditto
alias RBCord_t!(wchar) RBCordW;


unittest {
	RBCord r1;

	// Balancing

	auto expected_depth1 = [
	   0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
	  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  11,  12,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,
	  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,
	  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  15,  16,  17,  18,  19,  20,  21,  22,  23,
	  24,  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  15,  16,  17,  18,  19,  20,  21,  22,
	  23,  24,  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  16,  17,  18,  19,  20,  21,  22,
	  23,  24,  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  16,  17,  18,  19,  20,  21,  22,
	  23,  24,  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  17,  18,  19,  20,  21,  22,  23,
	  24,  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  17,  18,  19,  20,  21,  22,  23,  24,
	  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  17,  18,  19,  20,  21,  22,  23,  24,  25,
	  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,
	  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,
	  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44
	];

	auto expected_depth = [
	   0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
	  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  11,  12,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,
	  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,
	  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  15,  16,  17,  18,  19,  20,  21,  22,  23,
	  24,  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  15,  16,  17,  18,  19,  20,  21,  22,
	  23,  24,  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  16,  17,  18,  19,  20,  21,  22,
	  23,  24,  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  16,  17,  18,  19,  20,  21,  22,
	  23,  24,  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  17,  18,  19,  20,  21,  22,  23,
	  24,  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  17,  18,  19,  20,  21,  22,  23,  24,
	  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  17,  18,  19,  20,  21,  22,  23,  24,  25,
	  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,
	  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,
	  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44
	];

	r1 = RBCord();

	for (int i = 0; i < 400; i++) {
		r1 = r1 ~ "ABCDEFG123456";
		assert(r1.get_depth() == expected_depth1[i]);
	}
	assert(r1.get_depth() == 44);

	r1 = RBCord();

	for (int i = 0; i < 400; i++) {
		r1 = "ABCDEFG123456" ~ r1;
		assert(r1.get_depth() == expected_depth1[i]);
	}

	assert(r1.get_depth == 44);
}

/** Mutable cord
 *
 * This is wrapper for cord class, which is mutable (so allows opCatAssign, or opIndexAssign, opSliceAssign,
 * and string operations),
 *
 * It just simplifies all operations (especially append/prepend only mode, because there is no need for additionall variables),
 * and make transision from standard string implementation easier
 *
 * If you are writing code in functional maner don't use this wrapper.
 *
 * Note: Don't use r = p; for coping cord, use r = new Cord(p) or r = p[]; because first only copies reference to wrapper.
 *       So mutating one of them will change second (exactly like arrays and slices).
 *
 * Note: Operations like (p[5] = 'b') had different meaning than in immutable cords. Here they "change" cord "in place",
 *       in immutable cords they return new cord (r = (p[5] = 'b')), p is not really changed.
 *
 * Note: Internally we are using RBCord_t
 *
 * Example:
 * ---
 * auto porg = new Cord("The quick brown fox jumps over the lazy dog");
 * 
 * auto p = new Cord(porg);
 * auto f = p[16 .. 19];
 * assert(f == "fox");
 * auto p2 = p[0 .. 16] ~ "cat" ~ p[19 .. p.length];
 * auto p3 = porg[];
 * p3[16 .. 19] = "cat";
 * assert(p2 == p3 && p2[16 .. 19] == "cat");
 * auto p4 = porg[];
 * p4[16] = 'b';
 * assert(p4[16 .. 19] == "bat");
 * 
 * p = p ~ p;
 * p = p ~ p;
 * p = p ~ p;
 * p = p ~ p;
 * p = p ~ p;
 * p = p ~ p;
 * p = p ~ p;
 * p = p ~ p;
 * 
 * auto x = p[ 876123 ... 915123 ];
 * ---
 */
struct cord_wrap(T) {
	alias cord_wrap!(T) CW;
	alias cord_t!(T) CT;
	alias cord_t_string!(T) CTS;
	alias cord_t_empty!(T) CTE;

	alias RBCord_t!(T) RBC;

	/// internall RBCord_t
	private RBC c;

	/// opDot
	RBC opDot() {
		return c;
	}

	/// Empty cord
	static CW opCall() {
		CW r;
		//c = cast(const(CT))(new const(CTE)());
		return r;
	}

	/// Copy wrapped cord
	static CW opCall(CW c_) {
		CW r;
		r.c = c_.c;
		return r;
	}
	/// Copy real cord
	static CW opCall(in const(CT) c_) {
		CW r;
		r.c = c_;
		return r;
	}

	/// Create cord from string
	static CW opCall(invariant(T)[] s_) {
		CW r;
		r.c = new CTS(s_);
		return r;
	}

	/// ditto
	static CW opCall(T[] s_) {
		CW r;
		r.c = new CTS(s_.idup);
		return r;
	}

	/// ditto
	static CW opCall(RBC s_) {
		CW r;
		r.c = s_;
		return r;
	}


	/// Returns real cord
	const(CT) to_cord() const {
		return c.opDot();
	}


	/// Returns array
	invariant(T[]) toArray() const {
		return c.toArray();
	}


	static if (is(T == char)) {
	/// Returns array
	invariant(char[]) toString() const {
		return c.toString();
	}
	}


	/// length
	size_t length() const {
		return c.length;
	}


	/// opCat
	CW opCat(CW d) const {
		return CW(c ~ d.c);
	}

	/// ditto
	CW opCat(const(CT) d) const {
		return CW(c ~ d);
	}

	/// ditto
	CW opCat(invariant(T)[] d) const {
		return CW(c ~ d);
	}

	/// ditto
	CW opCat(T[] d) const {
		return CW(c ~ d.idup);
	}

	/// ditto
	CW opCat(T d) const {
		return CW(c ~ d);
	}


	/// opCat_r
	CW opCat_r(const(CT) d) const {
		return CW(d ~ c.opDot());
	}

	/// ditto
	CW opCat_r(invariant(T)[] d) const {
		return CW(d ~ c);
	}

	/// ditto
	CW opCat_r(T[] d) const {
		return CW(d.idup ~ c);
	}

	/// ditto
	CW opCat_r(T d) const {
		return CW(d ~ c.opDot());
	}


	/// opCatAssign
	CW opCatAssign(CW d) {
		c = c ~ d.c;
		return this;
	}

	/// ditto
	CW opCatAssign(const(CT) d) {
		c = c ~ d;
		return this;
	}

	/// ditto
	CW opCatAssign(invariant(T)[] d) {
		c = c ~ d;
		return this;
	}

	/// ditto
	CW opCatAssign(T[] d) {
		c = c ~ d.idup;
		return this;
	}

	/// ditto
	CW opCatAssign(T d) {
		c = c ~ d;
		return this;
	}


	/// prepend
	CW prepend(CW d) {
		c = d.c ~ c.opDot();
		return this;
	}

	/// ditto
	CW prepend(const(CT) d) {
		c = d ~ c.opDot();
		return this;
	}

	/// ditto
	CW prepend(invariant(T)[] d) {
		c = d ~ c;
		return this;
	}

	/// ditto
	CW prepend(T[] d) {
		c = d.idup ~ c;
		return this;
	}

	/// ditto
	CW prepend(T d) {
		c = d ~ c.opDot();
		return this;
	}


	/// opIndex
	T opIndex(size_t i) const {
		return c[i];
	}


	/// opIndexAssign
	CW opIndexAssign(CW d, size_t i) {
		c = (c[i] = d.c);
		return this;
	}

	/// ditto
	CW opIndexAssign(const(CT) d, size_t i) {
		c = (c[i] = d);
		return this;
	}

	/// ditto
	CW opIndexAssign(T d, size_t i) {
		c = (c[i] = d);
		return this;
	}

	/// ditto
	CW opIndexAssign(invariant(T)[] d, size_t i) {
		c = (c[i] = d);
		return this;
	}

	/// ditto
	CW opIndexAssign(T[] d, size_t i) {
		c = (c[i] = d.idup);
		return this;
	}


	/// opSlice
	CW opSlice(size_t i1, size_t i2) const {
		return CW(c[i1 .. i2]);
	}


	/// opSliceAssign/3
	CW opSliceAssign(T d, size_t i1, size_t i2) {
		c = (c[i1 .. i2] = d);
		return this;
	}

	/// ditto
	CW opSliceAssign(invariant(T)[] d, size_t i1, size_t i2) {
		c = (c[i1 .. i2] = d);
		return this;
	}

	/// ditto
	CW opSliceAssign(T[] d, size_t i1, size_t i2) {
		c = (c[i1 .. i2] = d.idup);
		return this;
	}

	/// ditto
	CW opSliceAssign(CW d, size_t i1, size_t i2) {
		c = (c[i1 .. i2] = d.c);
		return this;
	}

	/// ditto
	CW opSliceAssign(const(CT) d, size_t i1, size_t i2) {
		c = (c[i1 .. i2] = d);
		return this;
	}


	/// opSliceAssign/1
	CW opSliceAssign(CW d) {
		c = d.c;
		return this;
	}

	/// ditto
	CW opSliceAssign(const(CT) d) {
		c = d;
		return this;
	}

	/// ditto
	CW opSliceAssign(invariant(T)[] d) {
		c = new CTS(d);
		return this;
	}

	/// ditto
	CW opSliceAssign(T[] d) {
		c = new CTS(d.idup);
		return this;
	}

	/// ditto
	CW opSliceAssign(T d) {
		c = new CTS([d]);
		return this;
	}


	/// Get clone of cord
	CW opSlice() const {
		return CW(this.c.opDot());
	}


	/// Print underling structure
	void print() const {
		c.print();
	}


	/// Get depth of underling tree
	size_t get_depth() const {
		return c.get_depth();
	}


	/// Possibly rebalance tree. Call this function only if you know what are you doing. opCat*, etc. calls this function automatically.
	void balance() {
		c = c.balance();
	}

	/// Rebalance tree. Call this function only if you really know what are you doing.
	void force_balance() {
		c = c.force_balance();
	}


	/// Iterate over whole cord, chunk by chunk, You need to manually keep track of index currently
	int opApply(int delegate(inout invariant(T[])) dg) const {
		return c.opApply(dg);
	}

	// return's hash value of current value of cord
	hash_t toHash() const {
		return c.toHash();
	}
}

/// Usefull aliases
alias cord_wrap!(char) Cord;
/// ditto
alias cord_wrap!(dchar) CordD;
/// ditto
alias cord_wrap!(wchar) CordW;

import std.stdarg;

/// Catenate multiple cords, chars, strings, allowing implementation to optimalise layout
/// TODO: we are currently cheating slightly
auto cat(T)(...) {
	cord_t!(T) r = new cord_t_empty!(T)();

	for (int i = 0; i < _arguments.length; i++) {
		if (_arguments[i] == typeid(int)) {
			r = r ~ toArray(va_arg!(int)(_argptr));
		} else if (_arguments[i] == typeid(char[])) {
			r = r ~ va_arg!(char[])(_arguments[i]);
		} else if (_arguments[i] == typeid(CT)) {
			r = r ~ va_arg!(CT)(_arguments[i]);
		} else if (_arguments[i] == typeid(cord_wrap!(T))) {
			r = r ~ va_arg!(cord_wrap!(T))(_arguments[i]).c;
		} else {
			throw new Exception("Unknown type op argument in cat");
		}
	}
	return r;
}

/// Similar to writeln, snprintf, and so one
auto format(T)(...) {
	cord_t!(T) r = new cord_t_empty!(T)();
	return r;
}

/** Standard string, usefull for benchmarks
 *
 * Note: it isn't very efficient sometime because we are using immutable semantic
 *       so i.e. assigning single cell copies whole array!
 *
 * Note: some operations can be not completly optimal (ie. when string are constant, compiler notices this, here not)
 */
struct string_wrap(T) {
	alias string_wrap!(T) SW;
	alias cord_wrap!(T) CW;
	alias cord_t!(T) CT;

	private invariant(T)[] c;

	/// Empty cord
	static SW opCall() {
		SW r;
		r.c = [];
		return r;
	}

	/// Copy wrapped cord
	static SW opCall(CW c_) {
		return opCall(c_.c.opDot());
	}

	/// Copy real cord
	static SW opCall(const(CT) c_) {
		SW r;
		r.c = c_.toArray();
		return r;
	}

	/// Create cord from string
	static SW opCall(invariant(T)[] s_) {
		SW r;
		r.c = s_;
		return r;
	}

	/// ditto
	static SW opCall(T[] s_) {
		SW r;
		writefln("Warning: duplicating string: %s", s_);
		r.c = s_.idup;
		return r;
	}

	///  Returns array
	invariant(T[]) toArray() const {
		return c;
	}

	static if (is(T == char)) {
	///  Returns array
	invariant(T[]) toString() const {
		return c;
	}
	}

	/// length
	size_t length() const {
		return c.length;
	}

	/// opCat
	SW opCat(SW d) const {
		return SW(c ~ d.c);
	}

	/// ditto
	SW opCat(CW d) const {
		return opCat(d.c.opDot());
	}

	/// ditto
	SW opCat(const(CT) d) const {
		T[] buf = new T[c.length + d.length];
		buf[0 .. c.length] = c;
		d.toBuffer(buf[c.length .. $]);
		return SW(cast(invariant(T[]))buf);
	}

	/// ditto
	SW opCat(invariant(T)[] d) const {
		return SW(c ~ d);
	}

	/// ditto
	SW opCat(T[] d) const {
		return SW(c ~ d);
	}

	/// ditto
	SW opCat(T d) const {
		return SW(c ~ d);
	}

	/// opCat_r
	SW opCat_r(CW d) const {
		return opCat_r(d.c.opDot());
	}

	/// ditto
	SW opCat_r(const(CT) d) const {
		T[] buf = new T[d.length + c.length];
		d.toBuffer(buf);
		buf[d.length .. $] = c;
		return SW(cast(invariant(T[]))buf);
	}

	/// ditto
	SW opCat_r(invariant(T)[] d) const {
		return SW(d ~ c);
	}

	/// ditto
	SW opCat_r(T[] d) const { /// d.idup isn't needed because we are creating new array, is this safe if c is empty?
		return SW(d ~ c);
	}

	/// ditto
	SW opCat_r(T d) const {
		return SW(d ~ c);
	}


	/// opCatAssign
	SW opCatAssign(SW d) {
		c = c ~ d.c;
		return this;
	}

	/// ditto
	SW opCatAssign(CW d) {
		return opCatAssign(d.c.opDot());
	}

	/// ditto
	SW opCatAssign(const(CT) d) {
		T[] buf = new T[c.length + d.length];
		buf[0 .. c.length] = c;
		d.toBuffer(buf[c.length .. $]);
		c = cast(invariant(T)[])buf;
		return this;
	}

	/// ditto
	SW opCatAssign(invariant(T)[] d) {
		c = c ~ d;
		return this;
	}

	/// ditto
	SW opCatAssign(T[] d) {
		c = c ~ d.idup;
		return this;
	}

	/// ditto
	SW opCatAssign(T d) {
		c = c ~ d;
		return this;
	}

	/// prepend
	SW prepend(SW d) {
		c = d.c ~ c;
		return this;
	}

	/// ditto
	SW prepend(CW d) {
		return prepend(d.c.opDot());
	}

	/// ditto
	SW prepend(const(CT) d) {
		T[] buf = new T[c.length + d.length];
		d.toBuffer(buf[0 .. d.length]);
		buf[d.length .. $] = c;
		c = cast(invariant(T)[])buf;
		return this;
	}

	/// ditto
	SW prepend(invariant(T)[] d) {
		c = d ~ c;
		return this;
	}

	/// ditto
	SW prepend(T[] d) {
		c = d.idup ~ c;
		return this;
	}

	/// ditto
	SW prepend(T d) {
		c = d ~ c;
		return this;
	}


	/// opIndex
	T opIndex(size_t i) const {
		return c[i];
	}

	/// opIndexAssign
	SW opIndexAssign(SW d, size_t i) {
		//c = (c[i] = d.c);
		return this;
	}

	/// ditto
	SW opIndexAssign(CW d, size_t i) {
		//c = (c[i] = d.c);
		return this;
	}

	/// ditto
	SW opIndexAssign(const(CT) d, size_t i) {
		//c = (c[i] = d);
		return this;
	}

	/// ditto
	SW opIndexAssign(T d, size_t i) {
		//c = (c[i] = d);
		return this;
	}

	/// ditto
	SW opIndexAssign(invariant(T)[] d, size_t i) {
		//c = (c[i] = d);
		return this;
	}

	/// ditto
	SW opIndexAssign(T[] d, size_t i) {
		//c = (c[i] = d);
		return this;
	}

	/// opSlice
	SW opSlice(size_t i1, size_t i2) { // can't be const, c is escaping
		return SW(c[i1 .. i2]);
	}

	/// opSliceAssign/3
	SW opSliceAssign(T d, size_t i1, size_t i2) {
		//c = (c[i1 .. i2] = d);
		return this;
	}

	/// ditto
	SW opSliceAssign(invariant(T)[] d, size_t i1, size_t i2) {
		//c = (c[i1 .. i2] = d);
		return this;
	}

	/// ditto
	SW opSliceAssign(T[] d, size_t i1, size_t i2) {
		//c = (c[i1 .. i2] = d);
		return this;
	}

	/// ditto
	SW opSliceAssign(SW d, size_t i1, size_t i2) {
		//c = (c[i1 .. i2] = d.c);
		return this;
	}

	/// ditto
	SW opSliceAssign(CW d, size_t i1, size_t i2) {
		//c = (c[i1 .. i2] = d.c);
		return this;
	}

	/// ditto
	SW opSliceAssign(const(CT) d, size_t i1, size_t i2) {
		//c = (c[i1 .. i2] = d);
		return this;
	}

	/// opSliceAssign/1
	SW opSliceAssign(SW d) {
		c = d.c;
		return this;
	}

	/// ditto
	SW opSliceAssign(CW d) {
		c = d.c.toArray();
		return this;
	}

	/// ditto
	SW opSliceAssign(const(CT) d) {
		c = d.toArray();
		return this;
	}

	/// ditto
	SW opSliceAssign(invariant(T)[] d) {
		c = d;
		return this;
	}

	SW opSliceAssign(T[] d) {
		c = d.idup;
		return this;
	}

	/// ditto
	SW opSliceAssign(T d) {
		c = [d];
		return this;
	}

	/// Get clone of cord
	SW opSlice() { /// can't be const, c is escaping
		return this;
	}

	/// Prints nothing
	void print() const {
		return;
	}

	/// Returns zero
	size_t get_depth() const {
		return 0;
	}

	/// Does nothing
	void balance() {
		return;
	}

	/// Does nothing
	void force_balance() {
		return;
	}

	/// Runs just once delegate dg on internal array
	int opApply(int delegate(ref invariant(T[])) dg) const {
		return dg(c);
	}

	// return's hash value of current value of string
	hash_t toHash() const {
		return (new cord_t_string!(T)(c)).toHash();
	}
}

/// Usefull aliases
alias string_wrap!(char) String;
/// ditto
alias string_wrap!(dchar) StringD;
/// ditto
alias string_wrap!(wchar) StringW;
