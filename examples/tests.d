module m;

import std.stdio;
import utils.log: logging;
import utils.timer : Timer;
import std.string;

import iolists;
import cords;

import iolists_mutable;

const int n = 2000;
const int m = 200;

int main(string[] args) {
	logging = true;

	//cordy1();

	writefln();
	writefln();

//	cordy_t_manual();

	writefln();
	writefln();

	cordy_t();

	return 0;
}

/+
void cordy_l() {
	auto rr = gen();

	writefln("prepend tests:\n");
	test(rr, 100, false);

	writefln("\nappend&prepend tests:\n");
	auto r2 = test(rr, 100, true);


	{
	writefln("\ncord slicing");
	scope Timer = new Timer();
	auto r3 = r2[6231 .. 19123];
	}

	auto x = new cord_l!(char)("sd");
	auto y1 = x ~ "gg";
	auto y2 = "gg" ~ x;

	writefln("x=    %s", x);
	writefln("y1=   %s", y1);
	writefln("y2=   %s", y2);

	auto xx = x ~ x;
	writefln("xx=   %s", xx);

	auto x3 = y1 ~ x;
	writefln("x3=   %s", x3);

	auto x10 = xx ~ "b" ~ x3;
	writefln("x10=  %s", x10);
	writefln("x10'= %s", x10.remove(4, 5));

}
+/

char[] gen() {
	char[] r;
	r.length = 1000;
	r[] = 'a';
	return r;
}

CordI test(char[] rr, int n, bool pr) {
	writefln("cord");
	scope tr = new Timer();
	auto r2 = Cord();
	for(int i = 0; i < n; i++) {
		r2 ~= rr;
		if (pr) r2.prepend(rr);
	}
	delete tr;

	writefln("cord to string");
	scope tr2 = new Timer();
	auto xxc = r2.toString();
	delete tr2;

	writefln("string");
	scope ts = new Timer();
	char[] sr2;
	for(int i = 0; i < n; i++) {
		sr2 ~= rr;
		if (pr) sr2 = rr ~ sr2;
	}
	delete ts;

	return r2.to_cord();
}


//version=ps; // print structure
//version=pt; // print ruler

/** Tree cord using unions of structs and switch */
/+
void cordy_t() {
	auto x1 = new cord2();
	writefln("x1");
	version(ps) x1.print_structure();
	version (ps) writefln();

	auto x2 = new cord2("df");
	writefln("x2");
	version(ps) x2.print_structure();
	version (ps) writefln();

	auto x3 = new cord2("bf");
	writefln("x3");
	version(ps) x3.print_structure();
	version (ps) writefln();

	auto x4 = x2~x3;
	writefln("x4");
	version(ps) x4.print_structure();
	version (ps) writefln();

	auto x5 = x4~x4;
	writefln("x5");
	version(ps) x5.print_structure();
	version (ps) writefln();

	auto x6 = x5~x5;
	writefln("x6");
	version(ps) x6.print_structure();
	version (ps) writefln();

	auto x7 = x6~x6;
	writefln("x7");
	version(ps) x7.print_structure();
	version (ps) writefln();

	auto x8 = x7 ~ "ALA";
	writefln("x8");
	version(ps) x8.print_structure();
	version (ps) writefln();

	auto x9 = x8 ~ "KOT";
	writefln("x9");
	version(ps) x9.print_structure();
	version (ps) writefln();

	auto x9a = x8 ~ new cord2("KOT");
	writefln("x9a");
	version (ps) x9a.print_structure();
	version (ps) writefln();

	writefln("cord_zenek");
	scope tr = new Timer();

	for (int i = 0; i < n; i++) {
		x9a = x9a ~ x6;
		if (i % m == 0) x9a = x6 ~ x9a;
	}

	delete tr;

	writefln("cord_zenek toString");
	tr = new Timer();

	auto sx9a = x9a.toString();
	auto sx6 = x6.toString();
	delete tr;

	assert(sx9a.length == x9a.length);
	assert(sx6.length == x6.length);


/*
	writefln("char[] opCatAssign");
	tr = new Timer();

	for (int i = 0; i < n; i++) {
		sx9a ~= sx6;
		if (i % m == 0) sx9a = sx6 ~ sx9a;
	}

	delete tr;
*/
/*
	sx9a = x9a.toString();
	sx6 = x6.toString();


	writefln("char[] opCat");
	tr = new Timer();

	for (int i = 0; i < n; i++) {
		sx9a = sx9a ~ sx6;
		if (i % m == 0) sx9a = sx6 ~ sx9a;
	}

	delete tr;
*/
/*
	auto lx9a = new cord(sx9a);
	auto lx6 = new cord(sx6);

	writefln("cord_l opCatAssign");
	tr = new Timer();

	for (int i = 0; i < n; i++) {
		//lx9a = lx9a ~ lx6;
		lx9a ~= lx6;
		if (i % m == 0) lx9a = lx6 ~ lx9a;
	}

	delete tr;
*/

	ss();
}


void ss() {
	writefln();
	writefln("Substring tests");
	writefln();

	auto x = new cord2("abcdefghijk123456789");
	version(ps) x.print_structure();
	x.print();

	auto x1 = x[5..13];
	version(ps) x1.print_structure();
	x1.print();

	auto x2 = x ~ x1;
	version(ps) x2.print_structure();
	x2.print();

	auto x3 = x2[13..24];
	version(ps) x3.print_structure();
	x3.print();

	x3 = x2[0..20];
	version(ps) x3.print_structure();
	x3.print();

	x3 = x2[20..28];
	version(ps) x3.print_structure();
	x3.print();

	x3 = x2[0..28];
	version(ps) x3.print_structure();
	x3.print();

	ins();
}

void ins() {
	writefln();
	writefln("Inserts tests");
	writefln();

	auto t = new cord2("123456789012345678901234567890");

	auto x = new cord2("abcdefghijk123456789");
	writefln("Start:");
	version(ps) x.print_structure();
	version (pt) t.print();
	x.print();

	auto x1 = (x[3..5] = "PLOKIJ");
	writefln("4th and 5th change to PLOKIJ:");
	version (pt) t.print();
	x1.print();
	version(ps) x1.print_structure();
	writefln("Orginal:");
	version(ps) x.print_structure();
	version (pt) t.print();
	x.print();

	x1 = (x1[5..5] = "Y");
	writefln("After 5-th insert Y:");
	version(ps) x1.print_structure();
	version (pt) t.print();
	x1.print();

	x1 = (x[3..10] = "");
	writefln("Remove 4th - 10th:");
	version(ps) x1.print_structure();
	version (pt) t.print();
	x1.print();
}
++/


/** Tree cord with inheritance */

void cordy_t() {

	auto x1 = RBCord();
	writefln("x1");
	version(ps) x1.print_structure();
	version (ps) writefln();

	auto x2 = RBCord("df");
	writefln("x2");
	version(ps) x2.print_structure();
	version (ps) writefln();

	auto x3 = RBCord("bf");
	writefln("x3");
	version(ps) x3.print_structure();
	version (ps) writefln();

	auto x4 = x2~x3;
	writefln("x4");
	version(ps) x4.print_structure();
	version (ps) writefln();

	auto x5 = x4~x4;
	writefln("x5");
	version(ps) x5.print_structure();
	version (ps) writefln();

	auto x6 = x5~x5;
	writefln("x6");
	version(ps) x6.print_structure();
	version (ps) writefln();

	auto x7 = x6~x6;
	writefln("x7");
	version(ps) x7.print_structure();
	version (ps) writefln();

	auto x8 = x7 ~ "ALA";
	writefln("x8");
	version(ps) x8.print_structure();
	version (ps) writefln();

	auto x9 = x8 ~ "KOT";
	writefln("x9");
	version(ps) x9.print_structure();
	version (ps) writefln();

	auto x9a = x8 ~ RBCord("KOT");
	writefln("x9a");
	version (ps) x9a.print_structure();
	version (ps) writefln();

	writefln("cord_t");
	scope tr = new Timer();

	for (int i = 0; i < n; i++) {
		writefln(i);
		auto x9f = x9a ~ x6;
		auto x9ff = x9f;
		if (i % m == 0) {
			x9ff = x6 ~ x9a;
		}
		x9a = x9ff;
	}

	delete tr;

	writefln("cord_t toArray()");
	tr = new Timer();
	auto sx9a = x9a.toArray();
	auto sx6 = x6.toArray();
	delete tr;

	assert(sx9a.length == x9a.length);
	assert(sx6.length == x6.length);
	ss_i();
}

void ss_i() {
	writefln();
	writefln("Substring tests (cord_t)");
	writefln();

	auto x = RBCord("abcdefghijk123456789");
	version(ps) x.print_structure();
	x.print();

	auto x1 = x[5..13];
	version(ps) x1.print_structure();
	x1.print();

	auto x2 = x ~ x1;
	version(ps) x2.print_structure();
	x2.print();

	auto x3 = x2[13..24];
	version(ps) x3.print_structure();
	x3.print();

	auto x3a = x2[0..20];
	version(ps) x3a.print_structure();
	x3a.print();

	auto x3b = x2[20..28];
	version(ps) x3b.print_structure();
	x3b.print();

	auto x3c = x2[0..28];
	version(ps) x3c.print_structure();
	x3c.print();

	ins_i();
}

void ins_i() {
	writefln();
	writefln("Inserts tests (cord_t)");
	writefln();


	auto t = RBCord("123456789012345678901234567890");

	auto x = RBCord("abcdefghijk123456789");
	writefln("Start:");
	version(ps) x.print_structure();
	version (pt) t.print();
	x.print();

	auto x1 = (x[3..5] = "PLOKIJ");
	writefln("4th and 5th change to PLOKIJ:");
	version (pt) t.print();
	x1.print();
	version(ps) x1.print_structure();
	writefln("Orginal:");
	version(ps) x.print_structure();
	version (pt) t.print();
	x.print();

	auto x1a = (x1[5..5] = "Y");
	writefln("After 5-th insert Y:");
	version(ps) x1a.print_structure();
	version (pt) t.print();
	x1a.print();

	auto x1b = (x[3..10] = "");
	writefln("Remove 4th - 10th:");
	version(ps) x1b.print_structure();
	version (pt) t.print();
	x1b.print();

	iter();
}


void iter() {
	writefln();
	writefln("Single iter (cord_t)");
	writefln();

	auto a1 = RBCord("12345");
	auto a2 = RBCord("plokij");

//	auto a3 = (("start"~a1~a2)~"midle"~(a1~a2 ~ "end") )~ "after";
	auto a3 = ((a1~a2)~"midle"~(a1~a2 ~ "end") )~ "after";
	a3.print();
	version (ps) a3.print_structure();

	//for (auto a3i = a3.getIterator(); !a3i.isLast(); a3i.opInc()) {
	//	//writefln("path:");
	//	//a3i.print_path();
	//	writefln("a3[%d] = %s", a3i.get_index, a3i.fetch);
	//}
}
