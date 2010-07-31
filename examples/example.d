module example;

import std.stdio;
import std.string : ts = toString;
import cords : CordI, CordIString, CordIEmpty, RBCord;

void main() {

	// Creating

	auto porg = new CordIString("The quick brown fox jumps over the lazy dog\n"); 

	auto p = porg;

	p.print();

	// Edit

	auto f = p[16 .. 19];
//	assert(f == "fox");
	f.print();

	auto p2 = p[0 .. 16] ~ "cat" ~ p[19 .. p.length];
	auto p3 = (p[16 .. 19] = "cat");
//	assert(p2 == p3 && p2[16 .. 19] == "cat");

	p2.print();
	p3.print();

	auto p4 = (p[16] = 'b');
//	assert(p4[16 .. 19] == "bat");
	p4.print();

	// Large text

	auto pp = p2 ~ p2 ~ p2 ~p2;
	auto ppp = pp ~ pp ~ pp ~ pp;
	auto pppp = ppp ~ ppp ~ ppp ~ ppp;
	auto ppppp = pppp ~ pppp ~ pppp ~ pppp;
	auto pppppp = ppppp ~ ppppp ~ ppppp ~ ppppp;
	auto ppppppp = pppppp ~ pppppp ~ pppppp ~ pppppp;
	auto pppppppp = ppppppp ~ ppppppp ~ ppppppp ~ ppppppp;
	auto ppppppppp = pppppppp ~ pppppppp ~ pppppppp ~ pppppppp;
	auto pppppppppp = ppppppppp ~ ppppppppp ~ ppppppppp ~ ppppppppp;
	auto ppppppppppp = pppppppppp ~ pppppppppp ~ pppppppppp ~ pppppppppp;
	auto pppppppppppp = ppppppppppp ~ ppppppppppp ~ ppppppppppp ~ ppppppppppp;
	auto ppppppppppppp = pppppppppppp ~ pppppppppppp ~ pppppppppppp ~ pppppppppppp;
	auto pppppppppppppp = ppppppppppppp ~ ppppppppppppp ~ ppppppppppppp ~ ppppppppppppp;

	assert(pppppppppppppp.length == 2_952_790_016);

	// Substring of large text

	auto x = pppppppppppppp[ 387_612_123 .. 597_115_123 ];

	assert(x.length == 209503000);

	auto y = pppppppppppppp[ 17_612_123 .. 17_613_612 ];

	// IO

	foreach (chunk; y) {
		writefln("'%s'", chunk);
	}

	// Lorem ipsum

	auto pars = [
		new CordIString("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus cursus vestibulum lectus. Sed pharetra risus at turpis. Phasellus justo. In lobortis metus id ligula. Vivamus luctus tellus et ipsum. Duis a diam sollicitudin sapien aliquet imperdiet. Aliquam dapibus mattis ante. Pellentesque lacinia, est a interdum commodo, velit sapien vulputate quam, sed suscipit orci augue nec magna. Curabitur nibh. Sed quam. \n"),
		new CordIString("Sed nulla. Donec ut ipsum eu tortor viverra iaculis. Pellentesque commodo, velit at dignissim convallis, urna justo luctus tellus, nec feugiat est quam et ipsum. Cras convallis, nisl ut volutpat consectetur, magna turpis fringilla est, a feugiat odio felis nec enim. Nunc aliquet consectetur ligula. Fusce lacus erat, elementum eu, fringilla sed, rutrum et, est. Nullam fringilla justo a massa. Proin magna. Suspendisse tristique dolor a tellus. Nullam vel justo. Sed vitae nunc. Sed consequat ligula ut justo. Quisque vestibulum enim eget massa. Curabitur orci tortor, vestibulum non, posuere at, imperdiet eu, magna. Vivamus tristique dolor vitae tellus. Nunc interdum. Fusce faucibus arcu quis nibh. Nam diam massa, rutrum ut, sagittis vel, dapibus sit amet, nunc. \n"),
		new CordIString("Morbi fringilla imperdiet lacus. Nullam velit. Aenean enim risus, iaculis id, dignissim in, aliquam quis, purus. Cras imperdiet dui vitae mauris. Nam adipiscing, purus eget gravida molestie, enim eros dignissim velit, et blandit massa nisi eu lacus. Proin mollis fermentum lacus. Sed a justo sit amet ipsum ultricies accumsan. Sed tellus. Nunc et purus. Sed mollis tempor turpis. Sed vel justo quis diam posuere feugiat. Quisque congue. Cras consequat sagittis risus. Integer volutpat convallis lacus. In hac habitasse platea dictumst. \n"),
		new CordIString("Phasellus id enim vel sem facilisis posuere. Duis tellus tortor, consectetur et, commodo in, aliquam eu, turpis. Nullam adipiscing. Phasellus ut enim at lacus adipiscing rutrum. Sed massa. Cras sapien leo, euismod ut, aliquet id, laoreet adipiscing, est. Etiam nec tortor. Donec consectetur sodales velit. Etiam ultrices varius purus. Morbi sem tellus, laoreet sed, tempus a, elementum eu, felis. Nam ultricies molestie eros. Mauris bibendum gravida velit. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam vitae nibh quis nibh scelerisque dapibus. \n"),
		new CordIString("Phasellus ligula purus, aliquam ac, laoreet ac, convallis ac, mauris. Pellentesque eu lorem. Vivamus at neque convallis neque sodales pharetra. Nullam lacus. Fusce ipsum tellus, pulvinar nec, euismod quis, cursus vel, libero. Quisque vitae neque. Mauris suscipit eros eget velit. Nam condimentum lacinia odio. Etiam enim dui, gravida in, commodo et, blandit sit amet, magna. Morbi magna justo, faucibus eget, sodales quis, ultrices in, sem. In in nunc sit amet est faucibus bibendum. Praesent non magna. Aenean lacinia tortor eu sapien. In laoreet mauris suscipit metus. Curabitur hendrerit. Integer leo velit, volutpat eu, consectetur sit amet, egestas ut, tellus. Duis dapibus metus ut odio. Pellentesque in mi et dui accumsan volutpat. Aliquam euismod nibh in libero.\n"),
		new CordIString("Nunc venenatis dolor quis erat. Sed elit quam, porta ut, blandit eu, venenatis ornare, nisi. Sed ut dui vel metus sodales tempus. Phasellus sollicitudin. Maecenas blandit, odio sed lobortis condimentum, sem sem tristique nisl, vel fringilla nibh enim at erat. Sed tellus. Morbi eu mauris eget quam tempus consectetur. Vestibulum non dolor. Duis facilisis ultrices diam. Nullam vitae nulla. Pellentesque ornare aliquam nisl. Sed nec urna. Morbi cursus ante ut dolor. Suspendisse potenti. Aenean convallis. \n"),
		new CordIString("Vestibulum molestie luctus quam. Aenean molestie ligula ut nisi eleifend volutpat. Phasellus nulla nulla, imperdiet id, porttitor id, fringilla id, mauris. Suspendisse lectus. Nulla auctor mi ullamcorper ante condimentum pellentesque. Nulla consectetur. Donec et mauris at massa gravida blandit. In ullamcorper arcu id enim. Integer placerat lectus ac metus. Phasellus pretium. Sed quis felis. Nunc aliquet, dolor vitae molestie lacinia, nisi nibh dapibus ipsum, vel imperdiet est felis vitae nulla. Proin consequat quam nec velit. Suspendisse aliquet. Suspendisse potenti. Morbi nisi. \n"),
		new CordIString("Suspendisse sollicitudin. Phasellus in nisi viverra lacus tempor consequat. Aenean vitae lacus at erat commodo auctor. Sed justo. Sed arcu tortor, fringilla eu, mattis sit amet, pellentesque sit amet, nisl. Suspendisse sit amet risus. Nam sollicitudin lorem. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Nunc malesuada sem vitae risus. Integer sem tortor, vehicula id, venenatis eu, porttitor sit amet, nisl. Duis magna leo, lacinia eget, aliquam sit amet, pellentesque at, metus. Pellentesque fermentum ipsum ut lectus. \n"),
		new CordIString("Duis volutpat vestibulum orci. Fusce bibendum arcu at velit. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Fusce ut lacus quis odio interdum lacinia. Nulla sagittis odio vitae leo. Cras accumsan consectetur diam. Aliquam nec quam. Ut eleifend sem malesuada ipsum. Fusce id nibh. Etiam justo arcu, blandit eget, interdum ac, bibendum sit amet, est. Maecenas ultricies, sapien eget faucibus suscipit, dui quam euismod augue, convallis dictum enim nisi tempus nisi. Quisque eleifend aliquam ante. Donec pellentesque convallis tellus.\n")
	];

	RBCord lorem = RBCord();

	foreach (par; pars) {
		lorem = lorem ~ par ~ "\n";
	}
	
	foreach (chunk; lorem) {
		writef("'%s'", chunk);
	}


	// Balancing

	RBCord r = p;
	int prev_depth = 0;

	for (int i = 0; i < 1000; i++) {
		r = r ~ (ts(i) ~ ": ") ~ p;
		int depth = r.get_depth();
		if (depth <= prev_depth) {
			writefln("balancing occured, iteration %d, depth %d, previous %d", i, depth, prev_depth);
		}
		prev_depth = depth;
	}

	writefln("depth = ", r.get_depth());
}
