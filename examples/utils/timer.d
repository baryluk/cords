/** Timer for profiling, benchmaring and other timings
 * Authors: Witold Baryluk <baryluk@smp.if.uj.edu.pl>
 * License: BSD, see LICENSE file in root directory
 * Copyright: Witold Baryluk 2007
 */
module timer;

import std.date;
import std.stdio;

import log;

/** Timer class for profiling code
 *
 * Usage with scope storage type:
 * ---
 * import onp.utils.timer : Timer;
 * import onp.utils.log : logging;
 * void main() {
 *   logging = true;
 *   auto a = read_matrix("bigmatrix.dat");
 *   matrix ai;
 *   {
 *     scope t = Timer("matrix inversion");
 *     ai = a.inv();
 *   }
 * }
 * --- 
 *
 * Code above will gives output similar to this
 * ---
 * matrix inversion started.
 * matrix inversion done. elapsed time = 1293 ktics
 * ---
 *
 * If You can create scope, you can call destructor explicitly:
 * ---
 *  auto t = Timer("reading");
 *  auto x = read();
 *  delete t;
 * ---
 */
class Timer {
	// Gets time in milisecond resolution
	//static d_time getCount2() { return getUTCtime(); }

	/// Gets CPU tic (x86)
	static long getCount() {
		asm {
			naked	;
			rdtsc	;
			ret	;
		}
	}
	//d_time starttime;

	///
	long starttime;

	///
	string msg;

	/// Starts timer
	this() { starttime = getCount();}

	/// Starts timer and adds name to it
	this(string msg0) {
		this(); msg = msg0;
		log(msg ~ " started.");
	}

	/// Stops timer and prints (using log function) number of tics
	~this() {
		auto t = (getCount() - starttime)/1000.0;
		if (msg !is null) {
			log(msg ~ " done. elapsed time = %.0f ktics", t);
			//log(msg ~ " done. elapsed time = %.3f s", t);
		} else {
			log("elapsed time = %.0f ktics", t);
			//log("elapsed time = %.3f s", t);
		}
	}
}
