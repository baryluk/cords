/** Simple conditional logging using writefln
 * Authors: Witold Baryluk <baryluk@smp.if.uj.edu.pl>
 * License: BSD, see LICENSE file in root directory
 * Copyright: Witold Baryluk 2007
 */
module log;

import std.stdio;

/** should logging be enabled? */
bool logging = false;

/** log function */
void log(T...)(T args) {
	if (logging)
		writefln(args);
}
