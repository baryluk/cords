module readme;

import utils.timer : Timer;
import utils.log : logging;

string g(string x) {
	return x ~ " - <b>" ~ x ~ "</b>";
}


string extraargs(string s) {
	return "s=2";
}


string generate_1(string title, string[] points) {
	string s = "<head><title>" ~ title ~ "</title></head><body>"; // line a
	if (points.length > 0) {
		s ~= "<ul>"; // line b
		foreach (point; points) { // line c
			s ~= "<li>" ~ g(point) ~ "</li>"; // line d
		}
		s ~= "</ul>"; // line e
	}
	s ~= "</body>"; // line f
	
	return "<html "~extraargs(s)~">" ~ s ~ "</html>"; // line g
}




string extraargs(char[] s) {
	return "s=2";
}

size_t g_size(string x) {
	return x.length + " - <b>".length + x.length + "</b>".length;
}


size_t generate_2_size(string title, string[] points) {
	size_t s = "<head><title>".length + title.length + "</title></head><body>".length; // line a
	if (points.length > 0) {
		s += "<ul>".length; // line b
		foreach (point; points) { // line c
			s += "<li>".length + g_size(point) + "</li>".length; // line d
		}
		s += "</ul>".length; // line e
	}
	s += "</body>".length; // line f
	
	return "<html ".length+50+">".length + s + "</html>".length; // line g. estimated size of extraargs(s) == 50
}
string generate_2(string title, string[] points) {
	char[] s;
	size_t s_size = generate_2_size(title, points);
	s.length = s_size;
	size_t already = 0;
	void append(string str) {
		if (already+str.length >= s_size) {
			s.length = s_size = already+str.length;
		}
		s[already .. already+str.length] = str[];
		already += str.length;
	}
	append("<head><title>" ~ title ~ "</title></head><body>"); // line a
	if (points.length > 0) {
		append("<ul>"); // line b
		foreach (point; points) { // line c
			append("<li>" ~ g(point) ~ "</li>"); // line d
		}
		append("</ul>"); // line e
	}
	append("</body>"); // line f
	
	s.length = already; // line h
	
	s = "<html "~extraargs(s)~">" ~ s ~ "</html>"; // line g
	
	return cast(invariant)s;
}


import cords : Cord, RBCord, CordI;


RBCord g(RBCord x) {
	return x ~ " - <b>" ~ x ~ "</b>";
}

RBCord extraargs(RBCord s) {
	return RBCord("s=2");
}

RBCord generate_3(string title, string[] points) {
	RBCord s = "<head><title>" ~ title ~ "</title></head><body>"; // line a
	if (points.length > 0) {
		s = s ~ "<ul>"; // line b
		foreach (point; points) { // line c
			s = s ~ "<li>" ~ g(point) ~ "</li>"; // line d
		}
		s = s ~ "</ul>"; // line e
	}
	s = s ~ "</body>"; // line f
	
	s = "<html "~extraargs(s)~">" ~ s ~ "</html>"; // line g
	//return s.toString(); // allocate exactly s.length bytes and fill it
	return s;
}

import std.stdio : writefln;

import std.c.stdlib : atoi;

int main(string[] args) {
	auto title = "Simple test";

	string[] list;

	int c1 = atoi(args[1].ptr);
	int c2 = atoi(args[2].ptr);
	for (int i = 0; i < c1; i++) {
		auto temp = new char[c2];
		foreach(int j, ref z; temp) { 
			z = 'A' + j % ('Z'-'A');
		}
		list ~= [cast(string)temp];
	}

	logging = true;

	{
	scope t = new Timer("Standard");
	auto g = generate_1(title, list);
	writefln("%s", g.length);
	}
	{
	scope t = new Timer("With estimation of size");
	auto g = generate_2(title, list);
	writefln("%s", g.length);
	}
	{
	scope t = new Timer("Cords");
	auto g = generate_3(title, list);
	writefln("%s", g.length);
	}

	return 0;
}
