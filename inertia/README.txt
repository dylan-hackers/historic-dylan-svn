library:   inertia
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details


INTRODUCTION
------------

Inertia is a vector UI library written in Dylan.  It's a concept I've been
pursuing, in other incarnations, for the past few years.  Development
started in October 2005, so this is a very early release.  I'm open to any
comments or suggestions.

To compile inertia, you may need to edit inertia-main.dylan, and change
the c-include line that imports glu.h.  This is a temporary fix since I
could not get gluTessCallback() to register a callback without crashing.


BUILDING INERTIA
----------------

Building is simple on un*x or cygwin:

make
make install

If you want to see a quick demo, run the test program:

tests/inertia-test


MISCELLANIOUS
-------------

You can find related mockups and other implementations of inertia here:

http://www.mike-austin.com/interface-design/index.html

