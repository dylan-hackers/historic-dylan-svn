DylanLibs - Dylan Code Collection
=================================

The DylanLibs project, or Dylan Code Collection, is a repository of
libraries written in the Dylan programming language.

I've developed a number of re-usable libraries written in Dylan and
made them available through my web site [1]. To make maintenance
easier, and to provide the ability for others to help with the
development, I decided to create a SourceForge project to manage
them. The result is this project [2].

Some of the libraries available work only with Functional
Developer[3], a commercial Dylan system, while others work with
Gwydion Dylan[4], an Open Source Dylan implementation.

[1] http://www.double.co.nz/dylan
[2] http://dylanlibs.sourceforge.net
[3] http://www.functionalobjects.com
[4] http://www.gwydiondylan.org

Mailing List
============

A mailing list exists to enable discussion about Dylanlibs
projects. CVS activity also goes to this mailing list. Details on
subscribing and access to the list archives are available at:

  http://lists.sourceforge.net/lists/listinfo/dylanlibs-discuss

Available Libraries
===================

The libraries in the dylanlibs collection are listed below. Included
with the library description are the Dylan implementations it can be
compiled under.

1) MSXML Wrappers
=================

Dylan Implementations: 
  Functional Developer

msxml3-dispatch 
  Automatically generated dispatch interfaces for the Microsoft XML
  Parser V3.0, release. You may need to change the directory in the
  type-library.spec file to point to the correct location of the
  MSXML3.DLL file on your system. This generated library is quite
  low-level and you might not want to use it directly. 

msxml3 
  Contains a higher level interface to msxml3-dispatch. Using this
  library you need never know you are dealing with COM and types. All
  releasing and allocating of resources is handled automatically and
  it provides an OO hierarchy of nodes, elements, etc. Not everything
  is implemented or tested but 99% of it is there I think. 

msxml-viewer 
  An example program that uses the msxml3 library. It lets you select
  an XML file to load and displays the nodes in a DUIM tree
  control. You can optionally provide an xpath expression, hit the
  xpath button, and see the filtered list of nodes that matched the
  xpath expression. 

You will need to have the following installed on your system to use
the MSXML wrappers:

  Microsoft XML Parser V3.0 
  http://download.microsoft.com/download/xml/Install/3.10/W98NT42KMe/EN-US/msxml3sp1.EXE

  Microsft XML SDK (for documentation) 
  http://download.microsoft.com/download/xml/SDK/3.0/WIN98Me/EN-US/xmlsdk.exe

2) double-base64
================

Dylan Implementations:
  Functional Developer
  Gwydion Dylan

Exports methods to encode strings and decode strings to/from base64.

3) Examples
===========

Contains various example programs demonstrating aspects of Dylan. See
the examples.txt file in the examples directory for details.

4) Duim
=======

Various libraries that extend or build upon DUIM. See the duim.txt
file in the duim directory for details.

5) Win32
========

Libraries that wrap native Win32 functionality using C-FFI. See the
win32.txt file in the win32 directory for details.

6) lazy-evaluation
==================

A library that provides lazy evaluation functionality for
Dylan. Implements equivalents to Scheme's delay and force, along with
lazy lists, lazy mapping, etc.

7) parser-combinator
====================

A library of lazy parser combinators for Dylan, implemented along the lines
for the parser combinators described in the Clean book:
  ftp://ftp.cs.kun.nl/pub/Clean/papers/cleanbook/II.05.ParserCombinators.pdf

Not yet fully implemented!

8) net
======

Various net and internet protocols, socket extensions, xml-rpc, http,
etc. See the net.txt file in the net directory for details.

9) bin
======

Contains DLL's and third party libraries used by some of the
projects. See the bin.txt file in that directory for details.

Chris Double.
18 June 2001.


