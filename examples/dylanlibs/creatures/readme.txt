Creatures - Dylan Code Collection
=================================

The Creatures package of the Dylan Code Collection is a collection of
Dylan libraries and applications that work with the Creatures
artificial life game [1]. In particular, they target Creatures 3.

Creatures 3 is an artificial life simulation more than it is a
game. The 'creatures' that wander through the artificial world have a
complete biochemical system taht works similar to real life. They have
a 'digital DNA' and a neural network brain system. Through breeding,
and learning as they live, you can examine how artificial life can be,
well, life like.

Included in Creatures 3 is a complete world that can be modified via
an Agent scripting language called CAOS (Creatures Agent Object
Script). 

To facilitate exploring, and extending, the Creatures 3 world I
developed a series of programs that allowing inserting CAOS commands
into the world, editing the genomes of the creatures, and otherwise
examining the internals of the game engine of this fascinating
simulation.

Most of these programs are written in Dylan and I'm now providing them
here as part of the Dylan Code Collection to enable other Creatures
developers to use and extend the tools, and to provide more Dylan code
for people interested in learning Dylan to examine.

With the release of the free Creatures program, Docking Station, you
can now use Dylan with the free release to explore artificial life.

For more information about Creatures and Docking Station see [1] and
[2]. For operationation details on the programs that are in this
package see [3].

[1] http://www.creatures.co.uk
[2] http://www.double.co.nz/creatures
[3] http://www.double.co.nz/creatures/creatures3/index.htm

Libraries
=========

The following libraries are currently available:

1) c3-engine
============
A library that allows sending CAOS commands to a running Creatures 3
game, to add CAOS scripts or retrieve information about the artificial
world currently running. Most of the Creatures 3 programs in this
collection use this library in some manner.

2) c3-genome-injector
=====================

This program allows injecting an egg into the currently running
Creatures 3 world. You can key-in the moniker of the genome file that
contents the genetic DNA of the creature, and the gender of the
creature when the egg hatches. Pressing inject causes the egg to
appear in the world.

3) c3-injector
==============

A program that allows injecting any CAOS command into the Creatures 3
engine and retrieving the result.

4) c3-lobe-study
================

Shows details of the information going through the currently selected
creatures brain, including what it is looking at, what decisions are
planned, status of the neural-net, etc.

5) c3-gene
==========

Contains an example genome file (*.gen) from the Creatures 3 game and
the genome description file for it (*.gno). The various subdirectories
of this directory contain genetic editors and viewers and this example
genome can be used to test these programs.

5a) creatures-genes
===================

A library allowing reading of a genome file (*.gen) and converting the
file into a object hierarchy.

5b) gno-file
============

A library that reads the genome description file (*.gno).

5c) ui-creatures-genes
======================

A set of DUIM panes that can be used to display the objects in the
creatures-genes library.

5d) genetics-editor
===================

A program that allows loading of *.gen and *.gno files, viewing the
genetic information, editing it and saving it back to a genome file.

5e) gene-compare
================

A program that allows loading of a genome file and displaying all the
information in it in a textual description. A second genome file can
be loaded and a comparison between the two displayed. Very useful for
comparing children and parents to see what mutations and crossovers
occurred during breeding.

Notes
=====

These programs were some of the first Dylan programs I ever wrote and
may not represent good Dylan style. Starting from this base of
learning Dylan I've sinced hacked at them quite a bit, possibly
resulting in some rather unusually hard to understand code. One day I
may improve them to be a bit tidier :-).

-- 
Chris Double.
01 May 2001.

