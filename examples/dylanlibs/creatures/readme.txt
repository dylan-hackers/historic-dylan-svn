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
[2].

[1] http://www.creatures.co.uk
[2] http://www.double.co.nz/creatures

Libraries
=========

The following libraries are currently available:

1) c3-engine
============
A library that allows sending CAOS commands to a running Creatures 3
game, to add CAOS scripts or retrieve information about the artificial
world currently running. Most of the Creatures 3 programs in this
collection use this library in some manner.

Chris Double.
01 May 2001.

