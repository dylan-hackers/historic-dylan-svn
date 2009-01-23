<%dsp:taglib name="od"/>

<od:standard-header>12 Dylan Fragments</od:standard-header>

<!-- todo
function example($name, $file) {
  echo "<LI><A HREF=\"fragments/" . $file . ".dsp\">" . $name . "</A>";
}

function example_and_note($name, $file, $note) {
  echo "<LI><A HREF=\"fragments/" . $file . ".dsp\">" . $name . "</A>" .
       " (" . $note . ")";
}

function unfinished($name) {
  echo "<LI>" . $name;
}

-->

<P>The following examples show off various interesting features of
Dylan. Most of these should work just fine under recent versions of
Gwydion, and none of them rely on hypothetical external routines.
Everything but the C interfaces should work in Open Dylan, too.</P>

<P><STRONG>Basic Language Features</STRONG></P>

<UL><?php
example("Everything's a Value", "values");
example("Keyword Arguments", "keywords");
example("Multiple Values", "multiple-values");
unfinished("References & Garbage Collection");
?></UL>

<P><STRONG>Efficiency</STRONG></P>

<UL><?php
unfinished("Optional Type Declarations");
example("Limited Types", "limited-types");
?></UL>

<P><STRONG>Objects and Generic Functions</STRONG></P>

<UL><?php
example("Classes and <CODE>make</CODE>", "classes");
example("Getters and Setters are Functions", "getters-and-setters");
unfinished("Generic Functions");
?></UL>

<P><STRONG>Advanced Features</STRONG></P>

<UL><?php
unfinished("Conditions (a.k.a. Exceptions)");
example_and_note("Closures", "closures", "and interfacing to C");
example_and_note("Macros", "macros", "and interfacing to C");
?></UL>

<od:standard-footer/>
