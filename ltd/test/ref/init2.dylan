//  for non-allegro CL systems.
// LTD: No macros.
#"declaim";

// LTD: Function FIND-PACKAGE not yet implemented.
find-package(#"excl")
 | // LTD: Function MAKE-PACKAGE not yet implemented.
   make-package(#"excl");

// LTD: Function LOAD not yet implemented.
load("mma");

// LTD: Function USE-PACKAGE not yet implemented.
use-package(#"mma");

"(in-package mma)";

// LTD: Function SHADOW not yet implemented.
shadow(#"set");

// LTD: Function SHADOW not yet implemented.
shadow(#"exp");

// LTD: Function SHADOW not yet implemented.
shadow(#"log");

// LTD: Function SHADOW not yet implemented.
shadow(#"sin");

// LTD: Function SHADOW not yet implemented.
shadow(#"cos");

// LTD: Function SHADOW not yet implemented.
shadow(#"tan");

// LTD: Function SHADOW not yet implemented.
shadow(#"sinh");

// LTD: Function SHADOW not yet implemented.
shadow(#"cosh");

// LTD: Function IMPORT not yet implemented.
import(#"declaim");

// LTD: Function REQUIRE not yet implemented.
require("ucons1");

//  should really be uconsalt copied to this file
// LTD: Function REQUIRE not yet implemented.
require(#"math-parser", "parser");

// LTD: Function REQUIRE not yet implemented.
require("stack1");

// LTD: Function REQUIRE not yet implemented.
require("disp1");

// LTD: Function REQUIRE not yet implemented.
require(#"math-eval", "eval");

// LTD: Function REQUIRE not yet implemented.
require("poly");

// LTD: Function REQUIRE not yet implemented.
require("rat1");

// LTD: Function REQUIRE not yet implemented.
require("simp1");

// LTD: Function REQUIRE not yet implemented.
require("pf");

// LTD: Function REQUIRE not yet implemented.
require("match");

// LTD: Function REQUIRE not yet implemented.
require("diffrat");

