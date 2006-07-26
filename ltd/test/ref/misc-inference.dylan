//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Misc-Inference.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
"(in-package dtp)";

// ----------------------------------------------------------------------------
// 
// 	Generic functions
// Attempt to find another <answer> for NODE
define generic expand (node) ;

// Test whether there might be more answers from NODE
define generic exhausted-p (node, #key #all-keys) ;

// Inform NODE that ANSWER is another answer
define generic propagate (answer, node) ;

// ----------------------------------------------------------------------------
"eof";

