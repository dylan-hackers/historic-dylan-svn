documented: #t
module: dylan-user
copyright: Copyright (C) 1994, Carnegie Mellon University
	   All rights reserved.
	   This code was produced by the Gwydion Project at Carnegie Mellon
	   University.  If you are interested in using this code, contact
	   "Scott.Fahlman@cs.cmu.edu" (Internet).
rcs-header: $Header: /home/housel/work/rcs/gd/src/tools/melange/exports.dylan,v 1.12 1996/10/06 12:41:18 nkramer Exp $

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// All rights reserved.
//
//======================================================================

//======================================================================
// Library "melange" contains the complete functionality (except possibly for
// user extensions to name mappers) of the Melange interface generator (Mindy
// version).  Since it is intended as a stand-alone program, there is no need
// to export most of its functionality.  "Name-mappers" is exported to
// facilitate user extension of the program.  See "name-map.dylan" or the
// Melange documentation for further instructions on such extension.
//======================================================================
// Melange versions:
//   b1.0: (04/28/95)
//     Initial "beta" release
//   b1.1: (05/17/95)
//     Added "superclasses" option for structure and union clauses
//     Bug fixes:
//       Allow enumeration literals in compile time expressions (including
//         specification of other literal values)
//       Show token string rather than token type in error messages
//       Report line numbers rather than character numbers in error messages
//       Report name of interface files in error messages
//       Fix handling of empty strings in interfaces
//       Fix handling of CPP '#include "foo"'
//       Allow CPP '#pragma'
//       Fix handling of CPP foo##bar
//   b1.2: (10/25/95)
//     Improved protability handling:
//       portable size constants
//       multiple alignment models
//     Improved vector handling:
//       vector operations now only apply to subclasses of <c-vector>
//       added "pointer" clause to interface declaration.
//       <c-strings> now have more correct (and documented) behavior
//     Bug fixes:
//       Fixed various routines to work if "members" is #f.
//       Fixed handling of equated typedefs.
//       Fixed bug in explit-ony? keyword for mapped-name.
//       Fixed handling of types which are mapped to themselves.
//======================================================================

define library melange
  use dylan;
  use string-extensions;
  use collection-extensions;
  use regular-expressions;
  use streams;
  use standard-io;
  use format;
  use melange-c;
  export
    name-mappers;
end library melange;

define module int-lexer
  use dylan;
  use extensions;
  use self-organizing-list;
  use string-conversions;
  use regular-expressions;
  use character-type;
  use streams;
  export
    <tokenizer>, get-token, unget-token, <token>, value, string-value,
    generator, parse-error, position, <error-token>, <identifier-token>,
    <integer-token>, <eof-token>, <true-eof-token>, <keyword-token>,
    <symbol-literal-token>, <string-literal-token>, <comma-token>,
    <semicolon-token>, <lbrace-token>, <rbrace-token>, <arrow-token>,
    <define-token>, <interface-token>, <end-token>, <include-token>,
    <object-file-token>, <define-macro-token>, <undefine-token>,
    <name-mapper-token>, <import-token>, <prefix-token>, <exclude-token>,
    <rename-token>, <mapping-token>, <equate-token>, <superclass-token>,
    <all-token>, <function-token>, <map-result-token>, <equate-result-token>,
    <ignore-result-token>, <map-argument-token>, <equate-argument-token>,
    <input-argument-token>, <output-argument-token>,
    <input-output-argument-token>, <struct-token>, <union-token>,
    <pointer-token>,
    <constant-token>, <variable-token>, <getter-token>, <setter-token>,
    <read-only-token>, <seal-token>, <seal-functions-token>, <boolean-token>,
    <sealed-token>, <open-token>, <inline-token>, <value-token>,
    <literal-token>, <mindy-inc-token>;
end module int-lexer;

define module int-parse
  use dylan;
  use extensions;
  use self-organizing-list;
  use int-lexer;
  export
    parse, <parse-state>, include-file, object-files, mindy-include-file,
    container-options, macro-defines, macro-undefines, clauses,
    <container-options>, name-mapper, imports, prefix, exclude, rename,
    mappings, equates, read-only, seal-string, <clause>, <function-clause>,
    <struct-clause>, <union-clause>, <pointer-clause>,
    <constant-clause>, <variable-clause>,
    name, options, <undefined>, undefined;
end module int-parse;

define module name-mappers
  use dylan;
  use character-type;
  export
    map-name, hyphenate-case-breaks;
end module name-mappers;

define module define-interface
  // From Dylan
  use dylan;
  use extensions;		// required for "main" (as well as key-exists?)
  use %hash-tables;
#if (~mindy)
  use System,
     import: {import-string,
	      copy-bytes, call-out, c-expr, buffer-address, <raw-pointer>,
	      pointer-deref};
#endif

  // From string-extensions
  use regular-expressions;
  use substring-search;
  use character-type;

  // From streams
  use streams;
   
  // From standard-io
  use standard-io;

  // local packages
  use int-lexer;
  use int-parse, rename: {rename => renames};
  use c-lexer, import: {include-path, open-in-include-path};
  use c-declarations,
    rename: {parse => c-parse, <parse-state> => <c-parse-state>};
  use name-mappers;
  use portability;
end module define-interface;

