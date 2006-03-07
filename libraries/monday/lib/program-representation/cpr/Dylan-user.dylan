Module: Dylan-user


define library cpr
  use Common-Dylan;
  use Source-Location;
  use IO;
  use System;
  
  use regular;               // shouldn't need this directly 
  use grammar;               // shouldn't need this directly 
  use simple-parser;
            

  export cpr-preprocessor,
         cpr;
end library;
          
define module cpr-preprocessor
  
  // Preprocessor dialects
  create
    <C-preprocessor-dialect>,
    $C90-C-preprocessor-dialect,
    $C99-C-preprocessor-dialect,
    $MSVC6-C-preprocessor-dialect;
            
  // Translation units
  create
    <C-preprocessing-translation-unit-representation>,
    preprocessing-header-search-path,
    preprocessing-system-header-search-path,
    preprocessing-macros,

    preprocessor-define,
    preprocessor-undefine,

    preprocess-C-source-file,
    preprocess-C-header-file,
    preprocess-C-system-header-file,
    preprocess-C-stream,

    preprocessor-token-string;
            
end module;
          
define module cpr
  use cpr-preprocessor,
    export: all;
  
  // Language dialects
  create
    <C-language-dialect>,
    $C90-C-language-dialect,
    $C99-C-language-dialect,
    $MSVC6-C-language-dialect;
            
  // Platforms
  create
    <C-translation-platform>,
    platform-system-include-path,

    platform-char-bits,
    platform-char-signed?,

    platform-type-size,
    platform-type-alignment;
            
  // Translation units
  create
    <C-translation-unit-representation>,
    translation-unit-external-declarations,

    parse-C-source-file,
    parse-C-header-file,
    parse-C-system-header-file;
            
  // Types
  create
    <C-type-representation>,
    <C-integer-type-representation>,
    <C-enum-type-representation>,
    <C-float-type-representation>,
    <C-array-type-representation>,
    <C-function-type-representation>,
    <C-struct/union-type-representation>,
    <C-pointer-type-representation>,
    $C-void*-representation,
    $C-char*-representation,
    $C-const-char*-representation;
            
  // Expressions
  create
    <C-expression-representation>,
    expression-type,

    <C-constant-expression-representation>,
    <C-string-literal-expression-representation>,
    expression-value,

    <C-variable-reference-expression-representation>,
    expression-variable,

    <C-function-reference-expression-representation>,

    expression-operator,

    <C-unary-expression-representation>,
    expression-unary-operand,

    <C-binary-expression-representation>,
    expression-binary-left,
    expression-binary-right,

    <C-conditional-expression-representation>,
    expression-conditional-condition,
    expression-conditional-true,
    expression-conditional-false,

    <C-cast-expression-representation>,
    expression-cast-operand,

    <C-sizeof-type-expression-representation>,
    expression-sizeof-type,

    <C-function-call-expression-representation>,
    expression-call-function,
    expression-call-arguments,

    <C-member-expression-representation>,
    expression-member-operand,
    expression-member-name,

    print-C-expression,
    $precedence-level-assignment-expression,
    $precedence-level-expression;
            
end module;
          
define module interned-string
  use common-dylan;
  use byte-vector;
  export intern-string;
end module;
          
define module hierarchical-table
  use common-dylan;
  export <hierarchical-table>;
end module;
          
define module cpr-internals
  use Common-Dylan, exclude: { format-to-string };
  use Locators;
  use cpr-preprocessor;
  use cpr;
use format-out;
  
use streams;
use file-system;
use source-location;
use source-location-rangemap;
use source-location-conditions;
            
  use byte-vector;
  use simple-parser;
  use simple-lexical-scanner;
            
use interned-string;
            
use hierarchical-table;
            
  use simple-parser-automaton;
            
use date;
            
  use print;
  use pprint;
  use format;
            
end module;
          
