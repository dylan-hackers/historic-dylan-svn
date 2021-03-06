Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <ast-constant> (<ast-declarator>)
  slot constant-value :: false-or(<ast-expression>) = #f, init-keyword: value:;
  constant slot constant-expression-type :: <ast-type>, required-init-keyword: type:;
end class;

define method can-be-redefined? (constant :: <ast-constant>)
  #t;
end method;



