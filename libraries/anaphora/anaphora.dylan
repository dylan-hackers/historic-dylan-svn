module:    anaphora
synopsis:  Statements that convert their result to 'it', an unhygenic variable
see:       On Lisp (ch 14), by Paul Graham, for original Lisp implementation
author:    Douglas M. Auclair
copyright: (c) 2002, LGPL

define macro aif
  { aif(?:expression) ?b1:body else ?b2:body end }
    => { let ?=it = ?expression;
        if(?=it) ?b1 else ?b2 end }
  { aif(?:expression) ?:body end }
    => { let ?=it = ?expression;
        if(?=it) ?body end }
end macro aif;
