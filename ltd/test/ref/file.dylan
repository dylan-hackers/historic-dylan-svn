//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		File.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
"(in-package dtp)";

define module dtp export dtp-load; end module dtp;

// ----------------------------------------------------------------------------
// 
// 	Files can contain
// 	1. ":theory <theory-name>"
// 	2. ":includes <theory-name>"
// 	3. ":label <value> <label-structure-name>"
// 	4. ":nolabel"
// 	5. Logical sentences
// 
// 	Keywords apply to subsequent sentences
define method dtp-load (filename)
  if (instance?(filename, <string>))
    if (~ cl-find('.', filename))
      filename := concatenate-as(<string>, filename, ".dtp");
    end if;
    if (~ (cl-find('/', filename) | cl-find(':', filename)))
      filename := concatenate-as(<string>, *dtp-logic-directory*, filename);
    end if;
  end if;
  with-open-file (p = (filename, direction: #"input"))
    let *package* = *dtp-package*;
    let theory = #f;
    let theories = #f;
    let keyword
        = // LTD: Function READ not yet implemented.
          read(p, #f, #f);
    block (return)
      for (until empty?(keyword))
        theory
         := // LTD: Function READ not yet implemented.
            read(p, #f, #f);
        if (empty?(theory))
          // LTD: Function LOOP-FINISH not yet implemented.
          loop-finish();
        end if;
        theories := add!(theory, theories);
        begin
          let label = #f;
          let sentences = #f;
          for (sexp = // LTD: Function READ not yet implemented.
                      read(p, #f,
                           #f) then // LTD: Function READ not yet implemented.
                                    read(p, #f, #f),
               until empty?(sexp) | sexp == #"theory")
            select (sexp)
              #"includes"
                 => let included-theory
                        = // LTD: Function READ not yet implemented.
                          read(p, #f, #f);
                     if (included-theory)
                       includes(theory, included-theory);
                     end if;
              #"label"
                 => let value
                        = // LTD: Function READ not yet implemented.
                          read(p, #f, #f);
                     let structure-name
                         = // LTD: Function READ not yet implemented.
                           read(p, #f, #f);
                     let structure = #f;
                     structure := get-label-structure(structure-name);
                     if (structure)
                       label
                        := make-label(value: value, structure: structure);
                     else
                       format-out("\nError: Label structure %S not known\n",
                                  structure-name);
                     end if;
              #"nolabel"
                 => label := #f;
              otherwise
                 => push!(pair(sexp, label), sentences);
            end select;
          finally
            make-theory-from-sentences(theory, reverse(sentences));
            #f;
          end for;
        end;
      finally
        return(reverse(theories));
        #f;
      end for;
    end block;
  end with-open-file;
end method dtp-load;

// ----------------------------------------------------------------------------
"eof";

