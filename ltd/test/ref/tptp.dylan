//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		TPTP.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
// 
// 	PURPOSE
// 	Interface to TPTP (Thousands of Problems for Theorem Provers).
// 	TPTP is available on the World Wide Web at
// 		http://wwwjessen.informatik.tu-muenchen.de/~suttner/tptp.html
"(in-package dtp)";

define module dtp export convert-tptp, tptp-load; end module dtp;

// ----------------------------------------------------------------------------
// 
// 	Public
// ----------------------------------------------------------------------------
define method convert-tptp (#key category = #f, file = #f, overwrite = #f)
  // Convert one (or multiple) files from TPTP format to KIF format
  if (instance?(category, <string>) & instance?(file, <string>))
    tptp-to-kif(category, file, overwrite, #t);
  elseif (instance?(category, <string>) & file == #t)
    format-out("Converting all files in %S\n", category);
    for (f in // LTD: Function DIRECTORY not yet implemented.
              directory(tptp-pathname(category)))
      tptp-to-kif(category,
                  // LTD: Function PATHNAME-NAME not yet implemented.
                  pathname-name(f),
                  overwrite, #t);
    end for;
  elseif (instance?(category, <string>))
    begin
      format-out("\nAvailable files:\n  ");
      for (fl in sort!(map(// LTD: Function PATHNAME-NAME not yet implemented.
                           pathname-name,
                           // LTD: Function DIRECTORY not yet implemented.
                           directory(tptp-pathname(category))),
                       test: \<),
           flnum = copy-sequence(fl, 3) then copy-sequence(fl, 3),
           count from 0)
        if (count = 6) count := 0; format-out("\n  "); end if;
        (method (s, #rest args)
           apply(maybe-initiate-xp-printing,
                 method (xp, #rest args)
                   using-format(xp, "~10A", pop!(args));
                   if (args) copy-sequence(args); end if;
                 end method,
                 s, args);
         end method)(#t, flnum);
      end for;
    end;
    format-out("\nFile? ");
    file := read-line(*standard-input*, nil);
    file := as-uppercase!(file);
    file := concatenate-as(<string>, category, file);
    tptp-to-kif(category, file, overwrite, #t);
  elseif (category == #t)
    format-out("Converting all files in all categories\n");
    for (d in // LTD: Function DIRECTORY not yet implemented.
              directory(tptp-pathname()))
      convert-tptp(// LTD: Function PATHNAME-NAME not yet implemented.
                   pathname-name(d),
                   #t, overwrite);
    end for;
  else
    begin
      format-out("\nAvailable directories:\n");
      for (dirname in sort!(map(// LTD: Function PATHNAME-NAME not yet implemented.
                                pathname-name,
                                // LTD: Function DIRECTORY not yet implemented.
                                directory(tptp-pathname())),
                            test: \<),
           count from 0)
        if (count = 15) count := 0; format-out("\n"); end if;
        format-out("  %S", dirname);
      end for;
    end;
    format-out("\nDirectory? ");
    category := read-line(*standard-input*, nil);
    category := as-uppercase!(category);
    convert-tptp(category, #f, overwrite);
  end if;
end method convert-tptp;

// ----------------------------------------------------------------------------
define method tptp-load (pathname)
  if (instance?(pathname, <string>))
    pathname := kif-pathname(pathname);
  end if;
  with-open-file (p = (pathname, direction: #"input"))
    fluid-bind (*package* = *dtp-package*)
      let sentences = make(<deque>);
      for (sexp = // LTD: Function READ not yet implemented.
                  read(p, #f,
                       #f) then // LTD: Function READ not yet implemented.
                                read(p, #f, #f),
           while sexp)
        push-last(sentences, pair(sexp, #f));
      finally
        make-theory-from-sentences(#"tptp", sentences);
        sentences;
      end for;
    end fluid-bind;
  end with-open-file;
end method tptp-load;

// ----------------------------------------------------------------------------
// 
// 	Private
// ----------------------------------------------------------------------------
define method tptp-to-kif (dom, file, overwrite, comments)
  // Convert FILE from domain DOM, OVERWRITE old dest, keep COMMENTS
  block (return-from-tptp-to-kif)
    let source-path = tptp-pathname(dom, file);
    let dest-path = kif-pathname(file);
    if (~ overwrite
         & // LTD: Function PROBE-FILE not yet implemented.
           probe-file(dest-path))
      if (~ // LTD: Function Y-OR-N-P not yet implemented.
            y-or-n-p("File ~A~%  already exists.  Overwrite (y/n)? ",
                     dest-path))
        format-out("\nTPTP conversion aborted\n");
        return-from-tptp-to-kif();
      end if;
    end if;
    format-out("Converting %S\n..to %S\n", source-path, dest-path);
    with-open-file (in-f = (source-path, direction: #"input"))
      with-open-file (out-f
                       = (dest-path, direction: #"output",
                          if-exists: #"supersede"))
        tptp-header(out-f, dom, file);
        for (line = get-tptp-line(in-f) then get-tptp-line(in-f), while line)
          process-tptp(line, out-f, comments);
        end for;
      end with-open-file;
    end with-open-file;
    values();
  end block;
end method tptp-to-kif;

// ----------------------------------------------------------------------------
define method tptp-pathname (#key category = #f, file = #f)
  let path = #f;
  path := concatenate-as(<string>, *tptp-library*, *tptp-problems*);
  if (instance?(category, <string>))
    path
     := // LTD: Function MERGE-PATHNAMES not yet implemented.
        merge-pathnames(concatenate-as(<string>,
                                       category,
                                       *directory-separator*),
                        path);
  end if;
  if (instance?(file, <string>))
    if (~ cl-find('.', file))
      file := concatenate-as(<string>, file, ".p");
    end if;
    path
     := // LTD: Function MERGE-PATHNAMES not yet implemented.
        merge-pathnames(file, path);
  end if;
  path;
end method tptp-pathname;

define method kif-pathname (#key file = #f)
  let path = #f;
  path := concatenate-as(<string>, *tptp-library*, *tptp-kif*);
  if (file)
    if (instance?(file, <string>))
      file := as-lowercase!(file);
      if (~ // LTD: Function PATHNAME-TYPE not yet implemented.
            pathname-type(file))
        file := concatenate-as(<string>, file, ".kif");
      end if;
    end if;
    path
     := // LTD: Function MERGE-PATHNAMES not yet implemented.
        merge-pathnames(file, path);
  end if;
  path;
end method kif-pathname;

// ----------------------------------------------------------------------------
define method get-tptp-line (s)
  // Read a line from stream S, concat multiple if in the middle of clause
  block (return-from-get-tptp-line)
    let line = read-line(s, nil);
    if (~ line) return-from-get-tptp-line(#f); end if;
    line := string-trim(#(' '), line);
    if (~ (line = "") & line[0] == '[' & ~ cl-find(']', line))
      block (return)
        for (nl = read-line(s, nil) then read-line(s, nil),
             until empty?(nl) | cl-find(']', nl))
          nl := string-trim(#(' '), nl);
          line := concatenate-as(<string>, line, nl);
        finally
          if (nl)
            nl := string-trim(#(' '), nl);
            return(concatenate-as(<string>, line, nl));
          else
            return(#f);
          end if;
          #f;
        end for;
      end block;
    else
      line;
    end if;
  end block;
end method get-tptp-line;

// ----------------------------------------------------------------------------
define method tptp-header (s, cat, file)
  format(s, ";;; Automatic conversion of TPTP to KIF format\n");
  format(s, ";;; By DTP version %S\n", *dtp-version*);
  format(s, ";;; Category %S, File %S\n", cat, file);
  format(s, "\n");
end method tptp-header;

// ----------------------------------------------------------------------------
// :theorem, :axiom, :hypothesis
define variable *tptp-clause-type* = #f;

define method process-tptp (line, out-f, #key comments = #t)
  line := string-trim(#(' '), line);
  if (size(line) == 0)
    //  Blank line
    format(out-f, "\n");
  elseif (line[0] == '%')
    //  Comment
    if (comments) format(out-f, ";; %S\n", line); end if;
  elseif (input-clause-line(line))
    //  input_clause
    *tptp-clause-type* := tptp-clause-type(line);
    (method (s, #rest args)
       apply(maybe-initiate-xp-printing,
             method (xp, #rest args)
               begin
                 write-string++(";; ", xp, 0, 3);
                 fluid-bind (*print-escape* = #f)
                   write+(pop!(args), xp);
                 end fluid-bind;
                 write-string++(" (", xp, 0, 2);
                 begin
                   push-char-mode(xp, #"down");
                   fluid-bind (*print-escape* = #f)
                     write+(pop!(args), xp);
                   end fluid-bind;
                   pop-char-mode(xp);
                 end;
                 write-char++(')', xp);
                 pprint-newline+(unconditional: xp);
               end;
               if (args) copy-sequence(args); end if;
             end method,
             s, args);
     end method)(out-f, tptp-clause-name(line), *tptp-clause-type*);
  elseif (include-file-line(line))
    //  include
    format(out-f, ";; %S\n", line);
    with-open-file (in2-f = (include-file-name(line), direction: #"input"))
      for (line2 = read-line(in2-f, nil) then read-line(in2-f, nil),
           while line2)
        process-tptp(line2, out-f);
      end for;
    end with-open-file;
  else
    //  Clause
    tptp-parse-clause(line, out-f);
  end if;
end method process-tptp;

// ----------------------------------------------------------------------------
define method input-clause-line (line)
  size(line) > 12 & copy-sequence(line, 0, 12) = "input_clause";
end method input-clause-line;

define method tptp-clause-name (line)
  copy-sequence(line, find-key(line, curry(\==, '(')) + 1,
                find-key(line, curry(\==, ',')));
end method tptp-clause-name;

define method tptp-clause-type (line)
  let end = size(line) - 1;
  let type = #f;
  if (line[end] == ',')
    type
     := copy-sequence(line,
                      find-key(copy-subsequence(line, end: end - 1),
                               curry(\==, ','))
                       + 1,
                      end);
    if (type = "theorem")
      #"theorem";
    elseif (type = "hypothesis")
      #"hypothesis";
    elseif (type = "axiom")
      #"axiom";
    else
      type;
    end if;
  end if;
end method tptp-clause-type;

define method include-file-line (line)
  size(line) > 7 & copy-sequence(line, 0, 7) = "include";
end method include-file-line;

define method include-file-name (line)
  let path = #f;
  let fn = #f;
  fn
   := copy-sequence(line, find-key(line, curry(\==, '\'')) + 1,
                    find-key(line, curry(\==, '\'')));
  path := *tptp-library*;
  path
   := // LTD: Function MERGE-PATHNAMES not yet implemented.
      merge-pathnames(*tptp-axioms*, path);
  path
   := // LTD: Function MERGE-PATHNAMES not yet implemented.
      merge-pathnames(fn, path);
  path;
end method include-file-name;

// ----------------------------------------------------------------------------
define method tptp-parse-clause (line, out-f)
  //  Eliminate close of "input_clause" punctuation
  if (copy-sequence(line, size(line) - 2) = ").")
    line := copy-sequence(line, 0, size(line) - 2);
  end if;
  //  Start of clause
  if (line[0] == '[')
    format(out-f, "(or ");
    if (*tptp-clause-type* == #"theorem") format(out-f, "(goal) "); end if;
    line := copy-sequence(line, 1);
  end if;
  let lit-str = #f;
  for (until line = "")
    if (line[0] == ',')
      format(out-f, " ");
      line := copy-sequence(line, 1);
    elseif (line[0] == ']')
      format(out-f, ")\n");
      line := copy-sequence(line, 1);
    else
      begin
        let (#rest _lit-str, _line) = get-lit(line);
        lit-str := _lit-str;
        line := _line;
      end;
      process-literal(lit-str, out-f);
    end if;
  end for;
end method tptp-parse-clause;

// ----------------------------------------------------------------------------
define method get-lit (line)
  // Return: (1) literal string, (2) remaining line
  let paren-depth = 0;
  let lit-str = "";
  let lit-inc = #f;
  block (return)
    for (comma-pos = find-key(line, curry(\==, ',')) then find-key(line,
                                                                   curry(\==,
                                                                         ',')),
         open-paren-pos = find-key(line, curry(\==, '(')) then find-key(line,
                                                                        curry(\==,
                                                                              '(')),
         close-paren-pos = find-key(line, curry(\==, ')')) then find-key(line,
                                                                         curry(\==,
                                                                               ')')),
         close-bracket = find-key(line, curry(\==, ']')) then find-key(line,
                                                                       curry(\==,
                                                                             ']')),
         linesize = size(line) then size(line),
         until linesize = 0 | (paren-depth = 0 & ~ (lit-str = "")))
      if (open-paren-pos
           & (empty?(comma-pos) | paren-depth > 0 | comma-pos > open-paren-pos)
           & close-paren-pos > open-paren-pos)
        inc!(paren-depth);
        lit-inc := copy-sequence(line, 0, open-paren-pos + 1);
        lit-str := concatenate-as(<string>, lit-str, lit-inc);
        line := copy-sequence(line, open-paren-pos + 1);
      elseif (close-paren-pos & paren-depth > 0)
        dec!(paren-depth);
        lit-inc := copy-sequence(line, 0, close-paren-pos + 1);
        lit-str := concatenate-as(<string>, lit-str, lit-inc);
        line := copy-sequence(line, close-paren-pos + 1);
      elseif (comma-pos)
        if (paren-depth = 0)
          lit-inc := copy-sequence(line, 0, comma-pos);
          lit-str := concatenate-as(<string>, lit-str, lit-inc);
          line := copy-sequence(line, comma-pos);
        else
          lit-inc := copy-sequence(line, 0, comma-pos + 1);
          lit-str := concatenate-as(<string>, lit-str, lit-inc);
          line := copy-sequence(line, comma-pos + 1);
        end if;
      elseif (close-bracket)
        lit-inc := copy-sequence(line, 0, close-bracket);
        lit-str := concatenate-as(<string>, lit-str, lit-inc);
        line := copy-sequence(line, close-bracket);
      else
        format-out("Error: Can't parse literal from '%S'\n", line);
        return("", "");
      end if;
    finally
      return(lit-str, line);
      #f;
    end for;
  end block;
end method get-lit;

// ----------------------------------------------------------------------------
define method process-literal (lit-str, out-f)
  let not-flag = #f;
  if (lit-str[0] == '-') not-flag := #t; format(out-f, "(not "); end if;
  lit-str := copy-sequence(lit-str, 2);
  let pred-pos = find-key(lit-str, curry(\==, '('));
  if (pred-pos)
    format(out-f, "(%S ", copy-sequence(lit-str, 0, pred-pos));
    lit-str := copy-sequence(lit-str, pred-pos + 1);
  else
    format(out-f, "(%S", lit-str);
    lit-str := "";
  end if;
  let term-end = find-key(lit-str, curry(\==, ')'));
  if (term-end)
    process-terms(copy-sequence(lit-str, 0, term-end), out-f);
    lit-str := copy-sequence(lit-str, term-end + 1);
  end if;
  format(out-f, ")");
  if (not-flag) format(out-f, ")"); end if;
end method process-literal;

// ----------------------------------------------------------------------------
define method process-terms (line, out-f)
  for (end = size(line) then size(line), until end = 0,
       comma-pos = find-key(line, curry(\==, ',')) then find-key(line,
                                                                 curry(\==,
                                                                       ',')),
       open-pos = find-key(line, curry(\==, '(')) then find-key(line,
                                                                curry(\==,
                                                                      '(')),
       close-pos = find-key(line, curry(\==, ')')) then find-key(line,
                                                                 curry(\==,
                                                                       ')')),
       next-pos = min(comma-pos | end, open-pos | end,
                      close-pos | end) then min(comma-pos | end,
                                                open-pos | end,
                                                close-pos | end),
       next-punct = if (~ (next-pos = end))
                      line[next-pos];
                    end if then if (~ (next-pos = end))
                                  line[next-pos];
                                end if,
       symb = copy-sequence(line, 0, next-pos) then copy-sequence(line,
                                                                  0,
                                                                  next-pos))
    if (next-punct)
      select (next-punct)
        ','
           => if (~ (symb = "")) process-symb(symb, out-f); end if;
               format(out-f, " ");
               line := copy-sequence(line, next-pos + 1);
        '('
           => format(out-f, "(");
               process-symb(symb, out-f);
               format(out-f, " ");
               line := copy-sequence(line, next-pos + 1);
        ')'
           => if (~ (symb = "")) process-symb(symb, out-f); end if;
               format(out-f, ")");
               line := copy-sequence(line, next-pos + 1);
        otherwise
           => #f;
      end select;
    else
      process-symb(symb, out-f);
      line := "";
    end if;
  end for;
end method process-terms;

define method process-symb (term, out-f)
  if (upper-case?(term[0]))
    (method (s, #rest args)
       apply(maybe-initiate-xp-printing,
             method (xp, #rest args)
               begin
                 write-char++('?', xp);
                 begin
                   push-char-mode(xp, #"down");
                   fluid-bind (*print-escape* = #f)
                     write+(pop!(args), xp);
                   end fluid-bind;
                   pop-char-mode(xp);
                 end;
               end;
               if (args) copy-sequence(args); end if;
             end method,
             s, args);
     end method)(out-f, term);
  else
    (method (s, #rest args)
       apply(maybe-initiate-xp-printing,
             method (xp, #rest args)
               begin
                 push-char-mode(xp, #"down");
                 fluid-bind (*print-escape* = #f)
                   write+(pop!(args), xp);
                 end fluid-bind;
                 pop-char-mode(xp);
               end;
               if (args) copy-sequence(args); end if;
             end method,
             s, args);
     end method)(out-f, term);
  end if;
end method process-symb;

// ----------------------------------------------------------------------------
"eof";

