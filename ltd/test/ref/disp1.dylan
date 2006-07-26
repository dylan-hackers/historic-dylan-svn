//  -*- mode: common-lisp; package: mma; -*-
//  display package for pseudo-mathematica
//  written by Derek Lai, rewritten in parts by Richard Fateman
//  (c) 1990, 1991, Richard J. Fateman, Derek Lai.
//  known bugs:  Derivative[1][F][x]    or  (((Derivative 1)F)x)
//  does not display. Gives an error.  should be perhaps F'[x]. 
//  what a hack... 4 Oct 91.
// (provide 'disp1)
#"eval-when"(#"compile"(), #"load"("mma"));

"(in-package mma)";

// (export '(COL disp dispstruct))
#f;

//  number of columns of screen
//  % COL should be a parameter obtained from the CL system
define variable col = 65;

define class <dispstruct> (<object>)
  slot dispstruct-str = #f, init-keyword: #"dispstruct-str";
  slot dispstruct-x :: <integer> = 0, init-keyword: #"dispstruct-x";
  slot dispstruct-y :: <integer> = 0, init-keyword: #"dispstruct-y";
end class <dispstruct>;

//  testing display routine
define method td ()
  disp(buildformat(p()));
  write-element(*standard-output*, '\n');
  write-element(*standard-output*, '\n');
  td();
end method td;

// default is to send output to display
define variable stream = #t;

define method disp (form, #key stream = #t)
  let list = #f;
  format(stream, "\n");
  if (not(instance?(form, <list>)))
    if (atomwidth(form) <= col)
      format(stream, "%S", form);
    else
      let hform = makehform(atomwidth(form), 1, 0, 0, #f, #f, #f, form);
      testanddisplay(disp-helper(hform, 1, 1, #f), hform);
    end if;
  else
    list := disp-helper(form, 1, formatstruct-height(form), list);
    testanddisplay(list, form);
  end if;
end method disp;

define method disp-list (form)
  let list = #f;
  format(stream, "\n");
  if (not(instance?(form, <list>)))
    form;
  else
    list := disp-helper(form, 1, formatstruct-height(form), list);
    list := sort!(list, test: listyorderp);
    list := sort!(list, test: listxorderp);
  end if;
end method disp-list;

define method disp-helper (form, x, y, l)
  let f = #f;
  let vof = 0;
  if (vformp(form) | hformp(form))
    f := formatstruct-ls(form);
    vof := formatstruct-voffset(form);
  else
    f := form;
  end if;
  if (vformp(form))
    if (divideformp(form))
      l := disp-helper(head(f), x, y - (1 + vof), l);
      l
       := appendlist(x, y - vof,
                     make(<string>, size: second(cdadr(f)), fill: '-'), l);
      disp-helper(third(f), x, y, l);
    else
      //  should be due to exponents
      y := y - 1;
      disp-helper(f, x, y - vof, l);
    end if;
  elseif (repstrformp(form))
    appendlist(x, y,
               make(<string>, size: third(form),
                    fill: as(<character>, second(form))),
               l);
  else
    block (return)
      for (args = if (atom(f)) list(f); f; end if then cdr(args),
           until empty?(args))
        let ham = head(args);
        if (not(instance?(ham, <list>)))
          l := appendlist(x, y - vof, ham, l);
          x := x + atomwidth(ham);
        elseif (repstrformp(ham))
          l := disp-helper(ham, x, y - vof, l);
          x := x + third(ham);
        else
          l := disp-helper(ham, x, y - vof + formatstruct-voffset(ham), l);
          x := x + formatstruct-width(ham);
        end if;
      finally
        return(l);
      end for;
    end block;
  end if;
end method disp-helper;

define method appendlist (x, y, ham, l)
  //  do I believe this is really safe? I think it is
  //  a better way would have been to cons up a list and
  //  (n)reverse it at the end (if necessary)... 1/23/91 RJF
  concatenate!(l, list(make-dispstruct(str: ham, x: x, y: y)));
end method appendlist;

define method listyorderp (a, b)
  a.dispstruct-y < b.dispstruct-y;
end method listyorderp;

define method listxorderp (a, b)
  a.dispstruct-x < b.dispstruct-x;
end method listxorderp;

//  Breaklinex and display.  Right now COL is the limit per line
//  2 tests are performed:  1) if the dls is shorter than COL chars,
//  display it right away and exit.  2) if not, breaklines, "compress"
//  the Vertical forms if necessary.
define method testanddisplay (dls, form)
  dls := sort!(dls, test: listyorderp);
  if (formatwidth(form) < col)
    finaldisplay(dls, formatheight(form) - formatstruct-voffset(form));
  else
    begin
      let brkptset = #f;
      block (return)
        while (#t)
          brkptset := getbrkptset(copy-sequence(dls), form);
          if (brkptset)
            display(dls, brkptset,
                    formatheight(form) - formatstruct-voffset(form));
            return(#"done");
          end if;
          dls := disp-list(compress(form));
        end while;
      end block;
    end;
  end if;
end method testanddisplay;

//  brkptset is in the form of (74 145 224 ...)
//  dls will get shorter and shorter within this procedure
//  The function Compressible is not written yet.
define method getbrkptset (dls, form)
  let cursor = 1;
  let cplus = -1;
  let cminus = -1;
  let ctimes = -1;
  let ccomma = -1;
  let cdot = -1;
  let brkptset = #f;
  dls := sort!(dls, test: listyorderp);
  dls := sort!(dls, test: listxorderp);
  block (return)
    while (#t)
      if (empty?(dls)) return(brkptset); end if;
      cplus
       := getclosest(" + ", dls,
                     formatheight(form) - formatstruct-voffset(form), cursor);
      cminus
       := getclosest(" - ", dls,
                     formatheight(form) - formatstruct-voffset(form), cursor);
      ccomma
       := getclosest(", ", dls,
                     formatheight(form) - formatstruct-voffset(form), cursor);
      cdot
       := getclosest(" . ", dls,
                     formatheight(form) - formatstruct-voffset(form), cursor);
      if (cplus - cursor < col & cminus - cursor < col & ccomma - cursor < col
           & cdot - cursor < col)
        cplus := max(cplus, cminus, ccomma, cdot);
      else
        cplus := min(cplus, cminus, ccomma, cdot);
      end if;
      ctimes
       := getclosest(" ", dls,
                     formatheight(form) - formatstruct-voffset(form), cursor);
      if (cplus - cursor > col)
        if (ctimes - cursor > col)
          if (compressible(form, cursor))
            return(#f);
          else
            brkptset := concatenate(brkptset, list(cursor + col));
            //  questionable....
            cursor := cursor + col;
          end if;
        else
          brkptset := concatenate(brkptset, list(ctimes + 1));
          //  brute force break
          cursor := ctimes + 1;
        end if;
      else
        if (ccomma = cplus)
          //  then it is cComma in fact
          brkptset := concatenate(brkptset, list(ccomma + 2));
          cursor := ccomma + 2;
        else
          brkptset := concatenate(brkptset, list(cplus + 3));
          cursor := cplus + 3;
        end if;
      end if;
      dls := rmvhead(dls, cursor);
    end while;
  end block;
end method getbrkptset;

define method rmvhead (dls, cursor)
  block (return)
    while (#t)
      if (empty?(dls)) return(#f); end if;
      if (dispstruct-x(head(dls)) > cursor) return(dls); end if;
      dls := tail(dls);
    end while;
  end block;
end method rmvhead;

//  Endpt takes a dispstruct and returns the endpt of it
//  i.e.  the place where the last char of the string lies.
define method endpt (dst)
  dst.dispstruct-x + atomwidth(dst.dispstruct-str) - 1;
end method endpt;

//  axis = main axis y-coord
//  Closest sumbolpt (which this function finds) is defined as the place of the first character of
//  " + " , " - " or " "
//  Closest breakpt is defined as the place immediately following these symbols
define method getclosest (symb, dls, yaxis, cursor)
  let x :: <integer> = #f;
  block (return)
    while (#t)
      if (empty?(dls)) return(x); end if;
      if (endpt(head(dls)) - cursor > col)
        if (x) return(x); else return(endpt(head(dls))); end if;
      end if;
      if (dispstruct-y(head(dls)) = yaxis & symb = dispstruct-str(head(dls))
           & endpt(head(dls)) - cursor <= col)
        x := dispstruct-x(head(dls));
      end if;
      if (empty?(tail(dls))) x := endpt(head(dls)) + 1; end if;
      dls := tail(dls);
    end while;
  end block;
end method getclosest;

//  Compressible is not written yet
define method compressible (form, cursor) #f; end method compressible;

//  Compress is not written yet
define method compress (form) form; end method compress;

//  input is an unsorted list and a brkptset
//  not written yet
define method display (dls, brkptset, yaxis)
  let cursor = 1;
  let tempdls = #f;
  let ham1 = #f;
  let ham2 = #f;
  let bksl = #f;
  dls := sort!(dls, test: listxorderp);
  block (return)
    while (#t)
      if (empty?(dls))
        tempdls := sort!(tempdls, test: listyorderp);
        finaldisplay(tempdls, yaxis);
        return(#"done");
      end if;
      if (endpt(head(dls)) >= head(brkptset))
        if (dispstruct-x(head(dls)) < head(brkptset))
          //  brute force breakline
          ham1
           := make-dispstruct(str: copy-sequence(format(#f,
                                                        "%S",
                                                        dispstruct-str(head(dls))),
                                                 0,
                                                 head(brkptset)
                                                  - dispstruct-x(head(dls))),
                              x: dispstruct-x(head(dls)) - (cursor - 1),
                              y: dispstruct-y(head(dls)));
          bksl
           := make-dispstruct(str: '\\', x: ham1.dispstruct-x + 1,
                              y: dispstruct-y(head(dls)));
          tempdls := concatenate(tempdls, list(ham1), list(bksl));
          ham2
           := make-dispstruct(str: copy-sequence(format(#f,
                                                        "%S",
                                                        dispstruct-str(head(dls))),
                                                 head(brkptset)
                                                  - dispstruct-x(head(dls)),
                                                 atomwidth(dispstruct-str(head(dls)))),
                              x: head(brkptset), y: dispstruct-y(head(dls)));
          dls := sort!(concatenate(list(ham2), tail(dls)), test: listxorderp);
        else
          finaldisplay(tempdls, yaxis);
          write-element(*standard-output*, '\n');
          write-element(*standard-output*, '\n');
          tempdls := #f;
          cursor := head(brkptset);
          if (tail(brkptset))
            brkptset := tail(brkptset);
          else
            brkptset := list(col + head(brkptset));
          end if;
        end if;
        //  set brkptset to some "dont-care" value
        else
        tempdls
         := concatenate(tempdls,
                        list(make-dispstruct(str: dispstruct-str(head(dls)),
                                             x: dispstruct-x(head(dls))
                                                 - (cursor - 1),
                                             y: dispstruct-y(head(dls)))));
        dls := tail(dls);
      end if;
    end while;
  end block;
end method display;

//  Display the final output list.       
//  yaxis isn't used..
define method finaldisplay (dls, yaxis)
  let old = #f;
  let x :: <integer> = 1;
  let y :: <integer> = 1;
  dls := sort!(dls, test: listyorderp);
  block (return)
    while (#t)
      dispstruct-x(head(dls)) := dispstruct-x(head(dls));
      block (return)
        while (#t)
          if (y >= dispstruct-y(head(dls))) return(#"done"); end if;
          format(stream, "\n");
          y := y + 1;
        end while;
      end block;
      //  tab to the right column and print..
      (method (s, #rest args)
         apply(maybe-initiate-xp-printing,
               method (xp, #rest args)
                 begin
                   pprint-tab+(line: begin
                                       let _that = #f;
                                       if (_that := pop!(args))
                                       _that;
                                       else
                                       1;
                                       end if;
                                     end,
                               0, xp);
                   fluid-bind (*print-escape* = #f)
                     write+(pop!(args), xp);
                   end fluid-bind;
                 end;
                 if (args) copy-sequence(args); end if;
               end method,
               s, args);
       end method)(stream, (x := dispstruct-x(head(dls))) - 1,
                   dispstruct-str(head(dls)));
      old := head(dls);
      dls := tail(dls);
      if (empty?(dls)) return(#"done"); end if;
      if (y = dispstruct-y(head(dls)))
        x := x + atomwidth(old.dispstruct-str);
      else
        x := 1;
      end if;
    end while;
  end block;
end method finaldisplay;

