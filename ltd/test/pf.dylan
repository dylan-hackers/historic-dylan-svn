//  -*- Mode:Common-Lisp;Package:mma; Base:10 -*-
// 
//  (c) Copyright 1990, 1992, 1994 Richard J. Fateman, Derek T. Lai
//      FILE -- pf.lisp
// 
//  pf.lisp defines BuildFormat to convert the parser output
//  or the output of the manipulative routines into 
//  lists which resemble the PrintForm output of Mathematica.  
//  Differences between the two reflect the fact that some
//  apparently useless extra levels of list are used by
//  mathematica, and some extra data of uncertain provenance
//  is included in their first arguments to HorizontalForm
//  and VerticalForm  (the 6th entry).
//  This is then converted by the disp program to 2-D display.
// 
//  For example, if we type (x+y)^2 as input, the parser will return
//  
//            (Power (Plus x y ) 2)
//  (BuildFormat '(Power (Plus x y) 2)) returns
//  
//   (HorizontalForm 8 2 0 0 Power 190 right
//    ((HorizontalForm 7 1 0 0 Sequence 1000 none
//      ("(" (HorizontalForm 5 1 0 0 Plus 140 none (x " + " y)) ")"))
//     (VerticalForm 1 1 0 -1 Superscript 260 none 2)))
//  The version 2.0 mathematica provides for the same input.. 
// HorizontalForm[{8, 2, 0, 0, 0, {7, 1}}, {Power, 190, Right}, 
//   HorizontalForm[{7, 1, 0, 0, 0, {7, 7}}, {Sequence, 1000, None}, (, 
//     HorizontalForm[{5, 1, 0, 0, 0, {10, 2}}, {Plus, 140, None}, x,  + , y], 
//    )], , VerticalForm[{1, 1, 0, -1, 0}, {Sequence, 1000, None}, 2]]
//  patch history 
// 6/3/92 RJF fixed display of operators .. f[x][y]
// (provide 'pf)
// LTD: Function LOAD not yet implemented.
load("mma");

"(in-package mma)";

//   exported functions:
define module mma
  export makehform, makevform, formatwidth, formatheight, atomwidth, buildformat;
end module mma;

//   "formatstruct" is the structure representing the HorizontalForm
//   and the VerticalForm "PrintForm" lists.
// 
//   The meaning of its attributes are as follows:
//     width    --  width of box
//     height   --  height of box
//     hoffset  --  horizontal offset
//     voffset  --  vertical offset, distance from the base of box
//     op       --  name of operator represented by this box
//     prec     --  precedence of the operator
//     asso     --  associativity of the operator
//     ls       --  can be another HorizontalForm, VerticalForm,
//                  or some atoms or strings.
// 
//   In the context below, "PrintForm" lists, "format", "form" are basically
//   referring to the same thing.
define class <formatstruct> (<object>)
  slot formatstruct-form = #f, init-keyword: #"formatstruct-form";
  slot formatstruct-width :: <integer> = 0,
       init-keyword: #"formatstruct-width";
  slot formatstruct-height :: <integer> = 1,
       init-keyword: #"formatstruct-height";
  slot formatstruct-hoffset :: <integer> = 0,
       init-keyword: #"formatstruct-hoffset";
  slot formatstruct-voffset :: <integer> = 0,
       init-keyword: #"formatstruct-voffset";
  slot formatstruct-op = #f, init-keyword: #"formatstruct-op";
  slot formatstruct-prec :: <integer> = 0, init-keyword: #"formatstruct-prec";
  slot formatstruct-asso = #"none", init-keyword: #"formatstruct-asso";
  slot formatstruct-ls = #f, init-keyword: #"formatstruct-ls";
end class <formatstruct>;

//   MakeVForm & MakeHForm:   
//           functions that create horizontal and vertical forms respectively.
define method makevform (w, h, hof, vof, op, prc, as, l)
  make-formatstruct(form: #"verticalform", width: w, height: h, hoffset: hof,
                    voffset: vof, op: op, prec: prc, asso: as, ls: l);
end method makevform;

define method makehform (w, h, hof, vof, op, prc, as, l)
  make-formatstruct(form: #"horizontalform", width: w, height: h,
                    hoffset: hof, voffset: vof, op: op, prec: prc, asso: as,
                    ls: l);
end method makehform;

//   HFormp & VFormp:
//           boolean functions that test whether a given form is Horizontal 
//           or Vertical.
define method hformp (form)
  if (not(instance?(form, <list>)))
    #f;
  else
    head(form) = #"horizontalform";
  end if;
end method hformp;

define method vformp (form)
  if (not(instance?(form, <list>)))
    #f;
  else
    head(form) = #"verticalform";
  end if;
end method vformp;

//   DivideFormp, RepStrFormp, SeqFormp:
//           These functions tests some commonly encountered forms.
//           They check whether a given form is a Divide form, a 
//           RepeatedString Form or a Sequence Form respectively.
define method divideformp (f)
  if (not(instance?(f, <list>)))
    #f;
  else
    f.formatstruct-op = #"divide";
  end if;
end method divideformp;

define method repstrformp (f)
  if (not(instance?(f, <list>))) #f; else head(f) = #"repeatedstring"; end if;
end method repstrformp;

define method seqformp (f)
  if (not(instance?(f, <list>)))
    #f;
  else
    f.formatstruct-op = #"sequence";
  end if;
end method seqformp;

//   tp:   procedure for repeatedly testing the parser
define method tp ()
  format-out("%=", p());
  write-element(*standard-output*, '\n');
  tp();
end method tp;

//   printform:
//         procedure that repeatedly (by giving prompts) takes 
//         Mathematica-acceptable (ordinary) mathematical expressions, 
//         pass them to the parser whose output is further passwd to 
//         BuildFormat to form the
//         "PrintForm" lists.
define method printform ()
  format-out("%=", buildformat(p()));
  //  repeat, ad naseum
  write-element(*standard-output*, '\n');
  write-element(*standard-output*, '\n');
  printform();
end method printform;

//   formatwidth & formatheight:   
//           gets the width and height of a format respectively.
define method formatwidth (x)
  if (not(instance?(x, <list>)))
    atomwidth(x);
  else
    x.formatstruct-width;
  end if;
end method formatwidth;

define method formatheight (x)
  if (not(instance?(x, <list>))) 1; else x.formatstruct-height; end if;
end method formatheight;

//   Set the precedences of the operators.  This list is arranged in order of
//   increasing precedence.
symbol-get-property(#"set", #"stringprec") := 120;

symbol-get-property(#"rule", #"stringprec") := 130;

symbol-get-property(#"plus", #"stringprec") := 140;

symbol-get-property(#"times", #"stringprec") := 150;

symbol-get-property(#"divide", #"stringprec") := 160;

symbol-get-property(#"minus", #"stringprec") := 170;

symbol-get-property(#"power", #"stringprec") := 190;

symbol-get-property(#"dot", #"stringprec") := 210;

symbol-get-property(#"sequence", #"stringprec") := 1000;

//   Set the correct formatter (function name to be dispatched) corresponding 
//   to the operators
symbol-get-property(#"rule", #"formatter") := #"ruleformatter";

symbol-get-property(#"plus", #"formatter") := #"plusformatter";

symbol-get-property(#"times", #"formatter") := #"timesformatter";

symbol-get-property(#"power", #"formatter") := #"powerformatter";

symbol-get-property(#"list", #"formatter") := #"listformatter";

symbol-get-property(#"dot", #"formatter") := #"dotformatter";

symbol-get-property(#"real", #"formatter") := #"realformatter";

symbol-get-property(#"set", #"formatter") := #"setformatter";

//  sample definition of new formatter...  9/30/94
symbol-get-property(#"increment", #"formatter") := #"postformatter";

symbol-get-property(#"increment", #"stringprec") := 150;

// guess  9/30/94 RJF
symbol-get-property(#"increment", #"stringform") := "++ ";

//  probably could make use of more vertical/horizontal concatenators
//  have to keep track of precedences, though.
symbol-get-property(#"factorial", #"formatter") := #"postformatter";

symbol-get-property(#"factorial", #"stringprec") := 150;

// guess  9/30/94 RJF
symbol-get-property(#"factorial", #"stringform") := "!";

symbol-get-property(#"factorial2", #"formatter") := #"postformatter";

symbol-get-property(#"factorial2", #"stringprec") := 150;

// guess  9/30/94 RJF
symbol-get-property(#"factorial2", #"stringform") := "!!";

symbol-get-property(#"preincrement", #"formatter") := #"preformatter";

symbol-get-property(#"preincrement", #"stringprec") := 150;

// guess  9/30/94 RJF
symbol-get-property(#"preincrement", #"stringform") := "++";

//  rjf 9/30/94
define method postformatter (x)
  let op = head(x);
  let st = #f;
  let args = tail(x);
  let width = 0;
  let maxtoph = 1;
  let maxbotmh = 0;
  let hoffset = 0;
  let voffset = 0;
  let ls = #f;
  let heightpacket = #f;
  let ret_format = #f;
  ret_format := buildformat(head(args));
  st := symbol-get-property(op, #"stringform");
  if (not(instance?(ret_format, <list>)))
    inc!(width, atomwidth(ret_format));
    ls := list(ret_format);
  else
    heightpacket := topboths(ret_format, maxtoph, maxbotmh);
    maxtoph := head(heightpacket);
    maxbotmh := tail(heightpacket);
    inc!(width, ret_format.formatstruct-width);
    voffset := max(voffset, ret_format.formatstruct-voffset);
    inc!(hoffset, ret_format.formatstruct-hoffset);
  end if;
  makehform(width + atomwidth(st), maxtoph + maxbotmh, hoffset, voffset, op,
            prec(op), #"left", list(ret_format, st));
end method postformatter;

define method preformatter (x)
  let op = head(x);
  let args = tail(x);
  let st = #f;
  let width = 0;
  let maxtoph = 1;
  let maxbotmh = 0;
  let hoffset = 0;
  let voffset = 0;
  let ls = #f;
  let heightpacket = #f;
  let ret_format = #f;
  ret_format := buildformat(head(args));
  st := symbol-get-property(op, #"stringform");
  if (not(instance?(ret_format, <list>)))
    inc!(width, atomwidth(ret_format));
    ls := list(ret_format);
  else
    heightpacket := topboths(ret_format, maxtoph, maxbotmh);
    maxtoph := head(heightpacket);
    maxbotmh := tail(heightpacket);
    inc!(width, ret_format.formatstruct-width);
    voffset := max(voffset, ret_format.formatstruct-voffset);
    inc!(hoffset, ret_format.formatstruct-hoffset);
  end if;
  makehform(width + atomwidth(st), maxtoph + maxbotmh, hoffset, voffset, op,
            prec(op), #"left",
            list(symbol-get-property(op, #"stringform"), ret_format));
end method preformatter;

//   Get precedence:  
//           get the precedence of a given operator.  Default value is 260.
define method prec (x)
  if (instance?(x, <pair>))
    260;
  else
    let p = symbol-get-property(x, #"stringprec");
    if (p) p; else 260; end if;
  end if;
end method prec;

//   lessprec:
//           checks if the operator of a given form "f" has a lower
//           precedence than a given operator "op"
define method lessprec (f, op)
  ~ not(instance?(f, <list>)) & prec(f.formatstruct-op) < prec(op);
end method lessprec;

//   atomwidth:  
//           returns the width of an atom, a number or a string
//           (but not a formatstruct).
// 
//  much faster this way (RJF)
nil(#f, nil(),
    nil(#f, nil(nil(nil(#f, #f))), nil(nil(nil(#f, nil(#f)))),
        nil(nil(nil(#f), nil(nil(nil(#f))), nil(#f))),
        nil(nil(nil(#f, nil(#f, "~a", #f))))));

//  this is assuming IEEE arithmetic.  Also a decent logarithm.
//  Funny about that..
define method integer-length-base-10 (x)
  select (x by instance?)
    integer(0, 9)
       => 1;
    //  most common cases
    integer(10, 99)
       => 2;
    integer(100, 999)
       => 3;
    integer(0, 1000000)
       => floor(log(x, 10.0)) + 1;
    integer(0, 1000000000000000)
       => floor(log(x, 10.0d0)) + 1;
    integer
       => //  rest of the cases
           begin
             let fg
                 = floor/(ash(// LTD: Function INTEGER-LENGTH not yet implemented.
                              integer-length(x),
                              19),
                          // mult by 2^19
                          1741647);
             let try = 10 ^ fg;
             if (try <= x) fg + 1; else fg; end if;
           end;
  end select;
end method integer-length-base-10;

//  we could mess with this
// (defvar log10b2 (rationalize(log 10.0d0 2.0d0)))
// (defvar bigint (truncate most-positive-long-float))
// 
// 
//   TopBotHs:
//           Compare max numerator and denominator heights and
//           returns a height-packet of cons.
nil(#f, nil(#f, #f),
    nil((nil(nil(nil(#f), nil(#f))))(nil(nil(#f))),
        nil(nil(#f, #f), nil(#f, #f)), nil(nil(#f, #f), nil(#f, #f)),
        nil(#f, #f)));

//   BuildFormat:  
//           This function takes the output lisp expression from the parser,
//           look at its outermost operator (if any) and pass the whole list
//           to the appropriate formatter function.
define method buildformat (x)
  let type = head(x);
  let formatfun = #f;
  //  dispatch on the Head of the form if it is an atom
  if (not(instance?(type, <list>))
       & (formatfun := symbol-get-property(type, #"formatter")))
    formatfun(x);
    //  if it is an atom but no special formatter program defined
    //  then use the default atom-head formatter.
    elseif (not(instance?(type, <list>)))
    deflt-buildformat(x);
    //  some types are strange like (Derivative 1) or (Inverse F)
    //  use another default formatter
    else
    deflt-buildformat-op(x);
  end if;
end method buildformat;

//  set up number formatters
define method numberformatter (x)
  if (negative?(x)) minusformatter(abs(x)); else x; end if;
end method numberformatter;

begin
  do(method (x)
       symbol-get-property(x, #"formatter") := numberformatter;
     end method,
     #(#"single-float", #"double-float", #"fixnum", #"bignum", #"ratio"));
  #(#"single-float", #"double-float", #"fixnum", #"bignum", #"ratio");
end;

// not complex
symbol-get-property(#"symbol", #"formatter")
 := method (x) if (x) x; else #"false"; end if; end method;

//  nil is False
symbol-get-property(#"rat", #"formatter")
 := method (x) buildformat(outof-rat(x)); end method;

symbol-get-property(#"complex", #"formatter")
 := #[#"lambda", #(#"x"), #(#"complexnumformatter", #"x")];

symbol-get-property(#"bigfloat", #"formatter")
 := method (x) buildformat(format(#f, "%=", x)); end method;

//   deflt-buildformat:  default version...
//           If we do not know anything else about a given function (that is,
//           it is not one of plus, times, dot etc.), deflt-buildformat 
//           handles all general functions f[x].
// 
//           For example, it converts (f x y z) to
// 
//           (HORIZONTALFORM 10 1 0 0 F 260 LEFT (F "[" X ", " Y ", " Z "]"))
define method deflt-buildformat (x)
  if (not(instance?(x, <list>)))
    x;
  else
    begin
      let op = head(x);
      let width = 0;
      let maxtoph = 1;
      let maxbotmh = 0;
      let hoffset = 0;
      let voffset = 0;
      let ls = #f;
      let heightpacket = #f;
      let ret_format = #f;
      for (args = cdr(x) then cdr(args), until empty?(args))
        ret_format := buildformat(head(args));
        if (not(instance?(ret_format, <list>)))
          inc!(width, atomwidth(ret_format) + 2);
          ls := bq-list*(ret_format, ", ", ls);
        else
          heightpacket := topboths(ret_format, maxtoph, maxbotmh);
          maxtoph := head(heightpacket);
          maxbotmh := tail(heightpacket);
          inc!(width, ret_format.formatstruct-width + 2);
          voffset := max(voffset, ret_format.formatstruct-voffset);
          inc!(hoffset, ret_format.formatstruct-hoffset);
          ls := bq-list*(ret_format, ", ", ls);
        end if;
      finally
        makehform(width + atomwidth(op), maxtoph + maxbotmh, hoffset, voffset,
                  op, prec(op), #"left",
                  bq-list*(op, "[",
                           bq-append(tail(reverse!(ls)), bq-list("]"))));
      end for;
    end;
  end if;
end method deflt-buildformat;

define method deflt-buildformat-op (x)
  let op = buildformat(head(x));
  let width = 0;
  let maxtoph = 1;
  let maxbotmh = 0;
  let hoffset = 0;
  let voffset = 0;
  let ls = #f;
  let heightpacket = #f;
  let ret_format = #f;
  for (args = cdr(x) then cdr(args),
       until //  also format "op"
       empty?(args))
    ret_format := buildformat(head(args));
    if (not(instance?(ret_format, <list>)))
      inc!(width, atomwidth(ret_format) + 2);
      ls := bq-list*(ret_format, ", ", ls);
    else
      heightpacket := topboths(ret_format, maxtoph, maxbotmh);
      maxtoph := head(heightpacket);
      maxbotmh := tail(heightpacket);
      inc!(width, ret_format.formatstruct-width + 2);
      voffset := max(voffset, ret_format.formatstruct-voffset);
      inc!(hoffset, ret_format.formatstruct-hoffset);
      ls := bq-list*(ret_format, ", ", ls);
    end if;
  finally
    makehform(width + op.formatstruct-width, maxtoph + maxbotmh, hoffset,
              voffset, head(x), 260, #"left", //  will 260 do??
              bq-list*(op, "[", bq-append(tail(reverse!(ls)), bq-list("]"))));
  end for;
end method deflt-buildformat-op;

//   listformatter:
//           This function handle things like {a,b,c}.  It takes
//           (list a b c) and returns
// 
//           (HORIZONTALFORM 9 1 0 0 SEQUENCE 1000 NONE ("{" A ", " B ", " C "}"))
define method listformatter (x)
  let width = 0;
  let maxtoph = 1;
  let maxbotmh = 0;
  let hoffset = 0;
  let voffset = 0;
  let ls = #f;
  let heightpacket = #f;
  let ret_format = #f;
  for (args = cdr(x) then cdr(args), until empty?(args))
    ret_format := buildformat(head(args));
    if (not(instance?(ret_format, <list>)))
      inc!(width, atomwidth(ret_format) + 2);
      ls := bq-list*(ret_format, ", ", ls);
    else
      heightpacket := topboths(ret_format, maxtoph, maxbotmh);
      maxtoph := head(heightpacket);
      maxbotmh := tail(heightpacket);
      inc!(width, ret_format.formatstruct-width + 2);
      voffset := max(voffset, ret_format.formatstruct-voffset);
      inc!(hoffset, ret_format.formatstruct-hoffset);
      ls := bq-list*(ret_format, ", ", ls);
    end if;
  finally
    makehform(width, maxtoph + maxbotmh, hoffset, voffset, #"sequence",
              prec(#"sequence"), #"none",
              bq-cons("{", bq-append(tail(reverse!(ls)), bq-list("}"))));
  end for;
end method listformatter;

//   dotformatter:
//           This functions handles a.b.c .  It takes (Dot a b c)
//           and returns
// 
//           (HORIZONTALFORM 9 1 0 0 DOT 210 NONE (A " . " B " . " C))
define method dotformatter (x)
  let width = 0;
  let maxtoph = 1;
  let maxbotmh = 0;
  let hoffset = 0;
  let voffset = 0;
  let ls = #f;
  let heightpacket = #f;
  let ret_format = #f;
  for (args = cdr(x) then cdr(args), until empty?(args))
    ret_format := buildformat(head(args));
    if (not(instance?(ret_format, <list>)))
      inc!(width, atomwidth(ret_format) + 3);
      ls := bq-list*(ret_format, " . ", ls);
    else
      if (lessprec(ret_format, #"dot"))
        ret_format := parenformatter(ret_format);
      end if;
      heightpacket := topboths(ret_format, maxtoph, maxbotmh);
      maxtoph := head(heightpacket);
      maxbotmh := tail(heightpacket);
      inc!(width, ret_format.formatstruct-width + 3);
      voffset := max(voffset, ret_format.formatstruct-voffset);
      inc!(hoffset, ret_format.formatstruct-hoffset);
      ls := bq-list*(ret_format, " . ", ls);
    end if;
  finally
    makehform(width - 3, maxtoph + maxbotmh, hoffset, voffset, #"dot",
              prec(#"dot"), #"none", tail(reverse!(ls)));
  end for;
end method dotformatter;

define method ruleformatter (x)
  genformatter(x, #"rule");
end method ruleformatter;

symbol-get-property(#"rule", #"stringsym") := " -> ";

symbol-get-property(#"rule", #"stringsymlen") := 4;

define method andformatter (x) genformatter(x, #"and"); end method andformatter;

symbol-get-property(#"and", #"stringsym") := " && ";

symbol-get-property(#"and", #"stringsymlen") := 4;

symbol-get-property(#"and", #"stringprec") := 215;

symbol-get-property(#"and", #"formatter") := #"andformatter";

define method orformatter (x) genformatter(x, #"or"); end method orformatter;

symbol-get-property(#"or", #"stringsym") := " || ";

symbol-get-property(#"or", #"stringsymlen") := 4;

symbol-get-property(#"or", #"stringprec") := 214;

symbol-get-property(#"or", #"formatter") := #"orformatter";

// added 11/18/94 RJF
define method alternativesformatter (x)
  genformatter(x, #"alternatives");
end method alternativesformatter;

symbol-get-property(#"alternatives", #"stringsym") := " | ";

symbol-get-property(#"alternatives", #"stringsymlen") := 3;

symbol-get-property(#"alternatives", #"stringprec") := 140;

//  guess
symbol-get-property(#"alternatives", #"formatter") := #"alternativesformatter";

//  this will format most infix forms (binary, n-ary) from declarative info
define method genformatter (x, head)
  let width = 0;
  let maxtoph = 1;
  let maxbotmh = 0;
  let hoffset = 0;
  let voffset = 0;
  let ls = #f;
  let heightpacket = #f;
  let sym = symbol-get-property(head, #"stringsym");
  let len = symbol-get-property(head, #"stringsymlen");
  let ret_format = #f;
  for (args = cdr(x) then cdr(args), until empty?(args))
    ret_format := buildformat(head(args));
    if (not(instance?(ret_format, <list>)))
      inc!(width, atomwidth(ret_format) + len);
      ls := bq-list*(ret_format, sym, ls);
    else
      if (lessprec(ret_format, head))
        ret_format := parenformatter(ret_format);
      end if;
      heightpacket := topboths(ret_format, maxtoph, maxbotmh);
      maxtoph := head(heightpacket);
      maxbotmh := tail(heightpacket);
      inc!(width, ret_format.formatstruct-width + len);
      voffset := max(voffset, ret_format.formatstruct-voffset);
      inc!(hoffset, ret_format.formatstruct-hoffset);
      ls := bq-list*(ret_format, sym, ls);
    end if;
  finally
    makehform(width - len, maxtoph + maxbotmh, hoffset, voffset, head,
              prec(head), #"none", tail(reverse!(ls)));
  end for;
end method genformatter;

//   plusformatter:
//           This function handles a+b+c .  It converts (Plus a b c)
//           into
// 
//           (HORIZONTALFORM 9 1 0 0 PLUS 140 NONE (A " + " B " + " C))
define method plusformatter (x)
  let width = 0;
  let hoffset = 0;
  let voffset = 0;
  let ls = #f;
  let maxtoph = 1;
  let maxbotmh = 0;
  let heightpacket = #f;
  let ret_format = #f;
  for (args = cdr(x) then cdr(args), until empty?(args))
    ret_format := buildformat(head(args));
    if (not(instance?(ret_format, <list>)))
      inc!(width, atomwidth(ret_format) + 3);
      ls := bq-list*(ret_format, " + ", ls);
      //  beware of minuses like -x
      //  case 1: if a plus exp begins with a minus exp, we do NOT
      //  change the form of that minus exp
      elseif (ret_format.formatstruct-op = #"minus")
      if (args = tail(x))
        inc!(width, ret_format.formatstruct-width + 3);
        heightpacket := topboths(ret_format, maxtoph, maxbotmh);
        maxtoph := head(heightpacket);
        maxbotmh := tail(heightpacket);
        ls := bq-list*(ret_format, " - ", ls);
      else
        begin
          let ham = second(ret_format.formatstruct-ls);
          if (not(instance?(ham, <list>)))
            inc!(width, atomwidth(ham) + 3);
            //  check if ham is parenthesized, handle cases like
            //  a - b c which should not become a - (b c)
            else
            if (ham.formatstruct-op = #"sequence")
              let noparenham = second(ham.formatstruct-ls);
              if (~ lessprec(noparenham, #"plus")
                   & //  fix a-(b-c) case
                  ~ (noparenham.formatstruct-op = #"plus"))
                ham := noparenham;
              end if;
            end if;
            inc!(width, 3 + ham.formatstruct-width);
            heightpacket := topboths(ham, maxtoph, maxbotmh);
            maxtoph := head(heightpacket);
            maxbotmh := tail(heightpacket);
            inc!(hoffset, ham.formatstruct-hoffset);
            voffset := max(voffset, ham.formatstruct-voffset);
          end if;
          ls := bq-list*(ham, " - ", ls);
        end;
      end if;
      //  Non-minus stuff
      else
      inc!(width, 3 + ret_format.formatstruct-width);
      heightpacket := topboths(ret_format, maxtoph, maxbotmh);
      maxtoph := head(heightpacket);
      maxbotmh := tail(heightpacket);
      inc!(hoffset, ret_format.formatstruct-hoffset);
      voffset := max(voffset, ret_format.formatstruct-voffset);
      ls := bq-list*(ret_format, " + ", ls);
    end if;
  finally
    makehform(width - 3, //  adjust spacing
              maxtoph + maxbotmh, hoffset, voffset, #"plus", prec(#"plus"),
              #"none", tail(reverse!(ls)));
  end for;
end method plusformatter;

//   timesformatter:
//           It takes (times a b) and returns
// 
//           (HORIZONTALFORM 3 1 0 0 TIMES 150 NONE (A " " B))
define method timesformatter (x)
  let width = 0;
  let hoffset = 0;
  let voffset = 0;
  let ls = #f;
  let maxtoph = 1;
  let maxbotmh = 0;
  let heightpacket = #f;
  let ret_format = #f;
  let signs = 0;
  let x = timesreorg(x);
  if (isdivide(x))
    divideformatter(x);
    //  check for cases like (Times -1 Y ...), let minusformatter handle it
    //  we'd have to change this for other kinds of negatives, e.g. bigfloats.
    elseif (instance?(second(x), // LTD: Can't convert type specification.
                      and(number, not(complex)))
             & negative?(second(x)))
    if (second(x) = -1 & empty?(cdddr(x)))
      minusformatter(third(x));
    else
      //  else modify its lisp form to suit the routine
      minusformatter(bq-list*(#"times", - cadr(x), tail(tail(x))));
    end if;
    //  if no negative number, proceed as usual
    else
    for (args = cdr(x) then cdr(args), until empty?(args))
      ret_format := buildformat(head(args));
      //  count minuses, extract +ve form from it if necessary
      //  is this necessary?? rjf 1/91
      // 	  (format t "~%testing ~s" (formatstruct-op ret_format))
      if (~ not(instance?(ret_format, <list>))
           & ret_format.formatstruct-op == #"minus")
        ret_format := second(ret_format.formatstruct-ls);
        signs := signs + 1;
      elseif (nil);
      end if;
      if (not(instance?(ret_format, <list>)))
        inc!(width, atomwidth(ret_format) + 1);
      else
        if (lessprec(ret_format, #"times"))
          ret_format := parenformatter(ret_format);
        end if;
        inc!(width, 1 + ret_format.formatstruct-width);
        heightpacket := topboths(ret_format, maxtoph, maxbotmh);
        maxtoph := head(heightpacket);
        maxbotmh := tail(heightpacket);
        inc!(hoffset, ret_format.formatstruct-hoffset);
        voffset := max(voffset, ret_format.formatstruct-voffset);
      end if;
      ls := bq-list*(ret_format, " ", ls);
    finally
      //  if odd number of minuses is encountered in this times exp
      //  then pass to minusformatter
      if (odd?(signs))
        let ret_form
            = makehform(width - 1, //  adjust spacing
                        maxtoph + maxbotmh, hoffset, voffset, #"times",
                        prec(#"times"), #"none", tail(reverse!(ls)));
        if (lessprec(ret_form, #"minus"))
          ret_form := parenformatter(ret_form);
        end if;
        makehform(width + 2, maxtoph + maxbotmh, hoffset, voffset, #"minus",
                  prec(#"minus"), #"left", bq-list("-", ret_form));
      else
        makehform(width - 1, //  adjust spacing
                  maxtoph + maxbotmh, hoffset, voffset, #"times",
                  prec(#"times"), #"none", tail(reverse!(ls)));
      end if;
    end for;
  end if;
end method timesformatter;

//   TimesReorg:
//          Reorganize the times expression to form a proper
//          Divide expression (Times exp (power exp -1)) if
//          necessary.
define method timesreorg (x) x; end method timesreorg;

//  leave arg alone.
//  This was written by Derek Lai
//  keep this in reserve.  I think it's unnecessary -- RJF.
// 
//   powerformatter:
//          This function takes care of cases like e^x.  It takes (POWER E X)
//          and returns
// 
//          (HORIZONTALFORM 2 2 0 0 POWER 190 RIGHT
//               (E (VERTICALFORM 1 1 0 -1 SUPERSCRIPT 260 NONE X)))
nil(#f, nil(),
    nil((nil(nil(nil(#f))))(nil(nil(nil(#f))), nil(#f)),
        nil(nil(#f),
            nil(#f, nil(nil(#f), #f, #f, #f, #(), nil(#()), #(), #f)),
            nil(#f, nil(nil(#f), nil(#f), #f, #f, #(), nil(#()), #(), #f))),
        nil((nil(#f))(nil(nil(nil(#f), nil(#f)), nil(#f, nil(#f)), #f, #f,
                          #(), nil(#()), #(), #(#(), #()))),
            nil(nil((nil(nil(#f)))(),
                    //  find out whether expo is higher than numerator
                    nil((nil(nil(nil(nil(#f), nil(#f)), nil(#f, nil(#f)))))(),
                        nil(nil(nil(#f), nil(#f)), nil(#f, nil(#f)), #f,
                            nil(#f), #(), nil(#()), #(), #(#(), #()))))))));

//   realformatter:
//           This function takes a Lisp prefix form of a real number
//           and convert it to a real number string.
define method realformatter (x)
  let p1 = second(x);
  let p2 = third(x);
  let temp = format(#f, "%D.%D", p1, p2);
  temp;
end method realformatter;

//   parenformatter:
//           Wrap a given formatstruct with parentheses.
define method parenformatter (f)
  makehform(f.formatstruct-width + 2, f.formatstruct-height, 0,
            f.formatstruct-voffset, #"sequence", prec(#"sequence"), #"none",
            bq-list("(", f, ")"));
end method parenformatter;

//   minusformatter:
//           This function takes X and makes
//           a horizontal minus form (that is, for - X).
// 
//           (HORIZONTALFORM 2 1 0 0 MINUS 170 LEFT ("-" X))
define method minusformatter (x)
  let ret_form = buildformat(x);
  if (not(instance?(ret_form, <list>)))
    makehform(1 + atomwidth(ret_form), 1, 0, 0, #"minus", prec(#"minus"),
              #"left", bq-list("-", ret_form));
  else
    if (prec(ret_form.formatstruct-op) < prec(#"minus"))
      ret_form := parenformatter(ret_form);
    end if;
    makehform(1 + ret_form.formatstruct-width, ret_form.formatstruct-height,
              0, ret_form.formatstruct-voffset, #"minus", prec(#"minus"),
              #"left", bq-list("-", ret_form));
  end if;
end method minusformatter;

//   IsDivide:
//           Checks if a Lisp prefix form expression in fact
//           corresponds to a division expression.
//           For example, IsDivide returns TRUE with the following
//           input expression:
// 
//           (TIMES A (POWER B -1))
define method isdivide (x)
  if (size(x) ~= 3)
    #f;
  else
    let bool = #f;
    let p1 = second(x);
    let p2 = third(x);
    if (~ not(instance?(p2, <list>)))
      if (head(p2) = #"power" & third(p2) = -1) bool := #t; end if;
    end if;
    //  the first part of x must be a non -1 power
    if (~ not(instance?(p1, <list>)))
      if (head(p1) = #"power" & third(p1) = -1) bool := #f; end if;
    end if;
    bool;
  end if;
end method isdivide;

//   divideformatter:
//           In general, after a given Lisp prefix is identified as 
//           a division expression by the function "IsDivide" (above),
//           it is passed to divideformatter.  For example, it converts
//           (TIMES A (POWER B -1)) to
// 
//           (VERTICALFORM 1 3 0 1 DIVIDE 160 LEFT
//                                   (A (REPEATEDSTRING "-" 1) B))
define method divideformatter (x)
  let p1 = second(x);
  let p2 = third(x);
  let retf1 = buildformat(p1);
  let retf2
      = begin
          let temp = buildformat(second(p2));
          if (divideformp(temp))
            //  due to associativity
            parenformatter(temp);
          else
            temp;
          end if;
        end;
  let w1 = formatwidth(retf1);
  let w2 = formatwidth(retf2);
  let barwidth = max(w1, w2);
  //  stuff space
  if (abs(w1 - w2) > 1)
    if (w1 > w2)
      retf2 := stuffspace(retf2, barwidth);
    else
      retf1 := stuffspace(retf1, barwidth);
    end if;
  end if;
  makevform(barwidth, 1 + formatheight(retf1) + formatheight(retf2), 0,
            formatheight(retf2), #"divide", prec(#"divide"), #"left",
            bq-list(retf1, makerepstr(barwidth, "-"), retf2));
end method divideformatter;

//  handle complex numbers only.
//  perhaps this should be user-defined? (Could be over-ridden by
//  an appropriate mechanism like ComplexFormatter[] ... )
define method complexnumformatter (x)
  let imag
      = if (imag-part(x) = 0)
          #f;
        elseif (imag-part(x) = 1)
          #"i";
        else
          bq-list*(#"times", imag-part(x), #(#"i"));
        end if;
  buildformat(if (empty?(imag))
                real-part(x);
              elseif (0 = real-part(x))
                imag;
              else
                list(#"plus", real-part(x), imag);
              end if);
end method complexnumformatter;

//  handle the output of "Set" forms
symbol-get-property(#"set", #"stringsym") := " = ";

symbol-get-property(#"set", #"stringsymlen") := 3;

//  based on dotformatter
define method setformatter (x)
  let width = 0;
  let maxtoph = 1;
  let maxbotmh = 0;
  let hoffset = 0;
  let voffset = 0;
  let ls = #f;
  let heightpacket = #f;
  let ret_format = #f;
  for (args = cdr(x) then cdr(args), until empty?(args))
    ret_format := buildformat(head(args));
    if (not(instance?(ret_format, <list>)))
      inc!(width, atomwidth(ret_format) + 3);
      ls := bq-list*(ret_format, " = ", ls);
    else
      if (lessprec(ret_format, #"set"))
        ret_format := parenformatter(ret_format);
      end if;
      heightpacket := topboths(ret_format, maxtoph, maxbotmh);
      maxtoph := head(heightpacket);
      maxbotmh := tail(heightpacket);
      inc!(width, ret_format.formatstruct-width + 3);
      voffset := max(voffset, ret_format.formatstruct-voffset);
      inc!(hoffset, ret_format.formatstruct-hoffset);
      ls := bq-list*(ret_format, " = ", ls);
    end if;
  finally
    makehform(width - 3, maxtoph + maxbotmh, hoffset, voffset, #"set",
              prec(#"set"), #"none", tail(reverse!(ls)));
  end for;
end method setformatter;

// we can do this for now, since the results are similar
// for formatting + and *.  However the precedences are different.
//  also, perhaps the formatting symbol for Times is merely adjacency.
//   StuffSpace:
//           This function puts in spaces as, say,
//           (RepeatedString " " 6).
define method stuffspace (f, framewidth)
  let numspace = floor/(framewidth - formatwidth(f), 2);
  makehform(formatwidth(f) + numspace, formatheight(f), 0,
            if (not(instance?(f, <list>)))
              0;
            else
              f.formatstruct-voffset;
            end if,
            #"sequence", prec(#"sequence"), #"none",
            bq-list(makerepstr(numspace, " "), f));
end method stuffspace;

//   MakeRepStr:
//           It constructs repeated strings.
define method makerepstr (x, symb)
  list(#"repeatedstring", symb, x);
end method makerepstr;

