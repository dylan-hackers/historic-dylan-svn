module: icfp2001
synopsis: Dylan Hackers entry in the Fourth Annual (2001) ICFP Programming Contest
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose


define constant <color> = one-of(#"red", #"green", #"blue", 
                                 #"cyan", #"magenta", #"yellow",
                                 #"black", #"white");

define constant <underline> = limited(<integer>, min:0, max: 3);
define constant <font-size> = limited(<integer>, min:0, max: 9);

define constant <maybe-color> = false-or(<color>);
define constant <maybe-font-size> = false-or(<font-size>);

define constant $color-to-integer-table$ =
  #[#f, #"red", #"green", #"blue", 
    #"cyan", #"magenta", #"yellow",
    #"black", #"white"];

define inline method encode-color(c :: <color>)
 => res :: <integer>;
  block (return)
    for(item keyed-by i in $color-to-integer-table$)
      if (c == item)
	return(i);
      end;
    end;
  end;
end method encode-color;

define inline method decode-color(i :: <integer>)
 => res :: <maybe-color>;
  $color-to-integer-table$[i];
end method decode-color;



define inline method extract-field
    (i :: <integer>, pos :: <integer>, size :: <integer>)
 => field :: <integer>;
  let aligned = ash(i, -pos);
  let mask = ash(1, size) - 1;
  logand(aligned, mask);
end method extract-field;

define inline method insert-field
    (i :: <integer>, newVal :: <integer>, pos :: <integer>, size :: <integer>)
 => inserted :: <integer>;
  let aligned = ash(newVal, pos);
  let mask = ash(ash(1, size) - 1, pos);
  logior(logand(aligned, mask), logand(i, lognot(mask)));
end method insert-field;

define macro attribute-slot-definer
  {define attribute-slot ?bit:expression ?:name end} =>
    {define inline method ?name(a :: <attribute>) => res :: <boolean>;
       logand(a.value, ?bit) ~= 0;
     end method ?name;

     define inline method "set-" ## ?name(a :: <attribute>)
      => newObj :: <attribute>;
	 make(<attribute>, value: logior(a.value, ?bit));
     end method "set-" ## ?name;}
end macro attribute-slot-definer;


// The attributes are encoded in binary fields accessed as "virtual slots"
// It is intended that the default value is a binary zero.  Since the
// default values of color and font-size must be "unknown", we make
// the encidings for those 0, and increase the "natural" encoding of
// font-size by 1
//
// bitfield assignments are:
//
// Name      Offset  Size
// bold        0      1
// emphasis    1      1
// italic      2      1
// strong      3      1
// typewriter  4      1
// underline   5      2
// font-size   7      4
// color      11      4

define functional class <attribute> (<object>)
  slot value :: <integer> = 0,
    init-keyword: value:;
end class <attribute>;

define attribute-slot #x01 bold end;
define attribute-slot #x02 emphasis end;
define attribute-slot #x04 italic end;
define attribute-slot #x08 strong end;
define attribute-slot #x10 typewriter end;

define inline method underline(a :: <attribute>) => res :: <underline>;
  extract-field(a.value, 5, 2);
end method underline;

define inline method set-underline(a :: <attribute>)
 => newObj :: <attribute>;
  let old = a.underline;
  let new = if (old < 3) old + 1 else old end;
  make(<attribute>, value: insert-field(a.value, new, 5, 2));
end method set-underline;

define inline method font-size(a :: <attribute>) => res :: <maybe-font-size>;
  let f = extract-field(a.value, 7, 4);
  if (f == 0)
    #f;
  else
    f - 1;
  end;
end method font-size;

define inline method set-font-size(a :: <attribute>, new-size :: <font-size>)
 => newObj :: <attribute>;
  make(<attribute>, value: insert-field(a.value, new-size + 1, 7, 4));
end method set-font-size;

define inline method color(a :: <attribute>) => res :: <maybe-color>;
  extract-field(a.value, 11, 4).decode-color;
end method color;

define inline method set-color(a :: <attribute>, new-color :: <color>)
 => newObj :: <attribute>;
  make(<attribute>, value: insert-field(a.value, new-color.encode-color, 11, 4));
end method set-color;


define sealed domain make(singleton(<attribute>));
define sealed domain initialize(<attribute>);


define method print-object(a :: <attribute>, stream :: <stream>)
 => ();
  format(stream, "attribute {val=%=, B=%=, EM=%=, I=%=, S=%=, TT=%=, ",
	 a.value, a.bold, a.emphasis, a.italic, a.strong, a.typewriter);
  format(stream, "U=%=, size=%=, color=%=}",
	 a.underline, a.font-size, a.color);
end method print-object;


define function dump(name :: <byte-string>, val :: <attribute>) => ()
//  format-out("%= = %=\n", val);
  print-object(val, *standard-output*);
end function dump;


define function test-attributes()
  let a = make(<attribute>, value: 0);
  dump("a", a);
  let b = make(<attribute>, value: 31 + 2 * 32 + 7 * 128 + 3 * 2048);
  dump("b", b);
end test-attributes;
