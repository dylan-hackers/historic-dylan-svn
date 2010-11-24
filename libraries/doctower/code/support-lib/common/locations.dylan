module: common
synopsis: Source location methods.


define class <generated-source-location> (<source-location>)
end class;


define constant $unknown-source-location = make(<unknown-source-location>);
define constant $generated-source-location = make(<generated-source-location>);


define method merge-file-source-locations
   (loc1 :: <file-source-location>, loc2 :: <file-source-location>)
=> (merged :: <file-source-location>)
   debug-assert(loc1.source-file = loc2.source-file);
   let earlier = 
      case
         loc1.source-start-line < loc2.source-start-line => loc1;
         loc1.source-start-line > loc2.source-start-line => loc2;
         loc1.source-start-column < loc2.source-start-column => loc1;
         otherwise => loc2;
      end case;
   let later =
      case
         loc1.source-end-line < loc2.source-end-line => loc2;
         loc1.source-end-line > loc2.source-end-line => loc1;
         loc1.source-end-column < loc2.source-end-column => loc2;
         otherwise => loc1;
      end case;
   make(<file-source-location>, file: loc1.source-file,
        start-line: earlier.source-start-line,
        start-column: earlier.source-start-column,
        end-line: later.source-end-line,
        end-column: later.source-end-column)
end method;

define method \< (loc1 :: <file-source-location>, loc2 :: <file-source-location>)
=> (less-than :: <boolean>)
   if (loc1.source-file = loc2.source-file)
      case
         loc1.source-start-line < loc2.source-start-line => #t;
         loc1.source-start-line = loc2.source-start-line 
            & loc1.source-start-column < loc2.source-start-column => #t;
      end case;
   end if;
end method;

define method \= (loc1 :: <file-source-location>, loc2 :: <file-source-location>)
=> (equal :: <boolean>)
   (loc1 == loc2) |
   (loc1.source-file = loc2.source-file) &
   (loc1.source-start-line = loc2.source-start-line) &
   (loc1.source-start-column = loc2.source-start-column) &
   (loc1.source-end-line = loc2.source-end-line) &
   (loc1.source-end-column = loc2.source-end-column)
end method;


define method print-message (o :: <file-source-location>, s :: <stream>)
=> ()
   format(s, "%s:", o.source-file);
   if (o.source-start-line = o.source-end-line)
      format(s, "%d", o.source-start-line);
      if (o.source-start-column & o.source-start-column > 1)
         format(s, ":%d", o.source-start-column);
      end if;
   else
      format(s, "%d-%d", o.source-start-line, o.source-end-line);
   end if;
end method;

define method print-message (o :: <unknown-source-location>, s :: <stream>)
=> ()
   write(s, "unspecified location")
end method;

define method print-message (o :: <generated-source-location>, s :: <stream>)
=> ()
   write(s, "automatically-generated documentation")
end method;
