module: meta-base
author: David Lichteblau (david.lichteblau@snafu.de)
copyright: Copyright (c) 1999 David Lichteblau.  All Rights Reserved.

// Please read the file README and the documentation.
// For licensing information see LICENSE.

define macro with-collector
  { with-collector ?kind:name, ?keys:*;
      ?:body
    end }
    => { with-collector ?kind var, ?keys;
	   ?body;
	   var
	 end }

  { with-collector into-vector ?variable:name, ?keys:*; ?:body end }
    => { with-collector into-vector ?variable = make(<stretchy-vector>), ?keys;
	   ?body
	 end }

  { with-collector into-vector ?variable:name = ?init:expression,
        #key ?collect:name = dummy, ?append:name = dummy, ?index:name = index;
      ?:body
    end }
    => { let ?variable = ?init;
	 let ?index = 0;
	 local method ?collect (object) => ();
		 ?variable[?index] := object;
	         ?index := ?index + 1;
	       end method;
	 local method ?append (tail :: <vector>) => ();
		 for (i from ?index,
		      j from 0 below size(tail))
		   ?variable[i] := tail[j];
		 finally
		   ?index := i;
		 end for;
	       end method;
	 ?body }

  { with-collector into-buffer ?fn:name = ?buffer:expression, ?keys:*;
      ?:body
    end }
    => { with-collector into-vector buffer = ?buffer, ?keys, index: index;
	   local method ?fn () => (result :: <vector>);
		   copy-sequence(buffer, end: index);
		 end method ?fn;
	   ?body
	 end }

  { with-collector into-buffer ?fn:name like ?like:expression, ?keys:*;
      ?:body
    end }
    => { let like = ?like;
	 with-collector
	     into-buffer ?fn = make(type-for-copy(like), size: size(like)),
	     ?keys;
	   ?body
	 end }

  { with-collector into-list ?variable:name,
        #key ?collect:name = dummy, ?append:name = dummy;
      ?:body
    end }
    => { let ?variable = #();
	 let last-cons = #f;
	 local method ?collect (object) => ();
		 let new-cons = pair(object, #());
		 if (last-cons)
		   tail(last-cons) := new-cons;
		 else
		   ?variable := new-cons;
		 end if;
		 last-cons := new-cons;
	       end method;
	 local method ?append (new-tail :: <list>) => ();
		 unless (empty?(new-tail))
		   if (last-cons)
		     tail(last-cons) := new-tail;
		   else
		     ?variable := new-tail;
		   end if;
		   for (cons = new-tail then tail(cons),
			until: empty?(tail(cons)))
		   finally
		     last-cons := cons;
		   end for;
		 end unless;
	       end method;
	 ?body }

  { with-collector into-table ?variable:name = ?init:expression,
        #key ?collect:name = dummy;
      ?:body
    end }
    => { let ?variable = ?init;
	 local method ?collect (the-key, the-value) => ();
		 ?variable[the-key] := the-value;
	       end method;
	 ?body }

end macro with-collector;


// EOF
