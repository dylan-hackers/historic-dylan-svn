module: c
synopsis: 
author: 
copyright: 

begin
  local method analyze(c :: <class>)
	    let i = make(c);
	    format-out("instance of %=, %= results in: ", c, i);
	    block ()
	      m(i);
	      format-out(".\n");
	    exception (e :: <error>)
	      format-out(" exception %=\n", e);
	    end;
        end;

  
  map(analyze, list(<s>, <t>, <c>));
end;
