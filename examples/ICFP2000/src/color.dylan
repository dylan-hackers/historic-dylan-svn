module: icfp2000
synopsis: Color manipulation code
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define class <color> (<object>)
  slot red :: <float>, required-init-keyword: #"red";
  slot green :: <float>, required-init-keyword: #"green";
  slot blue :: <float>, required-init-keyword: #"blue";
end class <color>;

define method make-black() => (black :: <color>)
  make(<color>, red: 0.0, green: 0.0, blue: 0.0);
end method make-black;

define method make-white() => (white :: <color>)
  make(<color>, red: 1.0, green: 1.0, blue: 1.0);
end method make-white;

define method export-with-depth(c :: <color>, depth :: <integer>) 
 => (r :: <integer>, g :: <integer>, b :: <integer>)
  local method clampint(x)
	  if(x < 0)
	    0;
	  else
	    if(x > depth)
	      depth;
	    else
	      x;
	    end if;
	  end if;
	end method clampint;

  let d :: <float> = as(<float>, depth);
  let r = floor(c.red * d);
  let g = floor(c.green * d);
  let b = floor(c.blue * d);

  values(clampint(r), clampint(g), clampint(b));
end method export-with-depth;
