module: icfp2000
synopsis: PPM output widget
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define constant $buffer-size = 1000;

define class <ppm-image> (<object>)
  slot filename, required-init-keyword: #"filename";
  slot width, required-init-keyword: #"width";
  slot height, required-init-keyword: #"height";
  slot depth, required-init-keyword: #"depth";
  slot file-handle;
  slot buf :: <byte-string>, init-value: make(<string>, size: $buffer-size);
  slot bufpos :: limited(<integer>, min: 0, max: $buffer-size - 1),
    init-value: 0;
end class <ppm-image>;

define sealed domain make(singleton(<ppm-image>));

define method initialize(ppm :: <ppm-image>, #key, #all-keys)
 => ()
  next-method();
  ppm.file-handle := make(<file-stream>, locator: ppm.filename, direction:
			    #"output");
  format(ppm.file-handle, "P6\n# Dylan Hackers\n%= %=\n%=\n",
	 ppm.width, ppm.height, ppm.depth);
end method initialize;

define method write-pixel(ppm :: <ppm-image>, color :: <color>)
 => ()
  local method write-byte(b :: <integer>) => ();
	  ppm.buf[ppm.bufpos] := as(<character>, b);
	  if (ppm.bufpos < ($buffer-size - 1))
	    ppm.bufpos := ppm.bufpos + 1;
	  else
	    write(ppm.file-handle, ppm.buf);
	    ppm.bufpos := 0;
	  end;
	end;

  let (r, g, b) = export-with-depth(color, ppm.depth);
  write-byte(r);
  write-byte(g);
  write-byte(b);
end method write-pixel;

define method close-ppm(ppm :: <ppm-image>, #key, #all-keys)
  write(ppm.file-handle, ppm.buf, end: ppm.bufpos);
  close(ppm.file-handle);
end method close-ppm;