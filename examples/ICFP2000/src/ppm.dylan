module: icfp2000
synopsis: PPM output widget
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define class <ppm-image> (<object>)
  slot filename, required-init-keyword: #"filename";
  slot width, required-init-keyword: #"width";
  slot height, required-init-keyword: #"height";
  slot depth, required-init-keyword: #"depth";
  slot file-handle;
end class <ppm-image>;

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
  let (r, g, b) = export-with-depth(color, ppm.depth);
  
  format(ppm.file-handle, "%c%c%c", as(<character>, r),
	 as(<character>, g), as(<character>, b));
end method write-pixel;

define method close-ppm(ppm :: <ppm-image>, #key, #all-keys)
  close(ppm.file-handle);
end method close-ppm;