module: libpng

define function fd-to-file-ptr(fd :: <integer>)
 => (<png-FILE-p>)
  make(<png-FILE-p>, pointer: call-out("fdopen", ptr:, int: fd, ptr: (export-value(<c-string>, "r")).raw-value));
end function fd-to-file-ptr;

define function read-png(filename :: <string>) => (image, width, height, channels);
  let png-ptr 
    = png-create-read-struct($PNG-LIBPNG-VER-STRING,
                             as(<png-voidp>, 0),
                             as(<png-error-ptr>, 0),
                             as(<png-error-ptr>, 0));
  let info-ptr = png-create-info-struct(png-ptr);
    
  with-open-file(file = filename)
    png-init-io(png-ptr, fd-to-file-ptr(file.file-descriptor));
    
    png-read-png(png-ptr, info-ptr, 
                 logior($PNG-TRANSFORM-STRIP-16,
                        $PNG-TRANSFORM-PACKING,
                        $PNG-TRANSFORM-SHIFT), 
                 as(<png-voidp>, 0));
  end with-open-file;

  let width  = png-get-image-width(png-ptr, info-ptr);
  let height = png-get-image-height(png-ptr, info-ptr);
  let bit-depth = png-get-bit-depth(png-ptr, info-ptr);
  let color-type = png-get-color-type(png-ptr, info-ptr);
  let channels = png-get-channels(png-ptr, info-ptr);
  let row-pointers = png-get-rows(png-ptr, info-ptr);
      
  let image = make(<png-byte*>, element-count: width * height * channels);
  for(j from 0 below height)
    let row = pointer-value(row-pointers, index: height - j - 1);
    for(i from 0 below width)
      for(c from 0 below channels)
        pointer-value(image, index: (i + j * width) * channels + c) 
          := pointer-value(row, index: i * channels + c);
      end for;
    end for;
  end for;

  format-out("(%=x%=), %= bit depth, %= color type, %= channels\n",
             width,
             height,
             bit-depth,
             color-type,
             channels);
  // free all the PNG stuff
  values(image, width, height, channels);
end function read-png;
