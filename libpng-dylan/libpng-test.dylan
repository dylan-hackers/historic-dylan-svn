module: libpng

define function fd-to-file-ptr(fd :: <integer>)
 => (<png-FILE-p>)
  make(<png-FILE-p>, pointer: call-out("fdopen", ptr:, int: fd, ptr: (export-value(<c-string>, "r")).raw-value));
end function fd-to-file-ptr;

with-open-file(file = application-arguments()[0])
  let png-ptr 
    = png-create-read-struct($PNG-LIBPNG-VER-STRING,
                             as(<png-voidp>, 0),
                             as(<png-error-ptr>, 0),
                             as(<png-error-ptr>, 0));
  let info-ptr = png-create-info-struct(png-ptr);

  png-init-io(png-ptr, fd-to-file-ptr(file.file-descriptor));

  png-read-png(png-ptr, info-ptr, 0, as(<png-voidp>, 0));

  format-out("(%=x%=), %= bit depth, %= color type, %= channels\n",
             png-get-image-width(png-ptr, info-ptr),
             png-get-image-height(png-ptr, info-ptr),
             png-get-bit-depth(png-ptr, info-ptr),
             png-get-color-type(png-ptr, info-ptr),
             png-get-channels(png-ptr, info-ptr));
end with-open-file;

