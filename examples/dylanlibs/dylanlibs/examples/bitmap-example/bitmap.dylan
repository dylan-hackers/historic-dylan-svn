Module:    bitmap-example
Synopsis:  Example of reading bitmaps from files.
Author:    Chris Double
Copyright: (C) 2000, Chris Double. All rights reserved.

define class <windows-bitmap> (<image>)
  constant slot bitmap-handle :: <HBITMAP>, required-init-keyword: handle:;
end class <windows-bitmap>;

define method image-dimensions( image :: <windows-bitmap> ) 
  => (height :: <integer>, width :: <integer> )
  with-stack-structure( b :: <LPBITMAP> )
    GetObject( image.bitmap-handle, size-of(<BITMAP>), b );
    values( b.bmHeight-value, b.bmWidth-value );
  end with-stack-structure;
end method image-dimensions;

define method image-width ( image :: <windows-bitmap> ) => (width :: <integer>)
  let (h, w) = image-dimensions( image );
  w;
end method image-width;

define method image-height ( image :: <windows-bitmap> ) => (height :: <integer>)
  let (h, w) = image-dimensions(image);
  h;
end method image-height;

// Ideally this method should be specialised on <medium> not <drawing-pane> but
// when I do this with HD 1.2 I get a sealing error.
define method draw-image( pane :: <drawing-pane>, image :: <windows-bitmap>, x, y) => (record)
  let m = pane.sheet-medium;
  let hdc = m.medium-drawable.get-dc;
  let newdbc = CreateCompatibleDC(hdc);
  let old = SelectObject(newdbc, image.bitmap-handle);
  let (height, width) = image-dimensions( image );
  BitBlt(hdc, x, y, width, height, newdbc, 0, 0, $SRCCOPY);
  SelectObject(newdbc, old);
  DeleteDC(newdbc);
end method draw-image;

define method load-bitmap-from-file( filename )
  let handle = LoadImage($null-handle,
                         filename,
                         $IMAGE-BITMAP,
                         0,
                         0,
                         $LR-LOADFROMFILE); 
  if(null-handle?(handle))
    #f
  else
    make(<windows-bitmap>, handle: c-type-cast(<hbitmap>, handle));    
  end if;
end method;

