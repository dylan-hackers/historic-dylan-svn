module: libpng

c-include("/usr/include/png.h");

define functional class <anonymous-3> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-3>));

define method pointer-value
    (ptr :: <anonymous-3>, #key index = 0)
 => (result :: <integer>);
  signed-byte-at(ptr, offset: index * 1);
end method pointer-value;

define method pointer-value-setter
    (value :: <integer>, ptr :: <anonymous-3>, #key index = 0)
 => (result :: <integer>);
  signed-byte-at(ptr, offset: index * 1) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-3>)) => (result :: <integer>);
  1;
end method content-size;

define functional class <anonymous-291> (<anonymous-3>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-291>));

define method content-size (value == <anonymous-291>)  => (result :: <integer>);
  18;
end method content-size;

define sealed method png-libpng-ver () => (result :: <anonymous-291>);
  as(<anonymous-291>, c-variable-ref(ptr: "&png_libpng_ver"));
end method png-libpng-ver;

define functional class <anonymous-36> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-36>));

define method pointer-value
    (ptr :: <anonymous-36>, #key index = 0)
 => (result :: <integer>);
  signed-long-at(ptr, offset: index * 4);
end method pointer-value;

define method pointer-value-setter
    (value :: <integer>, ptr :: <anonymous-36>, #key index = 0)
 => (result :: <integer>);
  signed-long-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-36>)) => (result :: <integer>);
  4;
end method content-size;

define functional class <anonymous-292> (<anonymous-36>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-292>));

define method content-size (value == <anonymous-292>)  => (result :: <integer>);
  7;
end method content-size;

define sealed method png-pass-start () => (result :: <anonymous-292>);
  as(<anonymous-292>, c-variable-ref(ptr: "&png_pass_start"));
end method png-pass-start;

define functional class <anonymous-293> (<anonymous-36>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-293>));

define method content-size (value == <anonymous-293>)  => (result :: <integer>);
  7;
end method content-size;

define sealed method png-pass-inc () => (result :: <anonymous-293>);
  as(<anonymous-293>, c-variable-ref(ptr: "&png_pass_inc"));
end method png-pass-inc;

define functional class <anonymous-294> (<anonymous-36>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-294>));

define method content-size (value == <anonymous-294>)  => (result :: <integer>);
  7;
end method content-size;

define sealed method png-pass-ystart () => (result :: <anonymous-294>);
  as(<anonymous-294>, c-variable-ref(ptr: "&png_pass_ystart"));
end method png-pass-ystart;

define functional class <anonymous-295> (<anonymous-36>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-295>));

define method content-size (value == <anonymous-295>)  => (result :: <integer>);
  7;
end method content-size;

define sealed method png-pass-yinc () => (result :: <anonymous-295>);
  as(<anonymous-295>, c-variable-ref(ptr: "&png_pass_yinc"));
end method png-pass-yinc;

define functional class <anonymous-296> (<anonymous-36>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-296>));

define method content-size (value == <anonymous-296>)  => (result :: <integer>);
  7;
end method content-size;

define sealed method png-pass-mask () => (result :: <anonymous-296>);
  as(<anonymous-296>, c-variable-ref(ptr: "&png_pass_mask"));
end method png-pass-mask;

define functional class <anonymous-297> (<anonymous-36>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-297>));

define method content-size (value == <anonymous-297>)  => (result :: <integer>);
  7;
end method content-size;

define sealed method png-pass-dsp-mask () => (result :: <anonymous-297>);
  as(<anonymous-297>, c-variable-ref(ptr: "&png_pass_dsp_mask"));
end method png-pass-dsp-mask;

define constant <png-byte> = <integer>;

define functional class <png-color-struct> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<png-color-struct>));

define sealed method png_color_struct$red
    (ptr :: <png-color-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 0);
end method png_color_struct$red;

define sealed method png_color_struct$red-setter
    (value :: <png-byte>, ptr :: <png-color-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 0) := value;
  value;
end method png_color_struct$red-setter;

define sealed method png_color_struct$green
    (ptr :: <png-color-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 1);
end method png_color_struct$green;

define sealed method png_color_struct$green-setter
    (value :: <png-byte>, ptr :: <png-color-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 1) := value;
  value;
end method png_color_struct$green-setter;

define sealed method png_color_struct$blue
    (ptr :: <png-color-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 2);
end method png_color_struct$blue;

define sealed method png_color_struct$blue-setter
    (value :: <png-byte>, ptr :: <png-color-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 2) := value;
  value;
end method png_color_struct$blue-setter;

define method pointer-value (value :: <png-color-struct>, #key index = 0) => (result :: <png-color-struct>);
  value + index * 3;
end method pointer-value;

define method content-size (value :: subclass(<png-color-struct>)) => (result :: <integer>);
  3;
end method content-size;

define constant <png-color> = <png-color-struct>;

define constant <png-colorp> = <png-color>;

define functional class <anonymous-298> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-298>));

define method pointer-value
    (ptr :: <anonymous-298>, #key index = 0)
 => (result :: <png-color>);
  pointer-at(ptr, offset: index * 4, class: <png-color>);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-color>, ptr :: <anonymous-298>, #key index = 0)
 => (result :: <png-color>);
  pointer-at(ptr, offset: index * 4, class: <png-color>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-298>)) => (result :: <integer>);
  4;
end method content-size;

define constant <png-colorpp> = <anonymous-298>;

define constant <png-uint-16> = <integer>;

define functional class <png-color-16-struct> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<png-color-16-struct>));

define sealed method png_color_16_struct$index
    (ptr :: <png-color-16-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 0);
end method png_color_16_struct$index;

define sealed method png_color_16_struct$index-setter
    (value :: <png-byte>, ptr :: <png-color-16-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 0) := value;
  value;
end method png_color_16_struct$index-setter;

define sealed method png_color_16_struct$red
    (ptr :: <png-color-16-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 2);
end method png_color_16_struct$red;

define sealed method png_color_16_struct$red-setter
    (value :: <png-uint-16>, ptr :: <png-color-16-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 2) := value;
  value;
end method png_color_16_struct$red-setter;

define sealed method png_color_16_struct$green
    (ptr :: <png-color-16-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 4);
end method png_color_16_struct$green;

define sealed method png_color_16_struct$green-setter
    (value :: <png-uint-16>, ptr :: <png-color-16-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 4) := value;
  value;
end method png_color_16_struct$green-setter;

define sealed method png_color_16_struct$blue
    (ptr :: <png-color-16-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 6);
end method png_color_16_struct$blue;

define sealed method png_color_16_struct$blue-setter
    (value :: <png-uint-16>, ptr :: <png-color-16-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 6) := value;
  value;
end method png_color_16_struct$blue-setter;

define sealed method png_color_16_struct$gray
    (ptr :: <png-color-16-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 8);
end method png_color_16_struct$gray;

define sealed method png_color_16_struct$gray-setter
    (value :: <png-uint-16>, ptr :: <png-color-16-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 8) := value;
  value;
end method png_color_16_struct$gray-setter;

define method pointer-value (value :: <png-color-16-struct>, #key index = 0) => (result :: <png-color-16-struct>);
  value + index * 10;
end method pointer-value;

define method content-size (value :: subclass(<png-color-16-struct>)) => (result :: <integer>);
  10;
end method content-size;

define constant <png-color-16> = <png-color-16-struct>;

define constant <png-color-16p> = <png-color-16>;

define functional class <anonymous-299> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-299>));

define method pointer-value
    (ptr :: <anonymous-299>, #key index = 0)
 => (result :: <png-color-16>);
  pointer-at(ptr, offset: index * 4, class: <png-color-16>);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-color-16>, ptr :: <anonymous-299>, #key index = 0)
 => (result :: <png-color-16>);
  pointer-at(ptr, offset: index * 4, class: <png-color-16>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-299>)) => (result :: <integer>);
  4;
end method content-size;

define constant <png-color-16pp> = <anonymous-299>;

define functional class <png-color-8-struct> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<png-color-8-struct>));

define sealed method png_color_8_struct$red
    (ptr :: <png-color-8-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 0);
end method png_color_8_struct$red;

define sealed method png_color_8_struct$red-setter
    (value :: <png-byte>, ptr :: <png-color-8-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 0) := value;
  value;
end method png_color_8_struct$red-setter;

define sealed method png_color_8_struct$green
    (ptr :: <png-color-8-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 1);
end method png_color_8_struct$green;

define sealed method png_color_8_struct$green-setter
    (value :: <png-byte>, ptr :: <png-color-8-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 1) := value;
  value;
end method png_color_8_struct$green-setter;

define sealed method png_color_8_struct$blue
    (ptr :: <png-color-8-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 2);
end method png_color_8_struct$blue;

define sealed method png_color_8_struct$blue-setter
    (value :: <png-byte>, ptr :: <png-color-8-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 2) := value;
  value;
end method png_color_8_struct$blue-setter;

define sealed method png_color_8_struct$gray
    (ptr :: <png-color-8-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 3);
end method png_color_8_struct$gray;

define sealed method png_color_8_struct$gray-setter
    (value :: <png-byte>, ptr :: <png-color-8-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 3) := value;
  value;
end method png_color_8_struct$gray-setter;

define sealed method png_color_8_struct$alpha
    (ptr :: <png-color-8-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 4);
end method png_color_8_struct$alpha;

define sealed method png_color_8_struct$alpha-setter
    (value :: <png-byte>, ptr :: <png-color-8-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 4) := value;
  value;
end method png_color_8_struct$alpha-setter;

define method pointer-value (value :: <png-color-8-struct>, #key index = 0) => (result :: <png-color-8-struct>);
  value + index * 5;
end method pointer-value;

define method content-size (value :: subclass(<png-color-8-struct>)) => (result :: <integer>);
  5;
end method content-size;

define constant <png-color-8> = <png-color-8-struct>;

define constant <png-color-8p> = <png-color-8>;

define functional class <anonymous-300> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-300>));

define method pointer-value
    (ptr :: <anonymous-300>, #key index = 0)
 => (result :: <png-color-8>);
  pointer-at(ptr, offset: index * 4, class: <png-color-8>);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-color-8>, ptr :: <anonymous-300>, #key index = 0)
 => (result :: <png-color-8>);
  pointer-at(ptr, offset: index * 4, class: <png-color-8>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-300>)) => (result :: <integer>);
  4;
end method content-size;

define constant <png-color-8pp> = <anonymous-300>;

define functional class <png-sPLT-entry-struct> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<png-sPLT-entry-struct>));

define sealed method png_sPLT_entry_struct$red
    (ptr :: <png-sPLT-entry-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 0);
end method png_sPLT_entry_struct$red;

define sealed method png_sPLT_entry_struct$red-setter
    (value :: <png-uint-16>, ptr :: <png-sPLT-entry-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 0) := value;
  value;
end method png_sPLT_entry_struct$red-setter;

define sealed method png_sPLT_entry_struct$green
    (ptr :: <png-sPLT-entry-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 2);
end method png_sPLT_entry_struct$green;

define sealed method png_sPLT_entry_struct$green-setter
    (value :: <png-uint-16>, ptr :: <png-sPLT-entry-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 2) := value;
  value;
end method png_sPLT_entry_struct$green-setter;

define sealed method png_sPLT_entry_struct$blue
    (ptr :: <png-sPLT-entry-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 4);
end method png_sPLT_entry_struct$blue;

define sealed method png_sPLT_entry_struct$blue-setter
    (value :: <png-uint-16>, ptr :: <png-sPLT-entry-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 4) := value;
  value;
end method png_sPLT_entry_struct$blue-setter;

define sealed method png_sPLT_entry_struct$alpha
    (ptr :: <png-sPLT-entry-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 6);
end method png_sPLT_entry_struct$alpha;

define sealed method png_sPLT_entry_struct$alpha-setter
    (value :: <png-uint-16>, ptr :: <png-sPLT-entry-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 6) := value;
  value;
end method png_sPLT_entry_struct$alpha-setter;

define sealed method png_sPLT_entry_struct$frequency
    (ptr :: <png-sPLT-entry-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 8);
end method png_sPLT_entry_struct$frequency;

define sealed method png_sPLT_entry_struct$frequency-setter
    (value :: <png-uint-16>, ptr :: <png-sPLT-entry-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 8) := value;
  value;
end method png_sPLT_entry_struct$frequency-setter;

define method pointer-value (value :: <png-sPLT-entry-struct>, #key index = 0) => (result :: <png-sPLT-entry-struct>);
  value + index * 10;
end method pointer-value;

define method content-size (value :: subclass(<png-sPLT-entry-struct>)) => (result :: <integer>);
  10;
end method content-size;

define constant <png-sPLT-entry> = <png-sPLT-entry-struct>;

define constant <png-sPLT-entryp> = <png-sPLT-entry>;

define functional class <anonymous-301> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-301>));

define method pointer-value
    (ptr :: <anonymous-301>, #key index = 0)
 => (result :: <png-sPLT-entry>);
  pointer-at(ptr, offset: index * 4, class: <png-sPLT-entry>);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-sPLT-entry>, ptr :: <anonymous-301>, #key index = 0)
 => (result :: <png-sPLT-entry>);
  pointer-at(ptr, offset: index * 4, class: <png-sPLT-entry>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-301>)) => (result :: <integer>);
  4;
end method content-size;

define constant <png-sPLT-entrypp> = <anonymous-301>;

define constant <png-charp> = <anonymous-3>;

define constant <png-int-32> = <integer>;

define functional class <png-sPLT-struct> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<png-sPLT-struct>));

define sealed method png_sPLT_struct$name
    (ptr :: <png-sPLT-struct>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 0, class: <png-charp>);
end method png_sPLT_struct$name;

define sealed method png_sPLT_struct$name-setter
    (value :: <png-charp>, ptr :: <png-sPLT-struct>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 0, class: <png-charp>) := value;
  value;
end method png_sPLT_struct$name-setter;

define sealed method png_sPLT_struct$depth
    (ptr :: <png-sPLT-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 4);
end method png_sPLT_struct$depth;

define sealed method png_sPLT_struct$depth-setter
    (value :: <png-byte>, ptr :: <png-sPLT-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 4) := value;
  value;
end method png_sPLT_struct$depth-setter;

define sealed method png_sPLT_struct$entries
    (ptr :: <png-sPLT-struct>) => (result :: <png-sPLT-entryp>);
  pointer-at(ptr, offset: 8, class: <png-sPLT-entryp>);
end method png_sPLT_struct$entries;

define sealed method png_sPLT_struct$entries-setter
    (value :: <png-sPLT-entryp>, ptr :: <png-sPLT-struct>) => (result :: <png-sPLT-entryp>);
  pointer-at(ptr, offset: 8, class: <png-sPLT-entryp>) := value;
  value;
end method png_sPLT_struct$entries-setter;

define sealed method png_sPLT_struct$nentries
    (ptr :: <png-sPLT-struct>) => (result :: <png-int-32>);
  signed-long-at(ptr, offset: 12);
end method png_sPLT_struct$nentries;

define sealed method png_sPLT_struct$nentries-setter
    (value :: <png-int-32>, ptr :: <png-sPLT-struct>) => (result :: <png-int-32>);
  signed-long-at(ptr, offset: 12) := value;
  value;
end method png_sPLT_struct$nentries-setter;

define method pointer-value (value :: <png-sPLT-struct>, #key index = 0) => (result :: <png-sPLT-struct>);
  value + index * 16;
end method pointer-value;

define method content-size (value :: subclass(<png-sPLT-struct>)) => (result :: <integer>);
  16;
end method content-size;

define constant <png-sPLT-t> = <png-sPLT-struct>;

define constant <png-sPLT-tp> = <png-sPLT-t>;

define functional class <anonymous-302> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-302>));

define method pointer-value
    (ptr :: <anonymous-302>, #key index = 0)
 => (result :: <png-sPLT-t>);
  pointer-at(ptr, offset: index * 4, class: <png-sPLT-t>);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-sPLT-t>, ptr :: <anonymous-302>, #key index = 0)
 => (result :: <png-sPLT-t>);
  pointer-at(ptr, offset: index * 4, class: <png-sPLT-t>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-302>)) => (result :: <integer>);
  4;
end method content-size;

define constant <png-sPLT-tpp> = <anonymous-302>;

define constant <size-t> = <integer>;

define constant <png-size-t> = <size-t>;

define functional class <png-text-struct> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<png-text-struct>));

define sealed method png_text_struct$compression
    (ptr :: <png-text-struct>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0);
end method png_text_struct$compression;

define sealed method png_text_struct$compression-setter
    (value :: <integer>, ptr :: <png-text-struct>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0) := value;
  value;
end method png_text_struct$compression-setter;

define sealed method png_text_struct$key
    (ptr :: <png-text-struct>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 4, class: <png-charp>);
end method png_text_struct$key;

define sealed method png_text_struct$key-setter
    (value :: <png-charp>, ptr :: <png-text-struct>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 4, class: <png-charp>) := value;
  value;
end method png_text_struct$key-setter;

define sealed method png_text_struct$text
    (ptr :: <png-text-struct>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 8, class: <png-charp>);
end method png_text_struct$text;

define sealed method png_text_struct$text-setter
    (value :: <png-charp>, ptr :: <png-text-struct>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 8, class: <png-charp>) := value;
  value;
end method png_text_struct$text-setter;

define sealed method png_text_struct$text-length
    (ptr :: <png-text-struct>) => (result :: <png-size-t>);
  unsigned-long-at(ptr, offset: 12);
end method png_text_struct$text-length;

define sealed method png_text_struct$text-length-setter
    (value :: <png-size-t>, ptr :: <png-text-struct>) => (result :: <png-size-t>);
  unsigned-long-at(ptr, offset: 12) := value;
  value;
end method png_text_struct$text-length-setter;

define method pointer-value (value :: <png-text-struct>, #key index = 0) => (result :: <png-text-struct>);
  value + index * 16;
end method pointer-value;

define method content-size (value :: subclass(<png-text-struct>)) => (result :: <integer>);
  16;
end method content-size;

define constant <png-text> = <png-text-struct>;

define constant <png-textp> = <png-text>;

define functional class <anonymous-303> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-303>));

define method pointer-value
    (ptr :: <anonymous-303>, #key index = 0)
 => (result :: <png-text>);
  pointer-at(ptr, offset: index * 4, class: <png-text>);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-text>, ptr :: <anonymous-303>, #key index = 0)
 => (result :: <png-text>);
  pointer-at(ptr, offset: index * 4, class: <png-text>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-303>)) => (result :: <integer>);
  4;
end method content-size;

define constant <png-textpp> = <anonymous-303>;

define functional class <png-time-struct> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<png-time-struct>));

define sealed method png_time_struct$year
    (ptr :: <png-time-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 0);
end method png_time_struct$year;

define sealed method png_time_struct$year-setter
    (value :: <png-uint-16>, ptr :: <png-time-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 0) := value;
  value;
end method png_time_struct$year-setter;

define sealed method png_time_struct$month
    (ptr :: <png-time-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 2);
end method png_time_struct$month;

define sealed method png_time_struct$month-setter
    (value :: <png-byte>, ptr :: <png-time-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 2) := value;
  value;
end method png_time_struct$month-setter;

define sealed method png_time_struct$day
    (ptr :: <png-time-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 3);
end method png_time_struct$day;

define sealed method png_time_struct$day-setter
    (value :: <png-byte>, ptr :: <png-time-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 3) := value;
  value;
end method png_time_struct$day-setter;

define sealed method png_time_struct$hour
    (ptr :: <png-time-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 4);
end method png_time_struct$hour;

define sealed method png_time_struct$hour-setter
    (value :: <png-byte>, ptr :: <png-time-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 4) := value;
  value;
end method png_time_struct$hour-setter;

define sealed method png_time_struct$minute
    (ptr :: <png-time-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 5);
end method png_time_struct$minute;

define sealed method png_time_struct$minute-setter
    (value :: <png-byte>, ptr :: <png-time-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 5) := value;
  value;
end method png_time_struct$minute-setter;

define sealed method png_time_struct$second
    (ptr :: <png-time-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 6);
end method png_time_struct$second;

define sealed method png_time_struct$second-setter
    (value :: <png-byte>, ptr :: <png-time-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 6) := value;
  value;
end method png_time_struct$second-setter;

define method pointer-value (value :: <png-time-struct>, #key index = 0) => (result :: <png-time-struct>);
  value + index * 8;
end method pointer-value;

define method content-size (value :: subclass(<png-time-struct>)) => (result :: <integer>);
  8;
end method content-size;

define constant <png-time> = <png-time-struct>;

define constant <png-timep> = <png-time>;

define functional class <anonymous-304> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-304>));

define method pointer-value
    (ptr :: <anonymous-304>, #key index = 0)
 => (result :: <png-time>);
  pointer-at(ptr, offset: index * 4, class: <png-time>);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-time>, ptr :: <anonymous-304>, #key index = 0)
 => (result :: <png-time>);
  pointer-at(ptr, offset: index * 4, class: <png-time>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-304>)) => (result :: <integer>);
  4;
end method content-size;

define constant <png-timepp> = <anonymous-304>;

define functional class <anonymous-275> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-275>));

define method pointer-value
    (ptr :: <anonymous-275>, #key index = 0)
 => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: index * 1);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-byte>, ptr :: <anonymous-275>, #key index = 0)
 => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: index * 1) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-275>)) => (result :: <integer>);
  1;
end method content-size;

define functional class <anonymous-305> (<anonymous-275>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-305>));

define method content-size (value == <anonymous-305>)  => (result :: <integer>);
  5;
end method content-size;

define functional class <png-unknown-chunk-t> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<png-unknown-chunk-t>));

define sealed method png_unknown_chunk_t$name
    (ptr :: <png-unknown-chunk-t>) => (result :: <anonymous-305>);
  as(<anonymous-305>, ptr + 0);
end method png_unknown_chunk_t$name;

define sealed method png_unknown_chunk_t$data
    (ptr :: <png-unknown-chunk-t>) => (result :: <anonymous-275>);
  pointer-at(ptr, offset: 8, class: <anonymous-275>);
end method png_unknown_chunk_t$data;

define sealed method png_unknown_chunk_t$data-setter
    (value :: <anonymous-275>, ptr :: <png-unknown-chunk-t>) => (result :: <anonymous-275>);
  pointer-at(ptr, offset: 8, class: <anonymous-275>) := value;
  value;
end method png_unknown_chunk_t$data-setter;

define sealed method png_unknown_chunk_t$size
    (ptr :: <png-unknown-chunk-t>) => (result :: <png-size-t>);
  unsigned-long-at(ptr, offset: 12);
end method png_unknown_chunk_t$size;

define sealed method png_unknown_chunk_t$size-setter
    (value :: <png-size-t>, ptr :: <png-unknown-chunk-t>) => (result :: <png-size-t>);
  unsigned-long-at(ptr, offset: 12) := value;
  value;
end method png_unknown_chunk_t$size-setter;

define sealed method png_unknown_chunk_t$location
    (ptr :: <png-unknown-chunk-t>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 16);
end method png_unknown_chunk_t$location;

define sealed method png_unknown_chunk_t$location-setter
    (value :: <png-byte>, ptr :: <png-unknown-chunk-t>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 16) := value;
  value;
end method png_unknown_chunk_t$location-setter;

define method pointer-value (value :: <png-unknown-chunk-t>, #key index = 0) => (result :: <png-unknown-chunk-t>);
  value + index * 20;
end method pointer-value;

define method content-size (value :: subclass(<png-unknown-chunk-t>)) => (result :: <integer>);
  20;
end method content-size;

define constant <png-unknown-chunk> = <png-unknown-chunk-t>;

define constant <png-unknown-chunkp> = <png-unknown-chunk>;

define functional class <anonymous-306> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-306>));

define method pointer-value
    (ptr :: <anonymous-306>, #key index = 0)
 => (result :: <png-unknown-chunk>);
  pointer-at(ptr, offset: index * 4, class: <png-unknown-chunk>);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-unknown-chunk>, ptr :: <anonymous-306>, #key index = 0)
 => (result :: <png-unknown-chunk>);
  pointer-at(ptr, offset: index * 4, class: <png-unknown-chunk>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-306>)) => (result :: <integer>);
  4;
end method content-size;

define constant <png-unknown-chunkpp> = <anonymous-306>;

define constant <png-uint-32> = <integer>;

define functional class <anonymous-307> (<anonymous-275>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-307>));

define method content-size (value == <anonymous-307>)  => (result :: <integer>);
  8;
end method content-size;

define constant <png-bytep> = <anonymous-275>;

define functional class <anonymous-278> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-278>));

define method pointer-value
    (ptr :: <anonymous-278>, #key index = 0)
 => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: index * 2);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-uint-16>, ptr :: <anonymous-278>, #key index = 0)
 => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: index * 2) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-278>)) => (result :: <integer>);
  2;
end method content-size;

define constant <png-uint-16p> = <anonymous-278>;

define functional class <anonymous-73> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-73>));

define method pointer-value
    (ptr :: <anonymous-73>, #key index = 0)
 => (result :: <anonymous-3>);
  pointer-at(ptr, offset: index * 4, class: <anonymous-3>);
end method pointer-value;

define method pointer-value-setter
    (value :: <anonymous-3>, ptr :: <anonymous-73>, #key index = 0)
 => (result :: <anonymous-3>);
  pointer-at(ptr, offset: index * 4, class: <anonymous-3>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-73>)) => (result :: <integer>);
  4;
end method content-size;

define constant <png-charpp> = <anonymous-73>;

define functional class <anonymous-282> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-282>));

define method pointer-value
    (ptr :: <anonymous-282>, #key index = 0)
 => (result :: <anonymous-275>);
  pointer-at(ptr, offset: index * 4, class: <anonymous-275>);
end method pointer-value;

define method pointer-value-setter
    (value :: <anonymous-275>, ptr :: <anonymous-282>, #key index = 0)
 => (result :: <anonymous-275>);
  pointer-at(ptr, offset: index * 4, class: <anonymous-275>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-282>)) => (result :: <integer>);
  4;
end method content-size;

define constant <png-bytepp> = <anonymous-282>;

define constant <png-fixed-point> = <png-int-32>;

define functional class <png-info-struct> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<png-info-struct>));

define sealed method png_info_struct$width
    (ptr :: <png-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 0);
end method png_info_struct$width;

define sealed method png_info_struct$width-setter
    (value :: <png-uint-32>, ptr :: <png-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 0) := value;
  value;
end method png_info_struct$width-setter;

define sealed method png_info_struct$height
    (ptr :: <png-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 4);
end method png_info_struct$height;

define sealed method png_info_struct$height-setter
    (value :: <png-uint-32>, ptr :: <png-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 4) := value;
  value;
end method png_info_struct$height-setter;

define sealed method png_info_struct$valid
    (ptr :: <png-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 8);
end method png_info_struct$valid;

define sealed method png_info_struct$valid-setter
    (value :: <png-uint-32>, ptr :: <png-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 8) := value;
  value;
end method png_info_struct$valid-setter;

define sealed method png_info_struct$rowbytes
    (ptr :: <png-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 12);
end method png_info_struct$rowbytes;

define sealed method png_info_struct$rowbytes-setter
    (value :: <png-uint-32>, ptr :: <png-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 12) := value;
  value;
end method png_info_struct$rowbytes-setter;

define sealed method png_info_struct$palette
    (ptr :: <png-info-struct>) => (result :: <png-colorp>);
  pointer-at(ptr, offset: 16, class: <png-colorp>);
end method png_info_struct$palette;

define sealed method png_info_struct$palette-setter
    (value :: <png-colorp>, ptr :: <png-info-struct>) => (result :: <png-colorp>);
  pointer-at(ptr, offset: 16, class: <png-colorp>) := value;
  value;
end method png_info_struct$palette-setter;

define sealed method png_info_struct$num-palette
    (ptr :: <png-info-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 20);
end method png_info_struct$num-palette;

define sealed method png_info_struct$num-palette-setter
    (value :: <png-uint-16>, ptr :: <png-info-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 20) := value;
  value;
end method png_info_struct$num-palette-setter;

define sealed method png_info_struct$num-trans
    (ptr :: <png-info-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 22);
end method png_info_struct$num-trans;

define sealed method png_info_struct$num-trans-setter
    (value :: <png-uint-16>, ptr :: <png-info-struct>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 22) := value;
  value;
end method png_info_struct$num-trans-setter;

define sealed method png_info_struct$bit-depth
    (ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 24);
end method png_info_struct$bit-depth;

define sealed method png_info_struct$bit-depth-setter
    (value :: <png-byte>, ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 24) := value;
  value;
end method png_info_struct$bit-depth-setter;

define sealed method png_info_struct$color-type
    (ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 25);
end method png_info_struct$color-type;

define sealed method png_info_struct$color-type-setter
    (value :: <png-byte>, ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 25) := value;
  value;
end method png_info_struct$color-type-setter;

define sealed method png_info_struct$compression-type
    (ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 26);
end method png_info_struct$compression-type;

define sealed method png_info_struct$compression-type-setter
    (value :: <png-byte>, ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 26) := value;
  value;
end method png_info_struct$compression-type-setter;

define sealed method png_info_struct$filter-type
    (ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 27);
end method png_info_struct$filter-type;

define sealed method png_info_struct$filter-type-setter
    (value :: <png-byte>, ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 27) := value;
  value;
end method png_info_struct$filter-type-setter;

define sealed method png_info_struct$interlace-type
    (ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 28);
end method png_info_struct$interlace-type;

define sealed method png_info_struct$interlace-type-setter
    (value :: <png-byte>, ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 28) := value;
  value;
end method png_info_struct$interlace-type-setter;

define sealed method png_info_struct$channels
    (ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 29);
end method png_info_struct$channels;

define sealed method png_info_struct$channels-setter
    (value :: <png-byte>, ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 29) := value;
  value;
end method png_info_struct$channels-setter;

define sealed method png_info_struct$pixel-depth
    (ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 30);
end method png_info_struct$pixel-depth;

define sealed method png_info_struct$pixel-depth-setter
    (value :: <png-byte>, ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 30) := value;
  value;
end method png_info_struct$pixel-depth-setter;

define sealed method png_info_struct$spare-byte
    (ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 31);
end method png_info_struct$spare-byte;

define sealed method png_info_struct$spare-byte-setter
    (value :: <png-byte>, ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 31) := value;
  value;
end method png_info_struct$spare-byte-setter;

define sealed method png_info_struct$signature
    (ptr :: <png-info-struct>) => (result :: <anonymous-307>);
  as(<anonymous-307>, ptr + 32);
end method png_info_struct$signature;

define sealed method png_info_struct$gamma
    (ptr :: <png-info-struct>) => (result :: <single-float>);
  float-at(ptr, offset: 40);
end method png_info_struct$gamma;

define sealed method png_info_struct$gamma-setter
    (value :: <single-float>, ptr :: <png-info-struct>) => (result :: <single-float>);
  float-at(ptr, offset: 40) := value;
  value;
end method png_info_struct$gamma-setter;

define sealed method png_info_struct$srgb-intent
    (ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 44);
end method png_info_struct$srgb-intent;

define sealed method png_info_struct$srgb-intent-setter
    (value :: <png-byte>, ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 44) := value;
  value;
end method png_info_struct$srgb-intent-setter;

define sealed method png_info_struct$num-text
    (ptr :: <png-info-struct>) => (result :: <integer>);
  signed-long-at(ptr, offset: 48);
end method png_info_struct$num-text;

define sealed method png_info_struct$num-text-setter
    (value :: <integer>, ptr :: <png-info-struct>) => (result :: <integer>);
  signed-long-at(ptr, offset: 48) := value;
  value;
end method png_info_struct$num-text-setter;

define sealed method png_info_struct$max-text
    (ptr :: <png-info-struct>) => (result :: <integer>);
  signed-long-at(ptr, offset: 52);
end method png_info_struct$max-text;

define sealed method png_info_struct$max-text-setter
    (value :: <integer>, ptr :: <png-info-struct>) => (result :: <integer>);
  signed-long-at(ptr, offset: 52) := value;
  value;
end method png_info_struct$max-text-setter;

define sealed method png_info_struct$text
    (ptr :: <png-info-struct>) => (result :: <png-textp>);
  pointer-at(ptr, offset: 56, class: <png-textp>);
end method png_info_struct$text;

define sealed method png_info_struct$text-setter
    (value :: <png-textp>, ptr :: <png-info-struct>) => (result :: <png-textp>);
  pointer-at(ptr, offset: 56, class: <png-textp>) := value;
  value;
end method png_info_struct$text-setter;

define sealed method png_info_struct$mod-time
    (ptr :: <png-info-struct>) => (result :: <png-time>);
  as(<png-time>, ptr + 60);
end method png_info_struct$mod-time;

define sealed method png_info_struct$sig-bit
    (ptr :: <png-info-struct>) => (result :: <png-color-8>);
  as(<png-color-8>, ptr + 68);
end method png_info_struct$sig-bit;

define sealed method png_info_struct$trans
    (ptr :: <png-info-struct>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 76, class: <png-bytep>);
end method png_info_struct$trans;

define sealed method png_info_struct$trans-setter
    (value :: <png-bytep>, ptr :: <png-info-struct>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 76, class: <png-bytep>) := value;
  value;
end method png_info_struct$trans-setter;

define sealed method png_info_struct$trans-values
    (ptr :: <png-info-struct>) => (result :: <png-color-16>);
  as(<png-color-16>, ptr + 80);
end method png_info_struct$trans-values;

define sealed method png_info_struct$background
    (ptr :: <png-info-struct>) => (result :: <png-color-16>);
  as(<png-color-16>, ptr + 90);
end method png_info_struct$background;

define sealed method png_info_struct$x-offset
    (ptr :: <png-info-struct>) => (result :: <png-int-32>);
  signed-long-at(ptr, offset: 100);
end method png_info_struct$x-offset;

define sealed method png_info_struct$x-offset-setter
    (value :: <png-int-32>, ptr :: <png-info-struct>) => (result :: <png-int-32>);
  signed-long-at(ptr, offset: 100) := value;
  value;
end method png_info_struct$x-offset-setter;

define sealed method png_info_struct$y-offset
    (ptr :: <png-info-struct>) => (result :: <png-int-32>);
  signed-long-at(ptr, offset: 104);
end method png_info_struct$y-offset;

define sealed method png_info_struct$y-offset-setter
    (value :: <png-int-32>, ptr :: <png-info-struct>) => (result :: <png-int-32>);
  signed-long-at(ptr, offset: 104) := value;
  value;
end method png_info_struct$y-offset-setter;

define sealed method png_info_struct$offset-unit-type
    (ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 108);
end method png_info_struct$offset-unit-type;

define sealed method png_info_struct$offset-unit-type-setter
    (value :: <png-byte>, ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 108) := value;
  value;
end method png_info_struct$offset-unit-type-setter;

define sealed method png_info_struct$x-pixels-per-unit
    (ptr :: <png-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 112);
end method png_info_struct$x-pixels-per-unit;

define sealed method png_info_struct$x-pixels-per-unit-setter
    (value :: <png-uint-32>, ptr :: <png-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 112) := value;
  value;
end method png_info_struct$x-pixels-per-unit-setter;

define sealed method png_info_struct$y-pixels-per-unit
    (ptr :: <png-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 116);
end method png_info_struct$y-pixels-per-unit;

define sealed method png_info_struct$y-pixels-per-unit-setter
    (value :: <png-uint-32>, ptr :: <png-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 116) := value;
  value;
end method png_info_struct$y-pixels-per-unit-setter;

define sealed method png_info_struct$phys-unit-type
    (ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 120);
end method png_info_struct$phys-unit-type;

define sealed method png_info_struct$phys-unit-type-setter
    (value :: <png-byte>, ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 120) := value;
  value;
end method png_info_struct$phys-unit-type-setter;

define sealed method png_info_struct$hist
    (ptr :: <png-info-struct>) => (result :: <png-uint-16p>);
  pointer-at(ptr, offset: 124, class: <png-uint-16p>);
end method png_info_struct$hist;

define sealed method png_info_struct$hist-setter
    (value :: <png-uint-16p>, ptr :: <png-info-struct>) => (result :: <png-uint-16p>);
  pointer-at(ptr, offset: 124, class: <png-uint-16p>) := value;
  value;
end method png_info_struct$hist-setter;

define sealed method png_info_struct$x-white
    (ptr :: <png-info-struct>) => (result :: <single-float>);
  float-at(ptr, offset: 128);
end method png_info_struct$x-white;

define sealed method png_info_struct$x-white-setter
    (value :: <single-float>, ptr :: <png-info-struct>) => (result :: <single-float>);
  float-at(ptr, offset: 128) := value;
  value;
end method png_info_struct$x-white-setter;

define sealed method png_info_struct$y-white
    (ptr :: <png-info-struct>) => (result :: <single-float>);
  float-at(ptr, offset: 132);
end method png_info_struct$y-white;

define sealed method png_info_struct$y-white-setter
    (value :: <single-float>, ptr :: <png-info-struct>) => (result :: <single-float>);
  float-at(ptr, offset: 132) := value;
  value;
end method png_info_struct$y-white-setter;

define sealed method png_info_struct$x-red
    (ptr :: <png-info-struct>) => (result :: <single-float>);
  float-at(ptr, offset: 136);
end method png_info_struct$x-red;

define sealed method png_info_struct$x-red-setter
    (value :: <single-float>, ptr :: <png-info-struct>) => (result :: <single-float>);
  float-at(ptr, offset: 136) := value;
  value;
end method png_info_struct$x-red-setter;

define sealed method png_info_struct$y-red
    (ptr :: <png-info-struct>) => (result :: <single-float>);
  float-at(ptr, offset: 140);
end method png_info_struct$y-red;

define sealed method png_info_struct$y-red-setter
    (value :: <single-float>, ptr :: <png-info-struct>) => (result :: <single-float>);
  float-at(ptr, offset: 140) := value;
  value;
end method png_info_struct$y-red-setter;

define sealed method png_info_struct$x-green
    (ptr :: <png-info-struct>) => (result :: <single-float>);
  float-at(ptr, offset: 144);
end method png_info_struct$x-green;

define sealed method png_info_struct$x-green-setter
    (value :: <single-float>, ptr :: <png-info-struct>) => (result :: <single-float>);
  float-at(ptr, offset: 144) := value;
  value;
end method png_info_struct$x-green-setter;

define sealed method png_info_struct$y-green
    (ptr :: <png-info-struct>) => (result :: <single-float>);
  float-at(ptr, offset: 148);
end method png_info_struct$y-green;

define sealed method png_info_struct$y-green-setter
    (value :: <single-float>, ptr :: <png-info-struct>) => (result :: <single-float>);
  float-at(ptr, offset: 148) := value;
  value;
end method png_info_struct$y-green-setter;

define sealed method png_info_struct$x-blue
    (ptr :: <png-info-struct>) => (result :: <single-float>);
  float-at(ptr, offset: 152);
end method png_info_struct$x-blue;

define sealed method png_info_struct$x-blue-setter
    (value :: <single-float>, ptr :: <png-info-struct>) => (result :: <single-float>);
  float-at(ptr, offset: 152) := value;
  value;
end method png_info_struct$x-blue-setter;

define sealed method png_info_struct$y-blue
    (ptr :: <png-info-struct>) => (result :: <single-float>);
  float-at(ptr, offset: 156);
end method png_info_struct$y-blue;

define sealed method png_info_struct$y-blue-setter
    (value :: <single-float>, ptr :: <png-info-struct>) => (result :: <single-float>);
  float-at(ptr, offset: 156) := value;
  value;
end method png_info_struct$y-blue-setter;

define sealed method png_info_struct$pcal-purpose
    (ptr :: <png-info-struct>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 160, class: <png-charp>);
end method png_info_struct$pcal-purpose;

define sealed method png_info_struct$pcal-purpose-setter
    (value :: <png-charp>, ptr :: <png-info-struct>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 160, class: <png-charp>) := value;
  value;
end method png_info_struct$pcal-purpose-setter;

define sealed method png_info_struct$pcal-X0
    (ptr :: <png-info-struct>) => (result :: <png-int-32>);
  signed-long-at(ptr, offset: 164);
end method png_info_struct$pcal-X0;

define sealed method png_info_struct$pcal-X0-setter
    (value :: <png-int-32>, ptr :: <png-info-struct>) => (result :: <png-int-32>);
  signed-long-at(ptr, offset: 164) := value;
  value;
end method png_info_struct$pcal-X0-setter;

define sealed method png_info_struct$pcal-X1
    (ptr :: <png-info-struct>) => (result :: <png-int-32>);
  signed-long-at(ptr, offset: 168);
end method png_info_struct$pcal-X1;

define sealed method png_info_struct$pcal-X1-setter
    (value :: <png-int-32>, ptr :: <png-info-struct>) => (result :: <png-int-32>);
  signed-long-at(ptr, offset: 168) := value;
  value;
end method png_info_struct$pcal-X1-setter;

define sealed method png_info_struct$pcal-units
    (ptr :: <png-info-struct>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 172, class: <png-charp>);
end method png_info_struct$pcal-units;

define sealed method png_info_struct$pcal-units-setter
    (value :: <png-charp>, ptr :: <png-info-struct>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 172, class: <png-charp>) := value;
  value;
end method png_info_struct$pcal-units-setter;

define sealed method png_info_struct$pcal-params
    (ptr :: <png-info-struct>) => (result :: <png-charpp>);
  pointer-at(ptr, offset: 176, class: <png-charpp>);
end method png_info_struct$pcal-params;

define sealed method png_info_struct$pcal-params-setter
    (value :: <png-charpp>, ptr :: <png-info-struct>) => (result :: <png-charpp>);
  pointer-at(ptr, offset: 176, class: <png-charpp>) := value;
  value;
end method png_info_struct$pcal-params-setter;

define sealed method png_info_struct$pcal-type
    (ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 180);
end method png_info_struct$pcal-type;

define sealed method png_info_struct$pcal-type-setter
    (value :: <png-byte>, ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 180) := value;
  value;
end method png_info_struct$pcal-type-setter;

define sealed method png_info_struct$pcal-nparams
    (ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 181);
end method png_info_struct$pcal-nparams;

define sealed method png_info_struct$pcal-nparams-setter
    (value :: <png-byte>, ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 181) := value;
  value;
end method png_info_struct$pcal-nparams-setter;

define sealed method png_info_struct$free-me
    (ptr :: <png-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 184);
end method png_info_struct$free-me;

define sealed method png_info_struct$free-me-setter
    (value :: <png-uint-32>, ptr :: <png-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 184) := value;
  value;
end method png_info_struct$free-me-setter;

define sealed method png_info_struct$unknown-chunks
    (ptr :: <png-info-struct>) => (result :: <png-unknown-chunkp>);
  pointer-at(ptr, offset: 188, class: <png-unknown-chunkp>);
end method png_info_struct$unknown-chunks;

define sealed method png_info_struct$unknown-chunks-setter
    (value :: <png-unknown-chunkp>, ptr :: <png-info-struct>) => (result :: <png-unknown-chunkp>);
  pointer-at(ptr, offset: 188, class: <png-unknown-chunkp>) := value;
  value;
end method png_info_struct$unknown-chunks-setter;

define sealed method png_info_struct$unknown-chunks-num
    (ptr :: <png-info-struct>) => (result :: <png-size-t>);
  unsigned-long-at(ptr, offset: 192);
end method png_info_struct$unknown-chunks-num;

define sealed method png_info_struct$unknown-chunks-num-setter
    (value :: <png-size-t>, ptr :: <png-info-struct>) => (result :: <png-size-t>);
  unsigned-long-at(ptr, offset: 192) := value;
  value;
end method png_info_struct$unknown-chunks-num-setter;

define sealed method png_info_struct$iccp-name
    (ptr :: <png-info-struct>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 196, class: <png-charp>);
end method png_info_struct$iccp-name;

define sealed method png_info_struct$iccp-name-setter
    (value :: <png-charp>, ptr :: <png-info-struct>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 196, class: <png-charp>) := value;
  value;
end method png_info_struct$iccp-name-setter;

define sealed method png_info_struct$iccp-profile
    (ptr :: <png-info-struct>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 200, class: <png-charp>);
end method png_info_struct$iccp-profile;

define sealed method png_info_struct$iccp-profile-setter
    (value :: <png-charp>, ptr :: <png-info-struct>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 200, class: <png-charp>) := value;
  value;
end method png_info_struct$iccp-profile-setter;

define sealed method png_info_struct$iccp-proflen
    (ptr :: <png-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 204);
end method png_info_struct$iccp-proflen;

define sealed method png_info_struct$iccp-proflen-setter
    (value :: <png-uint-32>, ptr :: <png-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 204) := value;
  value;
end method png_info_struct$iccp-proflen-setter;

define sealed method png_info_struct$iccp-compression
    (ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 208);
end method png_info_struct$iccp-compression;

define sealed method png_info_struct$iccp-compression-setter
    (value :: <png-byte>, ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 208) := value;
  value;
end method png_info_struct$iccp-compression-setter;

define sealed method png_info_struct$splt-palettes
    (ptr :: <png-info-struct>) => (result :: <png-sPLT-tp>);
  pointer-at(ptr, offset: 212, class: <png-sPLT-tp>);
end method png_info_struct$splt-palettes;

define sealed method png_info_struct$splt-palettes-setter
    (value :: <png-sPLT-tp>, ptr :: <png-info-struct>) => (result :: <png-sPLT-tp>);
  pointer-at(ptr, offset: 212, class: <png-sPLT-tp>) := value;
  value;
end method png_info_struct$splt-palettes-setter;

define sealed method png_info_struct$splt-palettes-num
    (ptr :: <png-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 216);
end method png_info_struct$splt-palettes-num;

define sealed method png_info_struct$splt-palettes-num-setter
    (value :: <png-uint-32>, ptr :: <png-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 216) := value;
  value;
end method png_info_struct$splt-palettes-num-setter;

define sealed method png_info_struct$scal-unit
    (ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 220);
end method png_info_struct$scal-unit;

define sealed method png_info_struct$scal-unit-setter
    (value :: <png-byte>, ptr :: <png-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 220) := value;
  value;
end method png_info_struct$scal-unit-setter;

define sealed method png_info_struct$scal-pixel-width
    (ptr :: <png-info-struct>) => (result :: <double-float>);
  double-at(ptr, offset: 224);
end method png_info_struct$scal-pixel-width;

define sealed method png_info_struct$scal-pixel-width-setter
    (value :: <double-float>, ptr :: <png-info-struct>) => (result :: <double-float>);
  double-at(ptr, offset: 224) := value;
  value;
end method png_info_struct$scal-pixel-width-setter;

define sealed method png_info_struct$scal-pixel-height
    (ptr :: <png-info-struct>) => (result :: <double-float>);
  double-at(ptr, offset: 232);
end method png_info_struct$scal-pixel-height;

define sealed method png_info_struct$scal-pixel-height-setter
    (value :: <double-float>, ptr :: <png-info-struct>) => (result :: <double-float>);
  double-at(ptr, offset: 232) := value;
  value;
end method png_info_struct$scal-pixel-height-setter;

define sealed method png_info_struct$scal-s-width
    (ptr :: <png-info-struct>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 240, class: <png-charp>);
end method png_info_struct$scal-s-width;

define sealed method png_info_struct$scal-s-width-setter
    (value :: <png-charp>, ptr :: <png-info-struct>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 240, class: <png-charp>) := value;
  value;
end method png_info_struct$scal-s-width-setter;

define sealed method png_info_struct$scal-s-height
    (ptr :: <png-info-struct>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 244, class: <png-charp>);
end method png_info_struct$scal-s-height;

define sealed method png_info_struct$scal-s-height-setter
    (value :: <png-charp>, ptr :: <png-info-struct>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 244, class: <png-charp>) := value;
  value;
end method png_info_struct$scal-s-height-setter;

define sealed method png_info_struct$row-pointers
    (ptr :: <png-info-struct>) => (result :: <png-bytepp>);
  pointer-at(ptr, offset: 248, class: <png-bytepp>);
end method png_info_struct$row-pointers;

define sealed method png_info_struct$row-pointers-setter
    (value :: <png-bytepp>, ptr :: <png-info-struct>) => (result :: <png-bytepp>);
  pointer-at(ptr, offset: 248, class: <png-bytepp>) := value;
  value;
end method png_info_struct$row-pointers-setter;

define sealed method png_info_struct$int-gamma
    (ptr :: <png-info-struct>) => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: 252);
end method png_info_struct$int-gamma;

define sealed method png_info_struct$int-gamma-setter
    (value :: <png-fixed-point>, ptr :: <png-info-struct>) => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: 252) := value;
  value;
end method png_info_struct$int-gamma-setter;

define sealed method png_info_struct$int-x-white
    (ptr :: <png-info-struct>) => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: 256);
end method png_info_struct$int-x-white;

define sealed method png_info_struct$int-x-white-setter
    (value :: <png-fixed-point>, ptr :: <png-info-struct>) => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: 256) := value;
  value;
end method png_info_struct$int-x-white-setter;

define sealed method png_info_struct$int-y-white
    (ptr :: <png-info-struct>) => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: 260);
end method png_info_struct$int-y-white;

define sealed method png_info_struct$int-y-white-setter
    (value :: <png-fixed-point>, ptr :: <png-info-struct>) => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: 260) := value;
  value;
end method png_info_struct$int-y-white-setter;

define sealed method png_info_struct$int-x-red
    (ptr :: <png-info-struct>) => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: 264);
end method png_info_struct$int-x-red;

define sealed method png_info_struct$int-x-red-setter
    (value :: <png-fixed-point>, ptr :: <png-info-struct>) => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: 264) := value;
  value;
end method png_info_struct$int-x-red-setter;

define sealed method png_info_struct$int-y-red
    (ptr :: <png-info-struct>) => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: 268);
end method png_info_struct$int-y-red;

define sealed method png_info_struct$int-y-red-setter
    (value :: <png-fixed-point>, ptr :: <png-info-struct>) => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: 268) := value;
  value;
end method png_info_struct$int-y-red-setter;

define sealed method png_info_struct$int-x-green
    (ptr :: <png-info-struct>) => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: 272);
end method png_info_struct$int-x-green;

define sealed method png_info_struct$int-x-green-setter
    (value :: <png-fixed-point>, ptr :: <png-info-struct>) => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: 272) := value;
  value;
end method png_info_struct$int-x-green-setter;

define sealed method png_info_struct$int-y-green
    (ptr :: <png-info-struct>) => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: 276);
end method png_info_struct$int-y-green;

define sealed method png_info_struct$int-y-green-setter
    (value :: <png-fixed-point>, ptr :: <png-info-struct>) => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: 276) := value;
  value;
end method png_info_struct$int-y-green-setter;

define sealed method png_info_struct$int-x-blue
    (ptr :: <png-info-struct>) => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: 280);
end method png_info_struct$int-x-blue;

define sealed method png_info_struct$int-x-blue-setter
    (value :: <png-fixed-point>, ptr :: <png-info-struct>) => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: 280) := value;
  value;
end method png_info_struct$int-x-blue-setter;

define sealed method png_info_struct$int-y-blue
    (ptr :: <png-info-struct>) => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: 284);
end method png_info_struct$int-y-blue;

define sealed method png_info_struct$int-y-blue-setter
    (value :: <png-fixed-point>, ptr :: <png-info-struct>) => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: 284) := value;
  value;
end method png_info_struct$int-y-blue-setter;

define method pointer-value (value :: <png-info-struct>, #key index = 0) => (result :: <png-info-struct>);
  value + index * 288;
end method pointer-value;

define method content-size (value :: subclass(<png-info-struct>)) => (result :: <integer>);
  288;
end method content-size;

define constant <png-info> = <png-info-struct>;

define constant <png-infop> = <png-info>;

define functional class <anonymous-308> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-308>));

define method pointer-value
    (ptr :: <anonymous-308>, #key index = 0)
 => (result :: <png-info>);
  pointer-at(ptr, offset: index * 4, class: <png-info>);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-info>, ptr :: <anonymous-308>, #key index = 0)
 => (result :: <png-info>);
  pointer-at(ptr, offset: index * 4, class: <png-info>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-308>)) => (result :: <integer>);
  4;
end method content-size;

define constant <png-infopp> = <anonymous-308>;

define functional class <png-row-info-struct> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<png-row-info-struct>));

define sealed method png_row_info_struct$width
    (ptr :: <png-row-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 0);
end method png_row_info_struct$width;

define sealed method png_row_info_struct$width-setter
    (value :: <png-uint-32>, ptr :: <png-row-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 0) := value;
  value;
end method png_row_info_struct$width-setter;

define sealed method png_row_info_struct$rowbytes
    (ptr :: <png-row-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 4);
end method png_row_info_struct$rowbytes;

define sealed method png_row_info_struct$rowbytes-setter
    (value :: <png-uint-32>, ptr :: <png-row-info-struct>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 4) := value;
  value;
end method png_row_info_struct$rowbytes-setter;

define sealed method png_row_info_struct$color-type
    (ptr :: <png-row-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 8);
end method png_row_info_struct$color-type;

define sealed method png_row_info_struct$color-type-setter
    (value :: <png-byte>, ptr :: <png-row-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 8) := value;
  value;
end method png_row_info_struct$color-type-setter;

define sealed method png_row_info_struct$bit-depth
    (ptr :: <png-row-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 9);
end method png_row_info_struct$bit-depth;

define sealed method png_row_info_struct$bit-depth-setter
    (value :: <png-byte>, ptr :: <png-row-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 9) := value;
  value;
end method png_row_info_struct$bit-depth-setter;

define sealed method png_row_info_struct$channels
    (ptr :: <png-row-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 10);
end method png_row_info_struct$channels;

define sealed method png_row_info_struct$channels-setter
    (value :: <png-byte>, ptr :: <png-row-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 10) := value;
  value;
end method png_row_info_struct$channels-setter;

define sealed method png_row_info_struct$pixel-depth
    (ptr :: <png-row-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 11);
end method png_row_info_struct$pixel-depth;

define sealed method png_row_info_struct$pixel-depth-setter
    (value :: <png-byte>, ptr :: <png-row-info-struct>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 11) := value;
  value;
end method png_row_info_struct$pixel-depth-setter;

define method pointer-value (value :: <png-row-info-struct>, #key index = 0) => (result :: <png-row-info-struct>);
  value + index * 12;
end method pointer-value;

define method content-size (value :: subclass(<png-row-info-struct>)) => (result :: <integer>);
  12;
end method content-size;

define constant <png-row-info> = <png-row-info-struct>;

define constant <png-row-infop> = <png-row-info>;

define functional class <anonymous-309> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-309>));

define method pointer-value
    (ptr :: <anonymous-309>, #key index = 0)
 => (result :: <png-row-info>);
  pointer-at(ptr, offset: index * 4, class: <png-row-info>);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-row-info>, ptr :: <anonymous-309>, #key index = 0)
 => (result :: <png-row-info>);
  pointer-at(ptr, offset: index * 4, class: <png-row-info>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-309>)) => (result :: <integer>);
  4;
end method content-size;

define constant <png-row-infopp> = <anonymous-309>;

define functional class <anonymous-193> (<anonymous-36>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-193>));

define method content-size (value == <anonymous-193>)  => (result :: <integer>);
  6;
end method content-size;

define constant <__jmp-buf> = <anonymous-193>;

define functional class <anonymous-183> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-183>));

define method pointer-value
    (ptr :: <anonymous-183>, #key index = 0)
 => (result :: <integer>);
  unsigned-long-at(ptr, offset: index * 4);
end method pointer-value;

define method pointer-value-setter
    (value :: <integer>, ptr :: <anonymous-183>, #key index = 0)
 => (result :: <integer>);
  unsigned-long-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-183>)) => (result :: <integer>);
  4;
end method content-size;

define functional class <anonymous-182> (<anonymous-183>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-182>));

define method content-size (value == <anonymous-182>)  => (result :: <integer>);
  32;
end method content-size;

define functional class <__sigset-t> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<__sigset-t>));

define sealed method __sigset_t$__val
    (ptr :: <__sigset-t>) => (result :: <anonymous-182>);
  as(<anonymous-182>, ptr + 0);
end method __sigset_t$__val;

define method pointer-value (value :: <__sigset-t>, #key index = 0) => (result :: <__sigset-t>);
  value + index * 128;
end method pointer-value;

define method content-size (value :: subclass(<__sigset-t>)) => (result :: <integer>);
  128;
end method content-size;

define functional class <__jmp-buf-tag> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<__jmp-buf-tag>));

define sealed method __jmp_buf_tag$__jmpbuf
    (ptr :: <__jmp-buf-tag>) => (result :: <__jmp-buf>);
  as(<__jmp-buf>, ptr + 0);
end method __jmp_buf_tag$__jmpbuf;

define sealed method __jmp_buf_tag$__mask-was-saved
    (ptr :: <__jmp-buf-tag>) => (result :: <integer>);
  signed-long-at(ptr, offset: 24);
end method __jmp_buf_tag$__mask-was-saved;

define sealed method __jmp_buf_tag$__mask-was-saved-setter
    (value :: <integer>, ptr :: <__jmp-buf-tag>) => (result :: <integer>);
  signed-long-at(ptr, offset: 24) := value;
  value;
end method __jmp_buf_tag$__mask-was-saved-setter;

define sealed method __jmp_buf_tag$__saved-mask
    (ptr :: <__jmp-buf-tag>) => (result :: <__sigset-t>);
  as(<__sigset-t>, ptr + 28);
end method __jmp_buf_tag$__saved-mask;

define method pointer-value (value :: <__jmp-buf-tag>, #key index = 0) => (result :: <__jmp-buf-tag>);
  value + index * 156;
end method pointer-value;

define method content-size (value :: subclass(<__jmp-buf-tag>)) => (result :: <integer>);
  156;
end method content-size;

define functional class <anonymous-194> (<__jmp-buf-tag>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-194>));

define method content-size (value == <anonymous-194>)  => (result :: <integer>);
  1;
end method content-size;

define constant <jmp-buf> = <anonymous-194>;

define constant <png-struct> = <png-struct-def>;

define constant <png-structp> = <png-struct>;

define constant <png-const-charp> = <anonymous-3>;

define functional class <anonymous-310> (<function-pointer>) end;

define constant <png-error-ptr> = <anonymous-310>;

define constant <png-voidp> = <machine-pointer>;

define functional class <anonymous-311> (<function-pointer>) end;

define constant <png-rw-ptr> = <anonymous-311>;

define functional class <anonymous-318> (<function-pointer>) end;

define constant <png-user-transform-ptr> = <anonymous-318>;

define constant <Byte> = <integer>;

define constant <Bytef> = <Byte>;

define functional class <anonymous-2> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-2>));

define method pointer-value
    (ptr :: <anonymous-2>, #key index = 0)
 => (result :: <Bytef>);
  unsigned-byte-at(ptr, offset: index * 1);
end method pointer-value;

define method pointer-value-setter
    (value :: <Bytef>, ptr :: <anonymous-2>, #key index = 0)
 => (result :: <Bytef>);
  unsigned-byte-at(ptr, offset: index * 1) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-2>)) => (result :: <integer>);
  1;
end method content-size;

define constant <uInt> = <integer>;

define constant <uLong> = <integer>;

define functional class <internal-state> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<internal-state>));

define sealed method internal_state$dummy
    (ptr :: <internal-state>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0);
end method internal_state$dummy;

define sealed method internal_state$dummy-setter
    (value :: <integer>, ptr :: <internal-state>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0) := value;
  value;
end method internal_state$dummy-setter;

define method pointer-value (value :: <internal-state>, #key index = 0) => (result :: <internal-state>);
  value + index * 4;
end method pointer-value;

define method content-size (value :: subclass(<internal-state>)) => (result :: <integer>);
  4;
end method content-size;

define constant <voidpf> = <machine-pointer>;

define functional class <anonymous-0> (<function-pointer>) end;

define constant <alloc-func> = <anonymous-0>;

define functional class <anonymous-1> (<function-pointer>) end;

define constant <free-func> = <anonymous-1>;

define functional class <z-stream-s> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<z-stream-s>));

define sealed method z_stream_s$next-in
    (ptr :: <z-stream-s>) => (result :: <anonymous-2>);
  pointer-at(ptr, offset: 0, class: <anonymous-2>);
end method z_stream_s$next-in;

define sealed method z_stream_s$next-in-setter
    (value :: <anonymous-2>, ptr :: <z-stream-s>) => (result :: <anonymous-2>);
  pointer-at(ptr, offset: 0, class: <anonymous-2>) := value;
  value;
end method z_stream_s$next-in-setter;

define sealed method z_stream_s$avail-in
    (ptr :: <z-stream-s>) => (result :: <uInt>);
  unsigned-long-at(ptr, offset: 4);
end method z_stream_s$avail-in;

define sealed method z_stream_s$avail-in-setter
    (value :: <uInt>, ptr :: <z-stream-s>) => (result :: <uInt>);
  unsigned-long-at(ptr, offset: 4) := value;
  value;
end method z_stream_s$avail-in-setter;

define sealed method z_stream_s$total-in
    (ptr :: <z-stream-s>) => (result :: <uLong>);
  unsigned-long-at(ptr, offset: 8);
end method z_stream_s$total-in;

define sealed method z_stream_s$total-in-setter
    (value :: <uLong>, ptr :: <z-stream-s>) => (result :: <uLong>);
  unsigned-long-at(ptr, offset: 8) := value;
  value;
end method z_stream_s$total-in-setter;

define sealed method z_stream_s$next-out
    (ptr :: <z-stream-s>) => (result :: <anonymous-2>);
  pointer-at(ptr, offset: 12, class: <anonymous-2>);
end method z_stream_s$next-out;

define sealed method z_stream_s$next-out-setter
    (value :: <anonymous-2>, ptr :: <z-stream-s>) => (result :: <anonymous-2>);
  pointer-at(ptr, offset: 12, class: <anonymous-2>) := value;
  value;
end method z_stream_s$next-out-setter;

define sealed method z_stream_s$avail-out
    (ptr :: <z-stream-s>) => (result :: <uInt>);
  unsigned-long-at(ptr, offset: 16);
end method z_stream_s$avail-out;

define sealed method z_stream_s$avail-out-setter
    (value :: <uInt>, ptr :: <z-stream-s>) => (result :: <uInt>);
  unsigned-long-at(ptr, offset: 16) := value;
  value;
end method z_stream_s$avail-out-setter;

define sealed method z_stream_s$total-out
    (ptr :: <z-stream-s>) => (result :: <uLong>);
  unsigned-long-at(ptr, offset: 20);
end method z_stream_s$total-out;

define sealed method z_stream_s$total-out-setter
    (value :: <uLong>, ptr :: <z-stream-s>) => (result :: <uLong>);
  unsigned-long-at(ptr, offset: 20) := value;
  value;
end method z_stream_s$total-out-setter;

define sealed method z_stream_s$msg
    (ptr :: <z-stream-s>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 24, class: <anonymous-3>);
end method z_stream_s$msg;

define sealed method z_stream_s$msg-setter
    (value :: <anonymous-3>, ptr :: <z-stream-s>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 24, class: <anonymous-3>) := value;
  value;
end method z_stream_s$msg-setter;

define sealed method z_stream_s$state
    (ptr :: <z-stream-s>) => (result :: <internal-state>);
  pointer-at(ptr, offset: 28, class: <internal-state>);
end method z_stream_s$state;

define sealed method z_stream_s$state-setter
    (value :: <internal-state>, ptr :: <z-stream-s>) => (result :: <internal-state>);
  pointer-at(ptr, offset: 28, class: <internal-state>) := value;
  value;
end method z_stream_s$state-setter;

define sealed method z_stream_s$zalloc
    (ptr :: <z-stream-s>) => (result :: <alloc-func>);
  pointer-at(ptr, offset: 32, class: <alloc-func>);
end method z_stream_s$zalloc;

define sealed method z_stream_s$zalloc-setter
    (value :: <alloc-func>, ptr :: <z-stream-s>) => (result :: <alloc-func>);
  pointer-at(ptr, offset: 32, class: <alloc-func>) := value;
  value;
end method z_stream_s$zalloc-setter;

define sealed method z_stream_s$zfree
    (ptr :: <z-stream-s>) => (result :: <free-func>);
  pointer-at(ptr, offset: 36, class: <free-func>);
end method z_stream_s$zfree;

define sealed method z_stream_s$zfree-setter
    (value :: <free-func>, ptr :: <z-stream-s>) => (result :: <free-func>);
  pointer-at(ptr, offset: 36, class: <free-func>) := value;
  value;
end method z_stream_s$zfree-setter;

define sealed method z_stream_s$opaque
    (ptr :: <z-stream-s>) => (result :: <voidpf>);
  pointer-at(ptr, offset: 40, class: <voidpf>);
end method z_stream_s$opaque;

define sealed method z_stream_s$opaque-setter
    (value :: <voidpf>, ptr :: <z-stream-s>) => (result :: <voidpf>);
  pointer-at(ptr, offset: 40, class: <voidpf>) := value;
  value;
end method z_stream_s$opaque-setter;

define sealed method z_stream_s$data-type
    (ptr :: <z-stream-s>) => (result :: <integer>);
  signed-long-at(ptr, offset: 44);
end method z_stream_s$data-type;

define sealed method z_stream_s$data-type-setter
    (value :: <integer>, ptr :: <z-stream-s>) => (result :: <integer>);
  signed-long-at(ptr, offset: 44) := value;
  value;
end method z_stream_s$data-type-setter;

define sealed method z_stream_s$adler
    (ptr :: <z-stream-s>) => (result :: <uLong>);
  unsigned-long-at(ptr, offset: 48);
end method z_stream_s$adler;

define sealed method z_stream_s$adler-setter
    (value :: <uLong>, ptr :: <z-stream-s>) => (result :: <uLong>);
  unsigned-long-at(ptr, offset: 48) := value;
  value;
end method z_stream_s$adler-setter;

define sealed method z_stream_s$reserved
    (ptr :: <z-stream-s>) => (result :: <uLong>);
  unsigned-long-at(ptr, offset: 52);
end method z_stream_s$reserved;

define sealed method z_stream_s$reserved-setter
    (value :: <uLong>, ptr :: <z-stream-s>) => (result :: <uLong>);
  unsigned-long-at(ptr, offset: 52) := value;
  value;
end method z_stream_s$reserved-setter;

define method pointer-value (value :: <z-stream-s>, #key index = 0) => (result :: <z-stream-s>);
  value + index * 56;
end method pointer-value;

define method content-size (value :: subclass(<z-stream-s>)) => (result :: <integer>);
  56;
end method content-size;

define constant <z-stream> = <z-stream-s>;

define functional class <anonymous-323> (<anonymous-275>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-323>));

define method content-size (value == <anonymous-323>)  => (result :: <integer>);
  5;
end method content-size;

define functional class <anonymous-312> (<function-pointer>) end;

define constant <png-flush-ptr> = <anonymous-312>;

define functional class <anonymous-285> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-285>));

define method pointer-value
    (ptr :: <anonymous-285>, #key index = 0)
 => (result :: <anonymous-278>);
  pointer-at(ptr, offset: index * 4, class: <anonymous-278>);
end method pointer-value;

define method pointer-value-setter
    (value :: <anonymous-278>, ptr :: <anonymous-285>, #key index = 0)
 => (result :: <anonymous-278>);
  pointer-at(ptr, offset: index * 4, class: <anonymous-278>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-285>)) => (result :: <integer>);
  4;
end method content-size;

define constant <png-uint-16pp> = <anonymous-285>;

define functional class <anonymous-313> (<function-pointer>) end;

define constant <png-read-status-ptr> = <anonymous-313>;

define functional class <anonymous-314> (<function-pointer>) end;

define constant <png-write-status-ptr> = <anonymous-314>;

define functional class <anonymous-315> (<function-pointer>) end;

define constant <png-progressive-info-ptr> = <anonymous-315>;

define functional class <anonymous-317> (<function-pointer>) end;

define constant <png-progressive-row-ptr> = <anonymous-317>;

define functional class <anonymous-316> (<function-pointer>) end;

define constant <png-progressive-end-ptr> = <anonymous-316>;

define functional class <anonymous-319> (<function-pointer>) end;

define constant <png-user-chunk-ptr> = <anonymous-319>;

define functional class <png-struct-def> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<png-struct-def>));

define sealed method png_struct_def$jmpbuf
    (ptr :: <png-struct-def>) => (result :: <jmp-buf>);
  as(<jmp-buf>, ptr + 0);
end method png_struct_def$jmpbuf;

define sealed method png_struct_def$error-fn
    (ptr :: <png-struct-def>) => (result :: <png-error-ptr>);
  pointer-at(ptr, offset: 156, class: <png-error-ptr>);
end method png_struct_def$error-fn;

define sealed method png_struct_def$error-fn-setter
    (value :: <png-error-ptr>, ptr :: <png-struct-def>) => (result :: <png-error-ptr>);
  pointer-at(ptr, offset: 156, class: <png-error-ptr>) := value;
  value;
end method png_struct_def$error-fn-setter;

define sealed method png_struct_def$warning-fn
    (ptr :: <png-struct-def>) => (result :: <png-error-ptr>);
  pointer-at(ptr, offset: 160, class: <png-error-ptr>);
end method png_struct_def$warning-fn;

define sealed method png_struct_def$warning-fn-setter
    (value :: <png-error-ptr>, ptr :: <png-struct-def>) => (result :: <png-error-ptr>);
  pointer-at(ptr, offset: 160, class: <png-error-ptr>) := value;
  value;
end method png_struct_def$warning-fn-setter;

define sealed method png_struct_def$error-ptr
    (ptr :: <png-struct-def>) => (result :: <png-voidp>);
  pointer-at(ptr, offset: 164, class: <png-voidp>);
end method png_struct_def$error-ptr;

define sealed method png_struct_def$error-ptr-setter
    (value :: <png-voidp>, ptr :: <png-struct-def>) => (result :: <png-voidp>);
  pointer-at(ptr, offset: 164, class: <png-voidp>) := value;
  value;
end method png_struct_def$error-ptr-setter;

define sealed method png_struct_def$write-data-fn
    (ptr :: <png-struct-def>) => (result :: <png-rw-ptr>);
  pointer-at(ptr, offset: 168, class: <png-rw-ptr>);
end method png_struct_def$write-data-fn;

define sealed method png_struct_def$write-data-fn-setter
    (value :: <png-rw-ptr>, ptr :: <png-struct-def>) => (result :: <png-rw-ptr>);
  pointer-at(ptr, offset: 168, class: <png-rw-ptr>) := value;
  value;
end method png_struct_def$write-data-fn-setter;

define sealed method png_struct_def$read-data-fn
    (ptr :: <png-struct-def>) => (result :: <png-rw-ptr>);
  pointer-at(ptr, offset: 172, class: <png-rw-ptr>);
end method png_struct_def$read-data-fn;

define sealed method png_struct_def$read-data-fn-setter
    (value :: <png-rw-ptr>, ptr :: <png-struct-def>) => (result :: <png-rw-ptr>);
  pointer-at(ptr, offset: 172, class: <png-rw-ptr>) := value;
  value;
end method png_struct_def$read-data-fn-setter;

define sealed method png_struct_def$io-ptr
    (ptr :: <png-struct-def>) => (result :: <png-voidp>);
  pointer-at(ptr, offset: 176, class: <png-voidp>);
end method png_struct_def$io-ptr;

define sealed method png_struct_def$io-ptr-setter
    (value :: <png-voidp>, ptr :: <png-struct-def>) => (result :: <png-voidp>);
  pointer-at(ptr, offset: 176, class: <png-voidp>) := value;
  value;
end method png_struct_def$io-ptr-setter;

define sealed method png_struct_def$read-user-transform-fn
    (ptr :: <png-struct-def>) => (result :: <png-user-transform-ptr>);
  pointer-at(ptr, offset: 180, class: <png-user-transform-ptr>);
end method png_struct_def$read-user-transform-fn;

define sealed method png_struct_def$read-user-transform-fn-setter
    (value :: <png-user-transform-ptr>, ptr :: <png-struct-def>) => (result :: <png-user-transform-ptr>);
  pointer-at(ptr, offset: 180, class: <png-user-transform-ptr>) := value;
  value;
end method png_struct_def$read-user-transform-fn-setter;

define sealed method png_struct_def$write-user-transform-fn
    (ptr :: <png-struct-def>) => (result :: <png-user-transform-ptr>);
  pointer-at(ptr, offset: 184, class: <png-user-transform-ptr>);
end method png_struct_def$write-user-transform-fn;

define sealed method png_struct_def$write-user-transform-fn-setter
    (value :: <png-user-transform-ptr>, ptr :: <png-struct-def>) => (result :: <png-user-transform-ptr>);
  pointer-at(ptr, offset: 184, class: <png-user-transform-ptr>) := value;
  value;
end method png_struct_def$write-user-transform-fn-setter;

define sealed method png_struct_def$user-transform-ptr
    (ptr :: <png-struct-def>) => (result :: <png-voidp>);
  pointer-at(ptr, offset: 188, class: <png-voidp>);
end method png_struct_def$user-transform-ptr;

define sealed method png_struct_def$user-transform-ptr-setter
    (value :: <png-voidp>, ptr :: <png-struct-def>) => (result :: <png-voidp>);
  pointer-at(ptr, offset: 188, class: <png-voidp>) := value;
  value;
end method png_struct_def$user-transform-ptr-setter;

define sealed method png_struct_def$user-transform-depth
    (ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 192);
end method png_struct_def$user-transform-depth;

define sealed method png_struct_def$user-transform-depth-setter
    (value :: <png-byte>, ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 192) := value;
  value;
end method png_struct_def$user-transform-depth-setter;

define sealed method png_struct_def$user-transform-channels
    (ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 193);
end method png_struct_def$user-transform-channels;

define sealed method png_struct_def$user-transform-channels-setter
    (value :: <png-byte>, ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 193) := value;
  value;
end method png_struct_def$user-transform-channels-setter;

define sealed method png_struct_def$mode
    (ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 196);
end method png_struct_def$mode;

define sealed method png_struct_def$mode-setter
    (value :: <png-uint-32>, ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 196) := value;
  value;
end method png_struct_def$mode-setter;

define sealed method png_struct_def$flags
    (ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 200);
end method png_struct_def$flags;

define sealed method png_struct_def$flags-setter
    (value :: <png-uint-32>, ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 200) := value;
  value;
end method png_struct_def$flags-setter;

define sealed method png_struct_def$transformations
    (ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 204);
end method png_struct_def$transformations;

define sealed method png_struct_def$transformations-setter
    (value :: <png-uint-32>, ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 204) := value;
  value;
end method png_struct_def$transformations-setter;

define sealed method png_struct_def$zstream
    (ptr :: <png-struct-def>) => (result :: <z-stream>);
  as(<z-stream>, ptr + 208);
end method png_struct_def$zstream;

define sealed method png_struct_def$zbuf
    (ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 264, class: <png-bytep>);
end method png_struct_def$zbuf;

define sealed method png_struct_def$zbuf-setter
    (value :: <png-bytep>, ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 264, class: <png-bytep>) := value;
  value;
end method png_struct_def$zbuf-setter;

define sealed method png_struct_def$zbuf-size
    (ptr :: <png-struct-def>) => (result :: <png-size-t>);
  unsigned-long-at(ptr, offset: 268);
end method png_struct_def$zbuf-size;

define sealed method png_struct_def$zbuf-size-setter
    (value :: <png-size-t>, ptr :: <png-struct-def>) => (result :: <png-size-t>);
  unsigned-long-at(ptr, offset: 268) := value;
  value;
end method png_struct_def$zbuf-size-setter;

define sealed method png_struct_def$zlib-level
    (ptr :: <png-struct-def>) => (result :: <integer>);
  signed-long-at(ptr, offset: 272);
end method png_struct_def$zlib-level;

define sealed method png_struct_def$zlib-level-setter
    (value :: <integer>, ptr :: <png-struct-def>) => (result :: <integer>);
  signed-long-at(ptr, offset: 272) := value;
  value;
end method png_struct_def$zlib-level-setter;

define sealed method png_struct_def$zlib-method
    (ptr :: <png-struct-def>) => (result :: <integer>);
  signed-long-at(ptr, offset: 276);
end method png_struct_def$zlib-method;

define sealed method png_struct_def$zlib-method-setter
    (value :: <integer>, ptr :: <png-struct-def>) => (result :: <integer>);
  signed-long-at(ptr, offset: 276) := value;
  value;
end method png_struct_def$zlib-method-setter;

define sealed method png_struct_def$zlib-window-bits
    (ptr :: <png-struct-def>) => (result :: <integer>);
  signed-long-at(ptr, offset: 280);
end method png_struct_def$zlib-window-bits;

define sealed method png_struct_def$zlib-window-bits-setter
    (value :: <integer>, ptr :: <png-struct-def>) => (result :: <integer>);
  signed-long-at(ptr, offset: 280) := value;
  value;
end method png_struct_def$zlib-window-bits-setter;

define sealed method png_struct_def$zlib-mem-level
    (ptr :: <png-struct-def>) => (result :: <integer>);
  signed-long-at(ptr, offset: 284);
end method png_struct_def$zlib-mem-level;

define sealed method png_struct_def$zlib-mem-level-setter
    (value :: <integer>, ptr :: <png-struct-def>) => (result :: <integer>);
  signed-long-at(ptr, offset: 284) := value;
  value;
end method png_struct_def$zlib-mem-level-setter;

define sealed method png_struct_def$zlib-strategy
    (ptr :: <png-struct-def>) => (result :: <integer>);
  signed-long-at(ptr, offset: 288);
end method png_struct_def$zlib-strategy;

define sealed method png_struct_def$zlib-strategy-setter
    (value :: <integer>, ptr :: <png-struct-def>) => (result :: <integer>);
  signed-long-at(ptr, offset: 288) := value;
  value;
end method png_struct_def$zlib-strategy-setter;

define sealed method png_struct_def$width
    (ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 292);
end method png_struct_def$width;

define sealed method png_struct_def$width-setter
    (value :: <png-uint-32>, ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 292) := value;
  value;
end method png_struct_def$width-setter;

define sealed method png_struct_def$height
    (ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 296);
end method png_struct_def$height;

define sealed method png_struct_def$height-setter
    (value :: <png-uint-32>, ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 296) := value;
  value;
end method png_struct_def$height-setter;

define sealed method png_struct_def$num-rows
    (ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 300);
end method png_struct_def$num-rows;

define sealed method png_struct_def$num-rows-setter
    (value :: <png-uint-32>, ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 300) := value;
  value;
end method png_struct_def$num-rows-setter;

define sealed method png_struct_def$usr-width
    (ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 304);
end method png_struct_def$usr-width;

define sealed method png_struct_def$usr-width-setter
    (value :: <png-uint-32>, ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 304) := value;
  value;
end method png_struct_def$usr-width-setter;

define sealed method png_struct_def$rowbytes
    (ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 308);
end method png_struct_def$rowbytes;

define sealed method png_struct_def$rowbytes-setter
    (value :: <png-uint-32>, ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 308) := value;
  value;
end method png_struct_def$rowbytes-setter;

define sealed method png_struct_def$irowbytes
    (ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 312);
end method png_struct_def$irowbytes;

define sealed method png_struct_def$irowbytes-setter
    (value :: <png-uint-32>, ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 312) := value;
  value;
end method png_struct_def$irowbytes-setter;

define sealed method png_struct_def$iwidth
    (ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 316);
end method png_struct_def$iwidth;

define sealed method png_struct_def$iwidth-setter
    (value :: <png-uint-32>, ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 316) := value;
  value;
end method png_struct_def$iwidth-setter;

define sealed method png_struct_def$row-number
    (ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 320);
end method png_struct_def$row-number;

define sealed method png_struct_def$row-number-setter
    (value :: <png-uint-32>, ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 320) := value;
  value;
end method png_struct_def$row-number-setter;

define sealed method png_struct_def$prev-row
    (ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 324, class: <png-bytep>);
end method png_struct_def$prev-row;

define sealed method png_struct_def$prev-row-setter
    (value :: <png-bytep>, ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 324, class: <png-bytep>) := value;
  value;
end method png_struct_def$prev-row-setter;

define sealed method png_struct_def$row-buf
    (ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 328, class: <png-bytep>);
end method png_struct_def$row-buf;

define sealed method png_struct_def$row-buf-setter
    (value :: <png-bytep>, ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 328, class: <png-bytep>) := value;
  value;
end method png_struct_def$row-buf-setter;

define sealed method png_struct_def$sub-row
    (ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 332, class: <png-bytep>);
end method png_struct_def$sub-row;

define sealed method png_struct_def$sub-row-setter
    (value :: <png-bytep>, ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 332, class: <png-bytep>) := value;
  value;
end method png_struct_def$sub-row-setter;

define sealed method png_struct_def$up-row
    (ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 336, class: <png-bytep>);
end method png_struct_def$up-row;

define sealed method png_struct_def$up-row-setter
    (value :: <png-bytep>, ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 336, class: <png-bytep>) := value;
  value;
end method png_struct_def$up-row-setter;

define sealed method png_struct_def$avg-row
    (ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 340, class: <png-bytep>);
end method png_struct_def$avg-row;

define sealed method png_struct_def$avg-row-setter
    (value :: <png-bytep>, ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 340, class: <png-bytep>) := value;
  value;
end method png_struct_def$avg-row-setter;

define sealed method png_struct_def$paeth-row
    (ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 344, class: <png-bytep>);
end method png_struct_def$paeth-row;

define sealed method png_struct_def$paeth-row-setter
    (value :: <png-bytep>, ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 344, class: <png-bytep>) := value;
  value;
end method png_struct_def$paeth-row-setter;

define sealed method png_struct_def$row-info
    (ptr :: <png-struct-def>) => (result :: <png-row-info>);
  as(<png-row-info>, ptr + 348);
end method png_struct_def$row-info;

define sealed method png_struct_def$idat-size
    (ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 360);
end method png_struct_def$idat-size;

define sealed method png_struct_def$idat-size-setter
    (value :: <png-uint-32>, ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 360) := value;
  value;
end method png_struct_def$idat-size-setter;

define sealed method png_struct_def$crc
    (ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 364);
end method png_struct_def$crc;

define sealed method png_struct_def$crc-setter
    (value :: <png-uint-32>, ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 364) := value;
  value;
end method png_struct_def$crc-setter;

define sealed method png_struct_def$palette
    (ptr :: <png-struct-def>) => (result :: <png-colorp>);
  pointer-at(ptr, offset: 368, class: <png-colorp>);
end method png_struct_def$palette;

define sealed method png_struct_def$palette-setter
    (value :: <png-colorp>, ptr :: <png-struct-def>) => (result :: <png-colorp>);
  pointer-at(ptr, offset: 368, class: <png-colorp>) := value;
  value;
end method png_struct_def$palette-setter;

define sealed method png_struct_def$num-palette
    (ptr :: <png-struct-def>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 372);
end method png_struct_def$num-palette;

define sealed method png_struct_def$num-palette-setter
    (value :: <png-uint-16>, ptr :: <png-struct-def>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 372) := value;
  value;
end method png_struct_def$num-palette-setter;

define sealed method png_struct_def$num-trans
    (ptr :: <png-struct-def>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 374);
end method png_struct_def$num-trans;

define sealed method png_struct_def$num-trans-setter
    (value :: <png-uint-16>, ptr :: <png-struct-def>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 374) := value;
  value;
end method png_struct_def$num-trans-setter;

define sealed method png_struct_def$chunk-name
    (ptr :: <png-struct-def>) => (result :: <anonymous-323>);
  as(<anonymous-323>, ptr + 376);
end method png_struct_def$chunk-name;

define sealed method png_struct_def$compression
    (ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 381);
end method png_struct_def$compression;

define sealed method png_struct_def$compression-setter
    (value :: <png-byte>, ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 381) := value;
  value;
end method png_struct_def$compression-setter;

define sealed method png_struct_def$filter
    (ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 382);
end method png_struct_def$filter;

define sealed method png_struct_def$filter-setter
    (value :: <png-byte>, ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 382) := value;
  value;
end method png_struct_def$filter-setter;

define sealed method png_struct_def$interlaced
    (ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 383);
end method png_struct_def$interlaced;

define sealed method png_struct_def$interlaced-setter
    (value :: <png-byte>, ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 383) := value;
  value;
end method png_struct_def$interlaced-setter;

define sealed method png_struct_def$pass
    (ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 384);
end method png_struct_def$pass;

define sealed method png_struct_def$pass-setter
    (value :: <png-byte>, ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 384) := value;
  value;
end method png_struct_def$pass-setter;

define sealed method png_struct_def$do-filter
    (ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 385);
end method png_struct_def$do-filter;

define sealed method png_struct_def$do-filter-setter
    (value :: <png-byte>, ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 385) := value;
  value;
end method png_struct_def$do-filter-setter;

define sealed method png_struct_def$color-type
    (ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 386);
end method png_struct_def$color-type;

define sealed method png_struct_def$color-type-setter
    (value :: <png-byte>, ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 386) := value;
  value;
end method png_struct_def$color-type-setter;

define sealed method png_struct_def$bit-depth
    (ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 387);
end method png_struct_def$bit-depth;

define sealed method png_struct_def$bit-depth-setter
    (value :: <png-byte>, ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 387) := value;
  value;
end method png_struct_def$bit-depth-setter;

define sealed method png_struct_def$usr-bit-depth
    (ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 388);
end method png_struct_def$usr-bit-depth;

define sealed method png_struct_def$usr-bit-depth-setter
    (value :: <png-byte>, ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 388) := value;
  value;
end method png_struct_def$usr-bit-depth-setter;

define sealed method png_struct_def$pixel-depth
    (ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 389);
end method png_struct_def$pixel-depth;

define sealed method png_struct_def$pixel-depth-setter
    (value :: <png-byte>, ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 389) := value;
  value;
end method png_struct_def$pixel-depth-setter;

define sealed method png_struct_def$channels
    (ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 390);
end method png_struct_def$channels;

define sealed method png_struct_def$channels-setter
    (value :: <png-byte>, ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 390) := value;
  value;
end method png_struct_def$channels-setter;

define sealed method png_struct_def$usr-channels
    (ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 391);
end method png_struct_def$usr-channels;

define sealed method png_struct_def$usr-channels-setter
    (value :: <png-byte>, ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 391) := value;
  value;
end method png_struct_def$usr-channels-setter;

define sealed method png_struct_def$sig-bytes
    (ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 392);
end method png_struct_def$sig-bytes;

define sealed method png_struct_def$sig-bytes-setter
    (value :: <png-byte>, ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 392) := value;
  value;
end method png_struct_def$sig-bytes-setter;

define sealed method png_struct_def$filler
    (ptr :: <png-struct-def>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 394);
end method png_struct_def$filler;

define sealed method png_struct_def$filler-setter
    (value :: <png-uint-16>, ptr :: <png-struct-def>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 394) := value;
  value;
end method png_struct_def$filler-setter;

define sealed method png_struct_def$background-gamma-type
    (ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 396);
end method png_struct_def$background-gamma-type;

define sealed method png_struct_def$background-gamma-type-setter
    (value :: <png-byte>, ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 396) := value;
  value;
end method png_struct_def$background-gamma-type-setter;

define sealed method png_struct_def$background-gamma
    (ptr :: <png-struct-def>) => (result :: <single-float>);
  float-at(ptr, offset: 400);
end method png_struct_def$background-gamma;

define sealed method png_struct_def$background-gamma-setter
    (value :: <single-float>, ptr :: <png-struct-def>) => (result :: <single-float>);
  float-at(ptr, offset: 400) := value;
  value;
end method png_struct_def$background-gamma-setter;

define sealed method png_struct_def$background
    (ptr :: <png-struct-def>) => (result :: <png-color-16>);
  as(<png-color-16>, ptr + 404);
end method png_struct_def$background;

define sealed method png_struct_def$background-1
    (ptr :: <png-struct-def>) => (result :: <png-color-16>);
  as(<png-color-16>, ptr + 414);
end method png_struct_def$background-1;

define sealed method png_struct_def$output-flush-fn
    (ptr :: <png-struct-def>) => (result :: <png-flush-ptr>);
  pointer-at(ptr, offset: 424, class: <png-flush-ptr>);
end method png_struct_def$output-flush-fn;

define sealed method png_struct_def$output-flush-fn-setter
    (value :: <png-flush-ptr>, ptr :: <png-struct-def>) => (result :: <png-flush-ptr>);
  pointer-at(ptr, offset: 424, class: <png-flush-ptr>) := value;
  value;
end method png_struct_def$output-flush-fn-setter;

define sealed method png_struct_def$flush-dist
    (ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 428);
end method png_struct_def$flush-dist;

define sealed method png_struct_def$flush-dist-setter
    (value :: <png-uint-32>, ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 428) := value;
  value;
end method png_struct_def$flush-dist-setter;

define sealed method png_struct_def$flush-rows
    (ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 432);
end method png_struct_def$flush-rows;

define sealed method png_struct_def$flush-rows-setter
    (value :: <png-uint-32>, ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 432) := value;
  value;
end method png_struct_def$flush-rows-setter;

define sealed method png_struct_def$gamma-shift
    (ptr :: <png-struct-def>) => (result :: <integer>);
  signed-long-at(ptr, offset: 436);
end method png_struct_def$gamma-shift;

define sealed method png_struct_def$gamma-shift-setter
    (value :: <integer>, ptr :: <png-struct-def>) => (result :: <integer>);
  signed-long-at(ptr, offset: 436) := value;
  value;
end method png_struct_def$gamma-shift-setter;

define sealed method png_struct_def$gamma
    (ptr :: <png-struct-def>) => (result :: <single-float>);
  float-at(ptr, offset: 440);
end method png_struct_def$gamma;

define sealed method png_struct_def$gamma-setter
    (value :: <single-float>, ptr :: <png-struct-def>) => (result :: <single-float>);
  float-at(ptr, offset: 440) := value;
  value;
end method png_struct_def$gamma-setter;

define sealed method png_struct_def$screen-gamma
    (ptr :: <png-struct-def>) => (result :: <single-float>);
  float-at(ptr, offset: 444);
end method png_struct_def$screen-gamma;

define sealed method png_struct_def$screen-gamma-setter
    (value :: <single-float>, ptr :: <png-struct-def>) => (result :: <single-float>);
  float-at(ptr, offset: 444) := value;
  value;
end method png_struct_def$screen-gamma-setter;

define sealed method png_struct_def$gamma-table
    (ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 448, class: <png-bytep>);
end method png_struct_def$gamma-table;

define sealed method png_struct_def$gamma-table-setter
    (value :: <png-bytep>, ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 448, class: <png-bytep>) := value;
  value;
end method png_struct_def$gamma-table-setter;

define sealed method png_struct_def$gamma-from-1
    (ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 452, class: <png-bytep>);
end method png_struct_def$gamma-from-1;

define sealed method png_struct_def$gamma-from-1-setter
    (value :: <png-bytep>, ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 452, class: <png-bytep>) := value;
  value;
end method png_struct_def$gamma-from-1-setter;

define sealed method png_struct_def$gamma-to-1
    (ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 456, class: <png-bytep>);
end method png_struct_def$gamma-to-1;

define sealed method png_struct_def$gamma-to-1-setter
    (value :: <png-bytep>, ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 456, class: <png-bytep>) := value;
  value;
end method png_struct_def$gamma-to-1-setter;

define sealed method png_struct_def$gamma-16-table
    (ptr :: <png-struct-def>) => (result :: <png-uint-16pp>);
  pointer-at(ptr, offset: 460, class: <png-uint-16pp>);
end method png_struct_def$gamma-16-table;

define sealed method png_struct_def$gamma-16-table-setter
    (value :: <png-uint-16pp>, ptr :: <png-struct-def>) => (result :: <png-uint-16pp>);
  pointer-at(ptr, offset: 460, class: <png-uint-16pp>) := value;
  value;
end method png_struct_def$gamma-16-table-setter;

define sealed method png_struct_def$gamma-16-from-1
    (ptr :: <png-struct-def>) => (result :: <png-uint-16pp>);
  pointer-at(ptr, offset: 464, class: <png-uint-16pp>);
end method png_struct_def$gamma-16-from-1;

define sealed method png_struct_def$gamma-16-from-1-setter
    (value :: <png-uint-16pp>, ptr :: <png-struct-def>) => (result :: <png-uint-16pp>);
  pointer-at(ptr, offset: 464, class: <png-uint-16pp>) := value;
  value;
end method png_struct_def$gamma-16-from-1-setter;

define sealed method png_struct_def$gamma-16-to-1
    (ptr :: <png-struct-def>) => (result :: <png-uint-16pp>);
  pointer-at(ptr, offset: 468, class: <png-uint-16pp>);
end method png_struct_def$gamma-16-to-1;

define sealed method png_struct_def$gamma-16-to-1-setter
    (value :: <png-uint-16pp>, ptr :: <png-struct-def>) => (result :: <png-uint-16pp>);
  pointer-at(ptr, offset: 468, class: <png-uint-16pp>) := value;
  value;
end method png_struct_def$gamma-16-to-1-setter;

define sealed method png_struct_def$sig-bit
    (ptr :: <png-struct-def>) => (result :: <png-color-8>);
  as(<png-color-8>, ptr + 472);
end method png_struct_def$sig-bit;

define sealed method png_struct_def$shift
    (ptr :: <png-struct-def>) => (result :: <png-color-8>);
  as(<png-color-8>, ptr + 477);
end method png_struct_def$shift;

define sealed method png_struct_def$trans
    (ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 484, class: <png-bytep>);
end method png_struct_def$trans;

define sealed method png_struct_def$trans-setter
    (value :: <png-bytep>, ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 484, class: <png-bytep>) := value;
  value;
end method png_struct_def$trans-setter;

define sealed method png_struct_def$trans-values
    (ptr :: <png-struct-def>) => (result :: <png-color-16>);
  as(<png-color-16>, ptr + 488);
end method png_struct_def$trans-values;

define sealed method png_struct_def$read-row-fn
    (ptr :: <png-struct-def>) => (result :: <png-read-status-ptr>);
  pointer-at(ptr, offset: 500, class: <png-read-status-ptr>);
end method png_struct_def$read-row-fn;

define sealed method png_struct_def$read-row-fn-setter
    (value :: <png-read-status-ptr>, ptr :: <png-struct-def>) => (result :: <png-read-status-ptr>);
  pointer-at(ptr, offset: 500, class: <png-read-status-ptr>) := value;
  value;
end method png_struct_def$read-row-fn-setter;

define sealed method png_struct_def$write-row-fn
    (ptr :: <png-struct-def>) => (result :: <png-write-status-ptr>);
  pointer-at(ptr, offset: 504, class: <png-write-status-ptr>);
end method png_struct_def$write-row-fn;

define sealed method png_struct_def$write-row-fn-setter
    (value :: <png-write-status-ptr>, ptr :: <png-struct-def>) => (result :: <png-write-status-ptr>);
  pointer-at(ptr, offset: 504, class: <png-write-status-ptr>) := value;
  value;
end method png_struct_def$write-row-fn-setter;

define sealed method png_struct_def$info-fn
    (ptr :: <png-struct-def>) => (result :: <png-progressive-info-ptr>);
  pointer-at(ptr, offset: 508, class: <png-progressive-info-ptr>);
end method png_struct_def$info-fn;

define sealed method png_struct_def$info-fn-setter
    (value :: <png-progressive-info-ptr>, ptr :: <png-struct-def>) => (result :: <png-progressive-info-ptr>);
  pointer-at(ptr, offset: 508, class: <png-progressive-info-ptr>) := value;
  value;
end method png_struct_def$info-fn-setter;

define sealed method png_struct_def$row-fn
    (ptr :: <png-struct-def>) => (result :: <png-progressive-row-ptr>);
  pointer-at(ptr, offset: 512, class: <png-progressive-row-ptr>);
end method png_struct_def$row-fn;

define sealed method png_struct_def$row-fn-setter
    (value :: <png-progressive-row-ptr>, ptr :: <png-struct-def>) => (result :: <png-progressive-row-ptr>);
  pointer-at(ptr, offset: 512, class: <png-progressive-row-ptr>) := value;
  value;
end method png_struct_def$row-fn-setter;

define sealed method png_struct_def$end-fn
    (ptr :: <png-struct-def>) => (result :: <png-progressive-end-ptr>);
  pointer-at(ptr, offset: 516, class: <png-progressive-end-ptr>);
end method png_struct_def$end-fn;

define sealed method png_struct_def$end-fn-setter
    (value :: <png-progressive-end-ptr>, ptr :: <png-struct-def>) => (result :: <png-progressive-end-ptr>);
  pointer-at(ptr, offset: 516, class: <png-progressive-end-ptr>) := value;
  value;
end method png_struct_def$end-fn-setter;

define sealed method png_struct_def$save-buffer-ptr
    (ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 520, class: <png-bytep>);
end method png_struct_def$save-buffer-ptr;

define sealed method png_struct_def$save-buffer-ptr-setter
    (value :: <png-bytep>, ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 520, class: <png-bytep>) := value;
  value;
end method png_struct_def$save-buffer-ptr-setter;

define sealed method png_struct_def$save-buffer
    (ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 524, class: <png-bytep>);
end method png_struct_def$save-buffer;

define sealed method png_struct_def$save-buffer-setter
    (value :: <png-bytep>, ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 524, class: <png-bytep>) := value;
  value;
end method png_struct_def$save-buffer-setter;

define sealed method png_struct_def$current-buffer-ptr
    (ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 528, class: <png-bytep>);
end method png_struct_def$current-buffer-ptr;

define sealed method png_struct_def$current-buffer-ptr-setter
    (value :: <png-bytep>, ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 528, class: <png-bytep>) := value;
  value;
end method png_struct_def$current-buffer-ptr-setter;

define sealed method png_struct_def$current-buffer
    (ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 532, class: <png-bytep>);
end method png_struct_def$current-buffer;

define sealed method png_struct_def$current-buffer-setter
    (value :: <png-bytep>, ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 532, class: <png-bytep>) := value;
  value;
end method png_struct_def$current-buffer-setter;

define sealed method png_struct_def$push-length
    (ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 536);
end method png_struct_def$push-length;

define sealed method png_struct_def$push-length-setter
    (value :: <png-uint-32>, ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 536) := value;
  value;
end method png_struct_def$push-length-setter;

define sealed method png_struct_def$skip-length
    (ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 540);
end method png_struct_def$skip-length;

define sealed method png_struct_def$skip-length-setter
    (value :: <png-uint-32>, ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 540) := value;
  value;
end method png_struct_def$skip-length-setter;

define sealed method png_struct_def$save-buffer-size
    (ptr :: <png-struct-def>) => (result :: <png-size-t>);
  unsigned-long-at(ptr, offset: 544);
end method png_struct_def$save-buffer-size;

define sealed method png_struct_def$save-buffer-size-setter
    (value :: <png-size-t>, ptr :: <png-struct-def>) => (result :: <png-size-t>);
  unsigned-long-at(ptr, offset: 544) := value;
  value;
end method png_struct_def$save-buffer-size-setter;

define sealed method png_struct_def$save-buffer-max
    (ptr :: <png-struct-def>) => (result :: <png-size-t>);
  unsigned-long-at(ptr, offset: 548);
end method png_struct_def$save-buffer-max;

define sealed method png_struct_def$save-buffer-max-setter
    (value :: <png-size-t>, ptr :: <png-struct-def>) => (result :: <png-size-t>);
  unsigned-long-at(ptr, offset: 548) := value;
  value;
end method png_struct_def$save-buffer-max-setter;

define sealed method png_struct_def$buffer-size
    (ptr :: <png-struct-def>) => (result :: <png-size-t>);
  unsigned-long-at(ptr, offset: 552);
end method png_struct_def$buffer-size;

define sealed method png_struct_def$buffer-size-setter
    (value :: <png-size-t>, ptr :: <png-struct-def>) => (result :: <png-size-t>);
  unsigned-long-at(ptr, offset: 552) := value;
  value;
end method png_struct_def$buffer-size-setter;

define sealed method png_struct_def$current-buffer-size
    (ptr :: <png-struct-def>) => (result :: <png-size-t>);
  unsigned-long-at(ptr, offset: 556);
end method png_struct_def$current-buffer-size;

define sealed method png_struct_def$current-buffer-size-setter
    (value :: <png-size-t>, ptr :: <png-struct-def>) => (result :: <png-size-t>);
  unsigned-long-at(ptr, offset: 556) := value;
  value;
end method png_struct_def$current-buffer-size-setter;

define sealed method png_struct_def$process-mode
    (ptr :: <png-struct-def>) => (result :: <integer>);
  signed-long-at(ptr, offset: 560);
end method png_struct_def$process-mode;

define sealed method png_struct_def$process-mode-setter
    (value :: <integer>, ptr :: <png-struct-def>) => (result :: <integer>);
  signed-long-at(ptr, offset: 560) := value;
  value;
end method png_struct_def$process-mode-setter;

define sealed method png_struct_def$cur-palette
    (ptr :: <png-struct-def>) => (result :: <integer>);
  signed-long-at(ptr, offset: 564);
end method png_struct_def$cur-palette;

define sealed method png_struct_def$cur-palette-setter
    (value :: <integer>, ptr :: <png-struct-def>) => (result :: <integer>);
  signed-long-at(ptr, offset: 564) := value;
  value;
end method png_struct_def$cur-palette-setter;

define sealed method png_struct_def$current-text-size
    (ptr :: <png-struct-def>) => (result :: <png-size-t>);
  unsigned-long-at(ptr, offset: 568);
end method png_struct_def$current-text-size;

define sealed method png_struct_def$current-text-size-setter
    (value :: <png-size-t>, ptr :: <png-struct-def>) => (result :: <png-size-t>);
  unsigned-long-at(ptr, offset: 568) := value;
  value;
end method png_struct_def$current-text-size-setter;

define sealed method png_struct_def$current-text-left
    (ptr :: <png-struct-def>) => (result :: <png-size-t>);
  unsigned-long-at(ptr, offset: 572);
end method png_struct_def$current-text-left;

define sealed method png_struct_def$current-text-left-setter
    (value :: <png-size-t>, ptr :: <png-struct-def>) => (result :: <png-size-t>);
  unsigned-long-at(ptr, offset: 572) := value;
  value;
end method png_struct_def$current-text-left-setter;

define sealed method png_struct_def$current-text
    (ptr :: <png-struct-def>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 576, class: <png-charp>);
end method png_struct_def$current-text;

define sealed method png_struct_def$current-text-setter
    (value :: <png-charp>, ptr :: <png-struct-def>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 576, class: <png-charp>) := value;
  value;
end method png_struct_def$current-text-setter;

define sealed method png_struct_def$current-text-ptr
    (ptr :: <png-struct-def>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 580, class: <png-charp>);
end method png_struct_def$current-text-ptr;

define sealed method png_struct_def$current-text-ptr-setter
    (value :: <png-charp>, ptr :: <png-struct-def>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 580, class: <png-charp>) := value;
  value;
end method png_struct_def$current-text-ptr-setter;

define sealed method png_struct_def$palette-lookup
    (ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 584, class: <png-bytep>);
end method png_struct_def$palette-lookup;

define sealed method png_struct_def$palette-lookup-setter
    (value :: <png-bytep>, ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 584, class: <png-bytep>) := value;
  value;
end method png_struct_def$palette-lookup-setter;

define sealed method png_struct_def$dither-index
    (ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 588, class: <png-bytep>);
end method png_struct_def$dither-index;

define sealed method png_struct_def$dither-index-setter
    (value :: <png-bytep>, ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 588, class: <png-bytep>) := value;
  value;
end method png_struct_def$dither-index-setter;

define sealed method png_struct_def$hist
    (ptr :: <png-struct-def>) => (result :: <png-uint-16p>);
  pointer-at(ptr, offset: 592, class: <png-uint-16p>);
end method png_struct_def$hist;

define sealed method png_struct_def$hist-setter
    (value :: <png-uint-16p>, ptr :: <png-struct-def>) => (result :: <png-uint-16p>);
  pointer-at(ptr, offset: 592, class: <png-uint-16p>) := value;
  value;
end method png_struct_def$hist-setter;

define sealed method png_struct_def$heuristic-method
    (ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 596);
end method png_struct_def$heuristic-method;

define sealed method png_struct_def$heuristic-method-setter
    (value :: <png-byte>, ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 596) := value;
  value;
end method png_struct_def$heuristic-method-setter;

define sealed method png_struct_def$num-prev-filters
    (ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 597);
end method png_struct_def$num-prev-filters;

define sealed method png_struct_def$num-prev-filters-setter
    (value :: <png-byte>, ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 597) := value;
  value;
end method png_struct_def$num-prev-filters-setter;

define sealed method png_struct_def$prev-filters
    (ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 600, class: <png-bytep>);
end method png_struct_def$prev-filters;

define sealed method png_struct_def$prev-filters-setter
    (value :: <png-bytep>, ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 600, class: <png-bytep>) := value;
  value;
end method png_struct_def$prev-filters-setter;

define sealed method png_struct_def$filter-weights
    (ptr :: <png-struct-def>) => (result :: <png-uint-16p>);
  pointer-at(ptr, offset: 604, class: <png-uint-16p>);
end method png_struct_def$filter-weights;

define sealed method png_struct_def$filter-weights-setter
    (value :: <png-uint-16p>, ptr :: <png-struct-def>) => (result :: <png-uint-16p>);
  pointer-at(ptr, offset: 604, class: <png-uint-16p>) := value;
  value;
end method png_struct_def$filter-weights-setter;

define sealed method png_struct_def$inv-filter-weights
    (ptr :: <png-struct-def>) => (result :: <png-uint-16p>);
  pointer-at(ptr, offset: 608, class: <png-uint-16p>);
end method png_struct_def$inv-filter-weights;

define sealed method png_struct_def$inv-filter-weights-setter
    (value :: <png-uint-16p>, ptr :: <png-struct-def>) => (result :: <png-uint-16p>);
  pointer-at(ptr, offset: 608, class: <png-uint-16p>) := value;
  value;
end method png_struct_def$inv-filter-weights-setter;

define sealed method png_struct_def$filter-costs
    (ptr :: <png-struct-def>) => (result :: <png-uint-16p>);
  pointer-at(ptr, offset: 612, class: <png-uint-16p>);
end method png_struct_def$filter-costs;

define sealed method png_struct_def$filter-costs-setter
    (value :: <png-uint-16p>, ptr :: <png-struct-def>) => (result :: <png-uint-16p>);
  pointer-at(ptr, offset: 612, class: <png-uint-16p>) := value;
  value;
end method png_struct_def$filter-costs-setter;

define sealed method png_struct_def$inv-filter-costs
    (ptr :: <png-struct-def>) => (result :: <png-uint-16p>);
  pointer-at(ptr, offset: 616, class: <png-uint-16p>);
end method png_struct_def$inv-filter-costs;

define sealed method png_struct_def$inv-filter-costs-setter
    (value :: <png-uint-16p>, ptr :: <png-struct-def>) => (result :: <png-uint-16p>);
  pointer-at(ptr, offset: 616, class: <png-uint-16p>) := value;
  value;
end method png_struct_def$inv-filter-costs-setter;

define sealed method png_struct_def$time-buffer
    (ptr :: <png-struct-def>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 620, class: <png-charp>);
end method png_struct_def$time-buffer;

define sealed method png_struct_def$time-buffer-setter
    (value :: <png-charp>, ptr :: <png-struct-def>) => (result :: <png-charp>);
  pointer-at(ptr, offset: 620, class: <png-charp>) := value;
  value;
end method png_struct_def$time-buffer-setter;

define sealed method png_struct_def$free-me
    (ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 624);
end method png_struct_def$free-me;

define sealed method png_struct_def$free-me-setter
    (value :: <png-uint-32>, ptr :: <png-struct-def>) => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: 624) := value;
  value;
end method png_struct_def$free-me-setter;

define sealed method png_struct_def$user-chunk-ptr
    (ptr :: <png-struct-def>) => (result :: <png-voidp>);
  pointer-at(ptr, offset: 628, class: <png-voidp>);
end method png_struct_def$user-chunk-ptr;

define sealed method png_struct_def$user-chunk-ptr-setter
    (value :: <png-voidp>, ptr :: <png-struct-def>) => (result :: <png-voidp>);
  pointer-at(ptr, offset: 628, class: <png-voidp>) := value;
  value;
end method png_struct_def$user-chunk-ptr-setter;

define sealed method png_struct_def$read-user-chunk-fn
    (ptr :: <png-struct-def>) => (result :: <png-user-chunk-ptr>);
  pointer-at(ptr, offset: 632, class: <png-user-chunk-ptr>);
end method png_struct_def$read-user-chunk-fn;

define sealed method png_struct_def$read-user-chunk-fn-setter
    (value :: <png-user-chunk-ptr>, ptr :: <png-struct-def>) => (result :: <png-user-chunk-ptr>);
  pointer-at(ptr, offset: 632, class: <png-user-chunk-ptr>) := value;
  value;
end method png_struct_def$read-user-chunk-fn-setter;

define sealed method png_struct_def$num-chunk-list
    (ptr :: <png-struct-def>) => (result :: <integer>);
  signed-long-at(ptr, offset: 636);
end method png_struct_def$num-chunk-list;

define sealed method png_struct_def$num-chunk-list-setter
    (value :: <integer>, ptr :: <png-struct-def>) => (result :: <integer>);
  signed-long-at(ptr, offset: 636) := value;
  value;
end method png_struct_def$num-chunk-list-setter;

define sealed method png_struct_def$chunk-list
    (ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 640, class: <png-bytep>);
end method png_struct_def$chunk-list;

define sealed method png_struct_def$chunk-list-setter
    (value :: <png-bytep>, ptr :: <png-struct-def>) => (result :: <png-bytep>);
  pointer-at(ptr, offset: 640, class: <png-bytep>) := value;
  value;
end method png_struct_def$chunk-list-setter;

define sealed method png_struct_def$rgb-to-gray-status
    (ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 644);
end method png_struct_def$rgb-to-gray-status;

define sealed method png_struct_def$rgb-to-gray-status-setter
    (value :: <png-byte>, ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 644) := value;
  value;
end method png_struct_def$rgb-to-gray-status-setter;

define sealed method png_struct_def$rgb-to-gray-red-coeff
    (ptr :: <png-struct-def>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 646);
end method png_struct_def$rgb-to-gray-red-coeff;

define sealed method png_struct_def$rgb-to-gray-red-coeff-setter
    (value :: <png-uint-16>, ptr :: <png-struct-def>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 646) := value;
  value;
end method png_struct_def$rgb-to-gray-red-coeff-setter;

define sealed method png_struct_def$rgb-to-gray-green-coeff
    (ptr :: <png-struct-def>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 648);
end method png_struct_def$rgb-to-gray-green-coeff;

define sealed method png_struct_def$rgb-to-gray-green-coeff-setter
    (value :: <png-uint-16>, ptr :: <png-struct-def>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 648) := value;
  value;
end method png_struct_def$rgb-to-gray-green-coeff-setter;

define sealed method png_struct_def$rgb-to-gray-blue-coeff
    (ptr :: <png-struct-def>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 650);
end method png_struct_def$rgb-to-gray-blue-coeff;

define sealed method png_struct_def$rgb-to-gray-blue-coeff-setter
    (value :: <png-uint-16>, ptr :: <png-struct-def>) => (result :: <png-uint-16>);
  unsigned-short-at(ptr, offset: 650) := value;
  value;
end method png_struct_def$rgb-to-gray-blue-coeff-setter;

define sealed method png_struct_def$mng-features-permitted
    (ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 652);
end method png_struct_def$mng-features-permitted;

define sealed method png_struct_def$mng-features-permitted-setter
    (value :: <png-byte>, ptr :: <png-struct-def>) => (result :: <png-byte>);
  unsigned-byte-at(ptr, offset: 652) := value;
  value;
end method png_struct_def$mng-features-permitted-setter;

define sealed method png_struct_def$int-gamma
    (ptr :: <png-struct-def>) => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: 656);
end method png_struct_def$int-gamma;

define sealed method png_struct_def$int-gamma-setter
    (value :: <png-fixed-point>, ptr :: <png-struct-def>) => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: 656) := value;
  value;
end method png_struct_def$int-gamma-setter;

define method pointer-value (value :: <png-struct-def>, #key index = 0) => (result :: <png-struct-def>);
  value + index * 660;
end method pointer-value;

define method content-size (value :: subclass(<png-struct-def>)) => (result :: <integer>);
  660;
end method content-size;

define functional class <anonymous-320> (<function-pointer>) end;

define constant <png-unknown-chunk-ptr> = <anonymous-320>;

define functional class <anonymous-321> (<function-pointer>) end;

define constant <png-malloc-ptr> = <anonymous-321>;

define functional class <anonymous-322> (<function-pointer>) end;

define constant <png-free-ptr> = <anonymous-322>;

define constant <version-1-0-12> = <png-structp>;

define functional class <anonymous-324> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-324>));

define method pointer-value
    (ptr :: <anonymous-324>, #key index = 0)
 => (result :: <png-struct>);
  pointer-at(ptr, offset: index * 4, class: <png-struct>);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-struct>, ptr :: <anonymous-324>, #key index = 0)
 => (result :: <png-struct>);
  pointer-at(ptr, offset: index * 4, class: <png-struct>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-324>)) => (result :: <integer>);
  4;
end method content-size;

define constant <png-structpp> = <anonymous-324>;

define method png-access-version-number
    ()
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_access_version_number", long:);
  values(result-value);
end method png-access-version-number;

define method png-set-sig-bytes
    (arg1 :: <png-structp>, arg2 :: <integer>)
 => ();
  call-out("png_set_sig_bytes", void:, ptr: (arg1).raw-value, int: arg2);
  values();
end method png-set-sig-bytes;

define method png-sig-cmp
    (arg1 :: <png-bytep>, arg2 :: <png-size-t>, arg3 :: <png-size-t>)
 => (result :: <integer>);
  let result-value
    = call-out("png_sig_cmp", int:, ptr: (arg1).raw-value, long: arg2, long: arg3);
  values(result-value);
end method png-sig-cmp;

define method png-check-sig
    (arg1 :: <png-bytep>, arg2 :: <integer>)
 => (result :: <integer>);
  let result-value
    = call-out("png_check_sig", int:, ptr: (arg1).raw-value, int: arg2);
  values(result-value);
end method png-check-sig;

define method png-create-read-struct
    (arg1 :: <png-const-charp>, arg2 :: <png-voidp>, arg3 :: <png-error-ptr>, arg4 :: <png-error-ptr>)
 => (result :: <png-structp>);
  let result-value
    = call-out("png_create_read_struct", ptr:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value);
  let result-value = make(<png-structp>, pointer: result-value);
  values(result-value);
end method png-create-read-struct;

define method png-create-write-struct
    (arg1 :: <png-const-charp>, arg2 :: <png-voidp>, arg3 :: <png-error-ptr>, arg4 :: <png-error-ptr>)
 => (result :: <png-structp>);
  let result-value
    = call-out("png_create_write_struct", ptr:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value);
  let result-value = make(<png-structp>, pointer: result-value);
  values(result-value);
end method png-create-write-struct;

define method png-get-compression-buffer-size
    (arg1 :: <png-structp>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_compression_buffer_size", long:, ptr: (arg1).raw-value);
  values(result-value);
end method png-get-compression-buffer-size;

define method png-set-compression-buffer-size
    (arg1 :: <png-structp>, arg2 :: <png-uint-32>)
 => ();
  call-out("png_set_compression_buffer_size", void:, ptr: (arg1).raw-value, long: arg2);
  values();
end method png-set-compression-buffer-size;

define method png-reset-zstream
    (arg1 :: <png-structp>)
 => (result :: <integer>);
  let result-value
    = call-out("png_reset_zstream", int:, ptr: (arg1).raw-value);
  values(result-value);
end method png-reset-zstream;

define method png-write-chunk
    (arg1 :: <png-structp>, arg2 :: <png-bytep>, arg3 :: <png-bytep>, arg4 :: <png-size-t>)
 => ();
  call-out("png_write_chunk", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, long: arg4);
  values();
end method png-write-chunk;

define method png-write-chunk-start
    (arg1 :: <png-structp>, arg2 :: <png-bytep>, arg3 :: <png-uint-32>)
 => ();
  call-out("png_write_chunk_start", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, long: arg3);
  values();
end method png-write-chunk-start;

define method png-write-chunk-data
    (arg1 :: <png-structp>, arg2 :: <png-bytep>, arg3 :: <png-size-t>)
 => ();
  call-out("png_write_chunk_data", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, long: arg3);
  values();
end method png-write-chunk-data;

define method png-write-chunk-end
    (arg1 :: <png-structp>)
 => ();
  call-out("png_write_chunk_end", void:, ptr: (arg1).raw-value);
  values();
end method png-write-chunk-end;

define method png-create-info-struct
    (arg1 :: <png-structp>)
 => (result :: <png-infop>);
  let result-value
    = call-out("png_create_info_struct", ptr:, ptr: (arg1).raw-value);
  let result-value = make(<png-infop>, pointer: result-value);
  values(result-value);
end method png-create-info-struct;

define method png-info-init
    (arg1 :: <png-infop>)
 => ();
  call-out("png_info_init", void:, ptr: (arg1).raw-value);
  values();
end method png-info-init;

define method png-info-init-3
    (arg1 :: <png-infopp>, arg2 :: <png-size-t>)
 => ();
  call-out("png_info_init_3", void:, ptr: (arg1).raw-value, long: arg2);
  values();
end method png-info-init-3;

define method png-write-info-before-PLTE
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => ();
  call-out("png_write_info_before_PLTE", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-write-info-before-PLTE;

define method png-write-info
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => ();
  call-out("png_write_info", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-write-info;

define method png-read-info
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => ();
  call-out("png_read_info", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-read-info;

define method png-convert-to-rfc1123
    (arg1 :: <png-structp>, arg2 :: <png-timep>)
 => (result :: <png-charp>);
  let result-value
    = call-out("png_convert_to_rfc1123", ptr:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  let result-value = make(<png-charp>, pointer: result-value);
  values(result-value);
end method png-convert-to-rfc1123;

define functional class <tm> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<tm>));

define sealed method tm$tm-sec
    (ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0);
end method tm$tm-sec;

define sealed method tm$tm-sec-setter
    (value :: <integer>, ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0) := value;
  value;
end method tm$tm-sec-setter;

define sealed method tm$tm-min
    (ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 4);
end method tm$tm-min;

define sealed method tm$tm-min-setter
    (value :: <integer>, ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 4) := value;
  value;
end method tm$tm-min-setter;

define sealed method tm$tm-hour
    (ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8);
end method tm$tm-hour;

define sealed method tm$tm-hour-setter
    (value :: <integer>, ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8) := value;
  value;
end method tm$tm-hour-setter;

define sealed method tm$tm-mday
    (ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 12);
end method tm$tm-mday;

define sealed method tm$tm-mday-setter
    (value :: <integer>, ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 12) := value;
  value;
end method tm$tm-mday-setter;

define sealed method tm$tm-mon
    (ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 16);
end method tm$tm-mon;

define sealed method tm$tm-mon-setter
    (value :: <integer>, ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 16) := value;
  value;
end method tm$tm-mon-setter;

define sealed method tm$tm-year
    (ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 20);
end method tm$tm-year;

define sealed method tm$tm-year-setter
    (value :: <integer>, ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 20) := value;
  value;
end method tm$tm-year-setter;

define sealed method tm$tm-wday
    (ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 24);
end method tm$tm-wday;

define sealed method tm$tm-wday-setter
    (value :: <integer>, ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 24) := value;
  value;
end method tm$tm-wday-setter;

define sealed method tm$tm-yday
    (ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 28);
end method tm$tm-yday;

define sealed method tm$tm-yday-setter
    (value :: <integer>, ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 28) := value;
  value;
end method tm$tm-yday-setter;

define sealed method tm$tm-isdst
    (ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 32);
end method tm$tm-isdst;

define sealed method tm$tm-isdst-setter
    (value :: <integer>, ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 32) := value;
  value;
end method tm$tm-isdst-setter;

define sealed method tm$tm-gmtoff
    (ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 36);
end method tm$tm-gmtoff;

define sealed method tm$tm-gmtoff-setter
    (value :: <integer>, ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 36) := value;
  value;
end method tm$tm-gmtoff-setter;

define sealed method tm$tm-zone
    (ptr :: <tm>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 40, class: <anonymous-3>);
end method tm$tm-zone;

define sealed method tm$tm-zone-setter
    (value :: <anonymous-3>, ptr :: <tm>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 40, class: <anonymous-3>) := value;
  value;
end method tm$tm-zone-setter;

define method pointer-value (value :: <tm>, #key index = 0) => (result :: <tm>);
  value + index * 44;
end method pointer-value;

define method content-size (value :: subclass(<tm>)) => (result :: <integer>);
  44;
end method content-size;

define method png-convert-from-struct-tm
    (arg1 :: <png-timep>, arg2 :: <tm>)
 => ();
  call-out("png_convert_from_struct_tm", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-convert-from-struct-tm;

define constant <__time-t> = <integer>;

define constant <time-t> = <__time-t>;

define method png-convert-from-time-t
    (arg1 :: <png-timep>, arg2 :: <time-t>)
 => ();
  call-out("png_convert_from_time_t", void:, ptr: (arg1).raw-value, long: arg2);
  values();
end method png-convert-from-time-t;

define method png-set-expand
    (arg1 :: <png-structp>)
 => ();
  call-out("png_set_expand", void:, ptr: (arg1).raw-value);
  values();
end method png-set-expand;

define method png-set-gray-1-2-4-to-8
    (arg1 :: <png-structp>)
 => ();
  call-out("png_set_gray_1_2_4_to_8", void:, ptr: (arg1).raw-value);
  values();
end method png-set-gray-1-2-4-to-8;

define method png-set-palette-to-rgb
    (arg1 :: <png-structp>)
 => ();
  call-out("png_set_palette_to_rgb", void:, ptr: (arg1).raw-value);
  values();
end method png-set-palette-to-rgb;

define method png-set-tRNS-to-alpha
    (arg1 :: <png-structp>)
 => ();
  call-out("png_set_tRNS_to_alpha", void:, ptr: (arg1).raw-value);
  values();
end method png-set-tRNS-to-alpha;

define method png-set-bgr
    (arg1 :: <png-structp>)
 => ();
  call-out("png_set_bgr", void:, ptr: (arg1).raw-value);
  values();
end method png-set-bgr;

define method png-set-gray-to-rgb
    (arg1 :: <png-structp>)
 => ();
  call-out("png_set_gray_to_rgb", void:, ptr: (arg1).raw-value);
  values();
end method png-set-gray-to-rgb;

define method png-set-rgb-to-gray
    (arg1 :: <png-structp>, arg2 :: <integer>, arg3 :: <double-float>, arg4 :: <double-float>)
 => ();
  call-out("png_set_rgb_to_gray", void:, ptr: (arg1).raw-value, int: arg2, double: arg3, double: arg4);
  values();
end method png-set-rgb-to-gray;

define method png-set-rgb-to-gray-fixed
    (arg1 :: <png-structp>, arg2 :: <integer>, arg3 :: <png-fixed-point>, arg4 :: <png-fixed-point>)
 => ();
  call-out("png_set_rgb_to_gray_fixed", void:, ptr: (arg1).raw-value, int: arg2, long: arg3, long: arg4);
  values();
end method png-set-rgb-to-gray-fixed;

define method png-get-rgb-to-gray-status
    (arg1 :: <png-structp>)
 => (result :: <png-byte>);
  let result-value
    = call-out("png_get_rgb_to_gray_status", unsigned-char:, ptr: (arg1).raw-value);
  values(result-value);
end method png-get-rgb-to-gray-status;

define method png-build-grayscale-palette
    (arg1 :: <integer>, arg2 :: <png-colorp>)
 => ();
  call-out("png_build_grayscale_palette", void:, int: arg1, ptr: (arg2).raw-value);
  values();
end method png-build-grayscale-palette;

define method png-set-strip-alpha
    (arg1 :: <png-structp>)
 => ();
  call-out("png_set_strip_alpha", void:, ptr: (arg1).raw-value);
  values();
end method png-set-strip-alpha;

define method png-set-swap-alpha
    (arg1 :: <png-structp>)
 => ();
  call-out("png_set_swap_alpha", void:, ptr: (arg1).raw-value);
  values();
end method png-set-swap-alpha;

define method png-set-invert-alpha
    (arg1 :: <png-structp>)
 => ();
  call-out("png_set_invert_alpha", void:, ptr: (arg1).raw-value);
  values();
end method png-set-invert-alpha;

define method png-set-filler
    (arg1 :: <png-structp>, arg2 :: <png-uint-32>, arg3 :: <integer>)
 => ();
  call-out("png_set_filler", void:, ptr: (arg1).raw-value, long: arg2, int: arg3);
  values();
end method png-set-filler;

define method png-set-swap
    (arg1 :: <png-structp>)
 => ();
  call-out("png_set_swap", void:, ptr: (arg1).raw-value);
  values();
end method png-set-swap;

define method png-set-packing
    (arg1 :: <png-structp>)
 => ();
  call-out("png_set_packing", void:, ptr: (arg1).raw-value);
  values();
end method png-set-packing;

define method png-set-packswap
    (arg1 :: <png-structp>)
 => ();
  call-out("png_set_packswap", void:, ptr: (arg1).raw-value);
  values();
end method png-set-packswap;

define method png-set-shift
    (arg1 :: <png-structp>, arg2 :: <png-color-8p>)
 => ();
  call-out("png_set_shift", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-set-shift;

define method png-set-interlace-handling
    (arg1 :: <png-structp>)
 => (result :: <integer>);
  let result-value
    = call-out("png_set_interlace_handling", int:, ptr: (arg1).raw-value);
  values(result-value);
end method png-set-interlace-handling;

define method png-set-invert-mono
    (arg1 :: <png-structp>)
 => ();
  call-out("png_set_invert_mono", void:, ptr: (arg1).raw-value);
  values();
end method png-set-invert-mono;

define method png-set-background
    (arg1 :: <png-structp>, arg2 :: <png-color-16p>, arg3 :: <integer>, arg4 :: <integer>, arg5 :: <double-float>)
 => ();
  call-out("png_set_background", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, int: arg3, int: arg4, double: arg5);
  values();
end method png-set-background;

define method png-set-strip-16
    (arg1 :: <png-structp>)
 => ();
  call-out("png_set_strip_16", void:, ptr: (arg1).raw-value);
  values();
end method png-set-strip-16;

define method png-set-dither
    (arg1 :: <png-structp>, arg2 :: <png-colorp>, arg3 :: <integer>, arg4 :: <integer>, arg5 :: <png-uint-16p>, arg6 :: <integer>)
 => ();
  call-out("png_set_dither", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, int: arg3, int: arg4, ptr: (arg5).raw-value, int: arg6);
  values();
end method png-set-dither;

define method png-set-gamma
    (arg1 :: <png-structp>, arg2 :: <double-float>, arg3 :: <double-float>)
 => ();
  call-out("png_set_gamma", void:, ptr: (arg1).raw-value, double: arg2, double: arg3);
  values();
end method png-set-gamma;

define method png-permit-empty-plte
    (arg1 :: <png-structp>, arg2 :: <integer>)
 => ();
  call-out("png_permit_empty_plte", void:, ptr: (arg1).raw-value, int: arg2);
  values();
end method png-permit-empty-plte;

define method png-set-flush
    (arg1 :: <png-structp>, arg2 :: <integer>)
 => ();
  call-out("png_set_flush", void:, ptr: (arg1).raw-value, int: arg2);
  values();
end method png-set-flush;

define method png-write-flush
    (arg1 :: <png-structp>)
 => ();
  call-out("png_write_flush", void:, ptr: (arg1).raw-value);
  values();
end method png-write-flush;

define method png-start-read-image
    (arg1 :: <png-structp>)
 => ();
  call-out("png_start_read_image", void:, ptr: (arg1).raw-value);
  values();
end method png-start-read-image;

define method png-read-update-info
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => ();
  call-out("png_read_update_info", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-read-update-info;

define method png-read-rows
    (arg1 :: <png-structp>, arg2 :: <png-bytepp>, arg3 :: <png-bytepp>, arg4 :: <png-uint-32>)
 => ();
  call-out("png_read_rows", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, long: arg4);
  values();
end method png-read-rows;

define method png-read-row
    (arg1 :: <png-structp>, arg2 :: <png-bytep>, arg3 :: <png-bytep>)
 => ();
  call-out("png_read_row", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values();
end method png-read-row;

define method png-read-image
    (arg1 :: <png-structp>, arg2 :: <png-bytepp>)
 => ();
  call-out("png_read_image", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-read-image;

define method png-write-row
    (arg1 :: <png-structp>, arg2 :: <png-bytep>)
 => ();
  call-out("png_write_row", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-write-row;

define method png-write-rows
    (arg1 :: <png-structp>, arg2 :: <png-bytepp>, arg3 :: <png-uint-32>)
 => ();
  call-out("png_write_rows", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, long: arg3);
  values();
end method png-write-rows;

define method png-write-image
    (arg1 :: <png-structp>, arg2 :: <png-bytepp>)
 => ();
  call-out("png_write_image", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-write-image;

define method png-write-end
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => ();
  call-out("png_write_end", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-write-end;

define method png-read-end
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => ();
  call-out("png_read_end", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-read-end;

define method png-destroy-info-struct
    (arg1 :: <png-structp>, arg2 :: <png-infopp>)
 => ();
  call-out("png_destroy_info_struct", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-destroy-info-struct;

define method png-destroy-read-struct
    (arg1 :: <png-structpp>, arg2 :: <png-infopp>, arg3 :: <png-infopp>)
 => ();
  call-out("png_destroy_read_struct", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values();
end method png-destroy-read-struct;

define method png-read-destroy
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-infop>)
 => ();
  call-out("png_read_destroy", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values();
end method png-read-destroy;

define method png-destroy-write-struct
    (arg1 :: <png-structpp>, arg2 :: <png-infopp>)
 => ();
  call-out("png_destroy_write_struct", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-destroy-write-struct;

define method png-write-destroy-info
    (arg1 :: <png-infop>)
 => ();
  call-out("png_write_destroy_info", void:, ptr: (arg1).raw-value);
  values();
end method png-write-destroy-info;

define method png-write-destroy
    (arg1 :: <png-structp>)
 => ();
  call-out("png_write_destroy", void:, ptr: (arg1).raw-value);
  values();
end method png-write-destroy;

define method png-set-crc-action
    (arg1 :: <png-structp>, arg2 :: <integer>, arg3 :: <integer>)
 => ();
  call-out("png_set_crc_action", void:, ptr: (arg1).raw-value, int: arg2, int: arg3);
  values();
end method png-set-crc-action;

define method png-set-filter
    (arg1 :: <png-structp>, arg2 :: <integer>, arg3 :: <integer>)
 => ();
  call-out("png_set_filter", void:, ptr: (arg1).raw-value, int: arg2, int: arg3);
  values();
end method png-set-filter;

define functional class <anonymous-281> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-281>));

define method pointer-value
    (ptr :: <anonymous-281>, #key index = 0)
 => (result :: <double-float>);
  double-at(ptr, offset: index * 8);
end method pointer-value;

define method pointer-value-setter
    (value :: <double-float>, ptr :: <anonymous-281>, #key index = 0)
 => (result :: <double-float>);
  double-at(ptr, offset: index * 8) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-281>)) => (result :: <integer>);
  8;
end method content-size;

define constant <png-doublep> = <anonymous-281>;

define method png-set-filter-heuristics
    (arg1 :: <png-structp>, arg2 :: <integer>, arg3 :: <integer>, arg4 :: <png-doublep>, arg5 :: <png-doublep>)
 => ();
  call-out("png_set_filter_heuristics", void:, ptr: (arg1).raw-value, int: arg2, int: arg3, ptr: (arg4).raw-value, ptr: (arg5).raw-value);
  values();
end method png-set-filter-heuristics;

define method png-set-compression-level
    (arg1 :: <png-structp>, arg2 :: <integer>)
 => ();
  call-out("png_set_compression_level", void:, ptr: (arg1).raw-value, int: arg2);
  values();
end method png-set-compression-level;

define method png-set-compression-mem-level
    (arg1 :: <png-structp>, arg2 :: <integer>)
 => ();
  call-out("png_set_compression_mem_level", void:, ptr: (arg1).raw-value, int: arg2);
  values();
end method png-set-compression-mem-level;

define method png-set-compression-strategy
    (arg1 :: <png-structp>, arg2 :: <integer>)
 => ();
  call-out("png_set_compression_strategy", void:, ptr: (arg1).raw-value, int: arg2);
  values();
end method png-set-compression-strategy;

define method png-set-compression-window-bits
    (arg1 :: <png-structp>, arg2 :: <integer>)
 => ();
  call-out("png_set_compression_window_bits", void:, ptr: (arg1).raw-value, int: arg2);
  values();
end method png-set-compression-window-bits;

define method png-set-compression-method
    (arg1 :: <png-structp>, arg2 :: <integer>)
 => ();
  call-out("png_set_compression_method", void:, ptr: (arg1).raw-value, int: arg2);
  values();
end method png-set-compression-method;

define functional class <_IO-marker> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<_IO-marker>));

define sealed method _IO_marker$_next
    (ptr :: <_IO-marker>) => (result :: <_IO-marker>);
  pointer-at(ptr, offset: 0, class: <_IO-marker>);
end method _IO_marker$_next;

define sealed method _IO_marker$_next-setter
    (value :: <_IO-marker>, ptr :: <_IO-marker>) => (result :: <_IO-marker>);
  pointer-at(ptr, offset: 0, class: <_IO-marker>) := value;
  value;
end method _IO_marker$_next-setter;

define sealed method _IO_marker$_sbuf
    (ptr :: <_IO-marker>) => (result :: <_IO-FILE>);
  pointer-at(ptr, offset: 4, class: <_IO-FILE>);
end method _IO_marker$_sbuf;

define sealed method _IO_marker$_sbuf-setter
    (value :: <_IO-FILE>, ptr :: <_IO-marker>) => (result :: <_IO-FILE>);
  pointer-at(ptr, offset: 4, class: <_IO-FILE>) := value;
  value;
end method _IO_marker$_sbuf-setter;

define sealed method _IO_marker$_pos
    (ptr :: <_IO-marker>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8);
end method _IO_marker$_pos;

define sealed method _IO_marker$_pos-setter
    (value :: <integer>, ptr :: <_IO-marker>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8) := value;
  value;
end method _IO_marker$_pos-setter;

define method pointer-value (value :: <_IO-marker>, #key index = 0) => (result :: <_IO-marker>);
  value + index * 12;
end method pointer-value;

define method content-size (value :: subclass(<_IO-marker>)) => (result :: <integer>);
  12;
end method content-size;

define constant <__off-t> = <integer>;

define functional class <anonymous-82> (<anonymous-3>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-82>));

define method content-size (value == <anonymous-82>)  => (result :: <integer>);
  1;
end method content-size;

define functional class <anonymous-83> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-83>));

define method content-size (value :: subclass(<anonymous-83>)) => (result :: <integer>);
  0;
end method content-size;

define constant <__off64-t> = <double-integer>;

define functional class <anonymous-84> (<anonymous-3>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-84>));

define method content-size (value == <anonymous-84>)  => (result :: <integer>);
  52;
end method content-size;

define functional class <_IO-FILE> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<_IO-FILE>));

define sealed method _IO_FILE$_flags
    (ptr :: <_IO-FILE>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0);
end method _IO_FILE$_flags;

define sealed method _IO_FILE$_flags-setter
    (value :: <integer>, ptr :: <_IO-FILE>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0) := value;
  value;
end method _IO_FILE$_flags-setter;

define sealed method _IO_FILE$_IO-read-ptr
    (ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 4, class: <anonymous-3>);
end method _IO_FILE$_IO-read-ptr;

define sealed method _IO_FILE$_IO-read-ptr-setter
    (value :: <anonymous-3>, ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 4, class: <anonymous-3>) := value;
  value;
end method _IO_FILE$_IO-read-ptr-setter;

define sealed method _IO_FILE$_IO-read-end
    (ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 8, class: <anonymous-3>);
end method _IO_FILE$_IO-read-end;

define sealed method _IO_FILE$_IO-read-end-setter
    (value :: <anonymous-3>, ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 8, class: <anonymous-3>) := value;
  value;
end method _IO_FILE$_IO-read-end-setter;

define sealed method _IO_FILE$_IO-read-base
    (ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 12, class: <anonymous-3>);
end method _IO_FILE$_IO-read-base;

define sealed method _IO_FILE$_IO-read-base-setter
    (value :: <anonymous-3>, ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 12, class: <anonymous-3>) := value;
  value;
end method _IO_FILE$_IO-read-base-setter;

define sealed method _IO_FILE$_IO-write-base
    (ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 16, class: <anonymous-3>);
end method _IO_FILE$_IO-write-base;

define sealed method _IO_FILE$_IO-write-base-setter
    (value :: <anonymous-3>, ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 16, class: <anonymous-3>) := value;
  value;
end method _IO_FILE$_IO-write-base-setter;

define sealed method _IO_FILE$_IO-write-ptr
    (ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 20, class: <anonymous-3>);
end method _IO_FILE$_IO-write-ptr;

define sealed method _IO_FILE$_IO-write-ptr-setter
    (value :: <anonymous-3>, ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 20, class: <anonymous-3>) := value;
  value;
end method _IO_FILE$_IO-write-ptr-setter;

define sealed method _IO_FILE$_IO-write-end
    (ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 24, class: <anonymous-3>);
end method _IO_FILE$_IO-write-end;

define sealed method _IO_FILE$_IO-write-end-setter
    (value :: <anonymous-3>, ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 24, class: <anonymous-3>) := value;
  value;
end method _IO_FILE$_IO-write-end-setter;

define sealed method _IO_FILE$_IO-buf-base
    (ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 28, class: <anonymous-3>);
end method _IO_FILE$_IO-buf-base;

define sealed method _IO_FILE$_IO-buf-base-setter
    (value :: <anonymous-3>, ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 28, class: <anonymous-3>) := value;
  value;
end method _IO_FILE$_IO-buf-base-setter;

define sealed method _IO_FILE$_IO-buf-end
    (ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 32, class: <anonymous-3>);
end method _IO_FILE$_IO-buf-end;

define sealed method _IO_FILE$_IO-buf-end-setter
    (value :: <anonymous-3>, ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 32, class: <anonymous-3>) := value;
  value;
end method _IO_FILE$_IO-buf-end-setter;

define sealed method _IO_FILE$_IO-save-base
    (ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 36, class: <anonymous-3>);
end method _IO_FILE$_IO-save-base;

define sealed method _IO_FILE$_IO-save-base-setter
    (value :: <anonymous-3>, ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 36, class: <anonymous-3>) := value;
  value;
end method _IO_FILE$_IO-save-base-setter;

define sealed method _IO_FILE$_IO-backup-base
    (ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 40, class: <anonymous-3>);
end method _IO_FILE$_IO-backup-base;

define sealed method _IO_FILE$_IO-backup-base-setter
    (value :: <anonymous-3>, ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 40, class: <anonymous-3>) := value;
  value;
end method _IO_FILE$_IO-backup-base-setter;

define sealed method _IO_FILE$_IO-save-end
    (ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 44, class: <anonymous-3>);
end method _IO_FILE$_IO-save-end;

define sealed method _IO_FILE$_IO-save-end-setter
    (value :: <anonymous-3>, ptr :: <_IO-FILE>) => (result :: <anonymous-3>);
  pointer-at(ptr, offset: 44, class: <anonymous-3>) := value;
  value;
end method _IO_FILE$_IO-save-end-setter;

define sealed method _IO_FILE$_markers
    (ptr :: <_IO-FILE>) => (result :: <_IO-marker>);
  pointer-at(ptr, offset: 48, class: <_IO-marker>);
end method _IO_FILE$_markers;

define sealed method _IO_FILE$_markers-setter
    (value :: <_IO-marker>, ptr :: <_IO-FILE>) => (result :: <_IO-marker>);
  pointer-at(ptr, offset: 48, class: <_IO-marker>) := value;
  value;
end method _IO_FILE$_markers-setter;

define sealed method _IO_FILE$_chain
    (ptr :: <_IO-FILE>) => (result :: <_IO-FILE>);
  pointer-at(ptr, offset: 52, class: <_IO-FILE>);
end method _IO_FILE$_chain;

define sealed method _IO_FILE$_chain-setter
    (value :: <_IO-FILE>, ptr :: <_IO-FILE>) => (result :: <_IO-FILE>);
  pointer-at(ptr, offset: 52, class: <_IO-FILE>) := value;
  value;
end method _IO_FILE$_chain-setter;

define sealed method _IO_FILE$_fileno
    (ptr :: <_IO-FILE>) => (result :: <integer>);
  signed-long-at(ptr, offset: 56);
end method _IO_FILE$_fileno;

define sealed method _IO_FILE$_fileno-setter
    (value :: <integer>, ptr :: <_IO-FILE>) => (result :: <integer>);
  signed-long-at(ptr, offset: 56) := value;
  value;
end method _IO_FILE$_fileno-setter;

define sealed method _IO_FILE$_flags2
    (ptr :: <_IO-FILE>) => (result :: <integer>);
  signed-long-at(ptr, offset: 60);
end method _IO_FILE$_flags2;

define sealed method _IO_FILE$_flags2-setter
    (value :: <integer>, ptr :: <_IO-FILE>) => (result :: <integer>);
  signed-long-at(ptr, offset: 60) := value;
  value;
end method _IO_FILE$_flags2-setter;

define sealed method _IO_FILE$_old-offset
    (ptr :: <_IO-FILE>) => (result :: <__off-t>);
  signed-long-at(ptr, offset: 64);
end method _IO_FILE$_old-offset;

define sealed method _IO_FILE$_old-offset-setter
    (value :: <__off-t>, ptr :: <_IO-FILE>) => (result :: <__off-t>);
  signed-long-at(ptr, offset: 64) := value;
  value;
end method _IO_FILE$_old-offset-setter;

define sealed method _IO_FILE$_cur-column
    (ptr :: <_IO-FILE>) => (result :: <integer>);
  unsigned-short-at(ptr, offset: 68);
end method _IO_FILE$_cur-column;

define sealed method _IO_FILE$_cur-column-setter
    (value :: <integer>, ptr :: <_IO-FILE>) => (result :: <integer>);
  unsigned-short-at(ptr, offset: 68) := value;
  value;
end method _IO_FILE$_cur-column-setter;

define sealed method _IO_FILE$_vtable-offset
    (ptr :: <_IO-FILE>) => (result :: <integer>);
  signed-byte-at(ptr, offset: 70);
end method _IO_FILE$_vtable-offset;

define sealed method _IO_FILE$_vtable-offset-setter
    (value :: <integer>, ptr :: <_IO-FILE>) => (result :: <integer>);
  signed-byte-at(ptr, offset: 70) := value;
  value;
end method _IO_FILE$_vtable-offset-setter;

define sealed method _IO_FILE$_shortbuf
    (ptr :: <_IO-FILE>) => (result :: <anonymous-82>);
  as(<anonymous-82>, ptr + 71);
end method _IO_FILE$_shortbuf;

define sealed method _IO_FILE$_lock
    (ptr :: <_IO-FILE>) => (result :: <anonymous-83>);
  pointer-at(ptr, offset: 72, class: <anonymous-83>);
end method _IO_FILE$_lock;

define sealed method _IO_FILE$_lock-setter
    (value :: <anonymous-83>, ptr :: <_IO-FILE>) => (result :: <anonymous-83>);
  pointer-at(ptr, offset: 72, class: <anonymous-83>) := value;
  value;
end method _IO_FILE$_lock-setter;

define sealed method _IO_FILE$_offset
    (ptr :: <_IO-FILE>) => (result :: <__off64-t>);
  longlong-at(ptr, offset: 80);
end method _IO_FILE$_offset;

define sealed method _IO_FILE$_offset-setter
    (value :: <__off64-t>, ptr :: <_IO-FILE>) => (result :: <__off64-t>);
  longlong-at(ptr, offset: 80) := value;
  value;
end method _IO_FILE$_offset-setter;

define sealed method _IO_FILE$__pad1
    (ptr :: <_IO-FILE>) => (result :: <machine-pointer>);
  pointer-at(ptr, offset: 88, class: <machine-pointer>);
end method _IO_FILE$__pad1;

define sealed method _IO_FILE$__pad1-setter
    (value :: <machine-pointer>, ptr :: <_IO-FILE>) => (result :: <machine-pointer>);
  pointer-at(ptr, offset: 88, class: <machine-pointer>) := value;
  value;
end method _IO_FILE$__pad1-setter;

define sealed method _IO_FILE$__pad2
    (ptr :: <_IO-FILE>) => (result :: <machine-pointer>);
  pointer-at(ptr, offset: 92, class: <machine-pointer>);
end method _IO_FILE$__pad2;

define sealed method _IO_FILE$__pad2-setter
    (value :: <machine-pointer>, ptr :: <_IO-FILE>) => (result :: <machine-pointer>);
  pointer-at(ptr, offset: 92, class: <machine-pointer>) := value;
  value;
end method _IO_FILE$__pad2-setter;

define sealed method _IO_FILE$_mode
    (ptr :: <_IO-FILE>) => (result :: <integer>);
  signed-long-at(ptr, offset: 96);
end method _IO_FILE$_mode;

define sealed method _IO_FILE$_mode-setter
    (value :: <integer>, ptr :: <_IO-FILE>) => (result :: <integer>);
  signed-long-at(ptr, offset: 96) := value;
  value;
end method _IO_FILE$_mode-setter;

define sealed method _IO_FILE$_unused2
    (ptr :: <_IO-FILE>) => (result :: <anonymous-84>);
  as(<anonymous-84>, ptr + 100);
end method _IO_FILE$_unused2;

define method pointer-value (value :: <_IO-FILE>, #key index = 0) => (result :: <_IO-FILE>);
  value + index * 152;
end method pointer-value;

define method content-size (value :: subclass(<_IO-FILE>)) => (result :: <integer>);
  152;
end method content-size;

define constant <FILE> = <_IO-FILE>;

define constant <png-FILE-p> = <FILE>;

define method png-init-io
    (arg1 :: <png-structp>, arg2 :: <png-FILE-p>)
 => ();
  call-out("png_init_io", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-init-io;

define method png-set-error-fn
    (arg1 :: <png-structp>, arg2 :: <png-voidp>, arg3 :: <png-error-ptr>, arg4 :: <png-error-ptr>)
 => ();
  call-out("png_set_error_fn", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value);
  values();
end method png-set-error-fn;

define method png-get-error-ptr
    (arg1 :: <png-structp>)
 => (result :: <png-voidp>);
  let result-value
    = call-out("png_get_error_ptr", ptr:, ptr: (arg1).raw-value);
  let result-value = make(<png-voidp>, pointer: result-value);
  values(result-value);
end method png-get-error-ptr;

define method png-set-write-fn
    (arg1 :: <png-structp>, arg2 :: <png-voidp>, arg3 :: <png-rw-ptr>, arg4 :: <png-flush-ptr>)
 => ();
  call-out("png_set_write_fn", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value);
  values();
end method png-set-write-fn;

define method png-set-read-fn
    (arg1 :: <png-structp>, arg2 :: <png-voidp>, arg3 :: <png-rw-ptr>)
 => ();
  call-out("png_set_read_fn", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values();
end method png-set-read-fn;

define method png-get-io-ptr
    (arg1 :: <png-structp>)
 => (result :: <png-voidp>);
  let result-value
    = call-out("png_get_io_ptr", ptr:, ptr: (arg1).raw-value);
  let result-value = make(<png-voidp>, pointer: result-value);
  values(result-value);
end method png-get-io-ptr;

define method png-set-read-status-fn
    (arg1 :: <png-structp>, arg2 :: <png-read-status-ptr>)
 => ();
  call-out("png_set_read_status_fn", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-set-read-status-fn;

define method png-set-write-status-fn
    (arg1 :: <png-structp>, arg2 :: <png-write-status-ptr>)
 => ();
  call-out("png_set_write_status_fn", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-set-write-status-fn;

define method png-set-read-user-transform-fn
    (arg1 :: <png-structp>, arg2 :: <png-user-transform-ptr>)
 => ();
  call-out("png_set_read_user_transform_fn", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-set-read-user-transform-fn;

define method png-set-write-user-transform-fn
    (arg1 :: <png-structp>, arg2 :: <png-user-transform-ptr>)
 => ();
  call-out("png_set_write_user_transform_fn", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-set-write-user-transform-fn;

define method png-set-user-transform-info
    (arg1 :: <png-structp>, arg2 :: <png-voidp>, arg3 :: <integer>, arg4 :: <integer>)
 => ();
  call-out("png_set_user_transform_info", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, int: arg3, int: arg4);
  values();
end method png-set-user-transform-info;

define method png-get-user-transform-ptr
    (arg1 :: <png-structp>)
 => (result :: <png-voidp>);
  let result-value
    = call-out("png_get_user_transform_ptr", ptr:, ptr: (arg1).raw-value);
  let result-value = make(<png-voidp>, pointer: result-value);
  values(result-value);
end method png-get-user-transform-ptr;

define method png-set-read-user-chunk-fn
    (arg1 :: <png-structp>, arg2 :: <png-voidp>, arg3 :: <png-user-chunk-ptr>)
 => ();
  call-out("png_set_read_user_chunk_fn", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values();
end method png-set-read-user-chunk-fn;

define method png-get-user-chunk-ptr
    (arg1 :: <png-structp>)
 => (result :: <png-voidp>);
  let result-value
    = call-out("png_get_user_chunk_ptr", ptr:, ptr: (arg1).raw-value);
  let result-value = make(<png-voidp>, pointer: result-value);
  values(result-value);
end method png-get-user-chunk-ptr;

define method png-set-progressive-read-fn
    (arg1 :: <png-structp>, arg2 :: <png-voidp>, arg3 :: <png-progressive-info-ptr>, arg4 :: <png-progressive-row-ptr>, arg5 :: <png-progressive-end-ptr>)
 => ();
  call-out("png_set_progressive_read_fn", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value, ptr: (arg5).raw-value);
  values();
end method png-set-progressive-read-fn;

define method png-get-progressive-ptr
    (arg1 :: <png-structp>)
 => (result :: <png-voidp>);
  let result-value
    = call-out("png_get_progressive_ptr", ptr:, ptr: (arg1).raw-value);
  let result-value = make(<png-voidp>, pointer: result-value);
  values(result-value);
end method png-get-progressive-ptr;

define method png-process-data
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-bytep>, arg4 :: <png-size-t>)
 => ();
  call-out("png_process_data", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, long: arg4);
  values();
end method png-process-data;

define method png-progressive-combine-row
    (arg1 :: <png-structp>, arg2 :: <png-bytep>, arg3 :: <png-bytep>)
 => ();
  call-out("png_progressive_combine_row", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values();
end method png-progressive-combine-row;

define method png-malloc
    (arg1 :: <png-structp>, arg2 :: <png-uint-32>)
 => (result :: <png-voidp>);
  let result-value
    = call-out("png_malloc", ptr:, ptr: (arg1).raw-value, long: arg2);
  let result-value = make(<png-voidp>, pointer: result-value);
  values(result-value);
end method png-malloc;

define method png-free
    (arg1 :: <png-structp>, arg2 :: <png-voidp>)
 => ();
  call-out("png_free", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-free;

define method png-free-data
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-uint-32>, arg4 :: <integer>)
 => ();
  call-out("png_free_data", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, long: arg3, int: arg4);
  values();
end method png-free-data;

define method png-data-freer
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <integer>, arg4 :: <png-uint-32>)
 => ();
  call-out("png_data_freer", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, int: arg3, long: arg4);
  values();
end method png-data-freer;

define method png-memcpy-check
    (arg1 :: <png-structp>, arg2 :: <png-voidp>, arg3 :: <png-voidp>, arg4 :: <png-uint-32>)
 => (result :: <png-voidp>);
  let result-value
    = call-out("png_memcpy_check", ptr:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, long: arg4);
  let result-value = make(<png-voidp>, pointer: result-value);
  values(result-value);
end method png-memcpy-check;

define method png-memset-check
    (arg1 :: <png-structp>, arg2 :: <png-voidp>, arg3 :: <integer>, arg4 :: <png-uint-32>)
 => (result :: <png-voidp>);
  let result-value
    = call-out("png_memset_check", ptr:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, int: arg3, long: arg4);
  let result-value = make(<png-voidp>, pointer: result-value);
  values(result-value);
end method png-memset-check;

define method png-error
    (arg1 :: <png-structp>, arg2 :: <png-const-charp>)
 => ();
  call-out("png_error", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-error;

define method png-chunk-error
    (arg1 :: <png-structp>, arg2 :: <png-const-charp>)
 => ();
  call-out("png_chunk_error", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-chunk-error;

define method png-warning
    (arg1 :: <png-structp>, arg2 :: <png-const-charp>)
 => ();
  call-out("png_warning", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-warning;

define method png-chunk-warning
    (arg1 :: <png-structp>, arg2 :: <png-const-charp>)
 => ();
  call-out("png_chunk_warning", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method png-chunk-warning;

define method png-get-valid
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-uint-32>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_valid", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, long: arg3);
  values(result-value);
end method png-get-valid;

define method png-get-rowbytes
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_rowbytes", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values(result-value);
end method png-get-rowbytes;

define method png-get-rows
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => (result :: <png-bytepp>);
  let result-value
    = call-out("png_get_rows", ptr:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  let result-value = make(<png-bytepp>, pointer: result-value);
  values(result-value);
end method png-get-rows;

define method png-set-rows
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-bytepp>)
 => ();
  call-out("png_set_rows", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values();
end method png-set-rows;

define method png-get-channels
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => (result :: <png-byte>);
  let result-value
    = call-out("png_get_channels", unsigned-char:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values(result-value);
end method png-get-channels;

define method png-get-image-width
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_image_width", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values(result-value);
end method png-get-image-width;

define method png-get-image-height
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_image_height", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values(result-value);
end method png-get-image-height;

define method png-get-bit-depth
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => (result :: <png-byte>);
  let result-value
    = call-out("png_get_bit_depth", unsigned-char:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values(result-value);
end method png-get-bit-depth;

define method png-get-color-type
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => (result :: <png-byte>);
  let result-value
    = call-out("png_get_color_type", unsigned-char:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values(result-value);
end method png-get-color-type;

define method png-get-filter-type
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => (result :: <png-byte>);
  let result-value
    = call-out("png_get_filter_type", unsigned-char:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values(result-value);
end method png-get-filter-type;

define method png-get-interlace-type
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => (result :: <png-byte>);
  let result-value
    = call-out("png_get_interlace_type", unsigned-char:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values(result-value);
end method png-get-interlace-type;

define method png-get-compression-type
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => (result :: <png-byte>);
  let result-value
    = call-out("png_get_compression_type", unsigned-char:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values(result-value);
end method png-get-compression-type;

define method png-get-pixels-per-meter
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_pixels_per_meter", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values(result-value);
end method png-get-pixels-per-meter;

define method png-get-x-pixels-per-meter
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_x_pixels_per_meter", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values(result-value);
end method png-get-x-pixels-per-meter;

define method png-get-y-pixels-per-meter
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_y_pixels_per_meter", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values(result-value);
end method png-get-y-pixels-per-meter;

define method png-get-pixel-aspect-ratio
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => (result :: <single-float>);
  let result-value
    = call-out("png_get_pixel_aspect_ratio", float:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values(result-value);
end method png-get-pixel-aspect-ratio;

define method png-get-x-offset-pixels
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => (result :: <png-int-32>);
  let result-value
    = call-out("png_get_x_offset_pixels", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values(result-value);
end method png-get-x-offset-pixels;

define method png-get-y-offset-pixels
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => (result :: <png-int-32>);
  let result-value
    = call-out("png_get_y_offset_pixels", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values(result-value);
end method png-get-y-offset-pixels;

define method png-get-x-offset-microns
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => (result :: <png-int-32>);
  let result-value
    = call-out("png_get_x_offset_microns", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values(result-value);
end method png-get-x-offset-microns;

define method png-get-y-offset-microns
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => (result :: <png-int-32>);
  let result-value
    = call-out("png_get_y_offset_microns", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values(result-value);
end method png-get-y-offset-microns;

define method png-get-signature
    (arg1 :: <png-structp>, arg2 :: <png-infop>)
 => (result :: <png-bytep>);
  let result-value
    = call-out("png_get_signature", ptr:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  let result-value = make(<png-bytep>, pointer: result-value);
  values(result-value);
end method png-get-signature;

define functional class <anonymous-447> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-447>));

define method pointer-value
    (ptr :: <anonymous-447>, #key index = 0)
 => (result :: <png-color-16p>);
  pointer-at(ptr, offset: index * 4, class: <png-color-16p>);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-color-16p>, ptr :: <anonymous-447>, #key index = 0)
 => (result :: <png-color-16p>);
  pointer-at(ptr, offset: index * 4, class: <png-color-16p>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-447>)) => (result :: <integer>);
  4;
end method content-size;

define method png-get-bKGD
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <anonymous-447>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_bKGD", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values(result-value);
end method png-get-bKGD;

define method png-set-bKGD
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-color-16p>)
 => ();
  call-out("png_set_bKGD", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values();
end method png-set-bKGD;

define method png-get-cHRM
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <anonymous-281>, arg4 :: <anonymous-281>, arg5 :: <anonymous-281>, arg6 :: <anonymous-281>, arg7 :: <anonymous-281>, arg8 :: <anonymous-281>, arg9 :: <anonymous-281>, arg10 :: <anonymous-281>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_cHRM", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value, ptr: (arg5).raw-value, ptr: (arg6).raw-value, ptr: (arg7).raw-value, ptr: (arg8).raw-value, ptr: (arg9).raw-value, ptr: (arg10).raw-value);
  values(result-value);
end method png-get-cHRM;

define functional class <anonymous-280> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-280>));

define method pointer-value
    (ptr :: <anonymous-280>, #key index = 0)
 => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: index * 4);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-fixed-point>, ptr :: <anonymous-280>, #key index = 0)
 => (result :: <png-fixed-point>);
  signed-long-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-280>)) => (result :: <integer>);
  4;
end method content-size;

define method png-get-cHRM-fixed
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <anonymous-280>, arg4 :: <anonymous-280>, arg5 :: <anonymous-280>, arg6 :: <anonymous-280>, arg7 :: <anonymous-280>, arg8 :: <anonymous-280>, arg9 :: <anonymous-280>, arg10 :: <anonymous-280>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_cHRM_fixed", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value, ptr: (arg5).raw-value, ptr: (arg6).raw-value, ptr: (arg7).raw-value, ptr: (arg8).raw-value, ptr: (arg9).raw-value, ptr: (arg10).raw-value);
  values(result-value);
end method png-get-cHRM-fixed;

define method png-set-cHRM
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <double-float>, arg4 :: <double-float>, arg5 :: <double-float>, arg6 :: <double-float>, arg7 :: <double-float>, arg8 :: <double-float>, arg9 :: <double-float>, arg10 :: <double-float>)
 => ();
  call-out("png_set_cHRM", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, double: arg3, double: arg4, double: arg5, double: arg6, double: arg7, double: arg8, double: arg9, double: arg10);
  values();
end method png-set-cHRM;

define method png-set-cHRM-fixed
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-fixed-point>, arg4 :: <png-fixed-point>, arg5 :: <png-fixed-point>, arg6 :: <png-fixed-point>, arg7 :: <png-fixed-point>, arg8 :: <png-fixed-point>, arg9 :: <png-fixed-point>, arg10 :: <png-fixed-point>)
 => ();
  call-out("png_set_cHRM_fixed", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, long: arg3, long: arg4, long: arg5, long: arg6, long: arg7, long: arg8, long: arg9, long: arg10);
  values();
end method png-set-cHRM-fixed;

define method png-get-gAMA
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <anonymous-281>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_gAMA", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values(result-value);
end method png-get-gAMA;

define method png-get-gAMA-fixed
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <anonymous-280>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_gAMA_fixed", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values(result-value);
end method png-get-gAMA-fixed;

define method png-set-gAMA
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <double-float>)
 => ();
  call-out("png_set_gAMA", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, double: arg3);
  values();
end method png-set-gAMA;

define method png-set-gAMA-fixed
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-fixed-point>)
 => ();
  call-out("png_set_gAMA_fixed", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, long: arg3);
  values();
end method png-set-gAMA-fixed;

define functional class <anonymous-458> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-458>));

define method pointer-value
    (ptr :: <anonymous-458>, #key index = 0)
 => (result :: <png-uint-16p>);
  pointer-at(ptr, offset: index * 4, class: <png-uint-16p>);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-uint-16p>, ptr :: <anonymous-458>, #key index = 0)
 => (result :: <png-uint-16p>);
  pointer-at(ptr, offset: index * 4, class: <png-uint-16p>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-458>)) => (result :: <integer>);
  4;
end method content-size;

define method png-get-hIST
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <anonymous-458>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_hIST", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values(result-value);
end method png-get-hIST;

define method png-set-hIST
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-uint-16p>)
 => ();
  call-out("png_set_hIST", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values();
end method png-set-hIST;

define functional class <anonymous-276> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-276>));

define method pointer-value
    (ptr :: <anonymous-276>, #key index = 0)
 => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: index * 4);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-uint-32>, ptr :: <anonymous-276>, #key index = 0)
 => (result :: <png-uint-32>);
  unsigned-long-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-276>)) => (result :: <integer>);
  4;
end method content-size;

define method png-get-IHDR
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <anonymous-276>, arg4 :: <anonymous-276>, arg5 :: <anonymous-36>, arg6 :: <anonymous-36>, arg7 :: <anonymous-36>, arg8 :: <anonymous-36>, arg9 :: <anonymous-36>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_IHDR", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value, ptr: (arg5).raw-value, ptr: (arg6).raw-value, ptr: (arg7).raw-value, ptr: (arg8).raw-value, ptr: (arg9).raw-value);
  values(result-value);
end method png-get-IHDR;

define method png-set-IHDR
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-uint-32>, arg4 :: <png-uint-32>, arg5 :: <integer>, arg6 :: <integer>, arg7 :: <integer>, arg8 :: <integer>, arg9 :: <integer>)
 => ();
  call-out("png_set_IHDR", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, long: arg3, long: arg4, int: arg5, int: arg6, int: arg7, int: arg8, int: arg9);
  values();
end method png-set-IHDR;

define functional class <anonymous-277> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-277>));

define method pointer-value
    (ptr :: <anonymous-277>, #key index = 0)
 => (result :: <png-int-32>);
  signed-long-at(ptr, offset: index * 4);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-int-32>, ptr :: <anonymous-277>, #key index = 0)
 => (result :: <png-int-32>);
  signed-long-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-277>)) => (result :: <integer>);
  4;
end method content-size;

define method png-get-oFFs
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <anonymous-277>, arg4 :: <anonymous-277>, arg5 :: <anonymous-36>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_oFFs", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value, ptr: (arg5).raw-value);
  values(result-value);
end method png-get-oFFs;

define method png-set-oFFs
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-int-32>, arg4 :: <png-int-32>, arg5 :: <integer>)
 => ();
  call-out("png_set_oFFs", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, long: arg3, long: arg4, int: arg5);
  values();
end method png-set-oFFs;

define functional class <anonymous-465> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-465>));

define method pointer-value
    (ptr :: <anonymous-465>, #key index = 0)
 => (result :: <png-charp>);
  pointer-at(ptr, offset: index * 4, class: <png-charp>);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-charp>, ptr :: <anonymous-465>, #key index = 0)
 => (result :: <png-charp>);
  pointer-at(ptr, offset: index * 4, class: <png-charp>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-465>)) => (result :: <integer>);
  4;
end method content-size;

define functional class <anonymous-466> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-466>));

define method pointer-value
    (ptr :: <anonymous-466>, #key index = 0)
 => (result :: <png-charpp>);
  pointer-at(ptr, offset: index * 4, class: <png-charpp>);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-charpp>, ptr :: <anonymous-466>, #key index = 0)
 => (result :: <png-charpp>);
  pointer-at(ptr, offset: index * 4, class: <png-charpp>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-466>)) => (result :: <integer>);
  4;
end method content-size;

define method png-get-pCAL
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <anonymous-465>, arg4 :: <anonymous-277>, arg5 :: <anonymous-277>, arg6 :: <anonymous-36>, arg7 :: <anonymous-36>, arg8 :: <anonymous-465>, arg9 :: <anonymous-466>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_pCAL", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value, ptr: (arg5).raw-value, ptr: (arg6).raw-value, ptr: (arg7).raw-value, ptr: (arg8).raw-value, ptr: (arg9).raw-value);
  values(result-value);
end method png-get-pCAL;

define method png-set-pCAL
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-charp>, arg4 :: <png-int-32>, arg5 :: <png-int-32>, arg6 :: <integer>, arg7 :: <integer>, arg8 :: <png-charp>, arg9 :: <png-charpp>)
 => ();
  call-out("png_set_pCAL", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, long: arg4, long: arg5, int: arg6, int: arg7, ptr: (arg8).raw-value, ptr: (arg9).raw-value);
  values();
end method png-set-pCAL;

define method png-get-pHYs
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <anonymous-276>, arg4 :: <anonymous-276>, arg5 :: <anonymous-36>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_pHYs", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value, ptr: (arg5).raw-value);
  values(result-value);
end method png-get-pHYs;

define method png-set-pHYs
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-uint-32>, arg4 :: <png-uint-32>, arg5 :: <integer>)
 => ();
  call-out("png_set_pHYs", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, long: arg3, long: arg4, int: arg5);
  values();
end method png-set-pHYs;

define functional class <anonymous-471> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-471>));

define method pointer-value
    (ptr :: <anonymous-471>, #key index = 0)
 => (result :: <png-colorp>);
  pointer-at(ptr, offset: index * 4, class: <png-colorp>);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-colorp>, ptr :: <anonymous-471>, #key index = 0)
 => (result :: <png-colorp>);
  pointer-at(ptr, offset: index * 4, class: <png-colorp>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-471>)) => (result :: <integer>);
  4;
end method content-size;

define method png-get-PLTE
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <anonymous-471>, arg4 :: <anonymous-36>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_PLTE", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value);
  values(result-value);
end method png-get-PLTE;

define method png-set-PLTE
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-colorp>, arg4 :: <integer>)
 => ();
  call-out("png_set_PLTE", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, int: arg4);
  values();
end method png-set-PLTE;

define functional class <anonymous-474> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-474>));

define method pointer-value
    (ptr :: <anonymous-474>, #key index = 0)
 => (result :: <png-color-8p>);
  pointer-at(ptr, offset: index * 4, class: <png-color-8p>);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-color-8p>, ptr :: <anonymous-474>, #key index = 0)
 => (result :: <png-color-8p>);
  pointer-at(ptr, offset: index * 4, class: <png-color-8p>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-474>)) => (result :: <integer>);
  4;
end method content-size;

define method png-get-sBIT
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <anonymous-474>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_sBIT", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values(result-value);
end method png-get-sBIT;

define method png-set-sBIT
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-color-8p>)
 => ();
  call-out("png_set_sBIT", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values();
end method png-set-sBIT;

define method png-get-sRGB
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <anonymous-36>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_sRGB", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values(result-value);
end method png-get-sRGB;

define method png-set-sRGB
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <integer>)
 => ();
  call-out("png_set_sRGB", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, int: arg3);
  values();
end method png-set-sRGB;

define method png-set-sRGB-gAMA-and-cHRM
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <integer>)
 => ();
  call-out("png_set_sRGB_gAMA_and_cHRM", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, int: arg3);
  values();
end method png-set-sRGB-gAMA-and-cHRM;

define method png-get-iCCP
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-charpp>, arg4 :: <anonymous-36>, arg5 :: <png-charpp>, arg6 :: <anonymous-276>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_iCCP", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value, ptr: (arg5).raw-value, ptr: (arg6).raw-value);
  values(result-value);
end method png-get-iCCP;

define method png-set-iCCP
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-charp>, arg4 :: <integer>, arg5 :: <png-charp>, arg6 :: <png-uint-32>)
 => ();
  call-out("png_set_iCCP", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, int: arg4, ptr: (arg5).raw-value, long: arg6);
  values();
end method png-set-iCCP;

define method png-get-sPLT
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-sPLT-tpp>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_sPLT", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values(result-value);
end method png-get-sPLT;

define method png-set-sPLT
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-sPLT-tp>, arg4 :: <integer>)
 => ();
  call-out("png_set_sPLT", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, int: arg4);
  values();
end method png-set-sPLT;

define functional class <anonymous-484> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-484>));

define method pointer-value
    (ptr :: <anonymous-484>, #key index = 0)
 => (result :: <png-textp>);
  pointer-at(ptr, offset: index * 4, class: <png-textp>);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-textp>, ptr :: <anonymous-484>, #key index = 0)
 => (result :: <png-textp>);
  pointer-at(ptr, offset: index * 4, class: <png-textp>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-484>)) => (result :: <integer>);
  4;
end method content-size;

define method png-get-text
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <anonymous-484>, arg4 :: <anonymous-36>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_text", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value);
  values(result-value);
end method png-get-text;

define method png-set-text
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-textp>, arg4 :: <integer>)
 => ();
  call-out("png_set_text", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, int: arg4);
  values();
end method png-set-text;

define functional class <anonymous-487> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-487>));

define method pointer-value
    (ptr :: <anonymous-487>, #key index = 0)
 => (result :: <png-timep>);
  pointer-at(ptr, offset: index * 4, class: <png-timep>);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-timep>, ptr :: <anonymous-487>, #key index = 0)
 => (result :: <png-timep>);
  pointer-at(ptr, offset: index * 4, class: <png-timep>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-487>)) => (result :: <integer>);
  4;
end method content-size;

define method png-get-tIME
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <anonymous-487>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_tIME", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values(result-value);
end method png-get-tIME;

define method png-set-tIME
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-timep>)
 => ();
  call-out("png_set_tIME", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values();
end method png-set-tIME;

define functional class <anonymous-490> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-490>));

define method pointer-value
    (ptr :: <anonymous-490>, #key index = 0)
 => (result :: <png-bytep>);
  pointer-at(ptr, offset: index * 4, class: <png-bytep>);
end method pointer-value;

define method pointer-value-setter
    (value :: <png-bytep>, ptr :: <anonymous-490>, #key index = 0)
 => (result :: <png-bytep>);
  pointer-at(ptr, offset: index * 4, class: <png-bytep>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-490>)) => (result :: <integer>);
  4;
end method content-size;

define method png-get-tRNS
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <anonymous-490>, arg4 :: <anonymous-36>, arg5 :: <anonymous-447>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_tRNS", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value, ptr: (arg5).raw-value);
  values(result-value);
end method png-get-tRNS;

define method png-set-tRNS
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-bytep>, arg4 :: <integer>, arg5 :: <png-color-16p>)
 => ();
  call-out("png_set_tRNS", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, int: arg4, ptr: (arg5).raw-value);
  values();
end method png-set-tRNS;

define method png-get-sCAL
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <anonymous-36>, arg4 :: <anonymous-281>, arg5 :: <anonymous-281>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_sCAL", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value, ptr: (arg5).raw-value);
  values(result-value);
end method png-get-sCAL;

define method png-set-sCAL
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <integer>, arg4 :: <double-float>, arg5 :: <double-float>)
 => ();
  call-out("png_set_sCAL", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, int: arg3, double: arg4, double: arg5);
  values();
end method png-set-sCAL;

define method png-set-sCAL-s
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <integer>, arg4 :: <png-charp>, arg5 :: <png-charp>)
 => ();
  call-out("png_set_sCAL_s", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, int: arg3, ptr: (arg4).raw-value, ptr: (arg5).raw-value);
  values();
end method png-set-sCAL-s;

define method png-set-keep-unknown-chunks
    (arg1 :: <png-structp>, arg2 :: <integer>, arg3 :: <png-bytep>, arg4 :: <integer>)
 => ();
  call-out("png_set_keep_unknown_chunks", void:, ptr: (arg1).raw-value, int: arg2, ptr: (arg3).raw-value, int: arg4);
  values();
end method png-set-keep-unknown-chunks;

define method png-set-unknown-chunks
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-unknown-chunkp>, arg4 :: <integer>)
 => ();
  call-out("png_set_unknown_chunks", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, int: arg4);
  values();
end method png-set-unknown-chunks;

define method png-set-unknown-chunk-location
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <integer>, arg4 :: <integer>)
 => ();
  call-out("png_set_unknown_chunk_location", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, int: arg3, int: arg4);
  values();
end method png-set-unknown-chunk-location;

define method png-get-unknown-chunks
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <png-unknown-chunkpp>)
 => (result :: <png-uint-32>);
  let result-value
    = call-out("png_get_unknown_chunks", long:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values(result-value);
end method png-get-unknown-chunks;

define method png-set-invalid
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <integer>)
 => ();
  call-out("png_set_invalid", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, int: arg3);
  values();
end method png-set-invalid;

define method png-read-png
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <integer>, arg4 :: <png-voidp>)
 => ();
  call-out("png_read_png", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, int: arg3, ptr: (arg4).raw-value);
  values();
end method png-read-png;

define method png-write-png
    (arg1 :: <png-structp>, arg2 :: <png-infop>, arg3 :: <integer>, arg4 :: <png-voidp>)
 => ();
  call-out("png_write_png", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, int: arg3, ptr: (arg4).raw-value);
  values();
end method png-write-png;

define method png-sig-bytes
    ()
 => (result :: <png-bytep>);
  let result-value
    = call-out("png_sig_bytes", ptr:);
  let result-value = make(<png-bytep>, pointer: result-value);
  values(result-value);
end method png-sig-bytes;

define method png-get-copyright
    (arg1 :: <png-structp>)
 => (result :: <png-charp>);
  let result-value
    = call-out("png_get_copyright", ptr:, ptr: (arg1).raw-value);
  let result-value = make(<png-charp>, pointer: result-value);
  values(result-value);
end method png-get-copyright;

define method png-get-header-ver
    (arg1 :: <png-structp>)
 => (result :: <png-charp>);
  let result-value
    = call-out("png_get_header_ver", ptr:, ptr: (arg1).raw-value);
  let result-value = make(<png-charp>, pointer: result-value);
  values(result-value);
end method png-get-header-ver;

define method png-get-header-version
    (arg1 :: <png-structp>)
 => (result :: <png-charp>);
  let result-value
    = call-out("png_get_header_version", ptr:, ptr: (arg1).raw-value);
  let result-value = make(<png-charp>, pointer: result-value);
  values(result-value);
end method png-get-header-version;

define method png-get-libpng-ver
    (arg1 :: <png-structp>)
 => (result :: <png-charp>);
  let result-value
    = call-out("png_get_libpng_ver", ptr:, ptr: (arg1).raw-value);
  let result-value = make(<png-charp>, pointer: result-value);
  values(result-value);
end method png-get-libpng-ver;

define constant $PNG-LIBPNG-VER-SONUM = 2;

define constant $PNG-LIBPNG-VER-MAJOR = 1;

define constant $PNG-LIBPNG-VER-MINOR = 0;

define constant $PNG-LIBPNG-VER-RELEASE = 12;

define constant $PNG-LIBPNG-VER-BUILD = 0;

define constant $PNG-LIBPNG-BUILD-ALPHA = 1;

define constant $PNG-LIBPNG-BUILD-BETA = 2;

define constant $PNG-LIBPNG-BUILD-RC = 3;

define constant $PNG-LIBPNG-BUILD-STABLE = 4;

define constant $PNG-LIBPNG-BUILD-TYPEMASK = 7;

define constant $PNG-LIBPNG-BUILD-PATCH = 8;

define constant $PNG-LIBPNG-BUILD-TYPE = 4;

define constant $PNG-LIBPNG-VER = 10012;

define constant $PNG-TEXT-COMPRESSION-NONE-WR = -3;

define constant $PNG-TEXT-COMPRESSION-zTXt-WR = -2;

define constant $PNG-TEXT-COMPRESSION-NONE = -1;

define constant $PNG-TEXT-COMPRESSION-zTXt = 0;

define constant $PNG-ITXT-COMPRESSION-NONE = 1;

define constant $PNG-ITXT-COMPRESSION-zTXt = 2;

define constant $PNG-TEXT-COMPRESSION-LAST = 3;

define constant $PNG-MAX-UINT = 2147483647;

define constant $PNG-COLOR-MASK-PALETTE = 1;

define constant $PNG-COLOR-MASK-COLOR = 2;

define constant $PNG-COLOR-MASK-ALPHA = 4;

define constant $PNG-COLOR-TYPE-GRAY = 0;

define constant $PNG-COLOR-TYPE-PALETTE = 3;

define constant $PNG-COLOR-TYPE-RGB = 2;

define constant $PNG-COLOR-TYPE-RGB-ALPHA = 6;

define constant $PNG-COLOR-TYPE-GRAY-ALPHA = 4;

define constant $PNG-COLOR-TYPE-RGBA = 6;

define constant $PNG-COLOR-TYPE-GA = 4;

define constant $PNG-COMPRESSION-TYPE-BASE = 0;

define constant $PNG-COMPRESSION-TYPE-DEFAULT = 0;

define constant $PNG-FILTER-TYPE-BASE = 0;

define constant $PNG-INTRAPIXEL-DIFFERENCING = 64;

define constant $PNG-FILTER-TYPE-DEFAULT = 0;

define constant $PNG-INTERLACE-NONE = 0;

define constant $PNG-INTERLACE-ADAM7 = 1;

define constant $PNG-INTERLACE-LAST = 2;

define constant $PNG-OFFSET-PIXEL = 0;

define constant $PNG-OFFSET-MICROMETER = 1;

define constant $PNG-OFFSET-LAST = 2;

define constant $PNG-EQUATION-LINEAR = 0;

define constant $PNG-EQUATION-BASE-E = 1;

define constant $PNG-EQUATION-ARBITRARY = 2;

define constant $PNG-EQUATION-HYPERBOLIC = 3;

define constant $PNG-EQUATION-LAST = 4;

define constant $PNG-SCALE-UNKNOWN = 0;

define constant $PNG-SCALE-METER = 1;

define constant $PNG-SCALE-RADIAN = 2;

define constant $PNG-SCALE-LAST = 3;

define constant $PNG-RESOLUTION-UNKNOWN = 0;

define constant $PNG-RESOLUTION-METER = 1;

define constant $PNG-RESOLUTION-LAST = 2;

define constant $PNG-sRGB-INTENT-PERCEPTUAL = 0;

define constant $PNG-sRGB-INTENT-RELATIVE = 1;

define constant $PNG-sRGB-INTENT-SATURATION = 2;

define constant $PNG-sRGB-INTENT-ABSOLUTE = 3;

define constant $PNG-sRGB-INTENT-LAST = 4;

define constant $PNG-KEYWORD-MAX-LENGTH = 79;

define constant $PNG-MAX-PALETTE-LENGTH = 256;

define constant $PNG-INFO-gAMA = 1;

define constant $PNG-INFO-sBIT = 2;

define constant $PNG-INFO-cHRM = 4;

define constant $PNG-INFO-PLTE = 8;

define constant $PNG-INFO-tRNS = 16;

define constant $PNG-INFO-bKGD = 32;

define constant $PNG-INFO-hIST = 64;

define constant $PNG-INFO-pHYs = 128;

define constant $PNG-INFO-oFFs = 256;

define constant $PNG-INFO-tIME = 512;

define constant $PNG-INFO-pCAL = 1024;

define constant $PNG-INFO-sRGB = 2048;

define constant $PNG-INFO-iCCP = 4096;

define constant $PNG-INFO-sPLT = 8192;

define constant $PNG-INFO-sCAL = 16384;

define constant $PNG-INFO-IDAT = 32768;

define constant $PNG-TRANSFORM-IDENTITY = 0;

define constant $PNG-TRANSFORM-STRIP-16 = 1;

define constant $PNG-TRANSFORM-STRIP-ALPHA = 2;

define constant $PNG-TRANSFORM-PACKING = 4;

define constant $PNG-TRANSFORM-PACKSWAP = 8;

define constant $PNG-TRANSFORM-EXPAND = 16;

define constant $PNG-TRANSFORM-INVERT-MONO = 32;

define constant $PNG-TRANSFORM-SHIFT = 64;

define constant $PNG-TRANSFORM-BGR = 128;

define constant $PNG-TRANSFORM-SWAP-ALPHA = 256;

define constant $PNG-TRANSFORM-SWAP-ENDIAN = 512;

define constant $PNG-TRANSFORM-INVERT-ALPHA = 1024;

define constant $PNG-TRANSFORM-STRIP-FILLER = 2048;

define constant $PNG-FLAG-MNG-EMPTY-PLTE = 1;

define constant $PNG-FLAG-MNG-FILTER-64 = 4;

define constant $PNG-ALL-MNG-FEATURES = 5;

define constant $PNG-FILLER-BEFORE = 0;

define constant $PNG-FILLER-AFTER = 1;

define constant $PNG-BACKGROUND-GAMMA-UNKNOWN = 0;

define constant $PNG-BACKGROUND-GAMMA-SCREEN = 1;

define constant $PNG-BACKGROUND-GAMMA-FILE = 2;

define constant $PNG-BACKGROUND-GAMMA-UNIQUE = 3;

define constant $PNG-CRC-DEFAULT = 0;

define constant $PNG-CRC-ERROR-QUIT = 1;

define constant $PNG-CRC-WARN-DISCARD = 2;

define constant $PNG-CRC-WARN-USE = 3;

define constant $PNG-CRC-QUIET-USE = 4;

define constant $PNG-CRC-NO-CHANGE = 5;

define constant $PNG-NO-FILTERS = 0;

define constant $PNG-FILTER-NONE = 8;

define constant $PNG-FILTER-SUB = 16;

define constant $PNG-FILTER-UP = 32;

define constant $PNG-FILTER-AVG = 64;

define constant $PNG-FILTER-PAETH = 128;

define constant $PNG-ALL-FILTERS = 248;

define constant $PNG-FILTER-VALUE-NONE = 0;

define constant $PNG-FILTER-VALUE-SUB = 1;

define constant $PNG-FILTER-VALUE-UP = 2;

define constant $PNG-FILTER-VALUE-AVG = 3;

define constant $PNG-FILTER-VALUE-PAETH = 4;

define constant $PNG-FILTER-VALUE-LAST = 5;

define constant $PNG-FILTER-HEURISTIC-DEFAULT = 0;

define constant $PNG-FILTER-HEURISTIC-UNWEIGHTED = 1;

define constant $PNG-FILTER-HEURISTIC-WEIGHTED = 2;

define constant $PNG-FILTER-HEURISTIC-LAST = 3;

define constant $PNG-DESTROY-WILL-FREE-DATA = 1;

define constant $PNG-SET-WILL-FREE-DATA = 1;

define constant $PNG-USER-WILL-FREE-DATA = 2;

define constant $PNG-FREE-HIST = 8;

define constant $PNG-FREE-ICCP = 16;

define constant $PNG-FREE-SPLT = 32;

define constant $PNG-FREE-ROWS = 64;

define constant $PNG-FREE-PCAL = 128;

define constant $PNG-FREE-SCAL = 256;

define constant $PNG-FREE-UNKN = 512;

define constant $PNG-FREE-LIST = 1024;

define constant $PNG-FREE-PLTE = 4096;

define constant $PNG-FREE-TRNS = 8192;

define constant $PNG-FREE-TEXT = 16384;

define constant $PNG-FREE-ALL = 32767;

define constant $PNG-FREE-MUL = 16928;

