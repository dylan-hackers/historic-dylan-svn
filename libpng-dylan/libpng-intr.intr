module: libpng-internal

define interface
  #include "png.h",
    import: all,
    name-mapper: c-to-dylan,
    rename: { "Byte" => <b-yte> },
    exclude: {"png_sig_bytes", 
              "png_write_destroy_info", 
              "png_set_sCAL_s" };
  function "png_create_read_struct",
    map-argument: {1 => <byte-string>},
    equate-argument:  {1 => <c-string>};
end interface;
