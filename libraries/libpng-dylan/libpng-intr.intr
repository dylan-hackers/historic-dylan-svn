module: libpng

define interface
  #include "png.h",
    import: all,
    name-mapper: c-to-dylan,
    rename: { "Byte" => <b-yte> },
    equate: { "_IO_lock_t*" => <machine-pointer>},
    exclude: {"_IO_lock_t", 
              "png_sig_bytes", 
              "png_write_destroy_info", 
              "png_set_sCAL_s" };
  function "png_create_read_struct",
    map-argument: {1 => <byte-string>},
    equate-argument:  {1 => <c-string>};
end interface;
