module: gettimeofday

define interface
  #include "errno.h",
    import: { "errno" },
    name-mapper: c-to-dylan;
end interface;


