Module: dylan-user

define library web-objects-example
  use common-dylan,
    import: { common-dylan };
  use io,
    import: { streams };
  use koala,
    import: { dsp };
end;

define module web-objects-example
  use common-dylan,
    exclude: { split };
  use streams,
    import: { write };
  use dsp;           // from the koala library
end;


