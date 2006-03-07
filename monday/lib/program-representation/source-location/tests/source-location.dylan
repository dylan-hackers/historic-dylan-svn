Module: source-location-test-suite

define source-location class-test <source-location> ()
  // ---### Fill this in.
end;

define source-location class-test <unknown-source-location> ()
  // ---### Fill this in.
end;

define source-location class-test <union-source-location> ()
  // ---### Fill this in.
end;

define sideways method make-test-instance
    (class == <union-source-location>) => (object)
  make(<union-source-location>,
       head: make-test-instance(<file-source-location>),
       tail: make-test-instance(<file-source-location>));
end method;

define source-location function-test source-head ()
  // ---### Fill this in.
end;

define source-location function-test source-tail ()
  // ---### Fill this in.
end;

define source-location function-test merge-source-locations ()
  // ---### Fill this in.
end;

define source-location function-test source-location ()
  // ---### Fill this in.
end;

define source-location class-test <source-location-mixin> ()
  // ---### Fill this in.
end;

define source-location class-test <file-source-location> ()
  // ---### Fill this in.
end;

define sideways method make-test-instance
    (class == <file-source-location>) => (object)
  make(<file-source-location>,
       file: "test-file",
       start-line: 1, start-column: 1,
       end-line: 44, end-column: 28);
end method make-test-instance;

define source-location function-test source-file ()
  // ---### Fill this in.
end;

define source-location function-test source-start-line ()
  // ---### Fill this in.
end;

define source-location function-test source-start-column ()
  // ---### Fill this in.
end;

define source-location function-test source-end-line ()
  // ---### Fill this in.
end;

define source-location function-test source-end-column ()
  // ---### Fill this in.
end;
