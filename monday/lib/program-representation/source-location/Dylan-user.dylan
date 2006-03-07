Module: Dylan-user


define library source-location
  use common-dylan;
  use system;
  export source-location;
  export source-location-rangemap;
  export source-location-conditions;
end library;
          
define module source-location
  use common-dylan;
  use locators;
  
export
  <source-location>,
  <unknown-source-location>,
  <union-source-location>,
  source-head, source-tail,
  merge-source-locations;
          
export
  source-location,
  <source-location-mixin>;
          
export
  <file-source-location>,
  source-file,
  source-start-line,
  source-start-column,
  source-end-line,
  source-end-column;
          
end module;
          
define module source-location-rangemap
  use common-dylan;
  use locators;
  use source-location;
  
export
  <source-location-rangemap>,
  rangemap-one-to-one?, rangemap-one-to-one?-setter,
  range-source-location,
  rangemap-add-line,
  rangemap-add-line-file;
          
end module;
          
define module source-location-conditions
  use common-dylan;
  use source-location;
  
export
  <source-condition>,
  <source-warning>,
  <source-error>,
  source-warning,
  source-error;
          
end module;
          
