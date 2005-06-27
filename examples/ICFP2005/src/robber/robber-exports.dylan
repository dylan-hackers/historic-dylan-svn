module: dylan-user

define library robber
  use common-dylan;
  use io;

  use world;
end library;

define module random-walk-robber
  use common-dylan;
  use simple-random;
  use world;
  
  export <random-walk-robber>;
end module;

define module alterna-robber
  use common-dylan;
  use simple-random;
  use world;
  
  export <alterna-robber>;
end module;

define module bruce-robber
  use common-dylan;
  use simple-random;
  use world;
  
  export <bruce-robber>;
end module;

define module robber
  use common-dylan;
  use standard-io;
  use world, import: {drive-agent};

  use random-walk-robber;
  use alterna-robber;
  use bruce-robber;
end module;
