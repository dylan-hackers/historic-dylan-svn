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
end module;

define module alterna-robber
  use common-dylan;
  use simple-random;
  use world;
end module;

define module bruce-robber
  use common-dylan;
  use simple-random;
  use world;

  export <bruce-robber>;
end module;

define module cheating-robber
  use common-dylan;
  use simple-random;
  use world;
  use bruce-robber;
end module;

define module robber
  use common-dylan;
  use standard-io;
  use world, import: {drive-agent};
end module;
