module: dylan-user

define library cop
  use common-dylan;
  use io;
  use system;
  use collection-extensions;
  use world;
end library;


define module predicting-cop
  use common-dylan;
  use simple-random;
  use world;
  
  export <predicting-cop>,
    probability-map,
    planned-moves,
    plan-ranking,
    plan-ranking-setter,
    accusations,
    accusations-setter,
    all-moves,
    all-moves-setter,
    invalid-moves;
end module;


define module stupid-predicting-cop
  use common-dylan;
  use simple-random;
  use world;
  use predicting-cop;
  
  export <stupid-predicting-cop>;
end module;

define module dirty-cop
  use common-dylan;
  use simple-random;
  use world;
  use predicting-cop;
  use stupid-predicting-cop;

  export <dirty-cop>;
end module;

define module cop
  use common-dylan;
  use standard-io;
  use world, import: {drive-agent};

  use predicting-cop;
  use stupid-predicting-cop;
  use dirty-cop;
end module;
