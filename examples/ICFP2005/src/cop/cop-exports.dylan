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
  
  export <predicting-cop>, probability-map;
end module;


define module stupid-predicting-cop
  use common-dylan;
  use simple-random;
  use world;
  use predicting-cop;
  
  export <stupid-predicting-cop>;
end module;



define module cop
  use common-dylan;
  //use format-out;
  use standard-io;
  //use streams;
  //use simple-random;
  //use format, import: { format };
//  use subseq;

  use world, import: {drive-agent};
  use predicting-cop;
  use stupid-predicting-cop;
end module;
