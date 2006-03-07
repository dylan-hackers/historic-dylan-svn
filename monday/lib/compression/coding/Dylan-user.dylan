Module: Dylan-user


define library coding
  use common-dylan;

  
export coding-base;
          
end library;
          
define module coding-base
  use common-dylan;
  export
    <encoder>,
    encoder-output-object,
    encoder-output-function,
    <decoder>,
    decode-bits;
end module;
          
