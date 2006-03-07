Module: Dylan-user


define library simple-compiler
  use common-Dylan;
  
use io;
export p2k-lexer;
          
export p2k-rep;
          
export p2k-parser;
          
export p2k-cse;
          
export p2k-regalloc;
          
use io;
export p2k-test;
          
end library;
          
define module p2k-lexer
  use common-Dylan;
  use streams;
  
export <p2k-lexer>;
          
export match;
export token-value;
          
export expect;
          
end module;
          
define module p2k-rep
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format;
  
export <p2k-denotation>, denotation-name, denotation-scope-depth;
export <p2k-constant-denotation>, denotation-constant-value;
export <p2k-type-denotation>, denotation-type-kind, denotation-type-base;
export denotation-type-array-min, denotation-type-array-max;
export <p2k-var-denotation>, denotation-var-type;
export denotation-var-storage-base, denotation-var-storage-base-setter;
export <p2k-procedure-denotation>, denotation-procedure;
            
export sizeof-p2k-type;
            
export extend-environment, locate-denotation;
            
export <p2k-operand>, p2k-constant?;
export <p2k-instruction>;
export instruction-number, instruction-containing-block, instruction-opcode;
export instruction-operand-x, instruction-operand-x-setter;
export instruction-operand-y, instruction-operand-y-setter;
            
export instruction-replaced-with, instruction-replaced-with-setter;
            
export instruction-register, instruction-register-setter;
            
export instruction-side-effects?, instruction-needs-register?;
            
export p2k-print-instruction, p2k-print-operand;
            
export <p2k-basic-block>;
export block-number;
export block-phi-instructions, block-phi-instructions-setter;
export block-instructions, block-instructions-setter;
export block-branch-instruction, block-branch-instruction-setter;
export block-fail-block, block-fail-block-setter;
export block-branch-block, block-branch-block-setter;
export block-dominates;
            
export p2k-print-block-name, p2k-print-block;
            
export <p2k-value-table>;
export copy-value-table, join-value-tables!;
            
export <p2k-procedure>;
export procedure-name, procedure-entry-block, procedure-entry-block-setter;
export procedure-environment, procedure-environment-setter;
export procedure-storage, procedure-storage-setter;
            
export p2k-print-procedure;
            
end module;
          
define module p2k-parser
  use common-dylan;
  use p2k-lexer;
  use p2k-rep;
  
export p2k-parse-program;
          
end module;
          
define module p2k-cse
  use common-dylan;
  use p2k-rep;
  
export p2k-cse-optimize;
          
end module;
          
define module p2k-regalloc
  use common-dylan, exclude: { format-to-string };
  use p2k-rep;     // FIXME
  use streams;     // FIXME
  use standard-io; // FIXME
  use format;
  export p2k-regalloc, p2k-generate-moves;
end module;
          
define module p2k-test
  use common-dylan, exclude: { format-to-string };
  use streams;
  use standard-io;
  use format;
  use pprint;
  use print;
  use p2k-lexer;
  use p2k-rep;
  use p2k-parser;
  use p2k-cse;
  use p2k-regalloc;
end module;
          
