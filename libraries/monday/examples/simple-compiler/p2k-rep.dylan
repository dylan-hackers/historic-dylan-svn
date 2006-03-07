Module: p2k-rep


define abstract class <p2k-denotation> (<object>)
  constant slot denotation-name :: <string>,
    required-init-keyword: name:;
  constant slot denotation-scope-depth :: <integer>,
    required-init-keyword: scope-depth:;
end class;
            
define class <p2k-constant-denotation> (<p2k-denotation>)
  constant slot denotation-constant-value :: <integer>,
    required-init-keyword: constant-value:;
end class;
            
define class <p2k-type-denotation> (<p2k-denotation>)
  constant slot denotation-type-kind :: one-of(#"array", #"integer"),
    required-init-keyword: kind:;
  constant slot denotation-type-base :: false-or(<p2k-type-denotation>)
    = #f, init-keyword: type-base:;
  constant slot denotation-type-array-min :: false-or(<integer>) = #f,
    init-keyword: type-array-min:;
  constant slot denotation-type-array-max :: false-or(<integer>) = #f,
    init-keyword: type-array-max:;
end class;
            
define class <p2k-var-denotation> (<p2k-denotation>)
  constant slot denotation-var-type :: <p2k-type-denotation>,
    required-init-keyword: type:;
  slot denotation-var-storage-base :: <integer>,
    required-init-keyword: storage-base:;
end class;
            
define class <p2k-procedure-denotation> (<p2k-denotation>)
  constant slot denotation-procedure :: false-or(<p2k-procedure>),
    required-init-keyword: procedure:;
end class;
            
define method sizeof-p2k-type
    (type :: <p2k-type-denotation>)
 => (size :: <integer>);
  select(type.denotation-type-kind)
    #"integer" =>
     4;
    #"array" =>
     (type.denotation-type-array-max - type.denotation-type-array-min + 1)
       * sizeof-p2k-type(type.denotation-type-base);
  end select;
end method;
            
define method extend-environment
    (env :: <list>, class :: <class>, #rest keys)
 => (new-env :: <list>);
  pair(apply(make, class, keys), env);
end method;
            
define method locate-denotation
    (env :: <list>, name :: <string>)
 => (denotation :: false-or(<p2k-denotation>));
  if(empty?(env))
    #f;
  elseif(env.head.denotation-name = name)
    env.head;
  else
    locate-denotation(env.tail, name);
  end if;
end method;
            
define constant <p2k-operand>
  = type-union(<integer>, <p2k-instruction>, singleton(#f));
            
define method p2k-constant?
    (operand :: <p2k-operand>)
 => (constant? :: <boolean>);
  instance?(operand, <integer>);
end method;
            
define variable *instruction-count* :: <integer> = 0;
            
define class <p2k-instruction> (<object>)
  constant slot instruction-number :: <integer>
    = (*instruction-count* := *instruction-count* + 1);
  slot instruction-containing-block :: <p2k-basic-block>,
    required-init-keyword: block:;
  constant slot instruction-opcode :: <symbol>,
    required-init-keyword: opcode:;
  slot instruction-operand-x :: <p2k-operand> = #f,
    init-keyword: x:;
  slot instruction-operand-y :: <p2k-operand> = #f,
    init-keyword: y:;
  
slot instruction-replaced-with :: <p2k-operand> = #f;
            
slot instruction-register :: false-or(<integer>) = #f;
            
end class;
            
define method instruction-side-effects?
    (inst :: <p2k-instruction>) => (effects? :: <boolean>);
  select(inst.instruction-opcode)
    #"STORE", #"MOVE", #"END", #"READ", #"WRITE", #"WLN" => #t;
    otherwise => #f;
  end select;
end method;
            
define method instruction-needs-register?
    (inst :: <p2k-instruction>) => (effects? :: <boolean>);
  select(inst.instruction-opcode)
    #"ADDA", #"STORE", #"END", #"WRITE", #"WLN" => #f;
    #"BRA", #"BNE", #"BEQ", #"BLE", #"BLT", #"BGE", #"BGT" => #f;
    otherwise => #t;
  end select;
end method;
            
define method p2k-print-instruction
    (inst :: <p2k-instruction>, stream :: <stream>)
 => ();
  format(stream, "%d: ", inst.instruction-number);
  if(inst.instruction-register)
    format(stream, "R%d := ", inst.instruction-register);
  end if;
  format(stream, "%s ", as(<string>, inst.instruction-opcode));

  if(inst.instruction-operand-x)
    p2k-print-operand(inst.instruction-operand-x, stream);
    format(stream, " ");
  end if;
  if(inst.instruction-operand-y)
    p2k-print-operand(inst.instruction-operand-y, stream);
  end if;
  if(inst.instruction-operand-y == #f & inst.instruction-opcode == #"ADD")
    format(stream, "FP");
  end if;
  if(inst.instruction-replaced-with)
    format(stream, " replaced with ");
    p2k-print-operand(inst.instruction-replaced-with, stream);
  end if;
end method;
            
define method p2k-print-operand
    (op :: <p2k-instruction>, stream :: <stream>)
 => ();
  format(stream, "(%d)", op.instruction-number);
end;
            
define method p2k-print-operand
    (op :: <integer>, stream :: <stream>)
 => ();
  format(stream, "%d", op);
end;
            
define variable *block-count* :: <integer> = 0;
            
define class <p2k-basic-block> (<object>)
  constant slot block-number :: <integer>
    = (*block-count* := *block-count* + 1);
  slot block-phi-instructions :: <stretchy-vector>
    = make(<stretchy-vector>);
  slot block-instructions :: <stretchy-vector>
    = make(<stretchy-vector>);
  slot block-branch-instruction :: false-or(<p2k-instruction>) = #f;
  slot block-fail-block :: false-or(<p2k-basic-block>) = #f;
  slot block-branch-block :: false-or(<p2k-basic-block>) = #f;
  
constant slot block-dominates :: <stretchy-vector>
  = make(<stretchy-vector>);
            
end class;
            
define method p2k-print-block-name
    (blk :: <p2k-basic-block>, stream :: <stream>)
 => ();
  format(stream, "[%d]", blk.block-number);
end method;
            
define method p2k-print-block
    (blk :: <p2k-basic-block>, stream :: <stream>)
 => ();
  format(stream, "Block [%d]", blk.block-number);
  unless(empty?(blk.block-dominates))
    format(stream, " (dominates");
    do(method(ablk)
         format(stream, " ");
         p2k-print-block-name(ablk, stream);
       end, blk.block-dominates);
    format(stream, ")");
  end;
  new-line(stream);
  do(method(inst)
       p2k-print-instruction(inst, stream);
       new-line(stream);
     end, blk.block-phi-instructions);
  do(method(inst)
       p2k-print-instruction(inst, stream);
       new-line(stream);
     end, blk.block-instructions);
  if(blk.block-branch-instruction)
    p2k-print-instruction(blk.block-branch-instruction, stream);
    p2k-print-block-name(blk.block-branch-block, stream);
    new-line(stream);
  end if;
  if(blk.block-fail-block)
    format(stream, "==> ");
    p2k-print-block-name(blk.block-fail-block, stream);
    new-line(stream);
  end if;
  new-line(stream);

  do(method(ablk)
       p2k-print-block(ablk, stream);
     end, blk.block-dominates);
end method;
            
define constant <p2k-value-table> = <object-table>;
            
define method copy-value-table
    (from :: <p2k-value-table>)
 => (to :: <p2k-value-table>)
  let to = make(<p2k-value-table>);
  for(value keyed-by key in from)
    to[key] := value;
  end for;
  to;
end method;
            
define method join-value-tables!
    (left :: <p2k-value-table>, right :: <p2k-value-table>,
     join :: <p2k-basic-block>)
 => ();
  for(left-val keyed-by key in left)
    let right-val = element(right, key, default: #f);
    if(right-val)
      if(left-val ~= right-val)
        let phi = make(<p2k-instruction>, block: join, opcode: #"PHI",
                       x: left-val, y: right-val);
        add!(join.block-phi-instructions, phi);
        left[key] := phi;
      end if;
    else
      remove-key!(left, key);
    end if;
  end for;
end method;
            
define class <p2k-procedure> (<object>)
  constant slot procedure-name :: <string>,
    required-init-keyword: name:;
  slot procedure-entry-block :: <p2k-basic-block>;
  slot procedure-environment :: <list> = #();
  slot procedure-storage :: <integer> = 0;
end class;
            
define method p2k-print-procedure
    (procedure :: <p2k-procedure>, stream :: <stream>)
 => ();
  format(stream, "PROCEDURE %s: %d bytes of local storage, entry at ",
                  procedure.procedure-name,
                  procedure.procedure-storage);
  p2k-print-block-name(procedure.procedure-entry-block, stream);
  new-line(stream);

  p2k-print-block(procedure.procedure-entry-block, stream);
end method;
            
