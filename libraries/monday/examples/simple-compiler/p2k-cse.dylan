Module: p2k-cse


define method p2k-cse-optimize
    (procedure :: <p2k-procedure>)
 => ();
  
local
  method cse-optimize-block
      (blk :: <p2k-basic-block>, instruction-table :: <object-table>)
   => ();
    for(inst in blk.block-phi-instructions)
      replace-operands(inst);
      cse-optimize-instruction(inst, instruction-table);
    end;
    for(inst in blk.block-instructions)
      replace-operands(inst);
      constant-fold-instruction(inst);
      unless(inst.instruction-replaced-with)
        cse-optimize-instruction(inst, instruction-table);
      end;
    end for;
    
for(child-blk in blk.block-dominates)
  let child-instruction-table = make(<object-table>);
  for(instrs keyed-by opcode in instruction-table)
    unless(opcode == #"PHI") child-instruction-table[opcode] := instrs; end;
  end for;
  cse-optimize-block(child-blk, child-instruction-table);
end for;
          
  end,
          
  method replace-operands
      (inst :: <p2k-instruction>)
   => ();
    if(instance?(inst.instruction-operand-x, <p2k-instruction>)
       & inst.instruction-operand-x.instruction-replaced-with)
      inst.instruction-operand-x
        := inst.instruction-operand-x.instruction-replaced-with;
    end if;
    if(instance?(inst.instruction-operand-y, <p2k-instruction>)
       & inst.instruction-operand-y.instruction-replaced-with)
      inst.instruction-operand-y
        := inst.instruction-operand-y.instruction-replaced-with;
    end if;
  end,
            
  method constant-fold-instruction
      (inst :: <p2k-instruction>)
   => ();
    let x = inst.instruction-operand-x;
    let y = inst.instruction-operand-y;
    let x-integer? = instance?(x, <integer>);
    let y-integer? = instance?(y, <integer>);
    select(inst.instruction-opcode)
      #"NEG" =>
        if(x-integer?)
          inst.instruction-replaced-with := - x;
        end if;
      #"ADD" =>
        if(x-integer? & y-integer?)
          inst.instruction-replaced-with := x + y;
        end if;
      #"SUB" =>
        if(x-integer? & y-integer?)
          inst.instruction-replaced-with := x - y;
        end if;
      #"MUL" =>
        if(x-integer? & y-integer?)
          inst.instruction-replaced-with := x * y;
        end if;
      #"DIV" =>
        if(x-integer? & y-integer?)
          inst.instruction-replaced-with := floor/(x, y);
        end if;
      #"MOD" =>
        if(x-integer? & y-integer?)
          inst.instruction-replaced-with := modulo(x, y);
        end if;
      otherwise =>
        #f;
    end select;
  end,
            
  method cse-optimize-instruction
      (inst :: <p2k-instruction>, instruction-table :: <object-table>)
   => ();
    select(inst.instruction-opcode)
      #"READ", #"WRITE", #"WLN", #"ADDA", #"LOAD", #"STORE" =>
        #f;
      otherwise =>
        for(other in element(instruction-table, inst.instruction-opcode,
                             default: #()),
            until:
              inst.instruction-operand-x == other.instruction-operand-x
                & inst.instruction-operand-y == other.instruction-operand-y
                & (inst.instruction-replaced-with := other))
        end for;
        unless(inst.instruction-replaced-with)
          element(instruction-table, inst.instruction-opcode)
            := pair(inst, element(instruction-table, inst.instruction-opcode,
                                  default: #()));
        end;
    end select;
  end;
            
  let instruction-table = make(<object-table>);
  cse-optimize-block(procedure.procedure-entry-block, instruction-table);
end method;
          
