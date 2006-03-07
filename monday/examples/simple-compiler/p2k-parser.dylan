Module: p2k-parser


define method p2k-parse-program
    (lexer :: <p2k-lexer>)
 => (program :: <p2k-procedure>)
  unless(match(lexer, #"IDENT", value: "PROGRAM"))
    error("program must begin with PROGRAM");
  end;

  unless(match(lexer, #"IDENT") & instance?(lexer.token-value, <string>))
    error("program name must be an identifier");
  end;

  let program = make(<p2k-procedure>, name: lexer.token-value);
  expect(lexer, #";");

  
program.procedure-environment :=
  extend-environment(#(), <p2k-type-denotation>,
                     name: "INTEGER", scope-depth: 0, kind: #"integer");
program.procedure-environment :=
  extend-environment(program.procedure-environment,
                     <p2k-procedure-denotation>,
                     name: "ReadInt", scope-depth: 0, procedure: #f);
program.procedure-environment :=
  extend-environment(program.procedure-environment,
                     <p2k-procedure-denotation>,
                     name: "WriteInt", scope-depth: 0, procedure: #f);
program.procedure-environment :=
  extend-environment(program.procedure-environment,
                     <p2k-procedure-denotation>,
                     name: "WriteLn", scope-depth: 0, procedure: #f);
          

  p2k-parse-declarations(lexer, program, 0);
  if(match(lexer, #"IDENT", value: "BEGIN"))
    let entry-block :: <p2k-basic-block> = make(<p2k-basic-block>);
    let value-table :: <p2k-value-table> = make(<p2k-value-table>);
    program.procedure-entry-block := entry-block;
    let end-block
      = p2k-parse-statement-sequence(lexer, program, value-table, entry-block);
    let end-instruction
      = make(<p2k-instruction>, block: end-block, opcode: #"END");
    add!(end-block.block-instructions, end-instruction);
  end if;
  expect(lexer, #"IDENT", value: "END");
  expect(lexer, #".");
  program;
end method;
          
define method p2k-parse-declarations
    (lexer :: <p2k-lexer>, procedure :: <p2k-procedure>,
     level :: <integer>)
 => ();
  if(match(lexer, #"IDENT", value: "CONST"))
    p2k-parse-const-section(lexer, procedure, level);
    p2k-parse-declarations(lexer, procedure, level);
  elseif(match(lexer, #"IDENT", value: "TYPE"))
    p2k-parse-type-section(lexer, procedure, level);
    p2k-parse-declarations(lexer, procedure, level);
  elseif(match(lexer, #"IDENT", value: "VAR"))
    p2k-parse-var-section(lexer, procedure, level);
    p2k-parse-declarations(lexer, procedure, level);
  elseif(match(lexer, #"IDENT", value: "PROCEDURE"))
    p2k-parse-procedure(lexer, procedure, level);
    p2k-parse-declarations(lexer, procedure, level);
  end if;
end method;
            
define method p2k-parse-const-section
    (lexer :: <p2k-lexer>, procedure :: <p2k-procedure>,
     level :: <integer>)
 => ();
  while(match(lexer, #"IDENT", consume: #f)
        & lexer.token-value ~= "CONST"
        & lexer.token-value ~= "TYPE"
        & lexer.token-value ~= "VAR"
        & lexer.token-value ~= "PROCEDURE"
        & lexer.token-value ~= "BEGIN")
    match(lexer, #"IDENT", consume: #t);
    let name = lexer.token-value;
    expect(lexer, #"=");
    expect(lexer, #"NUMBER");
    procedure.procedure-environment
      := extend-environment(procedure.procedure-environment,
                            <p2k-constant-denotation>,
                            name: name, scope-depth: level,
                            constant-value: lexer.token-value);
    expect(lexer, #";");
  end while;
end method;
              
define method p2k-parse-type-section
    (lexer :: <p2k-lexer>, procedure :: <p2k-procedure>,
     level :: <integer>)
 => ();
  while(match(lexer, #"IDENT", consume: #f)
        & lexer.token-value ~= "CONST"
        & lexer.token-value ~= "TYPE"
        & lexer.token-value ~= "VAR"
        & lexer.token-value ~= "PROCEDURE"
        & lexer.token-value ~= "BEGIN")
    match(lexer, #"IDENT", consume: #t);
    let name = lexer.token-value;
    expect(lexer, #"=");
    let new-type = p2k-parse-type(lexer, procedure.procedure-environment);    
    expect(lexer, #";");
    procedure.procedure-environment
      := extend-environment(procedure.procedure-environment,
                            <p2k-type-denotation>,
                            name: name, scope-depth: level,
                            kind: new-type.denotation-type-kind,
                            type-base: new-type.denotation-type-base,
                            type-array-min: new-type.denotation-type-array-min,
                            type-array-max: new-type.denotation-type-array-max);                          
  end while;
end method;
              
define method p2k-parse-type
    (lexer :: <p2k-lexer>, environment :: <list>)
 => (type :: <p2k-type-denotation>);
  expect(lexer, #"IDENT");
  if(lexer.token-value = "ARRAY")
    expect(lexer, #"[");
    expect(lexer, #"NUMBER");
    let min = lexer.token-value;
    expect(lexer, #"..");
    expect(lexer, #"NUMBER");
    let max = lexer.token-value;
    if(min > max)
      error("lower array bound must be less than or equal to upper bound");
    end;
    expect(lexer, #"]");
    expect(lexer, #"IDENT", value: "OF");
    let base = p2k-parse-type(lexer, environment);
    make(<p2k-type-denotation>, name: "ARRAY", scope-depth: -1,
         kind: #"array", type-base: base,
         type-array-min: min, type-array-max: max);
  else
    let denotation = locate-denotation(environment, lexer.token-value);
    unless(denotation & instance?(denotation, <p2k-type-denotation>))
      error("%s: illegal type name", lexer.token-value);
    end;
    denotation;
  end if;
end method;
              
define method p2k-parse-var-section
    (lexer :: <p2k-lexer>, procedure :: <p2k-procedure>,
     level :: <integer>)
 => ();
  while(match(lexer, #"IDENT", consume: #f)
        & lexer.token-value ~= "CONST"
        & lexer.token-value ~= "TYPE"
        & lexer.token-value ~= "VAR"
        & lexer.token-value ~= "PROCEDURE"
        & lexer.token-value ~= "BEGIN")
    match(lexer, #"IDENT", consume: #t);
    let names :: <list> = list(lexer.token-value);
    while(match(lexer, #","))
      expect(lexer, #"IDENT");
      names := add(names, lexer.token-value);
    end while;
    expect(lexer, #":");
    let type = p2k-parse-type(lexer, procedure.procedure-environment);
    expect(lexer, #";");
    for(name in names)
      procedure.procedure-storage
        := procedure.procedure-storage + sizeof-p2k-type(type);
      procedure.procedure-environment
        := extend-environment(procedure.procedure-environment,
                              <p2k-var-denotation>,
                              name: name, scope-depth: level,
                              type: type,
                              storage-base: - procedure.procedure-storage);
    end for;
  end while;
end method;
              
define method p2k-parse-procedure
    (lexer :: <p2k-lexer>, procedure :: <p2k-procedure>,
     level :: <integer>)
 => ();
  error("This compiler doesn't support PROCEDURE declarations (yet)");
end method;
              
define method p2k-parse-statement-sequence
    (lexer :: <p2k-lexer>, procedure :: <p2k-procedure>,
     value-table :: <p2k-value-table>, blk :: <p2k-basic-block>)
 => (blk :: <p2k-basic-block>);
  if(match(lexer, #"IDENT", value: "IF"))
    
let then-blk = make(<p2k-basic-block>);
add!(blk.block-dominates, then-blk);
let else-blk = make(<p2k-basic-block>);
add!(blk.block-dominates, else-blk);
let join-blk = make(<p2k-basic-block>);
add!(blk.block-dominates, join-blk);

let (result, branchop)
  = p2k-parse-conditional-expression(lexer, procedure, value-table, blk);
blk.block-branch-instruction
  := make(<p2k-instruction>, block: blk, opcode: branchop, x: result);
blk.block-fail-block := then-blk;
blk.block-branch-block := else-blk;

let else-value-table = copy-value-table(value-table);
expect(lexer, #"IDENT", value: "THEN");
let then-end-blk
  = p2k-parse-statement-sequence(lexer, procedure, value-table, then-blk);
then-end-blk.block-branch-instruction
  := make(<p2k-instruction>, block: then-end-blk, opcode: #"BRA");
then-end-blk.block-branch-block := join-blk;

if(match(lexer, #"IDENT", value: "ELSE"))
  blk.block-branch-block := else-blk;
  let else-end-blk
    = p2k-parse-statement-sequence(lexer, procedure, else-value-table,
                                   else-blk);
  else-end-blk.block-fail-block := join-blk;
else
  else-blk.block-fail-block := join-blk;
end if;


expect(lexer, #"IDENT", value: "END");

join-value-tables!(value-table, else-value-table, join-blk);
blk := join-blk;
              
  elseif(match(lexer, #"IDENT", value: "WHILE"))
    
let new-blk = make(<p2k-basic-block>);
blk.block-fail-block := new-blk;
add!(blk.block-dominates, new-blk);
blk := new-blk;

let body-blk = make(<p2k-basic-block>);
add!(blk.block-dominates, body-blk);


for(val keyed-by key in value-table)
  value-table[key] := make(<p2k-instruction>, block: blk, opcode: #"PHI",
                           x: val, y: val);
end for;
              
let (result, branchop)
  = p2k-parse-conditional-expression(lexer, procedure, value-table, blk);
blk.block-branch-instruction
  := make(<p2k-instruction>, block: blk, opcode: branchop, x: result);
blk.block-fail-block := body-blk;

expect(lexer, #"IDENT", value: "DO");
let body-value-table = copy-value-table(value-table);
let body-end-blk
  = p2k-parse-statement-sequence(lexer, procedure, body-value-table, body-blk);
body-end-blk.block-branch-instruction
  := make(<p2k-instruction>, block: body-end-blk, opcode: #"BRA");
body-end-blk.block-branch-block := blk;

expect(lexer, #"IDENT", value: "END");

let exit-blk = make(<p2k-basic-block>);
add!(blk.block-dominates, exit-blk);
blk.block-branch-block := exit-blk;


let substitutions = make(<object-table>);
for(val keyed-by key in value-table)
  if(val == element(body-value-table, key))
    element(substitutions, val) := val.instruction-operand-x;
    element(value-table, key) := val.instruction-operand-x;
  else
    val.instruction-operand-x := element(body-value-table, key);
    add!(blk.block-phi-instructions, val);
  end if;
end for;
              
local
  method substitute-instruction(inst :: <p2k-instruction>) => ();
    inst.instruction-operand-x
      := element(substitutions, inst.instruction-operand-x,
                                default: inst.instruction-operand-x);
    inst.instruction-operand-y
      := element(substitutions, inst.instruction-operand-y,
                                default: inst.instruction-operand-y);
  end method,
  method substitute-block(blk :: <p2k-basic-block>) => ();
    do(substitute-instruction, blk.block-phi-instructions);
    do(substitute-instruction, blk.block-instructions);
    if(blk.block-branch-instruction)
      substitute-instruction(blk.block-branch-instruction);
    end if;
    do(substitute-block, blk.block-dominates);
  end method;
substitute-block(blk);
              

blk := exit-blk;
              
  else
    expect(lexer, #"IDENT");
    let name = lexer.token-value;
    let denotation :: false-or(<p2k-denotation>)
      = locate-denotation(procedure.procedure-environment, name);
    select(denotation by instance?)
      <p2k-procedure-denotation> =>
        
select(name by \=)
  
"ReadInt" =>
  expect(lexer, #"(");
  expect(lexer, #"IDENT");
  let denotation :: false-or(<p2k-denotation>)
    = locate-denotation(procedure.procedure-environment, lexer.token-value);
  if(~instance?(denotation, <p2k-var-denotation>))
    error("expected a variable name, got identifier '%s'", lexer.token-value);
  end if;
  let offset
    = p2k-parse-selector(lexer, procedure, value-table, blk, denotation);
  expect(lexer, #")");

  let val = make(<p2k-instruction>, block: blk, opcode: #"READ");
  add!(blk.block-instructions, val);
  p2k-assign(value-table, blk, denotation, offset, val);
              
"WriteInt" =>
  expect(lexer, #"(");
  let val = p2k-parse-simple-expression(lexer, procedure, value-table, blk);
  expect(lexer, #")");
  let wri = make(<p2k-instruction>, block: blk, opcode: #"WRITE",
                 x: val);
  add!(blk.block-instructions, wri);
              
"WriteLn" =>
  expect(lexer, #"(");
  expect(lexer, #")");
  let wri = make(<p2k-instruction>, block: blk, opcode: #"WLN");
  add!(blk.block-instructions, wri);
              
  otherwise =>
    error("hey... how'd %s get in here?", name);
end select;
              
      <p2k-var-denotation> =>
        
let offset
  = p2k-parse-selector(lexer, procedure, value-table, blk, denotation);

expect(lexer, #":=");
let val = p2k-parse-simple-expression(lexer, procedure, value-table, blk);
p2k-assign(value-table, blk, denotation, offset, val);
              
      otherwise =>
        error("the identifier '%s' is unexpected here", name);
    end select;
  end if;
  if(match(lexer, #";")
     & ~match(lexer, #"IDENT", value: "END", consume: #f)
     & ~match(lexer, #"IDENT", value: "ELSE", consume: #f))
    p2k-parse-statement-sequence(lexer, procedure, value-table, blk);
  else
    blk;
  end if;
end method;
            
define method p2k-parse-conditional-expression
    (lexer :: <p2k-lexer>, procedure :: <p2k-procedure>,
     value-table :: <p2k-value-table>, blk :: <p2k-basic-block>)
 => (result :: <p2k-operand>, operator :: <symbol>);
  let left = p2k-parse-simple-expression(lexer, procedure, value-table, blk);

  let op = #f;
  if(match(lexer, #"="))
     op := #"BNE";
  elseif(match(lexer, #"<>"))
    op := #"BEQ";
  elseif(match(lexer, #"<"))
    op := #"BGE";
  elseif(match(lexer, #"<="))
    op := #"BGT";
  elseif(match(lexer, #">"))
    op := #"BLE";
  elseif(match(lexer, #">="))
    op := #"BLT";
  else
    error("conditional operator expected");
  end if;

  let right = p2k-parse-simple-expression(lexer, procedure, value-table, blk);
  let cmp = make(<p2k-instruction>, block: blk, opcode: #"CMP",
                 x: left, y: right);
  add!(blk.block-instructions, cmp);
  values(cmp, op);
end method;
            
define method p2k-parse-simple-expression
    (lexer :: <p2k-lexer>, procedure :: <p2k-procedure>,
     value-table :: <p2k-value-table>, blk :: <p2k-basic-block>)
 => (result :: <p2k-operand>);
  let neg = match(lexer, #"-");
  unless(neg)
    match(lexer, #"+");
  end;
  let left = p2k-parse-term(lexer, procedure, value-table, blk);
  if(neg)
    left := make(<p2k-instruction>, block: blk, opcode: #"NEG", x: left);
    add!(blk.block-instructions, left);
  end;

  let op = #f;
  while(if(match(lexer, #"+")) op := #"ADD";
        elseif(match(lexer, #"-")) op := #"SUB"; else #f; end if)
    let right = p2k-parse-term(lexer, procedure, value-table, blk);
    left := make(<p2k-instruction>, block: blk, opcode: op,
                 x: left, y: right);
    add!(blk.block-instructions, left);
  end while;
  left;
end method;
            
define method p2k-parse-term
    (lexer :: <p2k-lexer>, procedure :: <p2k-procedure>,
     value-table :: <p2k-value-table>, blk :: <p2k-basic-block>)
 => (result :: <p2k-operand>);
  let left = p2k-parse-factor(lexer, procedure, value-table, blk);

  let op = #f;
  while(if(match(lexer, #"*")) op := #"MUL";
        elseif(match(lexer, #"IDENT", value: "DIV")) op := #"DIV";
        elseif(match(lexer, #"IDENT", value: "MOD")) op := #"MOD";
        else #f; end if)
    let right = p2k-parse-factor(lexer, procedure, value-table, blk);
    left := make(<p2k-instruction>, block: blk, opcode: op,
                 x: left, y: right);
    add!(blk.block-instructions, left);
  end while;
  left;
end method;
            
define method p2k-parse-factor
    (lexer :: <p2k-lexer>, procedure :: <p2k-procedure>,
     value-table :: <p2k-value-table>, blk :: <p2k-basic-block>)
 => (result :: <p2k-operand>);
  if(match(lexer, #"("))
    let result
      = p2k-parse-simple-expression(lexer, procedure, value-table, blk);
    expect(lexer, #")");
    result;
  elseif(match(lexer, #"NUMBER"))
    lexer.token-value;
  elseif(match(lexer, #"IDENT"))
    let name = lexer.token-value;
    
let denotation :: false-or(<p2k-denotation>)
  = locate-denotation(procedure.procedure-environment, name);
select(denotation by instance?)
  <p2k-constant-denotation> =>
    denotation.denotation-constant-value;
  <p2k-var-denotation> =>
    let offset
      = p2k-parse-selector(lexer, procedure, value-table, blk, denotation);
    if(offset)
      let base = make(<p2k-instruction>, block: blk, opcode: #"ADD",
                      x: denotation.denotation-var-storage-base,
                      y: #f); // #f is FP
      add!(blk.block-instructions, base);
      let addr = make(<p2k-instruction>, block: blk, opcode: #"ADDA",
                      x: base, y: offset);
      add!(blk.block-instructions, addr);
      let load = make(<p2k-instruction>, block: blk, opcode: #"LOAD",
                      y: addr);
      add!(blk.block-instructions, load);
      load;
    else
      let val = element(value-table, denotation, default: #f);
      if(val)
        val;
      else
        error("the variable '%s' is uninitialized at this point", name);
      end if;
    end if;
  otherwise =>
    error("the identifier '%s' is unexpected here", name);
end select;
            
  else
    error("expression syntax error (spare me, okay?)");
  end if;
end method;
            
define method p2k-parse-selector
    (lexer :: <p2k-lexer>, procedure :: <p2k-procedure>,
     value-table :: <p2k-value-table>, blk :: <p2k-basic-block>,
     denotation :: <p2k-var-denotation>)
 => (result :: <p2k-operand>);
  let type = denotation.denotation-var-type;
  let offset = #f;
  while(match(lexer, #"["))
    if(type.denotation-type-kind ~== #"array")
      error("attempt to subscript a non-array");
    end;
    let index
      = p2k-parse-simple-expression(lexer, procedure, value-table, blk);
    expect(lexer, #"]");

    
if(type.denotation-type-array-min ~= 0)
  index := make(<p2k-instruction>, block: blk, opcode: #"SUB",
                x: index, y: type.denotation-type-array-min);
  add!(blk.block-instructions, index);
end if;
let rel-offset = make(<p2k-instruction>, block: blk, opcode: #"MUL",
                      x: index,
                      y: sizeof-p2k-type(type.denotation-type-base));
add!(blk.block-instructions, rel-offset);
                
    if(offset)
      offset := make(<p2k-instruction>, block: blk, opcode: #"ADD",
                     x: offset, y: rel-offset);
      add!(blk.block-instructions, offset);
    else
      offset := rel-offset;
    end if;

    type := type.denotation-type-base;
  end while;
  if(type.denotation-type-kind == #"array")
    error("can't treat arrays as values");
  end;
  offset;
end method;
            
define method p2k-assign
    (value-table :: <p2k-value-table>, blk :: <p2k-basic-block>,
     denotation :: <p2k-var-denotation>, offset :: <p2k-operand>,
     value :: <p2k-operand>)
 => ();
  if(offset)
    let base = make(<p2k-instruction>, block: blk, opcode: #"ADD",
                    x: denotation.denotation-var-storage-base,
                    y: #f); // #f is FP
    add!(blk.block-instructions, base);
    let addr = make(<p2k-instruction>, block: blk, opcode: #"ADDA",
                    x: base, y: offset);
    add!(blk.block-instructions, addr);
    let store = make(<p2k-instruction>, block: blk, opcode: #"STORE",
                     x: value, y: addr);
    add!(blk.block-instructions, store);
  else
    value-table[denotation] := value;
  end if;
end method;
              
