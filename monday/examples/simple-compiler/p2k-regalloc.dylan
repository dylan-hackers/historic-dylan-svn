Module: p2k-regalloc


define method p2k-regalloc
    (procedure :: <p2k-procedure>, K :: <integer>)
 => ();
  
let edges :: <undirected-edge-table> = make(<undirected-edge-table>);
let adjacency-list :: <object-table> = make(<object-table>);
let degree :: <object-table> = make(<object-table>);
            
let move-list :: <object-table> = make(<object-table>);
let worklist-moves :: <deque> = make(<deque>);
let move-kind  :: <object-table> = make(<object-table>);
            
let spill-worklist :: <deque> = make(<deque>);
let freeze-worklist :: <deque> = make(<deque>);
let simplify-worklist :: <deque> = make(<deque>);
let worklist-kind  :: <object-table> = make(<object-table>);
            
let select-stack :: <deque> = make(<deque>);
            
let alias :: <object-table> = make(<object-table>);
let coalesced :: <deque> = make(<deque>);
            
let spilled :: <deque> = make(<deque>);
            
  local
    
  method add-edge(u :: <p2k-instruction>, v :: <p2k-instruction>) => ();
    let uv = pair(u, v);
    unless(u == v | element(edges, uv, default: #f))
      element(edges, uv) := #t;
      element(adjacency-list, u)
        := add(element(adjacency-list, u, default: #()), v);
      element(degree, u) := element(degree, u, default: 0) + 1;
      element(adjacency-list, v)
        := add(element(adjacency-list, v, default: #()), u);
      element(degree, v) := element(degree, v, default: 0) + 1;
    end;
  end method,
            
  method construct-liveness-adjacency(entry-blk :: <p2k-basic-block>) => ();
    let liveness-worklist :: <deque> = make(<deque>);
    let live-at-head-branch :: <object-table>
      = make(<object-table>);
    let branch-predecessor :: <object-table>
      = make(<object-table>);
    let live-at-head-fail :: <object-table>
      = make(<object-table>);
    let fail-predecessor :: <object-table>
      = make(<object-table>);
    local
      
  method compute-liveness-dependency(blk :: <p2k-basic-block>) => ();
    if(blk.block-branch-block)
      branch-predecessor[blk.block-branch-block] := blk;
    end if;
    if(blk.block-fail-block)
      fail-predecessor[blk.block-fail-block] := blk;      
    end if;
    if(~blk.block-branch-block & ~blk.block-fail-block)
      push-last(liveness-worklist, blk);
    end if;
    do(compute-liveness-dependency, blk.block-dominates);
  end;
            
    compute-liveness-dependency(entry-blk);
    while(~empty?(liveness-worklist))
      let blk = pop(liveness-worklist);
      
let live = #();
if(blk.block-branch-block)
  live := element(live-at-head-branch, blk.block-branch-block, default: #());
end if;
if(blk.block-fail-block)
  live := union(live, element(live-at-head-fail, blk.block-fail-block,
                              default: #()));
end if;
if(blk.block-branch-instruction
   & blk.block-branch-instruction.instruction-operand-x
   & ~p2k-constant?(blk.block-branch-instruction.instruction-operand-x))
  live := add-new(live, blk.block-branch-instruction.instruction-operand-x);
end if;
            
for(i from blk.block-instructions.size - 1 to 0 by -1)
  let inst = blk.block-instructions[i];
  unless(inst.instruction-side-effects? | inst.instruction-replaced-with)
    live := remove(live, inst);
  end;
  unless(inst.instruction-replaced-with)
    if(inst.instruction-needs-register?)
      for(val in live)
        add-edge(inst, val);
      end for;
      unless(element(degree, inst, default: #f))
        degree[inst] := 0;
        adjacency-list[inst] := #();
      end;
    end if;
    if(inst.instruction-operand-x
       & ~p2k-constant?(inst.instruction-operand-x))
      live := add-new(live, inst.instruction-operand-x);
    end if;
    if(inst.instruction-operand-y
       & ~p2k-constant?(inst.instruction-operand-y))
      live := add-new(live, inst.instruction-operand-y);
    end if;
  end;
end for;
            
let branch-live = live;
let fail-live = live;
for(i from blk.block-phi-instructions.size - 1 to 0 by -1)
  let inst = blk.block-phi-instructions[i];
  unless(inst.instruction-replaced-with)
    branch-live := remove(branch-live, inst);
    fail-live := remove(fail-live, inst);
  end;
  unless(inst.instruction-replaced-with)
   for(val in live)
     add-edge(inst, val);
   end for;
   unless(element(degree, inst, default: #f))
     degree[inst] := 0;
     adjacency-list[inst] := #();
   end;
   unless(p2k-constant?(inst.instruction-operand-x))
     branch-live := add-new(branch-live, inst.instruction-operand-x);
   end;
   unless(p2k-constant?(inst.instruction-operand-y))
     fail-live := add-new(fail-live, inst.instruction-operand-y);
   end;
  end unless;
end for;
            
if(element(fail-predecessor, blk, default: #f))
  if(element(live-at-head-fail, blk, default: #f))
    let fail-difference = difference(fail-live, live-at-head-fail[blk]);
    if(~empty?(fail-difference))
      unless(member?(fail-predecessor[blk], liveness-worklist))
        push(liveness-worklist, fail-predecessor[blk]);
      end;
    end if;
  else
    push(liveness-worklist, fail-predecessor[blk]);
  end if;
end if;
if(element(branch-predecessor, blk, default: #f))
  if(element(live-at-head-branch, blk, default: #f))
    let branch-difference = difference(branch-live, live-at-head-branch[blk]);
    if(~empty?(branch-difference))
      unless(member?(branch-predecessor[blk], liveness-worklist))
        push(liveness-worklist, branch-predecessor[blk]);
      end;
    end if;
  else
    push(liveness-worklist, branch-predecessor[blk]);
  end if;
end if;
live-at-head-branch[blk] := branch-live;
live-at-head-fail[blk] := fail-live;
            
    end;
  end method,
            
  method construct-worklists(entry-blk :: <p2k-basic-block>) => ();
    for(deg keyed-by inst in degree)
      let x = inst.instruction-operand-x;
      let y = inst.instruction-operand-y;
      if(inst.instruction-opcode == #"PHI")
         if(instance?(x, <p2k-instruction>))
           let move = make(<p2k-move>,
                           phi: inst, operand-getter: instruction-operand-x);
           move-list[inst]
             := add-new(element(move-list, inst, default: #()), move);
           move-list[x] := add-new(element(move-list, x, default: #()), move);
           push(worklist-moves, move);
           move-kind[move] := #"worklist";
         end if;
         if(instance?(y, <p2k-instruction>))
           let move = make(<p2k-move>,
                           phi: inst, operand-getter: instruction-operand-y);
           move-list[inst]
             := add-new(element(move-list, inst, default: #()), move);
           move-list[y] := add-new(element(move-list, y, default: #()), move);
           push(worklist-moves, move);
           move-kind[move] := #"worklist";
         end if;
      end if;
    end for;
    
for(deg keyed-by inst in degree)
  if(deg >= K)
    push(spill-worklist, inst);
    worklist-kind[inst] := #"spill";
  elseif(element(move-list, inst, default: #f))
    push(freeze-worklist, inst);
    worklist-kind[inst] := #"freeze";
  else
    push(simplify-worklist, inst);
    worklist-kind[inst] := #"simplify";
  end if;
end for;    
            
  end,
            
  method regalloc-simplify() => ();
    let inst = pop(simplify-worklist);
    push(select-stack, inst);
    worklist-kind[inst] := #"select";
    for(val in element(adjacency-list, inst, default: #()))
      unless(worklist-kind[val] == #"select"
             | worklist-kind[val] == #"coalesced")
        decrement-degree(val);
      end;
    end for;
  end method,
            
  method move-related?
      (inst :: <p2k-instruction>) 
   => (related? :: <boolean>);
    let moves = element(move-list, inst, default: #f);
    moves
      & any?(method(m)
                   move-kind[m] == #"active" | move-kind[m] == #"worklist";
                 end, moves);
  end method,
            
  method decrement-degree(inst :: <p2k-instruction>) => ();
    let d = degree[inst];
    degree[inst] := d - 1;
    if(d = K)
      enable-moves(inst);
      do(enable-moves, adjacency-list[inst]);
      remove!(spill-worklist, inst);
      if(move-related?(inst))
        push(freeze-worklist, inst);
        worklist-kind[inst] := #"freeze";
      else
        push(simplify-worklist, inst);
        worklist-kind[inst] := #"simplify";
      end if;
    end if;
  end method,
            
  method enable-moves(inst :: <p2k-instruction>) => ();
    for(m in element(move-list, inst, default: #()))
      if(move-kind[m] == #"active")
        push(worklist-moves, m);
        move-kind[m] := #"worklist";
      end if;
    end for;
  end method,
            
  method get-alias
      (inst :: <p2k-instruction>) => (a :: <p2k-instruction>);
    if(worklist-kind[inst] == #"coalesced")
      get-alias(alias[inst]);
    else
      inst;
    end if;
  end method,
            
  method regalloc-coalesce() => ();
    let m = pop(worklist-moves);
    let dest = get-alias(m.move-phi);
    let src = get-alias((m.move-operand-getter)(m.move-phi));

    if(dest == src)
      move-kind[m] := #"coalesced";
      coalesce-add-to-worklist(dest);
    elseif(element(edges, pair(dest, src), default: #f))
      move-kind[m] := #"constrained";
      coalesce-add-to-worklist(dest);
      coalesce-add-to-worklist(src);
    elseif(conservative?(union(adjacency-list[dest], adjacency-list[src])))
      move-kind[m] := #"coalesced";
      coalesce-combine(dest, src);
      coalesce-add-to-worklist(dest);
    else
      move-kind[m] := #"active";
    end if;      
  end method,
            
  method coalesce-add-to-worklist(inst :: <p2k-instruction>) => ();
    if(worklist-kind[inst] ~== #"freeze")
      error("coalesce-to-worklist but %= is in %=", inst, worklist-kind[inst]);
    end if;
    if(~move-related?(inst) & degree[inst] < K)
      remove!(freeze-worklist, inst);
      push(simplify-worklist, inst);
      worklist-kind[inst] := #"simplify";
    end if;
  end method,
            
  method conservative?
      (insts :: <sequence>) => (conservative? :: <boolean>);
    let c = for(inst in insts,
                c = 0 then if(degree[inst] >= K) c + 1 else c end)
              finally c;
            end;
    c < K;
  end method,
            
  method coalesce-combine
      (dest :: <p2k-instruction>, src :: <p2k-instruction>)
   => ();
    if(worklist-kind[src] == #"freeze")
      remove!(freeze-worklist, src);
    elseif(worklist-kind[src] == #"spill")
      remove!(spill-worklist, src);
    else
      error("combine: %= in the wrong worklist!", src);
    end if;

    push(coalesced, src);
    worklist-kind[src] := #"coalesced";
    alias[src] := dest;
    move-list[dest] := union(move-list[dest], move-list[src]);
    for(t in adjacency-list[src])
      add-edge(t, dest);
      decrement-degree(t);
    end for;
    if(worklist-kind[dest] == #"freeze" & degree[dest] >= K)
      remove!(freeze-worklist, dest);
      push(spill-worklist, dest);
    end if;
  end method,
            
  method regalloc-freeze() => ();
    let inst = pop(freeze-worklist);
    push(simplify-worklist, inst);
    worklist-kind[inst] := #"simplify";
    freeze-moves(inst);
  end method,
            
  method freeze-moves(inst :: <p2k-instruction>) => ();
    for(m in element(move-list, inst, default: #()))
      if(move-kind[m] == #"active")
        let dest = get-alias(m.move-phi);
        let src = get-alias((m.move-operand-getter)(m.move-phi));
        move-kind[m] := #"frozen";
        let release = if(get-alias(src) == get-alias(inst))
                        get-alias(dest);
                      else
                        get-alias(src);
                      end;
        if(~move-related?(release) & degree[release] < K)
          remove!(freeze-worklist, release);
          push(simplify-worklist, release);
          worklist-kind[release] := #"simplify";
        end if;
      end if;
    end for;
  end method,
            
  method regalloc-select-spill() => ();
    let inst = pop(spill-worklist);
    push(simplify-worklist, inst);
    worklist-kind[inst] := #"simplify";
    freeze-moves(inst);
  end method,
            
  method regalloc-assign-registers() => ();
    while(~empty?(select-stack))
      let registers = make(<vector>, size: K);
      for(i from 0 below K)
        registers[i] := i;
      end;
      
      let inst = pop(select-stack);
      for(interfering in adjacency-list[inst])
        let assigned = get-alias(interfering);
        if(worklist-kind[assigned] == #"colored")
          registers[assigned.instruction-register] := #f;
        end if;
      end for;
      let available = choose(method(x) x end, registers);
      if(empty?(available))
        push(spilled, inst);
        worklist-kind[inst] := #"spilled"
      else
        inst.instruction-register := first(available);
        worklist-kind[inst] := #"colored";
      end if;
    end;
    while(~empty?(coalesced))
      let inst = pop(coalesced);
      inst.instruction-register := get-alias(inst).instruction-register;
    end;
  end,
            
  method regalloc-perform-spill(procedure :: <p2k-procedure>) => ();
    
let spill-map :: <object-table> = make(<object-table>);
for(val in spilled)
  procedure.procedure-storage
   := procedure.procedure-storage + 4;
  spill-map[val] := - procedure.procedure-storage;
end for;
            
    local
      method rewrite-block(blk :: <p2k-basic-block>) => ();
        
let new-instructions = make(<stretchy-vector>);
for(phi in blk.block-phi-instructions)
  if(element(spill-map, phi, default: #f))
    let compute-addr = make(<p2k-instruction>, opcode: #"ADD",
                            block: blk, x: spill-map[phi]);
    add!(new-instructions, compute-addr);
    let store = make(<p2k-instruction>, opcode: #"STORE", block: blk,
                     x: phi, y: compute-addr);
    add!(new-instructions, store);
  end if;
end for;
for(inst in blk.block-instructions)
  let x = inst.instruction-operand-x;
  let y = inst.instruction-operand-y;
  if(element(spill-map, x, default: #f))
    let compute-addr = make(<p2k-instruction>, opcode: #"ADD",
                            block: blk, x: spill-map[x]);
    add!(new-instructions, compute-addr);
    let load = make(<p2k-instruction>, opcode: #"LOAD", block: blk,
                    y: compute-addr);
    add!(new-instructions, load);
    inst.instruction-operand-x := load;
  end if;
  if(element(spill-map, y, default: #f))
    if(x == y)
      inst.instruction-operand-y := inst.instruction-operand-x;
    else
      let compute-addr = make(<p2k-instruction>, opcode: #"ADD",
                              block: blk, x: spill-map[y]);
      add!(new-instructions, compute-addr);
      let load = make(<p2k-instruction>, opcode: #"LOAD", block: blk,
                      y: compute-addr);
      add!(new-instructions, load);
      inst.instruction-operand-y := load;
    end if;
  end if;
  add!(new-instructions, inst);
  if(element(spill-map, inst, default: #f))
    let compute-addr = make(<p2k-instruction>, opcode: #"ADD",
                            block: blk, x: spill-map[inst]);
    add!(new-instructions, compute-addr);
    let store = make(<p2k-instruction>, opcode: #"STORE", block: blk,
                     x: inst, y: compute-addr);
    add!(new-instructions, store);
  end if;
end for;
            
if(blk.block-branch-block)
  for(phi in blk.block-branch-block.block-phi-instructions)
    if(element(spill-map, phi.instruction-operand-x, default: #f))
      let compute-addr = make(<p2k-instruction>, opcode: #"ADD",
                              block: blk,
                              x: spill-map[phi.instruction-operand-x]);
      add!(new-instructions, compute-addr);
      let load = make(<p2k-instruction>, opcode: #"LOAD", block: blk,
                      y: compute-addr);
      add!(new-instructions, load);
      phi.instruction-operand-x := load;
    end if;
  end for;
end if;
if(blk.block-fail-block)
  for(phi in blk.block-fail-block.block-phi-instructions)
    if(element(spill-map, phi.instruction-operand-y, default: #f))
      let compute-addr = make(<p2k-instruction>, opcode: #"ADD",
                              block: blk,
                              x: spill-map[phi.instruction-operand-y]);
      add!(new-instructions, compute-addr);
      let load = make(<p2k-instruction>, opcode: #"LOAD", block: blk,
                      y: compute-addr);
      add!(new-instructions, load);
      phi.instruction-operand-y := load;
    end if;
  end for;
end if;    
blk.block-instructions := new-instructions;
            
        do(rewrite-block, blk.block-dominates);
      end method;
    rewrite-block(procedure.procedure-entry-block);
  end method,
            
    method regalloc-main(entry-blk :: <p2k-basic-block>) => ();
      construct-liveness-adjacency(entry-blk);
      construct-worklists(entry-blk);
      while(~empty?(simplify-worklist) | ~empty?(worklist-moves)
            | ~empty?(freeze-worklist) | ~empty?(spill-worklist))
        if(~empty?(simplify-worklist))
           regalloc-simplify();
        elseif(~empty?(worklist-moves))
          regalloc-coalesce();
        elseif(~empty?(freeze-worklist))
          regalloc-freeze();
        elseif(~empty?(spill-worklist))
          regalloc-select-spill();
        end if;
      end;
      regalloc-assign-registers();
      if(~empty?(spilled))
        regalloc-perform-spill(procedure);
        p2k-regalloc(procedure, K);
      end if;
    end method;
  regalloc-main(procedure.procedure-entry-block);
end method;
            
define class <undirected-edge-table> (<table>)
  // No additional slots
end class;
            
define method table-protocol(table :: <undirected-edge-table>)
  => (test-function :: <function>, hash-function :: <function>);
  values(method  // equality method
             (edge1 :: <pair>, edge2 :: <pair>)
          => (equal? :: <boolean>);
           (edge1.head == edge2.head & edge1.tail == edge2.tail)
           | (edge1.head == edge2.tail & edge1.tail == edge2.head)
         end,
         method  // hash method
             (edge :: <pair>, initial-state :: <object>)
          => (hash-id :: <integer>, hash-state :: <object>);
           let (head-id, head-state) = object-hash(edge.head, initial-state);
           let (tail-id, tail-state) = object-hash(edge.tail, initial-state);
           let merged-id = merge-hash-ids(head-id, tail-id, ordered: #f);
           values(merged-id, tail-state);
         end);
end method;
            
define sealed domain table-protocol(<undirected-edge-table>);
            
define class <p2k-move> (<object>)
  constant slot move-phi :: <p2k-instruction>,
    required-init-keyword: phi:;
  constant slot move-operand-getter :: <function>,
    required-init-keyword: operand-getter:;
end class;
            
define method p2k-generate-moves
    (procedure :: <p2k-procedure>)
 => ();
  local
    method generate-moves(blk :: <p2k-basic-block>) => ();
      if(blk.block-branch-block)
        for(phi in blk.block-branch-block.block-phi-instructions)
          if(p2k-constant?(phi.instruction-operand-x)
             | phi.instruction-register
                 ~= phi.instruction-operand-x.instruction-register)
            let move = make(<p2k-instruction>, opcode: #"MOVE",
                            block: blk, x: phi.instruction-operand-x);
            move.instruction-register := phi.instruction-register;
            add!(blk.block-instructions, move);
            phi.instruction-operand-x := move;
          end if;
        end for;
      end if;
      if(blk.block-fail-block)
        for(phi in blk.block-fail-block.block-phi-instructions)
          if(p2k-constant?(phi.instruction-operand-y)
             | phi.instruction-register
                 ~= phi.instruction-operand-y.instruction-register)
            let move = make(<p2k-instruction>, opcode: #"MOVE",
                            block: blk, x: phi.instruction-operand-y);
            move.instruction-register := phi.instruction-register;
            add!(blk.block-instructions, move);
            phi.instruction-operand-y := move;
          end if;
        end for;
      end if;
      do(generate-moves, blk.block-dominates);
    end method;
  generate-moves(procedure.procedure-entry-block);
end method;
            
