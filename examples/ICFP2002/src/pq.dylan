module: pq
copyright: this program may be freely used by anyone, for any purpose

// A first shot at a priority queue

define macro swap
  { swap(?var1:expression, ?var2:expression) }
    => { let temp = ?var1; ?var1 := ?var2; ?var2 := temp }
end macro; 

// A priority queue uses the relation \< to order the entries

define class <priority-queue> (<mutable-sequence>, <stretchy-collection>)
  slot heap :: <vector> = make(<stretchy-vector>);
  slot comparison-function :: <function>, init-value: \<, init-keyword: comparison-function:;
  virtual slot size :: <integer>, init-value: 0;
end class;

define method size (pq :: <priority-queue>) => (size :: <object>)
  pq.heap.size;
end method size;

define method add!(pq :: <priority-queue>, value) => (pq :: <priority-queue>)
  let index :: <integer> = pq.size;

  pq.heap.size := pq.heap.size + 1;
  pq.heap[index] := value;
  bottom-up(pq, index);
  pq;
end method add!;

define method bottom-up(pq :: <priority-queue>, index :: <integer>) => ();
  let bubble = pq.heap[index];
  let super-index :: <integer> = ash(index, -1);

  while(index > 0 & pq.comparison-function(pq.heap[super-index], bubble))
    pq.heap[index] := pq.heap[super-index];
    index := super-index;
    super-index := ash(index + 1, -1) - 1;
  end while;

  pq.heap[index] := bubble;
end method bottom-up;

define method remove-front!(pq :: <priority-queue>) => (first-element :: <object>);
  let first-element = pq.heap[0];

  pq.heap[0] := pq.heap[pq.size - 1];
  pq.heap.size := pq.size - 1;
  if(pq.size > 0)
    top-down(pq, 0);
  end if;
  first-element;
end method remove-front!;
	
define method top-down(pq :: <priority-queue>, index :: <integer>) => ();
  let bubble = pq.heap[index];
  let sub-index = ash(index + 1, 1) - 1;

  block(return)
    while(sub-index + 1 < pq.heap.size)
      if(pq.comparison-function(pq.heap[sub-index], pq.heap[sub-index + 1]))
	sub-index := sub-index + 1;
      end if;
      if(pq.comparison-function(pq.heap[sub-index], bubble))
	return();
      end if;
      pq.heap[index] := pq.heap[sub-index];
      index := sub-index;
      sub-index := ash(index + 1, 1) - 1;
    end while;
    if(sub-index < pq.heap.size & pq.comparison-function(bubble, pq.heap[sub-index]))
      pq.heap[index] := pq.heap[sub-index];
      index := sub-index;
    end if;
  end block;
  pq.heap[index] := bubble;
end method top-down;

/*
define method main(appname, #rest arguments)
  let pq = make(<priority-queue>, comparison-function: \>);

  for(i from 0 below 1000000)
    if(modulo(i, 1024) = 1023)
      format(*standard-error*, ".");
      force-output(*standard-error*);
    end if;
    add!(pq, i);
  end for;

  let temp = remove-front!(pq);
  let temp2 = remove-front!(pq);

  while(pq.size > 0)
    if(temp > temp2)
      format(*standard-output*, "PQ error!\n");
    end if;
    temp := temp2;
    temp2 := remove-front!(pq);
  end while;
  
end method main;
*/