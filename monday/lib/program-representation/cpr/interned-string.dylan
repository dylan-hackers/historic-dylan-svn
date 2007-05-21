Module: interned-string


define variable *interned-string-buckets* :: <simple-object-vector>
  = make(<simple-object-vector>, size: 53);
          
define variable *interned-string-count* :: <integer> = 0;
          
define class <interned-string-entry> (<object>)
  constant slot interned-string :: <byte-string>,
    required-init-keyword: interned-string:;
  constant slot interned-string-hash-value :: <integer>,
    required-init-keyword: hash:;
  slot interned-string-entry-next :: false-or(<interned-string-entry>),
    init-value: #f, init-keyword: next:;
end class;
          
define sealed domain make(singleton(<interned-string-entry>));
          
define sealed domain initialize(<interned-string-entry>);
          
define function interned-string-hash
    (name :: <byte-string>,
     start :: <integer>,
     _end  :: <integer>)
 => (hash-value :: <integer>);
  for (i :: <integer> from start below _end,
       hash :: <integer> = 0
         then logand(ash(hash, 5) + hash + 720 + as(<integer>, name[i]),
                     #x3FFFFF))
  finally
    hash
  end for
end function;
          
define method intern-string
    (name :: <byte-string>,
     #key start: start :: false-or(<integer>) = 0,
          end: _end :: false-or(<integer>) = name.size)
 => (canonical-string :: <byte-string>);
  let hash-value = interned-string-hash(name, start, _end);
  let bucket-value = modulo(hash-value, *interned-string-buckets*.size);
  block (return)
    for (entry = *interned-string-buckets*[bucket-value]
           then entry.interned-string-entry-next,
         while: entry)
      if (entry.interned-string-hash-value = hash-value
            & entry.interned-string.size = _end - start
            & block (next)
                for (i :: <integer> from start below _end,
                     j :: <integer> from 0)
                  if (name[i] ~== entry.interned-string[j])
                    next(#f);
                  end if;
                end for;
                #t;
              end block)
        return(entry.interned-string);
      end if;
    end for;
    
*interned-string-count* := *interned-string-count* + 1;
if (*interned-string-count* > *interned-string-buckets*.size)
  
let new-size =
  block(done)
    for (prime in $prime-table)
      if (prime > *interned-string-count*)
        done(prime);
      end if;
    end for;
  end block;
let new-buckets = make(<simple-object-vector>, size: new-size);

for (bucket in *interned-string-buckets*)
  iterate loop (entry = bucket)
    if (entry)
      let next = entry.interned-string-entry-next;
      let bucket-value = modulo(entry.interned-string-hash-value, new-size);
      entry.interned-string-entry-next := new-buckets[bucket-value];
      new-buckets[bucket-value] := entry;
      loop(next);
    end if;
  end iterate;
end for;
*interned-string-buckets* := new-buckets;
          
  bucket-value := modulo(hash-value, *interned-string-buckets*.size);
end if;
let new-string = make(<byte-string>, size: _end - start);
copy-bytes(new-string, 0, name, start, _end - start);
let new-entry = make(<interned-string-entry>,
                     interned-string: new-string,
                     hash: hash-value,
                     next: *interned-string-buckets*[bucket-value]);
*interned-string-buckets*[bucket-value] := new-entry;
new-string
          
  end block;
end method;
          
define constant $prime-table = 
  #[/* 53, */   97,         193,       389,       769,
    1543,       3079,       6151,      12289,     24593,
    49157,      98317,      196613,    393241,    786433,
    1572869,    3145739,    6291469,   12582917,  25165843,
    50331653,   100663319,  201326611, 402653189, 805306457];
          
