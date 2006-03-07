Module: dependent-set


define class <dependent-set> (<object>)
  slot dependent-set-initial :: <sequence> = #(),
    init-keyword: initial:;
  slot dependent-set-final-value :: <sequence>;
  slot dependent-set-dependents :: <list> = #();
end class;
              
define method add-set-dependent!
    (set :: <dependent-set>, depends-on :: <dependent-set>)
 => ();
  set.dependent-set-dependents
    := add-new!(set.dependent-set-dependents, depends-on);
end method;
              
define method dependent-set-value
    (set :: <dependent-set>)
 => (sequence :: <sequence>);
  if(~slot-initialized?(set, dependent-set-final-value))
    local
      
method visit
    (set :: <dependent-set>, traverse :: <function>)
 => ();
  set.dependent-set-final-value := set.dependent-set-initial;

  for(dependent in set.dependent-set-dependents,
      final-value = set.dependent-set-initial
        then
          begin
            if(~slot-initialized?(dependent, dependent-set-final-value))
              traverse(dependent);
            end if;
            union(final-value, dependent.dependent-set-final-value);
          end)
  finally
    set.dependent-set-final-value := final-value;
  end for;
end method,
              
method collapse
    (scc-head :: <dependent-set>, scc-other :: <dependent-set>)
 => ();
  scc-other.dependent-set-final-value := scc-head.dependent-set-final-value;
end method
              ;
    find-rooted-scc(set, visit, collapse);
  end if;
  set.dependent-set-final-value;
end method;
              
