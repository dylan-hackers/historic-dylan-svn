//  Stack handling routines for lmath
//  (c) Copyright 1990, Richard J. Fateman
// LTD: Function PROVIDE not yet implemented.
provide(#"stack1");

//  Two parallel stacks for names (vars) and values (vals)
//  are maintained. There's only one instance of this stack, reducing
//  benefit of using defstruct.
"(in-package mma)";

// (export '(stack make-stack spush spushframe spopframe spop sfind schange
// 		stack-ptr stack-frameptr sfindd stackprinter env))
define class <stack> (<object>)
  slot stack-size :: <integer> = 100, init-keyword: #"stack-size";
  // if no size is specified, how about 100?
  //  ptr points to top of stack.  0<=ptr<size : the index in which to
  //  store at the next "push".
  slot stack-ptr :: <integer> = 0, init-keyword: #"stack-ptr";
  // frameptr points to the bottom of current call-frame 
  //  a pair that looks like  <name of function> <next lower frameptr>
  //  -1 <= frameptr < ptr
  slot stack-frameptr :: <integer> = -1, init-keyword: #"stack-frameptr";
  slot stack-vars = make(<array>, dimensions: size),
       init-keyword: #"stack-vars";
  slot stack-vals = make(<array>, dimensions: size),
       init-keyword: #"stack-vals";
end class <stack>;

define method spush (s, var, val)
  s.stack-vars[s.stack-ptr] := var;
  s.stack-vals[s.stack-ptr] := val;
  // could check for overflow here
  inc!(s.stack-ptr);
  s;
end method spush;

//  establish a new call frame
define method spushframe (s, #key name = #"anony")
  spush(s, name, s.stack-frameptr);
  //  push old frame pointer on stack
  //  set frameptr to current top-of-stack.
  s.stack-frameptr := s.stack-ptr - 1;
  s;
end method spushframe;

//  Popframe.  Reset stack to remove all items from this "call"
define method spopframe (s)
  //  could check that s is a stack and is non-empty, but we don't
  s.stack-ptr := s.stack-frameptr;
  s.stack-frameptr := s.stack-vals[s.stack-ptr];
  s;
end method spopframe;

//  this version of pop returns 2 values  (variable, value) of the
//  item that was on the top of the stack, but has been removed.
//  If an additional argument n > 1 is supplied,  n-1 extra items
//  are removed, and then one is popped off.
define method spop (s, #key n = 1)
  let p = dec!(s.stack-ptr, n);
  values(s.stack-vars[p], s.stack-vals[p]);
end method spop;

//  to find an entry, use sfind.  A multiple value is returned.
//  first value is value found, if any
//  second value is nil, if no value was found, otherwise, the index
define method sfind (s, var)
  let loc
      = find-key(copy-subsequence(s.stack-vars, start: s.stack-frameptr + 1,
                                  end: s.stack-ptr),
                 curry(\==, var));
  if (loc)
    values(s.stack-vals[loc], loc);
  else
    //  found: 2nd val is index
    values(var, loc);
  end if;
end method sfind;

//  to change an entry, use schange -- change the binding of var
define method schange (s, var, val)
  let loc
      = find-key(copy-subsequence(s.stack-vars, start: s.stack-frameptr + 1,
                                  end: s.stack-ptr),
                 curry(\==, var));
  if (loc) s.stack-vals[loc] := val; else spush(s, var, val); end if;
end method schange;

//  a variation similar to gethash default usage. If you don't
//  find the variable on the stack, return the default value.
define method sfindd (s, var, default)
  let loc
      = find-key(copy-subsequence(s.stack-vars, end: s.stack-ptr),
                 curry(\==, var));
  if (loc) s.stack-vals[loc]; else default; end if;
end method sfindd;

define method stackprinter (a, stream, pl)
  let fp = a.stack-frameptr - 1;
  let sp = a.stack-ptr;
  // pl, print-level, is not used			
  //  we don't print the size of the stack. Should we?
  if (0 = sp)
    format(stream, "Empty Stack\n");
  else
    for (i = 1-(sp) then 1-(i), until i < 0)
      if (i == fp + 1)
        format(stream, "** bottom of frame %= **\n", a.stack-vars[i]);
        fp := a.stack-vals[i] - 1;
      else
        (method (s, #rest args)
           apply(maybe-initiate-xp-printing,
                 method (xp, #rest args)
                   begin
                     fluid-bind (*print-escape* = #t)
                       write+(pop!(args), xp);
                     end fluid-bind;
                     write-char++(' ', xp);
                     pprint-tab+(line: 5, 1, xp);
                     write-string++("-> ", xp, 0, 3);
                     fluid-bind (*print-escape* = #t)
                       write+(pop!(args), xp);
                     end fluid-bind;
                     pprint-newline+(unconditional: xp);
                   end;
                   if (args) copy-sequence(args); end if;
                 end method,
                 s, args);
         end method)(stream, // two column format separated by tab to col 5
                     a.stack-vars[i], a.stack-vals[i]);
      end if;
    finally
      #f;
    end for;
  end if;
end method stackprinter;

define variable env = make-stack();

spushframe(env, #"bot");

