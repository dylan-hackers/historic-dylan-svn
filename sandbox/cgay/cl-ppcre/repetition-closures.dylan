//  -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
//  $Header: /usr/local/cvsrep/cl-ppcre/repetition-closures.lisp,v 1.28 2007/01/01 23:43:10 edi Exp $
//  This is actually a part of closures.lisp which we put into a
//  separate file because it is rather complex. We only deal with
//  REPETITIONs here. Note that this part of the code contains some
//  rather crazy micro-optimizations which were introduced to be as
//  competitive with Perl as possible in tight loops.
//  Copyright (c) 2002-2007, Dr. Edmund Weitz. All rights reserved.
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions
//  are met:
//    * Redistributions of source code must retain the above copyright
//      notice, this list of conditions and the following disclaimer.
//    * Redistributions in binary form must reproduce the above
//      copyright notice, this list of conditions and the following
//      disclaimer in the documentation and/or other materials
//      provided with the distribution.
//  THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
//  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
//  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
//  ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
//  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
//  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
//  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
//  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
//  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
//  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
//  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"(in-package cl-ppcre)";

// LTD: No macros.
#"incf-after";

//  code for greedy repetitions with minimum zero
// LTD: No macros.
#"greedy-constant-length-closure";

// Creates a closure which tries to match REPETITION. It is assumed
// that REPETITION is greedy and the minimal number of repetitions is
// zero. It is furthermore assumed that the inner regex of REPETITION is
// of fixed length and doesn't contain registers.
define generic create-greedy-constant-length-matcher (repetition, next-fn) ;

// Creates a closure which tries to match REPETITION. It is assumed
// that REPETITION is greedy and the minimal number of repetitions is
// zero. It is furthermore assumed that the inner regex of REPETITION can
// never match a zero-length string (or instead the maximal number of
// repetitions is 1).
define generic create-greedy-no-zero-matcher (repetition, next-fn) ;

define method create-greedy-no-zero-matcher (repetition :: <repetition>,
                                             next-fn)
  let maximum = repetition.maximum;
  let repeat-matcher = #f;
  if (maximum == 1)
    //  this is essentially like the next case but with a known
    //  MAXIMUM of 1 we can get away without a counter; note that
    //  we always arrive here if CONVERT optimizes <regex>* to
    //  (?:<regex'>*<regex>)?
    repeat-matcher := create-matcher-aux(repetition.regex, next-fn);
    method (start-pos)
      repeat-matcher(start-pos) | next-fn(start-pos);
    end method;
  elseif (maximum)
    //  we make a reservation for our slot in *REPEAT-COUNTERS*
    //  because we need to keep track whether we've reached MAXIMUM
    //  repetitions
    begin
      let rep-num
          = begin
              let %temp2181 = *rep-num*;
              let g2182 = %temp2181 + 1;
              *rep-num* := g2182;
              %temp2181;
            end;
      let greedy-aux
          = method (start-pos :: <integer>)
              //  the actual matcher which first tries to match the
              //  inner regex of REPETITION (if we haven't done so
              //  too often) and on failure calls NEXT-FN
              *repeat-counters*[rep-num] < maximum
               & inc!(*repeat-counters*[rep-num])
               & //  note that REPEAT-MATCHER will call
                 //  GREEDY-AUX again recursively
              begin
                let _ = repeat-matcher(start-pos);
                dec!(*repeat-counters*[rep-num]);
                _;
              end
               | next-fn(start-pos);
            end method;
      //  create a closure to match the inner regex and to
      //  implement backtracking via GREEDY-AUX
      repeat-matcher := create-matcher-aux(repetition.regex, greedy-aux);
      //  the closure we return is just a thin wrapper around
      //  GREEDY-AUX to initialize the repetition counter
      method (start-pos :: <integer>)
        *repeat-counters*[rep-num] := 0;
        greedy-aux(start-pos);
      end method;
    end;
  else
    //  easier code because we're not bounded by MAXIMUM, but
    //  basically the same
    begin
      let greedy-aux
          = method (start-pos :: <integer>)
              repeat-matcher(start-pos) | next-fn(start-pos);
            end method;
      repeat-matcher := create-matcher-aux(repetition.regex, greedy-aux);
      greedy-aux;
    end;
  end if;
end method create-greedy-no-zero-matcher;

// Creates a closure which tries to match REPETITION. It is assumed
// that REPETITION is greedy and the minimal number of repetitions is
// zero.
define generic create-greedy-matcher (repetition, next-fn) ;

define method create-greedy-matcher (repetition :: <repetition>, next-fn)
  let maximum = repetition.maximum;
  let zero-length-num :: <integer>
      = begin
          let %temp2185 = *zero-length-num*;
          let g2186 = %temp2185 + 1;
          *zero-length-num* := g2186;
          %temp2185;
        end;
  let repeat-matcher = #f;
  if (maximum)
    //  we make a reservation for our slot in *REPEAT-COUNTERS*
    //  because we need to keep track whether we've reached MAXIMUM
    //  repetitions
    begin
      let rep-num
          = begin
              let %temp2183 = *rep-num*;
              let g2184 = %temp2183 + 1;
              *rep-num* := g2184;
              %temp2183;
            end;
      let greedy-aux
          = method (start-pos :: <integer>)
              let old-last-pos = *last-pos-stores*[zero-length-num];
              if (old-last-pos & old-last-pos = start-pos)
                //  stop immediately if we've been here before,
                //  i.e. if the last attempt matched a zero-length
                //  string
                return-from-greedy-aux(next-fn(start-pos));
              end if;
              //  otherwise remember this position for the next
              //  repetition
              *last-pos-stores*[zero-length-num] := start-pos;
              *repeat-counters*[rep-num] < maximum
               & inc!(*repeat-counters*[rep-num])
               & //  note that REPEAT-MATCHER will call
                 //  GREEDY-AUX again recursively
              begin
                let _ = repeat-matcher(start-pos);
                dec!(*repeat-counters*[rep-num]);
                (*last-pos-stores*[zero-length-num] := old-last-pos);
                _;
              end
               | next-fn(start-pos);
            end method;
      //  create a closure to match the inner regex and to
      //  implement backtracking via GREEDY-AUX
      repeat-matcher := create-matcher-aux(repetition.regex, greedy-aux);
      //  the closure we return is just a thin wrapper around
      //  GREEDY-AUX to initialize the repetition counter and our
      //  slot in *LAST-POS-STORES*
      method (start-pos :: <integer>)
        begin
          *repeat-counters*[rep-num] := 0;
          *last-pos-stores*[zero-length-num] := #f;
        end;
        greedy-aux(start-pos);
      end method;
    end;
  else
    //  easier code because we're not bounded by MAXIMUM, but
    //  basically the same
    begin
      let greedy-aux
          = method (start-pos :: <integer>)
              let old-last-pos = *last-pos-stores*[zero-length-num];
              if (old-last-pos & old-last-pos = start-pos)
                return-from-greedy-aux(next-fn(start-pos));
              end if;
              *last-pos-stores*[zero-length-num] := start-pos;
              begin
                let _ = repeat-matcher(start-pos);
                (*last-pos-stores*[zero-length-num] := old-last-pos);
                _;
              end
               | next-fn(start-pos);
            end method;
      repeat-matcher := create-matcher-aux(repetition.regex, greedy-aux);
      method (start-pos :: <integer>)
        *last-pos-stores*[zero-length-num] := #f;
        greedy-aux(start-pos);
      end method;
    end;
  end if;
end method create-greedy-matcher;

//  code for non-greedy repetitions with minimum zero
// LTD: No macros.
#"non-greedy-constant-length-closure";

// Creates a closure which tries to match REPETITION. It is assumed
// that REPETITION is non-greedy and the minimal number of repetitions is
// zero. It is furthermore assumed that the inner regex of REPETITION is
// of fixed length and doesn't contain registers.
define generic create-non-greedy-constant-length-matcher (repetition, next-fn) ;

// Creates a closure which tries to match REPETITION. It is assumed
// that REPETITION is non-greedy and the minimal number of repetitions is
// zero. It is furthermore assumed that the inner regex of REPETITION can
// never match a zero-length string (or instead the maximal number of
// repetitions is 1).
define generic create-non-greedy-no-zero-matcher (repetition, next-fn) ;

define method create-non-greedy-no-zero-matcher (repetition :: <repetition>,
                                                 next-fn)
  let maximum = repetition.maximum;
  let repeat-matcher = #f;
  if (maximum == 1)
    //  this is essentially like the next case but with a known
    //  MAXIMUM of 1 we can get away without a counter
    repeat-matcher := create-matcher-aux(repetition.regex, next-fn);
    method (start-pos)
      next-fn(start-pos) | repeat-matcher(start-pos);
    end method;
  elseif (maximum)
    //  we make a reservation for our slot in *REPEAT-COUNTERS*
    //  because we need to keep track whether we've reached MAXIMUM
    //  repetitions
    begin
      let rep-num
          = begin
              let %temp2187 = *rep-num*;
              let g2188 = %temp2187 + 1;
              *rep-num* := g2188;
              %temp2187;
            end;
      let non-greedy-aux
          = method (start-pos :: <integer>)
              next-fn(start-pos)
               | (*repeat-counters*[rep-num] < maximum
                   & inc!(*repeat-counters*[rep-num])
                   & //  note that REPEAT-MATCHER will call
                     //  NON-GREEDY-AUX again recursively
                  begin
                    let _ = repeat-matcher(start-pos);
                    dec!(*repeat-counters*[rep-num]);
                    _;
                  end);
            end method;
      //  create a closure to match the inner regex and to
      //  implement backtracking via NON-GREEDY-AUX
      repeat-matcher := create-matcher-aux(repetition.regex, non-greedy-aux);
      //  the closure we return is just a thin wrapper around
      //  NON-GREEDY-AUX to initialize the repetition counter
      method (start-pos :: <integer>)
        *repeat-counters*[rep-num] := 0;
        non-greedy-aux(start-pos);
      end method;
    end;
  else
    //  easier code because we're not bounded by MAXIMUM, but
    //  basically the same
    begin
      let non-greedy-aux
          = method (start-pos :: <integer>)
              next-fn(start-pos) | repeat-matcher(start-pos);
            end method;
      repeat-matcher := create-matcher-aux(repetition.regex, non-greedy-aux);
      non-greedy-aux;
    end;
  end if;
end method create-non-greedy-no-zero-matcher;

// Creates a closure which tries to match REPETITION. It is assumed
// that REPETITION is non-greedy and the minimal number of repetitions is
// zero.
define generic create-non-greedy-matcher (repetition, next-fn) ;

define method create-non-greedy-matcher (repetition :: <repetition>, next-fn)
  let zero-length-num :: <integer>
      = begin
          let %temp2191 = *zero-length-num*;
          let g2192 = %temp2191 + 1;
          *zero-length-num* := g2192;
          %temp2191;
        end;
  let maximum = repetition.maximum;
  let repeat-matcher = #f;
  if (maximum)
    //  we make a reservation for our slot in *REPEAT-COUNTERS*
    //  because we need to keep track whether we've reached MAXIMUM
    //  repetitions
    begin
      let rep-num
          = begin
              let %temp2189 = *rep-num*;
              let g2190 = %temp2189 + 1;
              *rep-num* := g2190;
              %temp2189;
            end;
      let non-greedy-aux
          = method (start-pos :: <integer>)
              let old-last-pos = *last-pos-stores*[zero-length-num];
              if (old-last-pos & old-last-pos = start-pos)
                //  stop immediately if we've been here before,
                //  i.e. if the last attempt matched a zero-length
                //  string
                return-from-non-greedy-aux(next-fn(start-pos));
              end if;
              //  otherwise remember this position for the next
              //  repetition
              *last-pos-stores*[zero-length-num] := start-pos;
              next-fn(start-pos)
               | (*repeat-counters*[rep-num] < maximum
                   & inc!(*repeat-counters*[rep-num])
                   & //  note that REPEAT-MATCHER will call
                     //  NON-GREEDY-AUX again recursively
                  begin
                    let _ = repeat-matcher(start-pos);
                    dec!(*repeat-counters*[rep-num]);
                    (*last-pos-stores*[zero-length-num] := old-last-pos);
                    _;
                  end);
            end method;
      //  create a closure to match the inner regex and to
      //  implement backtracking via NON-GREEDY-AUX
      repeat-matcher := create-matcher-aux(repetition.regex, non-greedy-aux);
      //  the closure we return is just a thin wrapper around
      //  NON-GREEDY-AUX to initialize the repetition counter and our
      //  slot in *LAST-POS-STORES*
      method (start-pos :: <integer>)
        begin
          *repeat-counters*[rep-num] := 0;
          *last-pos-stores*[zero-length-num] := #f;
        end;
        non-greedy-aux(start-pos);
      end method;
    end;
  else
    //  easier code because we're not bounded by MAXIMUM, but
    //  basically the same
    begin
      let non-greedy-aux
          = method (start-pos :: <integer>)
              let old-last-pos = *last-pos-stores*[zero-length-num];
              if (old-last-pos & old-last-pos = start-pos)
                return-from-non-greedy-aux(next-fn(start-pos));
              end if;
              *last-pos-stores*[zero-length-num] := start-pos;
              next-fn(start-pos)
               | begin
                   let _ = repeat-matcher(start-pos);
                   (*last-pos-stores*[zero-length-num] := old-last-pos);
                   _;
                 end;
            end method;
      repeat-matcher := create-matcher-aux(repetition.regex, non-greedy-aux);
      method (start-pos :: <integer>)
        *last-pos-stores*[zero-length-num] := #f;
        non-greedy-aux(start-pos);
      end method;
    end;
  end if;
end method create-non-greedy-matcher;

//  code for constant repetitions, i.e. those with a fixed number of repetitions
// LTD: No macros.
#"constant-repetition-constant-length-closure";

// Creates a closure which tries to match REPETITION. It is assumed
// that REPETITION has a constant number of repetitions. It is
// furthermore assumed that the inner regex of REPETITION is of fixed
// length and doesn't contain registers.
define generic create-constant-repetition-constant-length-matcher (repetition,
                                                                   next-fn)
;

// Creates a closure which tries to match REPETITION. It is assumed
// that REPETITION has a constant number of repetitions.
define generic create-constant-repetition-matcher (repetition, next-fn) ;

define method create-constant-repetition-matcher (repetition :: <repetition>,
                                                  next-fn)
  let repetitions :: <integer> = minimum(repetition);
  let rep-num :: <integer>
      = begin
          let %temp2203 = *rep-num*;
          let g2204 = %temp2203 + 1;
          *rep-num* := g2204;
          %temp2203;
        end;
  let repeat-matcher = #f;
  if (zero?(repetition.min-len))
    let zero-length-num :: <integer>
        = begin
            let %temp2201 = *zero-length-num*;
            let g2202 = %temp2201 + 1;
            *zero-length-num* := g2202;
            %temp2201;
          end;
    let constant-aux
        = method (start-pos :: <integer>)
            let old-last-pos = *last-pos-stores*[zero-length-num];
            if (old-last-pos & old-last-pos = start-pos)
              //  if we've been here before we matched a
              //  zero-length string the last time, so we can
              //  just carry on because we will definitely be
              //  able to do this again often enough
              return-from-constant-aux(next-fn(start-pos));
            end if;
            //  otherwise remember this position for the next
            //  repetition
            *last-pos-stores*[zero-length-num] := start-pos;
            if (*repeat-counters*[rep-num] < repetitions)
              //  not enough repetitions yet, try it again
              inc!(*repeat-counters*[rep-num]);
              //  note that REPEAT-MATCHER will call
              //  CONSTANT-AUX again recursively
              begin
                let _ = repeat-matcher(start-pos);
                dec!(*repeat-counters*[rep-num]);
                *last-pos-stores*[zero-length-num] := old-last-pos;
                _;
              end;
            else
              //  we're done - call NEXT-FN
              next-fn(start-pos);
            end if;
          end method;
    //  create a closure to match the inner regex and to
    //  implement backtracking via CONSTANT-AUX
    repeat-matcher := create-matcher-aux(repetition.regex, constant-aux);
    //  the closure we return is just a thin wrapper around
    //  CONSTANT-AUX to initialize the repetition counter
    method (start-pos :: <integer>)
      begin
        *repeat-counters*[rep-num] := 0;
        *last-pos-stores*[zero-length-num] := #f;
      end;
      constant-aux(start-pos);
    end method;
  else
    let constant-aux
        = method (start-pos :: <integer>)
            if (*repeat-counters*[rep-num] < repetitions)
              inc!(*repeat-counters*[rep-num]);
              begin
                let _ = repeat-matcher(start-pos);
                dec!(*repeat-counters*[rep-num]);
                _;
              end;
            else
              next-fn(start-pos);
            end if;
          end method;
    repeat-matcher := create-matcher-aux(repetition.regex, constant-aux);
    method (start-pos :: <integer>)
      *repeat-counters*[rep-num] := 0;
      constant-aux(start-pos);
    end method;
  end if;
end method create-constant-repetition-matcher;

//  the actual CREATE-MATCHER-AUX method for REPETITION objects which
//  utilizes all the functions and macros defined above
define method create-matcher-aux (repetition :: <repetition>, next-fn)
  if (repetition.maximum & zero?(repetition.maximum))
    //  this should have been optimized away by CONVERT but just
    //  in case...
    error("Got REPETITION with MAXIMUM 0 (should not happen)");
  elseif (repetition.maximum
           & begin
               let g2205 = repetition.maximum;
               (repetition.minimum = g2205 & g2205 = 1);
             end)
    //  this should have been optimized away by CONVERT but just
    //  in case...
    error("Got REPETITION with MAXIMUM 1 and MINIMUM 1 (should not happen)");
  elseif (repetition.minimum == repetition.maximum & repetition.len
           & ~ repetition.contains-register-p)
    create-constant-repetition-constant-length-matcher(repetition, next-fn);
  elseif (repetition.minimum == repetition.maximum)
    create-constant-repetition-matcher(repetition, next-fn);
  elseif (repetition.greedyp & repetition.len
           & ~ repetition.contains-register-p)
    create-greedy-constant-length-matcher(repetition, next-fn);
  elseif (repetition.greedyp
           & (positive?(repetition.min-len) | repetition.maximum == 1))
    create-greedy-no-zero-matcher(repetition, next-fn);
  elseif (repetition.greedyp)
    create-greedy-matcher(repetition, next-fn);
  elseif (repetition.len & positive?(repetition.len)
           & ~ repetition.contains-register-p)
    create-non-greedy-constant-length-matcher(repetition, next-fn);
  elseif (positive?(repetition.min-len) | repetition.maximum == 1)
    create-non-greedy-no-zero-matcher(repetition, next-fn);
  else
    create-non-greedy-matcher(repetition, next-fn);
  end if;
end method create-matcher-aux;

