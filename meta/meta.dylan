module: meta
synopsis: exports other modules and provides common scan functions
author:  Douglas M. Auclair
copyright: (c) 2001, LGPL

// a word is delimited by non-graphic characters: spaces, <>, {}, [],
// punctuation, or the single- or double- quotation-mark.

define collector word(w)
  loop([element-of($any-char, w), do(collect(w))]),
  (str.size > 0)
end collector word;

define constant $graphic-digit :: <byte-string> = concatenate($digit, "+-");


/*
// This is the right way to do it, but as at Oct02 takes three times longer than the collector method
define meta int (c :: <character> = ' ', sign :: <integer> = 1, val :: <integer> = 0) => (sign * val)
  {['-', do(sign := -1)], '+', []},
  [element-of("0123456789", c), do(val := charval(c))],
  loop([element-of("0123456789", c), do(val := val * 10 + charval(c))])
end int;

define collector int(i) => (as(<string>, str).string-to-integer)
  loop([element-of($graphic-digit, i), do(collect(i))]),
  (str.size > 0)
end collector int;
*/

// ... and this is 100 times faster than the collector, 300 times faster than the meta
//
define function scan-int(str :: <byte-string>, #key start: start :: <integer>, end: stop :: <integer>)
 => (pos :: false-or(<integer>), val :: <integer>);
  let pos = start;
  let sign = 1;
  let val = 0;
  if (pos < stop)
    let ch = str[pos];
    if ((ch == '-' & (sign := -1)) | (ch == '+'))
      pos := pos + 1;
    end;
  end;
  block (done)
    while (pos < stop)
      let ch = str[pos];
      if (ch < '0' | ch > '9')
        done();
      end;
      val := val * 10 + (as(<integer>, ch) - as(<integer>, '0'));
      pos := pos + 1;
    end;
  end block;
  values(pos > start & pos, sign * val);
end scan-int;


// this should really follow the Lisp meta example and return anything
// from an integer to a fraction to a float
//
define collector number(n) => (as(<string>, str).scan-double-float)
  loop([element-of($num-char, n), do(collect(n))]),
  (str.size > 0)
end collector number;

/*
// Maybe try ressurecting this from time to time if efficiency improvements
// are made to meta code generation.  Meantime, a handwritten version is below
//

define collector double-float(n) => (as(<string>, str).string-to-double-float)
  loop([element-of($num-char, n), do(collect(n))]),
  (str.size > 0)
end collector double-float;
*/

define meta s(c)
  element-of($space, c), loop(element-of($space, c))
end meta s;


// This is kinda long and complex, but it's several orders of magnitude faster
// than what used to be here
//
define function scan-double-float(str :: <byte-string>,
                                  #key start :: <integer> = 0,
                                       end: finish :: <integer> = str.size,
                                       base :: <integer> = 10)
 => (pos :: false-or(<integer>), val :: <double-float>);
  let pos = start;
  let sign = 1.0d0;
  let mantissa = 0.0d0;
  let dot-seen = #f;
  let scale = 0;
  let exponent-sign = 1;
  let exponent = 0;
  let float-base = as(<double-float>, base);
  
  // Parse the optional sign.
  if (pos < finish)
    let char = str[pos];
    if (char == '-')
      pos := pos + 1;
      sign := -1.0d0
    elseif (char == '+')
      pos := pos + 1;
    end if;
  end if;

  block (return)
    block (compute-value)
      block (parse-exponent)
        // Parse the mantissa.
        let mant-start = pos;
        while (pos < finish)
          let char = str[pos];
          if (char >= '0' & char <= '9')
            let digit = as(<integer>, char) - as(<integer>, '0');
            mantissa := mantissa * float-base + digit;
            if (dot-seen)
              scale := scale + 1;
            end if;
          elseif (char == '.')
            if (dot-seen)
              return(#f, 0.0d0); // bogus float with two dots
            end if;
            dot-seen := #t;
          elseif (char == 'e' | char == 'E' | char == 'd' | char == 'D')
            pos := pos + 1;
            parse-exponent();
          elseif (pos == mant-start)
            // no mantissa
            return(#f, 0.0d0);
          else
            // it's got no exponent
            compute-value();
          end if;
          pos := pos + 1;
        end while;
      end block;

      // Parse the exponent.
      if (pos < finish)
        let char = str[pos];
        if (char == '-')
          exponent-sign := -1;
          pos := pos + 1;
        elseif (char == '+')
          pos := pos + 1;
        end if;

        let exponent-start = pos;
        while (pos < finish)
          let char = str[pos];
          if (char >= '0' & char <= '9')
            let digit = as(<integer>, char) - as(<integer>, '0');
            exponent := exponent * base + digit;
          else
            if (exponent-start == pos)
              return (#f, 0.0d0); // bogus exponent
            else
              compute-value();
            end;
          end if;
          pos := pos + 1;
        end while;
      end if;
    end block; // compute-value
    
    // assemble the number from the components
    let mant = sign * mantissa;
    let exp10 = exponent-sign * exponent - scale;

    if (exp10 == 0)
      return(pos, mant)
    end;

    // calculate y = 10 ^ n
    let n = if (exp10 < 0) -exp10 else exp10 end;
    let y = 1.0d0;
    let z = float-base;
    for()
      let odd = logand(n, 1);
      n := ash(n, -1);
      if (odd ~== 0)
        y := y * z;
        if (n == 0)
          if (exp10 < 0)
            return (pos, mant / y)
          else
            return (pos, mant * y)
          end;
        end;
      end;
      z := z * z;
    end for;
  end block; // return
end function scan-double-float;


define function scan-single-float(str :: <byte-string>, #key start: start :: <integer>, end: finish :: <integer>)
 => (pos :: false-or(<integer>), val :: <single-float>);
  // no harm in reading as double-float and may be more accurate
  let (ok, val) = scan-double-float(str, start: start, end: finish);
  values(ok, as(<single-float>, val));
end scan-single-float;
