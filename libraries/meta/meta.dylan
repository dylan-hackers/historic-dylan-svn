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

define collector int(i) => (as(<string>, str).string-to-integer)
  loop([element-of($graphic-digit, i), do(collect(i))]),
  (str.size > 0)
end collector int;

define collector number(n) => (as(<string>, str).string-to-number)
  loop([element-of($num-char, n), do(collect(n))]),
  (str.size > 0)
end collector number;

define meta s(c)
  element-of($space, c), loop(element-of($space, c))
end meta s;

define function digit?(c :: <character>) => (ans :: <boolean>)
  c >= '0' & c <= '9'
end function digit?;

define constant $zero :: <integer> = as(<integer>, '0');
define function digit(c :: <character>) => (ans :: <integer>)
  as(<integer>, c) - $zero;
end function digit;

// this is far from perfect, but it's a lot better than what was here before!
//
define function string-to-number (string :: <byte-string>,
                                  #key start :: <integer> = 0,
                                       end: finish :: <integer> = string.size)
    => 	value :: <double-float>;
  let posn = start;
  let sign = 1.0d0;
  let mantissa = 0.0d0;
  let dot-seen = #f;
  let scale = 0;
  let exponent-sign = 1;
  let exponent = 0;

  // Parse the optional sign.
  if (posn < finish)
    let char = string[posn];
    if (char == '-')
      posn := posn + 1;
      sign := -1.0d0
    elseif (char == '+')
      posn := posn + 1;
    end if;
  end if;

  block (return)
    block (parse-exponent)
      // Parse the mantissa.
      while (posn < finish)
	let char = string[posn];
	posn := posn + 1;
	if (char >= '0' & char <= '9')
	  let digit = as(<integer>, char) - as(<integer>, '0');
	  mantissa := mantissa * 10.0d0 + digit;
	  if (dot-seen)
	    scale := scale + 1;
	  end if;
	elseif (char == '.')
	  if (dot-seen)
	    error("bogus float.");
	  end if;
          dot-seen := #t;
        elseif (char == 'e' | char == 'E' | char == 'd' | char == 'D')
	  parse-exponent();
	else
	  error("bogus float.");
	end if;
      end while;
      return();
    end block;

    // Parse the exponent.
    if (posn < finish)
      let char = string[posn];
      if (char == '-')
	exponent-sign := -1;
	posn := posn + 1;
      elseif (char == '+')
	posn := posn + 1;
      end if;

      while (posn < finish)
	let char = string[posn];
	posn := posn + 1;
	if (char >= '0' & char <= '9')
	  let digit = as(<integer>, char) - as(<integer>, '0');
	  exponent := exponent * 10 + digit;
	else
	  error("bogus float");
	end if;
      end while;
    end if;
  end block;

  // assemble the number from the components
  let mant = sign * mantissa;
  let exp10 = exponent-sign * exponent - scale;
  block (return)
    if (exp10 == 0)
      return(mant)
    end;

    // calculate y = 10 ^ n
    let n = if (exp10 < 0) -exp10 else exp10 end;
    let y = 1.0d0;
    let z = 10.0d0;
    for()
      let odd = logand(n, 1);
      n := ash(n, -1);
      if (odd ~== 0)
        y := y * z;
        if (n == 0)
          if (exp10 < 0)
            return (mant / y)
          else
            return (mant * y)
          end;
        end;
      end;
      z := z * z;
    end for;
  end;
end function string-to-number;
