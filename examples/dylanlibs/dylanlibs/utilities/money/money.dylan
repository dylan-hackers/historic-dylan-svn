Module:    money
Synopsis:  Money arithmetic type
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

define constant <dollar-type> = limited(<integer>, min: 0);
define constant <cent-type> = limited(<integer>, min: 0, max: 99);
define constant <money-sign> = one-of(#"positive", #"negative");

define class <money> (<object>)
  constant sealed slot money-sign :: <money-sign> = #"positive", init-keyword: sign:;
  constant sealed slot money-dollars :: <dollar-type> = 0, init-keyword: dollars:;
  constant sealed slot money-cents :: <cent-type> = 0, init-keyword: cents:;
end class <money>;

define sealed domain make(singleton(<money>));
define sealed domain initialize(<money>);

define sealed method sign-multiplier(sign :: <money-sign>) => (r :: one-of(-1, 1))
  if(sign == #"negative")
    -1
  else
    1
  end;
end method sign-multiplier;

define sealed method invert-sign(sign == #"negative") => (r == #"positive")
  #"positive"
end method invert-sign;

define sealed method invert-sign(sign == #"positive") => (r == #"negative")
  #"negative"
end method invert-sign;                         

define method print-object( m :: <money>, stream :: <stream> ) => ()
  format(stream, 
    "%s%s.%s",
    if(m.money-sign == #"negative") "-" else "" end,
    integer-to-string(m.money-dollars),
    integer-to-string(m.money-cents, size: 2))
end method print-object;

define method print-message( m :: <money>, stream :: <stream> ) => ()
  print-object(m, stream);
end method print-message;

define sealed method make-money(dollars :: <dollar-type>, cents :: <cent-type>, #key sign :: <money-sign> = #"positive") => (m :: <money>)
  make(<money>, 
       dollars: dollars, 
       cents: cents,
       sign: sign);
end method make-money;

define method as(t == <double-float>, m :: <money>) => (f :: <double-float>)
  as(<double-float>, (m.money-dollars + (m.money-cents / 100d0)) * sign-multiplier(m.money-sign));
end method as;

define method as(t == <money>, d :: <double-float>) => (m :: <money>)
  let (dollars, decimal) = truncate(d);
  let cents = truncate(decimal * 100);

  make-money(abs(dollars), 
             abs(cents), 
             sign: if(negative?(dollars)) 
                     #"negative" 
                   else 
                     #"positive" 
                   end);
end method as;

define method as(t == <money>, dollars :: <integer>) => (m :: <money>)
  make-money(abs(dollars), 
             0, 
             sign: if(negative?(dollars)) 
                     #"negative" 
                   else 
                     #"positive" 
                   end);
end method as;

define sealed method normalize(dollars, cents) => (dollars :: <dollar-type>, cents :: <cent-type>, sign :: <money-sign>)
  let sign = if(negative?(dollars)) #"negative" else #"positive" end;
  let (add-dollars, real-cents) = truncate/(cents, 100);
  let dollars = dollars + add-dollars;
  values(abs(dollars), abs(real-cents), sign);  
end method normalize;

define sealed method money-operation(operation == #"add", 
                                     lhs-sign == #"positive", 
                                     rhs-sign == #"positive") => (op :: <function>)
  \+
end method money-operation;

define sealed method money-operation(operation == #"add", 
                                     lhs-sign == #"negative", 
                                     rhs-sign == #"negative") => (op :: <function>)
  \+
end method money-operation;

define sealed method money-operation(operation == #"add", 
                                     lhs-sign == #"positive", 
                                     rhs-sign == #"negative") => (op :: <function>)
  \-
end method money-operation;

define sealed method money-operation(operation == #"add", 
                                     lhs-sign == #"negative", 
                                     rhs-sign == #"positive") => (op :: <function>)
  method(a, b)
    b - a;
  end method;
end method money-operation;

define method \+( lhs :: <money>, rhs :: <money> ) => (m :: <money>)
  let operation = money-operation(#"add", lhs.money-sign, rhs.money-sign);
  let cents :: <integer> = operation(lhs.money-cents, rhs.money-cents);
  let dollars :: <integer> = operation(lhs.money-dollars,  rhs.money-dollars);

  let (dollars, cents, sign) = normalize(dollars, cents);
  make-money(dollars, cents, sign: sign);
end method;

define method \+( lhs :: <money>, rhs :: <integer> ) => (m :: <money>)
  make-money(lhs.money-dollars + rhs, lhs.money-cents);
end method;

define method \+( lhs :: <integer>, rhs :: <money> ) => (m :: <money>)  
  make-money(rhs.money-dollars + lhs, rhs.money-cents);
end method;

define method \-( lhs :: <money>, rhs :: <money> ) => (m :: <money>)
  let operation = money-operation(#"add", lhs.money-sign, invert-sign(rhs.money-sign));
  let cents :: <integer> = operation(lhs.money-cents, rhs.money-cents);
  let dollars :: <integer> = operation(lhs.money-dollars,  rhs.money-dollars);

  let (dollars, cents, sign) = normalize(dollars, cents);
  make-money(dollars, cents, sign: if(rhs > lhs) #"negative" else #"positive" end);
end method;

define method \-( lhs :: <money>, rhs :: <integer> ) => (m :: <money>)
  \-(lhs, as(<money>, rhs));
end method;

define method \-( lhs :: <integer>, rhs :: <money> ) => (m :: <money>)
  \-(as(<money>, lhs), rhs);
end method;

define method \*( lhs :: <money>, rhs :: <integer> ) => (m :: <money>)
  let cents = lhs.money-cents * abs(rhs);
  let dollars = lhs.money-dollars * rhs * sign-multiplier(lhs.money-sign);

  let (dollars, cents, sign) = normalize(dollars, cents);
  make-money(dollars, cents, sign: sign);
end method;

define method \*( lhs :: <integer>, rhs :: <money> )
  \*(rhs, lhs);
end method;

define constant $digits = #['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];

define method digit?(m) => (r == #f)
  #f
end method digit?;

define method digit?(m :: <character>) => (r :: <boolean>)
  member?(m, $digits);
end method digit?;

define method digit-to-integer(m :: <character>) => (r :: <integer>)
  if(digit?(m))
    as(<integer>, m) - as(<integer>, '0');
  else
    error("digit-to-integer: Expected a digit character");
  end if;
end method digit-to-integer;

define method parse-money(source :: <string>, #key start, end: the-end) => (m :: false-or(<money>))
  let source = 
    if(start | the-end)
      copy-sequence(source, start: start, end: the-end);
    else 
      source
    end if;
  let state = #"start";
  let dollars = 0;
  let cents = 0;
  let sign = #"positive";
  block(return)
    local method process-state(ch :: <character>)
      select(state)
        #"start" =>
          select(ch)
            '+' => begin
                     sign := #"positive";
                     state := #"dollar";
                   end;
            '-' => begin
                     sign := #"negative";
                     state := #"dollar";
                   end;
            otherwise => begin
                           sign := #"positive";
                           state := #"dollar";
                           process-state(ch);
                         end;
          end select;
        #"dollar" => 
          case
            digit?(ch) => begin
                            dollars := dollars * 10 + digit-to-integer(ch);
                          end;
            ch == '.' => begin
                           state := #"cent";
                         end;
            otherwise => return(#f);
          end case;
        #"cent" =>
          case
            digit?(ch) => begin
                            cents := cents * 10 + digit-to-integer(ch);
                          end;
            otherwise => return(#f);
          end case; 
        otherwise => return(#f);
      end select; 
    end method;

    for(ch in source)
      process-state(ch);
    end for;

    let (dollars, cents) = normalize(dollars, cents);
    make-money(dollars, cents, sign: sign);
  end block;  
end method parse-money;

define method string-to-money(string :: <string>, #key start = 0, end: the-end = string.size, default = #f) 
 => (result :: false-or(<money>))
  for(i from start below the-end,
      while: string[i] = ' ')
  finally
    parse-money(string, start: i, end: the-end)
  end for;
end method string-to-money;

define method money-to-string(m :: <money>, #key size = 4, fill = '0') => (r :: <string>)
  let stream = make(<string-stream>, direction: #"output");
  format(stream, 
    "%s%s.%s",
    if(m.money-sign == #"negative") "-" else "" end,    
    integer-to-string(m.money-dollars, size: max(1, size - 3), fill: fill),
    integer-to-string(m.money-cents, size: 2));
  stream.stream-contents;
end method money-to-string;

define method \=(lhs :: <money>, rhs :: <money>) => (r :: <boolean>)
  lhs.money-dollars = rhs.money-dollars &
    lhs.money-cents = rhs.money-cents &
    lhs.money-sign = rhs.money-sign;
end method \=;

define method \<(lhs :: <money>, rhs :: <money>) => (r :: <boolean>)
  lhs.money-sign == #"negative" & rhs.money-sign == #"positive" |
    (lhs.money-sign == #"positive" &
     (lhs.money-dollars < rhs.money-dollars |
       (lhs.money-dollars = rhs.money-dollars &
          lhs.money-cents < rhs.money-cents))) |
    (lhs.money-sign == #"negative" &
     (lhs.money-dollars > rhs.money-dollars |
       (lhs.money-dollars = rhs.money-dollars &
          lhs.money-cents > rhs.money-cents)));     
end method \<;

define method zero?(m :: <money>) => (r :: <boolean>)
  m.money-dollars == 0 & m.money-cents == 0;
end method zero?;

define method positive?(m :: <money>) => (r :: <boolean>)
  m.money-sign == #"positive"
end method positive?;

define method negative?(m :: <money>) => (r :: <boolean>)
  m.money-sign == #"negative"
end method negative?;

