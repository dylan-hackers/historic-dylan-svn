module:         beer
author:         Peter Hinely <phinely@hawaii.edu>
version:        1.1,  1998-2004
synopsis:       Dylan version of 99 Bottles of Beer
disclaimer:     This program does not advocate the excessive use of alcohol
                (or the use of static programming languages).

////////////////////////////////////////////////////////////////////////////
define constant $number-table = begin
  let table = make(<table>);
    local method insert-entries (number-strings, #key start = 0, increment = 1)
      for (number-string in number-strings, number from start by increment)
        table[number] := number-string;
      end;
    end method;
    insert-entries(#("zero",    "one",       "two",      "three",
                     "four",    "five",      "six",      "seven",
                     "eight",   "nine",      "ten",      "eleven",
                     "twelve",  "thirteen",  "fourteen", "fifteen",
                     "sixteen", "seventeen", "eighteen", "nineteen"));
    insert-entries(#("twenty",  "thirty",    "forty",    "fifty",
                     "sixty",   "seventy",   "eighty",   "ninety"),
                   start: 20, increment: 10);
    table;
end;

////////////////////////////////////////////////////////////////////////////
define method as (type == <string>, number :: limited(<integer>, min: 0, max: 99))
 => number-string :: <string>;
  element($number-table, number, default: #f)
    | begin
        let (tens-digit, ones-digit) = truncate/(number, 10);
        concatenate($number-table[tens-digit * 10], "-", $number-table[ones-digit]);
      end;
end method;

////////////////////////////////////////////////////////////////////////////
define method as-sentence-case (original :: <string>) => new :: <string>;
  let new = shallow-copy(original);
  new[0] := as-uppercase(new[0]);
  new;
end method;

////////////////////////////////////////////////////////////////////////////
define method enumerate (count == 0) "no more bottles" end;
define method enumerate (count == 1) "one more bottle" end;
define method enumerate (count :: <integer>)
  concatenate(as(<string>, count), " bottles");
end method;

////////////////////////////////////////////////////////////////////////////
define method pronoun (count == 1) "it" end;
define method pronoun (count :: <integer>) "one" end;

////////////////////////////////////////////////////////////////////////////
define method main ()
  format-out("                       Dylan Bottles of Beer\n\n");
  for (i from 99 to 0 by -1)
    format-out("%s of beer on the wall, %s of beer...\n", as-sentence-case(enumerate(i)), enumerate(i));
    if (i > 0)
      format-out("Take %s down, pass it around; %s of beer on the wall.\n\n", pronoun(i), as-sentence-case(enumerate(i - 1)));
    end;
  end for;
  format-out("Go to the store, buy some more; Ninety-nine bottles of beer on the wall.\n\n");
end method;

////////////////////////////////////////////////////////////////////////////
main();
