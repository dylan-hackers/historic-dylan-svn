module: testgen
synopsis: 
author: 
copyright:

// document ::= document document 
//            | textchar * 
//            | <B> document </B> 
//            | <EM> document </EM> 
//            | <I> document </I> 
//            | <PL> document </PL> 
//            | <S> document </S> 
//            | <TT> document </TT> 
//            | <U> document </U> 
//            | <0> document </0> 
//            | <1> document </1> 
//            | <2> document </2> 
//            | <3> document </3> 
//            | <4> document </4> 
//            | <5> document </5> 
//            | <6> document </6> 
//            | <7> document </7> 
//            | <8> document </8> 
//            | <9> document </9> 
//            | <r> document </r> 
//            | <g> document </g> 
//            | <b> document </b> 
//            | <c> document </c> 
//            | <m> document </m> 
//            | <y> document </y> 
//            | <k> document </k> 
//            | <w> document </w>

define function document(depth :: <integer>) => ();
  select(random(32) by \=)
    0 =>
      write(*standard-output*, " ");
    1 =>
      write(*standard-output*, "  ");
    2 =>
      write(*standard-output*, "   ");
    3 =>
      write(*standard-output*, "    ");
    4 =>
      write(*standard-output*, "\n");
    5 =>
      write-element(*standard-output*, as(<character>, 64 + random(63)));
    6 =>
      write-element(*standard-output*, as(<character>, 64 + random(63)));
      write-element(*standard-output*, as(<character>, 64 + random(63)));
    7 =>
      write(*standard-output*, "<B>");
      document(depth + 1);
      write(*standard-output*, "</B>");
    8 =>
      write(*standard-output*, "<EM>");
      document(depth + 1);
      write(*standard-output*, "</EM>");
    9 =>
      write(*standard-output*, "<I>");
      document(depth + 1);
      write(*standard-output*, "</I>");
    10 =>
      write(*standard-output*, "<PL>");
      document(depth + 1);
      write(*standard-output*, "</PL>");
    11 =>
      write(*standard-output*, "<S>");
      document(depth + 1);
      write(*standard-output*, "</S>");
    12 =>
      write(*standard-output*, "<TT>");
      document(depth + 1);
      write(*standard-output*, "</TT>");
    13 =>
      write(*standard-output*, "<U>");
      document(depth + 1);
      write(*standard-output*, "</U>");
    14 =>
      write(*standard-output*, "<0>");
      document(depth + 1);
      write(*standard-output*, "</0>");
    15 =>
      write(*standard-output*, "<1>");
      document(depth + 1);
      write(*standard-output*, "</1>");
    16 =>
      write(*standard-output*, "<2>");
      document(depth + 1);
      write(*standard-output*, "</2>");
    17 =>
      write(*standard-output*, "<3>");
      document(depth + 1);
      write(*standard-output*, "</3>");
    18 =>
      write(*standard-output*, "<4>");
      document(depth + 1);
      write(*standard-output*, "</4>");
    19 =>
      write(*standard-output*, "<5>");
      document(depth + 1);
      write(*standard-output*, "</5>");
    20 =>
      write(*standard-output*, "<6>");
      document(depth + 1);
      write(*standard-output*, "</6>");
    21 =>
      write(*standard-output*, "<7>");
      document(depth + 1);
      write(*standard-output*, "</7>");
    22 =>
      write(*standard-output*, "<8>");
      document(depth + 1);
      write(*standard-output*, "</8>");
    23 =>
      write(*standard-output*, "<9>");
      document(depth + 1);
      write(*standard-output*, "</9>");
    24 =>
      write(*standard-output*, "<r>");
      document(depth + 1);
      write(*standard-output*, "</r>");
    25 =>
      write(*standard-output*, "<g>");
      document(depth + 1);
      write(*standard-output*, "</g>");
    26 =>
      write(*standard-output*, "<b>");
      document(depth + 1);
      write(*standard-output*, "</b>");
    27 =>
      write(*standard-output*, "<c>");
      document(depth + 1);
      write(*standard-output*, "</c>");
    28 =>
      write(*standard-output*, "<m>");
      document(depth + 1);
      write(*standard-output*, "</m>");
    29 =>
      write(*standard-output*, "<y>");
      document(depth + 1);
      write(*standard-output*, "</y>");
    30 =>
      write(*standard-output*, "<k>");
      document(depth + 1);
      write(*standard-output*, "</k>");
    31 =>
      write(*standard-output*, "<w>");
      document(depth + 1);
      write(*standard-output*, "</w>");
  end select;
  if(random(depth) < 2)
    document(depth + 1);
  end if;
end function;

define function main(name, arguments)
  document(2);
  write-element(*standard-output*, '\n');
  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
