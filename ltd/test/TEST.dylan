define class <point-record> (<graphics-record>)
  slot x, init-keyword: #"x";
  // ---*** all required-init-arg
  slot y, init-keyword: #"y";
end class <point-record>;

//  Many of these tests are from the DIRM (Dylan Interim Reference Manual)
begin
  foo;
  \=;
  123;
  1.5;
  -4.0;
  57;
  "abc";
  #"hello";
  #(1, 2, 3);
  #[1, 2, 3];
  'm';
  // these are from pages 7 and 8
  #t;
  // a multi-line
  // comment
  #f;
end;

define variable my-variable = 25;

begin let x = 50; x + x; end;

// following from pages 12-14
(method (x) x + 1; end method)(99);

method (a :: <number>, b :: <number>) list(a - b, a + b); end method;

if (moon-phase == #"full")
  wolf-form(werewolf);
  howl-at-moon(werewolf);
else
  human-form(werewolf);
end if;

define constant vect = #[7, 8, 9];

// page 21
begin let foo = 20; let foo = 50; foo + foo; end;

begin
  let foooooooooooooooooo = 20;
  let foooooooooooooooooo = 50;
  foooooooooooooooooo + foooooooooooooooooo;
end;

begin let x :: <integer> = sqrt(2); x; end;

let (foo, bar, baz) = values(1, 2, 3); list(foo, bar, baz);

define method opposite-sides (center :: <number>, radius :: <number>)
  let (min, max) = edges(center, radius);
  values(max, min);
end method opposite-sides;

// page 29
if (~ detect-gas?(nose)) light(match); end if;

if (player1.money <= 0)
  end-game(player1);
elseif (player2.money <= 0)
  end-game(player2);
else
  move(player1);
  move(player2);
end if;

// page 34
begin
  for (thing = first-thing then next(thing), until done?(thing))
    do-some(thing);
  end for;
  for (city in olympic-cities, year from start-year by 4)
    schedule-olympics(city, year);
  finally
    notify(press);
    sell(tickets);
    #f;
  end for;
  for (i from 0 below 100, zombies from 0 below 100,
       normals from 100 above 0 by --(-1))
    population[i] := zombies + normals;
  end for;
end;

define constant foo =
  block (return-from-bar) method (n) return-from-bar(n); end method; end block;

define method double (my-method :: <function>)
  method (#rest args)
    apply(my-method, args);
    apply(my-method, args);
    #f;
  end method;
end method double;

define method root-mean-square (s :: <sequence>)
  local method average (nums)
          reduce1(\+, nums) / size(nums);
        end method average,
        method square (n) n * n; end method square;
  sqrt(average(map(square, s)));
end method root-mean-square;

define class <sentient> (<life-form>); end class <sentient>;

define class <vulcan> (<intelligent>, <humanoid>); end class <vulcan>;

define class <animal> (<object>) slot n-legs = 4; end class <animal>;

define class <spider> (<animal>) slot n-legs = 0; end class <spider>;

//  page 160
block (nil) begin open-files(); result; end; cleanup close-files(); end block;

//  Not from the DIRM
define method f (a, b) => (integer); 42; end method f;

define method g (a, b) values(42, a + b); end method g;

literal(#(1, #"key", #(2, 3), 'c', 3.0, 0.6666667,
          #(#"+", 2, #(#"*", 3, #"$i")),
          #[4, 0.6666667, #(#"+", 2, #(#"*", 3, #"$i")), #"sym", #"key",
            "string"]));

a / (n + 1) + b * f(~ x, - y) == 2 ^ (2 ^ n) & (a[i] > m[i, j] | a.b = c.d);

check-keyword-args(f(#"only-arg"), x: 1, y: 2, z: #"last-one");

begin
  v1 := (always(a + b))(x);
  v2.slot := (compose(f, g))(x);
  a[i, j] := 0;
end;

for (until 3 == 4)
  for (while 5 == 6)
    if (~ (7 == 8))
      if (9 == 10)
        eleven();
        twelve();
      elseif (13 == 14)
        say("huh?");
        change(13, 14);
      elseif (a > b | c < d)
        f(a, b, c, d);
      else
        42;
        give(up);
      end if;
    end if;
  end for;
end for;

if (a == b)
  f(a, b);
  g(a, b);
elseif (a > b | a < b)
  f(g(a, b), g(b, a));
end if;

for (while c * (a + b) > z & z ^ 2 <= 0)
  f(m[i, j], a[i], 3 + 4 * (xyz - 34));
end for;

begin
  fff(1, 2, element(a, i, default: 0));
  //  inner
  subsequence-position('c', "string");
end;

//  This is a multiple
// line comment.
// This is a test to see what happens when the comment is very long and goes over the right margin. 
write(*standard-output*,
      "a string with a tab (\t), a newline (\n), a backslash (\\), and a \"funny\" character (\0127).");

define class <struct> (<object>)
  slot struct-slot1, init-keyword: #"struct-slot1";
  slot struct-slot2, init-keyword: #"struct-slot2";
end class <struct>;

define class <struct2> (<struct>)
  slot struck-slot3, init-keyword: #"struck-slot3";
  slot struck-slot4 :: <integer> = 42, init-keyword: #"struck-slot4";
end class <struct2>;

//  Test for read errors:
test1();

#f;

// 1
// 2
// 3
test2();

test3();

#(// a
  1 . // c
  2);

#();

#f;

//  Packages:
"(in-package system)";

exit;

