module: crypto
Author: Hannes Mehnert <hannes@mehnert.org>

//define constant $block-size :: <integer> = 64;

//define variable *T* :: <stretchy-vector> = make(<stretchy-vector>);

define method md5 (plain-text :: <byte-string>) => (result :: <byte-string>)
//  let result = make(<byte-string>, size: 32);
//  let cons /*:: <double-integer>*/ = 4294967296;
//  for (i from 1 to 64)
//    *T*[i] := as(<integer>, truncate(cons * abs(sin(i))));
//  end for;
//  let myt :: <list> = #(#xd76aa478, #xe8c7b756, #x242070db, #xc1bdceee, #xf57c0faf, #x4787c62a, #xa8304613, #xfd469501, #x698098d8, #x8b44f7af, #xffff5bb1, #x895cd7be, #x6b901122, #xfd987193, #xa679438e, #x49b40821, #xf61e2562, #xc040b340, #x265e5a51, #xe9b6c7aa, #xd62f105d, #x2441453, #xd8a1e681, #xe7d3fbc8, #x21e1cde6, #xc33707d6, #xf4d50d87, #x455a14ed, #xa9e3e905, #xfcefa3f8, #x676f02d9, #x8d2a4c8a, #xfffa3942, #x8771f681, #x6d9d6122, #xfde5380c, #xa4beea44, #x4bdecfa9, #xf6bb4b60, #xbebfbc70, #x289b7ec6, #xeaa127fa, #xd4ef3085, #x4881d05, #xd9d4d039, #xe6db99e5, #x1fa27cf8, #xc4ac5665, #xf4292244, #x432aff97, #xab9423a7, #xfc93a039, #x655b59c3, #x8f0ccc92, #xffeff47d, #x85845dd1, #x6fa87e4f, #xfe2ce6e0, #xa3014314, #x4e0811a1, #xf7537e82, #xbd3af235, #x2ad7d2bb, #xeb86d391);
//  format-out("as integer(foo): %d", as(<integer>, 'f'));
//  format-out("%d", blocks[1]);
//  format-out("cons: %d\n", cons);
  for (i from 1 below 64)
//    format-out("%d: %x", i, *T*[i]);
    let bar = 4294967296 * sin(as(<double-float>, i));
//    let fbar  = mytruncate(bar);
    let (fobar, fnord) = floor(bar);
//    let (foobar, nan) = truncate(bar);
//    format-out(" sin(i) %d cons * abs(sin) \n%d\n",
//               sin(as(<double-float>, i)), foo);
//    let bar = mytruncate(foo);
    format-out("%d\n", fobar);
//    format-out(" %d\n", myt[i - 1]);
  end for;
  //Step 1. Append Padding Bits
/*  let text = pad(copy-sequence(plain-text, start: floor/(plain-text.size, $block-size) * $block-size));

  let words = string-to-integer-vector(concatenate(plain-text, text));
  //Step 2. Append Length
  add-size!(words, plain-text.size);

  //format-out("%=\n", words);
  //Step 3. Initialize MD Buffer
  let a :: <double-integer> = #x01234567;
  let b :: <double-integer> = #x89ABCDEF;
  let c :: <double-integer> = #xFEDCBA98;
  let d :: <double-integer> = #x76543210;

  //Step 4. Process Message in 16-Word Blocks
  let count = floor/(words.size, 16);
  for (i from 0 to count - 1) */
    /* Save A as AA, B as BB, C as CC, and D as DD. */
//    let aa = a;
//    let bb = b;
//    let cc = c;
//    let dd = d;

    /* Round 1. */
    /* Let [abcd k s i] denote the operation
         a = b + ((a + F(b,c,d) + X[k] + T[i]) <<< s). */
    /* Do the following 16 operations. */
/*    md5-round(F,
    A B C D  0  7  1; D A B C  1 12  2; C D A B  2 17  3; B C D A  3 22  4;
    A B C D  4  7  5; D A B C  5 12  6; C D A B  6 17  7; B C D A  7 22  8;
    A B C D  8  7  9; D A B C  9 12 10; C D A B 10 17 11; B C D A 11 22 12;
    A B C D 12  7 13; D A B C 13 12 14; C D A B 14 17 15; B C D A 15 22 16;
    );
  *///  format-out("a %x b %x c %x d %x\n", a, b, c, d);
    /* Round 2. */
    /* Let [abcd k s i] denote the operation
         a = b + ((a + G(b,c,d) + X[k] + T[i]) <<< s). */
    /* Do the following 16 operations.
    [A B C D  1  5 17] [D A B C  6  9 18] [C D A B 11 14 19] [B C D A  0 20 20]
    [A B C D  5  5 21] [D A B C 10  9 22] [C D A B 15 14 23] [B C D A  4 20 24]
    [A B C D  9  5 25] [D A B C 14  9 26] [C D A B  3 14 27] [B C D A  8 20 28]
    [A B C D 13  5 29] [D A B C  2  9 30] [C D A B  7 14 31] [B C D A 12 20 32]
    */
    /* Round 3. */
    /* Let [abcd k s t] denote the operation
         a = b + ((a + H(b,c,d) + X[k] + T[i]) <<< s). */
    /* Do the following 16 operations.
    [A B C D  5  4 33] [D A B C  8 11 34] [C D A B 11 16 35] [B C D A 14 23 36]
    [A B C D  1  4 37] [D A B C  4 11 38] [C D A B  7 16 39] [B C D A 10 23 40]
    [A B C D 13  4 41] [D A B C  0 11 42] [C D A B  3 16 43] [B C D A  6 23 44]
    [A B C D  9  4 45] [D A B C 12 11 46] [C D A B 15 16 47] [B C D A  2 23 48]
    */
    /* Round 4. */
    /* Let [abcd k s t] denote the operation
         a = b + ((a + I(b,c,d) + X[k] + T[i]) <<< s). */
    /* Do the following 16 operations. 
    [A B C D  0  6 49] [D A B C  7 10 50] [C D A B 14 15 51] [B C D A  5 21 52]
    [A B C D 12  6 53] [D A B C  3 10 54] [C D A B 10 15 55] [B C D A  1 21 56]
    [A B C D  8  6 57] [D A B C 15 10 58] [C D A B  6 15 59] [B C D A 13 21 60]
    [A B C D  4  6 61] [D A B C 11 10 62] [C D A B  2 15 63] [B C D A  9 21 64]
    */
    /* Then perform the following additions. (That is increment each
       of the four registers by the value it had before this block
       was started.) */
 //   a := a + aa;
 //   b := b + bb;
 //   c := c + cc;
 //   d := d + dd;

 // end for;

  //Step 5. Output
 // result;
end method md5;
/*
define macro md5-round
 { md5-round (?fun, ?clauses:*) } => { ?clauses }
  //a = b + ((a + fun(b,c,d) + k + T[i]) <<< s)
  clauses:
    { } => { }
    { ?a ?b ?c ?d ?k ?s ?i; ... }
      => { a := b + (ash(a + fun(b, c, d) + k + $T[i], s)); ... }
end macro;

define function F (X :: <integer>, Y :: <integer>, Z :: <integer>)
    => (return :: <integer>)
  //XY v not(X) Z
  logior(logand(X, Y), logand(Z, lognot(X)));
end function F;

define function G (X :: <integer>, Y :: <integer>, Z :: <integer>)
    => (return :: <integer>)
  //XZ v Y not(Z)
  logior(logand(X, Z), logand(Y, lognot(Z)));
end function G;

define function H (X :: <integer>, Y :: <integer>, Z :: <integer>)
    => (return :: <integer>)
  //X xor Y xor Z
  logxor(X, Y, Z);
end function H;

define function I (X :: <integer>, Y :: <integer>, Z :: <integer>)
    => (return :: <integer>)
  //Y xor (X v not(Z))
  logxor(Y, logior(X, lognot(Z)));
end function I;

define method string-to-integer-vector
    (block-text :: <byte-string>) => (ret :: <simple-object-vector>)
  let ret = make(<simple-object-vector>,
                 size: ceiling/(block-text.size, 4) + 2);
//  format-out("string-to-integer-deque: size: %d\n", block-text.size);
  for (j :: <integer> from 0 below ceiling/(block-text.size, 4))
    let block-content :: <integer> = 0;
    for (k :: <integer> from 0 below 4)
      block-content := ash(block-content, 8);
      block-content := block-content + as(<integer>, block-text[j * 4 + k]);
    end for;
//    format-out("j: %d, block-content: %d\n", j, block-content);
    ret[j] := block-content;
  end for;
  ret;
end method string-to-integer-vector;

define method pad(block-text :: <byte-string>) => (result :: <byte-string>)
  let result = make(<byte-string>,
                    size: $block-size - 8 - block-text.size,
                    fill: as(<byte-character>, #x00));
  if (block-text.size >= 56)
    //we add another 512 bit block
    result := make(<byte-string>,
                   size: 2 * $block-size - 8 - block-text.size,
                   fill: as(<byte-character>, #x00));
  end if;
  result[0] := as(<byte-character>, #x80);
//  format-out("padded text: %= size: %d\n", result, result.size);
  result;
end method pad;

define method add-size!(words :: <simple-object-vector>, length :: <integer>)
  words[words.size - 2] := 0;
  words[words.size - 1] := length;
//  format-out("add-size!: size: %d\n", length);
end method add-size!;
*/
md5("23foobkl;dkl;k;lk;lk;lk;lk;arbarfooobarfoobar23hkjdnkjnkjnjknkjnkjnkj");
md5("12345678901234567890123456789012345678901234567890123456");
md5("1234567890123456789012345678901234567890123456789012345678901234");
md5("");
