module:   sorting
synopsis: A driver to test a number of different sorting algorithms
author:   Peter Hinely

/////////////////////////////////  random /////////////////////////////////
define constant $IM = 139968;
define constant $IA = 3877;
define constant $IC = 29573;

// use a closure to keep the state of the "last" variable
define function random-generator (seed :: <integer>) => random-generator :: <method>;
  let last = seed;

  method (maximum :: <double-float>) => result :: <double-float>;
    last := modulo((last * $IA + $IC), $IM);
    maximum * last / $IM;
  end;
end;


define function main () => ()
  let args = application-arguments();
  let test-number = string-to-integer(element(args, 0, default: "999"));
  let sz = string-to-integer(element(args, 1, default: "1"));
  let n = string-to-integer(element(args, 2, default: "1"));

  if ((args.size ~== 3) | (test-number == 999))
    format-out("usage: sorting algorithm-number size-of-sequence number-of-iterations");
    exit-application(0);
  end;

  let generate-random = random-generator(42);
  
  format-out("algorithm #%d: ", test-number);

  let input :: <list> = #();
  for (i from 0 below sz)
    input := add!(input, floor(generate-random(1000.0)));  // integers
    // input := add!(input, generate-random(100.0));  // floats
  end;

  let result = #f;
  //format-out("before = %=\n", input);

  let test = \<;
  //let test = method (a :: <integer>, b :: <integer>) a < b end;

  select (test-number)
    1 =>
      format-out("sort!\n");
      for (i from 1 to n)
        result := sort!(input, test: test);
      end;    
    2 =>
      format-out("sort! with stable: #t\n");
      for (i from 1 to n)
        result := sort!(input, test: test, stable: #t);
      end;    
    3 =>
      format-out("insertion-sort!\n");
      for (i from 1 to n)
        result := insertion-sort!(input, test: test);
      end;
    4 =>
      format-out("binary-insertion-sort!\n");
      for (i from 1 to n)
        result := binary-insertion-sort!(input, test: test);
      end;
    5 =>
      format-out("merge-sort!\n");
      for (i from 1 to n)
        result := merge-sort!(input, test: test);
      end;
    6 =>
      format-out("weak-heap-sort!\n");
      for (i from 1 to n)
        result := weak-heap-sort!(input, test: test);
      end;
    7 =>
      format-out("comb-sort-11!\n");
      for (i from 1 to n)
        result := comb-sort-11!(input, test: test);
      end;
    8 =>
      format-out("comb-sort-predefined!\n");
      for (i from 1 to n)
        result := comb-sort-predefined!(input, test: test);
      end;
  end select;    

  format-out("size of collection: %d\n", sz);
  format-out("repetitions: %d\n", n);

  // sanity checks
  if (result.size ~== sz)
    error("Something is horribly wrong with the sorting.\nThe output size does not have the same number of elements as input. Input had %d elements. Output has %d elements.", sz, result.size);
  end;

  if (n < 10)
    format-out("sanity check skipped because number of iterations is small and it will significantly affect the timing\n");
  else
    let qa-sorted-result = sort!(input);
    if (result = qa-sorted-result)
      format-out("sanity check PASSED\n");
    else
      format-out("sanity check FAILED\n");
      format-out("input\t\tcorrectly-sorted input\t\toutput of sorting function under test\n");
      for (elt1 in input, elt2 in qa-sorted-result, elt3 in result)
        format-out("%=\t\t%=\t\t%=\n", elt1, elt2, elt3);
      end;
      error("Something is horribly wrong with the sorting. The output elements do not match the input elements.");    
    end;
  end if;

  exit-application(0);
end function;

// not currently used but should be worked into the framework
define function additional-tests (sort-algorithm :: <function>) => ()
  // tests
  format-out("%=\n", sort-algorithm(vector()));

  format-out("%=\n", sort-algorithm(vector(1)));

  format-out("%=\n", sort-algorithm(vector(1, 2)));
  format-out("%=\n", sort-algorithm(vector(2, 1)));

  format-out("%=\n", sort-algorithm(vector(1, 2, 3)));
  format-out("%=\n", sort-algorithm(vector(1, 3, 2)));
  format-out("%=\n", sort-algorithm(vector(2, 1, 3)));
  format-out("%=\n", sort-algorithm(vector(2, 3, 1))); 
  format-out("%=\n", sort-algorithm(vector(3, 1, 2)));
  format-out("%=\n", sort-algorithm(vector(3, 2, 1))); 

  // stability tests
  let test = \<;
  format-out("%=\n", sort-algorithm(vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), test: test));
  format-out("%=\n", sort!(vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), test: test));
  format-out("%=\n", sort!(vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), test: test, stable: #t));
  format-out("\n");
  
  let test = method (a, b) #t end;
  format-out("%=\n", sort-algorithm(vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), test: test));
  format-out("%=\n", sort!(vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), test: test));
  format-out("%=\n", sort!(vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), test: test, stable: #t));
  format-out("\n");

  let test = method (a, b) #f end;
  format-out("%=\n", sort-algorithm(vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), test: test));
  format-out("%=\n", sort!(vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), test: test));
  format-out("%=\n", sort!(vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), test: test, stable: #t));
  format-out("\n");
end function;

main();
