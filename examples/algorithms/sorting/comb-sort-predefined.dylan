module:   sorting
synopsis: comb sort with pre-defined gap table
author:   Peter Hinely

// implementation of Jim Veale's "An Improved Comb Sort with Pre-Defined Gap Table"
// from http://world.std.com/~jdveale/combsort.htm

define constant <int-vector> = limited(<simple-vector>, of: <integer>);

//
//   pre-defined gap table for sorts of up to 4 million items
//

define constant gab-table :: <int-vector> = map-into(make(<int-vector>, size: 69), identity,
                 #[1,1,2,3,4,5,6,7,8,  // 1.247330950103979
                   9,    //*       9.40    1.5423
                  11,    //       11.72    1.6818
                  13,    //       14.62    1.7877
                  17,    //       18.23    1.9284
                  22,    //*      22.74    2.0284
                  26,    //*      28.37    2.0865
                  34,    //*      35.38    2.1680
                  43,    //       44.13    2.2243
                  53,    //       55.05    2.2696
                  67,    //       68.67    2.3153
                  83,    //       85.65    2.3529
                 103,    //      106.83    2.3885
                 131,    //      133.25    2.4235
                 163,    //      166.21    2.4519
                 206,    //*     207.32    2.4794
                 257,    //      258.60    2.5031
                 317,    //      322.56    2.5249
                 401,    //      402.34    2.5476
                 499,    //      501.85    2.5668
                 622,    //*     625.97    2.5853
                 778,    //*     780.79    2.6029
                 971,    //      973.91    2.6193
                1213,    //     1214.78    2.6347
                1511,    //     1515.24    2.6492
                1889,    //     1890.00    2.6631
                2357,    //     2357.46    2.6761
                2939,    //     2940.53    2.6884
                3662,    //*    3667.82    2.7002
                4567,    //     4574.98    2.7115
                5701,    //     5706.52    2.7222
                7114,    //*    7117.92    2.7324
                8871,    //*    8878.40    2.7421
               11071,    //    11074.30    2.7514
               13807,    //    13813.32    2.7602
               17229,    //*   17229.78    2.7687
               21491,    //    21491.23    2.7767
               26801,    //    26806.68    2.7845
               33427,    //    33436.80    2.7919
               41698,    //*   41706.76    2.7990
               52021,    //    52022.13    2.8059
               64879,    //    64888.81    2.8124
               80933,    //    80937.83    2.8188
              100948,    //*  100956.25    2.8248
              125921,    //   125925.86    2.8307
              157061,    //   157071.22    2.8363
              195919,    //   195919.80    2.8417
              244367,    //   244376.83    2.8470
              304813,    //   304818.78    2.8520
              380207,    //   380209.90    2.8569
              474241,    //   474247.58    2.8617
              591538,    //*  591543.68    2.8662
              737843,    //   737850.74    2.8706
              920333,    //   920344.07    2.8749
             1147969,    //  1147973.64    2.8790
             1431894,    //* 1431903.05    2.8831
             1786046,    //* 1786056.99    2.8869
             2227801,    //  2227804.16    2.8907
             2778799,    //  2778809.08    2.8944
             3466082,    //* 3466094.57    2.8979
          2147483647]);


define inline function comb-sort-predefined!-internal
    (data :: <simple-object-vector>, sz :: <integer>, test :: <function>)
 => (sorted-vector :: <simple-object-vector>)
  let swapped = #t;
  while (swapped)
    let gap-index = 0;
    while (gab-table[gap-index] < sz)
      gap-index := gap-index + 1;
    end;

    while (gap-index > 0)
      swapped := #f;
      gap-index := gap-index - 1;
      let gap = gab-table[gap-index];

      for (i from 0 below (sz - gap))
        let j = i + gap;
        let elt1 = data[i];
        let elt2 = data[j];
        if (test(elt2, elt1))
          data[i] := elt2;
          data[j] := elt1;
          swapped := #t;
        end;
      end;
    end;
  end while;
  data;
end function;

define function comb-sort-predefined!
    (sequence :: <sequence>, #key test :: <function> = \<)
 => (new-sequence :: <sequence>)
  let a = as(<simple-object-vector>, sequence);
  let n :: <integer> = sequence.size;

  if (n < 2)
    sequence;
  else
    as(type-for-copy(sequence), comb-sort-predefined!-internal(a, n, test));
  end if;
end function;