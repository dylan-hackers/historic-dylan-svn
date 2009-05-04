module: wrapper-streams-tester
author: Dustin Voss 

define constant $base-stream-contents = "aabbccddeeffgg";
define constant $base-stream =
      make(<string-stream>, contents: $base-stream-contents);


define constant $text-stream-contents =
      "Hello.\n"
      "My name is Inigo Montoya.\r"
      "You killed my father.\r\n"
      "Prepare to die!";
define constant $text-stream =
      make(<string-stream>, contents: $text-stream-contents);


define constant $tabbed-text =
      "\aInigo Montoya:\r\n"
      "\tHello.\tMy name is...";
define constant $tabbed-text-stream =
      make(<string-stream>, contents: $tabbed-text);


define suite wrapper-streams-suite ()
   suite basic-wrapper-stream-suite;
   suite replacing-stream-suite;
   suite canonical-text-stream-suite;
end suite;

define suite basic-wrapper-stream-suite ()
   test read-test;
   test read-into!-test;
   test write-test;
   test read-to-test;
   test read-through-test;
   test read-to-end-test;
   test skip-through-test;
   test read-line-test;
   test read-line-into!-test;
   test read-text-test;
   test read-text-into!-test;
   test write-line-test;
   test new-line-test;
end suite;

define suite replacing-stream-suite ()
   suite replacing-suite;
   suite reading-suite;
   suite position-suite;
   suite inner-position-suite;
end suite;

define suite replacing-suite ()
   test replace-at-start;
   test replace-in-middle;
   test replace-at-end;
   test replace-past-end;
   test insert-at-start;
   test insert-in-middle;
   test insert-past-end;
   test sequential-replacements;
   test empty-replacements;
   test no-stretchy-replacements;
   test no-addl-replacements;
   test no-illegal-elements;
   suite post-replacement-position-suite;
end suite;

define suite post-replacement-position-suite ()
   test replaced-position-unchanged-by-equal-replacement;
   test replaced-position-pushed-by-smaller-replacement;
   test replaced-position-unchanged-by-larger-replacement;
   test later-position-pulled-by-smaller-replacement;
   test later-position-pushed-by-larger-replacement;
end suite;

define suite reading-suite ()
   test read-larger-replacement;
   test read-smaller-replacement;
   test read-empty-replacement;
   test read-inner-stream;
   test read-entire-contents;
end suite;

define suite position-suite ()
   test replacing-stream-size;
   test position-in-replacement;
   test position-in-inner-stream;
   test dec-position;
   test inc-position;
   test dec-position-across-boundary;
   test inc-position-across-boundary;
   test inc-position-across-empties;
   test dec-position-across-empties;
end suite;

define suite inner-position-suite ()
   test one-to-one-inner-pos;
   test one-to-many-inner-pos;
   test zero-to-many-inner-pos;
   test zero-to-many-at-start-inner-pos;
   test zero-to-many-past-end-inner-pos;
   test many-to-one-inner-pos;
   test many-to-zero-inner-pos;
   test many-to-zero-at-end-inner-pos;
   test before-replacement-inner-pos;
   test between-replacement-inner-pos;
   test after-replacement-inner-pos;
end suite;

define suite canonical-text-stream-suite ()
   test cts-works;
end suite;
