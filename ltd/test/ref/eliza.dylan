//  -*- Mode: Lisp; Syntax: Common-Lisp; -*-
//  Code from Paradigms of Artificial Intelligence Programming
//  Copyright (c) 1991 Peter Norvig
//  File eliza.lisp: Advanced version of Eliza.
//  Has more rules, and accepts input without parens.
requires("eliza1");

//  ==============================
define method read-line-no-punct ()
  // Read an input line, ignoring punctuation.
  // LTD: Function READ-FROM-STRING not yet implemented.
  read-from-string(concatenate-as(<string>, "(",
                                  replace-elements(read-line(*standard-input*,
                                                             nil),
                                                   punctuation-p,
                                                   always(' ')),
                                  ")"));
end method read-line-no-punct;

define method punctuation-p (char)
  cl-find(char, ".,;:`!?#-()\\\"");
end method punctuation-p;

//  ==============================
define method eliza ()
  // Respond to user input using pattern matching rules.
  block (return)
    while (#t)
      print(#"eliza>", *standard-output*);
      let input = read-line-no-punct();
      let response = flatten(use-eliza-rules(input));
      print-with-spaces(response);
      if (response = #(#"good", #"bye")) return(#f); end if;
    end while;
  end block;
end method eliza;

define method print-with-spaces (list)
  begin
    do(method (x)
         print(x, *standard-output*);
         print(" ", *standard-output*);
       end method,
       list);
    list;
  end;
end method print-with-spaces;

define method print-with-spaces (list)
  (method (s, #rest args)
     apply(maybe-initiate-xp-printing,
           method (xp, #rest args)
             let args = pop!(args);
             block (return)
               local method go-l ()
                       if (empty?(args)) return(#f); end if;
                       fluid-bind (*print-escape* = #f)
                         write+(pop!(args), xp);
                       end fluid-bind;
                       write-char++(' ', xp);
                       go-l();
                     end method go-l;
               go-l();
             end block;
             if (args) copy-sequence(args); end if;
           end method,
           s, args);
   end method)(#t, list);
end method print-with-spaces;

//  ==============================
define method mappend (fn, #rest lists)
  // Apply fn to each element of lists and append the results.
  apply(concatenate,
        apply(// LTD: Can't convert complex function MAPCAR.
              mapcar, fn, lists));
end method mappend;

//  ==============================
define variable *eliza-rules* =
  #(#(#(#(#"?*", #"?x"), #"hello", #(#"?*", #"?y")),
      #(#"how", #"do", #"you", #"do.", #"please", #"state", #"your",
        #"problem.")),
    #(#(#(#"?*", #"?x"), #"computer", #(#"?*", #"?y")),
      #(#"do", #"computers", #"worry", #"you?"),
      #(#"what", #"do", #"you", #"think", #"about", #"machines?"),
      #(#"why", #"do", #"you", #"mention", #"computers?"),
      #(#"what", #"do", #"you", #"think", #"machines", #"have", #"to", #"do",
        #"with", #"your", #"problem?")),
    #(#(#(#"?*", #"?x"), #"name", #(#"?*", #"?y")),
      #(#"i", #"am", #"not", #"interested", #"in", #"names")),
    #(#(#(#"?*", #"?x"), #"sorry", #(#"?*", #"?y")),
      #(#"please", #"don", #(#"quote", #"t"), #"apologize"),
      #(#"apologies", #"are", #"not", #"necessary"),
      #(#"what", #"feelings", #"do", #"you", #"have", #"when", #"you",
        #"apologize")),
    #(#(#(#"?*", #"?x"), #"i", #"remember", #(#"?*", #"?y")),
      #(#"do", #"you", #"often", #"think", #"of", #"?y"),
      #(#"does", #"thinking", #"of", #"?y", #"bring", #"anything", #"else",
        #"to", #"mind?"),
      #(#"what", #"else", #"do", #"you", #"remember"),
      #(#"why", #"do", #"you", #"recall", #"?y", #"right", #"now?"),
      #(#"what", #"in", #"the", #"present", #"situation", #"reminds", #"you",
        #"of", #"?y"),
      #(#"what", #"is", #"the", #"connection", #"between", #"me", #"and",
        #"?y")),
    #(#(#(#"?*", #"?x"), #"do", #"you", #"remember", #(#"?*", #"?y")),
      #(#"did", #"you", #"think", #"i", #"would", #"forget", #"?y", #"?"),
      #(#"why", #"do", #"you", #"think", #"i", #"should", #"recall", #"?y",
        #"now"),
      #(#"what", #"about", #"?y"), #(#"you", #"mentioned", #"?y")),
    #(#(#(#"?*", #"?x"), #"if", #(#"?*", #"?y")),
      #(#"do", #"you", #"really", #"think", #"its", #"likely", #"that",
        #"?y"),
      #(#"do", #"you", #"wish", #"that", #"?y"),
      #(#"what", #"do", #"you", #"think", #"about", #"?y"),
      #(#"really--", #"if", #"?y")),
    #(#(#(#"?*", #"?x"), #"i", #"dreamt", #(#"?*", #"?y")),
      #(#"really--", #"?y"),
      #(#"have", #"you", #"ever", #"fantasized", #"?y", #"while", #"you",
        #"were", #"awake?"),
      #(#"have", #"you", #"dreamt", #"?y", #"before?")),
    #(#(#(#"?*", #"?x"), #"dream", #"about", #(#"?*", #"?y")),
      #(#"how", #"do", #"you", #"feel", #"about", #"?y", #"in", #"reality?")),
    #(#(#(#"?*", #"?x"), #"dream", #(#"?*", #"?y")),
      #(#"what", #"does", #"this", #"dream", #"suggest", #"to", #"you?"),
      #(#"do", #"you", #"dream", #"often?"),
      #(#"what", #"persons", #"appear", #"in", #"your", #"dreams?"),
      #(#"don", #(#"quote", #"t"), #"you", #"believe", #"that", #"dream",
        #"has", #"to", #"do", #"with", #"your", #"problem?")),
    #(#(#(#"?*", #"?x"), #"my", #"mother", #(#"?*", #"?y")),
      #(#"who", #"else", #"in", #"your", #"family", #"?y"),
      #(#"tell", #"me", #"more", #"about", #"your", #"family")),
    #(#(#(#"?*", #"?x"), #"my", #"father", #(#"?*", #"?y")),
      #(#"your", #"father"),
      #(#"does", #"he", #"influence", #"you", #"strongly?"),
      #(#"what", #"else", #"comes", #"to", #"mind", #"when", #"you", #"think",
        #"of", #"your", #"father?")),
    #(#(#(#"?*", #"?x"), #"i", #"want", #(#"?*", #"?y")),
      #(#"what", #"would", #"it", #"mean", #"if", #"you", #"got", #"?y"),
      #(#"why", #"do", #"you", #"want", #"?y"),
      #(#"suppose", #"you", #"got", #"?y", #"soon")),
    #(#(#(#"?*", #"?x"), #"i", #"am", #"glad", #(#"?*", #"?y")),
      #(#"how", #"have", #"i", #"helped", #"you", #"to", #"be", #"?y"),
      #(#"what", #"makes", #"you", #"happy", #"just", #"now"),
      #(#"can", #"you", #"explain", #"why", #"you", #"are", #"suddenly",
        #"?y")),
    #(#(#(#"?*", #"?x"), #"i", #"am", #"sad", #(#"?*", #"?y")),
      #(#"i", #"am", #"sorry", #"to", #"hear", #"you", #"are", #"depressed"),
      #(#"i", #(#"quote", #"m"), #"sure", #"its", #"not", #"pleasant", #"to",
        #"be", #"sad")),
    #(#(#(#"?*", #"?x"), #"are", #"like", #(#"?*", #"?y")),
      #(#"what", #"resemblance", #"do", #"you", #"see", #"between", #"?x",
        #"and", #"?y")),
    #(#(#(#"?*", #"?x"), #"is", #"like", #(#"?*", #"?y")),
      #(#"in", #"what", #"way", #"is", #"it", #"that", #"?x", #"is", #"like",
        #"?y"),
      #(#"what", #"resemblance", #"do", #"you", #"see?"),
      #(#"could", #"there", #"really", #"be", #"some", #"connection?"),
      #(#"how?")),
    #(#(#(#"?*", #"?x"), #"alike", #(#"?*", #"?y")),
      #(#"in", #"what", #"way?"),
      #(#"what", #"similarities", #"are", #"there?")),
    #(#(#(#"?*", #"?x"), #"same", #(#"?*", #"?y")),
      #(#"what", #"other", #"connections", #"do", #"you", #"see?")),
    #(#(#(#"?*", #"?x"), #"i", #"was", #(#"?*", #"?y")),
      #(#"were", #"you", #"really?"),
      #(#"perhaps", #"i", #"already", #"knew", #"you", #"were", #"?y"),
      #(#"why", #"do", #"you", #"tell", #"me", #"you", #"were", #"?y",
        #"now?")),
    #(#(#(#"?*", #"?x"), #"was", #"i", #(#"?*", #"?y")),
      #(#"what", #"if", #"you", #"were", #"?y", #"?"),
      #(#"do", #"you", #"thin", #"you", #"were", #"?y"),
      #(#"what", #"would", #"it", #"mean", #"if", #"you", #"were", #"?y")),
    #(#(#(#"?*", #"?x"), #"i", #"am", #(#"?*", #"?y")),
      #(#"in", #"what", #"way", #"are", #"you", #"?y"),
      #(#"do", #"you", #"want", #"to", #"be", #"?y", #"?")),
    #(#(#(#"?*", #"?x"), #"am", #"i", #(#"?*", #"?y")),
      #(#"do", #"you", #"believe", #"you", #"are", #"?y"),
      #(#"would", #"you", #"want", #"to", #"be", #"?y"),
      #(#"you", #"wish", #"i", #"would", #"tell", #"you", #"you", #"are",
        #"?y"),
      #(#"what", #"would", #"it", #"mean", #"if", #"you", #"were", #"?y")),
    #(#(#(#"?*", #"?x"), #"am", #(#"?*", #"?y")),
      #(#"why", #"do", #"you", #"say", "AM?"),
      #(#"i", #"don", #(#"quote", #"t"), #"understand", #"that")),
    #(#(#(#"?*", #"?x"), #"are", #"you", #(#"?*", #"?y")),
      #(#"why", #"are", #"you", #"interested", #"in", #"whether", #"i", #"am",
        #"?y", #"or", #"not?"),
      #(#"would", #"you", #"prefer", #"if", #"i", #"weren", #(#"quote", #"t"),
        #"?y"),
      #(#"perhaps", #"i", #"am", #"?y", #"in", #"your", #"fantasies")),
    #(#(#(#"?*", #"?x"), #"you", #"are", #(#"?*", #"?y")),
      #(#"what", #"makes", #"you", #"think", #"i", #"am", #"?y", #"?")),
    #(#(#(#"?*", #"?x"), #"because", #(#"?*", #"?y")),
      #(#"is", #"that", #"the", #"real", #"reason?"),
      #(#"what", #"other", #"reasons", #"might", #"there", #"be?"),
      #(#"does", #"that", #"reason", #"seem", #"to", #"explain", #"anything",
        #"else?")),
    #(#(#(#"?*", #"?x"), #"were", #"you", #(#"?*", #"?y")),
      #(#"perhaps", #"i", #"was", #"?y"),
      #(#"what", #"do", #"you", #"think?"),
      #(#"what", #"if", #"i", #"had", #"been", #"?y")),
    #(#(#(#"?*", #"?x"), #"i", #"can", #(#"quote", #"t"), #(#"?*", #"?y")),
      #(#"maybe", #"you", #"could", #"?y", #"now"),
      #(#"what", #"if", #"you", #"could", #"?y", #"?")),
    #(#(#(#"?*", #"?x"), #"i", #"feel", #(#"?*", #"?y")),
      #(#"do", #"you", #"often", #"feel", #"?y", #"?")),
    #(#(#(#"?*", #"?x"), #"i", #"felt", #(#"?*", #"?y")),
      #(#"what", #"other", #"feelings", #"do", #"you", #"have?")),
    #(#(#(#"?*", #"?x"), #"i", #(#"?*", #"?y"), #"you", #(#"?*", #"?z")),
      #(#"perhaps", #"in", #"your", #"fantasy", #"we", #"?y", #"each",
        #"other")),
    #(#(#(#"?*", #"?x"), #"why", #"don", #(#"quote", #"t"), #"you",
        #(#"?*", #"?y")),
      #(#"should", #"you", #"?y", #"yourself?"),
      #(#"do", #"you", #"believe", #"i", #"don", #(#"quote", #"t"), #"?y"),
      #(#"perhaps", #"i", #"will", #"?y", #"in", #"good", #"time")),
    #(#(#(#"?*", #"?x"), #"yes", #(#"?*", #"?y")),
      #(#"you", #"seem", #"quite", #"positive"), #(#"you", #"are", #"sure"),
      #(#"i", #"understand")),
    #(#(#(#"?*", #"?x"), #"no", #(#"?*", #"?y")), #(#"why", #"not?"),
      #(#"you", #"are", #"being", #"a", #"bit", #"negative"),
      #(#"are", #"you", #"saying", "NO", #"just", #"to", #"be",
        #"negative?")),
    #(#(#(#"?*", #"?x"), #"someone", #(#"?*", #"?y")),
      #(#"can", #"you", #"be", #"more", #"specific?")),
    #(#(#(#"?*", #"?x"), #"everyone", #(#"?*", #"?y")),
      #(#"surely", #"not", #"everyone"),
      #(#"can", #"you", #"think", #"of", #"anyone", #"in", #"particular?"),
      #(#"who", #"for", #"example?"),
      #(#"you", #"are", #"thinking", #"of", #"a", #"special", #"person")),
    #(#(#(#"?*", #"?x"), #"always", #(#"?*", #"?y")),
      #(#"can", #"you", #"think", #"of", #"a", #"specific", #"example"),
      #(#"when?"),
      #(#"what", #"incident", #"are", #"you", #"thinking", #"of?"),
      #(#"really--", #"always")),
    #(#(#(#"?*", #"?x"), #"what", #(#"?*", #"?y")),
      #(#"why", #"do", #"you", #"ask?"),
      #(#"does", #"that", #"question", #"interest", #"you?"),
      #(#"what", #"is", #"it", #"you", #"really", #"want", #"to", #"know?"),
      #(#"what", #"do", #"you", #"think?"),
      #(#"what", #"comes", #"to", #"your", #"mind", #"when", #"you", #"ask",
        #"that?")),
    #(#(#(#"?*", #"?x"), #"perhaps", #(#"?*", #"?y")),
      #(#"you", #"do", #"not", #"seem", #"quite", #"certain")),
    #(#(#(#"?*", #"?x"), #"are", #(#"?*", #"?y")),
      #(#"did", #"you", #"think", #"they", #"might", #"not", #"be", #"?y"),
      #(#"possibly", #"they", #"are", #"?y")),
    #(#(#(#"?*", #"?x")), #(#"very", #"interesting"),
      #(#"i", #"am", #"not", #"sure", #"i", #"understand", #"you", #"fully"),
      #(#"what", #"does", #"that", #"suggest", #"to", #"you?"),
      #(#"please", #"continue"), #(#"go", #"on"),
      #(#"do", #"you", #"feel", #"strongly", #"about", #"discussing", #"such",
        #"things?")));

//  ==============================
"eof";

