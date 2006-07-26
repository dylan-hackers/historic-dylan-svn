// 
//  File: LibraryATN.Lisp
//  This is a natural language interface for a library database system
//  Copyright 1990 by Mark Watson
//
#f;

// 
//  Main interface function:
//   Parse(sentence) - sentence is the sentence to process
//                   - returned value is a semantic memory structure
//
define method parse (sentence)
  begin
    *subject* := #f;
    *verb* := #f;
    *book-title* := #f;
    *object-1* := #f;
    *modifier-1* := #f;
    *number-1* := #f;
    *modifier-type-1* := #f;
    *card-holder* := #f;
    *action* := #f;
    *last-noun* := #f;
    *last-verb* := #f;
    *last-prep* := #f;
    *last-word* := #f;
    *response* := #f;
    *response-type* := #f;
  end;
  begin
    *sentence* := sentence;
    original-sentence := copy-sequence(sentence);
  end;
  if (~ sentence?())
    //  discard the first word and try again
    *last-word* := head(*sentence*);
    *sentence* := tail(*sentence*);
    sentence?();
  end if;
  //  if *object-1* has not been set, look for a book title:
  if (empty?(*object-1*))
    for (w in original-sentence)
      if (instance?(w, <string>)) *object-1* := w; end if;
    end for;
  end if;
  print("Semantic memory structure:", *standard-output*);
  write-element(*standard-output*, '\n');
  print(" Subject       : ", *standard-output*);
  print(*subject*, *standard-output*);
  write-element(*standard-output*, '\n');
  print(" Action       : ", *standard-output*);
  print(*action*, *standard-output*);
  write-element(*standard-output*, '\n');
  print(" Library card holder : ", *standard-output*);
  print(*card-holder*, *standard-output*);
  write-element(*standard-output*, '\n');
  print("   Action object   : ", *standard-output*);
  print(*object-1*, *standard-output*);
  write-element(*standard-output*, '\n');
  print("   Object modifier type: ", *standard-output*);
  print(*modifier-type-1*, *standard-output*);
  write-element(*standard-output*, '\n');
  print("   Object modifier  : ", *standard-output*);
  print(*modifier-1*, *standard-output*);
  write-element(*standard-output*, '\n');
  print("   Number modifier  : ", *standard-output*);
  print(*number-1*, *standard-output*);
  write-element(*standard-output*, '\n');
  write-element(*standard-output*, '\n');
  //  Use semantics of user prompt to perform data base query
  //  or update operation:
  select (*action*)
    #()
       => write-element(*standard-output*, '\n');
    #"list-info"
       => if (*response*)
            select (*response-type*)
              #()
                 => print("What?", *standard-output*);
                     write-element(*standard-output*, '\n');
              #"author"
                 => pp-author(*response*);
              #"book"
                 => pp-book(*response*);
              otherwise
                 => #f;
            end select;
          end if;
    #"return-book"
       => print("Returning the book: ", *standard-output*);
           print(*object-1*, *standard-output*);
           print(".", *standard-output*);
           write-element(*standard-output*, '\n');
           print("Returned by ", *standard-output*);
           for (x in *card-holder*)
             print(x, *standard-output*);
             print(" ", *standard-output*);
           end for;
           print("and ", *standard-output*);
           print(*object-1*, *standard-output*);
           print(" is returned to circulation.", *standard-output*);
           write-element(*standard-output*, '\n');
           check-out-book(print("Checking out the book: ", *standard-output*),
                          print(*object-1*, *standard-output*),
                          print(".", *standard-output*),
                          write-element(*standard-output*, '\n'),
                          print("Checked out by confirmed library card holder",
                                *standard-output*),
                          write-element(*standard-output*, '\n'),
                          for (x in *card-holder*)
                            print(x, *standard-output*);
                            print(" ", *standard-output*);
                          end for,
                          print("and data base is modified to show",
                                *standard-output*),
                          write-element(*standard-output*, '\n'),
                          print("that this volume is checked out.",
                                *standard-output*),
                          write-element(*standard-output*, '\n'));
    otherwise
       => #f;
  end select;
end method parse;

// 
//  Print out information about author:
//
define method pp-author (data)
  print("The following book(s) are by ", *standard-output*);
  for (x in cadaar(data))
    print(x, *standard-output*);
    if (x = head(cadaar(data))) print(",", *standard-output*); end if;
    print(" ", *standard-output*);
  end for;
  print(":", *standard-output*);
  write-element(*standard-output*, '\n');
  for (book in head(data))
    print(head(book), *standard-output*);
    write-element(*standard-output*, '\n');
  end for;
end method pp-author;

// 
//  Print out information about a book:
//
define method pp-book (data)
  print(cadaaar(data), *standard-output*);
  print("is the author.", *standard-output*);
  write-element(*standard-output*, '\n');
end method pp-book;

// 
//  Word discriminators:
//
define method isnoun? (word)
  if (member?(word, *nouns*))
    *last-noun* := word;
    if (*subject*
         | //  already defined
        *last-verb*)
      //  must be an imperative
      *object-1* := word;
    end if;
    #t;
  end if;
end method isnoun?;

define method isverb? (word)
  if (member?(word, *verbs*, test: \=))
    if (empty?(*subject*))
      //  assume that sentence is an imperative
      *subject* := #"computer";
    end if;
    //  semantic hooks to set *action* == 'list-info:
    if (member?(word, #(#"list", #"show"))
         | (word = #"are" & *last-word* = #"what"))
      *action* := #"list-info";
    end if;
    //  semantic hooks to set *action* == 'return-book
    if (member?(word, #(#"return", #"returned"), test: \=)
         | (member?(word, #(#"check", #"checked"), test: \=)
             & if (size(*sentence*) > 1)
                 second(*sentence*) = #(#"in", #"back");
               end if))
      *action* := #"return-book";
      *sentence* := tail(*sentence*);
    end if;
    //  semantic hooks to set *action* == 'check-out-book
    if (member?(word, #(#"got", #"receive", #"received", #"withdrew"),
                test: \=)
         | (member?(word, #(#"check", #"checked"), test: \=)
             & if (size(*sentence*) > 1)
                 member?(second(*sentence*), #(#"out"), test: \=);
               end if))
      *action* := #"check-out-book";
      *sentence* := tail(*sentence*);
    end if;
    *last-verb* := word;
  end if;
end method isverb?;

define method isadj? (word)
  if (member?(word, *adjs*))
    if (empty?(*modifier-1*)) *modifier-1* := word; else #t; end if;
  end if;
end method isadj?;

define method isprep? (word)
  if (member?(word, *preps*))
    *last-prep* := word;
    if (member?(word, #(#"a", #"an", #"one", #"this", #"that")))
      *number-1* := 1;
    elseif (member?(word, #(#"all", #"every")))
      *number-1* := 999999;
    else
      #t;
    end if;
  end if;
end method isprep?;

define method isbooktitle? (sentenceelement)
  instance?(sentenceelement, <string>);
end method isbooktitle?;

// 
//  Parsing networks:
//
define method np? ()
  let save = *sentence*;
  let temp = #f;
  if (//  name of member of library?:
      //  embedded semantics: do not allow parse to generate
      //  a form like 'by <author name>' if known action in
      //  the sentence is 'list-info':
      ~ (*action* = #"list-info")
       & (temp := card-holder-name?())
       & if ((temp & empty?(*card-holder*))) (*card-holder* := temp); end if
       | (word?(#"adj") & (np?() | word?(#"noun")))
       | word?(#"noun"))
    if (empty?(*subject*)) *subject* := *last-noun*; end if;
    #t;
  else
    *sentence* := save;
    #f;
  end if;
end method np?;

define method prepp? ()
  let save = *sentence*;
  let last-author-name = #f;
  let last-card-holder-name = #f;
  let temp = #f;
  // 
  //  Define an ATN to detect author names. Since this ATN is
  //  only called from inside the 'prepp?' ATN, we will define
  //  its LISP function lexically inside 'prepp?'
  //
  define method author-name? ()
    if (*author-hash-table*[head(*sentence*)])
      last-author-name := head(*sentence*);
      *sentence* := tail(*sentence*);
      //  throw away processed word
      #t;
    elseif (size(*sentence*) > 1)
      //  more than 1 word left ?
      if (*author-hash-table*[second(*sentence*)])
        last-author-name := second(*sentence*);
        *sentence* := tail(tail(*sentence*));
        //  throw away processed words
        #t;
      end if;
    end if;
  end method author-name?;
  //  return True: ATN successfully traversed
  if (word?(#"prep") & np?()
       | (//  special check for 'by <author name>'
          (*sentence* := save)
           & word?(#"prep")
           & *last-prep* = #"by"
           & author-name?()
           & begin
               //  we found a form like: 'by <author name>'
               (*modifier-type-1* := #"by");
               (*modifier-1* := last-author-name);
               #t;
             end)
       | (//  special check for 'by <card holder name>
          (*sentence* := save)
           & word?(#"prep")
           & *last-prep* = #"by"
           & //  embedded semantics: do not allow parse to generate
             //  a form like: 'by <author name>' if known action in
             //  the sentence is 'list-info'
          ~ (*action* = #"list-info")
           & (temp := card-holder-name?())
           & begin
               //  we found a form like: 'by <card holder name>'
               (*modifier-type-1+ := #"by");
               (*modifier-1* := temp);
               #t;
             end))
    *sentence* := save;
    #f;
  end if;
end method prepp?;

define method vp? ()
  let save = *sentence*;
  if (word?(#"verb") & np?()
       | begin (*sentence* := save); word?(#"verb"); end)
    if (empty?(*action*)) *action* := *last-verb*; end if;
    #t;
  else
    *sentence* := save;
    #f;
  end if;
end method vp?;

define method sentence? ()
  let save = *sentence*;
  if (vp?()
       & //  imperative
      prepp?()
       & prepp?()
       | (vp?()
           & //  imperative
          prepp?())
       | begin (*sentence* := save); (np?() & vp?() & prepp?()); end
       | begin (*sentence* := save); (np?() & vp?()); end
       | begin
           (*sentence* := save);
           if (vp?())
             (*subject* := #"computer");
           else
             //  imperative (command)
             #f;
           end if;
         end)
    #t;
  else
    *sentence* := save;
    #f;
  end if;
end method sentence?;

// 
//  Define an ATN to detect card holder names:
//
define method card-holder-name? ()
  //  sentence must be long enough for a first and last name:
  if (size(*sentence*) > 1)
    let name-list = list(head(*sentence*), second(*sentence*));
    if (member?(name-list, *library-card-holders*, test: \=))
      last-card-holder-name := name-list;
      *sentence* := tail(tail(*sentence*));
      //  throw away processed word
      name-list;
    end if;
  end if;
end method card-holder-name?;

//  return True: ATN successfully traversed
// 
//  Test for word type:
//
define method word? (type)
  //  grab semantic information on book/author if appropriate:
  if (~ *response*)
    if (member?(*last-prep*, #(#"by")))
      let author-data1 = *author-hash-table*[head(*sentence*)];
      let author-data2
          = if (size(*sentence*) > 1)
              *author-hash-table*[second(*sentence*)];
            end if;
      if (author-data1)
        *response-type* := #"author";
        *modifier-1* := head(*sentence*);
        *response* := pair(author-data1, *response*);
      elseif (author-data2)
        begin
          *response-type* := #"author";
          *modifier-1* := second(*sentence*);
          *response* := pair(author-data2, *response*);
        end;
      end if;
    end if;
  end if;
  let save = *sentence*;
  let w = if (empty?(*sentence+)) #f; else head(*sentence*); end if;
  let func
      = cl-assoc(type,
                 #(#(#"noun", #"isnoun?"), #(#"verb", #"isverb?"),
                   #(#"prep", #"isprep?"), #(#"adj", #"isadj?")));
  if (func)
    if ((second(func))(w)) remove-word(); #t; else #f; end if;
  else
    #f;
  end if;
end method word?;

// 
//  Utility to remove first word remaining in sentence AND to
//  save the discarded word in *last-word*
//
define method remove-word ()
  *last-word* := head(*sentence*);
  *sentence* := tail(*sentence*);
end method remove-word;

// 
//  Define words in the system:
//
*nouns* := #(#"i", #"book", #"books", #"library");

*verbs*
 := #(#"list", #"show", #"are", #"is", #"add", #"was", #"see", #"check",
      #"checked", #"got", #"returned");

*adjs*
 := #(#"what", #"the", #"a", #"an", #"one", #"this", #"that", #"all", #"every");

*preps* := #(#"in", #"behind", #"under", #"by");

// 
//  Turn on tracing for debug output:
//
// LTD: Function TRACE not yet implemented.
trace(isnoun?, isverb?, isadj?, isprep?, np?, prepp?, vp?, sentence?,
      card-holder-name?);

// 
//  Sample Data Base:
//
*book-list*
 := #(#("The Sound And The Fury", #(#"faulkner", #"william")),
      #("The Sound of Rain", #(#"peebody", #"alfred")),
      #("Broca's Brain", #(#"sagan", #"carl")),
      #("Dragons of Eden", #(#"sagan", #"carl")));

*library-card-holders*
 := #(#(#"mark", #"watson"), #(#"carol", #"watson"), #(#"julie", #"kimmel"),
      #(#"david", #"kimmel"));

// 
//  Utilities to pre-process book list to remove prepositions
//  Book titles for more robust pattern matching. Also convert
//  from a string to a list of words:
//
define method shrink-title (title-string)
  let retlist = #f;
  let astream
      = // LTD: Function MAKE-STRING-INPUT-STREAM not yet implemented.
        make-string-input-stream(as-uppercase!(title-string), 0);
  block (nil)
    begin
      for (i from 0 below 20)
        let token
            = // LTD: Function READ not yet implemented.
              read(astream, #f, #f, #f);
        if (token)
          if (~ member?(token, #(#"a", #"an", #"the", #"that", #"this")))
            retlist := pair(token, retlist);
          end if;
        end if;
      end for;
    end;
  cleanup
    close(astream);
  end block;
  pair(#"book:", reverse(retlist));
end method shrink-title;

// 
//  Data management functions: Initialize the database by
//  shrinking down book titles and building two hash tables:
// 
//  1. *book-hash-table*     - key is the first word of the
//                             shrunken book title.
//  2. *author-hash-table*   - key is the author's last name.
// 
//  *library-card-holders* = list of members of the library
//
define method init-database ()
  *book-hash-table* := make(<table>);
  *author-hash-table* := make(<table>);
  for (abook in *book-list*)
    let title-hash-key = second(shrink-title(head(abook)));
    let author-hash-key = caadr(abook);
    *book-hash-table*[title-hash-key]
                       := pair(abook, *book-hash-table*[title-hash-key]);
    *author-hash-table*[author-hash-key]
                         := pair(abook, *author-hash-table*[author-hash-key]);
  end for;
end method init-database;

init-database();

//  initialize the system when this file is loaded.
"eof";

