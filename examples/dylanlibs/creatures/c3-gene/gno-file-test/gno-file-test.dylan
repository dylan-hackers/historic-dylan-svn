Module:    gno-file-test
Synopsis:  Testing program for the gno-file library
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define method main () => ()
  let args = application-arguments();
  if(args.size ~= 1)
    format-out("Using library gno-file V%s\n", gno-file-version());
    format-out("Usage: gno-file-test filename.gno\n");
  else
    display-gno(args.first);
  end if;
end method main;

define method display-documentation(doc :: <gno-documentation>)
 => ()
    format-out("Type: %d Subtype: %d, Number: %d\n",
               doc.gno-type,
               doc.gno-subtype,
               doc.gno-number);
    format-out("\tDescription: %s\n", doc.gno-description);
    format-out("\tComment: %s\n", doc.gno-comment);
end method display-documentation;

define method display-documentation(doc :: <gno-extra-documentation>)
 => ()
    format-out("Type: %d Subtype: %d, Sequence: %d, Number: %d\n",
               doc.gno-type,
               doc.gno-subtype,
               doc.gno-sequence,
               doc.gno-number);
    for(comment keyed-by index in doc.gno-comments)
      unless(empty?(comment))
        format-out("\tComment (%d): %s\n",
                   index,
                   comment);
      end;
    end for;
end method display-documentation;

define method display-gno(filename :: <string>)
 => ()
  // load-gno-file returns a <sequence> of <gno-documentation> instances.
  // For #"creatures3" files, it is a <sequence> of <gno-extra-documentation>
  // or <gno-documentation>.
  let gno = load-gno-file(filename, version: #"creatures3");

  for(doc in gno)
    display-documentation(doc);
  end for;  
end method display-gno;

begin
  main();
end;
