module: main
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/main/main.dylan,v 1.2.1.1 1994/12/19 13:02:40 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


define method file-tokenizer (name :: <byte-string>)
  let source = make(<source-file>, name: name);
  let (header, start-line, start-posn) = parse-header(source);
//  *Current-Module* := find-module(*Current-Library*,
//				  as(<symbol>, header[#"module"]),
//				  create: #t);
  make(<lexer>,
       source: source,
       start-posn: start-posn,
       start-line: start-line);
end;


define method compile (#rest files) => res :: <region>;
  $Top-Level-Forms.size := 0;
  for (file in files)
//    if (file.size > 2 & file[0] == '-' & file[1] == 'l')
//      *Current-Library*
//	:= find-library(as(<symbol>, copy-sequence(file, start: 2)),
//			create: #t);
//    else
      format(*debug-output*, "Parsing %s\n", file);
      parse-program(file-tokenizer(file));
//    end;
  end;
  format(*debug-output*, "Finalizing definitions\n");
  do(finalize-top-level-form, $Top-Level-Forms);
  format(*debug-output*, "Converting into FER\n");
  let component = make(<fer-component>);
  let builder = make-builder(component);
  do(curry(convert-top-level-form, builder), $Top-Level-Forms);
  builder-result(builder);
end;

define method main (argv0, #rest files)
  if (empty?(files))
    error("No files supplied.");
    #f;
  else
    apply(compile, files);
  end;
end;
