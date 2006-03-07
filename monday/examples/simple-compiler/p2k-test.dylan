Module: p2k-test


begin
  let lexer = make(<p2k-lexer>, stream: *standard-input*);
  let program = p2k-parse-program(lexer);
  p2k-cse-optimize(program);
  p2k-regalloc(program, 6);
  p2k-generate-moves(program);
  p2k-print-procedure(program, *standard-output*);
end;
          
define method print-object
    (object :: <p2k-instruction>, stream :: <stream>)
 => ();
  pprint-logical-block
    (stream,
     prefix: "{",
     body: method (stream)
             write(stream, "p2k-instruction ");
             print(object.instruction-number, stream);
	   end method,
     suffix: "}");
end method;
          
