module: template-engine


define method generate-output (template :: <template>, token :: <template-token>)
=> (output :: <string>)
   generate-output(template, token.contents)
end method;


define method generate-output (template :: <template>, empty :: <empty-directive-token>)
=> (output :: <string>)
   ""
end method;


define method generate-output (template :: <template>, nothing == #f)
=> (output :: <string>)
   ""
end method;


define method generate-output (template :: <template>, string :: <string>)
=> (output :: <string>)
   string
end method;


define method generate-output (template :: <template>, contents :: <sequence>)
=> (output :: <string>)
   reduce(concatenate, "", map(curry(generate-output, template), contents))
end method;


define method generate-output
   (template :: <template>, case-block :: <case-directive-block-token>)
=> (output :: <string>)
   let matching-pair = find-element(case-block.case-pairs,
         compose(curry(directive-true?, template), first));
   if (matching-pair)
      generate-output(template, matching-pair.second)
   elseif (case-block.else-pair)
      generate-output(template, case-block.else-pair.second)
   else
      ""
   end if
end method;


define method generate-output
   (template :: <template>, if-block :: <if-directive-block-token>)
=> (output :: <string>)
   if (directive-true?(template, if-block.if-pair.first))
      generate-output(template, if-block.if-pair.second)
   elseif (if-block.else-pair)
      generate-output(template, if-block.else-pair.second)
   else
      ""
   end if
end method;


define class <repeat-info> (<object>)
   slot first-rep? :: <boolean> = #f;
   slot last-rep? :: <boolean> = #f;
   slot rep-number :: <integer> = 0;
   slot rep-key :: <object> = 0;
end class;


define method generate-output
   (template :: <template>, repeat-block :: <repeat-directive-block-token>)
=> (output :: <string>)
   let output :: <string> = "";
   let dirspec :: <repeat-dirspec-token> = repeat-block.block-pair.first.directive-specifier;
   let content :: <sequence> = repeat-block.block-pair.second;
   let collection :: <collection>
         = resolve-dyn-expression(template, dirspec.collection-expression);

   let info = make(<repeat-info>);
   let vars :: <table> = table(template.vocabulary-table-type, dirspec.loop-item-name => #f);
   when (dirspec.loop-info-name)
      vars[dirspec.loop-info-name] := info;
   end when;
   push(template.variable-scopes, vars);
   
   let (initial-state, limit, next-state, finished-state?, current-key, current-element)
         = collection.forward-iteration-protocol;
   let state = initial-state;
   while (~finished-state?(collection, state, limit))
      info.rep-number := info.rep-number + 1;
      info.rep-key := current-key(collection, state);
      vars[dirspec.loop-item-name] := current-element(collection, state);

      info.first-rep? := (state == initial-state);
      state := next-state(collection, state);
      info.last-rep? := finished-state?(collection, state, limit);
      
      output := concatenate(output, generate-output(template, content));
   end while;
         
   pop(template.variable-scopes);
   output
end method;


define method generate-output
   (template :: <template>, with-block :: <with-directive-block-token>)
=> (output :: <string>)
   let dirspec :: <with-dirspec-token> = with-block.block-pair.first.directive-specifier;
   let content :: <sequence> = with-block.block-pair.second;

   let vars :: <table> = make(template.vocabulary-table-type);
   let ops :: <table> = make(template.vocabulary-table-type);
   let has-vars? = #f;
   let has-ops? = #f;

   for (assign :: <assignment-token> in dirspec.assignments)
      if (assign.operation?)
         has-ops? := #t;
         let val = resolve-operation(template, assign.value-token.text, assign.value-token);
         ops[assign.name] := val;
      else
         has-vars? := #t;
         let val = resolve-dyn-expression(template, assign.value-token);
         vars[assign.name] := val;
      end if
   end for;

   when (has-vars?)
      push(template.variable-scopes, vars)
   end when;
   when (has-ops?)
      push(template.operation-scopes, ops)
   end when;

   let output = generate-output(template, content);
   
   when (has-vars?)
      pop(template.variable-scopes)
   end when;
   when (has-ops?)
      pop(template.operation-scopes)
   end when;
   
   output
end method;


define method generate-output
   (template :: <template>, direc :: <simple-directive-token>)
=> (output :: <string>)
   let expr :: <expression-token> = direc.directive-specifier.expression;
   let val = resolve-dyn-expression(template, expr);
   let output :: <string> = template.stringifier(val);
   wrap-directive-output(template, output, direc)
end method;


define method directive-true? (template :: <template>, direc :: <case-directive-token>)
=> (true? :: <boolean>)
   let dirspec :: <case-dirspec-token> = direc.directive-specifier;
   resolve-dyn-expression(template, dirspec.test-expression).true?
end method;


define method directive-true? (template :: <template>, direc :: <if-directive-token>)
=> (true? :: <boolean>)
   let dirspec :: <if-dirspec-token> = direc.directive-specifier;
   let expr-truth = resolve-dyn-expression(template, dirspec.test-expression).true?;
   if (instance?(dirspec.dirspec-lexeme, <lex-unless-token>))
      ~expr-truth
   else
      expr-truth
   end if
end method;