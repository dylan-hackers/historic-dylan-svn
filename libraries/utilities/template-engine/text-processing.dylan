module: template-engine


define parser text :: <string>
   rule seq(opt-space,
            many(seq(not-next(whitespace), not-next(lf-directive-mark), char)))
      => tokens;
   yield concat-strings(tokens[0], as(<string>, collect-subelements(tokens[1], 2)))
end;


define parser text-til-escape-end :: <string>
   rule seq(opt-space,
            many(seq(not-next(rt-directive-esc), not-next(whitespace),
                     not-next(lf-directive-mark), char)))
      => tokens;
   yield concat-strings(tokens[0], as(<string>, collect-subelements(tokens[1], 3)))
end;


define parser escaped-directive :: <sequence>
   rule seq(opt-space, lf-directive-esc, opt(escaped-content), rt-directive-esc)
      => tokens;
   yield concat-adj-strings(integrate-sequences(tokens))
end;


define parser lf-directive-esc :: <string>
   rule seq(lf-directive-mark, lf-directive-char) => tokens;
   yield as(<string>, tokens.last)
end;


define parser rt-directive-esc :: <string>
   rule seq(rt-directive-char, rt-directive-mark) => tokens;
   yield as(<string>, tokens.first)
end;


define parser escaped-content :: <sequence>
   rule many(content-element-til-escape-end) => tokens;
   yield tokens
end;


define method concat-adj-strings (items :: <sequence>) => (items :: <sequence>)
   if (items.size >= 2)
      reduce(add-string-or-other, make(<stretchy-vector>), items);
   else
      items
   end if;
end method;

define method add-string-or-other (vec :: <stretchy-vector>, item == #f)
=> (vec :: <stretchy-vector>)
   vec
end method;

define method add-string-or-other (vec :: <stretchy-vector>, item :: <object>)
=> (vec :: <stretchy-vector>)
   add!(vec, item)
end method;

define method add-string-or-other (vec :: <stretchy-vector>, item :: <string>)
=> (vec :: <stretchy-vector>)
   if (vec.size > 0 & instance?(vec.last, <string>))
      vec.last := concatenate!(vec.last, item);
      vec
   else
      next-method()
   end if
end method;


define method concat-strings (string :: <string>, #rest strings)
=> (string :: <string>)
   let strings = replace-elements!(strings, false?, always(""));
   debug-assert(every?(rcurry(instance?, <string>), strings),
                "concat-strings applied to non-string in %=", strings);
   apply(concatenate, string, strings)
end method;


/// Synopsis: Flatten nested sequences. For example, `#[a, #[b, c, #[d]]]` would
/// become `#[a, b, c, d]`.
define function integrate-sequences (items :: <sequence>)
=> (integrated-items :: <sequence>)
   let new-items = make(<deque>);
   for (item in items)
      if (instance?(item, <sequence>) & ~instance?(item, <string>))
         new-items := concatenate!(new-items, integrate-sequences(item));
      else
         push-last(new-items, item);
      end if;
   end for;
   new-items
end function;


define function subelements-string
   (elements :: false-or(<sequence>), index :: <integer>)
=> (string :: <string>)
   let subelements = collect-subelements(elements, index);
   let strings = map(curry(as, <string>), subelements);
   reduce(concatenate, "", strings)
end function;
   

define method wrap-directive-output
   (template :: <template>, output :: <string>, directive :: <delimited-directive-token>)
=> (output :: <string>)
   if (directive.right-delimiter.raw-output?)
      output
   else
      template.sanitizer(output)
   end if
end method;

