module: name-processing


/// Synopsis: Removes spaces and makes titlecase.
define function standardize-qualified-name (name :: <string>) => (name :: <string>)
   choose(curry(\~=, ' '), name.as-titlecase);
end function;


/// Synopsis: Ensures legal characters in automatically-generated IDs.
define function standardize-id (id :: <string>) => (id :: <string>)
   replace-elements!(id, curry(\=, '/'), always('.'));
end function;


/// Synopsis: Fixes spaces and makes titlecase in API titles.
define function standardize-title (title :: <string>) => (title :: <string>)
   title := as-titlecase(title);
   title := regexp-replace(title, " {2,}", " ");
   title := regexp-replace(title, " ?, ?([^ ])", ", \\1");
   title := regexp-replace(title, " ?([.\\(\\)\\[\\]]) ?", "\\1");
end function;


define function as-titlecase (str :: <string>) => (str :: <string>)
   let cased-str = str.copy-sequence;
   let should-cap? :: <boolean> = #t;
   for (c keyed-by i in cased-str)
      case
         c.alphabetic? =>
            when (should-cap?)
               cased-str[i] := c.as-uppercase;
               should-cap? := #f
            end when;
         member?(c, "!&*<=>|^$%@_-+~?/0123456789") =>
            #f;
         otherwise =>
            should-cap? := #t;
      end case
   end for;
   cased-str
end function;
