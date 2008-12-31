module: conditions
synopsis: Condition support and error handling.

/**
Synopsis: Indicates an special request for a list of locations.

Recovery protocol is for handler to return list of <source-location>s.
**/
define class <need-locations> (<condition>)
   constant slot specifier-for-locations = #f, init-keyword: #"specifier";
end class;


/** Synopsis: Conditions that may be displayed for the user. **/
define abstract class <user-visible-condition>
      (<source-location-mixin>, <format-string-condition>)
   constant slot error-code, required-init-keyword: #"error-code";
end class;


/**
Synopsis: Indicates illegal options or syntax.

Recovery protocol is handler cannot return values, but can signal
<simple-restart>.
**/
define class <syntax-error> (<serious-condition>, <user-visible-condition>)
end class;

define method make
   (cls == <syntax-error>,
    #key format-string, format-arguments, error-code, source-location)
=> (inst :: <syntax-error>)
   let (prefix-string, prefix-args) =
         if (instance?(source-location, <unknown-source-location>))
            values("Error: ", #[])
         else
            values("%s: Error: ", vector(source-location))
         end if;
   let (postfix-string, postfix-args) = values(" (%2d)", vector(error-code));
   next-method(cls, error-code: error-code, source-location: source-location,
         format-string: concatenate(prefix-string, format-string, postfix-string),
         format-arguments: concatenate(prefix-args, format-arguments, postfix-args))
end method;


/**
Synopsis: Indicates an issue with topic layout or content.

Recovery protocol is handler cannot return values, but can signal
<simple-restart>.
**/
define class <design-error> (<serious-condition>, <user-visible-condition>)
end class;

define method make
   (cls == <design-error>,
    #key format-string, format-arguments, error-code, source-location)
=> (inst :: <design-error>)
   let (prefix-string, prefix-args) =
         if (instance?(source-location, <unknown-source-location>))
            values("Error: ", #[])
         else
            values("%s: Error: ", vector(source-location))
         end if;
   let (postfix-string, postfix-args) = values(" (%2d)", vector(error-code));
   next-method(cls, error-code: error-code, source-location: source-location,
         format-string: concatenate(prefix-string, format-string, postfix-string),
         format-arguments: concatenate(prefix-args, format-arguments, postfix-args))
end method;


/**
Synopsis: Indicates unparsable syntax. User may want to look into it, but
processing can continue.

Recovery protocol is handler can return any value (which is ignored), but cannot
signal <simple-restart>.
**/
define class <syntax-warning> (<warning>, <user-visible-condition>)
end class;

define method make
   (cls == <syntax-warning>,
    #key format-string, format-arguments, error-code, source-location)
=> (inst :: <syntax-warning>)
   let (prefix-string, prefix-args) =
         if (instance?(source-location, <unknown-source-location>))
            values("Warning: ", #[])
         else
            values("%s: Warning: ", vector(source-location))
         end if;
   let (postfix-string, postfix-args) = values(" (%2d)", vector(error-code));
   next-method(cls, error-code: error-code, source-location: source-location,
         format-string: concatenate(prefix-string, format-string, postfix-string),
         format-arguments: concatenate(prefix-args, format-arguments, postfix-args))
end method;


define macro errors-definer
   {  define errors (?class:expression)
         ?code:expression ?:name ?format-string:expression, ?format-args:*;
         ?more:*
      end
   }
   => {  define function ?name (location :: <source-location>, #key ?format-args)
            signal(make(?class,
                        error-code: ?code,
                        source-location: location,
                        format-string: ?format-string,
                        format-arguments: vector(?format-args)));
         end;
         define errors (?class) ?more end
      }

   { define errors (?class:expression) end } => { }
end macro;


define errors (<syntax-warning>)
   00 nonspecific-syntax-warning
      "Syntax warning";
      
   01 unparsable-expression-in-code
      "Skipping unparsable expression";
   
   02 unsupported-syntax-in-code
      "Skipping unsupported syntactic form";
end errors;


define errors (<syntax-error>)
   20 nonspecific-syntax-error
      "Syntax error";

   21 illegal-character-in-id
      "Tags may not include space, slash, open bracket, or close bracket characters";

   22 leading-colon-in-id
      "Tags may not include a leading colon";

   23 leading-colon-in-title
      "Titles may not include a leading colon";

   24 duplicate-section-in-topic
      "Topics may only include one %s section", section-type;

   25 illegal-section-in-topic
      "Topic may not include %s section", section-type;

   26 q-and-qq-in-spec
      "Quoted phrase options may not include both \"q\" and \"qq\"";

   27 qv-or-vi-in-title
      "Titles may not include quoted phrase options \"qv\" or \"vi\"";

   28 bad-syntax-in-toc-file
      "Incorrect syntax in table of contents file";
   
   29 skipped-level-in-toc-file
      "Over-indented title or tag in table of contents file";
end errors;


define errors (<design-error>)
   40 nonspecific-design-error
      "Semantic error";

   41 no-context-topic-in-block
      "Topic for content cannot be inferred";

   42 target-not-found-in-link
      "Title or tag \"%s\" not found", target-text;

   43 duplicate-id-in-topics
      "Tag is already used at %s", id-locations;

   44 id-matches-topic-title
      "Tag is already used as title at %s", title-location;

   45 ambiguous-title-in-link
      "\"%s\" is ambiguous and may refer to any of %s", target-text, topic-locations;

   46 conflicting-locations-in-tree
      "Topic is placed ambiguously by %s", arranger-locations;
end errors;
