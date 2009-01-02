module: conditions
synopsis: Condition support and error handling.

/**
Synopsis: Indicates a request for a list of locations matching a given
specifier.

Recovery protocol is for handler to return an appropriate list of
<source-location>s.
**/
define class <need-locations> (<condition>)
   constant slot specifier-for-locations = #f, init-keyword: #"specifier";
end class;


define macro errors-definer
   {  define errors (?class:expression)
         ?code:expression ?:name ?format-string:expression, ?format-args:*;
         ?more:*
      end
   }
   => {  define function ?name (location, #key ?format-args)
            signal(make(?class,
                        error-code: ?code,
                        error-location: location,
                        format-string: ?format-string,
                        format-arguments: vector(?format-args)));
         end;
         define errors (?class) ?more end
      }

   { define errors (?class:expression) end } => { }
end macro;

define abstract class <user-visible-condition> (<format-string-condition>)
   constant slot error-location ::
         type-union(<source-location>, <file-locator>, singleton(#f)),
      required-init-keyword: #"error-location";
   constant slot error-code :: <integer>,
      required-init-keyword: #"error-code";
end class;

define method make
   (cls :: subclass(<user-visible-condition>),
    #key format-string, format-arguments, error-code, error-location,
         error-class :: <string>)
=> (inst :: <user-visible-condition>)
   let (prefix-string, prefix-args) =
         if (~error-location | instance?(error-location, <unknown-source-location>))
            values("%s: ", vector(error-class))
         else
            values("%s: %s: ", vector(error-location, error-class))
         end if;
   let (postfix-string, postfix-args) = values(" (%2d)", vector(error-code));
   next-method(cls, error-code: error-code, error-location: error-location,
         format-string: concatenate(prefix-string, format-string, postfix-string),
         format-arguments: concatenate(prefix-args, format-arguments, postfix-args))
end method;


/**
Synopsis: Conditions that may be displayed for the user and end execution.

Recovery protocol is handler cannot return values, but can signal
<simple-restart> to, e.g., a restart handler that will skip a troublesome file.
**/
define abstract class <user-visible-error> (<user-visible-condition>, <serious-condition>)
end class;

define method make
   (cls :: subclass(<user-visible-error>), #rest keys, #key, #all-keys)
=> (inst :: <user-visible-error>)
   apply(next-method, cls, #"error-class", "Error", keys);
end method;


/**
Synopsis: Conditions that are informative for the user.

Recovery protocol is handler can return values (which are ignored).
**/
define abstract class <user-visible-warning> (<user-visible-condition>, <warning>)
end class;

define method make
   (cls :: subclass(<user-visible-warning>), #rest keys, #key, #all-keys)
=> (inst :: <user-visible-warning>)
   apply(next-method, cls, #"error-class", "Warning", keys);
end method;


/**
Synopsis: Indicates unparsable syntax. User may want to look into it, but
processing can continue.
**/
define class <syntax-warning> (<user-visible-warning>)
end class;

define errors (<syntax-warning>)
   01 unparsable-expression-in-code
      "Skipping unparsable expression";
   
   02 unsupported-syntax-in-code
      "Skipping unsupported syntactic form";
end errors;


/** Synopsis: Indicates illegal options or syntax. **/
define class <syntax-error> (<user-visible-error>) end;

define errors (<syntax-error>)
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


/** Synopsis: Indicates an issue with topic layout or content. **/
define class <design-error> (<user-visible-error>) end;

define errors (<design-error>)
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


/** Synopsis: Indicates some other error that can be ignored. */
define class <general-warning> (<user-visible-warning>) end;

define errors (<general-warning>)
   61 file-not-found
      "File \"%s\" not found", filename;
end errors;
