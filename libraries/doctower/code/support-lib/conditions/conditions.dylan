module: conditions
synopsis: Condition support and error handling.

/**
Synopsis: Indicates a request for a list of locations matching a given
specifier (defined by the caller).

Recovery protocol is for handler to return an appropriate list of
<source-location>s.
**/
define class <need-locations> (<condition>)
   constant slot specifier-for-locations = #f, init-keyword: #"specifier";
end class;

define method return-allowed? (warning :: <need-locations>)
=> (yes :: singleton(#t))
   #t
end method;


define macro errors-definer
   {  define errors (?class:expression)
         ?code:expression ?:name ?format-string:expression, location, ?format-args:*;
         ?more:*
      end
   }
   => {  define function ?name (#key location, ?format-args)
            signal(make(?class,
                        error-code: ?code,
                        error-location: location,
                        format-string: ?format-string,
                        format-arguments: vector(?format-args)));
         end;
         define errors (?class) ?more end
      }

   {  define errors (?class:expression)
         ?code:expression ?:name ?format-string:expression, ?format-args:*;
         ?more:*
      end
   }
   => {  define function ?name (#key ?format-args)
            signal(make(?class,
                        error-code: ?code,
                        error-location: #f,
                        format-string: ?format-string,
                        format-arguments: vector(?format-args)));
         end;
         define errors (?class) ?more end
      }

   { define errors (?class:expression) end } => { }
end macro;


/** Synopsis: Conditions that may be displayed for the user. **/
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
   let (postfix-string, postfix-args) = values(" (%02d)", vector(error-code));
   next-method(cls, error-code: error-code, error-location: error-location,
         format-string: concatenate(prefix-string, format-string, postfix-string),
         format-arguments: concatenate(prefix-args, format-arguments, postfix-args))
end method;


/**
Synopsis: Can be disabled and safely ignored by the user; content is still valid
or as valid as possible. The program must have a fallback.

Recovery protocol is handler can return values (which are ignored).
**/
define class <user-visible-warning> (<user-visible-condition>, <warning>)
end class;

define method make
   (cls == <user-visible-warning>, #rest keys, #key, #all-keys)
=> (inst :: <user-visible-warning>)
   apply(next-method, cls, #"error-class", "Warning", keys);
end method;

define method return-allowed? (warning :: <user-visible-warning>)
=> (yes :: singleton(#t))
   #t
end method;


/**
Synopsis: Cannot be disabled or safely ignored by the user; results in incorrect
content. The program may have a fallback allowing it to display further errors.

Recovery protocol is handler cannot return values, but can signal
<skip-error-restart>.
**/
define class <user-visible-error> (<user-visible-condition>, <serious-condition>)
end class;

define method make
   (cls == <user-visible-error>, #rest keys, #key, #all-keys)
=> (inst :: <user-visible-error>)
   apply(next-method, cls, #"error-class", "Error", keys);
end method;


/**
Synopsis: Retry handler for <user-visible-error>, in case program can continue.
**/
define abstract class <user-visible-restart> (<restart>)
   slot error-condition :: <user-visible-error>, required-init-keyword: #"condition";
end class;

/** Synopsis: Handler to skip an erroneous thing. **/
define class <skip-error-restart> (<user-visible-restart>)
end class;


define errors (<user-visible-warning>)
   01 unparsable-expression-in-code
      "Unparsable expression will not be in automatically-generated documentation",
      location;
   
   02 unsupported-syntax-in-code
      "Unsupported syntactic form will not be in automatically-generated documentation",
      location;

   03 library-exports-not-known
      "Modules and bindings of used library are unknown and might not be documented",
      location;

   04 no-definition-for-bindings
      "No definition of exported binding %s",
      location, names;

   05 qv-or-vi-in-title
      "Titles may not include quoted phrase options \"qv\" or \"vi\"",
      location;
end errors;


define errors (<user-visible-error>)
   51 illegal-character-in-id
      "Tags may not include space, slash, open bracket, or close bracket characters",
      location;

   52 leading-colon-in-id
      "Tags may not include a leading colon",
      location;

   53 leading-colon-in-title
      "Titles may not include a leading colon",
      location;

   54 duplicate-section-in-topic
      "Topics may only include one %s section",
      location, section-type;

   55 illegal-section-in-topic
      "Topic may not include %s section",
      location, section-type;

   56 q-and-qq-in-spec
      "Quoted phrase options may not include both \"q\" and \"qq\"",
      location;

   57 bad-syntax-in-toc-file
      "Incorrect syntax",
      location;
   
   58 skipped-level-in-toc-file
      "Over-indented title or tag",
      location;
   
   59 parse-error-in-markup
      "Unparsable markup, expected %s",
      location, expected;

   60 parse-error-in-dylan
      "Unparsable syntax, expected %s",
      location, expected;

   61 no-context-topic-in-block
      "Topic for content cannot be inferred",
      location;

   62 target-not-found-in-link
      "Title or tag \"%s\" not found",
      location, target-text;

   63 duplicate-id-in-topics
      "Tag is already used at %s", 
      location, id-locations;

   64 id-matches-topic-title
      "Tag is already used as title at %s",
      location, title-location;

   65 ambiguous-title-in-link
      "\"%s\" is ambiguous and might refer to any of %s",
      location, target-text, topic-locations;

   66 conflicting-locations-in-tree
      "Topic is placed ambiguously by %s",
      location, arranger-locations;
   
   67 multiple-libraries-in-fileset
      "Multiple library definitions found in library at %s",
      location, defn-locations;
   
   68 no-library-in-fileset
      "No library definition found in %s",
      filenames;
   
   69 no-definition-for-modules
      "No definition of exported module %s",
      location, names;

   70 file-error
      "File error with %s: %s",
      filename, error;

   71 file-not-found
      "File %s not found",
      location, filename;
   
   72 empty-header-in-interchange-file
      "\"%s\" header is empty",
      location, header;
   
   73 error-in-command-option
      "Incorrect %s option; see --help",
      option;
   
   74 no-files-in-command-arguments
      "No files specified; see --help";
   
   75 conflicting-modules-in-library
      "Differing definitions of module \"%s\" at %s",
      location, name, defn-locations;
   
   76 duplicate-modules-in-fileset
      "Multiple definitions of module \"%s\" at %s",
      location, name, defn-locations;

   77 no-header-in-interchange-file
      "\"%s\" header is missing",
      location, header;

   78 file-type-not-known
      "File %s has unknown extension",
      filename;
   
   79 undefined-module-for-interchange-file
      "No definition of module \"%s\"",
      location, name;
   
   80 conflicting-bindings-in-module
      "Differing definitions of binding \"%s\" at %s",
      location, name, defn-locations;
   
   /*
   81 conflicting-rule-types-in-macro
      "Differing kinds of main rule",
      location;
   */
   
   82 circular-definition
      "Circular dependency for \"%s\" between %s",
      location, name, defn-locations;

   83 conflicting-definitions-in-code
      "Differing binding definitions at %s",
      location, defn-locations;
end errors;
