module: template-engine


/** Synopsis: A parsed template, ready for processing.

This class contains everything needed to produce output from a template document
given some variables. An instance of this class may be used repeatedly with
different variables each time.

--- Conditions: ---

'Make' on this class may signal a <parse-error> if the template document has
syntax errors.

--- Slot Accessors: ---

vocabulary-table-type -
   Either <case-insensitive-string-table> or <string-table>, depending on the
   value of the 'case-sensitive' init keyword.

--- Init Keywords: ---

document -
   A <positionable-stream> containing the template document. The document is
   parsed and stored; this stream is not needed after the <template> instance
   is created. Required.

base-variables -
   Base variable values that the template may refer to. An instance of
   <string-table> or <case-insensitive-string-table>, depending on the value of
   'case-sensitive'.

   The keys are variable names as used in the template document, e.g., "person"
   in the case of "{{person.name}}". These variables may be shadowed by
   variables set by 'process-template' or created in the template document
   itself.
   
   Defaults to an empty table.

base-operations -
   Operations and getter methods available for use in template expressions. An
   instance of <string-table> or <case-insensitive-string-table>, depending on
   the value of 'case-sensitive'.
   
   The keys are operation names as used in the template document, e.g., "name"
   in the case of "{{person.name}}". The elements are <function>s; the engine
   passes an <object> argument to the function and expects an <object> value.
   These operations may be redefined by 'process-template' or the template
   document itself.
   
   Defaults to the repeat block information accessors if 'control-structures' is
   #t, or an empty table otherwise. Repeat block information accessors are
   'first-rep?', 'last-rep?', 'rep-number', 'rep-key'.

case-sensitive -
   A <boolean> indicating whether template document expressions use case
   sensitive names. If #t, they do. Defaults to #f.

delimiter -
   One of #"{}", #"[]", #"<>", or #"()", indicating the type of delimiter around
   template directives, e.g. "{{person.name}}" or "((person.name))". May be
   overridden by a template header directive. Defaults to #"{}".

stringifier -
   Function to convert variable values to <string>. The engine passes an
   <object> argument to the function and expects a <string> value. Defaults to
   'curry(format-to-string, "%s")'. [code]

sanitizer -
   Function to sanitize the results of the 'stringifier' function. The engine
   passes a <string> argument to the function and expects a <string> value.
   Defaults to 'identity'. [code]

control-structures -
   A flag indicating whether advanced directives are available in the template
   document. If #t, the default, all directives are available. If #f, only the
   basic substitution directive is available; other directives in the template
   document are treated as substitution directives and result in parse errors.
   This flag does not affect the availability of the template style directive;
   see 'header'.

header -
   A flag indicating whether the first line of the template document can contain
   a template header directive ("{{template}}", "((template))", etc.). If #t,
   the default, the directive is available. If #f and the directive is present,
   it is treated like plain text or a substitution directive, depending on the
   delimiter style it uses.
*/
define class <template> (<object>)
   constant slot variable-scopes
      = make(<deque>); /* of 'vocabulary-table-type' */
   constant slot operation-scopes
      = make(<deque>); /* of 'vocabulary-table-type' of <function> */
         
   constant slot stringifier :: <function> = curry(format-to-string, "%s"),
      init-keyword: #"stringifier";
   constant slot sanitizer :: <function> = identity,
      init-keyword: #"sanitizer";
      
   slot vocabulary-table-type :: one-of(<string-table>, <case-insensitive-string-table>);
   slot parsed-template :: <template-token>;

   required keyword document:;
   keyword base-variables:;
   keyword base-operations:;
   keyword case-sensitive:;
   keyword control-structures:;
   keyword header:;
   keyword delimiter:;
end class;



define method initialize
   (template :: <template>, #rest keys, #key
    document: template-stream :: <positionable-stream>,
    case-sensitive :: <boolean> = #f, base-variables, base-operations,
    control-structures :: <boolean> = #t, header :: <boolean> = #t,
    delimiter :: false-or(one-of(#"()", #"[]", #"{}", #"<>")),
    #all-keys)
=> ()
   next-method();

   // Set up scopes.

   template.vocabulary-table-type :=
         if (case-sensitive) <string-table> else <case-insensitive-string-table> end;
         
   if (base-variables)
      check-type(base-variables, template.vocabulary-table-type);
   else
      base-variables := make(template.vocabulary-table-type);
   end;
   
   if (base-operations)
      check-type(base-operations, template.vocabulary-table-type)
   elseif (control-structures)
      base-operations := table(template.vocabulary-table-type,
            "first-rep?" => first-rep?, "last-rep?" => last-rep?,
            "rep-key" => rep-key, "rep-number" => rep-number);
   else
      base-operations := make(template.vocabulary-table-type);
   end;
   
   push(template.variable-scopes, base-variables);
   push(template.operation-scopes, base-operations);
   
   // Set up parse context.
   
   let context :: <template-context>
         = apply(make, <template-context>, config:, template, keys);

   when (delimiter)
      let delim-string = as(<string>, delimiter);
      context.lf-directive-character := delim-string.first;
      context.rt-directive-character := delim-string.second;
   end when;
   
   let catalog-choices = 
         if (context.allow-control-structures?)
            vector(parse-case-directive-block, parse-if-directive-block,
                   parse-repeat-directive-block, parse-with-directive-block,
                   parse-simple-directive, parse-empty-directive)
         else
            vector(parse-with-directive-block, parse-simple-directive,
                   parse-empty-directive)
         end if;
   context.directive-catalog-parser := apply(choice, catalog-choices);

   // Parse template.
   
   let (result, success?, failure) = parse-template(template-stream, context);
   if (success?)
      template.parsed-template := result
   else
      error(failure)
   end if;

   // for (count keyed-by parser in context.parser-cache-hits)
   //    format-out("%5d %s\n", count, parser);
   // end for;
end method;


/** Synopsis: Convenience macro for generating template vocabulary tables.

--- Syntax ---
: template-vocabulary(TEMPLATE, NAME...)

--- Arguments: ---
template - An instance of <template>.
name     - A Dylan name to add to the table.

--- Values: ---
An instance of <case-insensitive-string-table> or <string-table>.
*/
define macro template-vocabulary
   { template-vocabulary(?template:name, ?items) } =>
   { table(?template.vocabulary-table-type, ?items) }
   
items:
   { ?:name, ... } => { ?"name" => ?name, ... }
   { } => { }
end macro;


//
// Processing
//


/** Synopsis: Process a template into output.

Creates a new <string> object from 'template'.

--- Conditions: ---

If a variable or operation named in a template expression has not been
specified, this function signals <missing-name-error>. A handler may respond by
returning a value or function that will be used in place of the missing one.

If an operation or expression fails for some other reason, this function signals
<expression-error>. There is no recovery protocol for this error.

--- Arguments: ---
template    - An instance of <template>.
variables:  - An instance of <string-table> or <case-insensitive-string-table>
              containing values to substitute for variables in the template; see
              'base-variables'. [qv :::<Template>/:Keywords]
              Optional.
operations: - An instance of <string-table> or <case-insensitive-string-table>
              containing operations applicable to variables in the template; see
              'base-operations'. [qv :::<Template>/:Keywords]
              Optional.
*/
define function process-template
   (template :: <template>,
    #key variables :: false-or(<table>), operations :: false-or(<table>))
=> (output :: <string>)

   if (variables)
      check-type(variables, template.vocabulary-table-type);
      push(template.variable-scopes, variables)
   end if;
   
   if (operations)
      check-type(operations, template.vocabulary-table-type);
      push(template.operation-scopes, operations)
   end if;
   
   let output = generate-output(template, template.parsed-template);
   
   when (variables) pop(template.variable-scopes) end;
   when (operations) pop(template.operation-scopes) end;
   output
end function;


define method named-value (name :: <string>, scopes :: <deque>)
=> (value :: <object>, found? :: <boolean>)
   block (found)
      for (scope :: <table> in scopes)
         let val = element(scope, name, default: $unfound);
         if (val.found?) found(val, #t) end if;
      finally
         values(#f, #f)
      end for
   end block
end method;


//
// Conditions
//


define class <processing-error> (<error>)
   /** The stream position of the failed expression in the original template
       document. */
   constant slot error-position :: type-union(<stream-position>, <integer>),
      required-init-keyword: #"position";
end class;


/**
Synopsis: Condition indicating that a name used in the template has not been
defined.

See 'process-template' for recovery protocol.

--- Getters: ---
missing-name       - A <string>. The undefined name.
missing-operation? - A <boolean>, indicating whether the name is an operation.
                     If #t, the name should correspond to a function taking one
                     argument and returning one argument.
*/
define class <missing-name-error> (<processing-error>)
   constant slot missing-name :: <string>, required-init-keyword: #"name";
   constant slot missing-operation? :: <boolean>, required-init-keyword: #"operation";
end class;

define method condition-to-string (cond :: <missing-name-error>)
=> (desc :: <string>)
   format-to-string("Name \"%s\" not defined at template position %s",
         cond.missing-name, cond.error-position)
end method;

define method return-allowed? (cond :: <missing-name-error>)
=> (true :: singleton(#t)) #t end method;

define method return-description (cond :: <missing-name-error>)
=> (desc :: <string>)
   "Value to use for the missing name"
end method;


/**
Synopsis: Condition indicating that an expression could not be evaluated.

See 'process-template' for recovery protocol.

--- Getters: ---
actual-error   - The <error> that caused the expression to fail.
*/
define class <expression-error> (<processing-error>)
   constant slot actual-error :: <error>, required-init-keyword: #"error";
end class;

define method condition-to-string (cond :: <expression-error>)
=> (desc :: <string>)
   format-to-string("Operation failed at template position %s: %s",
         cond.error-position, cond.actual-error)
end method;


//
// Parser context
//


define class <template-context> (<parse-context>)
   constant slot template-config :: <template>,
      required-init-keyword: #"config";

   constant slot allow-control-structures? :: <boolean> = #t,
      init-keyword: #"control-structures";
   slot directive-catalog-parser :: false-or(<function>) = #f;

   constant slot allow-header? :: <boolean> = #t,
      init-keyword: #"header";

   // These things may be configured via the {{template}} directive.
   slot lf-directive-character :: <character> = '{';
   slot rt-directive-character :: <character> = '}';
   constant virtual slot lf-directive-char-parser :: <function>;
   constant virtual slot lf-directive-mark-parser :: <function>;
   constant virtual slot lf-directive-parser :: <function>;
   constant virtual slot rt-directive-char-parser :: <function>;
   constant virtual slot rt-directive-mark-parser :: <function>;
   constant virtual slot rt-directive-parser :: <function>;
end class;


define method lf-directive-char-parser (context :: <template-context>)
=> (parser :: <function>)
   select (context.lf-directive-character)
      '(' => parse-lit-lf-paren;
      '<' => parse-lit-lf-angle;
      '{' => parse-lit-lf-brace;
      '[' => parse-lit-lf-brack;
   end select
end method;

define method lf-directive-mark-parser (context :: <template-context>)
=> (parser :: <function>)
   select (context.lf-directive-character)
      '(' => parse-lex-lf-parens;
      '<' => parse-lex-lf-angles;
      '{' => parse-lex-lf-braces;
      '[' => parse-lex-lf-bracks;
   end select
end method;

define method lf-directive-parser (context :: <template-context>)
=> (parser :: <function>)
   select (context.lf-directive-character)
      '(' => parse-lf-paren-directive;
      '<' => parse-lf-angle-directive;
      '{' => parse-lf-brace-directive;
      '[' => parse-lf-brack-directive;
   end select
end method;


define method rt-directive-char-parser (context :: <template-context>)
=> (parser :: <function>)
   select (context.rt-directive-character)
      ')' => parse-lit-rt-paren;
      '>' => parse-lit-rt-angle;
      '}' => parse-lit-rt-brace;
      ']' => parse-lit-rt-brack;
   end select
end method;

define method rt-directive-mark-parser (context :: <template-context>)
=> (parser :: <function>)
   select (context.rt-directive-character)
      ')' => parse-lex-rt-parens;
      '>' => parse-lex-rt-angles;
      '}' => parse-lex-rt-braces;
      ']' => parse-lex-rt-bracks;
   end select
end method;

define method rt-directive-parser (context :: <template-context>)
=> (parser :: <function>)
   select (context.rt-directive-character)
      ')' => parse-rt-paren-directive;
      '>' => parse-rt-angle-directive;
      '}' => parse-rt-brace-directive;
      ']' => parse-rt-brack-directive;
   end select
end method;

