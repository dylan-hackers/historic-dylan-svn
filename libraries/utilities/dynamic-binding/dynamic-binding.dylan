module: dynamic-binding
synopsis: Implementation of dynamically bound variables.
author: Dustin Voss


define class <dynamic-binding-access> (<condition>)
   constant slot binding-name :: <symbol>, required-init-keyword: #"name";
   constant slot binding-value :: <object>, init-keyword: #"value";
end class;


/**
Synopsis: Indicates an attempt to access a dynamically-scoped binding when the
binding is not in the current dynamic scope.

Slots:
   binding-name   - An instance of <symbol>. The name of the binding to which
                    access was attempted.
*/
define class <binding-not-in-dynamic-scope> (<error>)
   constant slot binding-name :: <symbol>, required-init-keyword: #"name";
end class;


/**
Synopsis: Declares a dynamic scope and bindings accessible within that scope.

The bindings created are accessible within BODY and all code executed within
BODY. The bindings created are only accessible via the 'dynamic-binding' and
'dynamic-binding-setter' macros.

Syntax:
   : with-dynamic-bindings (BINDING, ...)
   :   BODY
   : end

Arguments:
   BINDING  - A binding declaration, e.g. "count :: <integer> = 0". The
              name and initial value are required, but the type is not.
   BODY     - A body.

Values:
   The BODY values.
*/
define macro with-dynamic-bindings
   {  with-dynamic-bindings (?bindings)
         ?:body
      end }
   => {  ?bindings; ?body }
bindings:
   { ?:name :: ?type:expression = ?init:expression, ... }
   => {  let ?name ## "-dyn-bind" :: ?type = ?init;
         let handler
               (<dynamic-binding-access>,
                test:   method (cond :: <dynamic-binding-access>)
                           cond.binding-name = ?#"name"
                        end)
            =  method (cond :: <dynamic-binding-access>, next)
               => (value :: ?type, found? :: <boolean>)
                  if (slot-initialized?(cond, binding-value))
                     ?name ## "-dyn-bind" := cond.binding-value;
                  end if;
                  values(?name ## "-dyn-bind", #t);
               end method;
         ... }
   { } => { }
end macro;


/**
Synopsis: Gets the value of a dynamic binding.

Syntax:
   : dynamic-binding(NAME, default: DEFAULT)

Arguments:
   NAME     - The name of a dynamic binding declared elsewhere by the
              'with-dynamic-bindings' macro.
   DEFAULT  - An optional default value if the dynamic binding is not in the
              current dynamic scope.

Values:
   The value of the NAME.

Conditions:
   Signals <binding-not-in-dynamic-scope> if the dynamic binding is not defined
   in the current dynamic scope and a default expression is not provided.
*/
define macro dynamic-binding
   {  dynamic-binding (?:name) }
   => {  dynamic-binding(
               ?name,
               default: error(make(<binding-not-in-dynamic-scope>, name: ?#"name"))) }

   {  dynamic-binding (?:name, #key ?default:expression) }
   => {  let (val, found?) = signal(make(<dynamic-binding-access>, name: ?#"name"));
         if (found?)
            val
         else
            ?default
         end if }
end macro;


/**
Synopsis: Sets the value of a dynamic binding.

Syntax:
   : dynamic-binding-setter(VALUE, NAME)
   or
   : dynamic-binding(NAME) := VALUE

Arguments:
   NAME  - The name of a dynamic binding declared elsewhere by the
           'with-dynamic-bindings' macro.
   VALUE - The new value to be assigned to the dynamic binding.

Values:
   The VALUE argument.

Conditions:
   Signals <binding-not-in-dynamic-scope> if the dynamic binding is not defined
   in the current dynamic scope.
*/
define macro dynamic-binding-setter
   {  dynamic-binding-setter(?:expression, ?:name) }
   => {  let (val, found?)
            = signal(make(<dynamic-binding-access>, name: ?#"name", value: ?expression));
         if (found?)
            val
         else
            error(make(<binding-not-in-dynamic-scope>, name: ?#"name"))
         end if }
end macro;
