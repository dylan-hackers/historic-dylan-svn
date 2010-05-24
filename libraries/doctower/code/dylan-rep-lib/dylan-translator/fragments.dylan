module: dylan-translator


//
// Conversion from token to representation
//


define method name-from-text (context :: <context>, token :: <text-name-token>)
=> (name :: <source-name>)
   let text-name = token.api-name;
   select (context.context-name by instance?)
      <module-name> =>
         make(<binding-name>, binding: text-name, within: context.context-name,
              source-location: token.token-src-loc);
      <library-name> =>
         make(<module-name>, module: text-name, within: context.context-name,
              source-location: token.token-src-loc);
   end select
end method;


define method fragment-from-text
   (context :: <context>, frag-class :: subclass(<fragment>), token :: <text-token>)
=> (frag :: <fragment>)
   let frag-text = map-as(<vector>,
         method (text-elem :: type-union(<character>, <text-name-token>))
         => (frag-elem :: type-union(<character>, <source-name>))
            if (instance?(text-elem, <text-name-token>))
               name-from-text(context, text-elem)
            else
               text-elem
            end if
         end, token.token-text);
   make(frag-class, text: frag-text, source-location: token.token-src-loc)
end method;


define method type-fragment-from-text (context :: <context>, token :: <text-token>)
=> (frag :: <type-fragment>)
   fragment-from-text(context, <type-fragment>, token)
end method;


define method code-fragment-from-text (context :: <context>, token :: <text-token>)
=> (frag :: <code-fragment>)
   fragment-from-text(context, <code-fragment>, token)
end method;


define method computed-constant-from-text (context :: <context>, token :: <text-token>)
=> (frag :: <computed-constant>)
   fragment-from-text(context, <computed-constant>, token)
end method;


define method singleton-from-text (context :: <context>, token :: <text-token>)
=> (frag :: <singleton-type-fragment>)
   let operand-frag = fragment-from-text(context, <computed-constant>, token);
   let operand-text = operand-frag.source-text;
   let full-text = concatenate(vector($singleton-name, '('), operand-text, vector(')'));
   make(<singleton-type-fragment>, source-location: token.token-src-loc,
        text: full-text, expression: operand-frag)
end method;


define method vendor-option-from-property
   (context :: <context>, tok :: <property-token>)
=> (opt :: <vendor-option>)
   make(<vendor-option>, symbol: tok.prop-name,
        fragment: fragment-from-text(context, <code-fragment>, tok.prop-value))
end method;


//
// Convenience
//


define method as (cls == <type-fragment>, name :: <binding-name>)
=> (frag :: <type-fragment>)
   make(<type-fragment>, text: vector(name), source-location: name.source-location)
end method;


define constant $singleton-name = 
      make(<binding-name>, library: "Dylan", module: "Dylan", binding: "Singleton");
      
define constant $object-type = as(<type-fragment>, 
      make(<binding-name>, library: "Dylan", module: "Dylan", binding: "<Object>"));

