module: template-engine


//
// Constant expressions
//


define method resolve-const-expression
   (context :: <template-context>, exp :: <expression-token>)
=> ()
   #f
end method;


define method resolve-const-expression
   (context :: <template-context>, exp :: <binary-expression-token>)
=> ()
   if (exp.left-operand.const-value? & exp.right-operand.const-value?)
      exp.const-value? := #t;
      exp.value := resolve-expression(context.template-config, exp);
   end if
end method;


define method resolve-const-expression
   (context :: <template-context>, exp :: <binary-operand-token>)
=> ()
   if (exp.operand.const-value?)
      exp.const-value? := #t;
      exp.value := resolve-expression(context.template-config, exp)
   end if
end method;


define method resolve-const-expression
   (context :: <template-context>, exp :: <operand-token>)
=> ()
   if (exp.chained-calls.empty?)
      exp.const-value? := exp.base-value.const-value?;
      exp.value := exp.base-value.value;
   end if
end method;


//
// Dynamic expressions
//


define method resolve-dyn-expression
   (template :: <template>, exp :: <expression-token>)
=> (value)
   assert(exp.const-value?,
         "Non-constant token %= is not being resolved at pos %s",
         exp, exp.parse-start);
   exp.value
end method;
   

define method resolve-dyn-expression
   (template :: <template>, exp :: <binary-expression-token>)
=> (value)
   unless (exp.const-value?)
      resolve-dyn-expression(template, exp.left-operand);
      resolve-dyn-expression(template, exp.right-operand);
      exp.value := resolve-expression(template, exp)
   end unless;
   exp.value
end method;


define method resolve-dyn-expression
   (template :: <template>, exp :: <binary-tier-1-expression-token>)
=> (value)
   unless (exp.const-value?)
      let op = exp.operator;
      let left = resolve-dyn-expression(template, exp.left-operand);
      case
         instance?(op, <lex-ampersand-token>) & left.false? => exp.value := #f;
         instance?(op, <lex-vert-bar-token>) & left.true? => exp.value := #t;
         otherwise =>
            resolve-dyn-expression(template, exp.right-operand);
            exp.value := resolve-expression(template, exp);
      end case;
   end unless;
   exp.value
end method;


define method resolve-dyn-expression
   (template :: <template>, exp :: <binary-operand-token>)
=> (value)
   unless (exp.const-value?)
      resolve-dyn-expression(template, exp.operand);
      exp.value := resolve-expression(template, exp)
   end unless;
   exp.value
end method;


define method resolve-dyn-expression
   (template :: <template>, exp :: <operand-token>)
=> (value)
   unless (exp.const-value?)
      resolve-dyn-expression(template, exp.base-value);
      exp.value := resolve-expression(template, exp);
   end unless;
   exp.value
end method;


define method resolve-dyn-expression
   (template :: <template>, exp :: <var-name-token>)
=> ()
   exp.value := resolve-expression(template, exp)
end method;


//
// Expression resolution
//


define method resolve-expression
   (template :: <template>, exp :: <binary-expression-token>)
=> (res :: <object>)
   let left = exp.left-operand.value;
   let right = exp.right-operand.value;
   block ()
      select (exp.operator by instance?)
         <lex-ampersand-token> => left & right;
         <lex-vert-bar-token>  => left | right;
         <lex-star-token>      => left * right;
         <lex-slash-token>     => left / right;
         <lex-percent-token>   => modulo(left, right);
         <lex-plus-token>      => left + right;
         <lex-minus-token>     => left - right;
         <lex-equal-token>     => left = right;
         <lex-not-equal-token> => left ~= right;
         <lex-lf-angle-token>  => left < right;
         <lex-rt-angle-token>  => left > right;
         <lex-lte-token>       => left <= right;
         <lex-gte-token>       => left >= right;
      end select
   exception (err :: <error>)
      error(make(<expression-error>, position: exp.parse-start, error: err))
   end block
end method;


define method resolve-expression
   (template :: <template>, exp :: <binary-operand-token>)
=> (res :: <object>)
   let op-value = exp.operand.value;
   block ()
      select (exp.operator by instance?)
         <lex-minus-token> => -op-value;
         <lex-not-token>   => ~op-value;
         otherwise         => op-value;
      end select
   exception (err :: <error>)
      error(make(<expression-error>, position: exp.parse-start, error: err))
   end block
end method;


define method resolve-expression
   (template :: <template>, exp :: <operand-token>)
=> (result :: <object>)
   let result = exp.base-value.value;
   for (call :: <chained-call-token> in exp.chained-calls)
      let called-func = resolve-operation(template, call.name, call);
      let new-result =
            block ()
               result.called-func
            exception (err :: <error>)
               error(make(<expression-error>, position: call.parse-start, error: err))
            end block;
      result := new-result
   end for;
   result
end method;


define method resolve-expression
   (template :: <template>, exp :: <var-name-token>)
=> (result :: <object>)
   let (result, found?) = named-value(exp.name, template.variable-scopes);
   when (~found?)
      result := signal(make(<missing-name-error>, position: exp.parse-start,
                            name: exp.name, operation: #f))
   end when;
   result
end method;


define method resolve-operation
   (template :: <template>, name :: <string>, token :: <token>)
=> (operation :: <function>)
   named-value(name, template.operation-scopes)
         | signal(make(<missing-name-error>, position: token.parse-start,
                       name: name, operation: #t))
end method;
