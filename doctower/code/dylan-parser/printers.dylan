module: dylan-parser
synopsis: Print-object implementations.

define method print-object (o :: <class-definer-token>, s :: <stream>) => ()
   format(s, "{class %= supers %=", o.api-name, o.class-supers);
   unless (o.class-slots.empty?)
      format(s, " slots %=", o.class-slots)
   end unless;
   unless (o.class-keywords.empty?)
      format(s, " keywords %=", o.class-keywords)
   end unless;
   unless (o.scoped-docs.empty?)
      format(s, " docs %=", o.scoped-docs)
   end unless;
   write-element(s, '}');
end method;

define method print-object (o :: <constant-definer-token>, s :: <stream>) => ()
   format(s, "{constant %=", o.api-name);
   when (o.api-type)
      format(s, " :: %=", o.api-type);
   end when;
   when (o.api-value)
      format(s, " = %=", o.api-value);
   end when;
   unless (o.scoped-docs.empty?)
      format(s, " docs %=", o.scoped-docs)
   end unless;
   write-element(s, '}');
end method;

define method print-object (o :: <variable-definer-token>, s :: <stream>) => ()
   format(s, "{variable %=", o.api-name);
   when (o.api-type)
      format(s, " :: %=", o.api-type);
   end when;
   when (o.api-value)
      format(s, " = %=", o.api-value);
   end when;
   unless (o.scoped-docs.empty?)
      format(s, " docs %=", o.scoped-docs)
   end unless;
   write-element(s, '}');
end method;

define method print-object (o :: <doc-comment-token>, s :: <stream>) => ()
   format(s, "{doc %s}", o.source-location);
end method;

define method print-object (o :: <function-definer-token>, s :: <stream>) => ()
   format(s, "{function %= params %= values %=",
      o.api-name, o.func-params, o.func-values);
   unless (o.scoped-docs.empty?)
      format(s, " docs %=", o.scoped-docs)
   end unless;
   write-element(s, '}');
end method;

define method print-object (o :: <method-definer-token>, s :: <stream>) => ()
   format(s, "{method %= params %= values %=",
      o.api-name, o.func-params, o.func-values);
   unless (o.scoped-docs.empty?)
      format(s, " docs %=", o.scoped-docs)
   end unless;
   write-element(s, '}');
end method;

define method print-object (o :: <generic-definer-token>, s :: <stream>) => ()
   format(s, "{generic %= params %= values %=",
      o.api-name, o.func-params, o.func-values);
   unless (o.func-options.empty?)
      format(s, " options %=", o.func-options)
   end unless;
   unless (o.scoped-docs.empty?)
      format(s, " docs %=", o.scoped-docs)
   end unless;
   write-element(s, '}');
end method;

define method print-object (o :: <header-token>, s :: <stream>) => ()
   format(s, "{header %=: %=}", o.hdr-keyword, o.hdr-value);
end method;

// define method print-object (o :: <property-token>, s :: <stream>) => ()
// end method;

define method print-object (o :: <source-record-token>, s :: <stream>) => ()
   format(s, "{source-record hdrs %= defns %=", o.headers, o.definitions);
   unless (o.unscoped-docs.empty?)
      format(s, " docs %=", o.unscoped-docs)
   end unless;
   write-element(s, '}');
end method;

define method print-object (o :: <text-token>, s :: <stream>) => ()
   format(s, "{expr %s}", o.source-location)
end method;

// define method print-object (o :: <all-keys-argument>, s :: <stream>) => ()
// end method;
// 
// define method print-object (o :: <class-keyword>, s :: <stream>) => ()
// end method;
// 
// define method print-object (o :: <class-slot>, s :: <stream>) => ()
// end method;
// 
// define method print-object (o :: <func-argument>, s :: <stream>) => ()
// end method;
// 
// define method print-object (o :: <func-param>, s :: <stream>) => ()
// end method;
// 
// define method print-object (o :: <func-value>, s :: <stream>) => ()
// end method;
// 
// define method print-object (o :: <keyword-argument>, s :: <stream>) => ()
// end method;
// 
// define method print-object (o :: <required-argument>, s :: <stream>) => ()
// end method;
// 
// define method print-object (o :: <required-singleton-argument>, s :: <stream>) => ()
// end method;
// 
// define method print-object (o :: <required-typed-argument>, s :: <stream>) => ()
// end method;
// 
// define method print-object (o :: <required-value>, s :: <stream>) => ()
// end method;
// 
// define method print-object (o :: <rest-argument>, s :: <stream>) => ()
// end method;
// 
// define method print-object (o :: <rest-value>, s :: <stream>) => ()
// end method;
