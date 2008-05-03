module: internal-rep

define method print-object (o :: <topic>, s :: <stream>) => ()
   format(s, "{topic title %=, id %s, parent %=, content %=}",
          o.title, o.id, o.parent, o.content);
end method;

define method print-object (o :: <title>, s :: <stream>) => ()
   print-object(o.content, s);
end method;

define method print-object (o :: <paragraph>, s :: <stream>) => ()
   format(s, "{para %=}", o.content)
end method;

define method print-object (o :: <toc-xref>, s :: <stream>) => ()
   format(s, "{toc-xref %= ", o.text);
   if (instance?(o.target, <topic>))
      format(s, "{topic title %=, id %s}", o.target.title, o.target.id);
   else
      format(s, "%=", o.target);
   end if;
   format(s, "}");
end method;

define method print-object (o :: <target-placeholder>, s :: <stream>) => ()
   format(s, "{placeholder %=}", o.target)
end method;
