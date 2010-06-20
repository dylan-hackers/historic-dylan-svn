module: markup-rep


/// Synopsis: Creates a limited sequence of the given type with the given elements.
define method make-limited-seq (type :: <type>, elements :: <sequence>)
=> (seq :: <sequence>)
   map-as(type, identity, elements)
end method;


/// Synopsis: Converts markup to a string. This can be done without resolving
/// anything.
define method stringify-markup (markup :: <markup-seq>) => (markup :: <string>)
   stringify-part(markup)
end method;

/// Synopsis: Converts a title to a string. This can be done without resolving
/// anything.
define method stringify-title (title :: <title-seq>) => (title :: <string>)
   stringify-part(title)
end method;


define method stringify-part (string :: <string>) => (string :: <string>)
   string
end method;

define method stringify-part (char :: <character>) => (string :: <string>)
   as(<string>, char)
end method;

define method stringify-part (img :: <inline-image>) => (string :: <string>)
   concatenate("[", img.alt-text | "img", "]")
end method;

define method stringify-part (conref :: <conref>) => (string :: <string>)
   if (instance?(conref.target, <topic>) & conref.style = #"title")
      stringify-title(conref.target.title)
   else
      ""
   end if
end method;

define method stringify-part
   (qt :: type-union(<emphasis>, <term-style>, <underline>, <italic>,
                     <bold>, <cite>, <code-phrase>, <term>, <xref>,
                     <api-name>, <parm-name>))
=> (string :: <string>)
   stringify-part(qt.text)
end method;

define method stringify-part (entity :: <entity>) => (string :: <string>)
   select (entity.code)
      #x2018, #x2019 => "'";
      #x201C, #x201C => "\"";
      otherwise =>
         if (entity.code < #x100) as(<string>, as(<character>, entity.code)) else "?" end;
   end select
end method;

define method stringify-part
   (raw :: type-union(<html-content>, <dita-content>))
=> (string :: <string>)
   ""
end method;

define method stringify-part (seq :: <sequence>) => (string :: <string>)
   let strings = map-as(<vector>, stringify-part, seq);
   apply(concatenate, "", strings)
end method;

