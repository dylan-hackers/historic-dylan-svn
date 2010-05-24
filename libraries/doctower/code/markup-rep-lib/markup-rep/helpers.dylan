module: markup-rep


/// Synopsis: Creates a limited sequence of the given type with the given elements.
define method make-limited-seq (type :: <type>, elements :: <sequence>)
=> (seq :: <sequence>)
   map-as(type, identity, elements)
end method;


/// Synopsis: Converts a title to a string. This can be done without resolving
/// anything.
define method stringify-title (title :: <title-seq>) => (title :: <string>)
   stringify-title-part(title)
end method;

define method stringify-title-part (string :: <string>) => (string :: <string>)
   string
end method;

define method stringify-title-part (char :: <character>) => (string :: <string>)
   as(<string>, char)
end method;

define method stringify-title-part (img :: <inline-image>) => (string :: <string>)
   concatenate("[", img.alt-text | "img", "]")
end method;

define method stringify-title-part
   (qt :: type-union(<emphasis>, <term-style>, <underline>, <italic>,
                     <bold>, <cite>, <code-phrase>, <term>))
=> (string :: <string>)
   stringify-title-part(qt.text)
end method;

define method stringify-title-part (entity :: <entity>) => (string :: <string>)
   select (entity.code)
      #x2018, #x2019 => "'";
      #x201C, #x201C => "\"";
      otherwise =>
         if (entity.code < #x100) as(<string>, as(<character>, entity.code)) else "?" end;
   end select
end method;

define method stringify-title-part
   (raw :: type-union(<html-content>, <dita-content>))
=> (string :: <string>)
   ""
end method;

define method stringify-title-part (seq :: <sequence>) => (string :: <string>)
   let strings = map-as(<vector>, stringify-title-part, seq);
   apply(concatenate, "", strings)
end method;
