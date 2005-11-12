module: changes
author: Hannes Mehnert <hannes@mehnert.org>

define class <feed> (<object>)
  /* slot CommonAttributes */
  slot author :: <list>, init-keyword: author:;
  slot category :: <list> = #(), init-keyword: category:;
  slot contributor :: <list> = #(), init-keyword: contributor:;
  slot generator :: false-or(<generator>) = #f, init-keyword: generator:;
  slot icon :: false-or(<uri>) = #f, init-keyword: icon:;
  slot identifier :: <uri>, init-keyword: identifier:;
  slot link :: <list> = #(), init-keyword: link:;
  slot logo :: false-or(<uri>) = #f, init-keyword: logo:;
  slot rights :: false-or(<text>) = #f, init-keyword: rights:;
  slot subtitle :: false-or(<text>) = #f, init-keyword: subtitle:;
  slot title :: <text>, init-keyword: title:;
  slot updated :: <date>, init-keyword: updated:;
  /* repeated slot extensionElement */
  slot entry :: <list> = #(), init-keyword: entry:;
end;

define class <entry> (<object>)
  /* slot CommonAttributes */
  slot author :: <list> = #(), init-keyword: author:;
  slot category :: <list> = #(), init-keyword: category:;
  slot content :: false-or(<content>) = #f, init-keyword: content:;
  slot contributor :: <list> = #(), init-keyword: contributor:;
  slot identifier :: <uri>, init-keyword: identifier:;
  slot link :: <list> = #(), init-keyword: link:;
  slot published :: false-or(<date>) = #f, init-keyword: published:;
  slot rights :: false-or(<text>) = #f, init-keyword: rights:;
  slot source :: false-or(<source>) = #f, init-keyword: source:;
  slot summary :: false-or(<text>) = #f, init-keyword: summary:;
  slot title :: <text>, init-keyword: title:;
  slot updated :: <date>, init-keyword: updated:;
  /* repeated slot extensionElement */
end;

define abstract class <content> (<object>)
  /* slot CommonAttributes */
  slot type :: <string>, init-keyword: type:;
end;

define class <inline-text-content> (<content>)
  slot content :: <text>, init-keyword: content:;
end;

define class <inline-xhtml-content> (<content>)
  slot content :: <xhtml-div>, init-keyword: content:;
end;

define class <inline-other-content> (<content>)
  slot content :: <text>, init-keyword: text:;
end;

define class <out-of-line-content> (<content>)
  slot content :: <source>, init-keyword: content:;
end;

define class <person> (<object>)
  slot person-name :: <text>, init-keyword: name:;
  slot uri :: false-or(<uri>) = #f, init-keyword: uri:;
  slot email :: false-or(<email>) = #f, init-keyword: email:;
end;

define class <date> (<object>)
end; 

define class <category> (<object>)
  slot term :: <text>, init-keyword: term:;
  slot scheme :: false-or(<uri>) = #f, init-keyword: scheme:;
  slot label :: false-or(<text>) = #f, init-keyword: label:;
end;

define class <text> (<object>)
end;

define class <xhtml-div> (<object>)
end;

define class <uri> (<object>)
end;

define class <email> (<object>)
end;

define class <generator> (<object>)
  slot uri :: false-or(<uri>) = #f, init-keyword: uri:;
  slot version :: false-or(<text>) = #f, init-keyword: version:;
  slot text :: <text>, init-keyword: text:;
end;

define class <link> (<object>)
  slot href :: <uri>, init-keyword: uri:;
  slot rel :: <uri>, init-keyword: rel:;
  slot type :: <string>, init-keyword: type:;
  slot hreflang :: <string>, init-keyword: hreflang:;
  slot title :: false-or(<text>) = #f, init-keyword: title:;
  slot length :: false-or(<text>) = #f, init-keyword: length:;
end;
    
define constant <source> = <feed>;


