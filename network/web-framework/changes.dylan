module: changes
author: Hannes Mehnert <hannes@mehnert.org>

define open class <feed> (<object>)
  /* slot CommonAttributes */
  slot authors :: <list> = #(),
    init-keyword: authors:;
  slot categories :: <vector> = #[],
    init-keyword: categories:;
  slot contributors :: <list> = #(),
    init-keyword: contributors:;
  slot generator :: false-or(<generator>) = #f,
    init-keyword: generator:;
  slot icon :: false-or(<uri>) = #f,
    init-keyword: icon:;
  slot identifier :: <uri>,
    init-keyword: identifier:;
  slot links :: <list> = #(),
    init-keyword: links:;
  slot logo :: false-or(<uri>) = #f,
    init-keyword: logo:;
  slot rights :: false-or(<text>) = #f,
    init-keyword: rights:;
  slot subtitle :: false-or(<text>) = #f,
    init-keyword: subtitle:;
  slot title :: <text>,
    init-keyword: title:;
  slot updated :: <date>,
    init-keyword: updated:;
  /* repeated slot extensionElement */
  slot entries :: <string-table> = make(<string-table>),
    init-keyword: entries:;
  slot languages :: <list> = #(),
    init-keyword: languages:;
  slot description :: <text>,
    init-keyword: description:;
  slot published :: <date> = current-date(),
    init-keyword: published:;
end;

define open class <entry> (<object>)
  /* slot CommonAttributes */
  slot authors :: <list> = #(),
    init-keyword: authors:;
  slot categories :: <vector> = #[],
    init-keyword: categories:;
  slot content :: false-or(<content>) = #f,
    init-keyword: content:;
  slot contributors :: <list> = #(),
    init-keyword: contributors:;
  slot identifier :: <uri>,
    init-keyword: identifier:;
  slot links :: <list> = #(),
    init-keyword: links:;
  slot published :: <date> = current-date(),
    init-keyword: published:;
  slot rights :: false-or(<text>) = #f,
    init-keyword: rights:;
  slot source :: false-or(<source>) = #f,
    init-keyword: source:;
  slot summary :: false-or(<text>) = #f,
    init-keyword: summary:;
  slot title :: <text>,
    init-keyword: title:;
  slot updated :: false-or(<date>) = #f,
    init-keyword: updated:;
  slot comments :: <table> = make(<table>),
    init-keyword: comments:;
  slot %comments-count :: <integer> = 0;
  /* repeated slot extensionElement */
end;

define method comments-count (entry :: <entry>)
 => (res :: <integer>);
  entry.%comments-count := entry.%comments-count + 1;
  entry.%comments-count;
end;

define open class <comment> (<object>)
  slot name :: <string>,
    required-init-keyword: name:;
  slot website :: false-or(<uri>) = #f,
    init-keyword: website:;
  slot content :: <content>,
    required-init-keyword: content:;
  slot published :: <date> = current-date(),
    init-keyword: published:; 
end;

define abstract class <content> (<object>)
  /* slot CommonAttributes */
  slot type :: <string>, init-keyword: type:;
  slot content :: <text>, init-keyword: content:;
end;

define class <raw-content> (<content>)
  inherited slot type :: <string> = "raw";
end;

define class <textile-content> (<content>)
  inherited slot type :: <string> = "textile";
end;

define class <xhtml-content> (<content>)
  inherited slot type :: <string> = "xhtml";
end;

/*
define class <inline-text-content> (<content>)
  inherited slot content :: <text>, init-keyword: content:;
end;

define class <inline-xhtml-content> (<content>)
  inherited slot content :: <xhtml-div>, init-keyword: content:;
end;

define class <inline-other-content> (<content>)
  inherited slot content :: <text>, init-keyword: text:;
end;

define class <out-of-line-content> (<content>)
  inherited slot content :: <source>, init-keyword: content:;
end;
*/
define class <person> (<object>)
  slot person-name :: <text>, init-keyword: name:;
  slot uri :: false-or(<uri>) = #f, init-keyword: uri:;
  slot email :: false-or(<email>) = #f, init-keyword: email:;
end;

define class <category> (<object>)
  slot term :: <text>,
    init-keyword: term:;
  slot scheme :: false-or(<uri>) = #f,
    init-keyword: scheme:;
  slot label :: false-or(<text>) = #f,
    init-keyword: label:;
  slot description :: false-or(<text>) = #f,
    init-keyword: description:;
end;

define constant <text> = <string>;

define class <xhtml-div> (<object>)
end;

define constant <uri> = <string>;

define constant <email> = <string>;

define class <generator> (<object>)
  slot uri :: false-or(<uri>) = #f, init-keyword: uri:;
  slot system-version :: false-or(<text>) = #f,
    init-keyword: version:;
  slot text :: <text>, init-keyword: text:;
end;

define class <link> (<object>)
  slot href :: <uri>,
    required-init-keyword: href:;
  slot rel :: false-or(<uri>) = #f,
    init-keyword: rel:;
  slot type :: false-or(<string>) = #f,
    init-keyword: type:;
  slot hreflang :: false-or(<string>) = #f,
    init-keyword: hreflang:;
  slot title :: false-or(<text>) = #f,
    init-keyword: title:;
  slot length :: false-or(<text>) = #f,
    init-keyword: length:;
end;
    
define constant <source> = <feed>;

define open generic permanent-link (object :: <object>, #key #all-keys) => (uri :: <uri>);

define method permanent-link (entry :: <entry>, #key)
 => (uri :: <uri>);
  entry.identifier 
end;

// RSS
define generic generate-rss (object :: <object>);
define method generate-rss (feed :: <feed>)
  with-xml-builder()
    rss (version => "2.0",
          xmlns :: content => "http://purl.org/rss/1.0/modules/content/",
          xmlns :: wfw => "http://wellformedweb.org/CommentAPI/",
          xmlns :: dc => "http://purl.org/dc/elements/1.1/") {
      channel {
        title(feed.title),
        link(first(feed.links)),
        description(feed.description),
        language(feed.language),
        copyright(feed.rights),
        pubDate(as-rfc822-string(feed.published)),
        image {
          url(feed.logo),
          title(feed.title),
          link(first(feed.links))
        },
        do(do(method(entry) collect(generate-rss(entry)) end, feed.entries))
      }
    }
  end;
end method generate-rss;
        
define method generate-rss (entry :: <entry>)
  with-xml()
    item {
      title(entry.title),
      description(escape-xml(entry.content.content)),
      link(entry.identifier),
      guid(entry.identifier),
      pubDate(as-rfc822-string(entry.published)),
    }
  end;       
end method generate-rss;

  
define method generate-xhtml (feed :: <feed>)
  with-xml()
    div {
      h1(concatenate("Recent changes: ", feed.title)),
      text(feed.subtitle),
      ul { do(do(method(x) collect(generate-xhtml(x)) end, feed.entries)) }
    }
  end;
end;

define method generate-xhtml (entry :: <entry>)
  with-xml()
    li {
      do(collect(generate-xhtml(entry.updated))),
      text(entry.title), //link to content
//      do(do(method(x) collect(generate-xhtml(x)) end, entry.authors))
//link to each author
    }
  end;
end;

define method generate-xhtml (person :: <person>)
end;

define method generate-xhtml (date :: <date>)
end;

define open generic generate-atom (object :: <object>, #key #all-keys);

define method generate-atom (feed :: <feed>, #key entries: feed-entries :: false-or(<sequence>))
  with-xml-builder()
    feed (xmlns => "http://www.w3.org/2005/Atom") {
      id(feed.identifier),
      updated(generate-atom(feed.updated)),
      title(feed.title),
      do(if (feed.subtitle & feed.subtitle ~= "")
        with-xml()
          subtitle(feed.subtitle)
        end;
      end if),
      do(do(method(x) collect(generate-atom(x)) end, feed.links)),
      do(if (feed.rights & feed.rights ~= "")
        with-xml()
          rights(feed.rights)
        end;
      end if),
      do(collect(generate-atom(feed.generator))),
      do(do(method(x) collect(generate-atom(x)) end, feed-entries | feed.entries))
    } //missing: category, contributor, icon, logo
  end; 
end;

define method generate-atom (link :: <link>, #key)
  let element = with-xml()
      link(href => link.href)
    end;
  link.rel & add-attribute(element, with-xml()
      !attribute(rel => link.rel) 
    end);
  link.type & add-attribute(element, with-xml()
      !attribute(type => link.type)
    end);
  //missing: title, hreflang, length
  element;
end;

define method generate-atom (person :: <person>, #key)
end;

define method generate-atom (date :: <date>, #key)
  format-date("%Y-%m-%dT%H:%M:%S%:z", date);
end;

define method generate-atom (generator :: <generator>, #key)
  with-xml()
    generator (uri => generator.uri, version => generator.system-version)
    {
      text(generator.text)
    }
  end;
end;

define method generate-atom (entry :: <entry>, #key)
  with-xml()
    entry
    {
      title(entry.title),
//      do(do(method(x) collect(generate-atom(x)) end, entry.links)),
      id(permanent-link(entry)),
      published(generate-atom(entry.published)),
//      updated { do(collect(generate-atom(entry.updated))) },
//      do(do(method(x) collect(generate-atom(x)) end, entry.authors)),
//      do(do(method(x) collect(generate-atom(x)) end, entry.contributors)),
      do(collect(generate-atom(entry.content))),
    } //missing: category, summary
  end;
end;

define method generate-atom (con :: <content>, #key)
  with-xml()
    content {
      text(con.content)
    }
  end;
end;
