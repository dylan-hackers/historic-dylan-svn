module: turboblog
author: turbo24prg

define method serve-feed (blog :: <blog>, type, #key) end;


define method serve-feed (blog :: <blog>, type == #"atom", #key feed-entries :: false-or(<sequence>))

  let feed-entries = feed-entries | begin
      let sorted-entries = sort-table(blog.entries, published);
      copy-sequence(sorted-entries, end: if (size(sorted-entries) > 10)
          10 else size(sorted-entries)
        end if);
    end;

  let feed-updated = if (size(feed-entries) > 0)
      first(feed-entries).updated | first(feed-entries).published
    end if;

  let feed-authors = #[];
  for (entry in feed-entries)
    for (author in entry.authors)
      feed-authors := add-new!(feed-authors, author);
    end for;
  end for;

  let feed-categories = #[];
  for (entry in feed-entries)
    for (category in entry.categories)
      feed-categories := add-new!(feed-categories, category);
    end for;
  end for;

  let feed = make(<blog-feed>,
    blog: blog,
    generator: $generator,
    title: blog.title,
    subtitle: blog.subtitle,
    description: blog.description,
    updated: feed-updated | current-date(),
    author: feed-authors,
    categories: feed-categories);
  feed.identifier := requested-url();
  feed.links["self"] := make(<blog-link>, rel: "self", href: requested-url());

  set-content-type(current-response(), "application/atom+xml");
  format(output-stream(current-response()), "%s", generate-atom(feed, entries: feed-entries));
end;

define method generate-atom (entry :: <blog-entry>, #key)
  with-xml()
    entry
    { 
      title(entry.title),
      do(do(method(x) collect(generate-atom(x)) end, entry.links)),
      id(build-uri(permanent-link(entry, full?: #t, escaped?: #t))),
      published(generate-atom(entry.published)),
      updated(generate-atom(entry.updated | entry.published)),
      do(do(method(x) collect(generate-atom(x)) end, entry.authors)),
//      do(do(method(x) collect(generate-atom(x)) end, entry.contributors)),
      do(collect(generate-atom(entry.content)))
    } //missing: category, summary
  end;
end;

define method generate-atom (participant :: <blog-participant>, #key)
  with-xml()
    author {
      name(participant.participant-user.username),
      uri(build-uri(permanent-link(participant.participant-user, full?: #t, escaped?: #t)))
    }
  end;
end;

define method generate-atom (link :: <blog-link>, #key)
  let link = next-method();
  let rel = attribute(link, #"rel");
  if (rel & rel.attribute-value = "edit")
    let href = attribute(link, #"href");
    href.attribute-value := concatenate(href.attribute-value, "?atom");
  end if;
  link;
end;

define function add-atom-namespace (element :: <element>) 
  add-namespace(element, "http://www.w3.org/2005/Atom");
end;
