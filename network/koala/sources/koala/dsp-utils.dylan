Module: dsp
Author: Carl Gay
Synopsis: Utilities for DSP that otherwise can stand alone.

define open class <paginator> (<sequence>)
  // The underlying sequence we're paging over.
  constant slot paginator-sequence :: <sequence>,
    required-init-keyword: sequence:;

  // 1-based current page number
  slot current-page-number :: <integer> = 1,
    init-keyword: current-page-number:;

  // The number of elements in paginator-sequence to display per page.
  constant slot page-size :: <integer>,
    required-init-keyword: page-size:;
end class <paginator>;

// Total number of pages in this paginator.
define open generic page-count
    (paginator :: <paginator>) => (count :: <integer>);

define method page-count
    (paginator :: <paginator>) => (count :: <integer>)
  ceiling/(paginator.paginator-sequence.size,
           paginator.page-size)
end;

define method next-page-number
    (paginator :: <paginator>) => (pnum :: false-or(<integer>))
  let next = paginator.current-page-number + 1;
  if (paginator.page-count >= next)
    next
  end
end;

define method previous-page-number
    (paginator :: <paginator>) => (pnum :: false-or(<integer>))
  let prev = paginator.current-page-number - 1;
  if (prev >= 1)
    prev
  end
end;

// A sequence of these is returned from the page-links method.
//
define open class <page-link> (<object>)
  // a page number, or #f to indicate that this item shouldn't
  // be a hyperlink.
  constant slot page-link-page-number :: false-or(<integer>),
    required-init-keyword: page-number:;

  // The text to display for this page link.  e.g., the page
  // number or "...".
  constant slot page-link-label :: <string>,
    required-init-keyword: label:;
end class <page-link>;

// Returns a sequence of <page-link>s.
//
define open generic page-links
    (paginator :: <paginator>, #key maximum :: false-or(<integer>))
 => (page-nums :: <sequence>);

define method page-links
    (paginator :: <paginator>, #key maximum :: false-or(<integer>))
 => (page-nums :: <sequence>)
  let total = paginator.page-count;
  let next  = paginator.next-page-number;
  let prev  = paginator.previous-page-number;
  let current = paginator.current-page-number;
  let links = make(<stretchy-vector>);
  if (prev)
    add!(links, make(<page-link>, page-number: prev, label: "Prev"));
  end;
  // todo -- for now we just display all the pages.  should elide some when
  // there are too many, based on the "maximum" parameter...
  for (pn from 1 to total)
    add!(links, make(<page-link>,
                     page-number: iff(pn = current, #f, pn),
                     label: integer-to-string(pn)));
  end;
  if (next)
    add!(links, make(<page-link>, page-number: next, label: "Next"));
  end;
  links
end method page-links;

// fip iterates over the elements in the current page.
// this is probably silly, since just defining paginator-current-page-sequence
// would have been a lot easier.  but i wanted to have the experience of writing
// a fip. :)
//
define method forward-iteration-protocol
    (ptor :: <paginator>)
 => (initial-state :: <object>,
     limit :: <object>,
     next-state :: <function>,
     finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>,
     current-element-setter :: <function>,
     copy-state :: <function>)
  let start-index = (ptor.current-page-number - 1) * ptor.page-size;
  values(start-index,                                   // initial state
	 min(ptor.paginator-sequence.size,              // limit
             start-index + ptor.page-size),
         method (ptor, state) state + 1 end,            // next state
         method (ptor, state, limit) state = limit end, // finished state?
         method (ptor, state) state end,                // current key
	 method (ptor, state)                           // element
           ptor.paginator-sequence[state]
         end,
         method (new-value, ptor, state)                // element setter
           error("<paginator>s are immutable");
         end,
         method (ptor, state) state end)                // copy-state
end method forward-iteration-protocol;

define tag show-page-links in dsp
    (page :: <dylan-server-page>)
    (name :: <string>, url :: <string>, query-value :: <string>, context)
  let paginator :: false-or(<paginator>) = get-context-value(name, context);
  output("%s",
         with-xml ()
           span (class => "paginator") {
             do(let links = paginator.page-links;
                for (page-link :: <page-link> in links,
                     i from 1)
                  let pn = page-link.page-link-page-number;
                  let label = page-link.page-link-label;
                  collect(if (pn)
                            with-xml ()
                              a(label,
                                href => format-to-string("%s%d", url, pn),
                                class => "page-number-link")
                            end
                          else
                            with-xml ()
                              span(label, class => "unlinked-page-number")
                            end;
                          end);
                  if (i < links.size)
                    collect(with-xml ()
                              span(",", class => "page-number-separator")
                            end)
                  end;
                  collect(with-xml () text("\n") end);
                end)
           }
         end with-xml);
end tag show-page-links;

