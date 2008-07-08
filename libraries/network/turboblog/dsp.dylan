module: turboblog
author: turbo24prg

define taglib turboblog () end;

// general page
define class <turboblog-page> (<dylan-server-page>) end;

// specific page with theme
define class <themed-turboblog-page> (<turboblog-page>) end;
 
// errors

define variable *no-blogs-page* =
  make(<turboblog-page>, source: "no-blogs.dsp");

define variable *logged-out-page* =
  make(<turboblog-page>, source: "logged-out.dsp");

define variable *unprivileged-page* =
  make(<turboblog-page>, source: "unprivileged.dsp");

define variable *wrong-username-page* =
  make(<turboblog-page>, source: "wrong-username.dsp");

define variable *wrong-password-page* =
  make(<turboblog-page>, source: "wrong-password.dsp");

define variable *not-logged-in-page* =
  make(<turboblog-page>, source: "not-logged-in.dsp");

define variable *non-existing-user-page* =
  make(<turboblog-page>, source: "non-existing-user.dsp");

define variable *non-existing-blog-page* =
  make(<turboblog-page>, source: "non-existing-blog.dsp");

define variable *non-existing-entry-page* =
  make(<turboblog-page>,
       source: "themes/default/non-existing-entry.dsp");

define variable *non-existing-comment-page* =
  make(<turboblog-page>,
       source: "themes/default/non-existing-comment.dsp");

define variable *missing-page-error-page* =
  make(<turboblog-page>, source: "missing-page-error.dsp");


define method respond-to (type == #"get", page :: <turboblog-page>)

  add-header(current-response().response-headers,
    "X-Pingback", build-uri(transform-uris(*blog-server*.blog-server-url, make(<url>, path: "/RPC2"))));

  let accept-language = header-value(#"accept-language");
  let supported-language = if (accept-language)
      block(return)
        for (language in accept-language.avalue-alist)
          let supported-language =
            element(*http-languages*, head(language), default: #f);
          supported-language & return(supported-language);
        end for;
      end block;
    end if;

  dynamic-bind(*client-language* = supported-language | #"english")
    next-method();
  end;
end;

define method respond-to (type :: one-of(#"get", #"post"), page :: <themed-turboblog-page>)
  if (*blog*)
    last(page.page-source.locator-path) := *blog*.theme;
    page.page-template := #f;
  end if;
  next-method();
end;
