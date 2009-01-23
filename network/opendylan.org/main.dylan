Module: opendylan-dot-org
Synopsis: Web back-end for opendylan.org
Author: Carl Gay

// Add new pages to this list, mapping urls -> source files
// When there are multiple urls for a single page, put the one
// we want to be "canonical" first.
define constant $urls
  = list(#(#("/", "/index"), "/index.dsp"),
         #("/about-dylan", "/about-dylan.dsp"),
         #("/about-gwydion.dsp", "/about-gwydion.dsp"),
         #("/browser", "/browser.dsp"),
         #("/building-win32-cygwin", "/building-win32-cygwin.dsp"),
         #("/community", "/community.dsp"),
         #("/contests", "/contests.dsp"),
         #("/dhc-2002", "/dhc-2002.dsp"),
         #("/dhc2004", "/dhc2004.dsp"),
         #("/dhc", "/dhc.dsp"),
         #(#("/doc", "/docs", "/documentation"), "/documentation.dsp"),
         #(#("/download", "/downloads", "/downloading"), "/downloading.dsp"),
         #("/footer", "/footer.dsp"),
         #("/fragments", "/fragments.dsp"),
         #("/goals", "/goals.dsp"),
         #("/gui", "/gui.dsp"),
         #("/header", "/header.dsp"),
         #("/implementations", "/implementations.dsp"),
         #("/learning", "/learning.dsp"),
         #("/limitations", "/limitations.dsp"),
         #("/maintainers", "/maintainers.dsp"),
         #("/melange", "/melange.dsp"),
         #("/menu", "/menu.dsp"),
         #("/packaging", "/packaging.dsp"),
         #("/ports", "/ports.dsp"),
         #("/projects", "/projects.dsp"),
         #("/release-2.2", "/release-2.2.dsp"),
         #("/release-2.3.1", "/release-2.3.1.dsp"),
         #("/release-2.3.2", "/release-2.3.2.dsp"),
         #("/release-2.3.3", "/release-2.3.3.dsp"),
         #("/release-2.3.4", "/release-2.3.4.dsp"),
         #("/repository", "/repository.dsp"),
         #("/screenshots", "/screenshots.dsp"),
         #("/search", "/search.dsp"),
         #("/shared-libs", "/shared-libs.dsp"),
         #("/statistics", "/statistics.dsp"),
         #("/tools", "/tools.dsp"));

define variable *server-directory* :: <string>
  = "/var/www/opendylan.org";

define variable *static-directory* :: <string>
  = concatenate(*server-directory*, "/static");

define variable *dsp-directory* :: <string>
  = concatenate(*server-directory*, "/dsp");

define class <od-page> (<dylan-server-page>)
end;

define method initialize
    (page :: <od-page>, #rest args, #key source)
  apply(next-method, page,
        source: concatenate(*dsp-directory*, source),
        args)
end;

define constant $header-page :: <od-page>
  = make(<od-page>, source: "/header.dsp");

define constant $footer-page :: <od-page>
  = make(<od-page>, source: "/footer.dsp");

define constant $menu-page :: <od-page>
  = make(<od-page>, source: "/menu.dsp");

define taglib od ()

  body tag standard-header (page :: <od-page>, process-body) ()
    begin // this begin/end shouldn't be necessary, but the macro barfs
      process-template($header-page);
      process-body();
      process-template($menu-page);
    end;

  tag standard-footer (page :: <od-page>) ()
    process-template($footer-page);

end taglib od;

define method main ()
  let http-server = make(<http-server>,
                         root-directory: *server-directory*,
                         document-root: *static-directory*,
                         dsp-root: *dsp-directory*);
  for (item in $urls)
    let (urls, source) = apply(values, item);
    let page = make(<od-page>, source: source);
    for (url in if (instance?(urls, <string>))
                  list(urls)
                else
                  urls
                end)
      add-responder(http-server, parse-uri(url), page);
    end;
  end;

  // Start up
  // todo -- Accept arg to specify *root-url*
  koala-main(server: http-server,
             description: "www.opendylan.org web server");
end method main;

begin
  main()
end;
