Module: opendylan-dot-org
Synopsis: Web back-end for opendylan.org
Author: Carl Gay

// Add new pages to this list, mapping urls -> source files
// When there are multiple urls for a single page, put the one
// we want to be "canonical" first.
define constant $urls
  = list(#(#("/", "/index"), "opendylan.org/index.dsp"),
         #("/about-dylan", "opendylan.org/about-dylan.dsp"),
         #("/about-gwydion.dsp", "opendylan.org/about-gwydion.dsp"),
         #("/browser", "opendylan.org/browser.dsp"),
         #("/building-win32-cygwin", "opendylan.org/building-win32-cygwin.dsp"),
         #("/community", "opendylan.org/community.dsp"),
         #("/contests", "opendylan.org/contests.dsp"),
         #("/dhc-2002", "opendylan.org/dhc-2002.dsp"),
         #("/dhc2004", "opendylan.org/dhc2004.dsp"),
         #("/dhc", "opendylan.org/dhc.dsp"),
         #(#("/doc", "/docs", "/documentation"), "opendylan.org/documentation.dsp"),
         #(#("/download", "/downloads", "/downloading"), "opendylan.org/downloading.dsp"),
         #("/footer", "opendylan.org/footer.dsp"),
         #("/fragments", "opendylan.org/fragments.dsp"),
         #("/goals", "opendylan.org/goals.dsp"),
         #("/gui", "opendylan.org/gui.dsp"),
         #("/header", "opendylan.org/header.dsp"),
         #("/implementations", "opendylan.org/implementations.dsp"),
         #("/learning", "opendylan.org/learning.dsp"),
         #("/limitations", "opendylan.org/limitations.dsp"),
         #("/maintainers", "opendylan.org/maintainers.dsp"),
         #("/melange", "opendylan.org/melange.dsp"),
         #("/menu", "opendylan.org/menu.dsp"),
         #("/packaging", "opendylan.org/packaging.dsp"),
         #("/ports", "opendylan.org/ports.dsp"),
         #("/projects", "opendylan.org/projects.dsp"),
         #("/release-2.2", "opendylan.org/release-2.2.dsp"),
         #("/release-2.3.1", "opendylan.org/release-2.3.1.dsp"),
         #("/release-2.3.2", "opendylan.org/release-2.3.2.dsp"),
         #("/release-2.3.3", "opendylan.org/release-2.3.3.dsp"),
         #("/release-2.3.4", "opendylan.org/release-2.3.4.dsp"),
         #("/repository", "opendylan.org/repository.dsp"),
         #("/screenshots", "opendylan.org/screenshots.dsp"),
         #("/search", "opendylan.org/search.dsp"),
         #("/shared-libs", "opendylan.org/shared-libs.dsp"),
         #("/statistics", "opendylan.org/statistics.dsp"),
         #("/tools", "opendylan.org/tools.dsp"));

define class <od-page> (<dylan-server-page>)
end;

define constant $header-page :: <od-page>
  = make(<od-page>, source: "opendylan.org/header.dsp");

define constant $footer-page :: <od-page>
  = make(<od-page>, source: "opendylan.org/footer.dsp");

define constant $menu-page :: <od-page>
  = make(<od-page>, source: "opendylan.org/menu.dsp");

define taglib od ()

  body tag short-header (page :: <od-page>, process-body) ()
    begin // this begin/end shouldn't be necessary, but the macro barfs
      process-template($header-page);
      output("<title>");
      process-body();
      output("</title>");
      process-template($menu-page);
    end;

  body tag standard-header (page :: <od-page>, process-body) ()
    begin // this begin/end shouldn't be necessary, but the macro barfs
      short-header-tag(page, process-body);
      output("<h2>");
      process-body();
      output("</h2>\n");
    end;

  tag standard-footer (page :: <od-page>) ()
    process-template($footer-page);

end taglib od;

define method add-opendylan-responders
    (http-server :: <http-server>)
  add-wiki-responders(http-server);
  for (item in $urls)
    let (urls, source) = apply(values, item);
    let page = make(<od-page>, source: source);
    for (url in if (instance?(urls, <string>))
                  list(urls)
                else
                  urls
                end)
      add-responder(http-server, url, page);
    end;
  end;
  add-cgi-directory-responder(http-server,
                              "/cgi-bin/cvszilla",
                              "/usr/share/cvszilla/cgi-bin");
  add-cgi-directory-responder(http-server,
                              "/cgi-bin",
                              "/usr/local/viewvc-1.0.5/bin/cgi");
  add-cgi-directory-responder(http-server,
                              "/cgi-bin/bugzilla",
                              "/usr/lib/cgi-bin/bugzilla");
end method add-opendylan-responders;

define method main
    ()
  koala-main(description: "www.opendylan.org web server",
             before-startup: add-opendylan-responders);
end;

begin
  main()
end;
