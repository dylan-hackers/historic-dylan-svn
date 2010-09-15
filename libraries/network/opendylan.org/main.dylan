Module: opendylan-dot-org
Synopsis: Web back-end for opendylan.org
Author: Carl Gay

// Add new pages to this list, mapping urls -> source files
// When there are multiple urls for a single page, put the one
// we want to be "canonical" first.
define constant $urls
  = list(#(#("/", "/index"), "index.dsp"),
         #("/about-dylan", "about-dylan.dsp"),
         #("/about-gwydion.dsp", "about-gwydion.dsp"),
         #("/browser", "browser.dsp"),
         #("/building-win32-cygwin", "building-win32-cygwin.dsp"),
         #("/community", "community.dsp"),
         #("/contests", "contests.dsp"),
         #("/dhc-2002", "dhc-2002.dsp"),
         #("/dhc2004", "dhc2004.dsp"),
         #("/dhc", "dhc.dsp"),
         #(#("/doc", "/docs", "/documentation"), "documentation.dsp"),
         #(#("/download", "/downloads", "/downloading"), "downloading.dsp"),
         #("/footer", "footer.dsp"),
         #("/fragments", "fragments.dsp"),
         #("/goals", "goals.dsp"),
         #("/gui", "gui.dsp"),
         #("/header", "header.dsp"),
         #("/implementations", "implementations.dsp"),
         #("/learning", "learning.dsp"),
         #("/limitations", "limitations.dsp"),
         #("/maintainers", "maintainers.dsp"),
         #("/melange", "melange.dsp"),
         #("/menu", "menu.dsp"),
         #("/packaging", "packaging.dsp"),
         #("/ports", "ports.dsp"),
         #("/projects", "projects.dsp"),
         #("/release-2.2", "release-2.2.dsp"),
         #("/release-2.3.1", "release-2.3.1.dsp"),
         #("/release-2.3.2", "release-2.3.2.dsp"),
         #("/release-2.3.3", "release-2.3.3.dsp"),
         #("/release-2.3.4", "release-2.3.4.dsp"),
         #("/repository", "repository.dsp"),
         #("/screenshots", "screenshots.dsp"),
         #("/search", "search.dsp"),
         #("/shared-libs", "shared-libs.dsp"),
         #("/statistics", "statistics.dsp"),
         #("/tools", "tools.dsp"));

define class <od-page> (<dylan-server-page>)
end;

define constant $header-page :: <od-page>
  = make(<od-page>, source: "header.dsp");

define constant $footer-page :: <od-page>
  = make(<od-page>, source: "footer.dsp");

define constant $menu-page :: <od-page>
  = make(<od-page>, source: "menu.dsp");

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

define method add-opendylan-resources
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
      add-resource(http-server, url, page);
    end;
  end;
end method add-opendylan-resources;

define method main
    ()
  koala-main(description: "www.opendylan.org web server",
             before-startup: add-opendylan-resources);
  // temp workaround for bug
  force-output(*standard-output*);
  force-output(*standard-error*);
end;

begin
  main()
end;
