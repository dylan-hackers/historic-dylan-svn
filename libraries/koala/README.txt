                    README for Koala/DSP version 0.2


This is a preliminary release of the Koala HTTP server and Dylan Server
Pages.


Organization
------------

 koala/config/          -- Koala configuration files.
 koala/sources/koala    -- Koala and Dylan Server Pages sources.
 koala/sources/example  -- A demo application using DSP.
 koala/bin              -- Binaries for the demo application.
 koala/www              -- Web pages for the demo application.
 koala/www/koala        -- Some preliminary Koala/DSP documentation.


Running Koala
-------------

So far Koala only runs on Windows.  I hope to fix this in the next
release.

If you don't have Functional Developer installed, you can start the demo
application by double clicking on koala-example.exe in the koala/bin/
directory.

If you do have Functional Developer installed, open and run the
koala-example.hdp project in koala/sources/example/.  This code only
works in FunDev 2.0.  It does not work in more recent versions (i.e.,
internal development versions) because of recent changes to the locators
library.

Then point your browser at http://localhost:7020/ to browse the
documents in koala/www, or http://localhost:7020/demo/home.dsp for the
DSP demo.


Building Koala
--------------

If you have a source distribution then you can build Koala using
Functional Developer.  Because Koala uses the XML, DOM, and Expat
libraries from Functional Developer, and those libraries aren't (yet)
open source, some extra steps are required to build Koala.  Perform
the following file copy operations:

copy koala/lib/*.ddb to <fundev-root>/Databases/
copy koala/lib/*.lib to <fundev-root>/Lib/
copy koala/lib/*.mkf to <fundev-root>/Lib/

Now open koala/sources/example/koala-example.hdp and build the project.

If you want to make a new distribution of Koala, build the koala-example
project, then use the Build Release command in Functional Developer,
then cd to koala/build and type "ant dist".  This should create a
directory called koala/dist containing the distribution, which you can
then zip up.  See koala/build/build.xml or type "ant help" for details
of other Ant targets.

Any and all feedback appreciated.
carlgay@attbi.com
