                    README for Koala/DSP version 0.2


This is a preliminary release of the Koala HTTP server and Dylan Server
Pages.


Organization
------------

koala/bin              -- Binaries for the demo application.
koala/config/          -- Koala configuration files.
koala/lib/             -- Third-party libraries.
koala/sources/koala    -- Koala and Dylan Server Pages sources.
koala/sources/example  -- A demo application using DSP.
koala/www              -- Web pages, including the demo application.
koala/www/koala        -- Some preliminary Koala/DSP documentation.


Running Koala
-------------

So far Koala only runs on Windows.  I hope to fix this in the next
release.

If you're using the binary version, you can start the demo application
by double clicking on koala-example.exe in the koala/bin/ directory.

If you have the source version, see Building Koala, below.

Point your browser at http://localhost:7020/ to browse the documents in
koala/www, or http://localhost:7020/demo/home.dsp for the DSP demo.


Building Koala
--------------

If you have a source distribution then you can build Koala using
Functional Developer.  Just open sources/example/koala-example.hdp and
build the project.  You may want to put a copy of
config/koala-config.xml into sources/example/config/koala-config.xml and
set DEBUG-SERVER to "on".

If you want to make a new distribution of Koala, build the koala-example
project, then use the Build Release command in Functional Developer,
then cd to koala/build and type "ant dist".  This should create a
directory called koala/dist containing the distribution, which you can
then zip up.  See koala/build/build.xml or type "ant help" for details
of other Ant targets.  I use Ant 1.4.1.


Feedback
--------

Any and all feedback appreciated.
carlgay [at] attbi [dot] com
