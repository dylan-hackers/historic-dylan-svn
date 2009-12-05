This is the project library for the opendylan.org web site.

INSTALLATION
------------

* Modify the provided config.xml for your site.

* Create symlinks for the wiki .dsp and static files.
  cd <your configured dsp root directory>
  ln -s .../trunk/libraries/network/wiki/dsp wiki
  ln -s .../trunk/libraries/network/wiki/www/style.css
  ...TODO: probably more...
  ln -s <wherever>/downloads  (don't want to store these in svn)

* Build the opendylan-dot-org library

* Start the server:

  opendylan-dot-org --config config.xml
