This is the project library for the opendylan.org web site.

INSTALLATION
------------

* Modify the provided config.xml for your site.  The only changes
  necessary SHOULD be to the "location" attributes of the config
  elements.

* Build the opendylan-dot-org library

* Start the server:
  opendylan-dot-org --config /home/cgay/opendylan.org/config.xml

TODO
----

* Add a config element for opendylan-dot-org so we don't depend on
  the --working-directory command-line option.

* The links on the "downloading" page are all broken.  Need to copy
  the downloading directory or symlink to it.
