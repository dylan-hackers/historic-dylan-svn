This is the project library for the opendylan.org web site.

INSTALLATION
------------

* Create the directory /var/www/opendylan.org.  For now this is
  hard-coded.

* cd /var/www/opendylan.org
  ln -s trunk/libraries/network/opendylan-dot-org/dsp
  ln -s trunk/libraries/network/opendylan-dot-org/static
  ln -s <wherever>/downloads  (don't want to store these in svn)

* Build the opendylan-dot-org library

* Start the server:

  opendylan-dot-org --help



DEV NOTES
---------

Items not implemented in the new version:

* Recent news.  Handle this with the wiki?
  Or by committing to a recent-news.dsp file?

* news.phtml -- Move to wiki


DSP bugs/features:

* modified?(<file-page-mixin>) seems broken
