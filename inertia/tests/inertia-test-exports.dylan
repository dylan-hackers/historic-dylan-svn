module:    dylan-user
synopsis:  Core UI shapes
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

define library inertia-test
  use common-dylan;
  use opengl;
  use inertia;
end;

define module inertia-test
  use common-dylan;
  use opengl;
  use opengl-glut;
  use simple-io;
  use inertia-shapes;
end;

