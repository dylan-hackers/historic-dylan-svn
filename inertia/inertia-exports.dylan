module:    dylan-user
synopsis:  Core UI shapes
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

define library inertia
  use common-dylan;
  use random;
  use io;
  use transcendental;
  use opengl;
  use melange-support;

  export inertia-geometry;
  export inertia-shapes;
end;

// ---------------------------------------------------------------------------------------------- //

define module inertia
  use common-dylan;
  use simple-io;
  use opengl;
  use opengl-glu;
  use opengl-glut;
  use melange-support;
  use inertia-geometry;
  use inertia-shapes;

  export *tess-object*, *screen*, *menu*;
end;

define module inertia-gl-utils
  use common-dylan;
  use simple-io;
  use opengl;
  use opengl-glu;
  use opengl-glut;
  use melange-support;
  
  // export as, element, element-setter;
  export glutxBitmapString, glutxBitmapLength;
end;

define module inertia-geometry
  use common-dylan, exclude: {format-to-string};
  use transcendental;
  use format;
  use print;
  use opengl;

  export
    <point>,
      point-x, point-y, point-x-setter, point-y-setter,
      point-length;
end;

define module inertia-shapes
  use common-dylan;
  use transcendental;
  use random;
  use simple-io;
  use melange-support;
  use opengl;
  use opengl-glu;
  use opengl-glut;
  use inertia;
  use inertia-gl-utils;
  use inertia-geometry;
  
  export
    <shape>,
      add-child, contains-point?, send-event,
      draw-shape, draw-content, draw-outline,
    <polygon>, <spinning-polygon>,
    <rectangle>,
    <screen>,
      mouse-origin, mouse-origin-setter,
    <shape-menu>,
    <shape-editor>,
    <push-button>;
  
  export
    <event>,
      <mouse-event>,
        <mouse-down-event>,
        <mouse-up-event>,
        <mouse-motion-event>,
        <mouse-drag-event>,
        <mouse-gripper-event>;
  
  export timer;
end;

