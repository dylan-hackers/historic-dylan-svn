module:    dylan-user
synopsis:  Inertia exports
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

//
// inertia-exports.dylan
//

define library inertia
  use common-dylan;
  use string-extensions;
  use random;
  use io;
  use transcendental;
  use opengl;
  use melange-support;

  export inertia;
  export inertia-geometry;
  export inertia-shapes;
end;

// ---------------------------------------------------------------------------------------------- //

define module inertia
  use common-dylan; //, rename: {method-definer => dylan-method-definer};
  use simple-io;
  use streams;
  use character-type;
  //use string-conversions;
  use opengl;
  use opengl-glu;
  use opengl-glut;
  use melange-support;
  use inertia-geometry;
  use inertia-shapes;

  export *tess-object*, *screen*, *menu*;
  export 
    <field>,
    <persistent>,
      fields,
      build-object;

  export table, make-from-symbol;
  export *function-table*;
end;

define module inertia-gl-utils
  use common-dylan;
  use simple-io;
  use streams;
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
      shape-left, shape-top, shape-left-setter, shape-top-setter,
      shape-width, shape-height, shape-width-setter, shape-height-setter,
      add-child, contains-point?, send-event, on-mouse-event,
      draw-shape, draw-content, draw-outline, draw-overlay,
      fill-color, reshape,
    <polygon>, <spinning-polygon>,
    <button>,
      on-click,
    <rectangle>,
    <screen>,
      mouse-origin, mouse-origin-setter,
    <shape-menu>,
    <widget>,
    <shape-editor>,
    <push-button>,
    <window>;
  
  export
    <event>,
      <mouse-event>,
        <mouse-down-event>,
        <mouse-up-event>,
        <mouse-motion-event>,
        <mouse-drag-event>,
        <mouse-gripper-event>;

  export
    <mouse-button>,
      $left-button, $middle-button, $right-button;
end;

