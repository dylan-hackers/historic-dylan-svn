module:    inertia-shapes
synopsis:  Core UI shapes
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

//
// inertia-shapes.dylan
//

define variable *angle* = 0.0;

// ------------------------------------------------------------------------- //
// class definitions
// ------------------------------------------------------------------------- //

//define open class <shape> (<object>)
define open class <shape> (<persistent>)
  inherited slot fields = table (
    #"shape-left", make (<field>, getter: shape-left, setter: shape-left-setter),
    #"shape-top", make (<field>, getter: shape-top, setter: shape-top-setter)
  );
  
  slot delegate :: subclass(<shape>), init-keyword: delegate:;
  slot children = make (<deque>);
  slot parent :: false-or (<shape>);
  slot origin = make(<point>, x: 0.0, y: 0.0);
  slot extent = make(<point>, x: 100.0, y: 100.0), setter: %extent-setter;
  slot z-angle :: <double-float> = 0.0, init-keyword: angle:;
  slot z-scale :: <double-float> = 1.0;
  slot mouse-mode = #"normal";
  slot fill-color :: <vector> = vector (random-float (0.5) + 0.5,
                                        random-float (0.5) + 0.5,
                                        random-float (0.5) + 0.5, 0.9);
  slot line-color = vector (0.0, 0.0, 0.0, 1.0);
  slot line-width = 3.0;
  slot effects :: <vector> = #[];
  slot reshape = #[#"none", #"none"], init-keyword: reshape:;
  
  virtual slot shape-left :: <double-float>;
  virtual slot shape-top :: <double-float>;
  virtual slot shape-width :: <double-float>;
  virtual slot shape-height :: <double-float>;
end;

define method class-name (shape :: <shape>) "<shape>" end;

define open generic draw-overlay (shape :: <shape>, delegate :: <shape>);

// ------------------------------------------------------------------------- //

define class <container> (<shape>)
end;

define class <screen> (<rectangle>)
  inherited slot fill-color = vector (1.0, 1.0, 1.0, 1.0);
  slot mouse-origin :: <point>;
  slot grabbed-shape :: false-or (<shape>) = #f;
end;

define method class-name (screen :: <screen>) "<screen>" end;

// ------------------------------------------------------------------------- //
// methods definitions
// ------------------------------------------------------------------------- //

// - slot getters and setters ---------------------------------------------- //

define method shape-left (shape :: <shape>) => (result :: <double-float>)
  shape.origin.point-x;
end;

define method shape-left-setter (value :: <double-float>, shape :: <shape>)
 => (result :: <double-float>)
  shape.origin.point-x := value;
end;

define method shape-top (shape :: <shape>)
 => (result :: <double-float>)
  shape.origin.point-y;
end;

define method shape-top-setter (value :: <double-float>, shape :: <shape>)
 => (result :: <double-float>)
  shape.origin.point-y := value;
end;

define method shape-width (shape :: <shape>) => (result :: <double-float>)
  shape.extent.point-x;
end;

define method shape-width-setter (value :: <double-float>, shape :: <shape>)
 => (result :: <double-float>)
  //shape.extent.point-x := value;
  shape.extent := make (<point>, x: value, y: shape.shape-height);
  shape.shape-width;
end;

define method shape-height (shape :: <shape>)
 => (result :: <double-float>)
  shape.extent.point-y;
end;

define method shape-height-setter (value :: <double-float>, shape :: <shape>)
 => (result :: <double-float>)
  //shape.extent.point-y := value;
  shape.extent := make (<point>, x: shape.shape-width, y: value);
  shape.shape-height;
end;

define method extent-setter (value :: <point>, shape :: <shape>)
 => (result :: <point>)
  for (child in shape.children)
    send-event (child, make (<parent-reshape-event>), value - shape.extent);
  end;
  shape.%extent := value;
end;

// ------------------------------------------------------------------------- //

define method screen-origin (shape :: <shape>)
  shape.origin + shape.parent.screen-origin;
end;

define method screen-origin (screen :: <screen>)
  screen.origin;
end;

// ------------------------------------------------------------------------- //

define method add-child (shape :: <shape>, child :: <shape>) => ()
  child.parent := shape;
  shape.children := add! (shape.children, child);
end;

define method shape-add-child (shape :: <shape>, child :: <shape>) => ()
  child.parent := shape;
  shape.children := add! (shape.children, child);
end;

define method remove-child (shape :: <shape>, child :: <shape>) => ()
  child.parent := shape;
  shape.children := remove! (shape.children, child);
end;

define method shape-remove-child (shape :: <shape>, child :: <shape>) => ()
  child.parent := shape;
  shape.children := remove! (shape.children, child);
end;

// - drawing routines ------------------------------------------------------ //

define method draw-shape (shape :: <shape>) => ()
  glPushMatrix ();
    glTranslate (shape.shape-left, shape.shape-top, 0.0);
    glRotate (shape.z-angle, 0.0, 0.0, 1.0);

    glPushMatrix ();
      glScale (shape.z-scale, shape.z-scale, 0.0);

      draw-effects (shape, #"below");

      glClearStencil (#x0);
      glClear ($GL-STENCIL-BUFFER-BIT);
      glEnable ($GL-STENCIL-TEST);
      glStencilFunc ($GL-ALWAYS, #x1, #x1);
      glStencilOp ($GL-REPLACE, $GL-REPLACE, $GL-REPLACE);

      glColor (shape.fill-color[0], shape.fill-color[1], shape.fill-color[2],
               shape.fill-color[3]);
      draw-content (shape, if (slot-initialized? (shape, delegate)) shape.delegate
                           else shape end);

      glStencilFunc ($GL-EQUAL, #x1, #x1);
      glStencilOp ($GL-KEEP, $GL-KEEP, $GL-KEEP);

      draw-effects (shape, #"inside");

      for (i from shape.children.size - 1 to 0 by -1)
      //for (child in shape.children using reverse-iteration-protocol)
        let child = shape.children[i];
        draw-shape (child);
      end;

      glDisable ($GL-STENCIL-TEST);

      draw-effects (shape, #"above");

      glColor (shape.fill-color[0], shape.fill-color[1], shape.fill-color[2],
               shape.fill-color[3]);
      draw-overlay (shape, if (slot-initialized? (shape, delegate)) shape.delegate
                           else shape end);

      glLineWidth (as(<single-float>, shape.line-width));
      glColor (shape.line-color[0], shape.line-color[1], shape.line-color[2],
               shape.line-color[3]);
      draw-outline (shape);
    glPopMatrix ();
  glPopMatrix ();
end;

define method draw-effects (shape :: <shape>, layer :: <effect-layer>)
  for (effect in shape.effects)
    if (effect.effect-layer == layer) draw-effect (shape, effect) end;
  end;
end;

define constant $PI = 3.14159;

// ------------------------------------------------------------------------- //

define method draw-content (shape :: <shape>, delegate :: <shape>) => () end;
define method draw-overlay (shape :: <shape>, delegate :: <shape>) => () end;
define method draw-outline (shape :: <shape>) => () end;

define method contains-point? (shape :: <shape>, point :: <point>)
 => (result :: <boolean>)
  #f
end;

define method on-mouse-event (shape :: <shape>, event :: <mouse-event>,
                              button :: <mouse-button>)
 => ()
  //format-out ("on-mouse-event (%=, %=, %=)\n", shape, event, button);
end;

