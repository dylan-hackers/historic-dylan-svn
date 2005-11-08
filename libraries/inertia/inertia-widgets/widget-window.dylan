module:    inertia-shapes
synopsis:  Core UI widgets
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

//
// widget-window.dylan
//

// ---------------------------------------------------------------------------------------------- //
// class definitions
// ---------------------------------------------------------------------------------------------- //

// - title-bar ---------------------------------------------------------------------------------- //

define class <title-bar> (<widget>)
  inherited slot fill-color = vector (0.75, 0.87, 1.0, 1.0);
  inherited slot effects = vector (make (<gradient-effect>));
  inherited slot reshape = #[#"size", #"none"];
  slot first-mouse :: <point>;
end;

define method class-name (title-bar :: <title-bar>) "<title-bar>" end;

// - window-sizer ------------------------------------------------------------------------------- //

define class <window-sizer> (<widget>)
  inherited slot reshape = #[#"move", #"move"];
  slot first-mouse :: <point>;
  keyword width: = 25.0;
  keyword height: = 10.0;
end;

define method class-name (window-sizer :: <window-sizer>) "<window-sizer>" end;

// - window ------------------------------------------------------------------------------------- //

define class <window> (<widget>)
  inherited slot line-width = 1.0;
  inherited slot fill-color = vector (0.95, 0.95, 0.95, 1.0);
  inherited slot effects = vector (make (<shadow-effect>));
  slot caption :: <string> = "", init-keyword: caption:;
  slot window-client :: <shape>;
  keyword width: = 300.0;
  keyword height: = 200.0;
end;

define method class-name (window :: <window>) "<window>" end;

define method make-from-symbol (symbol == #"<window>", #rest init-args, #key)
  let object = apply (make, <window>, init-args);
  object;
end;

define method initialize (window :: <window>, #rest init-args, #key width = 300.0, height = 200.0, client = #f) => ()
  next-method ();
  if (client == #f)
    window.window-client := make (<widget>, left: 0.0, top: 25.0, width: width, height: height,
                                  reshape: #[#"size", #"size"]);
  else
    client.shape-top := 25.0;
    window.window-client := client;
  end;
  window.extent := make (<point>, x: window.window-client.shape-width, y: window.window-client.shape-height + 35.0);
  shape-add-child (window, make (<title-bar>, width: window.shape-width, height: 25.0));
  shape-add-child (window, make (<window-sizer>, left: window.shape-width - 25.0, top: window.shape-height - 10.0));
  shape-add-child (window, window.window-client);
end;

// ---------------------------------------------------------------------------------------------- //
// title-bar methods definitions
// ---------------------------------------------------------------------------------------------- //

define method draw-overlay (shape :: <shape>, menu :: <title-bar>)
  next-method ();
  glColor (0.0, 0.0, 0.0, 1.0);
  glPushMatrix ();
  glTranslate (shape.extent.point-x / 2.0, shape.extent.point-y / 2.0, 0.0);
    draw-centered-string (0, 5, shape.parent.caption);
  glPopMatrix ();
end;

define method on-mouse-event (title-bar :: <title-bar>, event :: <mouse-down-event>, button == $left-button)
  title-bar.first-mouse := event.origin;
  next-method ();
end;

define method on-mouse-event (title-bar :: <title-bar>, event :: <mouse-drag-event>, button == $left-button)
  title-bar.parent.origin := title-bar.parent.origin + (event.origin - title-bar.first-mouse);
  next-method ();
end;

// ---------------------------------------------------------------------------------------------- //
// window-sizer methods definitions
// ---------------------------------------------------------------------------------------------- //

define method on-mouse-event (window-sizer :: <window-sizer>, event :: <mouse-down-event>, mouse-button == $left-button)
  window-sizer.first-mouse := event.origin;
  next-method ();
end;

define method on-mouse-event (window-sizer :: <window-sizer>, event :: <mouse-drag-event>, mouse-button == $left-button)
  window-sizer.parent.extent := window-sizer.parent.extent + (event.origin - window-sizer.first-mouse);
  next-method ();
end;

// ---------------------------------------------------------------------------------------------- //
// window methods definitions
// ---------------------------------------------------------------------------------------------- //

define method send-event (window :: <window>, event :: <mouse-down-event>, button :: <mouse-button>)
 => (result :: <shape>)
  remove-child (window.parent, window);
  add-child (window.parent, window);
  next-method ();
end;

define method add-child (window :: <window>, child :: <shape>) => ()
  add-child( window.window-client, child );
end;

define method remove-child (window :: <window>, child :: <shape>) => ()
  remove-child( window.window-client, child );
end;

