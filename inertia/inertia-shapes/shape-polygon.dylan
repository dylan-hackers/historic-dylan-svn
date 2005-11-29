module:    inertia-shapes
synopsis:  Core UI shapes
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

//
// shape-polygon.dylan
//

define class <polygon> (<shape>)
  slot data = #[
    #[0.0, 0.0,  0.0], #[15.0,  50.0,  0.0], #[0.0, 100.0,  0.0], #[50.0,  85.0,  0.0],
    #[ 100.0,  100.0,  0.0], #[ 85.0,  50.0,  0.0], #[100.0, 0.0,  0.0], #[50.0, 15.0,  0.0]
  ];
  slot data2 = #[
    #[-50.0, -50.0,  0.0], #[-35.0,  0.0,  0.0], #[-50.0, 50.0,  0.0], #[0.0,  35.0,  0.0],
    #[ 50.0,  50.0,  0.0], #[ 35.0,  0.0,  0.0], #[50.0, -50.0,  0.0], #[0.0, -35.0,  0.0]
  ];
  slot vertices;
end;

define class <spinning-polygon> (<polygon>)
  inherited slot effects = vector (make (<shadow-effect>));
end;

define method class-name (polygon :: <polygon>) "<polygon>" end;

define open class <rectangle> (<shape>)
end;

// ------------------------------------------------------------------------- //
// polygon methods definitions
// ------------------------------------------------------------------------- //

define method initialize (polygon :: <polygon>, #rest init-args,
                          #key left = 0.0, top = 0.0)
 => ()
  //apply (next-method, init-args);
  next-method ();
  polygon.shape-left := left;
  polygon.shape-top := top;
  polygon.z-angle := 5.0;

  polygon.vertices := map (method (vertex) as(<GLdouble*>, vertex) end, polygon.data);
end;

define method draw-content (shape :: <shape>, polygon :: <polygon>) => ()
  gluBeginPolygon (*tess-object*);
    for (vertex in polygon.vertices)
      gluTessVertex (*tess-object*, vertex, as(<GLvoid*>, vertex));
    end;
  gluEndPolygon (*tess-object*);
end;

define method draw-outline (polygon :: <polygon>) => ()
  glBegin ($GL-LINE-LOOP);
    for (vertex in polygon.vertices)
      glVertex (vertex[0], vertex[1], vertex[2]);
    end;
  glEnd ();
end;

define constant $X = 0;
define constant $Y = 1;
define constant $Z = 2;

define method contains-point? (polygon :: <polygon>, point :: <point>)
 => (result :: <boolean>)
  let x = point.point-x; let y = point.point-y;
  let v = polygon.data;
  let inside :: <boolean> = #f;

  for (i :: <integer> from 0 below v.size)
    let j :: <integer> = if (i = v.size - 1) 0 else i + 1 end;
    if (((v[i][$Y] <= y) & (v[j][$Y] > y)) | ((v[i][$Y] > y) & (v[j][$Y] <= y)))
      let vt = (y - v[i][$Y]) / (v[j][$Y] - v[i][$Y]);
      if (x < v[i][$X] + vt * (v[j][$X] - v[i][$X]))
        inside := ~inside;
      end;
    end;
  end;  

  inside;
end;

define variable *polygon* = 0;
define variable *speed* = 10;

define variable xtimer = callback-method (n :: <integer>) => ();
  if (*speed* > 0.01)
    glutTimerFunc (n, xtimer, n);
    *polygon*.z-angle := *polygon*.z-angle + *speed*;
    *speed* := *speed* * 0.9;
    glutPostRedisplay ();
  else
    *speed* := 10;
  end;
end;

define method on-mouse-event
    (polygon :: <spinning-polygon>, event :: <mouse-down-event>, button :: <mouse-button>)
 => ()
  next-method ();
  *polygon* := polygon;
  glutTimerFunc (10, xtimer, 10);
end;

