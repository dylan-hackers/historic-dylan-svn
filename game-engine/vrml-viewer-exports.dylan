module: dylan-user
author: Andreas Bogk <andreas@andreas.org>
copyright: (C) Andreas Bogk, under LGPL

define library vrml-viewer
  use dylan;
  use common-dylan;
  use io;
  use garbage-collection;
  use opengl;
  use melange-support;
  use meta;
  use collection-extensions;
  
//  use melange-support // for null-pointer
end library;

define module vector-math
  use dylan;
  use transcendentals;
  use subseq;

  export <3D-vector>, 3D-vector, <3D-point>, 3D-point, <3D-rotation>, 3D-rotation,
    <color>, color;

  export cross-product, normalize, magnitude;
end module vector-math;

define module vrml-model
  use common-dylan;
  use transcendentals;
  use vector-math;
  
  export
    <node>,
    <container-node>, children,
    <indexed-face-set>, ccw, points, polygon-indices, vertex-normals, face-normals,
    <transform>, scale, translation,
    <shape>, appearance, geometry,
    <line-grid>,
    <sphere>,
    preorder-traversal;
end;

define module vrml-parser
  use dylan;
  use streams;
  use vrml-model;
  use vector-math, import: {color, 3d-rotation, 3d-vector};
  use meta;
  use format-out;
  
  export parse-vrml;
end;

define module gettimeofday
  use dylan;
  use melange-support;
  use format-out;
  use standard-io;
  use streams;

  export current-time;
end module gettimeofday;

define module vrml-viewer
  use common-dylan;
  use extensions, exclude: { assert };
  use standard-io;
  use system;
  use streams;
  use format;
  use format-out;
  use garbage-collection;

  use opengl;
  use opengl-glu;
  use opengl-glut;
  use gettimeofday;
  use vrml-model;
  use vrml-parser;
  use vector-math;
  
//  use melange-support // for null-pointer
end module;
