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
  use format-out;
  
  export
    <node>,
    <container-node>, children,
    <my-indexed-face-set>, ccw, points, polygon-indices, vertex-normals, face-normals,
    <indexed-face-set>, coord, coord-index, normal, normal-index, normal-per-vertex, ifs-id, ifs-id-setter,
    <transform>, scale, translation, center, rotation, scale-orientation,
    <shape>, appearance, geometry,
    <line-grid>,
    <sphere>,
    <camera>, eye-position, looking-at, up, eye-position-setter, angle, angle-setter,
              viewport, viewport-setter,
    <spotlight>, light-position, ambient, diffuse, specular, spot-direction, light-id,
    <appearance>, material, texture, texture-transform,
    <material>, ambient-intensity, diffuse-color, emissive-color, 
    shininess, specular-color, transparency,
    preorder-traversal;
end;

define module vrml-parser
  use common-dylan;
  use streams;
  use vrml-model;
  use vector-math, import: {color, 3d-rotation, 3d-vector};
  use meta;
  use standard-io;
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
  use transcendentals;

  use opengl;
  use opengl-glu;
  use opengl-glut;
  use gettimeofday;
  use vrml-model;
  use vrml-parser;
  use vector-math;
  
//  use melange-support // for null-pointer
end module;
