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
  use matrix;
  use libpng;
  
//  use melange-support // for null-pointer
end library;

define module vector-math
  use dylan;
  use extensions;
  use transcendentals;
  use subseq;
  use matrix, export: all;

  export <3D-vector>, 3D-vector, <3D-point>, 3D-point, <3D-rotation>, 3D-rotation,
    <color>, color;

  export cross-product, normalize, magnitude;
  export rotate-x, rotate-y, rotate-z;
end module vector-math;

define module vrml-model
  use common-dylan;
  use transcendentals;
  use vector-math;
  use format-out;
  use libpng; // uh, this definitely doesn't belong here
  

  export
    <node>,
    <container-node>, children,
    <my-indexed-face-set>, ccw, points, polygon-indices, vertex-normals, face-normals,
    <indexed-face-set>, coord, coord-index, normal, normal-index, normal-per-vertex, ifs-id, ifs-id-setter, tex-coord, tex-coord-index,
    <transform>, scale, translation, center, rotation, scale-orientation,
    <shape>, appearance, geometry,
    <line-grid>,
    <sphere>,
    <camera>, eye-position, looking-at, looking-at-setter, up, eye-position-setter,
              viewport, viewport-setter,
    <spotlight>, light-position, ambient, diffuse, specular, spot-direction, light-id,
    light-position-setter, spot-direction-setter,
    <appearance>, material, texture, texture-transform,
    <material>, ambient-intensity, diffuse-color, emissive-color, 
    shininess, specular-color, transparency,
    <on-screen-display>,
    <2d-translation>,
    <text>, text, text-setter,
    <texture>, texture-id, width, height, pixel-data, texture-id-setter, repeat-s, repeat-t, depth,
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
  use extensions, exclude: { \assert };
  use standard-io;
  use system;
  use streams;
  use format, exclude: { format-to-string };
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
  use melange-support;
end module;
