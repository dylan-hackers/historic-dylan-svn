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
  
//  use melange-support // for null-pointer
end library;

define module vrml-model
  use dylan;
  
  export
    <node>,
    <container-node>, children,
    <indexed-face-set>, ccw, points, polygon-indices,
    <transform>, scale, translation,
    <line-grid>,
    <sphere>,
    preorder-traversal;
end;

define module vrml-parser
  use dylan;
  use streams;
  use vrml-model;
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
  
//  use melange-support // for null-pointer
end module;
