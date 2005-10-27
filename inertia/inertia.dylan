module: dylan-user

define library inertia
  use /* library */ dylan;
  use /* library */ opengl;
  export /* module */ inertia-geometry;
  export /* module */ inertia-views;
end;

define module inertia-geometry
  use /* module */ dylan;
  use /* module */ opengl;
  
  export /* classes */ <point>;
end;

define module inertia-views
  use /* module */ inertia-geometry;
  use /* module */ dylan;
  use /* module */ system;
  use /* module */ opengl;
  use /* module */ opengl-glu;
  use /* module */ opengl-glut;
  
  export /* classes */ <view>, <screen>;
  export /* methods */ add-child, draw-view;
end;

// ---------------------------------------------------------------------------------------------- //
// main
// ---------------------------------------------------------------------------------------------- //
/*
begin
  glut-init ();
  glutInitDisplayMode ($GLUT-RGB + $GLUT-DEPTH + $GLUT-DOUBLE);

  glutInitWindowPosition (0, 0);
  glutInitWindowSize (512, 512);
  glutCreateWindow ("Dylan Balls3D");

  glutDisplayFunc (draw);
  glutReshapeFunc (reshape);

  glutMainLoop ();
end;
*/
