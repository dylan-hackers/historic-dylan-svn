module: vrml-viewer

define method render-to-opengl(ifs :: <indexed-face-set>)
  if(ifs.ccw)
    glFrontFace($GL-CCW);
  else
    glFrontFace($GL-CW);
  end if;
  let inPoly = #f;
  for(e :: <integer> in ifs.coord-index)
    if (e == -1)
      if (inPoly)
        glEnd();
        inPoly := #f;
      end;
    else
      unless (inPoly)
        glBegin($GL-POLYGON);
        inPoly := #t;
      end;
      let v :: <3d-point> = ifs.coord[e];
      let (x :: <single-float>, y :: <single-float>, z :: <single-float>)
        = values(v[0], v[1], v[2]);
      glVertex(x, y, z);
    end;
  end;
  if (inPoly)
    glEnd();
    inPoly := #f;
  end;
end method render-to-opengl;

define method render-to-opengl(ifs :: <my-indexed-face-set>)
  if(ifs.ccw)
    glFrontFace($GL-CCW);
  else
    glFrontFace($GL-CW);
  end if;
  for(p keyed-by pindex in ifs.polygon-indices)
    with-glBegin($GL-POLYGON)
//      glColor(0.5, 0.5, 0.6);
      for(i keyed-by vindex in p)
        apply(glNormal, ifs.vertex-normals[pindex][vindex]);
        glVertex(ifs.points[i][0], ifs.points[i][1], ifs.points[i][2]);
      end for;
    end with-glBegin;
  end for;
end method render-to-opengl;

define constant $PI = atan(1.0) * 4.0;
define constant rad2deg :: <single-float> = as(<single-float>, 180.0 / $PI);

define method render-to-opengl(transform :: <transform>)
  local 
    method gl-rotate*(v :: <vector>)
      glRotate(v[3] * rad2deg, v[0], v[1], v[2]);
    end method gl-rotate*;
  glPushMatrix();
  transform.translation & apply(glTranslate, transform.translation);
  transform.center      & apply(glTranslate, transform.center);
  transform.rotation    & gl-rotate*        (transform.rotation);
  transform.scale-orientation 
                        & gl-rotate*        (transform.scale-orientation);
  transform.scale       & apply(glScale,     transform.scale);
  transform.scale-orientation 
                        & gl-rotate*        (transform.scale-orientation * -1);
  transform.center      & apply(glTranslate, transform.center * -1);
  next-method();
  glPopMatrix();
end method render-to-opengl;

define method render-to-opengl(line-grid :: <line-grid>)
  glDisable($GL-LIGHTING);
  glPushAttrib($GL-CURRENT-BIT);
  glColor(0.3, 0.3, 0.3);
  with-glBegin($GL-LINES)
    for(x from -10 to 10)
      glVertex( 10.0s0, 0.0s0, as(<single-float>, x));
      glVertex(-10.0s0, 0.0s0, as(<single-float>, x));
      glVertex(as(<single-float>, x), 0.0s0,  10.0s0);
      glVertex(as(<single-float>, x), 0.0s0, -10.0s0);
    end for;
  end with-glBegin;
  glPopAttrib();
  glEnable($GL-LIGHTING);
end method render-to-opengl;

define method render-to-opengl(node :: <shape>)
  if(node.appearance)
    glPushAttrib($GL-ALL-ATTRIB-BITS); // need to find out which ones
                                       // actually need to be saved
//    render-to-opengl(node.appearance);
    render-to-opengl(node.geometry);
    glPopAttrib();
  else
    render-to-opengl(node.geometry);
  end if;
end method render-to-opengl;

define method render-to-opengl(node :: <container-node>)
  for(i in node.children)
    render-to-opengl(i);
  end for;
end method render-to-opengl;

define method render-to-opengl(node :: <sphere>)
  glutSolidSphere(1.0, 10, 10);
end method render-to-opengl;

define method render-to-opengl(node :: <camera>)
  glMatrixMode($GL-PROJECTION);
  glLoadIdentity();
  glFrustum(-0.25, 0.25, -0.25, 0.25, 0.5, 100.0);
  apply(gluLookAt, concatenate(node.eye-position, node.looking-at, node.up));
  glMatrixMode($GL-MODELVIEW);
end method render-to-opengl;

define method render-to-opengl(node :: <spotlight>)
  let id = node.light-id + $GL-LIGHT0;
  glEnable(id);
  apply(glLight, id, $GL-POSITION, node.light-position);
  apply(glLight, id, $GL-SPOT-DIRECTION, node.spot-direction);
  if(node.ambient)
    apply(glLight, id, $GL-AMBIENT, node.ambient);
  end if;
  if(node.diffuse)
    apply(glLight, id, $GL-DIFFUSE, node.diffuse);
  end if;
  if(node.specular)
    apply(glLight, id, $GL-SPECULAR, node.specular);
  end if;
end method render-to-opengl;

define method render-to-opengl(node :: <appearance>)
//  node.material          & render-to-opengl(node.material);
//  node.texture           & render-to-opengl(node.texture);
//  node.texture-transform & render-to-opengl(node.texture-transform);
//  format-out("Material: %=\n", node.material);
end method render-to-opengl;

define method render-to-opengl(node :: <true>)
  format-out("huh?\n");
end method render-to-opengl;
