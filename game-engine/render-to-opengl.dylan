module: vrml-viewer

define method render-to-opengl(ifs :: <indexed-face-set>)
  for(p in ifs.polygon-indices)
    let normal = cross-product(ifs.points[p[1]] - ifs.points[p[0]],
                               ifs.points[p[2]] - ifs.points[p[1]]);

    if(~ifs.ccw)
      normal := -1.0 * normal;
    end if;
                               
    with-glBegin($GL-POLYGON)
//      glColor(0.5, 0.5, 0.6);
      glNormal(normal[0], normal[1], normal[2]);
      for(i in p)
        glVertex(ifs.points[i][0], ifs.points[i][1], ifs.points[i][2]);
      end for;
    end with-glBegin;
  end for;
end method render-to-opengl;

define method render-to-opengl(transform :: <transform>)
  glPushMatrix();
  if(transform.translation)
    apply(glTranslate, transform.translation);
  end if;
  if(transform.scale)
    apply(glScale, transform.scale);
  end if;
  next-method();
  glPopMatrix();
end method render-to-opengl;

define method render-to-opengl(line-grid :: <line-grid>)
  glDisable($GL-LIGHTING);
//  glColor(0.3, 0.3, 0.3);
  with-glBegin($GL-LINES)
    for(x from -10 to 10)
      glVertex( 10.0s0, 0.0s0, as(<single-float>, x));
      glVertex(-10.0s0, 0.0s0, as(<single-float>, x));
      glVertex(as(<single-float>, x), 0.0s0,  10.0s0);
      glVertex(as(<single-float>, x), 0.0s0, -10.0s0);
    end for;
  end with-glBegin;
  glEnable($GL-LIGHTING);
end method render-to-opengl;

define method render-to-opengl(node :: <container-node>)
  for(i in node.children)
    render-to-opengl(i);
  end for;
end method render-to-opengl;

define method render-to-opengl(node :: <sphere>)
  glutSolidSphere(1.0, 10, 10);
end method render-to-opengl;
