module: vrml-viewer

define method render-to-opengl(ifs :: <indexed-face-set>)
  for(p in ifs.polygon-indices)
    with-glBegin($GL-POLYGON)
      glColor(0.0, 1.0, 0.0);
      for(i in p)
        glVertex(ifs.points[i][0], ifs.points[i][1], ifs.points[i][2]);
      end for;
    end with-glBegin;
  end for;
end method render-to-opengl;

define method render-to-opengl(transform :: <transform>)
  glPushMatrix();
  if(transform.scale)
    apply(glScale, transform.scale);
  end if;
  for(i in transform.children)
    render-to-opengl(i);
  end for;
  glPopMatrix();
end method render-to-opengl;
