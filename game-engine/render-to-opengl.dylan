module: vrml-viewer

define class <indexed-face-set> (<object>)
  slot points, init-keyword: points:;
  slot polygon-indices, init-keyword: indices:;
end class <indexed-face-set>;

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
