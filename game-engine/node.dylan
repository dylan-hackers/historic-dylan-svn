module: vrml-model


define abstract class <node> (<object>)
end class <node>;

// <child-vector> can contain any of
// Anchor, Background, Billboard, Collision, ColorInterpolator, CoordinateInterpolator,
// CylinderSensor, DirectionalLight, Fog, Group, Inline, LOD, NavigationInfo,
// NormalInterpolator, OrientationInterpolator, PlaneSensor, PointLight,
// PositionInterpolator, ProximitySensor, ScalarInterpolator, Script, Shape, Sound,
// SpotLight, SphereSensor, Switch, TimeSensor, TouchSensor, Transform, Viewpoint,
// VisibilitySensor, WorldInfo

define constant <child-vector> = <simple-object-vector>;

define class <container-node> (<node>)
  slot children :: <simple-object-vector>, required-init-keyword: children:;
end class <container-node>;

define class <indexed-face-set> (<node>)
  slot ccw :: <boolean> = #f, init-keyword: ccw:; // orientation of faces
  slot points, init-keyword: points:;
  slot polygon-indices, init-keyword: indices:;
  slot face-normals :: false-or(<vector>) = #f, init-keyword: face-normals:;
  slot vertex-normals :: false-or(<vector>) = #f, init-keyword: vertex-normals:;
  slot crease-angle :: <float> = 0.0, init-keyword: crease-angle:;
end class <indexed-face-set>;

define method initialize(ifs :: <indexed-face-set>, #key, #all-keys)
 => ()
  next-method();
  unless(ifs.face-normals)
    ifs.face-normals := make(<vector>, size: ifs.polygon-indices.size);
    for(p keyed-by i in ifs.polygon-indices)
      let normal = cross-product(ifs.points[p[1]] - ifs.points[p[0]],
                                 ifs.points[p[2]] - ifs.points[p[1]]);
      
      if(~ifs.ccw)
        normal := -1.0 * normal;
      end if;
      ifs.face-normals[i] := normalize(normal);
    end for;
  end unless;
  unless(ifs.vertex-normals)
    ifs.vertex-normals := make(<vector>, size: ifs.polygon-indices.size);
    for(p keyed-by i in ifs.polygon-indices)
      ifs.vertex-normals[i] := make(<vector>, size: p.size);
      for(j from 0 below p.size)
        let adjoining-faces = #();
        for(k from 0 below ifs.polygon-indices.size)
          if((i ~= k) 
               & member?(ifs.polygon-indices[i][j], ifs.polygon-indices[k])
               & (ifs.face-normals[i] * ifs.face-normals[k] > cos(ifs.crease-angle)))
            adjoining-faces := add(adjoining-faces, k);
          end if;
        end for;
        let sum = ifs.face-normals[i];
        for(k in adjoining-faces)
          sum := sum + ifs.face-normals[k];
        end for;
        sum := sum / (adjoining-faces.size + 1);
          
        ifs.vertex-normals[i][j] := sum;
      end for;
    end for;
  end unless;
end method initialize;

define class <transform> (<container-node>)
  slot center :: false-or(<3d-vector>) = #f, init-keyword: center:;
  slot rotation :: false-or(<3d-rotation>) = #f, init-keyword: rotation:;
  slot scale :: false-or(<3d-vector>) = #f, init-keyword: scale:;
  slot scale-orientation :: false-or(<3d-rotation>) = #f, init-keyword: scale-orientation:;
  slot translation :: false-or(<3d-vector>) = #f, init-keyword: translate:;
end class <transform>;

define constant <geometry-node> = <node>;
// IndexedFaceSet, Box, Cone, Cylinder, ElevationGrid, Extrusion, IndexedLineSet, PointSet, Sphere, Text

define class <shape> (<node>)
  slot appearance, init-keyword: appearance:;
  slot geometry :: <geometry-node>, required-init-keyword: geometry:;
end class <shape>;

define class <line-grid> (<node>)
end class <line-grid>;

define class <sphere> (<node>)
end class <sphere>;

define class <camera> (<node>)
  slot eye-position :: <3d-point> = 3d-point(0.0, 1.7, 3.0), init-keyword: position:;
  slot looking-at :: <3d-point> = 3d-point(0.0, 1.7, 0.0), init-keyword: looking-at:;
  slot up :: <3d-vector> = 3d-vector(0.0, 1.0, 0.0), init-keyword: up:;
end class <camera>;

define class <spotlight> (<node>)
  slot light-position :: <3d-point> = 3d-point(0.0, 0.0, 0.0, 1.0), init-keyword: position:;
  slot spot-direction :: <3d-vector> = 3d-vector(0.0, 0.0, -1.0), init-keyword: direction:;
  slot ambient :: false-or(<vector>) = #f, init-keyword: ambient:;
  slot diffuse :: false-or(<vector>) = #f, init-keyword: diffuse:;
  slot specular :: false-or(<vector>) = #f, init-keyword: specular:;
  slot light-id;
  class slot next-free-id = 0;
end class <spotlight>;

define method initialize(light :: <spotlight>, #key, #all-keys)
  next-method();
  light.light-id := light.next-free-id;
  light.next-free-id := light.next-free-id + 1;
end method initialize;

define generic preorder-traversal(node :: <node>, function :: <function>);

define method preorder-traversal(node :: <node>, function :: <function>)
  function(node);
end method preorder-traversal;

define method preorder-traversal(node :: <container-node>, 
                                 function :: <function>)
  function(node);
  for(i in node.children)
    preorder-traversal(i, function);
  end for;
end method preorder-traversal;
