module: vrml-parser

define inline method concatenate-strings(v :: <stretchy-vector>)
 => result :: <byte-string>;
  let length = for (total = 0 then total + str.size,
                    str :: <byte-string> in v)
               finally total;
               end for;
  
  let result :: <byte-string> = make(<byte-string>, size: length);
  let (init-state, limit, next-state, done?, current-key, current-element,
       current-element-setter) = forward-iteration-protocol(result);
  
  for (result-state = init-state
         then for (char in str,
                   state = result-state then next-state(result, state))
                current-element(result, state) := char;
              finally state;
              end for,
       str :: <byte-string> in v)
  end for;
  result;
end method concatenate-strings;

define method slurp-input(stream :: <buffered-stream>)
 => contents :: <byte-string>;
  let v = make(<stretchy-vector>);
  block ()
    for (buf :: false-or(<buffer>) = get-input-buffer(stream)
	   then next-input-buffer(stream),
	 while: buf)
      let s = buffer-subsequence(buf, <byte-string>,
				 buf.buffer-next,
				 buf.buffer-end);
      add!(v, s);
      buf.buffer-next := buf.buffer-end;
    end for;
  cleanup
    release-input-buffer(stream);
  end block;
  v.concatenate-strings;
end method slurp-input;

define method parse-vrml(file-name :: <string>)
  => (model :: <node>);
  let save-debug = *debug-meta-functions?*;
  block ()
    *debug-meta-functions?* := #t;

    let input-stream = make(<file-stream>, direction: #"input", locator: file-name);
    let input = slurp-input(input-stream);

    let (pos, scene) = scan-vrmlScene(input);
    scene;
  cleanup
    *debug-meta-functions?* := save-debug;
  end;
end;

// possibly skip over white-space, including vrml # comments to EOL
//
define function ws?(str :: <byte-string>, #key start: start :: <integer>, end: stop :: <integer>)
 => (pos :: <integer>);
  let pos = start;
  let in-comment? = #f;
  block (return)
    while (pos < stop)
      let ch = str[pos];
      if (in-comment?)
        if (ch == '\r' | ch == '\n')
          in-comment? := #f;
        end;
      else
        if (ch == '#')
          in-comment? := #t;
        elseif (ch ~== ' ' & ch ~== '\t' & ch ~== '\r' & ch ~== '\n')
          return();
        end;
      end;
      pos := pos + 1;
    end while;
  end block;
  pos;
end ws?;

// should peek() to see if there is a {}[] etc if there is no space
define function ws(str :: <byte-string>, #key start: start :: <integer>, end: stop :: <integer>)
 => (pos :: <integer>);
  let pos = start;
  let newpos = ws?(str, start: start, end: stop);
  if (newpos ~== pos)
    newpos
  else
    #f
  end;
end ws;


// vrmlScene ::=
//     statements ;

define meta vrmlScene (c, node) => (node)
  "#VRML V2.0 utf8",

  // optional comment
  {[element-of(" \t", c), loop({[element-of("\r\n",c), finish()], accept(c)})],
   element-of("\r\n",c)},

  scan-nodeStatement(node) //TODO BGH multiple statements
end vrmlScene;

// statements ::=
//     statement |
//     statement statements |
//     empty ;
// 
// statement ::=
//     nodeStatement |
//     protoStatement |
//     routeStatement ;
// 
// nodeStatement ::=
//     node |
//     DEF nodeNameId node |
//     USE nodeNameId ;

define meta nodeStatement (c, name, node) => (node)
  //TODO BGH implement DEF/USE table
  ws?(c),
  {["DEF", ws(c), scan-Id(name), ws(c)], []},  // optional name
  scan-node(node)
end nodeStatement;
  
// 
// rootNodeStatement ::=
//     node | DEF nodeNameId node ;
// 
// protoStatement ::=
//     proto |
//     externproto ;
// 
// protoStatements ::=
//     protoStatement |
//     protoStatement protoStatements |
//     empty ;
// 
// proto ::=
//     PROTO nodeTypeId [ interfaceDeclarations ] { protoBody } ;
// 
// protoBody ::=
//     protoStatements rootNodeStatement statements ;
// 
// interfaceDeclarations ::=
//     interfaceDeclaration |
//     interfaceDeclaration interfaceDeclarations |
//     empty ;
// 
// restrictedInterfaceDeclaration ::=
//     eventIn fieldType eventInId |
//     eventOut fieldType eventOutId |
//     field fieldType fieldId fieldValue ;
// 
// interfaceDeclaration ::=
//     restrictedInterfaceDeclaration |
//     exposedField fieldType fieldId fieldValue ;
// 
// externproto ::=
//     EXTERNPROTO nodeTypeId [ externInterfaceDeclarations ] URLList ;
// 
// externInterfaceDeclarations ::=
//     externInterfaceDeclaration |
//     externInterfaceDeclaration externInterfaceDeclarations |
//     empty ;
// 
// externInterfaceDeclaration ::=
//     eventIn fieldType eventInId |
//     eventOut fieldType eventOutId |
//     field fieldType fieldId |
//     exposedField fieldType fieldId ;
// 
// routeStatement ::=
//     ROUTE nodeNameId . eventOutId TO nodeNameId . eventInId ;
// 
// URLList ::=
//     mfstringValue ;
// 
// empty ::=
//     ;
//     
//     
//     
//      A.3 Nodes
//     
// 
// node ::=
//     nodeTypeId { nodeBody } |
//     Script { scriptBody } ;

define meta node (c, node) => (node)
  //TODO BGH implement Script

  ws?(c),
  {
   ["Appearance",       ws?(c), "{", scan-AppearanceNode(node), ws?(c), "}"],
   ["Coordinate",       ws?(c), "{", scan-CoordinateNode(node), ws?(c), "}"],
   ["IndexedFaceSet",   ws?(c), "{", scan-IndexedFaceSetNode(node), ws?(c), "}"],
   ["Material",         ws?(c), "{", scan-MaterialNode(node), ws?(c), "}"],
   ["Shape",            ws?(c), "{", scan-ShapeNode(node), ws?(c), "}"],
   ["Transform",        ws?(c), "{", scan-TransformNode(node), ws?(c), "}"]
  }

end node;

// nodeBody ::=
//     nodeBodyElement |
//     nodeBodyElement nodeBody |
//     empty ;

// ignore appearance for now
define meta AppearanceNode (c, material, texture, textureTransform) => (#f)
  loop([ws?(c),
        {["material",         ws(c), scan-SFNode(material)],
         ["texture",          ws(c), scan-SFNode(texture)],
         ["textureTransform", ws(c), scan-SFNode(textureTransform)]}])
end AppearanceNode;


define meta CoordinateNode (c, point) => (point)
  ws?(c),
  "point", ws?(c), scan-MFVec3f(point)
end CoordinateNode;

//aaaaaaaaarrrrrrrrrggggghhhh ... we absolutely need to get backtracking working!!
define meta IndexedFaceSetNode
  (c, coord, ccw, coordIndex, creaseAngle)
  => (begin
        // need to translate polygons from -1 delimited list of points to
        // list of lists of points
        format-out("polygons = %=\n", coordIndex);
        let polys = make(<stretchy-vector>);
        let start = 0;
        for(e in coordIndex, i from 0)
          if (e == -1)
            let poly = as(<simple-object-vector>, copy-sequence(coordIndex, start: start, end: i));
            add!(polys, poly);
            start := i + 1;
          end;
        end;
        format-out("polys = %=\n", polys);
        make(<indexed-face-set>,
             ccw: ccw,
             crease-angle: creaseAngle | 0.0,
             points: as(<simple-object-vector>, coord),
             indices: as(<simple-object-vector>, polys))
      end)
  
  loop([ws?(c),
        {//["color",         ws(c), scan-SFNode(material)],
         ["coordIndex",     ws?(c), scan-MFInt32(coordIndex)],
         ["coord",          ws(c), scan-SFNode(coord)],
         //["normal",          ws(c), scan-SFNode()],
         //["texCoord",          ws(c), scan-SFNode()],
         ["ccw",          ws(c), scan-SFBool(ccw)],
         //["colorIndex",          ws(c), scan-SFNode()],
         //["colorPerVertex",          ws(c), scan-SFNode()],
         //["convex",          ws(c), scan-SFNode()],
         ["creaseAngle",          ws(c), scan-SFFloat(creaseAngle)] // ,
         //["normalIndex",          ws(c), scan-SFNode()],
         //["normalPerVertex",          ws(c), scan-SFNode()],
         //["solid",          ws(c), scan-SFNode()],
         //["texCoordIndex", ws(c), scan-SFNode()]
         }]),
  do(format-out("leaving IndexedFaceSet\n"))
end IndexedFaceSetNode;


define meta MaterialNode (c)
end MaterialNode;
  

define meta ShapeNode (c, appearance, geometry)
  => (make(<shape>, appearance: appearance, geometry: geometry))
  loop([ws?(c),
        {["appearance", ws(c), scan-SFNode(appearance)],
         ["geometry",   ws(c), scan-SFNode(geometry)]}])
end ShapeNode;


define meta TransformNode
  (c, name, center, children, rotation, scale, scaleOrientation, translation)
  => (make(<transform>,
           center: center,
           children: as(<simple-object-vector>, children),
           rotation: rotation,
           scale: scale,
           scale-orientation: scaleOrientation,
           translation: translation))
  loop([ws?(c),
        {["center",           ws(c), scan-SFVec3f(center)],
         ["children",         ws(c), scan-MFNode(children)],
         ["rotation",         ws(c), scan-SFRotation(rotation)],
         ["scale",            ws(c), scan-SFVec3f(scale)],
         ["scaleOrientation", ws(c), scan-SFRotation(scaleOrientation)],
         ["translation",      ws(c), scan-SFVec3f(translation)]}])
end TransformNode;

  
  
// scriptBody ::=
//     scriptBodyElement |
//     scriptBodyElement scriptBody |
//     empty ;
// 
// scriptBodyElement ::=
//     nodeBodyElement |
//     restrictedInterfaceDeclaration |
//     eventIn fieldType eventInId IS eventInId |
//     eventOut fieldType eventOutId IS eventOutId |
//     field fieldType fieldId IS fieldId ;
// 
// nodeBodyElement ::=
//     fieldId fieldValue |
//     fieldId IS fieldId |
//     eventInId IS eventInId |
//     eventOutId IS eventOutId |
//     routeStatement |
//     protoStatement ;
// 
// nodeNameId ::=
//     Id ;
// 
// nodeTypeId ::=
//     Id ;
// 
// fieldId ::=
//     Id ;
// 
// eventInId ::=
//     Id ;
// 
// eventOutId ::=
//     Id ;
// 
// Id ::=
//     IdFirstChar |
//     IdFirstChar IdRestChars ;
// 
// IdFirstChar ::=
//     Any ISO-10646 character encoded using UTF-8 except:
//        0x30-0x39, 0x0-0x20, 0x22, 0x23, 0x27, 0x2b, 0x2c, 0x2d, 0x2e,
//        0x5b, 0x5c, 0x5d, 0x7b, 0x7d, 0x7f ;
// 
// IdRestChars ::=
//     Any number of ISO-10646 characters except:
//        0x0-0x20, 0x22, 0x23, 0x27, 0x2c, 0x2e, 0x5b, 0x5c, 0x5d,
//        0x7b, 0x7d, 0x7f ;

//TODO BGH these are not correct
define constant $IdFirstChar = $letter;
define constant $IdRestChars = concatenate($letter, $digit, "+-_");
    
define collector Id (c)
  element-of($IdFirstChar, c), do(collect(c)),
  loop([element-of($IdRestChars, c), do(collect(c))])
end Id;


//     
//     
//     
//      A.4 Fields
//     
// 
// fieldType ::=
//     MFColor |
//     MFFloat |
//     MFInt32 |
//     MFNode |
//     MFRotation |
//     MFString |
//     MFTime |
//     MFVec2f |
//     MFVec3f |
//     SFBool |
//     SFColor |
//     SFFloat |
//     SFImage |
//     SFInt32 |
//     SFNode |
//     SFRotation |
//     SFString |
//     SFTime |
//     SFVec2f |
//     SFVec3f ;
// 
// fieldValue ::=
//     sfboolValue |
//     sfcolorValue |
//     sffloatValue |
//     sfimageValue |
//     sfint32Value |
//     sfnodeValue |
//     sfrotationValue |
//     sfstringValue |
//     sftimeValue |
//     sfvec2fValue |
//     sfvec3fValue |
//     mfcolorValue |
//     mffloatValue |
//     mfint32Value |
//     mfnodeValue |
//     mfrotationValue |
//     mfstringValue |
//     mftimeValue |
//     mfvec2fValue |
//     mfvec3fValue ;
// 
// sfboolValue ::=
//     TRUE |
//     FALSE ;

define meta SFBool (c, bool) => (bool)
  {["TRUE", yes!(bool)], "FALSE"}
end SFBool;

// sfcolorValue ::=
//     float float float ;

define meta SFColor (c, r, g, b) => (color(r, g, b))
  ws?(c), scan-number(r),
  ws(c), scan-number(g),
  ws(c), scan-number(b)
end SFColor;
  
// sffloatValue ::=
//     float ;

define meta SFFloat (num) => (num)
  scan-number(num)
end SFFloat;
     
// float ::=
//     ([+/-]?((([0-9]+(\.)?)|([0-9]*\.[0-9]+))([eE][+\-]?[0-9]+)?)).
// 
// sfimageValue ::=
//     int32 int32 int32 ...
// 
// sfint32Value ::=
//     int32 ;
// 
// int32 ::=
//     ([+\-]?(([0-9]+)|(0[xX][0-9a-fA-F]+)))
// 
// sfnodeValue ::=
//     nodeStatement |
//     NULL ;

define meta SFNode (c, node) => (node)
  ws?(c),
  {"NULL",
   scan-nodeStatement(node)}
end SFNode;

// sfrotationValue ::=
//     float float float float ;

define meta SFRotation (c, x, y, z, r) => (3d-rotation(x, y, z, r))
  ws?(c), scan-number(x),
  ws(c), scan-number(y),
  ws(c), scan-number(z),
  ws(c), scan-number(r)
end SFRotation;


// sfstringValue ::=
//     string ;
// 
// string ::=
//     ".*" ... double-quotes must be \", backslashes must be \\...
// 
// sftimeValue ::=
//     double ;
// 
// double ::=
//     ([+/-]?((([0-9]+(\.)?)|([0-9]*\.[0-9]+))([eE][+\-]?[0-9]+)?))
// 
// mftimeValue ::=
//     sftimeValue |
//     [ ] |
//     [ sftimeValues ] ;
// 
// sftimeValues ::=
//     sftimeValue |
//     sftimeValue sftimeValues ;
// 
// sfvec2fValue ::=
//     float float ;
// 
// sfvec3fValue ::=
//     float float float ;

define meta SFVec3f (c, x, y, z) => (3d-vector(x, y, z))
  ws?(c), scan-number(x),
  ws(c), scan-number(y),
  ws(c), scan-number(z)
end SFVec3f;

// mfcolorValue ::=
//     sfcolorValue |
//     [ ] |
//     [ sfcolorValues ] ;
// 
// sfcolorValues ::=
//     sfcolorValue |
//     sfcolorValue sfcolorValues ;
// 
// mffloatValue ::=
//     sffloatValue |
//     [ ] |
//     [ sffloatValues ] ;
// 
// sffloatValues ::=
//     sffloatValue |
//     sffloatValue sffloatValues ;
// 
// mfint32Value ::=
//     sfint32Value |
//     [ ] |
//     [ sfint32Values ] ;
//
// sfint32Values ::=
//     sfint32Value |
//     sfint32Value sfint32Values ;

define meta MFInt32 (c, val, vals) => (vals)
  do(vals := make(<stretchy-vector>)),
  ws?(c),
  {[scan-int(val), do(add!(vals, val))],
   ["[", ws?(c),
    {"]",
     [scan-int(val), do(add!(vals, val)),
      loop([ws?(c), ",", ws?(c),
            scan-int(val), do(add!(vals, val))]),
      ws?(c), "]"]}]}
end MFInt32;

// 
// mfnodeValue ::=
//     nodeStatement |
//     [ ] |
//     [ nodeStatements ] ;

define meta MFNode (c, node, nodes) => (nodes | vector(node))
  ws?(c),
  {["[", ws?(c), {"]",
                  [scan-nodeStatements(nodes), ws?(c), "]"]}],
   scan-nodeStatement(node)}   
end MFNode;


// nodeStatements ::=
//     nodeStatement |
//     nodeStatement nodeStatements ;

define meta nodeStatements (c, nodes, node) => (if (nodes.size > 0) nodes else #f end)
  do(nodes := make(<stretchy-vector>)),
  ws?(c),
  scan-nodeStatement(node), do(add!(nodes, node)),
  loop([scan-nodeStatement(node), do(add!(nodes, node))])
end nodeStatements;

// mfrotationValue ::=
//     sfrotationValue |
//     [ ] |
//     [ sfrotationValues ] ;
// 
// sfrotationValues ::=
//     sfrotationValue |
//     sfrotationValue sfrotationValues ;
// 
// mfstringValue ::=
//     sfstringValue |
//     [ ] |
//     [ sfstringValues ] ;
// 
// sfstringValues ::=
//     sfstringValue |
//     sfstringValue sfstringValues ;
// 
// mfvec2fValue ::=
//     sfvec2fValue |
//     [ ] |
//     [ sfvec2fValues] ;
// 
// sfvec2fValues ::=
//     sfvec2fValue |
//     sfvec2fValue sfvec2fValues ;
// 
// mfvec3fValue ::=
//     sfvec3fValue |
//     [ ] |
//     [ sfvec3fValues ] ;
// 
// sfvec3fValues ::=
//     sfvec3fValue |
//     sfvec3fValue sfvec3fValues ;

define meta MFVec3f (c, val, vals) => (vals)
  do(vals := make(<stretchy-vector>)),
  ws?(c),
  {[scan-SFVec3f(val), do(add!(vals, val))],
   
   ["[", ws?(c),
    {"]",
     [scan-SFVec3f(val), do(add!(vals, val)), ws?(c),
      loop([",", ws?(c), scan-SFVec3f(val), do(add!(vals, val)), ws?(c)]),
      "]"]}]},
  ws?(c)
end MFVec3f;
