library: opengl
module: dylan-user
author: Jeff Dubrule <igor@pobox.com>
copyright: (C) Jefferson Dubrule.  See COPYING.LIB for license details.

define library opengl
  use dylan;
  use melange-support;
  export opengl;
  export opengl-glut;
  export opengl-glu;
end library;

define module opengl
  use dylan;
  use extensions;
  use melange-support;
  
  // Helpful macros:
  export
    with-glBegin, with-glNewList;

  // Massaged functions:
  export
    glColor, glColor3, glColor4,
    glEvalCoord, glEvalCoord1, glEvalCoord2,
    glFog,
    glIndex,
    glLight,
    glLightModel,
    glMaterial,
    glMultMatrix,
    glNormal,
    glPixelStore,
    glPixelTransfer,
    glRasterPos, glRasterPos2, glRasterPos3, glRasterPos4,
    glRect,
    glRotate,
    glScale,
    glTexCoord, glTexCoord1, glTexCoord2, glTexCoord3, glTexCoord4,
    glTexEnv,
    glTexGen,
    glTexParameter,
    glTranslate,
    glVertex, glVertex2, glVertex3, glVertex4;

  // Will go away to be replaced by 'smart' methods:
  export
    glLoadMatrixd,
    glLoadMatrixf,
    glMap1d,
    glMap1f,
    glMap2d,
    glMap2f,
    glMapGrid1d,
    glMapGrid1f,
    glMapGrid2d,
    glMapGrid2f,
    glPixelMapfv,
    glPixelMapuiv,
    glPixelMapusv;

  // OpenGL 1.2 Functions:
  export
    glDrawRangeElements,
    glCopyTexSubImage3D;

  // Add error checking, where possible:
  export
    glAccum,
    glAlphaFunc,
    glBegin, glEnd,
    glBindTexture,
    glBlendFunc,
    glColorMaterial,
    glCopyPixels,
    glCopyTexImage1D,
    glCopyTexImage2D,

    glCopyTexSubImage1D,
    glCopyTexSubImage2D,
    glCullFace,
    glDeleteLists,
    glDepthFunc,
    glDepthMask,
    glEnable, glDisable,
    glEnableClientState, glDisableClientState,
    glDrawArrays,
    glDrawBuffer,
    glEvalMesh1,
    glEvalMesh2,
    glHint,
    glLogicOp,
    glMatrixMode,
    glNewList, glEndList,
    glPolygonMode,
    glReadBuffer,
    glRenderMode,
    glScissor,
    glShadeModel,
    glStencilFunc,
    glStencilOp,
    glViewport;
    
  // Need work:
  export
    glCallLists,
    glAreTexturesResident,
    glBitmap,
    glCallLists,
    glClear,
    glClipPlane,
    glColorMask,
    glColorPointer,
    glDeleteTextures,
    glDepthRange,
    glDrawElements,
    glDrawPixels,
    glEdgeFlag,
    glEdgeFlagPointer,
    glEvalPoint1,
    glEvalPoint2,
    glFeedbackBuffer,
    glFrontFace,
    glFrustum,
    glGenTextures,
    glGetBooleanv,
    glGetDoublev,
    glGetFloatv,
    glGetIntegerv,
    glGetLightfv,
    glGetLightiv,
    glGetMapdv,
    glGetMapfv,
    glGetMapiv,
    glGetMaterialfv,
    glGetMaterialiv,
    glGetPixelMapfv,
    glGetPixelMapuiv,
    glGetPixelMapusv,
    glGetPointerv,
    glGetTexEnvfv,
    glGetTexEnviv,
    glGetTexGendv,
    glGetTexGenfv,
    glGetTexGeniv,
    glGetTexLevelParameterfv,
    glGetTexLevelParameteriv,
    glGetTexParameterfv,
    glGetTexParameteriv,
    glGetClipPlane,
    glGetError,
    glGetPolygonStipple,
    glGetTexImage,
    glIndexPointer,
    glInterleavedArrays,
    glIsEnabled,
    glIsList,
    glIsTexture,
    glNormalPointer,
    glPolygonStipple,
    glPrioritizeTextures,
    glPushAttrib, glPopAttrib,
    glPushClientAttrib, glPopClientAttrib,
    glReadPixels,
    glSelectBuffer,
    glTexCoordPointer,
    glTexImage1D,
    glTexImage2D,
    glTexImage3D,
    glTexSubImage1D,
    glTexSubImage2D,
    glTexSubImage3D,
    glVertexPointer;

  // Done:
  export
    glArrayElement,
    glCallList,
    glClearAccum,
    glClearColor,
    glClearDepth,
    glClearIndex,
    glClearStencil,
    glFinish,
    glFlush,
    glGenLists,
    glGetString,  // Get rid of warning?
    glIndexMask,
    glInitNames,
    glLineStipple,
    glLineWidth,
    glListBase,
    glLoadIdentity,
    glLoadName,
    glOrtho,
    glPassThrough,
    glPixelZoom,
    glPointSize,
    glPolygonOffset,
    glPushMatrix, glPopMatrix,
    glPushName, glPopName,
    glStencilMask;

  // Constants:
  export 
    $GL-CURRENT-BIT,
    $GL-POINT-BIT,
    $GL-LINE-BIT,
    $GL-POLYGON-BIT,
    $GL-POLYGON-STIPPLE-BIT,
    $GL-PIXEL-MODE-BIT,
    $GL-LIGHTING-BIT,
    $GL-FOG-BIT,
    $GL-DEPTH-BUFFER-BIT,
    $GL-ACCUM-BUFFER-BIT,
    $GL-STENCIL-BUFFER-BIT,
    $GL-VIEWPORT-BIT,
    $GL-TRANSFORM-BIT,
    $GL-ENABLE-BIT,
    $GL-COLOR-BUFFER-BIT,
    $GL-HINT-BIT,
    $GL-EVAL-BIT,
    $GL-LIST-BIT,
    $GL-TEXTURE-BIT,
    $GL-SCISSOR-BIT,
    $GL-ALL-ATTRIB-BITS,
    $GL-CLIENT-PIXEL-STORE-BIT,
    $GL-CLIENT-VERTEX-ARRAY-BIT;
    // $GL-CLIENT-ALL-ATTRIB-BITS;
  
  // GLenum stuff:
  export <GLenum>,
    $GL-FALSE,
    $GL-TRUE,
    $GL-BYTE,
    $GL-UNSIGNED-BYTE,
    $GL-SHORT,
    $GL-UNSIGNED-SHORT,
    $GL-INT,
    $GL-UNSIGNED-INT,
    $GL-FLOAT,
    $GL-DOUBLE,
    $GL-2-BYTES,
    $GL-3-BYTES,
    $GL-4-BYTES,
    $GL-LINES,
    $GL-POINTS,
    $GL-LINE-STRIP,
    $GL-LINE-LOOP,
    $GL-TRIANGLES,
    $GL-TRIANGLE-STRIP,
    $GL-TRIANGLE-FAN,
    $GL-QUADS,
    $GL-QUAD-STRIP,
    $GL-POLYGON,
    $GL-EDGE-FLAG,
    $GL-VERTEX-ARRAY,
    $GL-NORMAL-ARRAY,
    $GL-COLOR-ARRAY,
    $GL-INDEX-ARRAY,
    $GL-TEXTURE-COORD-ARRAY,
    $GL-EDGE-FLAG-ARRAY,
    $GL-VERTEX-ARRAY-SIZE,
    $GL-VERTEX-ARRAY-TYPE,
    $GL-VERTEX-ARRAY-STRIDE,
    $GL-NORMAL-ARRAY-TYPE,
    $GL-NORMAL-ARRAY-STRIDE,
    $GL-COLOR-ARRAY-SIZE,
    $GL-COLOR-ARRAY-TYPE,
    $GL-COLOR-ARRAY-STRIDE,
    $GL-INDEX-ARRAY-TYPE,
    $GL-INDEX-ARRAY-STRIDE,
    $GL-TEXTURE-COORD-ARRAY-SIZE,
    $GL-TEXTURE-COORD-ARRAY-TYPE,
    $GL-TEXTURE-COORD-ARRAY-STRIDE,
    $GL-EDGE-FLAG-ARRAY-STRIDE,
    $GL-VERTEX-ARRAY-POINTER,
    $GL-NORMAL-ARRAY-POINTER,
    $GL-COLOR-ARRAY-POINTER,
    $GL-INDEX-ARRAY-POINTER,
    $GL-TEXTURE-COORD-ARRAY-POINTER,
    $GL-EDGE-FLAG-ARRAY-POINTER,
    $GL-V2F,
    $GL-V3F,
    $GL-C4UB-V2F,
    $GL-C4UB-V3F,
    $GL-C3F-V3F,
    $GL-N3F-V3F,
    $GL-C4F-N3F-V3F,
    $GL-T2F-V3F,
    $GL-T4F-V4F,
    $GL-T2F-C4UB-V3F,
    $GL-T2F-C3F-V3F,
    $GL-T2F-N3F-V3F,
    $GL-T2F-C4F-N3F-V3F,
    $GL-T4F-C4F-N3F-V4F,
    $GL-MATRIX-MODE,
    $GL-MODELVIEW,
    $GL-PROJECTION,
    $GL-TEXTURE,
    $GL-POINT-SMOOTH,
    $GL-POINT-SIZE,
    $GL-POINT-SIZE-GRANULARITY,
    $GL-POINT-SIZE-RANGE,
    $GL-LINE-SMOOTH,
    $GL-LINE-STIPPLE,
    $GL-LINE-STIPPLE-PATTERN,
    $GL-LINE-STIPPLE-REPEAT,
    $GL-LINE-WIDTH,
    $GL-LINE-WIDTH-GRANULARITY,
    $GL-LINE-WIDTH-RANGE,
    $GL-POINT,
    $GL-LINE,
    $GL-FILL,
    $GL-CCW,
    $GL-CW,
    $GL-FRONT,
    $GL-BACK,
    $GL-CULL-FACE,
    $GL-CULL-FACE-MODE,
    $GL-POLYGON-SMOOTH,
    $GL-POLYGON-STIPPLE,
    $GL-FRONT-FACE,
    $GL-POLYGON-MODE,
    $GL-POLYGON-OFFSET-FACTOR,
    $GL-POLYGON-OFFSET-UNITS,
    $GL-POLYGON-OFFSET-POINT,
    $GL-POLYGON-OFFSET-LINE,
    $GL-POLYGON-OFFSET-FILL,
    $GL-COMPILE,
    $GL-COMPILE-AND-EXECUTE,
    $GL-LIST-BASE,
    $GL-LIST-INDEX,
    $GL-LIST-MODE,
    $GL-NEVER,
    $GL-LESS,
    $GL-GEQUAL,
    $GL-LEQUAL,
    $GL-GREATER,
    $GL-NOTEQUAL,
    $GL-EQUAL,
    $GL-ALWAYS,
    $GL-DEPTH-TEST,
    $GL-DEPTH-BITS,
    $GL-DEPTH-CLEAR-VALUE,
    $GL-DEPTH-FUNC,
    $GL-DEPTH-RANGE,
    $GL-DEPTH-WRITEMASK,
    $GL-DEPTH-COMPONENT,
    $GL-LIGHTING,
    $GL-LIGHT0,
    $GL-LIGHT1,
    $GL-LIGHT2,
    $GL-LIGHT3,
    $GL-LIGHT4,
    $GL-LIGHT5,
    $GL-LIGHT6,
    $GL-LIGHT7,
    $GL-SPOT-EXPONENT,
    $GL-SPOT-CUTOFF,
    $GL-CONSTANT-ATTENUATION,
    $GL-LINEAR-ATTENUATION,
    $GL-QUADRATIC-ATTENUATION,
    $GL-AMBIENT,
    $GL-DIFFUSE,
    $GL-SPECULAR,
    $GL-SHININESS,
    $GL-EMISSION,
    $GL-POSITION,
    $GL-SPOT-DIRECTION,
    $GL-AMBIENT-AND-DIFFUSE,
    $GL-COLOR-INDEXES,
    $GL-LIGHT-MODEL-TWO-SIDE,
    $GL-LIGHT-MODEL-LOCAL-VIEWER,
    $GL-LIGHT-MODEL-AMBIENT,
    $GL-FRONT-AND-BACK,
    $GL-SHADE-MODEL,
    $GL-FLAT,
    $GL-SMOOTH,
    $GL-COLOR-MATERIAL,
    $GL-COLOR-MATERIAL-FACE,
    $GL-COLOR-MATERIAL-PARAMETER,
    $GL-NORMALIZE,
    $GL-CLIP-PLANE0,
    $GL-CLIP-PLANE1,
    $GL-CLIP-PLANE2,
    $GL-CLIP-PLANE3,
    $GL-CLIP-PLANE4,
    $GL-CLIP-PLANE5,
    $GL-ACCUM-RED-BITS,
    $GL-ACCUM-GREEN-BITS,
    $GL-ACCUM-BLUE-BITS,
    $GL-ACCUM-ALPHA-BITS,
    $GL-ACCUM-CLEAR-VALUE,
    $GL-ACCUM,
    $GL-ADD,
    $GL-LOAD,
    $GL-MULT,
    $GL-RETURN,
    $GL-ALPHA-TEST,
    $GL-ALPHA-TEST-REF,
    $GL-ALPHA-TEST-FUNC,
    $GL-BLEND,
    $GL-BLEND-SRC,
    $GL-BLEND-DST,
    $GL-ZERO,
    $GL-ONE,
    $GL-SRC-COLOR,
    $GL-ONE-MINUS-SRC-COLOR,
    $GL-DST-COLOR,
    $GL-ONE-MINUS-DST-COLOR,
    $GL-SRC-ALPHA,
    $GL-ONE-MINUS-SRC-ALPHA,
    $GL-DST-ALPHA,
    $GL-ONE-MINUS-DST-ALPHA,
    $GL-SRC-ALPHA-SATURATE,
    $GL-CONSTANT-COLOR,
    $GL-ONE-MINUS-CONSTANT-COLOR,
    $GL-CONSTANT-ALPHA,
    $GL-ONE-MINUS-CONSTANT-ALPHA,
    $GL-FEEDBACK,
    $GL-RENDER,
    $GL-SELECT,
    $GL-2D,
    $GL-3D,
    $GL-3D-COLOR,
    $GL-3D-COLOR-TEXTURE,
    $GL-4D-COLOR-TEXTURE,
    $GL-POINT-TOKEN,
    $GL-LINE-TOKEN,
    $GL-LINE-RESET-TOKEN,
    $GL-POLYGON-TOKEN,
    $GL-BITMAP-TOKEN,
    $GL-DRAW-PIXEL-TOKEN,
    $GL-COPY-PIXEL-TOKEN,
    $GL-PASS-THROUGH-TOKEN,
    $GL-FEEDBACK-BUFFER-POINTER,
    $GL-FEEDBACK-BUFFER-SIZE,
    $GL-FEEDBACK-BUFFER-TYPE,
    $GL-FOG,
    $GL-FOG-MODE,
    $GL-FOG-DENSITY,
    $GL-FOG-COLOR,
    $GL-FOG-INDEX,
    $GL-FOG-START,
    $GL-FOG-END,
    $GL-LINEAR,
    $GL-EXP,
    $GL-EXP2,
    $GL-LOGIC-OP,
    $GL-INDEX-LOGIC-OP,
    $GL-COLOR-LOGIC-OP,
    $GL-LOGIC-OP-MODE,
    $GL-CLEAR,
    $GL-SET,
    $GL-COPY,
    $GL-COPY-INVERTED,
    $GL-NOOP,
    $GL-INVERT,
    $GL-AND,
    $GL-NAND,
    $GL-OR,
    $GL-NOR,
    $GL-XOR,
    $GL-EQUIV,
    $GL-AND-REVERSE,
    $GL-AND-INVERTED,
    $GL-OR-REVERSE,
    $GL-OR-INVERTED,
    $GL-STENCIL-TEST,
    $GL-STENCIL-WRITEMASK,
    $GL-STENCIL-BITS,
    $GL-STENCIL-FUNC,
    $GL-STENCIL-VALUE-MASK,
    $GL-STENCIL-REF,
    $GL-STENCIL-FAIL,
    $GL-STENCIL-PASS-DEPTH-PASS,
    $GL-STENCIL-PASS-DEPTH-FAIL,
    $GL-STENCIL-CLEAR-VALUE,
    $GL-STENCIL-INDEX,
    $GL-KEEP,
    $GL-REPLACE,
    $GL-INCR,
    $GL-DECR,
    $GL-NONE,
    $GL-LEFT,
    $GL-RIGHT,
    $GL-FRONT-LEFT,
    $GL-FRONT-RIGHT,
    $GL-BACK-LEFT,
    $GL-BACK-RIGHT,
    $GL-AUX0,
    $GL-AUX1,
    $GL-AUX2,
    $GL-AUX3,
    $GL-COLOR-INDEX,
    $GL-RED,
    $GL-GREEN,
    $GL-BLUE,
    $GL-ALPHA,
    $GL-LUMINANCE,
    $GL-LUMINANCE-ALPHA,
    $GL-ALPHA-BITS,
    $GL-RED-BITS,
    $GL-GREEN-BITS,
    $GL-BLUE-BITS,
    $GL-INDEX-BITS,
    $GL-SUBPIXEL-BITS,
    $GL-AUX-BUFFERS,
    $GL-READ-BUFFER,
    $GL-DRAW-BUFFER,
    $GL-DOUBLEBUFFER,
    $GL-STEREO,
    $GL-BITMAP,
    $GL-COLOR,
    $GL-DEPTH,
    $GL-STENCIL,
    $GL-DITHER,
    $GL-RGB,
    $GL-RGBA,
    $GL-MAX-LIST-NESTING,
    $GL-MAX-ATTRIB-STACK-DEPTH,
    $GL-MAX-MODELVIEW-STACK-DEPTH,
    $GL-MAX-NAME-STACK-DEPTH,
    $GL-MAX-PROJECTION-STACK-DEPTH,
    $GL-MAX-TEXTURE-STACK-DEPTH,
    $GL-MAX-EVAL-ORDER,
    $GL-MAX-LIGHTS,
    $GL-MAX-CLIP-PLANES,
    $GL-MAX-TEXTURE-SIZE,
    $GL-MAX-PIXEL-MAP-TABLE,
    $GL-MAX-VIEWPORT-DIMS,
    $GL-MAX-CLIENT-ATTRIB-STACK-DEPTH,
    $GL-ATTRIB-STACK-DEPTH,
    $GL-CLIENT-ATTRIB-STACK-DEPTH,
    $GL-COLOR-CLEAR-VALUE,
    $GL-COLOR-WRITEMASK,
    $GL-CURRENT-INDEX,
    $GL-CURRENT-COLOR,
    $GL-CURRENT-NORMAL,
    $GL-CURRENT-RASTER-COLOR,
    $GL-CURRENT-RASTER-DISTANCE,
    $GL-CURRENT-RASTER-INDEX,
    $GL-CURRENT-RASTER-POSITION,
    $GL-CURRENT-RASTER-TEXTURE-COORDS,
    $GL-CURRENT-RASTER-POSITION-VALID,
    $GL-CURRENT-TEXTURE-COORDS,
    $GL-INDEX-CLEAR-VALUE,
    $GL-INDEX-MODE,
    $GL-INDEX-WRITEMASK,
    $GL-MODELVIEW-MATRIX,
    $GL-MODELVIEW-STACK-DEPTH,
    $GL-NAME-STACK-DEPTH,
    $GL-PROJECTION-MATRIX,
    $GL-PROJECTION-STACK-DEPTH,
    $GL-RENDER-MODE,
    $GL-RGBA-MODE,
    $GL-TEXTURE-MATRIX,
    $GL-TEXTURE-STACK-DEPTH,
    $GL-VIEWPORT,
    $GL-AUTO-NORMAL,
    $GL-MAP1-COLOR-4,
    $GL-MAP1-GRID-DOMAIN,
    $GL-MAP1-GRID-SEGMENTS,
    $GL-MAP1-INDEX,
    $GL-MAP1-NORMAL,
    $GL-MAP1-TEXTURE-COORD-1,
    $GL-MAP1-TEXTURE-COORD-2,
    $GL-MAP1-TEXTURE-COORD-3,
    $GL-MAP1-TEXTURE-COORD-4,
    $GL-MAP1-VERTEX-3,
    $GL-MAP1-VERTEX-4,
    $GL-MAP2-COLOR-4,
    $GL-MAP2-GRID-DOMAIN,
    $GL-MAP2-GRID-SEGMENTS,
    $GL-MAP2-INDEX,
    $GL-MAP2-NORMAL,
    $GL-MAP2-TEXTURE-COORD-1,
    $GL-MAP2-TEXTURE-COORD-2,
    $GL-MAP2-TEXTURE-COORD-3,
    $GL-MAP2-TEXTURE-COORD-4,
    $GL-MAP2-VERTEX-3,
    $GL-MAP2-VERTEX-4,
    $GL-COEFF,
    $GL-DOMAIN,
    $GL-ORDER,
    $GL-FOG-HINT,
    $GL-LINE-SMOOTH-HINT,
    $GL-PERSPECTIVE-CORRECTION-HINT,
    $GL-POINT-SMOOTH-HINT,
    $GL-POLYGON-SMOOTH-HINT,
    $GL-DONT-CARE,
    $GL-FASTEST,
    $GL-NICEST,
    $GL-SCISSOR-TEST,
    $GL-SCISSOR-BOX,
    $GL-MAP-COLOR,
    $GL-MAP-STENCIL,
    $GL-INDEX-SHIFT,
    $GL-INDEX-OFFSET,
    $GL-RED-SCALE,
    $GL-RED-BIAS,
    $GL-GREEN-SCALE,
    $GL-GREEN-BIAS,
    $GL-BLUE-SCALE,
    $GL-BLUE-BIAS,
    $GL-ALPHA-SCALE,
    $GL-ALPHA-BIAS,
    $GL-DEPTH-SCALE,
    $GL-DEPTH-BIAS,
    $GL-PIXEL-MAP-S-TO-S-SIZE,
    $GL-PIXEL-MAP-I-TO-I-SIZE,
    $GL-PIXEL-MAP-I-TO-R-SIZE,
    $GL-PIXEL-MAP-I-TO-G-SIZE,
    $GL-PIXEL-MAP-I-TO-B-SIZE,
    $GL-PIXEL-MAP-I-TO-A-SIZE,
    $GL-PIXEL-MAP-R-TO-R-SIZE,
    $GL-PIXEL-MAP-G-TO-G-SIZE,
    $GL-PIXEL-MAP-B-TO-B-SIZE,
    $GL-PIXEL-MAP-A-TO-A-SIZE,
    $GL-PIXEL-MAP-S-TO-S,
    $GL-PIXEL-MAP-I-TO-I,
    $GL-PIXEL-MAP-I-TO-R,
    $GL-PIXEL-MAP-I-TO-G,
    $GL-PIXEL-MAP-I-TO-B,
    $GL-PIXEL-MAP-I-TO-A,
    $GL-PIXEL-MAP-R-TO-R,
    $GL-PIXEL-MAP-G-TO-G,
    $GL-PIXEL-MAP-B-TO-B,
    $GL-PIXEL-MAP-A-TO-A,
    $GL-PACK-ALIGNMENT,
    $GL-PACK-LSB-FIRST,
    $GL-PACK-ROW-LENGTH,
    $GL-PACK-SKIP-PIXELS,
    $GL-PACK-SKIP-ROWS,
    $GL-PACK-SWAP-BYTES,
    $GL-UNPACK-ALIGNMENT,
    $GL-UNPACK-LSB-FIRST,
    $GL-UNPACK-ROW-LENGTH,
    $GL-UNPACK-SKIP-PIXELS,
    $GL-UNPACK-SKIP-ROWS,
    $GL-UNPACK-SWAP-BYTES,
    $GL-ZOOM-X,
    $GL-ZOOM-Y,
    $GL-TEXTURE-ENV,
    $GL-TEXTURE-ENV-MODE,
    $GL-TEXTURE-1D,
    $GL-TEXTURE-2D,
    $GL-TEXTURE-WRAP-S,
    $GL-TEXTURE-WRAP-T,
    $GL-TEXTURE-MAG-FILTER,
    $GL-TEXTURE-MIN-FILTER,
    $GL-TEXTURE-ENV-COLOR,
    $GL-TEXTURE-GEN-S,
    $GL-TEXTURE-GEN-T,
    $GL-TEXTURE-GEN-MODE,
    $GL-TEXTURE-BORDER-COLOR,
    $GL-TEXTURE-WIDTH,
    $GL-TEXTURE-HEIGHT,
    $GL-TEXTURE-BORDER,
    $GL-TEXTURE-COMPONENTS,
    $GL-TEXTURE-RED-SIZE,
    $GL-TEXTURE-GREEN-SIZE,
    $GL-TEXTURE-BLUE-SIZE,
    $GL-TEXTURE-ALPHA-SIZE,
    $GL-TEXTURE-LUMINANCE-SIZE,
    $GL-TEXTURE-INTENSITY-SIZE,
    $GL-NEAREST-MIPMAP-NEAREST,
    $GL-NEAREST-MIPMAP-LINEAR,
    $GL-LINEAR-MIPMAP-NEAREST,
    $GL-LINEAR-MIPMAP-LINEAR,
    $GL-OBJECT-LINEAR,
    $GL-OBJECT-PLANE,
    $GL-EYE-LINEAR,
    $GL-EYE-PLANE,
    $GL-SPHERE-MAP,
    $GL-DECAL,
    $GL-MODULATE,
    $GL-NEAREST,
    $GL-REPEAT,
    $GL-CLAMP,
    $GL-S,
    $GL-T,
    $GL-R,
    $GL-Q,
    $GL-TEXTURE-GEN-R,
    $GL-TEXTURE-GEN-Q,
    $GL-PROXY-TEXTURE-1D,
    $GL-PROXY-TEXTURE-2D,
    $GL-TEXTURE-PRIORITY,
    $GL-TEXTURE-RESIDENT,
    $GL-TEXTURE-BINDING-1D,
    $GL-TEXTURE-BINDING-2D,
    $GL-PACK-SKIP-IMAGES,
    $GL-PACK-IMAGE-HEIGHT,
    $GL-UNPACK-SKIP-IMAGES,
    $GL-UNPACK-IMAGE-HEIGHT,
    $GL-TEXTURE-3D,
    $GL-PROXY-TEXTURE-3D,
    $GL-TEXTURE-DEPTH,
    $GL-TEXTURE-WRAP-R,
    $GL-MAX-3D-TEXTURE-SIZE,
    $GL-TEXTURE-BINDING-3D,
    $GL-ALPHA4,
    $GL-ALPHA8,
    $GL-ALPHA12,
    $GL-ALPHA16,
    $GL-LUMINANCE4,
    $GL-LUMINANCE8,
    $GL-LUMINANCE12,
    $GL-LUMINANCE16,
    $GL-LUMINANCE4-ALPHA4,
    $GL-LUMINANCE6-ALPHA2,
    $GL-LUMINANCE8-ALPHA8,
    $GL-LUMINANCE12-ALPHA4,
    $GL-LUMINANCE12-ALPHA12,
    $GL-LUMINANCE16-ALPHA16,
    $GL-INTENSITY,
    $GL-INTENSITY4,
    $GL-INTENSITY8,
    $GL-INTENSITY12,
    $GL-INTENSITY16,
    $GL-R3-G3-B2,
    $GL-RGB4,
    $GL-RGB5,
    $GL-RGB8,
    $GL-RGB10,
    $GL-RGB12,
    $GL-RGB16,
    $GL-RGBA2,
    $GL-RGBA4,
    $GL-RGB5-A1,
    $GL-RGBA8,
    $GL-RGB10-A2,
    $GL-RGBA12,
    $GL-RGBA16,
    $GL-VENDOR,
    $GL-RENDERER,
    $GL-VERSION,
    $GL-EXTENSIONS,
    $GL-INVALID-VALUE,
    $GL-INVALID-ENUM,
    $GL-INVALID-OPERATION,
    $GL-STACK-OVERFLOW,
    $GL-STACK-UNDERFLOW,
    $GL-OUT-OF-MEMORY,
    $GL-RESCALE-NORMAL,
    $GL-CLAMP-TO-EDGE,
    $GL-MAX-ELEMENTS-VERTICES,
    $GL-MAX-ELEMENTS-INDICES,
    $GL-BGR,
    $GL-BGRA,
    $GL-UNSIGNED-BYTE-3-3-2,
    $GL-UNSIGNED-BYTE-2-3-3-REV,
    $GL-UNSIGNED-SHORT-5-6-5,
    $GL-UNSIGNED-SHORT-5-6-5-REV,
    $GL-UNSIGNED-SHORT-4-4-4-4,
    $GL-UNSIGNED-SHORT-4-4-4-4-REV,
    $GL-UNSIGNED-SHORT-5-5-5-1,
    $GL-UNSIGNED-SHORT-1-5-5-5-REV,
    $GL-UNSIGNED-INT-8-8-8-8,
    $GL-UNSIGNED-INT-8-8-8-8-REV,
    $GL-UNSIGNED-INT-10-10-10-2,
    $GL-UNSIGNED-INT-2-10-10-10-REV,
    $GL-LIGHT-MODEL-COLOR-CONTROL,
    $GL-SINGLE-COLOR,
    $GL-SEPARATE-SPECULAR-COLOR,
    $GL-TEXTURE-MIN-LOD,
    $GL-TEXTURE-MAX-LOD,
    $GL-TEXTURE-BASE-LEVEL,
    $GL-TEXTURE-MAX-LEVEL;

end module;

define module opengl-glu
  use dylan;
  use extensions;
  use melange-support;
  use opengl;

  // Functions:
  export 
    gluLookAt,
    gluOrtho2D,
    gluPerspective,
    gluPickMatrix,
    gluProject,
    gluUnProject,
    gluErrorString,
    gluScaleImage,
    gluBuild1DMipmaps,
    gluBuild2DMipmaps,
    gluNewQuadric,
    gluDeleteQuadric,
    gluQuadricDrawStyle,
    gluQuadricOrientation,
    gluQuadricNormals,
    gluQuadricTexture,
    gluCylinder,
    gluSphere,
    gluDisk,
    gluPartialDisk,
    gluNewNurbsRenderer,
    gluDeleteNurbsRenderer,
    gluLoadSamplingMatrices,
    gluNurbsProperty,
    gluGetNurbsProperty,
    gluBeginCurve,
    gluEndCurve,
    gluNurbsCurve,
    gluBeginSurface,
    gluEndSurface,
    gluNurbsSurface,
    gluBeginTrim,
    gluEndTrim,
    gluPwlCurve,
    gluNewTess,
    gluDeleteTess,
    gluBeginPolygon,
    gluEndPolygon,
    gluNextContour,
    gluTessVertex,
    gluGetString;

  // Classes:
  export
    <GLUquadricObj>,
    <GLUtriangulatorObj>,
    <GLUnurbsObj>;
    
  // Constants:
  export
    $GLU-VERSION-1-1,
    $GLU-TRUE,
    $GLU-FALSE;

  // GLUenum:
  export
    $GLU-SMOOTH,
    $GLU-FLAT,
    $GLU-NONE,
    $GLU-POINT,
    $GLU-LINE,
    $GLU-FILL,
    $GLU-SILHOUETTE,
    $GLU-OUTSIDE,
    $GLU-INSIDE,
    $GLU-BEGIN,
    $GLU-VERTEX,
    $GLU-END,
    $GLU-ERROR,
    $GLU-EDGE-FLAG,
    $GLU-CW,
    $GLU-CCW,
    $GLU-INTERIOR,
    $GLU-EXTERIOR,
    $GLU-UNKNOWN,
    $GLU-TESS-ERROR1,
    $GLU-TESS-ERROR2,
    $GLU-TESS-ERROR3,
    $GLU-TESS-ERROR4,
    $GLU-TESS-ERROR5,
    $GLU-TESS-ERROR6,
    $GLU-TESS-ERROR7,
    $GLU-TESS-ERROR8,
    // $GLU-TESS-ERROR9,
    $GLU-AUTO-LOAD-MATRIX,
    $GLU-CULLING,
    $GLU-PARAMETRIC-TOLERANCE,
    $GLU-SAMPLING-TOLERANCE,
    $GLU-DISPLAY-MODE,
    $GLU-SAMPLING-METHOD,
    $GLU-U-STEP,
    $GLU-V-STEP,
    $GLU-PATH-LENGTH,
    $GLU-PARAMETRIC-ERROR,
    $GLU-DOMAIN-DISTANCE,
    $GLU-MAP1-TRIM-2,
    $GLU-MAP1-TRIM-3,
    $GLU-OUTLINE-POLYGON,
    $GLU-OUTLINE-PATCH,
    $GLU-NURBS-ERROR1,
    $GLU-NURBS-ERROR2,
    $GLU-NURBS-ERROR3,
    $GLU-NURBS-ERROR4,
    $GLU-NURBS-ERROR5,
    $GLU-NURBS-ERROR6,
    $GLU-NURBS-ERROR7,
    $GLU-NURBS-ERROR8,
    $GLU-NURBS-ERROR9,
    $GLU-NURBS-ERROR10,
    $GLU-NURBS-ERROR11,
    $GLU-NURBS-ERROR12,
    $GLU-NURBS-ERROR13,
    $GLU-NURBS-ERROR14,
    $GLU-NURBS-ERROR15,
    $GLU-NURBS-ERROR16,
    $GLU-NURBS-ERROR17,
    $GLU-NURBS-ERROR18,
    $GLU-NURBS-ERROR19,
    $GLU-NURBS-ERROR20,
    $GLU-NURBS-ERROR21,
    $GLU-NURBS-ERROR22,
    $GLU-NURBS-ERROR23,
    $GLU-NURBS-ERROR24,
    $GLU-NURBS-ERROR25,
    $GLU-NURBS-ERROR26,
    $GLU-NURBS-ERROR27,
    $GLU-NURBS-ERROR28,
    $GLU-NURBS-ERROR29,
    $GLU-NURBS-ERROR30,
    $GLU-NURBS-ERROR31,
    $GLU-NURBS-ERROR32,
    $GLU-NURBS-ERROR33,
    $GLU-NURBS-ERROR34,
    $GLU-NURBS-ERROR35,
    $GLU-NURBS-ERROR36,
    $GLU-NURBS-ERROR37,
    $GLU-INVALID-ENUM,
    $GLU-INVALID-VALUE,
    $GLU-OUT-OF-MEMORY,
    $GLU-INCOMPATIBLE-GL-VERSION,
    $GLU-VERSION,
    $GLU-EXTENSIONS;
end module;

define module opengl-glut
  use dylan;
  use extensions;
  use melange-support;
  use opengl;

  // Functions:
  export
  	glut-init,	// Utility wrapper version
    glutInit,
    glutInitDisplayMode,
    glutInitDisplayString,
    glutInitWindowPosition,
    glutInitWindowSize,
    glutMainLoop,
    glutCreateWindow,
    glutCreateSubWindow,
    glutDestroyWindow,
    glutPostRedisplay,
    glutSwapBuffers,
    glutGetWindow,
    glutSetWindow,
    glutSetWindowTitle,
    glutSetIconTitle,
    glutPositionWindow,
    glutReshapeWindow,
    glutPopWindow,
    glutPushWindow,
    glutIconifyWindow,
    glutShowWindow,
    glutHideWindow,
    glutFullScreen,
    glutSetCursor,
    glutWarpPointer,
    glutEstablishOverlay,
    glutRemoveOverlay,
    glutUseLayer,
    glutPostOverlayRedisplay,
    glutShowOverlay,
    glutHideOverlay,
    glutCreateMenu,
    glutDestroyMenu,
    glutGetMenu,
    glutSetMenu,
    glutAddMenuEntry,
    glutAddSubMenu,
    glutChangeToMenuEntry,
    glutChangeToSubMenu,
    glutRemoveMenuItem,
    glutAttachMenu,
    glutDetachMenu,

    glutDisplayFunc,
    glutTimerFunc,
    glutMouseFunc,
    glutReshapeFunc,
    glutKeyboardFunc,
    glutMouseFunc,
    glutMotionFunc,
    glutPassiveMotionFunc,
    glutEntryFunc,
    glutVisibilityFunc,
    glutIdleFunc,
    glutMenuStateFunc,

    glutSpecialFunc,
    glutSpaceballMotionFunc,
    glutSpaceballRotateFunc,
    glutSpaceballButtonFunc,
    glutButtonBoxFunc,
    glutDialsFunc,
    glutTabletMotionFunc,
    glutTabletButtonFunc,

    glutMenuSTatusFunc,
    glutOverlayDisplayFunc,
    glutWIndowStatusFunc,
    glutKeyboardUpFunc,
    glutSpecialUpFunc,
    glutJoystickFunc,

    glutIgnoreKeyRepeat,
    glutSetColor,
    glutGetColor,
    glutCopyColormap,
    glutGet,
    glutDeviceGet,
    glutExtensionSupported,
    glutGetModifiers,
    glutLayerGet,
    glutBitmapCharacter,
    glutBitmapWidth,
    glutStrokeCharacter,
    glutStrokeWidth,
    glutBitmapLength,
    glutStrokeLength,
    glutWireSphere,
    glutSolidSphere,
    glutWireCone,
    glutSolidCone,
    glutWireCube,
    glutSolidCube,
    glutWireTorus,
    glutSolidTorus,
    glutWireDodecahedron,
    glutSolidDodecahedron,
    glutWireTeapot,
    glutSolidTeapot,
    glutWireOctahedron,
    glutSolidOctahedron,
    glutWireTetrahedron,
    glutSolidTetrahedron,
    glutWireIcosahedron,
    glutSolidIcosahedron,
    glutVideoResizeGet,
    glutSetupVideoResizing,
    glutStopVideoResizing,
    glutVideoResize,
    glutVideoPan,
    glutReportErrors;

  export
    $GLUT-BITMAP-8-BY-13,
    $GLUT-BITMAP-9-BY-15,
    $GLUT-BITMAP-8-BY-13,
    $GLUT-BITMAP-TIMES-ROMAN-10,
    $GLUT-BITMAP-TIMES-ROMAN-24,
    $GLUT-BITMAP-HELVETICA-10,
    $GLUT-BITMAP-HELVETICA-12,
    $GLUT-BITMAP-HELVETICA-18;


  // GLUT constants:
  export
    $GLUT-API-VERSION,
    $GLUT-XLIB-IMPLEMENTATION,
    $GLUT-RGB,
    $GLUT-RGBA,
    $GLUT-INDEX,
    $GLUT-SINGLE,
    $GLUT-DOUBLE,
    $GLUT-ACCUM,
    $GLUT-ALPHA,
    $GLUT-DEPTH,
    $GLUT-STENCIL,
    $GLUT-MULTISAMPLE,
    $GLUT-STEREO,
    $GLUT-LUMINANCE,
    $GLUT-LEFT-BUTTON,
    $GLUT-MIDDLE-BUTTON,
    $GLUT-RIGHT-BUTTON,
    $GLUT-DOWN,
    $GLUT-UP,
    $GLUT-KEY-F1,
    $GLUT-KEY-F2,
    $GLUT-KEY-F3,
    $GLUT-KEY-F4,
    $GLUT-KEY-F5,
    $GLUT-KEY-F6,
    $GLUT-KEY-F7,
    $GLUT-KEY-F8,
    $GLUT-KEY-F9,
    $GLUT-KEY-F10,
    $GLUT-KEY-F11,
    $GLUT-KEY-F12,
    $GLUT-KEY-LEFT,
    $GLUT-KEY-UP,
    $GLUT-KEY-RIGHT,
    $GLUT-KEY-DOWN,
    $GLUT-KEY-PAGE-UP,
    $GLUT-KEY-PAGE-DOWN,
    $GLUT-KEY-HOME,
    $GLUT-KEY-END,
    $GLUT-KEY-INSERT,
    $GLUT-LEFT,
    $GLUT-ENTERED,
    $GLUT-MENU-NOT-IN-USE,
    $GLUT-MENU-IN-USE,
    $GLUT-NOT-VISIBLE,
    $GLUT-VISIBLE,
    $GLUT-HIDDEN,
    $GLUT-FULLY-RETAINED,
    $GLUT-PARTIALLY-RETAINED,
    $GLUT-FULLY-COVERED,
    $GLUT-RED,
    $GLUT-GREEN,
    $GLUT-BLUE,
    $GLUT-WINDOW-X,
    $GLUT-WINDOW-Y,
    $GLUT-WINDOW-WIDTH,
    $GLUT-WINDOW-HEIGHT,
    $GLUT-WINDOW-BUFFER-SIZE,
    $GLUT-WINDOW-STENCIL-SIZE,
    $GLUT-WINDOW-DEPTH-SIZE,
    $GLUT-WINDOW-RED-SIZE,
    $GLUT-WINDOW-GREEN-SIZE,
    $GLUT-WINDOW-BLUE-SIZE,
    $GLUT-WINDOW-ALPHA-SIZE,
    $GLUT-WINDOW-ACCUM-RED-SIZE,
    $GLUT-WINDOW-ACCUM-GREEN-SIZE,
    $GLUT-WINDOW-ACCUM-BLUE-SIZE,
    $GLUT-WINDOW-ACCUM-ALPHA-SIZE,
    $GLUT-WINDOW-DOUBLEBUFFER,
    $GLUT-WINDOW-RGBA,
    $GLUT-WINDOW-PARENT,
    $GLUT-WINDOW-NUM-CHILDREN,
    $GLUT-WINDOW-COLORMAP-SIZE,
    $GLUT-WINDOW-NUM-SAMPLES,
    $GLUT-WINDOW-STEREO,
    $GLUT-WINDOW-CURSOR,
    $GLUT-SCREEN-WIDTH,
    $GLUT-SCREEN-HEIGHT,
    $GLUT-SCREEN-WIDTH-MM,
    $GLUT-SCREEN-HEIGHT-MM,
    $GLUT-MENU-NUM-ITEMS,
    $GLUT-DISPLAY-MODE-POSSIBLE,
    $GLUT-INIT-WINDOW-X,
    $GLUT-INIT-WINDOW-Y,
    $GLUT-INIT-WINDOW-WIDTH,
    $GLUT-INIT-WINDOW-HEIGHT,
    $GLUT-INIT-DISPLAY-MODE,
    $GLUT-ELAPSED-TIME,
    $GLUT-HAS-KEYBOARD,
    $GLUT-HAS-MOUSE,
    $GLUT-HAS-SPACEBALL,
    $GLUT-HAS-DIAL-AND-BUTTON-BOX,
    $GLUT-HAS-TABLET,
    $GLUT-NUM-MOUSE-BUTTONS,
    $GLUT-NUM-SPACEBALL-BUTTONS,
    $GLUT-NUM-BUTTON-BOX-BUTTONS,
    $GLUT-NUM-DIALS,
    $GLUT-NUM-TABLET-BUTTONS,
    $GLUT-OVERLAY-POSSIBLE,
    $GLUT-LAYER-IN-USE,
    $GLUT-HAS-OVERLAY,
    $GLUT-TRANSPARENT-INDEX,
    $GLUT-NORMAL-DAMAGED,
    $GLUT-OVERLAY-DAMAGED,
    $GLUT-VIDEO-RESIZE-POSSIBLE,
    $GLUT-VIDEO-RESIZE-IN-USE,
    $GLUT-VIDEO-RESIZE-X-DELTA,
    $GLUT-VIDEO-RESIZE-Y-DELTA,
    $GLUT-VIDEO-RESIZE-WIDTH-DELTA,
    $GLUT-VIDEO-RESIZE-HEIGHT-DELTA,
    $GLUT-VIDEO-RESIZE-X,
    $GLUT-VIDEO-RESIZE-Y,
    $GLUT-VIDEO-RESIZE-WIDTH,
    $GLUT-VIDEO-RESIZE-HEIGHT,
    $GLUT-NORMAL,
    $GLUT-OVERLAY,
    $GLUT-ACTIVE-SHIFT,
    $GLUT-ACTIVE-CTRL,
    $GLUT-ACTIVE-ALT,
    $GLUT-CURSOR-RIGHT-ARROW,
    $GLUT-CURSOR-LEFT-ARROW,
    $GLUT-CURSOR-INFO,
    $GLUT-CURSOR-DESTROY,
    $GLUT-CURSOR-HELP,
    $GLUT-CURSOR-CYCLE,
    $GLUT-CURSOR-SPRAY,
    $GLUT-CURSOR-WAIT,
    $GLUT-CURSOR-TEXT,
    $GLUT-CURSOR-CROSSHAIR,
    $GLUT-CURSOR-UP-DOWN,
    $GLUT-CURSOR-LEFT-RIGHT,
    $GLUT-CURSOR-TOP-SIDE,
    $GLUT-CURSOR-BOTTOM-SIDE,
    $GLUT-CURSOR-LEFT-SIDE,
    $GLUT-CURSOR-RIGHT-SIDE,
    $GLUT-CURSOR-TOP-LEFT-CORNER,
    $GLUT-CURSOR-TOP-RIGHT-CORNER,
    $GLUT-CURSOR-BOTTOM-RIGHT-CORNER,
    $GLUT-CURSOR-BOTTOM-LEFT-CORNER,
    $GLUT-CURSOR-INHERIT,
    $GLUT-CURSOR-NONE,
    $GLUT-CURSOR-FULL-CROSSHAIR;

end module;

define module opengl-osmesa
  use dylan;
  use extensions;
  use melange-support;

  export
    OSMesaCreateContext,
    OSMesaDestroyContext,
    OSMesaMakeCurrent,
    OSMesaGetCurrentContext,
    OSMesaPixelStore,
    OSMesaGetIntegerv,
    OSMesaGetDepthBuffer,
    OSMesaGetColorBuffer,
    
    $OSMESA-MAJOR-VERSION,
    $OSMESA-MINOR-VERSION,
    $OSMESA-COLOR-INDEX,
    $OSMESA-RGBA,
    $OSMESA-BGRA,
    $OSMESA-ARGB,
    $OSMESA-RGB,
    $OSMESA-BGR,
    $OSMESA-ROW-LENGTH,
    $OSMESA-Y-UP,
    $OSMESA-WIDTH,
    $OSMESA-HEIGHT,
    $OSMESA-FORMAT,
    $OSMESA-TYPE;

end module opengl-osmesa;
