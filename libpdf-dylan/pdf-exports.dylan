module: dylan-user

define library pdf
  use common-dylan;
  use format;
  use io;
  use streams;
  use transcendental;

  export pdf;
end library pdf;

define module pdf
  use common-dylan;
  use format;
  use format-out;
  use streams;
  use transcendental;

  export 
    $pdf-header,
    
    $pdf-null,
    
    $Courier,
    $Helvetica, 
    $Times-Roman, 
    $Symbol,
    
    $Courier-Bold, 
    $Helvetica-Bold, 
    $Times-Bold, 
    
    $Courier-Oblique, 
    $Helvetica-Oblique, 
    $Times-Italic,
    
    $Courier-BoldOblique, 
    $Helvetica-BoldOblique, 
    $Times-BoldItalic,
    $ZapfDingbats,
    
    <pdf-object>,
    <pdf-boolean>,
    <pdf-integer>,
    <pdf-real>,
    <pdf-string>,
    <pdf-name>,
    <pdf-null>,
    <pdf-indirect-object>,
    value,
    
    write-pdf-object,
    
    <pdf-array>,
    make-pdf-rectangle,
    
    <pdf-dictionary>,
    make-pdf-font,
    make-pdf-catalog,
    make-pdf-outline-tree,
    make-pdf-page-tree,
    make-pdf-page,
    pdf-page-contents-stream,
    add-pdf-page-font,
    
    <pdf-document>,
    write-pdf-document,
    
    <pdf-stream>,
    
    push-state,
    pop-state,
    concatenate-matrix,
    line-cap,
    line-width,
    line-join,
    miter-limit,
    dash-pattern,
    flatness-tolerance,
    
    transform-matrix,
    translate-matrix,
    scale-matrix,
    rotate-matrix,
    skew-matrix,
  
    move-to,
    append-line,
    close-subpath,
    append-bezier-path,
    append-bezier-path2,
    append-bezier-path3,
    rectangle-path,
    circle-path,
    
    stroke-path,
    close-and-stroke-path,
    fill-path,
    fill-path-even-odd-winding,
    
    gray-fill,
    gray-stroke,
    rgb-fill,
    rgb-stroke,
    cmyk-fill,
    cmyk-stroke,
  
    text-begin,
    text-end,
    text-font,
    text-position,
    text-leading,
    text-character-spacing,
    text-word-spacing,
    text-scale,
    text-rise,
    text-rendering-mode,
    text-show,
    text-show-next-line,
    text-next-line
  ;
end module pdf;
