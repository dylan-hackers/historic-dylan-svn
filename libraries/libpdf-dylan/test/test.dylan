module: test
synopsis: Test for the pdf library.
author: Rob Myers.
copyright: LGPL, Standard Gwydion Dylan Maintainers.

// Page and resource constants

define constant $page-height :: <pdf-real> = 500.0;
define constant $page-width :: <pdf-real> = $page-height;
define constant $font-size :: <pdf-real> = 24.0;

// Shape test constants

define constant $shape-size :: <pdf-real> = 20.0;
define constant $shape-offset :: <pdf-real> = 50.0;

// main

define function main(name, arguments)
    let document :: <pdf-document> = make( <pdf-document> );
    
    // Page 1
    
    let page :: <pdf-dictionary> = value( make-pdf-page( document, bounds: make-pdf-rectangle( top: $page-height, left: 0.0, right: $page-width, bottom: 0.0 ) ) );
    let font = make-pdf-font( document, "F1" );
    add-pdf-page-font( page, font );
     
    let stream = pdf-page-contents-stream( page );
    text-begin( stream );
        text-font( stream, as( <pdf-name>, "F1" ), $font-size );
        text-leading( stream, $font-size );
        text-position( stream, 40.0, ($page-height / 2.0) + $font-size );
        text-show( stream, "Gwydion Dylan PDF Library V0.7" );
        text-show-next-line( stream, "Test Program Output" );
    text-end( stream );
  
    // Page 2
    
    // Text state
    
    let page2 :: <pdf-dictionary> = value( make-pdf-page( document, bounds: make-pdf-rectangle( top: $page-height, left: 0.0, right: $page-width, bottom: 0.0 ) ) );
    add-pdf-page-font( page2, font );

    let stream2 = pdf-page-contents-stream( page2 );

    text-begin( stream2 );
        // Setup
        text-font( stream2, as( <pdf-name>, "F1" ), $font-size );
        text-leading( stream2, $font-size );
        text-position( stream2, 40.0, $page-height - $font-size );
        // Normal
        text-show( stream2, "text-font" );
        // Rendering mode
        text-rendering-mode( stream2, 1 );
        text-show-next-line( stream2, "text-rendering-mode" );
        text-rendering-mode( stream2, 0 );
        // Character spacing
        text-character-spacing( stream2, 10.0 );
        text-show-next-line( stream2, "text-character-spacing" );
        text-character-spacing( stream2, 0.0 );
        // Word spacing
        text-word-spacing( stream2, 20.0 );
        text-show-next-line( stream2, "text word spacing" );
        text-word-spacing( stream2, 0.0 );
        // Horizontal scale
        text-scale( stream2, 50.0 );
        text-show-next-line( stream2, "text-scale" );
        text-scale( stream2, 100.0 );
        // Rise
        text-rise( stream2, 10.0 );
        text-show-next-line( stream2, "text-rise" );
        text-rise( stream2, 0.0 );
    text-end( stream2 );
  
    // Page 3
    
    // Matrices and colours
    
    let page3 :: <pdf-dictionary> = value( make-pdf-page( document, bounds: make-pdf-rectangle( top: $page-height, left: 0.0, right: $page-width, bottom: 0.0 ) ) );

    let stream3 = pdf-page-contents-stream( page3 );
    
    // Normal
    push-state( stream3 );
        circle-path( stream3, 100.0, $shape-offset, $shape-size );
        fill-path( stream3 );
    pop-state( stream3 );
    // Scale
    push-state( stream3 );
        scale-matrix( stream3, 0.25, 2.2 );
        rgb-fill( stream3, 1.0, 0.0, 0.0 );
        rectangle-path( stream3, x: 100.0, y: $shape-offset * 2.0, width: $shape-size, height: $shape-size );
        fill-path( stream3 );
    pop-state( stream3 );
    // Rotate
    push-state( stream3 );
        rotate-matrix( stream3, 20.0 );
        cmyk-fill( stream3, 1.0, 0.0, 0.0, 0.0 );
        move-to( stream3, 100.0, $shape-offset * 3.0 );
            append-line( stream3, 110.0, $shape-offset * 3.0 );
            append-line( stream3, 105.0, ($shape-offset * 3.0) + 25 );
        close-subpath( stream3 );
        fill-path( stream3 );
    pop-state( stream3 );
    // Skew
    push-state( stream3 );
        skew-matrix( stream3, 10.0, 3.0 );
        gray-fill( stream3, 0.1 );
        circle-path( stream3, 100.0, $shape-offset * 4.0, $shape-size );
        fill-path( stream3 );
    pop-state( stream3 );
    
    // Page 4
    
    // Paths
    
    let page4 :: <pdf-dictionary> = value( make-pdf-page( document, bounds: make-pdf-rectangle( top: $page-height, left: 0.0, right: $page-width, bottom: 0.0 ) ) );

    let stream4 = pdf-page-contents-stream( page4 );
    
    push-state( stream4 );
        rgb-fill( stream4, 1.0, 0.0, 0.0 );
        draw-silly-path( stream4 );
        fill-path( stream4 );
        
        translate-matrix( stream4, 0.0, 100.0 );
        rgb-fill( stream4, 0.0, 1.0, 0.0 );
        draw-silly-path( stream4 );
        fill-path-even-odd-winding( stream4 );
        
        translate-matrix( stream4, 0.0, 100.0 );
        cmyk-stroke( stream4, 1.0, 0.1, 0.3, 4.0 );
        line-width( stream4, 5.0 );
        dash-pattern( stream4, as( <pdf-array>, #[1, 2] ), 2 );        
        draw-silly-path( stream4 );
        stroke-path( stream4 );
        
        translate-matrix( stream4, 0.0, 100.0 );
        rgb-stroke( stream4, 0.0, 0.0, 1.0 );
        line-cap( stream4, 1 );
        line-join( stream4, 1 );
        miter-limit( stream4, 60.0 );
        draw-silly-path( stream4 );
        stroke-path( stream4 );
    
    pop-state( stream4 );
    
    // Write to file
    
    let file-stream = make( <file-stream>, locator: "test.pdf", direction: #"output" );
    write-pdf-document( file-stream, document );
    close( file-stream );
    
    exit-application(0);
end function main;

define method draw-silly-path( stream )
    move-to( stream, 100.0, 100.0 );
    append-line( stream, 150.0, 150.0 );
    append-line( stream, 50.0, 70.0 );
    append-bezier-path( stream, 90.0, 90.0, 120.0, 40.0, 300.0, 120.0 );
    append-bezier-path2( stream, 170.0, 30.0, 30.0, 120.0 );
    append-bezier-path3( stream, 200.0, 10.0, 100.0, 100.0 );
    close-subpath( stream );
end method draw-silly-path;


// Invoke our main() function.
main(application-name(), application-arguments());
