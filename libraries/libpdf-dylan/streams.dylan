module: pdf
synopsis: PDF stream class and methods.
author: Rob Myers. dylan@robmyers.org
copyright: Standard Gwydion Dylan.


// <pdf-stream>
// WARNING: Make sure that these are made with direction: #"output" !

define constant <pdf-stream> = <string-stream>;

define method write-pdf-object( stream :: <stream>, pdf-stream :: <pdf-stream>, depth :: <integer> )
=> ()
    format( stream, "<< /Length %d >>\n", size( stream-contents( pdf-stream, clear-contents?: #f ) )  );
    write-line( stream, "stream" );
    format( stream, "%s", stream-contents( pdf-stream, clear-contents?: #f ) );
    write-line( stream, "endstream" );
    
    values();
end method write-pdf-object;