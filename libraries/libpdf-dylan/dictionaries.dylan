module: pdf
synopsis: PDF dictionary class and methods
author: Rob Myers. dylan@robmyers.org
copyright: Standard Gwydion Dylan.


// <pdf-dictionary>

define constant <pdf-dictionary> = <table>;


// write-pdf-object

define method write-pdf-object( stream :: <stream>, dictionary :: <pdf-dictionary>, depth :: <integer> )
=> ()
    let keys :: <sequence> = key-sequence( dictionary );
    if( dictionary.size == 1 )
        format( stream, "<< /%s ", keys[ 0 ] );
        write-pdf-object( stream, dictionary[ keys[ 0 ] ], depth + 1 );
        write( stream, " " );
    elseif( dictionary.size > 1 )
        write( stream, "<< " );
        for( key in keys )
            format( stream, "/%s ", key );
            write-pdf-object( stream, dictionary[ key ], depth + 1 );
            write( stream, "\n" );
        end for;
    end if;
    write-line( stream, ">>" );
    
    values();
end method write-pdf-object;


// make-pdf-font

define method make-pdf-font( document :: <pdf-document>, name :: <string>, #key sub-type :: <string> = "Type1",
                             base-font :: <string> ="Helvetica", encoding :: <string> = "MacRomanEncoding" )
=> ( result :: <pdf-indirect-object> )
    let font :: <pdf-dictionary> = make( <pdf-dictionary> );
    
    font[ "Type" ] := as( <pdf-name>, "Font" );
    font[ "Subtype" ] := as( <pdf-name>, sub-type );
    font[ "Name" ] := as( <pdf-name>, name );
    font[ "BaseFont" ] := as( <pdf-name>, base-font );
    font[ "Encoding" ] := as( <pdf-name>, encoding );
    
    let indirect-font = make( <pdf-indirect-object>, document: document, value: font );
    add!( document.body, indirect-font );
    
    indirect-font;
end method make-pdf-font;


// make-pdf-catalog

define method make-pdf-catalog( document :: <pdf-document> )
=> ( result :: <pdf-indirect-object> )
    let catalog :: <pdf-dictionary> = make( <pdf-dictionary> );
    
    catalog[ "Type" ] := as( <pdf-name>, "Catalog" );
    //catalog[ "Pages" ] := pages;
    //catalog[ "Outlines" ] := outlines;
    
    make( <pdf-indirect-object>, value: catalog, document: document );
end method make-pdf-catalog;


// make-pdf-outline-tree

define method make-pdf-outline-tree( document :: <pdf-document>  )
=> ( result :: <pdf-indirect-object> )
    let outline-tree :: <pdf-dictionary> = make( <pdf-dictionary> );

    outline-tree[ "Type" ] := as( <pdf-name>, "Outlines" );
    outline-tree[ "Kids" ] := make( <pdf-array> );
    outline-tree[ "Count" ] := 0;

    let indirect-outline-tree = make( <pdf-indirect-object>, value: outline-tree, document: document );
    document.catalog.value[ "Outlines" ] := indirect-outline-tree;
    
    indirect-outline-tree;
end method make-pdf-outline-tree;


// make-pdf-page-tree

define method make-pdf-page-tree( document :: <pdf-document>  )
=> ( result :: <pdf-indirect-object> )
    let page-tree :: <pdf-dictionary> = make( <pdf-dictionary> );

    page-tree[ "Type" ] := as( <pdf-name>, "Pages" );
    page-tree[ "Kids" ] := make( <pdf-array> );
    page-tree[ "Count" ] := 0;
    
    let indirect-page-tree = make( <pdf-indirect-object>, value: page-tree, document: document );
    document.catalog.value[ "Pages" ] := indirect-page-tree;

    indirect-page-tree;
end method make-pdf-page-tree;


// make-pdf-page

define method make-pdf-page( document :: <pdf-document>, #key bounds :: <pdf-array> )
=> ( result :: <pdf-indirect-object> )
    let page :: <pdf-dictionary> = make( <pdf-dictionary> );
    
    page[ "Type" ] := as( <pdf-name>, "Page" );
    page[ "Parent" ] := document.page-tree-root;
    page[ "Resources" ] := make( <pdf-dictionary> );
    page[ "MediaBox" ] := bounds;
    
    let indirect-page = make( <pdf-indirect-object>, value: page, document: document );
    add!( document.page-tree-root.value[ "Kids" ], indirect-page );
    add!( document.body, indirect-page );
    
    let contents-stream = make( <pdf-stream>, direction: #"output" );
    let contents = make( <pdf-indirect-object>, document: document, value: contents-stream );
    page[ "Contents" ] := contents;
    add!( document.body, contents );
    
    page[ "Resources" ][ "ProcSet" ] := document.proc-set;
    
    document.page-tree-root.value[ "Count" ] := document.page-tree-root.value[ "Count" ] + 1;
    
    indirect-page;
end method make-pdf-page;


// pdf-page-contents-stream

define method pdf-page-contents-stream( page :: <pdf-dictionary> )
=> ( <pdf-stream> )
    page[ "Contents" ].value;
end method pdf-page-contents-stream;


// add-pdf-page-font

define method add-pdf-page-font(  page :: <pdf-dictionary>, font :: <pdf-indirect-object> )
=> ()
    let font-dictionary = element( page[ "Resources" ], "Font", default: #f );
    if( ~ font-dictionary )
        font-dictionary := make( <pdf-dictionary> );
        page[ "Resources" ][ "Font" ] := font-dictionary;
    end if;
    font-dictionary[ font.value[ "Name" ] ] := font;

    values();
end method add-pdf-page-font;