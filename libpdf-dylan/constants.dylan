module: pdf
synopsis: PDF document class and methods
author: Rob Myers. dylan@robmyers.org
copyright: Standard Gwydion Dylan. 

// Standard European and American page sizes

// Standard colours

// standard units


// PDF Document Header

define constant $pdf-header :: <string> = "%PDF-1.3";


// null

define constant $pdf-null = singleton( make( <pdf-null> ) );


// Standard Fonts

define constant $Courier :: <pdf-name> = as( <pdf-name>, "Courier" ); 
define constant $Helvetica  :: <pdf-name> = as( <pdf-name>, "Helvetica" );
define constant $Times-Roman :: <pdf-name> = as( <pdf-name>, "Times-Roman" );
define constant $Symbol :: <pdf-name> = as( <pdf-name>, "Symbol" );

define constant $Courier-Bold :: <pdf-name> = as( <pdf-name>, "Courier-Bold" ); 
define constant $Helvetica-Bold :: <pdf-name> = as( <pdf-name>, "Helvetica-Bold" ); 
define constant $Times-Bold :: <pdf-name> = as( <pdf-name>, "Times-Bold" ); 

define constant $Courier-Oblique :: <pdf-name> = as( <pdf-name>, "Courier-Oblique" ); 
define constant $Helvetica-Oblique :: <pdf-name> = as( <pdf-name>, "Helvetica-Oblique" ); 
define constant $Times-Italic :: <pdf-name> = as( <pdf-name>, "Times-Italic" );

define constant $Courier-BoldOblique :: <pdf-name> = as( <pdf-name>, "Courier-BoldOblique" ); 
define constant $Helvetica-BoldOblique :: <pdf-name> = as( <pdf-name>, "Helvetica-BoldOblique" ); 
define constant $Times-BoldItalic :: <pdf-name> = as( <pdf-name>, "Times-BoldItalic" );
define constant $ZapfDingbats :: <pdf-name> = as( <pdf-name>, "ZapfDingbats" );