module: types
synopsis: Common repository for type constants
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define constant <fp> :: <class> = <double-float>; // change them when settled
define constant $pi :: <fp> = $double-pi;


/* This should be replaced by <vector3D> */
define sealed class <point>(<object>)
     slot point-x :: <fp>, required-init-keyword: x:;
     slot point-y :: <fp>, required-init-keyword: y:;
     slot point-z :: <fp>, required-init-keyword: z:;
end class <point>;

