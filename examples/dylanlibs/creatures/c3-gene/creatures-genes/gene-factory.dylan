Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define class <gene-class-entry> (<object>)
    constant slot gene-class-description :: <string>, required-init-keyword: description:;
    constant slot gene-class-class :: subclass(<gene>), required-init-keyword: class:;
    constant slot gene-class-type :: <byte>, required-init-keyword: type:;
    constant slot gene-class-subtype :: <byte>, required-init-keyword: subtype:;
end class <gene-class-entry>;

define constant $gene-classes = make(<stretchy-vector>);

define function install-gene-class(description :: <string>, 
    class :: subclass(<gene>), 
    type :: <byte>, 
    subtype :: <byte>) => ()
    add!($gene-classes, make(<gene-class-entry>, description: description, class: class, type: type, subtype: subtype));
end function install-gene-class;




