Module:    Dylan-User
Synopsis:  Simple property-list management library
Author:    Scott McKay
Copyright: 1997 The Harlequin Group Limited.  All rights reserved.

define module plists
  use dylan;
  use dylan-extensions,
    import: { \when,
	      \assert,
	      \without-bounds-checks,
	      <stretchy-object-vector> };
  use harlequin-extensions,
    import: { concatenate! };
  create get-property,
	 \put-property!, do-put-property!,
         keyword-sequence, value-sequence,
	 \remove-property!, do-remove-property!,
	 remove-keywords, \with-keywords-removed;
end module plists;
