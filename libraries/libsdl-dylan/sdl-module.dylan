module: dylan-user
author: Rob Myers
copyright: Copyright (c) 2003 Gwydion Dylan Maintainers
license: LGPL

define module sdl
  use dylan;
  use extensions;
  use melange-support;
	
	use sdl-wrappers,
		export: all;
	export 
		SDL-Run;
end module sdl;