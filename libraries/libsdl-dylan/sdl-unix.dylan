module: sdl
author: Rob Myers
copyright: Copyright (c) 2003 Gwydion Dylan Maintainers
license: LGPL

define method SDL-Run( fun, name, args )
=> (result :: <integer> )
	fun( name, args );
end;