module: test
synopsis: test sdl
author: Rob Myers
copyright: Copyright (c) 2003 GwydionDylan Maintainers
license: LGPL

define function main(name, arguments)
	let init-flags :: <integer> = $SDL-INIT-VIDEO;
	let  video-bpp :: <integer> = 0;
	let video-flags :: <integer> = $SDL-SWSURFACE;

	/* Initialize the SDL library */
	if ( SDL-Init( init-flags ) < 0 ) 
		format( *standard-error*, "Couldn't initialize SDL: %s\n", SDL-Get-Error());
		exit-application( 1 );
	end if;

	/* Set 640x480 video mode */
	let screen :: <SDL-Surface*> = SDL-Set-Video-Mode( 640,480, video-bpp, video-flags );
  if( screen = make( <SDL-Surface*>, pointer: 0 ) )
		format( *standard-error*, "Couldn't set 640x480x%d video mode: %s\n", video-bpp, SDL-Get-Error());
		SDL-Quit();
		exit-application( 2 );
	end if;

	let loop :: <boolean> = #t;
	block( break )
		let event :: <SDL-Event*> = make( <SDL-Event*> );
		while( loop )
			/* Check for events */ 		
			let got :: <integer> = SDL-Poll-Event( event );
			if( got ~= 0 )
				break( got );
			end if;
			select( event.type-value)
				$SDL-QUIT => 
					loop := #f;
				otherwise => 
					#f;
			end select;
		end while;
	end block;
	/* Clean up the SDL library */
	SDL-Quit();
  exit-application(0);
end function main;

// Go!
SDL-Run(main, application-name(), application-arguments());
