module: sdl
module: dylan-user
author: Rob Myers
copyright: Copyright (c) 2003 Rob Myers

define interface
  #include { "SDL/SDL.h", "SDL/SDL_active.h", "SDL/SDL_audio.h", "SDL/SDL_byteorder.h", "SDL/SDL_cdrom.h", "SDL/SDL_copying.h",
					"SDL/SDL_endian.h", "SDL/SDL_error.h", "SDL/SDL_events.h", "SDL/SDL_getenv.h", "SDL/SDL_joystick.h",
					"SDL/SDL_keyboard.h", "SDL/SDL_keysym.h", "SDL/SDL_main.h", "SDL/SDL_mouse.h", "SDL/SDL_mutex.h", 
					"SDL/SDL_opengl.h", "SDL/SDL_quit.h", "SDL/SDL_rwops.h", "SDL/SDL_syswm.h", "SDL/SDL_thread.h", "SDL/SDL_timer.h",
					"SDL/SDL_types.h", "SDL/SDL_version.h", "SDL/SDL_video.h"/*, "SDL/begin_code.h", "SDL/close_code.h"*/ },
    import: all,  
	exclude: { "SDL_dummy_enum", "main" },
    map: { "char*" => <byte-string> },
    equate: { "char*" => <c-string> };
end interface