Module:    c3-engine
Synopsis:  Interface to the Creatures 3 engine
Author:    Chris Double
Copyright: (C) 1999, Chris Double. All rights reserved.
License:   See License.txt

define method c3-engine-version () => (full-name :: <byte-string>)
  "1.1.1";
end method c3-engine-version;

define C-function OpenFileMapping
  parameter dwDesiredAccess :: <DWORD>;
  parameter bInheritHandle :: <BOOL>;
  parameter lpName :: <LPCSTR>;
  result value :: <HANDLE>;
  c-name: "OpenFileMappingA", c-modifiers: "__stdcall";
end;

define constant $EVENT-ALL-ACCESS = logior($STANDARD-RIGHTS-REQUIRED,
    $SYNCHRONIZE, 3);
define constant $PROCESS-ALL-ACCESS = logior($STANDARD-RIGHTS-REQUIRED,
    $SYNCHRONIZE, #xfff);

define constant $WAIT-OBJECT-0 = 0;


define class <creatures-engine> (<object>)
  slot engine-memory-handle = #f;
  slot engine-memory-pointer = #f;
  slot engine-request-event = #f;
  slot engine-result-event = #f;
  slot engine-mutex = #f;
end class;

define method connect ( engine :: <creatures-engine>, game-name )
  let memory-name = concatenate( game-name, "_mem" );
  let result-event-name = concatenate( game-name, "_result");
  let request-event-name = concatenate( game-name, "_request");
  let mutex-name = concatenate( game-name, "_mutex");

  engine.engine-memory-handle := OpenFileMapping($FILE-MAP-ALL-ACCESS,
                                                 #f,
                                                 memory-name);
  when(null-pointer?(engine.engine-memory-handle))
    engine.engine-memory-handle := #f;
    error("Could not create memory handle when connecting to Creatures.");
  end when;

  engine.engine-memory-pointer := MapViewOfFile(engine.engine-memory-handle,
                                                $FILE-MAP-ALL-ACCESS,
                                                0, 0, 0);
  when(null-pointer?(engine.engine-memory-pointer))
    engine.engine-memory-pointer := #f;
    error("Could not map to shared memory area when connecting to Creatures.");
  end when;

  engine.engine-mutex := OpenMutex($MUTEX-ALL-ACCESS, #f, mutex-name);
  when(null-pointer?(engine.engine-mutex))
    engine.engine-mutex := #f;
    error("Could not open Creatures mutex.");
  end when;

  engine.engine-request-event := OpenEvent($EVENT-ALL-ACCESS, #f, request-event-name);
  when(null-pointer?(engine.engine-request-event))
    engine.engine-request-event := #f;
    error("Could not open Creatures request event.");
  end when;

  engine.engine-result-event := OpenEvent($EVENT-ALL-ACCESS, #f, result-event-name);
  when(null-pointer?(engine.engine-result-event))
    engine.engine-result-event := #f;
    error("Could not open Creatures result event.");
  end when;
end;
  
define method disconnect ( engine :: <creatures-engine> )
  when(engine.engine-result-event)
    CloseHandle(engine.engine-result-event); 
    engine.engine-result-event := #f;
  end when;

  when(engine.engine-request-event)
    CloseHandle(engine.engine-request-event); 
    engine.engine-request-event := #f;
  end when;
 
  when(engine.engine-mutex)
    CloseHandle(engine.engine-mutex); 
    engine.engine-mutex := #f;
  end when;

  when(engine.engine-memory-pointer)
    UnmapViewOfFile(engine.engine-memory-pointer); 
    engine.engine-memory-pointer := #f;
  end when;
                                                 
  when(engine.engine-memory-handle)
    CloseHandle(engine.engine-memory-handle); 
    engine.engine-memory-handle := #f;
  end when;
end;

define macro with-creatures-engine
  { with-creatures-engine (?engine:name, ?game-name:expression) ?:body end }
    => { begin
           let ?engine = make(<creatures-engine>);
           block()
             connect(?engine, ?game-name);
             ?body
           cleanup
             disconnect(?engine);
           end block;
         end begin;
        }
end macro;
    
define macro with-mutex
  { with-mutex (?mutex:expression) ?:body end }
    => { begin
           when (WaitForSingleObject(?mutex, 10000) = $WAIT-OBJECT-0)
             block()
               ?body
             cleanup
               ReleaseMutex(?mutex);
             end;
           end;
         end }
end macro;

define method raw-execute-caos ( engine :: <creatures-engine>, caos )
  let macro-ptr = c-type-cast(<c-unsigned-char*>, engine.engine-memory-pointer);
  with-mutex(engine.engine-mutex)
    let index = 24;
    for(c in caos)
      macro-ptr[index] := as(<integer>, c);
      index := index + 1;
    end for;
    macro-ptr[index] := 0;
    ResetEvent(engine.engine-result-event);
    PulseEvent(engine.engine-request-event);
    let server-process = make(<LPDWORD>, 
      address: \%+(pointer-address(engine.engine-memory-pointer), 4));
    let process-handle = OpenProcess($PROCESS-ALL-ACCESS, #f, pointer-value(server-process));
    if(null-pointer?(process-handle))
      WaitForSingleObject(engine.engine-result-event, 60000);
    else
      block()
        with-stack-structure( handle-array :: <LPHANDLE>, element-count: 2 )
          handle-array[0] := process-handle;
          handle-array[1] := engine.engine-result-event;
          WaitForMultipleObjects(2, handle-array, #f, 60000);
        end with-stack-structure;
      cleanup
        CloseHandle(process-handle);
      end block;
    end if;
    as(<byte-string>, make(<c-string>, address: \%+(pointer-address(engine.engine-memory-pointer), 24)));
  end with-mutex;
end method;  

define method execute-caos ( engine :: <creatures-engine>, caos )
  raw-execute-caos(engine, concatenate("execute\n", caos));
end method;
  
define function raw-c3-caos( caos )
  with-creatures-engine( engine, "Creatures 3" )
    raw-execute-caos( engine, caos );
  end with-creatures-engine;
end function;

define function raw-docker-caos( caos )
  with-creatures-engine( engine, "Docking Station" )
    raw-execute-caos( engine, caos );
  end with-creatures-engine;
end function;

define function c3-caos( caos )
  with-creatures-engine( engine, "Creatures 3" )
    execute-caos( engine, caos );
  end with-creatures-engine;
end function;
