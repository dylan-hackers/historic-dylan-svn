Module:    send-keys-internal
Synopsis:  Library to send keystrokes to the keyboard buffer
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

define C-struct <KEYBDINPUT>
  slot wVk-value :: <WORD>;
  slot wScan-value :: <WORD>;
  slot dwFlags-value :: <DWORD>;
  slot dwTime-value :: <DWORD>;
  slot dwExtraInfo-value :: <DWORD>;
  pointer-type-name: <LPKEYBDINPUT>;
end C-struct <KEYBDINPUT>;

define C-struct <MOUSEINPUT>
  slot dx-value :: <LONG>;
  slot dy-value :: <LONG>;
  slot mouseData-value :: <DWORD>;
  slot dwFlags-value :: <DWORD>;
  slot dwTime-value :: <DWORD>;
  slot dwExtraInfo-value :: <DWORD>;
  pointer-type-name: <LPMOUSEINPUT>;
end C-struct <MOUSEINPUT>;

define C-union <input-struct>
  slot mi-value :: <MOUSEINPUT>;
  slot ki-value :: <KEYBDINPUT>;
end;

define C-struct <INPUT>
  slot type-value :: <DWORD>;
  slot is-value :: <input-struct>;
  pointer-type-name: <LPINPUT>;
end C-struct <INPUT>;

define C-function SendInput
  parameter nInputs :: <UINT>;
  parameter pInputs :: <LPINPUT>;
  parameter cvSize  :: <UINT>;
  result value :: <UINT>;
  c-name: "SendInput", c-modifiers: "__stdcall";
end;

define constant $INPUT-MOUSE = 0;
define constant $INPUT-KEYBOARD = 1;
define constant $INPUT-HARDWARE = 2;

define constant <key-state> = one-of(#"down", #"up");

define constant $keymap = make(<table>);

begin
  let s = make(<string>, size: 1);
  for(index from as(<integer>, 'A') to as(<integer>, 'Z'))
    s[0] := as-uppercase(as(<character>, index));
    $keymap[as(<symbol>, s)] := index;
  end for;

  for(index from as(<integer>, '0') to as(<integer>, '9'))
    s[0] := as(<character>, index);
    $keymap[as(<symbol>, s)] := index;
  end for;

  $keymap[#"."] := $VK-DECIMAL;  
  $keymap[#" "] := $VK-SPACE;  
  $keymap[#"space"] := $VK-SPACE;  
  $keymap[#"shift"] := $VK-SHIFT;  
  $keymap[#"control"] := $VK-CONTROL;  
  $keymap[#"end"] := $VK-END; 
  $keymap[#"return"] := $VK-RETURN;  
  $keymap[#"down"] := $VK-DOWN;  
  $keymap[#"up"] := $VK-UP;  
  $keymap[#"insert"] := $VK-INSERT;  
  $keymap[#"escape"] := $VK-ESCAPE;  
  $keymap[#"f1"] := $VK-F1;  
end;

define method keysym->vk(keysym) => (vk)
  $keymap[keysym];
end method keysym->vk;

define class <key-entry> (<object>)
  constant slot key-entry-keysym, required-init-keyword: keysym:;
  constant slot key-entry-state :: <key-state>, required-init-keyword: state:;
end class <key-entry>;

define method create-input-structure(key-entries)
  let count = key-entries.size;
  let key-input = make(<LPINPUT>, element-count: count);
  for(entry in key-entries, index from 0 by 1)
    let address = pointer-value-address(key-input, index: index);
    let vk = keysym->vk(entry.key-entry-keysym);
    address.type-value := $INPUT-KEYBOARD;
    address.is-value.ki-value.wVk-value := vk;
    address.is-value.ki-value.wScan-value := MapVirtualKey(vk, 0);
    address.is-value.ki-value.dwFlags-value := 
      if(entry.key-entry-state == #"up") 
        $KEYEVENTF-KEYUP;
      else
        0
      end if;
    address.is-value.ki-value.dwTime-value := 0;
    address.is-value.ki-value.dwExtraInfo-value := 0;
  end for;  
  key-input;
end method create-input-structure;

define function create-up-down-key-press(keysym)
  vector(
    make(<key-entry>, keysym: keysym, state: #"down"),
    make(<key-entry>, keysym: keysym, state: #"up"))
end function create-up-down-key-press;

define method send-keys(key-entries, #key delay)
  let input = create-input-structure(key-entries);
  if(key-entries.size < 20)
    SendInput(key-entries.size, input, size-of(<INPUT>));
    when(delay)
      sleep(delay)
    end;
  else
    for(index from 0 below key-entries.size by 1)
      let len = min(index + 1, key-entries.size) - index;
      SendInput(len, pointer-value-address(input, index: index), size-of(<INPUT>));
      sleep(.5);
    end for;
  end if;
    
end method send-keys;

define method char->keysym(ch)
  if(ch = '\n')
    #"return"
  else
    let s = make(<string>, size: 1);
    s[0] := ch;
    as(<symbol>, s);
  end if;
end method;

define method get-key-entries(s :: <string>)
  let key-entries = make(<stretchy-vector>);
  for(ch in s)
    let keysym = char->keysym(ch);
    key-entries := add!(key-entries, make(<key-entry>, keysym: keysym, state: #"down"));
    key-entries := add!(key-entries, make(<key-entry>, keysym: keysym, state: #"up"));
  finally
    key-entries;
  end for;
end method get-key-entries;

define method send-keys(s :: <string>, #key delay)
  send-keys(get-key-entries(s), delay: delay);
end method;

define C-struct <COORD>
  slot x-value :: <USHORT>;
  slot y-value :: <USHORT>;
  pointer-type-name: <COORD*>;
end C-struct <COORD>;

define C-function ReadConsoleOutputCharacter
  parameter hConsoleOutput :: <HANDLE>;
  parameter lpCharacter :: <LPCSTR>;
  parameter nLength :: <DWORD>;
  parameter dwReadCoord :: <COORD>;
  parameter lpNumberOfCharsRead :: <LPDWORD>;
  result value :: <BOOL>;
  c-name: "ReadConsoleOutputCharacterA", c-modifiers: "__stdcall"; 
end;

define method get-console-text(x :: <integer>, y :: <integer>, length :: <integer>) => (r :: <string>)
  let std-out = GetStdHandle($STD-OUTPUT-HANDLE);

  with-stack-structure(text :: <c-string>, element-count: length)  
    clear-memory!(text, length);
    
    with-stack-structure( coord :: <COORD*> )
      with-stack-structure(result-size :: <LPDWORD>)
        coord.x-value := x;	
        coord.y-value := y;	

        ReadConsoleOutputCharacter(std-out,
                                   text,
                                   length,
                                   coord, 
                                   result-size);
        as(<byte-string>, text);
      end;
    end;
  end;
end method get-console-text;

define method wait-for-console-text(x, y, look-for, #key poll = 1)
  let found = get-console-text(x, y, look-for.size) = look-for;
  while(~found)
    sleep(poll);
    found := get-console-text(x, y, look-for.size) = look-for;
  end;
end method wait-for-console-text;


