module: icfp2001
synopsis: Dylan Hackers entry in the Fourth Annual (2001) ICFP Programming Contest
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define class <document> (<object>)
  slot characters;
end class <document>;

define class <character> (<object>)
  slot character;
  slot attribute; // make that a flyweight pattern one day
end class <character>;

define class <attribute> (<object>)
  slot bold       :: <boolean>;
  slot emphasis   :: <boolean>;
  slot italic     :: <boolean>;
  slot strong     :: <boolean>;
  slot typewriter :: <boolean>;
  slot underline  :: limited(<integer>, min:0, max: 3);
  slot size       :: limited(<integer>, min:0, max: 9);
  slot color      :: 

define constant <color> = one-of(#"red", #"green", #"blue", 
                                 #"cyan", #"magenta", #"yellow",
                                 #"black", #"white");

define constant <space> =
  one-of(as(<character>, #x20), as(<character>, #x9), as(<character>, #xd),
         as(<character>, #x0a));


define function main(name, arguments)
  let input-stream = *standard-input*;
/*
  if (arguments.size > 0 & arguments[0] ~= "-")
    input-stream := make(<file-stream>, direction: #"input", locator: arguments[0]);
  end if;
*/

  block ()


    format-out("Run sucessful\n");

  exception (e :: <condition>)

    format-out("Sorry, Dylan Hacker has detected an error\n");
    format-out("\n=========================================\n");
    report-condition(e, *standard-output*);
    format-out("\n=========================================\n");
    format-out("\nProgram terminating with error status\n");

    exit-application(1);
  end;

  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
