module: icfp2001
synopsis: Dylan Hackers entry in the Fourth Annual (2001) ICFP Programming Contest
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define class <document> (<object>)
  slot characters;
end class <document>;

define class <char> (<object>)
  slot character;
  slot attribute;
end class <char>;

define class <attribute> (<object>)  // make that a flyweight pattern one day
  slot bold       :: <boolean>;
  slot emphasis   :: <boolean>;
  slot italic     :: <boolean>;
  slot strong     :: <boolean>;
  slot typewriter :: <boolean>;
  slot underline  :: false-or(limited(<integer>, min:0, max: 3));
  slot size       :: false-or(limited(<integer>, min:0, max: 9));
  slot color      :: <color>;
end class <attribute>;

define constant <color> = one-of(#"red", #"green", #"blue", 
                                 #"cyan", #"magenta", #"yellow",
                                 #"black", #"white");

define constant <space> =
  one-of(as(<character>, #x20), as(<character>, #x9), as(<character>, #xd),
         as(<character>, #x0a));

define function debug(#rest args)
  apply(format, *standard-error*, args);
end function debug;


define function main(name, arguments)
  let input-stream = *standard-input*;

  block ()

    let original-input = read-to-end(input-stream);

    write(*standard-output*, original-input);

    debug("Run successful. Original size: %i. Size after optimization: %i.\n");

  /*
<bruce> - readin the original text and save it in case we can't do any better
<bruce> - convert to fully-annotated runs of characters
<bruce> - do a one-pass simple-minded markup and keep it if it's better than the original in 
                                                        case we can't do any better
<andreas> - generate permutations of the markup until time runs out.
<bruce> - step through the text.  At each change you have to consider whether to open a new tag
           or close the most recent old one.  If multiple attributes change then you have 
           multiuple choices for which to open first
<bruce> all could be done in parallel, keeping track of the comparitive resulting sizes
<bruce> (this is the "big risk, big win" approach...)
<andreas> Some sort of pruning will be required.
                                 */
                                 

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
