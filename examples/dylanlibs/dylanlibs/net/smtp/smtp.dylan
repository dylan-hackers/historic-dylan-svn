Module:    smtp
Synopsis:  SMTP mail interface
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// SMTP negotiation based on Open Source Common Lisp SMTP code available from
// http://opensource.franz.com
//

define constant $linefeed = as(<character>, 10);
define constant $return =   as(<character>, 13);

// A receiver of an email can be a string containing
// the email address of a sequence of strings for multiple
// recipients.
define constant <receiver> = type-union(<string>, <list>);

define method send-mail(server :: <string>,
                        from :: <string>,
                        to :: <receiver>, 
                        message :: <string>,
                        #key cc, bcc, subject, reply-to, headers)
 => ()
  local method ensure-list(r :: false-or(<receiver>))
   => (r :: <list>)
     if(r)
       if(instance?(r, <list>))
         r
       else
         list(r)
       end if
     else
       #()
     end if;
  end method;

  do-send-mail(server,
               from,
               ensure-list(to),
               ensure-list(cc),
               ensure-list(bcc),
               subject,
               reply-to,
               ensure-list(headers),
               message);               
end method send-mail;
  
define method build-headers(from :: <string>,
                            to :: <list>,
                            cc :: <list>,
                            subject :: false-or(<string>),
                            reply-to :: false-or(<string>),
                            headers :: <list>)
 => (r :: <string>)
  let stream = make(<string-stream>, direction: #"output");
  format(stream, "From: %s%c%c", from, $return, $linefeed); 

  format(stream, "To: ");
  for(address keyed-by index in to)
    unless(zero?(index))
      format(stream, ", ");
    end unless;

    format(stream, "%s", address);
  finally
    format(stream, "%c%c", $return, $linefeed);
  end for;

  unless(empty?(cc))
    format(stream, "Cc: ");
    for(address keyed-by index in cc)
      unless(zero?(index))
        format(stream, ", ");
      end unless;

      format(stream, "%s", address);
    finally
      format(stream, "%c%c", $return, $linefeed);
    end for;
  end unless;

  when(subject)
    format(stream, "Subject: %s%c%c", subject, $return, $linefeed);
  end when;

  when(reply-to)
    format(stream, "Reply-To: %s%c%c", reply-to, $return, $linefeed);
  end when;

  for(header in headers)
    format(stream, "%s%c%c", header, $return, $linefeed);
  end for;

  stream.stream-contents;
end method build-headers;

define method do-send-mail(server :: <string>,
                           from :: <string>,
                           to :: <list>,
                           cc :: <list>,
                           bcc :: <list>,
                           subject :: false-or(<string>),
                           reply-to :: false-or(<string>),
                           headers :: <list>,
                           message :: <string>)
 => ()
  let header-string = build-headers(from, to, cc, subject, reply-to, headers);
  let message = concatenate(header-string, message);


  with-socket(socket, host: server, port: 25)
    let (code, response) = read-smtp-response(socket);
    unless(code == 2)
      error("Initial SMTP connect failed: %s", response);
    end unless;

// local-host on a tcp socket fails for some reason with
// no method found. Workaround to send local host address.
//    let local-name = host-name(local-host(socket));
//    send-smtp-command(socket, "HELO %s", local-name);
    send-smtp-command(socket, "HELO 127.0.0.1");
    let (code, response) = read-smtp-response(socket);
    unless(code == 2)
      error("SMTP HELO command failed: %s", response);
    end unless;

    send-smtp-command(socket, "MAIL from:<%s>", from);
    let (code, response) = read-smtp-response(socket);
    unless(code == 2)
      error("SMTP MAIL from command failed: %s", response);
    end unless;

    let all-to = concatenate(to, cc, bcc);
    for(address in all-to)
      send-smtp-command(socket, "RCPT to:<%s>", address);
      let (code, response) = read-smtp-response(socket);
      unless(code == 2)
        error("SMTP RCPT to command failed: %s", response);
      end unless;
    end for;

    send-smtp-command(socket, "DATA");
    let (code, response) = read-smtp-response(socket);
    unless(code == 3)
      error("SMTP DATA command failed: %s", response);
    end unless;

    let beginning? = #t;
    for(ch keyed-by index in message)
      // Send a '.' at the beginning of a line twice 
      // so it is not interpreted as an end of line.
      when(beginning? & ch == '.')
        write-element(socket, '.');
      end when;

      if(ch == $linefeed)
        beginning? := #t;
        write-element(socket, $return);
      else
        beginning? := #f;
      end if;
      
      write-element(socket, ch);
    end for;

    format(socket, 
           "%c%c.%c%c", 
           $return, $linefeed,
           $return, $linefeed);
    
    let (code, response) = read-smtp-response(socket);
    unless(code == 2)
      error("SMTP Message not sent: %s", response);
    end unless;

    send-smtp-command(socket, "QUIT");
    let (code, response) = read-smtp-response(socket);
    unless(code == 2)
      error("SMTP Quit failed: %s", response);
    end unless;
  end with-socket;
end method do-send-mail;

// Read a line from the socket. If the line ends in a 
// carriage return/line feed consider that the end of the line.
// If an end of file occurs before the line finishes, return the
// current string of characters read and the second return value 
// is #t.
define method read-socket-line(socket :: <stream>)
 => (r :: <string>, eof? :: <boolean>)
  let line = make(<stretchy-vector>);
  let state = #"character";
  block(return)
    while(#t)
      let ch = read-element(socket);
      select(state)
        #"character" 
         => begin
              if(ch == $return)
                state := #"character-or-linefeed";
              else
                line := add!(line, ch);
              end if;
            end begin;
        #"character-or-linefeed"
         => begin
              if(ch == $linefeed)
                return(as(<string>, line), #f);
              else
                state := #"character";
                line := add!(line, $return);
                line := add!(line, ch);
              end if;
            end begin;
      end select;
    end while;            
  exception(e :: <end-of-stream-error>)
    values(as(<string>, line), #t);
  end block;       
end method read-socket-line;

define method send-smtp-command(socket :: <stream>, #rest rest)
  let command = apply(format-to-string, rest);
  apply(format, socket, rest);
  format(socket, "%c%c", $return, $linefeed);
  force-output(socket);
end method send-smtp-command;

// Return true if the given character is a digit
define method digit?(ch :: <character>)
 => (r :: <boolean>)
  #f
end method digit?;

define method digit?(ch :: one-of('0', '1', '2', '3', '4', 
                                  '5', '6', '7', '8', '9'))
 => (r :: <boolean>)
  #t
end method digit?;

// Read a response from the SMTP server, returning the
// response code and the response as a string. Multiline
// responses are handled by concatenating the lines onto the
// returned string.
define method read-smtp-response(socket :: <stream>)
 => (code :: <integer>, response :: <string>)
  force-output(socket);
  let (line, eof?) = read-socket-line(socket);
  if(eof?)
    // Should this be an error?
    values(-1, line)
  else
    let response = line;

    // Check for multi-line response and handle.
    when(line.size > 3 & line[3] == '-')
      block(return)
        while(#t)
          let (new-line, eof?) = read-socket-line(socket);
          if(eof?)
            // Should this be an error?
            return();
          else
            if(new-line.size > 3 &
               new-line[3] == ' ' &
               copy-sequence(line, start: 0, end: 3) = 
                 copy-sequence(new-line, start: 0, end: 3))
              return();
            else
              response := concatenate(response, line, "\n");
            end if;            
          end if;
        end while;
      end block;
    end when;

    values(if(~empty?(response) & digit?(response[0]))
             as(<integer>, response[0]) - as(<integer>, '0');
           else
             -1
           end, response);
  end if;    
end method read-smtp-response;

define method initialize-smtp-library()
 => ()
  start-sockets();
end method initialize-smtp-library;
