Module:    http-client-example
Synopsis:  An example of using http-client library to send SMS messages via a web form.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define frame <sms-sender-frame> (<simple-frame>)
  pane help-pane (frame)
    make(<label>, label: "Enter your username and password for http://www.mtnsms.com");

  pane username-pane (frame)
    make(<text-field>);

  pane password-pane (frame)
    make(<password-field>);

  pane number-pane (frame)
    make(<text-field>);

  pane message-pane (frame)
    make(<text-editor>);

  pane send-button (frame)
    make(<push-button>, 
         label: "Send", 
         activate-callback: method(g) on-send-button(frame) end);

  layout (frame)
    vertically()
      frame.help-pane;
      tabling(spacing: 2, columns: 2)
        make(<label>, label: "Username:");
        frame.username-pane;
        make(<label>, label: "Password:");
        frame.password-pane;
        make(<label>, label: "Phone Number:");
        frame.number-pane;
      end tabling;
      make(<label>, label: "Message");
      frame.message-pane;
      frame.send-button;
    end vertically;

  keyword title: = "SMS Sender";
end frame <sms-sender-frame>;

define method on-send-button(frame :: <sms-sender-frame>)
  send-sms-message(frame.username-pane.gadget-value,
                   frame.password-pane.gadget-value,
                   frame.number-pane.gadget-value,
                   frame.message-pane.gadget-value);
  notify-user("Message sent!");
end method on-send-button;

define method send-sms-message(username :: <string>,
                               password :: <string>,
                               number :: <string>,
                               message :: <string>)
  with-http-session(session = make-http-session(user-agent: "User-Agent: Mozilla/4.0 (compatible; MSIE 5.5; Windows NT 4.0)"))
    do-http-request(session, "http://www.mtnsms.com", follow-location?: #f);
    let query = list(pair("returl", ""),
                     pair("username", username),
                     pair("password", password),
                     pair("akey", "yes"),
                     pair("joinusclick", "no"),
                     pair("email", ""),
                     pair("x", "40"),
                     pair("y", "38"));
    let result1 = do-http-request(session,
                                  "http://www.mtnsms.com/session.asp",
                                  method: #"post",
                                  query: query,
                                  follow-location?: #f);
    let (server-start, server-end) = find-between(result1, 
                                                  "http://",
                                                  "/");
    let server = copy-sequence(result1, start: server-start, end: server-end);
    let result2 = do-http-request(session,
                                  format-to-string("http://%s/sms/xsms.asp", server),
                                  follow-location?: #f);
    let query2 = list(pair("smsToNumbers", number),
                      pair("smsMessage", message),
                      pair("smsSig", "0"),
                      pair("smsSigDyna", "============"),
                      pair("lenSSig", "6"),
                      pair("lenLSig", "13"),
                      pair("lenSysSig", "11"));
    do-http-request(session,
                    format-to-string("http://%s/sms/xsms.asp", server),
                    method: #"post",
                    query: query2,
                    follow-location?: #f);                    
  end with-http-session;
end method send-sms-message;

define method main () => ()
  start-http-client();
  block()
    start-frame(make(<sms-sender-frame>));
  cleanup
    stop-http-client();
  end;
end method main;

begin
  main();
end;
