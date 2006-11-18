Module:    xmpp-bot
Author:    Hannes Mehnert
Copyright: (C) 2006,  All rights reserved.


define variable *online-users* = make(<list>);

define class <xmpp-bot> (<object>)
  slot online-users :: <list> = make(<list>);
  slot client :: <xmpp-client>, init-keyword: client:;
  slot password :: <string>, required-init-keyword: password:;
end;

define method make (class == <xmpp-bot>,
                    #next next-method,
                    #rest rest,
                    #key jid,
                    #all-keys) => (xmpp-bot :: <xmpp-bot>)
  let args = rest;
  if (jid)
    if (instance?(jid, <string>))
      jid := as(<jid>, jid);
    end;
    let client = make(<xmpp-client>, jid: jid);
    args := add!(args, #"client");
    args := add!(args, client);
  end;
  apply(next-method, class, args);
end;

define method initialize (xmpp-bot :: <xmpp-bot>,
                          #rest rest, #key, #all-keys)
  let subscription-callback = make(<callback>, reference: #"default", priority: 3, handler: curry(auto-subscriber, xmpp-bot));
  add-callback(xmpp-bot.client, <presence>, subscription-callback);
  if (~ connect(xmpp-bot.client))
    exit-application(1);
  end if;
  authenticate(xmpp-bot.client, xmpp-bot.password, #f);
  send(xmpp-bot.client, make(<presence>, priority: 23));
  send(xmpp-bot.client, make(<iq>, type: #"get", query: with-xml() query(xmlns => "jabber:iq:roster") end, id: "roster"));
end;

define method auto-subscriber (xmpp-bot, client, presence)
  if (presence.type)
    select (presence.type)
      #"subscribe" => begin
                        send(client, make(<presence>,
                                          to: presence.from,
                                          type: #"subscribed"));
                        send(client, make(<presence>,
                                          to: presence.from,
                                          type: #"subscribe"));
                        
                      end;
      #"unsubscribe" =>
                        begin
                          send(client, make(<presence>,
                                            to: presence.from,
                                            type: #"unsubscribed"));
                          send(client, make(<presence>,
                                            to: presence.from,
                                            type: #"unavailable"));
                          send(client, make(<presence>,
                                            to: presence.from,
                                            type: #"unsubscribe"));
                          xmpp-bot.online-users := remove!(xmpp-bot.online-users,
                                                           as(<string>, presence.from),
                                                           test: subsequence-position);
                        end;
      #"unavailable" => begin
                          format-out("%s went offline\n", as(<string>, presence.from));
                          xmpp-bot.online-users := remove!(xmpp-bot.online-users,
                                                           as(<string>, presence.from),
                                                           test: \=);
                        end;
       otherwise => begin
                      format-out("Didn't know what to do with type %=\n", presence.type);
                    end;
    end select;
  else
    unless (any?(method(a) a = as(<string>, presence.from) end, xmpp-bot.online-users))
      xmpp-bot.online-users := add!(xmpp-bot.online-users, as(<string>, presence.from));
    end;
  end;
end;

define method broadcast-message (bot :: <xmpp-bot>, message :: <string>)
  do(method (user)
       send(bot.client,
            make(<message>,
                 type: #"chat",
                 body: message,
                 to: user));
     end, bot.online-users);
end;

