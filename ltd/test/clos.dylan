//  -*- Mode: Lisp; Syntax: Common-Lisp;  -*-
//  Code from Paradigms of Artificial Intelligence Programming
//  Copyright (c) 1991 Peter Norvig
//  File clos.lisp: Object-oriented programming examples
define class <account> (<object>)
  slot account-name = "", init-keyword: #"account-name";
  slot account-balance = 0.0, init-keyword: #"account-balance";
  slot account-interest-rate = 0.06, init-keyword: #"account-interest-rate";
end class <account>;

define method account-withdraw (account, amt)
  // Make a withdrawal from this account.
  if (amt <= account.account-balance)
    dec!(account.account-balance, amt);
  else
    #"insufficient-funds";
  end if;
end method account-withdraw;

define method account-deposit (account, amt)
  // Make a deposit to this account.
  inc!(account.account-balance, amt);
end method account-deposit;

define method account-interest (account)
  // Accumulate interest in this account.
  inc!(account.account-balance,
       account.account-interest-rate * account.account-balance);
end method account-interest;

//  ==============================
define method new-account (name, #key balance = 0.0, interest-rate = 0.06)
  // Create a new account that knows the following messages:
  method (message)
    select (message)
      #"withdraw"
         => method (amt)
              if (amt <= balance)
                dec!(balance, amt);
              else
                #"insufficient-funds";
              end if;
            end method;
      #"deposit"
         => method (amt) inc!(balance, amt); end method;
      #"balance"
         => method () balance; end method;
      #"name"
         => method () name; end method;
      #"interest"
         => method () inc!(balance, interest-rate * balance); end method;
      otherwise
         => #f;
    end select;
  end method;
end method new-account;

//  ==============================
define method get-method (object, message)
  // Return the method that implements message for this object.
  object(message);
end method get-method;

define method send (object, message, #rest args)
  // Get the function to implement the message,
  //   and apply the function to the args.
  apply(get-method(object, message), args);
end method send;

//  ==============================
define method withdraw (object, #rest args)
  // Define withdraw as a generic function on objects.
  apply(get-method(object, #"withdraw"), args);
end method withdraw;

//  ==============================
// LTD: No macros.
#"define-class";

define method make-clause (clause)
  // Translate a message from define-class into a case clause.
  bq-list(first(clause),
          bq-list(#"function",
                  bq-list*(#"lambda", second(clause), rest2(clause))));
end method make-clause;

define method ensure-generic-fn (message)
  // Define an object-oriented dispatch function for a message,
  //   unless it has already been defined as one.
  if (~ generic-fn-p(message))
    let fn
        = method (object, #rest args)
            apply(get-method(object, message), args);
          end method;
    message := fn;
    symbol-get-property(message, #"generic-fn") := fn;
  end if;
end method ensure-generic-fn;

define method generic-fn-p (fn-name)
  // Is this a generic function?
  // LTD: Function FBOUNDP not yet implemented.
  fboundp(fn-name)
   & symbol-get-property(fn-name, #"generic-fn") == fn-name;
end method generic-fn-p;

//  ==============================
"eof";

