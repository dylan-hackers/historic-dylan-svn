module: macros
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/parser/macros.dylan,v 1.4.1.1 1994/12/19 13:02:38 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define abstract class <macro-definition> (<definition>)
  slot macro-intermediate-words :: <simple-object-vector>;
  slot macro-main-rule-set :: <simple-object-vector>,
    required-init-keyword: main-rule-set:;
  slot macro-auxiliary-rule-sets :: <simple-object-vector>,
    required-init-keyword: auxiliary-rule-sets:;
end;

define class <define-macro-definition> (<macro-definition>)
end;

define class <define-bindings-macro-definition> (<macro-definition>)
end;

define class <statement-macro-definition> (<macro-definition>)
end;

define class <function-macro-definition> (<macro-definition>)
end;


// Syntax table manipulation routines.

define method sans-definer (name :: <symbol>)
    => res :: union(<false>, <symbol>);
  let name-str = as(<string>, name);
  let name-size = name-str.size;
  if (name-size > 8)
    block (return)
      for (i from name-size - 8, char in "-definer")
	unless (as-lowercase(name-str[i]) == char)
	  return(#f);
	end;
      end;
      as(<symbol>, copy-sequence(name-str, end: name-size - 8));
    end;
  else
    #f;
  end;
end;

define method check-syntax-table-additions (table :: <table>,
					    defn :: <define-macro-definition>,
					    name :: <symbol>)
    => ();
  let name = name.sans-definer;
  if (name)
    unless (merge-category(table, name, <define-word-token>))
      error("Inconsistent syntax for %=", name);
    end;
  end;
end;

define method make-syntax-table-additions (table :: <table>,
					   defn :: <define-macro-definition>,
					   name :: <symbol>)
    => ();
  let name = name.sans-definer;
  if (name)
    table[name] := merge-category(table, name, <define-word-token>);
  end;
end;

define method check-syntax-table-additions
    (table :: <table>,
     defn :: <define-bindings-macro-definition>,
     name :: <symbol>)
    => ();
  let name = name.sans-definer;
  if (name)
    unless (merge-category(table, name, <define-bindings-word-token>))
      error("Inconsistent syntax for %=", name);
    end;
  end;
end;

define method make-syntax-table-additions
    (table :: <table>,
     defn :: <define-bindings-macro-definition>,
     name :: <symbol>)
    => ();
  let name = name.sans-definer;
  if (name)
    table[name] := merge-category(table, name, <define-bindings-word-token>);
  end;
end;

define method check-syntax-table-additions
    (table :: <table>,
     defn :: <statement-macro-definition>,
     name :: <symbol>)
    => ();
  unless (merge-category(table, name, <begin-word-token>))
    error("Inconsistent syntax for %=", name);
  end;
end;

define method make-syntax-table-additions
    (table :: <table>,
     defn :: <statement-macro-definition>,
     name :: <symbol>)
    => ();
  table[name] := merge-category(table, name, <begin-word-token>);
end;



// process-top-level-form methods.
// 

define method process-top-level-form (form :: <macro-statement>) => ();
  let expansion = expand(form, #f);
  if (expansion)
    do(process-top-level-form, expansion);
  else
    error("Macro statement didn't expand?");
  end;
end;

define method process-top-level-form (form :: <define-parse>) => ();
  let expansion = expand(form, #f);
  if (expansion)
    do(process-top-level-form, expansion);
  else
    error("Use of define macro %s didn't expand?",
	  form.define-word.token-symbol);
  end;
end;

define method process-top-level-form (form :: <define-bindings-parse>) => ();
  let expansion = expand(form, #f);
  if (expansion)
    do(process-top-level-form, expansion);
  else
    error("Use of define bindings macro %s didn't expand?",
	  form.define-word.token-symbol);
  end;
end;


define method process-top-level-form
    (defmacro :: <define-statement-macro-parse>)
    => ();
  define-macro(defmacro, <statement-macro-definition>);
end;

define method process-top-level-form
    (defmacro :: <define-function-macro-parse>)
    => ();
  define-macro(defmacro, <function-macro-definition>);
end;

define method process-top-level-form
    (defmacro :: <define-define-macro-parse>)
    => ();
  fix-define-rules(defmacro);
  define-macro(defmacro, <define-macro-definition>);
end;

define method process-top-level-form
    (defmacro :: <define-define-bindings-macro-parse>)
    => ();
  fix-define-rules(defmacro);
  define-macro(defmacro, <define-bindings-macro-definition>);
end;



// define-macro & fix-define-rules

define method define-macro (defmacro :: <define-macro-parse>,
			    defn-class :: <class>)
    => ();
  let name = defmacro.defmacro-name;
  let defn = make(defn-class,
		  name: make(<basic-name>,
			     symbol: name.token-symbol,
			     module: *Current-Module*),
		  main-rule-set: defmacro.defmacro-main-rule-set,
		  auxiliary-rule-sets: defmacro.defmacro-auxiliary-rule-sets);
  find-wildcards(defn);
  find-end-variables(defn, #t);
  find-intermediate-words(defn);
  note-variable-definition(defn);
end;

define method fix-define-rules (defmacro :: <define-macro-parse>) => ();
  let name = sans-definer(defmacro.defmacro-name.token-symbol);
  unless (name)
    error("Name of define macro doesn't end with -definer: %s",
	  defmacro.defmacro-name.token-symbol);
  end;
  for (rule in defmacro.defmacro-main-rule-set)
    let pattern = rule.rule-pattern;
    let pattern-list = first(pattern.pattern-pieces);
    let pattern-sequence = first(pattern-list.pattern-list-pieces);
    if (instance?(pattern-sequence, <pattern-sequence>))
      block (return)
	let pieces = pattern-sequence.pattern-sequence-pieces;
	for (piece in pieces, index from 0)
	  if (instance?(piece, <identifier-pattern>)
		& piece.pattern-identifier.token-symbol == name)
	    rule.define-rule-modifiers-pattern
	      := make(<pattern-sequence>,
		      pieces: copy-sequence(pieces, end: index));
	    pattern-sequence.pattern-sequence-pieces
	      := copy-sequence(pieces, start: index + 1);
	    return();
	  end;
	end;
	error("Can't find macro name (%s) in rule.", name);
	#f;
      end;
    else
      error("Can't find macro name (%s) in rule.", name);
      #f;
    end;
  end;
end;



// expand methods.

define method expand (form :: <define-parse>,
		      lexenv :: union(<false>, <lexenv>))
    => results :: union(<simple-object-vector>, <false>);
  let name = form.define-word;
  let var = find-variable(name.token-module,
			  as(<symbol>,
			     concatenate(as(<string>, name.token-symbol),
					 "-definer")));
  unless (var)
    error("syntax table and variable table inconsistent.");
  end;
  let defn = var.variable-definition;
  unless (instance?(defn, <define-macro-definition>))
    error("syntax table and variable table inconsistent.");
  end;
  expand-macro-aux(form, form.define-fragment, defn);
end;

define method expand (form :: <define-bindings-parse>,
		      lexenv :: union(<false>, <lexenv>))
    => results :: union(<simple-object-vector>, <false>);
  let name = form.define-word;
  let var = find-variable(name.token-module,
			  as(<symbol>,
			     concatenate(as(<string>, name.token-symbol),
					 "-definer")));
  unless (var)
    error("syntax table and variable table inconsistent.");
  end;
  let defn = var.variable-definition;
  unless (instance?(defn, <define-bindings-macro-definition>))
    error("syntax table and variable table inconsistent.");
  end;
  let piece = make(<piece>,
		   source-location: form.source-location,
		   token: form.define-bindings);
  let fragment = make(<fragment>, piece: piece);
  expand-macro-aux(form, fragment, defn);
end;

define method expand (form :: <macro-statement>,
		      lexenv :: union(<false>, <lexenv>))
    => results :: union(<simple-object-vector>, <false>);
  let name = form.statement-begin-word;
  let var = find-variable(name.token-module, name.token-symbol);
  unless (var)
    error("syntax table and variable table inconsistent.");
  end;
  let defn = var.variable-definition;
  unless (instance?(defn, <statement-macro-definition>))
    error("syntax table and variable table inconsistent.");
  end;
  expand-macro-aux(form, form.statement-fragment, defn);
end;

define method expand (form :: <funcall>,
		      lexenv :: union(<false>, <lexenv>))
    => results :: union(<simple-object-vector>, <false>);
  let fun = form.funcall-function;
  if (instance?(fun, <varref>))
    let name = fun.varref-name;
    if (name.token-module)
      let var = find-variable(name.token-module, name.token-symbol);
      if (var)
	let defn = var.variable-definition;
	if (instance?(defn, <function-macro-definition>))
	  //
	  // Build a fragment that looks like the function call.
	  //
	  let fragment = make(<fragment>);
	  let args = form.funcall-arguments;
	  let need-comma? = #f;
	  for (arg in args,
	       possible-keyword? = even?(args.size) then ~possible-keyword?)
	    if (need-comma?)
	      postpend-piece!(fragment,
			      make(<piece>, token: make(<comma-token>)));
	    end;
	    if (possible-keyword?
		  & instance?(arg, <literal>)
		  & instance?(arg.lit-value, <symbol>))
	      postpend-piece!(fragment,
			      make(<piece>,
				   source-location: arg.source-location,
				   token: make(<keyword-token>,
					       literal: arg.lit-value)));
	      need-comma? := #f;
	    else
	      postpend-piece!(fragment,
			      make(<piece>,
				   source-location: arg.source-location,
				   token: make(<expression-token>,
					       expression: arg)));
	      need-comma? := #t;
	    end;
	  end;
	  //
	  // Now feed it to the pattern matcher
	  expand-macro-aux(form, fragment, defn);
	end;
      end;
    end;
  end;
end;

define method expand-macro-aux (form :: <constituent>,
				fragment :: <fragment>,
				defn :: <macro-definition>)
    => results :: union(<simple-object-vector>, <false>);
  let intermediate-words = defn.macro-intermediate-words;
  block (return)
    for (rule in defn.macro-main-rule-set)
      let results = match-rule(rule, form, fragment, intermediate-words);
      if (results)
	let replacement = expand-template(rule.rule-template, results, #f,
					  defn.macro-auxiliary-rule-sets,
					  intermediate-words,
					  make(<uniquifier>));
	let tokenizer = make(<fragment-tokenizer>,
			     fragment: replacement,
			     macro: form);
	return(parse-body(tokenizer));
      end;
    end;
    error("Syntax error in %=", form);
  end;
end;


define method find-intermediate-words (defn :: <macro-definition>)
    => res :: <simple-object-vector>;
  let aux-rule-sets = defn.macro-auxiliary-rule-sets;
  find-body-variable-rule-sets(aux-rule-sets);
  let results = #();
  for (rule in defn.macro-main-rule-set)
    results := find-intermediate-words-in(rule.rule-pattern, aux-rule-sets,
					  results);
  end;
  for (aux-rule-set in aux-rule-sets)
    for (rule in aux-rule-set.rule-set-rules)
      results := find-intermediate-words-in(rule.rule-pattern, aux-rule-sets,
					    results);
    end;
  end;
  defn.macro-intermediate-words := as(<simple-object-vector>, results);
end;

define method find-body-variable-rule-sets
    (aux-rule-sets :: <simple-object-vector>)
    => ();
  let again? = #t;
  while (again?)
    again? := #f;
    for (rule-set in aux-rule-sets)
      for (rule in rule-set.rule-set-rules,
	   until: rule-set.rule-set-body-variable?)
	let pieces = rule.rule-pattern.pattern-pieces;
	unless (empty?(pieces))
	  let pattern-list = pieces.last;
	  let list-tail = pattern-list.pattern-list-pieces.last;
	  if (instance?(list-tail, <pattern-sequence>)
		& body-variable?(list-tail.pattern-sequence-pieces.last,
				 aux-rule-sets))
	    rule-set.rule-set-body-variable? := #t;
	    again? := #t;
	  end;
	end;
      end;
    end;
  end;
end;

define method body-variable? (thing, aux-rule-sets :: <simple-object-vector>)
    => res :: <boolean>;
  #f;
end;

define method body-variable? (patvar :: <pattern-variable>,
			      aux-rule-sets :: <simple-object-vector>)
    => res :: <boolean>;
  let constraint = patvar.patvar-constraint;
  if (constraint == #"body" | constraint == #"case-body")
    #t;
  elseif (patvar.patvar-name)
    let aux-rule-set = find-aux-rule-set(patvar.patvar-name, aux-rule-sets);
    aux-rule-set & aux-rule-set.rule-set-body-variable?;
  else
    #f;
  end;
end;

define method find-aux-rule-set (name :: <symbol>,
				 aux-rule-sets :: <simple-object-vector>)
    => res :: union(<auxiliary-rule-set>, <false>);
  block (return)
    for (aux-rule-set in aux-rule-sets)
      if (aux-rule-set.rule-set-name == name)
	return(aux-rule-set);
      end;
    end;
    #f;
  end;
end;

define method find-intermediate-words-in (pattern :: <pattern>,
					  aux-rule-sets
					    :: <simple-object-vector>,
					  results :: <list>)
    => res :: <list>;
  for (piece in pattern.pattern-pieces)
    results := find-intermediate-words-in(piece, aux-rule-sets, results);
  end;
  results;
end;

define method find-intermediate-words-in (pattern :: <pattern-list>,
					  aux-rule-sets
					    :: <simple-object-vector>,
					  results :: <list>)
    => res :: <list>;
  for (piece in pattern.pattern-list-pieces)
    results := find-intermediate-words-in(piece, aux-rule-sets, results);
  end;
  results;
end;

define method find-intermediate-words-in (pattern :: <pattern-sequence>,
					  aux-rule-sets
					    :: <simple-object-vector>,
					  results :: <list>)
    => res :: <list>;
  for (piece in pattern.pattern-sequence-pieces,
       prev = #f then piece)
    if (body-variable?(prev, aux-rule-sets))
      results := find-intermediate-words-in(piece, aux-rule-sets, results);
    end;
  end;
  results;
end;

define method find-intermediate-words-in (pattern :: <simple-pattern>,
					  aux-rule-sets
					    :: <simple-object-vector>,
					  results :: <list>)
    => res :: <list>;
  results;
end;

define method find-intermediate-words-in (pattern :: <identifier-pattern>,
					  aux-rule-sets
					    :: <simple-object-vector>,
					  results :: <list>)
    => res :: <list>;
  let sym = pattern.pattern-identifier.token-symbol;
  if (member?(sym, results))
    results;
  else
    pair(sym, results);
  end;
end;

define method find-intermediate-words-in (pattern :: <pattern-variable>,
					  aux-rule-sets
					    :: <simple-object-vector>,
					  results :: <list>)
    => res :: <list>;
  if (pattern.patvar-name)
    let aux-rule-set = find-aux-rule-set(pattern.patvar-name, aux-rule-sets);
    if (aux-rule-set & ~aux-rule-set.rule-set-processed-intermediate-words?)
      aux-rule-set.rule-set-processed-intermediate-words? := #t;
      for (rule in aux-rule-set.rule-set-rules)
	results := find-intermediate-words-at-start(rule.rule-pattern,
						    aux-rule-sets,
						    results);
      end;
    end;
  end;
  results;
end;

define method find-intermediate-words-in (pattern :: <property-list-pattern>,
					  aux-rule-sets
					    :: <simple-object-vector>,
					  results :: <list>)
    => res :: <list>;
  results;
end;

define method find-intermediate-words-in (pattern :: <details-pattern>,
					  aux-rule-sets
					    :: <simple-object-vector>,
					  results :: <list>)
    => res :: <list>;
  find-intermediate-words-in(pattern.pattern-sub-pattern, aux-rule-sets,
			     results);
end;

define method find-intermediate-words-at-start (pattern :: <pattern>,
						aux-rule-sets
						  :: <simple-object-vector>,
						results :: <list>)
    => res :: <list>;
  if (empty?(pattern.pattern-pieces))
    results;
  else
    find-intermediate-words-at-start(pattern.pattern-pieces.first,
				     aux-rule-sets, results);
  end;
end;

define method find-intermediate-words-at-start (pattern :: <pattern-list>,
						aux-rule-sets
						  :: <simple-object-vector>,
						results :: <list>)
    => res :: <list>;
  if (empty?(pattern.pattern-list-pieces))
    results;
  else
    find-intermediate-words-at-start(pattern.pattern-list-pieces.first,
				     aux-rule-sets, results);
  end;
end;

define method find-intermediate-words-at-start (pattern :: <pattern-sequence>,
						aux-rule-sets
						  :: <simple-object-vector>,
						results :: <list>)
    => res :: <list>;
  if (empty?(pattern.pattern-sequence-pieces))
    results;
  else
    find-intermediate-words-in(pattern.pattern-sequence-pieces.first,
			       aux-rule-sets, results);
  end;
end;



define generic find-end-variables (within, at-end?) => ();

define method find-end-variables (defn :: <macro-definition>,
				  at-end? :: <boolean>)
    => ();
  for (rule in defn.macro-main-rule-set)
    find-end-variables(rule, at-end?);
  end;
  for (rule-set in defn.macro-auxiliary-rule-sets)
    for (rule in rule-set.rule-set-rules)
      find-end-variables(rule, at-end?);
    end;
  end;
end;

define method find-end-variables (rule :: <rule>, at-end? :: <boolean>) => ();
  find-end-variables(rule.rule-pattern, at-end?);
end;

define method find-end-variables (rule :: <abstract-define-rule>,
				  at-end? :: <boolean>,
				  #next next-method)
    => ();
  find-end-variables(rule.define-rule-modifiers-pattern, at-end?);
  next-method();
end;

define method find-end-variables (pattern :: <pattern>, at-end? :: <boolean>)
    => ();
  let pieces = pattern.pattern-pieces;
  let length = pieces.size;
  unless (length == 0)
    for (index from 0 below length - 1)
      find-end-variables(pieces[index], #f);
    finally
      find-end-variables(pieces[index], at-end?);
    end;
  end;
end;
				 
define method find-end-variables (pattern :: <pattern-list>,
				  at-end? :: <boolean>)
    => ();
  let pieces = pattern.pattern-list-pieces;
  let length = pieces.size;
  unless (length == 0)
    for (index from 0 below length - 1)
      find-end-variables(pieces[index], #f);
    finally
      find-end-variables(pieces[index], at-end?);
    end;
  end;
end;

define method find-end-variables (pattern :: <pattern-sequence>,
				  at-end? :: <boolean>)
    => ();
  let pieces = pattern.pattern-sequence-pieces;
  let length = pieces.size;
  unless (length == 0)
    for (index from 0 below length - 1)
      find-end-variables(pieces[index], #f);
    finally
      find-end-variables(pieces[index], at-end?);
    end;
  end;
end;

define method find-end-variables (pattern :: <property-list-pattern>,
				  at-end? :: <boolean>)
    => ();
end;

define method find-end-variables (pattern :: <simple-pattern>,
				  at-end? :: <boolean>)
    => ();
end;

define method find-end-variables (pattern :: <details-pattern>,
				  at-end? :: <boolean>)
    => ();
  find-end-variables(pattern.pattern-sub-pattern, #t);
end;

define method find-end-variables (pattern :: <pattern-variable>,
				  at-end? :: <boolean>)
    => ();
  pattern.patvar-at-end? := at-end?;
end;


define generic find-wildcards (within) => ();

define method find-wildcards (defn :: <macro-definition>) => ();
  do(find-wildcards, defn.macro-main-rule-set);
  for (rule-set in defn.macro-auxiliary-rule-sets)
    do(find-wildcards, rule-set.rule-set-rules);
  end;
end;

define method find-wildcards (rule :: <rule>) => ();
  find-wildcards(rule.rule-pattern);
end;

define method find-wildcards (rule :: <abstract-define-rule>,
			      #next next-method)
    => ();
  find-wildcards(rule.define-rule-modifiers-pattern);
  next-method();
end;

define method find-wildcards (pattern :: <pattern>) => ();
  for (piece in pattern.pattern-pieces)
    find-wildcards(piece);
  end;
end;
				 
define method find-wildcards (pattern :: <pattern-list>) => ();
  for (piece in pattern.pattern-list-pieces)
    find-wildcards(piece);
  end;
end;

define method find-wildcards (pattern :: <pattern-sequence>) => ();
  let wildcard = #f;
  for (piece in pattern.pattern-sequence-pieces)
    if (instance?(piece, <pattern-variable>) & ~piece.patvar-constraint)
      if (~piece.patvar-name | ~wildcard)
	wildcard := piece;
      end;
    elseif (instance?(piece, <details-pattern>))
      find-wildcards(piece.pattern-sub-pattern);
    end;
  end;
  if (wildcard)
    wildcard.patvar-wildcard? := #t;
  end;
end;

define method find-wildcards (pattern :: <property-list-pattern>) => ();
end;


// Stuff to figure extents of things.

define constant <type-part-type>
  = type-or(<left-paren-token>, <left-bracket-token>, <dot-token>,
	    <literal-token>, <string-token>, <true-token>, <false-token>,
	    <literal>, <expression>, <name-token>);
define constant <var-part-type>
  = type-or(<type-part-type>, <double-colon-token>);
define constant <expr-part-type>
  = type-or(<type-part-type>, <operator-token>, <abstract-literal-token>);
define constant <plist-part-type>
  = type-or(<expr-part-type>, <comma-token>, <property-set>);


define method guess-extent-of (fragment :: <fragment>, type :: <type>)
  let start = fragment.fragment-start;
  let stop = fragment.fragment-end;
  for (piece = start then piece.piece-next,
       until: (piece == stop | ~instance?(piece.piece-token, type)))
    if (instance?(piece, <balanced-piece>))
      piece := piece.piece-other;
    end;
  finally
    values(make(<fragment>, start: start, end: piece),
	   make(<fragment>, start: piece, end: stop));
  end;
end;


define method find-intermediate-word
    (fragment :: <fragment>,
     intermediate-words :: <simple-object-vector>)
  let start = fragment.fragment-start;
  let stop = fragment.fragment-end;
  for (piece = start then piece.piece-next,
       until: (piece == stop
		 | (instance?(piece.piece-token, <word-token>)
		      & member?(piece.piece-token.token-symbol,
				intermediate-words))))
    if (instance?(piece, <balanced-piece>))
      piece := piece.piece-other;
    end;
  finally
    values(make(<fragment>, start: start, end: piece),
	   make(<fragment>, start: piece, end: stop));
  end;
end;


define method trim-until-parsable (fragment :: <fragment>,
				   remaining :: <fragment>,
				   parser :: <function>)
    => (result :: <object>,
	fragment :: union(<fragment>, <false>),
	remaining :: union(<fragment>, <false>));
  block (return)
    while (#t)
      block ()
	let result = parser(make(<fragment-tokenizer>, fragment: fragment));
	return(result, fragment, remaining);
      exception <error>
	if (more?(fragment))
	  let prev-piece = fragment.fragment-end.piece-prev;
	  if (instance?(prev-piece, <balanced-piece>))
	    prev-piece := prev-piece.piece-other;
	  end;
	  remaining.fragment-start := prev-piece;
	  fragment.fragment-end := prev-piece;
	else
	  return(#f, #f, #f);
	end;
      end;
    end;
  end;
end;



// Utilities

define method wrap-with-token (expr :: <expression>) => res :: <token>;
  make(<expression-token>, expression: expr);
end;

define method wrap-with-token (propset :: <property-set>) => res :: <token>;
  make(<property-set-token>, property-set: propset);
end;




define method match-rule (rule :: <rule>,
			  form :: union(<constituent>, <false>),
			  fragment :: <fragment>,
			  intermediate-words :: <simple-object-vector>)
    => res :: union(<list>, <false>);
  match(rule.rule-pattern, fragment, intermediate-words,
	method () #f end,
	method (fragment, fail, results)
	  if (fragment.more?) fail() else results end;
	end,
	#());
end;

define method match-rule (rule :: <abstract-define-rule>,
			  form :: <defining-form>,
			  fragment :: <fragment>,
			  intermediate-words :: <simple-object-vector>)
    => res :: union(<list>, <false>);
  let modifiers-fragment = make(<fragment>);
  for (modifier in form.define-modifiers)
    postpend-piece!(modifiers-fragment,
		   make(<piece>, token: modifier));
  end;
  match(rule.define-rule-modifiers-pattern, modifiers-fragment, #[],
	method () #f end,
	method (modifiers-fragment, fail, results)
	  if (modifiers-fragment.more?)
	    fail();
	  else
	    match(rule.rule-pattern, fragment, intermediate-words,
		  method () #f end,
		  method (fragment, fail, results)
		    if (fragment.more?) fail() else results end;
		  end,
		  results);
	  end;
	end,
	#());
end;

define method more? (fragment :: <fragment>) => res :: <boolean>;
  ~(fragment.fragment-start == fragment.fragment-end);
end;

define method next-token (fragment :: <fragment>) => res;
  fragment.fragment-start.piece-token;
end;

define method consume-token (fragment :: <fragment>) => res :: <fragment>;
  make(<fragment>,
       start: fragment.fragment-start.piece-next,
       end: fragment.fragment-end);
end;

define method add-binding (name :: union(<false>, <symbol>),
			   fragment :: union(<fragment>, <vector>),
			   other-results :: <list>)
    => res :: <list>;
  pair(pair(name, fragment), other-results);
end;

define method add-binding (var :: <pattern-variable>, fragment :: <fragment>,
			   other-results :: <list>)
    => res :: <list>;
  add-binding(var.patvar-name, fragment, other-results);
end;

define method add-binding (var :: <pattern-keyword>, fragment :: <fragment>,
			   other-results :: <list>)
    => res :: <list>;
  add-binding(var.patkey-name, fragment, other-results);
end;



define method match (pattern :: <pattern>, fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>, continue :: <function>,
		     results :: <list>)
    => res :: union(<false>, <list>);
  match-pieces(pattern.pattern-pieces, <semicolon-token>, fragment,
	       intermediate-words, fail, continue, results);
end;

define method match (pattern :: <pattern-list>, fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>, continue :: <function>,
		     results :: <list>)
    => res :: union(<false>, <list>);
  match-pieces(pattern.pattern-list-pieces, <comma-token>, fragment,
	       intermediate-words, fail, continue, results);
end;

define method match (pattern :: <pattern-sequence>, fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>, continue :: <function>,
		     results :: <list>)
    => res :: union(<false>, <list>);
  match-pieces(pattern.pattern-sequence-pieces, #f, fragment,
	       intermediate-words, fail, continue, results);
end;


define method match-pieces (pieces, separator, fragment,
			    intermediate-words :: <simple-object-vector>,
			    fail, continue, results)
  local
    method match-pieces-aux (this-piece, fragment, fail, results)
      match(pieces[this-piece], fragment, intermediate-words, fail,
	    method (fragment, fail, results)
	      let next-piece = this-piece + 1;
	      if (next-piece == pieces.size)
		continue(fragment, fail, results);
	      elseif (~separator)
		match-pieces-aux(next-piece, fragment, fail, results);
	      elseif (fragment.more?
			& instance?(fragment.next-token, separator))
		match-pieces-aux(next-piece, consume-token(fragment),
				 fail, results);
	      elseif (next-piece == pieces.size - 1)
		match-empty(pieces[next-piece], fail,
			    method (results)
			      continue(fragment, fail, results);
			    end,
			    results);
	      else
		fail();
	      end;
	    end,
	    results);
    end;
  if (pieces.empty?)
    continue(fragment, fail, results);
  else
    match-pieces-aux(0, fragment, fail, results);
  end;
end;


define method match-empty (pattern :: <pattern>, fail, continue, results)
  match-empty-pieces (pattern.pattern-pieces, fail, continue, results);
end;

define method match-empty (pattern :: <pattern-list>, fail, continue, results)
  match-empty-pieces (pattern.pattern-list-pieces, fail, continue, results);
end;

define method match-empty (pattern :: <pattern-sequence>, fail, continue,
			   results)
  match-empty-pieces(pattern.pattern-sequence-pieces, fail, continue,
		     results);
end;

define method match-empty-pieces (pieces, fail, continue, results)
  if (pieces.size == 1)
    match-empty(pieces[0], fail, continue, results);
  else
    fail();
  end;
end;

define method match-empty (pattern :: <simple-pattern>, fail, continue,
			   results)
  fail();
end;

define method match-empty (pattern :: <pattern-variable>, fail, continue,
			   results)
  if (pattern.patvar-wildcard?
	| pattern.patvar-constraint == #"body"
	| pattern.patvar-constraint == #"case-body")
    continue(add-binding(pattern, make(<fragment>), results));
  else
    fail();
  end;
end;

define method match-empty (pattern :: <property-list-pattern>, fail, continue,
			   results)
  block (return)
    if (pattern.plistpat-rest)
      results := add-binding(pattern.plistpat-rest, make(<fragment>), results);
    end;
    if (pattern.plistpat-keys)
      for (key in pattern.plistpat-keys)
	let default = key.patkey-default;
	if (default)
	  let token = wrap-with-token(default);
	  let piece = make(<piece>, source-location: default.source-location,
			   token: token);
	  let fragment = make(<fragment>, piece: piece);
	  results := add-binding(key.patkey-name,
				 if (key.patkey-all?)
				   vector(fragment);
				 else
				   fragment;
				 end,
				 results);
	else
	  if (key.patkey-all?)
	    results := add-binding(pattern, #[], results);
	  else
	    return(fail());
	  end;
	end;
      end;
    end;
    continue(results);
  end;
end;

define method match (pattern :: <details-pattern>, fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>, continue :: <function>,
		     results :: <list>)
    => res :: union(<false>, <list>);
  if (fragment.more? & instance?(fragment.next-token, <left-paren-token>))
    let left = fragment.fragment-start;
    let right = left.piece-other;
    match(pattern.pattern-sub-pattern,
	  make(<fragment>, start: left.piece-next, end: right),
	  intermediate-words, fail,
	  method (remaining-guts-fragment, fail, results)
	    if (remaining-guts-fragment.more?)
	      fail();
	    else
	      continue(make(<fragment>,
			    start: right.piece-next,
			    end: fragment.fragment-end),
		       fail,
		       results);
	    end;
	  end,
	  results);
  else
    fail();
  end;
end;

define method match (pattern :: <identifier-pattern>, fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>, continue :: <function>,
		     results :: <list>)
    => res :: union(<false>, <list>);
  if (fragment.more?)
    let token = fragment.next-token;
    if (instance?(token, <identifier-token>)
	  & token.token-symbol == pattern.pattern-identifier.token-symbol)
      continue(consume-token(fragment), fail, results);
    else
      fail();
    end;
  else
    fail();
  end;
end;

define method match (pattern :: <variable-pattern>, fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>, continue :: <function>,
		     results :: <list>)
    => res :: union(<false>, <list>);
  if (fragment.more? & instance?(fragment.next-token, <name-token>))
    let results = add-binding(pattern.variable-name-pattern,
			      make(<fragment>, piece: fragment.fragment-start),
			      results);
    let fragment = consume-token(fragment);
    local method no-type ()
	    let object-token = make(<name-token>,
				    symbol: #"<object>",
				    module: $Dylan-Module);
	    let piece = make(<piece>, token: object-token);
	    continue(fragment, fail,
		     add-binding(pattern.variable-type-pattern,
				 make(<fragment>, piece: piece),
				 results));
	  end;
    if (fragment.more? & instance?(fragment.next-token, <double-colon-token>))
      let (type-guess, remaining-guess)
	= guess-extent-of(consume-token(fragment), <type-part-type>);
      let (type, type-fragment, remaining)
	= trim-until-parsable(type-guess, remaining-guess, parse-type);
      if (type)
	continue(remaining, fail,
		 add-binding(pattern.variable-type-pattern,
			     type-fragment, results));
      else
	no-type();
      end;
    else
      no-type();
    end;
  else
    fail();
  end;
end;

define method match (pattern :: <bound-variable-pattern>,
		     fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>,
		     continue :: <function>, results :: <list>)
    => res :: union(<false>, <list>);
  match(pattern.bound-variable-variable, fragment, intermediate-words, fail,
	method (fragment, fail, results)
	  if (fragment.more? & instance?(fragment.next-token, <equal-token>))
	    match(pattern.bound-variable-value, consume-token(fragment),
		  intermediate-words, fail, continue, results);
	  else
	    fail();
	  end;
	end,
	results);
end;

define method match (pattern :: <pattern-variable>, fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>, continue :: <function>,
		     results :: <list>)
    => res :: union(<false>, <list>);
  select (pattern.patvar-constraint)
    #"expr" =>
      let (expr-guess, remaining-guess)
	= guess-extent-of(fragment, <expr-part-type>);
      let (expr, expr-fragment, remaining)
	= trim-until-parsable(expr-guess, remaining-guess, parse-expression);
      if (expr)
	continue(remaining, fail,
		 add-binding(pattern, expr-fragment, results));
      else
	fail();
      end;
    #"var" =>
      let (var-guess, remaining-guess)
	= guess-extent-of(fragment, <var-part-type>);
      let (var, var-fragment, remaining)
	= trim-until-parsable(var-guess, remaining-guess, parse-variable);
      if (var)
	continue(remaining, fail, add-binding(pattern, var-fragment, results));
      else
	fail();
      end;
    #"name" =>
      if (fragment.more? & instance?(fragment.next-token, <name-token>))
	continue(consume-token(fragment), fail,
		 add-binding(pattern,
			     make(<fragment>, piece: fragment.fragment-start),
			     results));
      else
	fail();
      end;
    #"body" =>
      let (body-guess, remaining-guess)
	= find-intermediate-word(fragment, intermediate-words);
      match-variable(pattern, body-guess, remaining-guess, parse-body,
		     fail, continue, results);
    #"case-body" =>
      let (body-guess, remaining-guess)
	= find-intermediate-word(fragment, intermediate-words);
      match-variable(pattern, body-guess, remaining-guess, parse-case-body,
		     fail, continue, results);
    #f =>
      if (pattern.patvar-wildcard?)
	if (pattern.patvar-at-end?)
	  continue(make(<fragment>), fail,
		   add-binding(pattern, fragment, results));
	else
	  match-wildcard(pattern, fragment, fail, continue, results);
	end;
      elseif (fragment.more?)
	let start = fragment.fragment-start;
	if (instance?(start, <balanced-piece>))
	  let stop = start.piece-other.piece-next;
	  continue(make(<fragment>, start: stop, end: fragment.fragment-end),
		   fail,
		   add-binding(pattern,
			       make(<fragment>, start: start, end: stop),
			       results));
	else
	  continue(consume-token(fragment), fail,
		   add-binding(pattern,
			       make(<fragment>, piece: start),
			       results));
	end;
      else
	fail();
      end;
  end;
end;

define method match-variable (pattern :: <pattern-variable>,
			      fragment :: <fragment>, remaining :: <fragment>,
			      parser :: <function>, fail :: <function>,
			      continue :: <function>, results :: <list>)
    => res :: union(<false>, <list>);
  let fail = if (fragment.more?)
	       method ()
		 let prev-piece = fragment.fragment-end.piece-prev;
		 if (instance?(prev-piece, <balanced-piece>))
		   prev-piece := prev-piece.piece-other;
		 end;
		 match-variable(pattern,
				make(<fragment>,
				     start: fragment.fragment-start,
				     end: prev-piece),
				make(<fragment>,
				     start: prev-piece,
				     end: remaining.fragment-end),
				parser, fail, continue, results);
	       end;
	     else
	       fail;
	     end;
  block (return)
    block ()
      parser(make(<fragment-tokenizer>, fragment: fragment));
    exception <error>
      return(fail());
    end;
    continue(remaining, fail, add-binding(pattern, fragment, results));
  end;
end;

define method match-wildcard (pattern :: <pattern-variable>,
			      fragment :: <fragment>, fail :: <function>,
			      continue :: <function>, results :: <list>)
    => res :: union(<false>, <list>);
  local method match-wildcard-aux (split :: <piece>)
	  if (instance?(split, <balanced-piece>))
	    split := split.piece-other;
	  end;
	  let next = split.piece-next;
	  let matched-fragment
	    = make(<fragment>, start: fragment.fragment-start, end: next);
	  let remaining-fragment
	    = make(<fragment>,
		   start: next,
		   end: fragment.fragment-end);
	  continue(remaining-fragment,
		   if (next == fragment.fragment-end)
		     fail;
		   else
		     curry(match-wildcard-aux, next);
		   end,
		   add-binding(pattern, matched-fragment, results));
	end;
  continue(fragment,
	   if (fragment.more?)
	     curry(match-wildcard-aux, fragment.fragment-start);
	   else
	     fail;
	   end,
	   add-binding(pattern, make(<fragment>), results));
end;

define method match (pattern :: <otherwise-pattern>, fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>, continue :: <function>,
		     results :: <list>)
    => res :: union(<false>, <list>);
  if (fragment.more? & instance?(fragment.next-token, <otherwise-token>))
    let fragment = consume-token(fragment);
    if (fragment.more? & instance?(fragment.next-token, <arrow-token>))
      continue(consume-token(fragment), fail, results);
    else
      continue(fragment, fail, results);
    end;
  else
    fail();
  end;
end;

define method match (pattern :: <arrow-pattern>, fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>, continue :: <function>,
		     results :: <list>)
    => res :: union(<false>, <list>);
  if (fragment.more? & instance?(fragment.next-token, <arrow-token>))
    continue(consume-token(fragment), fail, results);
  else
    fail();
  end;
end;

define method match (pattern :: <property-list-pattern>,
		     fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>, continue :: <function>,
		     results :: <list>)
    => res :: union(<false>, <list>);
  block (return)
    let (plist-frag-guess, remaining-guess)
      = guess-extent-of(fragment, <plist-part-type>);
    let (plist, plist-frag, remaining)
      = trim-until-parsable(plist-frag-guess, remaining-guess,
			    parse-property-list);
    unless (plist)
      return(fail());
    end;
    if (pattern.plistpat-rest)
      results := add-binding(pattern.plistpat-rest, plist-frag, results);
    end;
    if (pattern.plistpat-keys)
      unless (pattern.plistpat-all-keys?)
	for (prop in plist)
	  block (okay)
	    for (key in pattern.plistpat-keys)
	      if (prop.prop-keyword.token-literal == key.patkey-name)
		okay();
	      end;
	    end;
	    return(fail());
	  end;
	end;
      end;
      for (key in pattern.plistpat-keys)
	if (key.patkey-all?)
	  let this-result = make(<stretchy-vector>);
	  for (prop in plist)
	    if (prop.prop-keyword.token-symbol == key.patkey-name)
	      let value = prop.prop-value;
	      let token = wrap-with-token(value);
	      let piece = make(<piece>, source-location: value.source-location,
			       token: token);
	      let frag = make(<fragment>, piece: piece);
	      add!(this-result, frag);
	    end;
	  end;
	  if (empty?(this-result) & key.patkey-default)
	    add!(this-result, key.patkey-default);
	  end;
	  results := add-binding(key.patkey-name,
				 as(<simple-object-vector>, this-result),
				 results);
	else
	  block (found-key)
	    for (prop in plist)
	      if (prop.prop-keyword.token-literal == key.patkey-name)
		let value = prop.prop-value;
		let token = wrap-with-token(value);
		let piece = make(<piece>,
				 source-location: value.source-location,
				 token: token);
		let frag = make(<fragment>, piece: piece);
		results := add-binding(key.patkey-name, frag, results);
		found-key();
	      end;
	    end;
	    let default = key.patkey-default;
	    if (default)
	      let token = wrap-with-token(default);
	      let piece = make(<piece>,
			       source-location: default.source-location,
			       token: token);
	      let frag = make(<fragment>, piece: piece);
	      results := add-binding(key.patkey-name, frag, results);
	    else
	      return(fail());
	    end;
	  end;
	end;
      end;
    end;
    continue(remaining, fail, results);
  end;
end;




define method expand-template (template :: <template>,
			       bindings :: <list>,
			       this-rule-set :: union(<symbol>, <false>),
			       aux-rule-sets :: <simple-object-vector>,
			       intermediate-words :: <simple-object-vector>,
			       uniquifier :: <uniquifier>)
  //
  // First, expand out any recursive rules.
  for (binding in bindings)
    let name = binding.head | this-rule-set;
    unless (name)
      error("Can't use ... in the main rule set.");
    end;
    let aux-rule-set = find-aux-rule-set(name, aux-rule-sets);
    if (aux-rule-set)
      block (return)
	for (rule in aux-rule-set.rule-set-rules)
	  let results = match-rule(rule, #f, binding.tail, intermediate-words);
	  if (results)
	    binding.tail := expand-template(rule.rule-template, results, name,
					    aux-rule-sets, intermediate-words,
					    uniquifier);
	    return();
	  end;
	end;
	error("None of the rules for auxiliary rule set %= matched.", name);
      end;
    end;
  end;
  //
  // Produce a new fragment built from the template.
  let result = make(<fragment>);
  for (piece in template.template-parts)
    expand-template-aux(piece, bindings, this-rule-set, uniquifier, result);
  end;
  result;
end;

define method expand-template-aux (template :: <pattern-variable-reference>,
				   bindings :: <list>,
				   this-rule-set :: union(<symbol>, <false>),
				   uniquifier :: <uniquifier>,
				   result :: <fragment>)
    => ();
  block (return)
    let name = template.patvarref-name | this-rule-set;
    for (binding in bindings)
      if ((binding.head | this-rule-set) == name)
	local
	  method append-frag (frag :: <fragment>)
	    let stop = frag.fragment-end;
	    for (piece = frag.fragment-start then piece.piece-next,
		 until: piece == stop)
	      postpend-piece!(result,
			      make(<piece>,
				   source-location: piece.source-location,
				   token: piece.piece-token));
	    end;
	  end,
	  method maybe-nuke-separator ()
	    let stop = result.fragment-end;
	    let tail = stop.piece-prev;
	    if (tail)
	      let tail-token = tail.piece-token;
	      if (instance?(tail-token, <binary-operator-token>)
		    | instance?(tail-token, <semicolon-token>)
		    | instance?(tail-token, <comma-token>))
		let prev = tail.piece-prev;
		stop.piece-prev := prev;
		prev.piece-next := stop;
	      end;
	    end;
	  end;
	if (instance?(binding.tail, <fragment>))
	  if (more?(binding.tail))
	    append-frag(binding.tail);
	  else
	    maybe-nuke-separator();
	  end;
	else
	  if (empty?(binding.tail))
	    maybe-nuke-separator();
	  else
	    let separator = template.patvarref-separator;
	    for (frag in binding.tail,
		 first? = #t then #f)
	      append-frag(frag);
	      if (separator & ~first?)
		postpend-piece!(result, make(<piece>, token: separator));
	      end;
	    end;
	  end;
	end;
	return();
      end;
    end;
    error("Unbound pattern variable: %=", name);
  end;
end;

define method expand-template-aux (template :: <paren-template>,
				   bindings :: <list>,
				   this-rule-set :: union(<symbol>, <false>),
				   uniquifier :: <uniquifier>,
				   result :: <fragment>)
    => ();
  let left-token = template.template-left-token;
  let left = make(<balanced-piece>,
		  source-location: left-token.source-location,
		  token: left-token);
  let right-token = template.template-right-token;
  let right = make(<balanced-piece>,
		   source-location: right-token.source-location,
		   token: right-token,
		   other: left);
  left.piece-other := right;
  postpend-piece!(result, left);
  for (sub-template in template.template-parts)
    expand-template-aux(sub-template, bindings, this-rule-set,
			uniquifier, result);
  end;
  postpend-piece!(result, right);
end;

define method expand-template-aux (template :: <token>,
				   bindings :: <list>,
				   this-rule-set :: union(<symbol>, <false>),
				   uniquifier :: <uniquifier>,
				   result :: <fragment>)
    => ();
  postpend-piece!(result,
		  make(<piece>,
		       source-location: template.source-location,
		       token: template));
end;

define method expand-template-aux (template :: <identifier-token>,
				   bindings :: <list>,
				   this-rule-set :: union(<symbol>, <false>),
				   uniquifier :: <uniquifier>,
				   result :: <fragment>)
    => ();
  let mod = template.token-module;
  postpend-piece!(result,
		  make(<piece>,
		       source-location: template.source-location,
		       token: make(if (mod)
				     element(mod.module-syntax-table,
					     template.token-symbol,
					     default: <name-token>);
				   else
				     object-class(template);
				   end,
				   source-location: template.source-location,
				   symbol: template.token-symbol,
				   module: mod,
				   uniquifier: uniquifier)));
end;

define method expand-template-aux (template :: <quoted-name-token>,
				   bindings :: <list>,
				   this-rule-set :: union(<symbol>, <false>),
				   uniquifier :: <uniquifier>,
				   result :: <fragment>)
    => ();
  postpend-piece!(result,
		  make(<piece>,
		       source-location: template.source-location,
		       token: make(<quoted-name-token>,
				   source-location: template.source-location,
				   symbol: template.token-symbol,
				   module: template.token-module,
				   uniquifier: uniquifier)));
end;

define method expand-template-aux (template :: <operator-token>,
				   bindings :: <list>,
				   this-rule-set :: union(<symbol>, <false>),
				   uniquifier :: <uniquifier>,
				   result :: <fragment>)
    => ();
  postpend-piece!(result,
		  make(<piece>,
		       source-location: template.source-location,
		       token: make(object-class(template),
				   source-location: template.source-location,
				   symbol: template.token-symbol,
				   module: template.token-module,
				   uniquifier: uniquifier)));
end;
