module: configs
synopsis: Configurable parameters. These are read from the project file if present.

/// Synopsis: The set of characters allowed for underline/overline.
/// TODO: Should be configurable.
define constant $ascii-line-chars = "=-:.~^_*+#";


/// Synopsis: The set of characters allowed for bullets.
/// TODO: Should be configurable.
define constant $bullet-chars = "-*+oO";


/// Synopsis: The set of quote characters.
/// TODO: Should be configurable.
define constant $open-quote-chars  = "'`\"";
define constant $close-quote-chars = "'`\""; //< [ditto $open-quote-chars]


/// Synopsis: The default specifiers for each quote type, outside of hyphenated
/// lists.
define constant $default-quote-spec =
      table(<string-table>, "'" => #[#"qv"], "`" => #[#"code"], "\"" => #[#"qq"]);

      
/// Synopsis: The default specifiers for each quote type in the phrase part of
/// a hyphenated list.
define constant $default-list-quote-spec =
      table(<string-table>, "'" => #[#"qv"], "`" => #[#"code"], "\"" => #[#"unq"]);


/// Synopsis: The underline/overline style of a section (as opposed to topic).
define constant $section-style =
      make(<topic-level-style>, char: '=', under: #f, mid: #t, over: #f);


/// Synopsis: The size of the tab character, in spaces. The markup parser
/// doesn't want tab characters.
define constant $tab-size = 8;
