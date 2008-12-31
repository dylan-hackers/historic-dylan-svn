module: configs
synopsis: Configurable parameters. These are read from config files if present.

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


/// Synopsis: The default specifiers for each quote type in normal markup.
define constant $default-markup-quote-specs =
      table(<string-table>, "'" => #[#"qv"], "`" => #[#"code"], "\"" => #[#"qq"]);

/// Synopsis: The default specifiers for each quote type in hyphenated and
/// phrase lists.
define constant $default-list-quote-specs =
      table(<string-table>, "'" => #[#"qv"], "`" => #[#"code"], "\"" => #[#"qq"]);

/// Synopsis: The default specifiers for each quote type in titles.
define constant $default-title-quote-specs =
      table(<string-table>, "'" => #[#"q"], "`" => #[#"code"], "\"" => #[#"qq"]);

/// Synopsis: The underline/overline style of a section (as opposed to topic).
define constant $section-style =
      make(<topic-level-style>, char: '-', under: #f, mid: #t, over: #f);


/// Synopsis: The size of the tab character, in spaces. The markup parser
/// doesn't want tab characters.
define constant $tab-size = 8;
