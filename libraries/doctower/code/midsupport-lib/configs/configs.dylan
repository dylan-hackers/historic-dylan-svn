module: configs
synopsis: Configurable parameters. These are read from config files if present.


define variable *api-list-file* :: false-or(<file-locator>) = #f;
define variable *generated-topics-directory* :: false-or(<directory-locator>) = #f;
define variable *template-directory* :: <directory-locator>
      = as(<directory-locator>, "../defaults");

define variable *topic-file-extension* :: <string> = "txt";
define variable *contents-file-extension* :: <string> = "toc";
define variable *config-file-extension* :: <string> = "cfg";

define variable *package-title* :: <string> = "Untitled";
define variable *output-directory* :: <directory-locator>
      = as(<directory-locator>, "./doc");
define variable *output-types* :: <sequence> = #[ #"html" ];


/// Synopsis: The set of characters allowed for underline/overline.
/// TODO: Should be configurable.
define constant $ascii-line-chars = "=-:.~^_*+#";


/// Synopsis: The set of characters allowed for bullets.
/// TODO: Should be configurable.
define constant $bullet-chars = "-*+oO";


/// Synopsis: The set of quote characters.
/// TODO: Should be configurable.
define constant $open-quote-chars  = "'\"`";
define constant $close-quote-chars = "'\"`"; //< [ditto $open-quote-chars]


/// Synopsis: The default specifiers for each quote type in normal markup.
define constant $default-markup-quote-specs =
      table(<string-table>,
            "'" => #[#"api", #"qv"],
            "\"" => #[#"qq", #"qv"],
            "`" => #[#"code"]
      );

/// Synopsis: The default specifiers for each quote type in hyphenated and
/// phrase lists.
define constant $default-list-quote-specs =
      table(<string-table>,
            "'" => #[#"api"],
            "\"" => #[#"qq"],
            "`" => #[#"code"]
      );

/// Synopsis: The default specifiers for each quote type in titles.
/// TODO: Should maybe be the same as $default-markup-quote-specs without qv or vi.
define constant $default-title-quote-specs =
      table(<string-table>,
            "'" => #[#"api"],
            "\"" => #[#"qq"],
            "`" => #[#"code"]
      );


/// Synopsis: The size of the tab character, in spaces. The parsers do not want
/// tab characters.
define constant $tab-size = 8;
