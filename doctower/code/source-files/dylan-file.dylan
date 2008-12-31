module: source-files
synopsis: Gets doc markup out of doc files or comment blocks.


define method topics-from-dylan-file (file :: <file-stream>)
=> (topics :: <sequence>)
   // Read source code.
   let text = make(<canonical-text-stream>, inner-stream: file);
   let token = block ()
                  parse-dylan-file(text, file.stream-locator);
               cleanup
                  text.close;
               end block;
   
   // Generate implicit topics and sections.
   // Associate comments to topics and sections.
   // Read and parse comments.
   // Generate explicit topics, sections, and placeholders.
   #()
end method;


/// Synopsis: Records what is known a priori about a block of markup.
/// Discussion: Used to resolve API references and to construct a topic for the
/// block.
define class <topic-context> (<object>)
   /// Synopsis: The default parent topic of all topics in this block.
   ///
   /// Discussion: The default parent for a comment block is a topic in the
   /// reference section of the documentation for the file's library and
   /// module. For a file, there is no parent topic. The result either stands
   /// alone at the top level, or is placed in a hierarchy based on other
   /// directives or the TOC file.
   slot default-parent :: false-or(type-union(<topic>, <section>)) = #f;
   
   /// Synopsis: The skeleton of the first or anonymous topic within the block.
   ///
   /// Discussion: A comment block or file has a main topic. Any topics within
   /// the block after the first are supplementary; they are either sibling
   /// topics to the main one, or subtopics of the main one. The main topic
   /// for an API element can be partially filled out by scanning the source
   /// code; the comment's main topic puts meat on that skeleton.
   ///
   /// The main topic of a comment block or file must be implied, in which case
   /// there will be a skeleton topic, or else must be specified in the comment
   /// block or file itself. In the latter case, this field is #f.
   slot topic :: false-or(<topic>) = #f;
end class;

