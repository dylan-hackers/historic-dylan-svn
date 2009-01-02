module: topic-resolver
synopsis: This file merges API topic fragments.

/// Synopsis: Combine a series of partial explicit and implicit topics into one.
define method merge-topics (topics :: <sequence>) => (topic :: <topic>)
end method;
