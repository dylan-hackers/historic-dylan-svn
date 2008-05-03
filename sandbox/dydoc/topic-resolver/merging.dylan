module: topic-resolver
synopsis: This file merges implicit and explicit topics.

/// Synopsis: Combine a series of partial explicit and implicit topics into one.
define method merge-topics (topics :: <sequence>) => (topic :: <topic>)
end method;
