module: turboblog
author: turbo24prg

define method add-category (blog :: <blog>, category :: <category>)
 => (blog :: <blog>);
  blog.categories := add-new!(blog.categories, category, test: \=);
  blog;
end;

define method \= (category1 :: <category>, category2 :: <category>)
 => (equal? :: <boolean>);
  category1.term = category2.term
end;
