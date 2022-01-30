An implementation of two papers of Krishnaswami and Yallop.

1. A Typed, Algebraic Approach to Parsing
  - This shows how to define parser combinators that can be typechecked to ensure they run in linear time with no backtracking and single token lookahead.

2. Fusing Lexing and Parsing
  - This shows how to _fuse_ lexers and parsers defined in the above style. This avoids the need for tokens to be allocated, so the result is several times faster than standard approaches.

Both papers explicitly support _staging_ (ie code generation), for performance.
