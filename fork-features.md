# Added Features

**This fork adds support for parsing:**

- `cgr_fallthru` statements inside `case` labels (and only there)
- `cgr_nullable` and `cgr_not_null` qualifiers inside pointer declarations 
  (right after the star `*`, before other qualifiers, such as `const`)
- `cgr_in`, `cgr_out` and `cgr_inout` right before pointer declarations inside 
  function signatures, and only inside function signatures (should they be 
  declarations or definitions). 
