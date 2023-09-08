  $ dune exec -- toml-check --error E-21 test3.toml
  Parsing "test3.toml"
  {
    "comments": {
      "a": 1,
      "x": 1,
      "y": 1
    },
    "extensions": {
      "w": 1,
      "x": 2,
      "z": {
        "b": 1
      }
    }
  }
  $ dune exec -- toml-check --error E-21 --to-toml test3.toml
  Parsing "test3.toml"
  
  # Extensions on operators
  
  [extensions]
  
  w = 1
  x = 2
  [extensions.z]
  b = 1
  
  # Comments
  
  [comments]
  
  # This is a comment on field x
  
  x = 1
  
  # these two comments are separated
  
  # by an empty line
  
  y = 1
  
  # and finally, this field has both a comment before
  
  a = 1 # and after the definition
  
