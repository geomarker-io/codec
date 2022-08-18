# make_tdr_from_attr

    Code
      make_tdr_from_attr(d_attrs)
    Output
      $name
      [1] "example"
      
      $path
      [1] "d.csv"
      
      $title
      [1] "Example Data Set"
      
      $license
      [1] "MIT"
      
      $schema
      $schema$fields
      $schema$fields$id
      $schema$fields$id$name
      [1] "id"
      
      $schema$fields$id$type
      [1] "string"
      
      
      $schema$fields$date
      $schema$fields$date$name
      [1] "date"
      
      $schema$fields$date$type
      [1] "date"
      
      
      $schema$fields$measure
      $schema$fields$measure$name
      [1] "measure"
      
      $schema$fields$measure$type
      [1] "number"
      
      
      $schema$fields$rating
      $schema$fields$rating$name
      [1] "rating"
      
      $schema$fields$rating$type
      [1] "string"
      
      $schema$fields$rating$constraints
      $schema$fields$rating$constraints$enum
      [1] "good"   "better" "best"  
      
      
      
      $schema$fields$ranking
      $schema$fields$ranking$name
      [1] "ranking"
      
      $schema$fields$ranking$type
      [1] "integer"
      
      
      $schema$fields$impt
      $schema$fields$impt$name
      [1] "impt"
      
      $schema$fields$impt$type
      [1] "boolean"
      
      
      
      

# can read example tdr file

    Code
      read_tdr(test_path("tabular-data-resource.yaml"))
    Output
      $name
      [1] "example"
      
      $path
      [1] "d.csv"
      
      $title
      [1] "Example Data Set"
      
      $license
      [1] "MIT"
      
      $schema
      $schema$fields
      $schema$fields$id
      $schema$fields$id$name
      [1] "id"
      
      $schema$fields$id$type
      [1] "string"
      
      
      $schema$fields$date
      $schema$fields$date$name
      [1] "date"
      
      $schema$fields$date$type
      [1] "date"
      
      
      $schema$fields$measure
      $schema$fields$measure$name
      [1] "measure"
      
      $schema$fields$measure$type
      [1] "number"
      
      
      $schema$fields$rating
      $schema$fields$rating$constraints
      [1] "good"   "better" "best"  
      
      $schema$fields$rating$name
      [1] "rating"
      
      $schema$fields$rating$type
      [1] "string"
      
      
      $schema$fields$ranking
      $schema$fields$ranking$name
      [1] "ranking"
      
      $schema$fields$ranking$type
      [1] "integer"
      
      
      $schema$fields$impt
      $schema$fields$impt$name
      [1] "impt"
      
      $schema$fields$impt$type
      [1] "boolean"
      
      
      
      

