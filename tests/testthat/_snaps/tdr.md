# can translate between tdr and attributes

    Code
      make_tdr_from_attr(d_attrs)
    Output
      $name
      [1] "example"
      
      $title
      [1] "Example Data Set"
      
      $path
      [1] "d.csv"
      
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
      [1] "c(\"good\", \"better\", \"best\")"
      
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
      
      
      
      

# get_descriptors() and get_schema()

    Code
      knitr::kable(get_descriptors(classy_attrs, codec = FALSE))
    Output
      
      
      |name        |value                                                                |
      |:-----------|:--------------------------------------------------------------------|
      |class       |tbl_df, tbl, data.frame                                              |
      |row.names   |1, 2, 3                                                              |
      |names       |id, date, measure, rating, ranking, awesomeness, datetime, timesince |
      |name        |classy                                                               |
      |title       |The Classiest Data Set                                               |
      |year        |2022                                                                 |
      |description |A toy data frame with many different column classes.                 |

---

    Code
      purrr::map(get_schema(classy_attrs, bind = FALSE), knitr::kable)
    Output
      $id
      
      
      |name |value  |
      |:----|:------|
      |name |id     |
      |type |string |
      
      $date
      
      
      |name |value |
      |:----|:-----|
      |name |date  |
      |type |date  |
      
      $measure
      
      
      |name |value   |
      |:----|:-------|
      |name |measure |
      |type |number  |
      
      $rating
      
      
      |name        |value                       |
      |:-----------|:---------------------------|
      |constraints |c("good", "better", "best") |
      |name        |rating                      |
      |type        |string                      |
      
      $ranking
      
      
      |name |value   |
      |:----|:-------|
      |name |ranking |
      |type |integer |
      
      $awesomeness
      
      
      |name |value       |
      |:----|:-----------|
      |name |awesomeness |
      |type |boolean     |
      
      $datetime
      
      
      |name |value    |
      |:----|:--------|
      |name |datetime |
      |type |datetime |
      
      $timesince
      
      
      |name |value     |
      |:----|:---------|
      |name |timesince |
      |type |number    |
      

---

    Code
      get_schema(classy_attrs)
    Output
      # A tibble: 8 x 4
        col         name        type     constraints                        
        <chr>       <chr>       <chr>    <chr>                              
      1 id          id          string    <NA>                              
      2 date        date        date      <NA>                              
      3 measure     measure     number    <NA>                              
      4 rating      rating      string   "c(\"good\", \"better\", \"best\")"
      5 ranking     ranking     integer   <NA>                              
      6 awesomeness awesomeness boolean   <NA>                              
      7 datetime    datetime    datetime  <NA>                              
      8 timesince   timesince   number    <NA>                              

# can create data resource metadata from attributes

    Code
      make_tdr_from_attr(my_mtcars)
    Output
      $name
      [1] "Motor Trend Cars"
      
      $schema
      $schema$fields
      $schema$fields$mpg
      $schema$fields$mpg$name
      [1] "MPG"
      
      $schema$fields$mpg$type
      [1] "number"
      
      $schema$fields$mpg$description
      [1] "Miles Per Gallon"
      
      
      $schema$fields$cyl
      $schema$fields$cyl$name
      [1] "cyl"
      
      $schema$fields$cyl$type
      [1] "number"
      
      
      $schema$fields$disp
      $schema$fields$disp$name
      [1] "disp"
      
      $schema$fields$disp$type
      [1] "number"
      
      
      $schema$fields$hp
      $schema$fields$hp$name
      [1] "hp"
      
      $schema$fields$hp$type
      [1] "number"
      
      
      $schema$fields$drat
      $schema$fields$drat$name
      [1] "drat"
      
      $schema$fields$drat$type
      [1] "number"
      
      
      $schema$fields$wt
      $schema$fields$wt$name
      [1] "wt"
      
      $schema$fields$wt$type
      [1] "number"
      
      
      $schema$fields$qsec
      $schema$fields$qsec$name
      [1] "qsec"
      
      $schema$fields$qsec$type
      [1] "number"
      
      
      $schema$fields$vs
      $schema$fields$vs$name
      [1] "vs"
      
      $schema$fields$vs$type
      [1] "number"
      
      
      $schema$fields$am
      $schema$fields$am$name
      [1] "am"
      
      $schema$fields$am$type
      [1] "number"
      
      
      $schema$fields$gear
      $schema$fields$gear$name
      [1] "gear"
      
      $schema$fields$gear$type
      [1] "number"
      
      
      $schema$fields$carb
      $schema$fields$carb$name
      [1] "carb"
      
      $schema$fields$carb$type
      [1] "number"
      
      
      
      

---

    Code
      make_tdr_from_attr(classy)
    Output
      $schema
      $schema$fields
      $schema$fields$id
      named list()
      
      $schema$fields$date
      named list()
      
      $schema$fields$measure
      named list()
      
      $schema$fields$rating
      named list()
      
      $schema$fields$ranking
      named list()
      
      $schema$fields$awesomeness
      named list()
      
      $schema$fields$datetime
      named list()
      
      $schema$fields$timesince
      named list()
      
      
      

# can read example tdr file

    Code
      read_tdr(test_path("tabular-data-resource.yaml"))
    Output
      $name
      [1] "example"
      
      $title
      [1] "Example Data Set"
      
      $path
      [1] "d.csv"
      
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
      
      
      
      

# read_tdr_csv

    Code
      get_schema(d_tdr)
    Output
      # A tibble: 6 x 4
        col     name    type    constraints       
        <chr>   <chr>   <chr>   <chr>             
      1 id      id      string  <NA>              
      2 date    date    date    <NA>              
      3 measure measure number  <NA>              
      4 rating  rating  string  good, better, best
      5 ranking ranking integer <NA>              
      6 impt    impt    boolean <NA>              

---

    Code
      get_descriptors(d_tdr)
    Output
      # A tibble: 4 x 2
        name    value           
        <chr>   <chr>           
      1 name    example         
      2 title   Example Data Set
      3 path    d.csv           
      4 license MIT             

