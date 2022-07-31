# can create data resource metadata from attributes

    Code
      make_data_resource_from_attr(my_mtcars)
    Output
      $name
      [1] "Motor Trend Cars"
      
      $year
      [1] "1974"
      
      $schema
      $schema$fields
      $schema$fields$mpg
      $schema$fields$mpg$name
      [1] "MPG"
      
      $schema$fields$mpg$description
      [1] "Miles Per Gallon"
      
      $schema$fields$mpg$type
      [1] "number"
      
      
      $schema$fields$cyl
      $schema$fields$cyl$type
      [1] "number"
      
      
      $schema$fields$disp
      $schema$fields$disp$type
      [1] "number"
      
      
      $schema$fields$hp
      $schema$fields$hp$type
      [1] "number"
      
      
      $schema$fields$drat
      $schema$fields$drat$type
      [1] "number"
      
      
      $schema$fields$wt
      $schema$fields$wt$type
      [1] "number"
      
      
      $schema$fields$qsec
      $schema$fields$qsec$type
      [1] "number"
      
      
      $schema$fields$vs
      $schema$fields$vs$type
      [1] "number"
      
      
      $schema$fields$am
      $schema$fields$am$type
      [1] "number"
      
      
      $schema$fields$gear
      $schema$fields$gear$type
      [1] "number"
      
      
      $schema$fields$carb
      $schema$fields$carb$type
      [1] "number"
      
      
      
      

---

    Code
      make_data_resource_from_attr(classy)
    Output
      $schema
      $schema$fields
      $schema$fields$id
      NULL
      
      $schema$fields$date
      $schema$fields$date$class
      [1] "Date"
      
      
      $schema$fields$measure
      NULL
      
      $schema$fields$rating
      $schema$fields$rating$levels
      [1] "good"   "better" "best"  
      
      $schema$fields$rating$class
      [1] "factor"
      
      
      $schema$fields$ranking
      NULL
      
      $schema$fields$awesomeness
      NULL
      
      $schema$fields$datetime
      $schema$fields$datetime$class
      [1] "POSIXct" "POSIXt" 
      
      
      $schema$fields$timesince
      $schema$fields$timesince$class
      [1] "difftime"
      
      $schema$fields$timesince$units
      [1] "secs"
      
      
      
      

# get_descriptors works

    Code
      get_descriptors(my_mtcars, codec = FALSE)
    Output
      # A tibble: 5 x 2
        name      value                                                               
        <chr>     <chr>                                                               
      1 name      Motor Trend Cars                                                    
      2 year      1974                                                                
      3 names     mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb              
      4 row.names Mazda RX4, Mazda RX4 Wag, Datsun 710, Hornet 4 Drive, Hornet Sporta~
      5 class     data.frame                                                          

