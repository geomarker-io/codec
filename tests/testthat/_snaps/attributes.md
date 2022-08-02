# get_descriptors() and get_schema()

    Code
      knitr::kable(get_descriptors(classy_attrs, codec = FALSE))
    Output
      
      
      |name        |value                                                                |
      |:-----------|:--------------------------------------------------------------------|
      |class       |tbl_df, tbl, data.frame                                              |
      |row.names   |1, 2, 3                                                              |
      |names       |id, date, measure, rating, ranking, awesomeness, datetime, timesince |
      |name        |The Classiest Data Set                                               |
      |year        |2022                                                                 |
      |description |A toy data frame with many different column classes.                 |

---

    Code
      purrr::map(get_schema(classy_attrs), knitr::kable)
    Output
      $id
      
      
      |name |value  |
      |:----|:------|
      |type |string |
      
      $date
      
      
      |name |value |
      |:----|:-----|
      |type |date  |
      
      $measure
      
      
      |name |value  |
      |:----|:------|
      |type |number |
      
      $rating
      
      
      |name        |value                       |
      |:-----------|:---------------------------|
      |constraints |c("good", "better", "best") |
      |type        |string                      |
      
      $ranking
      
      
      |name |value   |
      |:----|:-------|
      |type |integer |
      
      $awesomeness
      
      
      |name |value   |
      |:----|:-------|
      |type |boolean |
      
      $datetime
      
      
      |name |value    |
      |:----|:--------|
      |type |datetime |
      
      $timesince
      
      
      |name |value  |
      |:----|:------|
      |type |number |
      

