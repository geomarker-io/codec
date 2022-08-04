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
      purrr::map(get_schema(classy_attrs), knitr::kable)
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
      

