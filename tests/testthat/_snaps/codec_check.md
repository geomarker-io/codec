# codec_tdr

    Code
      codec_tdr()
    Output
      $property
                                                                                                          profile 
                                                                                        "`tabular-data-resource`" 
                                                                                                             name 
                                     "identifer composed of lower case alphanumeric characters, `_`, `-`, or `.`" 
                                                                                                             path 
                                                                         "relative file path or URL of data file" 
                                                                                                          version 
      "semantic [version](https://specs.frictionlessdata.io/patterns/#data-package-version) of the data resource" 
                                                                                                            title 
                                                                           "human-friendly title of the resource" 
                                                                                                         homepage 
                     "homepage on the web related to the data; ideally a code repository used to create the data" 
                                                                                                      description 
                                                                            "additional notes about the resource" 
                                                                                                           schema 
                                                                       "a list object containing items in schema" 
      
      $schema
                                                                                   fields 
      "a list object as long as the number of fields each containing the items in fields" 
                                                                            missingValues 
                       "the string values that should be considered missing observations" 
                                                                               primaryKey 
                             "a field or set of fields that uniquely identifies each row" 
                                                                              foreignKeys 
                              "a field or set of fields that connect to a separate table" 
      
      $fields
                                                                                            name 
                                                            "machine-friendly name of the field" 
                                                                                           title 
                                                              "human-friendly name of the field" 
                                                                                     description 
                                                          "any additional notes about the field" 
                                                                                            type 
                                                                "Frictionless type of the field" 
                                                                                     constraints 
      "Frictionless constraints, including `enum`, an array of possible values or factor levels" 
      

