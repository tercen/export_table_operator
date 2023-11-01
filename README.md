# Export CSV

##### Description

This operator exports a crosstab projection into a CSV file.

##### Usage

Input projection|.
---|---
`y-axis`        | Output table measurement values
`row`           | Output table rows 
`column`        | Output table columns

Input parameters|.
---|---
`na_encoding`        | How to encode missing values
`filename`        | Custom file name
`time_stamp`        | Whether to add a time stamp to the file name
`decimal_character`        | How to encode the decimal symbol
`data_separator`        | How to encode data separator (default is ",")

Output relations|.
---|---
`Table`        | A table containing the exported CSV file.

