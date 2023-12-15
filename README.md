# Export Table

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
`format`            | Desired file format
`filename`          | Custom file name. WORKFLOW, GROUP and DATASTEP will be replaced by the workflow, group (subworkflow) and data step names, respectively.
`export_to_project` | Whether to upload a copy of the generated file to the project folder.
`na_encoding`       | How to encode missing values
`time_stamp`        | Whether to add a time stamp to the file name
`decimal_character` | How to encode the decimal symbol
`data_separator`    | How to encode data separator (default is ",")

Output relations|.
---|---
`Table`        | A table containing the exported CSV file.

