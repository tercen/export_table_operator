# Export Table

##### Description

This operator exports a crosstab projection as a CSV, TSV or XLSX file.

##### Usage

Input projection|.
---|---
`y-axis`        | Output table measurement values
`row`           | Output table rows 
`column`        | Output table columns

Input parameters|.
---|---
`format`            | Output file format: `CSV`, `TSV` or `XLSX`.
`collapse_cols`     | If `true`, write one header row with column factors joined by `_`. If `false` (default), write a multi-row crosstab header (one row per column factor plus a Y-axis row).
`collapse_rows`     | If `true`, join all row factors into a single column with values separated by `_`. If `false` (default), keep each row factor as its own column.
`filename`          | Custom file name. WORKFLOW, GROUP and DATASTEP will be replaced by the workflow, group (subworkflow) and data step names, respectively.
`export_to_project` | Whether to upload a copy of the generated file to the project folder. Only honoured when `collapse_cols = true`.
`na_encoding`       | How to encode missing values
`decimal_character` | How to encode the decimal symbol
`data_separator`    | Data separator for `CSV` format (default is ","). Ignored for `TSV` (tabs) and `XLSX`.

Output relations|.
---|---
`Table`        | A table containing the exported file (extension matches `format`).

