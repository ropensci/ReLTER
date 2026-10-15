# Provide a taxon ID [LSID](https://en.wikipedia.org/wiki/LSID) to a taxon list.

**\[stable\]** This function provide a taxon ID, usually a
[LSID](https://en.wikipedia.org/wiki/LSID), from a taxonomic list. The
input of the function is a csv file with a list of taxa. The Taxon ID
provided by this function is currently taken from Pan-European Species
directories Infrastructure - [PESI](http://eu-nomen.eu/pesi/). This
function takes advantage of taxize's `eubon_search` function
<https://docs.ropensci.org/taxize/> and the [PESI
RestAPI](http://www.eu-nomen.eu/portal/rest/).

## Usage

``` r
taxon_id_pesi(table, taxaColumn)
```

## Arguments

- table:

  A `data.frame` containing column with a taxa (e.g. Sphaerosoma
  seidlitzi, Malthinus, etc.).

- taxaColumn:

  A `numeric` that identify the column containing taxa value.

## Value

The output of the function is a `tibble` containing all the columns
provided as input and new columns as: 'canonicalName', 'authorship',
'synonyms', 'LSID', 'url', 'accordingTo', 'checkStatus' gathered from
PESI.

An example to export dataset obtained by this function is: datasetMerged
\<- dplyr::bind_rows(table) write.csv( datasetMerged, "table.csv",
row.names = FALSE, fileEncoding = "UTF-8" )

Someone could have problems of characters encoding when CSV file is
written. To resolve we suggest two different solutions:

Solution 1 -

1.  Open the CSV in Notepad.

2.  Click “File” and “Save As”.

3.  In the new popup that displays, select “ANSI” from the “Encoding”
    field.

4.  Click “Save”.

5.  Now, you should be able to open the file in Excel and display the
    characters correctly.

Solution 2 -

1.  Open Excel

2.  Click “File” and “New”

3.  Click on the “Data” tab

4.  Click “From Text” and select the CSV file

5.  Select “Delimited”

6.  For “File origin”, select “65001 : Unicode (UTF-8)”

7.  Click “Next”

8.  Select “Comma”

9.  Click “Finish”

10. Excel should now show you the CSV file and display the characters
    correctly.

## See also

[`taxize::eubon_search()`](https://docs.ropensci.org/taxize/reference/eubon_search.html)

## Author

Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
insects <- data.frame(
   taxonID = c(1, 2, 3, 4, 5, 6),
   family = c(
     "Alexiidae", "Anthicidae",
     "Anthribidae", "Anthribidae",
     "Biphyllidae", "Brentidae"
   ),
   scientificName = c(
     "Sphaerosoma seidlitzi", "Endomia tenuicollis tenuicollis",
     "Anthribus fasciatus", "Phaenotherion fasciculatum fasciculatum",
     "Diplocoelus fagi", "Holotrichapion (Apiops) pisi"
   )
)

output <- taxon_id_pesi(
  table = insects,
  taxaColumn = 3
)

# The annotated URIs of columns label are achieved by:
attributes(output)$uri

} # }
```
