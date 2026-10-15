# Enrich and certify a list of species names by comparing with [WoRMS](https://www.marinespecies.org).

**\[stable\]** This function provide tibble object with all the columns
of input table of taxa plus new columns such as valid_name,
valid_authority, valid_AphiaID, status, synonyms, LSID, url, matchType,
nOfWormsRecords, wormsRecords obtained from Word Register of Marine
Species [WoRMS rest API](http://www.marinespecies.org/rest/).

## Usage

``` r
taxon_id_worms(input, taxaColumn = 1, verbose = TRUE, refine = FALSE)
```

## Arguments

- input:

  A `tibble`. The table that contain the species names list to be
  checked.

- taxaColumn:

  A `numeric`. The cardinal number of the column where species list is.
  Default is `1`.

- verbose:

  A `logical`. Whit this selection, the function returns a message with
  number of record(s) that don't match with any Worms names and the
  number of record(s) that match with more that one Worms name. Default
  is `TRUE`.

- refine:

  A `logical`. With this selection, the function allows to refine the
  result(s) that match with more Worms records. By a interactive use of
  the terminal, the user can chose the result. Default is `FALSE`.

## Value

The output of the function is a `tibble` with the columns provided and
new columns such as: valid_name, valid_authority, valid_AphiaID, status,
synonyms, LSID, url, matchType, nOfWormsRecords, wormsRecords obtained
by [Worms rest API](http://www.marinespecies.org/rest/). The function
also return, if verbose is TRUE, the list of records that don't match
with Worms name species.

Most of the labels of the columns are the terms of [Darwin Core
terms](https://dwc.tdwg.org/terms/#record-level). The columns labels are
annotate with the link (URI) of the [Darwin Core
terms](https://dwc.tdwg.org/terms/#record-level) as attributes of the
`tibble`.

## See also

[`worrms::wm_records_names()`](https://docs.ropensci.org/worrms/reference/wm_records_names.html)

## Author

Alessandro Oggioni, phD (2021) <oggioni.a@irea.cnr.it>

Paolo Tagliolato, phD (2021) <tagliolato.p@irea.cnr.it>

## Examples

``` r
phytoplankton <- tibble::tibble(
   ID = c(1, 2, 3, 4, 5, 6, 7),
   species = c(
   "Asterionella formosa", "Chrysococcus sp.",
   "Cryptomonas rostrata", "Dinobryon divergens",
   "Mallomonas akrokomos", "Melosira varians",
   "Cryptomonas rostrata"
 )
)
table <- taxon_id_worms(
  input = phytoplankton,
  taxaColumn = 2,
  verbose = TRUE,
  refine = TRUE
)
#> 
#> After the finding operation the number of record(s) that don't match
#> with any Worms names are:
#> *--- 0 on 7 examined ---*
#> please verify the species name provided and run again this
#> function. The record(s) that match with more that one Worms name are:
#> *--- 7 on 7 examined ---*
#> please use the function taxon_id_worms_refine for specify wich
#> is the exact corrispondence with your given species name.
#> This is the taxa name provided by you:
#> Cryptomonas rostrata
#> Worms don't contain a unique records that match with this name.
#> The Worms records most similar are:
#> 
#> 1:  Cryptomonas rostrata (Skuja, 1948)
#>     Worms status: accepted
#>     Unaccept reason: NA
#>     Match type: exact
#>     Modified: 2015-06-26T12:14:04.327Z
#> 2:  Cryptomonas rostrata (O.V.Troitzkaja, 1922)
#>     Worms status: unaccepted
#>     Unaccept reason: synonym
#>     Match type: exact
#>     Modified: 2021-08-27T06:27:33.180Z
#> 
#> ----
#> Please select the record that you think
#> most similar to the taxa name that you have provided.
#> Insert the number of record:
#> This is the taxa name provided by you:
#> Cryptomonas rostrata
#> Worms don't contain a unique records that match with this name.
#> The Worms records most similar are:
#> 
#> 1:  Cryptomonas rostrata (Skuja, 1948)
#>     Worms status: accepted
#>     Unaccept reason: NA
#>     Match type: exact
#>     Modified: 2015-06-26T12:14:04.327Z
#> 2:  Cryptomonas rostrata (O.V.Troitzkaja, 1922)
#>     Worms status: unaccepted
#>     Unaccept reason: synonym
#>     Match type: exact
#>     Modified: 2021-08-27T06:27:33.180Z
#> 
#> ----
#> Please select the record that you think
#> most similar to the taxa name that you have provided.
#> Insert the number of record:
table
#> # A tibble: 7 × 17
#>      ID originalNameUsage    scientificName       scientificNameAuthor…¹ taxonID
#>   <dbl> <chr>                <chr>                <chr>                    <int>
#> 1     1 Asterionella formosa Asterionella formosa Hassall, 1850           148954
#> 2     2 Chrysococcus sp.     NA                   NA                          NA
#> 3     3 Cryptomonas rostrata NA                   NA                          NA
#> 4     4 Dinobryon divergens  Dinobryon divergens  O.E.Imhof, 1887         157248
#> 5     5 Mallomonas akrokomos Mallomonas akrokomos Ruttner, 1913           249722
#> 6     6 Melosira varians     Melosira varians     C.Agardh, 1827          149043
#> 7     7 Cryptomonas rostrata NA                   NA                          NA
#> # ℹ abbreviated name: ¹​scientificNameAuthorship
#> # ℹ 12 more variables: taxonomicStatus <chr>, synonyms <chr>, taxonRank <chr>,
#> #   kingdom <chr>, phylum <chr>, class <chr>, order <chr>, family <chr>,
#> #   genus <chr>, scientificNameID <chr>, nOfWormsResults <dbl>,
#> #   wormsRecords <list>

# The annotated URIs of columns label are achieved by:
attributes(table)$uri
#>  [1] ""                                                     
#>  [2] "http://rs.tdwg.org/dwc/terms/originalNameUsage"       
#>  [3] "http://rs.tdwg.org/dwc/terms/scientificName"          
#>  [4] "http://rs.tdwg.org/dwc/terms/scientificNameAuthorship"
#>  [5] "http://rs.tdwg.org/dwc/terms/taxonID"                 
#>  [6] "http://rs.tdwg.org/dwc/terms/taxonomicStatus"         
#>  [7] ""                                                     
#>  [8] "http://rs.tdwg.org/dwc/terms/taxonRank"               
#>  [9] "http://rs.tdwg.org/dwc/terms/kingdom"                 
#> [10] "http://rs.tdwg.org/dwc/terms/phylum"                  
#> [11] "http://rs.tdwg.org/dwc/terms/class"                   
#> [12] "http://rs.tdwg.org/dwc/terms/order"                   
#> [13] "http://rs.tdwg.org/dwc/terms/family"                  
#> [14] "http://rs.tdwg.org/dwc/terms/genus"                   
#> [15] "http://rs.tdwg.org/dwc/terms/scientificNameID"        
#> [16] ""                                                     
#> [17] ""                                                     
```
