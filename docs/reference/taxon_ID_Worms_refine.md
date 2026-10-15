# eLTER taxon_id_worms_refine

This internal function provides a refining of the output of function
taxon_id_worms. It prints a list of possible matches and possibly
enables the user to interactively make his choice.

## Usage

``` r
taxon_id_worms_refine(
  input,
  taxaColumn = 1,
  interaction = FALSE,
  choiceNumber = NULL
)
```

## Arguments

- input:

  a `tibble`. The output of taxon_ID_Worms.

- taxaColumn:

  a `numeric`. The cardinal number of the column where species list is.
  Default is `1`.

- interaction:

  a `boolean`. When `TRUE` the user can interact with the choice of one
  of the proposed matching. Default is `FALSE`.

- choiceNumber:

  a `numeric`. The number of selected choice. Default is `NULL`. When
  `NULL` no choice is performed.

## Value

In interactive mode or when the `choiceNumber` is `NULL` the output of
the function is a `tibble` with the refinement of the input, else the
function simply returns the unchanged input.

## Author

Alessandro Oggioni, phD (2021) <oggioni.a@irea.cnr.it>

Paolo Tagliolato, PhD (2021) <tagliolato.p@irea.cnr.it>
