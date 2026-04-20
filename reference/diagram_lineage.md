# Visualize `xpose_set`

**\[experimental\]**

In its current state, this function is intended to provide a simple
visual representation of an `xpose_set`. Functionality and aesthetic
enhancements are expected in future releases.

## Usage

``` r
diagram_lineage(xpdb_s, ...)
```

## Arguments

- xpdb_s:

  \<`xpose_set`\> object

- ...:

  For later expansion. Will be ignored.

## Value

A DiagrammeR-compliant graph object.

## Examples

``` r
if (FALSE) { # \dontrun{
diagram_lineage(pheno_set) %>%
  DiagrammeR::render_graph(layout="tree")
} # }
```
