# Binary check if LHS is parent of LHS

Binary check if LHS is parent of LHS

## Usage

``` r
possible_parent %p% possible_child
```

## Arguments

- possible_parent:

  \<`xpose_set_item`\> object suspected as parent to ...

- possible_child:

  ... \<`xpose_set_item`\> object suspected child

## Value

`<logical>` `TRUE` if LHS is parent of RHS

## Examples

``` r
# Detect direct parent
pheno_set$run6 %p% pheno_set$run7
#> [1] TRUE

# Detect non-parentage (does not try to "flip" parentage)
pheno_set$run6 %p% pheno_set$run5
#> [1] FALSE

# Does not detect grand-parentage
pheno_set$run6 %p% pheno_set$run13
#> [1] FALSE
```
