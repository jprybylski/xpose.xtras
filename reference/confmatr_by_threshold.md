# Calculate all binary confusion matrix indices

Calculate all binary confusion matrix indices

## Usage

``` r
confmatr_by_threshold(
  test_vec,
  true_vec,
  ...,
  threshold = 0.5,
  pos_val = 1,
  prepend = "",
  cols = dplyr::everything()
)
```

## Arguments

- test_vec:

  \<`double`\> Vector of probabilities

- true_vec:

  \<`integer`\> Vector true values

- ...:

  For future extension

- threshold:

  \<`double`\> Number that defines probability as positive.

- pos_val:

  \<`integer`\> Positive value in `true_vec`

- prepend:

  \<`character`\> Prepend column names with this to prevent name
  conflicts

## Value

dataframe of confusion matrix indices
