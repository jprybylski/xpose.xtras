# Place .problem, .subprob and .method into environment consistently

Since this is a common need, it is being functionalized to ensure
consistency.

## Usage

``` r
fill_prob_subprob_method(
  xpdb,
  .problem,
  .subprob,
  .method,
  envir = parent.frame(),
  for_summary = FALSE
)
```

## Arguments

- xpdb:

  \<`xpose_data`\> or related object

- .problem:

  `NULL` or missing

- .subprob:

  `NULL` or missing

- .method:

  `NULL` or missing

- envir:

  \<`environment`\> in which to assign the problem info.

- for_summary:

  \<`logical`\> If used for summary functions, subprob needs to be
  adjusted for zero-indexing
