# Write current process presets out to a `.Rprofile`

Syncs the in-session
[`add_process_preset()`](https://jprybylski.github.io/xpose.xtras/reference/add_process_preset.md)
registry out to a `.Rprofile` file, so it's available again in future
sessions. This is what `persist = TRUE` on
[`add_process_preset()`](https://jprybylski.github.io/xpose.xtras/reference/add_process_preset.md)/[`remove_process_preset()`](https://jprybylski.github.io/xpose.xtras/reference/add_process_preset.md)/
[`amend_process_preset()`](https://jprybylski.github.io/xpose.xtras/reference/add_process_preset.md)
calls internally; call it directly to persist several in-session changes
(each made with `persist = FALSE`) in one write/confirmation instead of
one per change. See the CRAN-policy notes in
[`add_process_preset()`](https://jprybylski.github.io/xpose.xtras/reference/add_process_preset.md)
– in particular, this always errors instead of writing anything when
[`rlang::is_interactive()`](https://rlang.r-lib.org/reference/is_interactive.html)
is `FALSE`.

## Usage

``` r
persist_process_presets(ask = TRUE, profile = "project")
```

## Arguments

- ask:

  \<`logical(1)`\> When `persist = TRUE`, ask for interactive
  confirmation before writing? (default: `TRUE`; only ever consulted
  when
  [`rlang::is_interactive()`](https://rlang.r-lib.org/reference/is_interactive.html)
  is already `TRUE`)

- profile:

  \<`character(1)`\> Where to persist to when `persist = TRUE`:
  `"project"` (default, `.Rprofile` in
  [`getwd()`](https://rdrr.io/r/base/getwd.html)), `"user"` (the
  user-level profile), or a literal file path.

## Value

`TRUE` if the file was written, `FALSE` if declined (invisibly)

## Examples

``` r
if (FALSE) { # \dontrun{
add_process_preset(~ .x %>% as_xpdb_x(), name = "convert", persist = FALSE)
persist_process_presets()
} # }
```
