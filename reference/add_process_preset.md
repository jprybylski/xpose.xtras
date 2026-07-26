# Add, apply, list, amend or remove `xpdb` processing presets

**\[experimental\]**

A "process preset" is a one-sided formula (using `.x` for the incoming
`xpdb`, as in a `purrr`-style lambda) or a plain function that bundles
up a repeated processing pipeline – e.g. converting to `xp_xtras`,
dropping unused `ETA`s, assigning labels/levels – so it can be
re-applied with `process_preset()` instead of being retyped for every
model.

`add_process_preset()` stores a preset in the current session (by name,
or an auto-incrementing integer if `name` isn't given).
`process_preset()` applies a stored preset to an `xpdb`.
`print_process_preset()` lists stored presets. `remove_process_preset()`
deletes one. `amend_process_preset()` replaces an existing preset's
definition in place.

## Usage

``` r
add_process_preset(
  preset,
  name = NULL,
  overwrite = FALSE,
  persist = FALSE,
  ask = TRUE,
  profile = "project"
)

process_preset(xpdb, preset, ...)

print_process_preset(name = NULL)

remove_process_preset(name, persist = FALSE, ask = TRUE, profile = "project")

amend_process_preset(
  name,
  preset,
  persist = FALSE,
  ask = TRUE,
  profile = "project"
)
```

## Arguments

- preset:

  \<`formula`\> or \<`function`\> One-sided formula (e.g.
  `~ .x %>% as_xpdb_x() %>% set_var_type(na = any_of(paste0("ETA", 5:9)))`)
  or a function taking an `xpdb` as its first argument. For
  `process_preset()`, instead the `character` name or `numeric`
  (1-based) index of a previously-added preset.

- name:

  \<`character(1)`\> Name to store/look up/amend the preset under. For
  `add_process_preset()`, defaults to the next unused integer (as a
  string) if omitted.

- overwrite:

  \<`logical(1)`\> If a preset already exists under `name`, should it be
  replaced? (default: `FALSE`, i.e. error)

- persist:

  \<`logical(1)`\> Write the resulting set of presets out to a
  `.Rprofile` so they're available in future sessions too? See Details.
  (default: `FALSE`)

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

- xpdb:

  \<[`xpose_data`](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)\>
  or \<`xp_xtras`\> object to apply the preset to

- ...:

  For `process_preset()`, forwarded to the preset if it is a function
  (ignored by formula presets, which only ever see `xpdb`)

## Value

`add_process_preset()`/`amend_process_preset()`/
`remove_process_preset()` return the (`character(1)`) preset name,
invisibly. `process_preset()` returns the processed `xpdb`.
`print_process_preset()` returns the printed presets (a named list),
invisibly.

## Persistence and CRAN policy

By default, presets are session-only: they vanish when R restarts. Set
`persist = TRUE` to additionally write the current set of presets to a
`.Rprofile` so they're recreated automatically in future sessions.

Per CRAN policy, a package must not write to files outside
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) without the user's
explicit, interactive consent, and never as a side effect of a
non-interactive process (`R CMD check`, tests, vignette builds,
`Rscript`, ...). Accordingly, `persist = TRUE`:

- only ever writes when
  [`rlang::is_interactive()`](https://rlang.r-lib.org/reference/is_interactive.html)
  is `TRUE` – it errors otherwise, so it is always a no-op under
  `R CMD check`/`testthat`/ `knitr` – and

- asks for confirmation (via
  [`utils::askYesNo()`](https://rdrr.io/r/utils/askYesNo.html)) before
  writing, unless `ask = FALSE` is passed explicitly by the
  (already-interactive) caller.

The target file defaults to a **project**-scoped `.Rprofile` (in
[`getwd()`](https://rdrr.io/r/base/getwd.html)), which only affects R
sessions started in that directory; pass `profile = "user"` to instead
target the user-level profile (`Sys.getenv("R_PROFILE_USER")`, falling
back to `~/.Rprofile`), or any string to use it as a literal file path.
Presets are written as a single marked block (bounded by
`# >>> xpose.xtras process presets ... >>>` / `# <<< ... <<<`) so
re-syncing replaces the whole block rather than accumulating duplicate
calls, and the block is removed entirely once the last preset is
deleted. Persisted presets must be self-contained – since they're
recreated from deparsed source on each new session, they cannot depend
on transient local variables from the session that created them.

## Formula presets and `.x` vs `.`

Use `.x` (not a bare `.`) as the placeholder for the incoming xpdb in a
formula preset, e.g. `~ .x %>% as_xpdb_x() %>% set_var_type(...)`. A
*leading* `.` immediately before `%>%` is itself magrittr syntax for
building a reusable function (see `` ?magrittr::`%>%` ``): `~ . %>% f()`
would silently return a function instead of applying it, since the
formula's own `.` placeholder collides with magrittr's.

## See also

[xpose::xpose_data](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)

## Examples

``` r
add_process_preset(
  ~ .x %>% as_xpdb_x() %>% set_var_types(na = any_of(paste0("ETA", 5:9))),
  name = "drop_higher_etas"
)
#> Added `process_preset()`("drop_higher_etas")
print_process_preset()
#> • "drop_higher_etas": `~.x %>% as_xpdb_x() %>% set_var_types(na =
#>   any_of(paste0("ETA", 5:9)))`

xpdb_ex_pk_processed <- xpose::xpdb_ex_pk %>%
  process_preset("drop_higher_etas")

amend_process_preset(
  "drop_higher_etas",
  ~ .x %>% as_xpdb_x() %>% set_var_types(na = any_of(paste0("ETA", 7:9)))
)
#> Added `process_preset()`("drop_higher_etas")

remove_process_preset("drop_higher_etas")
#> Removed `process_preset()`("drop_higher_etas")
```
