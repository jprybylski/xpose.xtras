# Manual build reminders
# Send question to self, is version correct?
if (tolower(readline(
  prompt = sprintf("Is %s the right version number? [y/*] ", usethis:::proj_version())
  ))!="y") cli::cli_abort("Change version")
# Send question to self that "\()" and "|>" are not used (backwards compatability)
if (tolower(readline(
  prompt = "Have `|>` pipes and `\\()` lambdas been converted to older versions? [y/*] "
))!="y") cli::cli_abort("Ensure backwards compatability")
# Regenerate examples
source(here::here("data-raw","-regerate_all-R.R"))
devtools::load_all()
# Render readme
rmarkdown::render("README.Rmd")
# Run documentation
devtools::document()
# Spell Check
usethis::use_spell_check(error = TRUE)
devtools::spell_check()
# final checks (some of the above is re-run)
devtools::check()
# build
if (tolower(readline(
  prompt = "Really build? [y/*] "
))=="y") devtools::build(path = "builds")
