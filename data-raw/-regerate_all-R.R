# Regenerate all data. Should be run prior to build
devtools::load_all()
raw_data_scripts <- list.files(here::here("data-raw"), pattern = "^\\w", full.names = TRUE)

# nlmixr2 data is now generated on demand via nlmixr_example(); skip those scripts
raw_data_scripts <- raw_data_scripts[
  !grepl("nlmixr2|xpdb_nlmixr2", basename(raw_data_scripts))
]

for (data_script in raw_data_scripts) source(data_script)

# We will do this twice, once after loading-all, so templating based on examples is followed
devtools::load_all()

raw_data_scripts <- list.files(here::here("data-raw"), pattern = "^\\w", full.names = TRUE)
raw_data_scripts <- raw_data_scripts[
  !grepl("nlmixr2|xpdb_nlmixr2", basename(raw_data_scripts))
]
for (data_script in raw_data_scripts) source(data_script)
