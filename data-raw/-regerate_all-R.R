# Regenerate all data. Should be run prior to build
devtools::load_all()
raw_data_scripts <- list.files(here::here("data-raw"), pattern = "^\\w", full.names = TRUE)


for (data_script in raw_data_scripts) source(data_script)

# We will do this twice, once after loading-all, so templating based on examples is followed
devtools::load_all()


raw_data_scripts <- list.files(here::here("data-raw"), pattern = "^\\w", full.names = TRUE)
for (data_script in raw_data_scripts) source(data_script)
