# If xpose is not loaded into namespace, notify the user but also
# set conflicted::conflict_prefer for functions overwritten here.
# conflicted is imported into tidyverse, so it should be considered
# usable given the existing constraints of xpose.

# Remove CRAN note on no visible binding for global variable
utils::globalVariables('.')
