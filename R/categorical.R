#######
# Categorical dv plots
#######

# Pearson residuals can already be treated as res in res plots
# Just need to add documentation in that regard

# sum of n in cat for each cat faceted
#catdv_vs_idv <- function() {}
# Distribution of res faceted over each category
#res_vs_catdv <- function() {}


#######
# Categorical pred plots
#######

# Plots where likelihood of values is x axis and
# violins of those values are on Y. For binary,
# this forms a nice logistic-like smooth curve.
# For mulitple levels, need to facet by levels,
# So you have prob at level 0, prob at greater,
# and numb at zero number at other (and same
# for higher cutpoints)

set_dv_probs <- function(
    xpdb, 
    .problem = NULL, 
    ..., 
    .dv_var = NULL, # default is first DV var
    .missing = "Other", 
    .handle_missing = c("quiet","warn","error")
    ) {
  # Like set_var_levels, but it is expected that
  # variables listed as dvprobs, these represent
  # the probabilities of certain DV values.
  # Can be as simple as c(0~P0) or complex as
  # c( ge(2)~P23  ). Requires function parsing.
  # No convenience functions like lvl_Px(),
  # as this should be more straightforward than leveling (which can require long strings)
  # 
  
  #### Top part is similar to set_var_levels
  # Basic check
  if (!check_xpdb_x(xpdb)) rlang::abort("xp_xtras object required.")
  xpose::check_xpdb(xpdb, check = "data")
  xp_d <- xpdb$data
  if (!is.null(.problem) && !.problem %in% xp_d$problem) cli::cli_abort("Problem number { .problem} not valid.")
  
  # Arg process
  .handle_missing = rlang::arg_match0(arg = .handle_missing, values = c("quiet","warn","error"))
  
  # Relevant index
  full_index <- get_index(xpdb, .problem=.problem)
  # Relevant data
  full_data <- xpose::get_data(xpdb, .problem=.problem, quiet = TRUE)
  
  # Consume dots
  lvl_list <- rlang::dots_list(..., .ignore_empty = "all", .homonyms = "keep")
  check_levels(lvl_list, full_index)
}