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