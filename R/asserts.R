# Preferred wrappers for checkmate assertions

# TODO: Swap out verbose assertions for this
xpa <- function(
    that = "",
    expr,
    custom_msg = "",
    caller = parent.frame(),
    ...
) {
  checkmate::assert_string(custom_msg) # can be vanilla since this is internal
  literal_expression <- rlang::as_label(rlang::enquo(expr))
  fun_to_call <- paste0("assert_",tolower(that))
  rlang::try_fetch(
    rlang::exec(
      rlang::as_function(fun_to_call, env = rlang::ns_env("checkmate")),
      expr,
      ...
    ),
    error = function(s) {
      rlang::abort(
        ifelse(custom_msg=="",
               paste0("Error evaluating `",literal_expression,"`"),
               custom_msg),
        parent = s, call = caller, use_cli_format = TRUE
      )
    }
  )
}
