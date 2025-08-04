# Preferred wrappers for checkmate assertions

# TODO: Swap out verbose assertions for this
xpa <- function(
    that = "",
    expr,
    custom_msg = "",
    caller = parent.frame(),
    ...
) {
  checkmate::check_atomic(custom_msg)
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
               paste0("Error evaluating `",deparse(expr),"`"),
               custom_msg),
        parent = s, call = caller, use_cli_format = TRUE
      )
    }
  )
}
