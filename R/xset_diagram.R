# This is just a rough implementation for now
# Final version would use df input
# color based on focus and base
# allow extra info to be shown (like exposed_props)
# Final version is expected to use multiple functions.

#' Visualize `xpose_set`
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' In its current state, this function is intended to
#' provide a simple visual representation of an `xpose_set`.
#' Functionality and aesthetic enhancements are expected in
#' future releases.
#'
#' @param xpdb_s <`xpose_set`> object
#' @param ... For later expansion. Will be ignored.
#'
#' @export
#'
#' @examples
#'
#' diagram_lineage(pheno_set) %>%
#'   DiagrammeR::render_graph(layout="tree")
#'
diagram_lineage <- function(xpdb_s) {
  base_diag <- DiagrammeR::create_graph() %>%
    DiagrammeR::add_n_nodes(length(xpdb_s)) %>%
    DiagrammeR::set_node_attrs(
      node_attr = label,
      values = names(xpdb_s)
    ) %>%
    DiagrammeR::set_node_attr_to_display(
      nodes = seq_along(xpdb_s),
      attr = label)

  # TODO: Should use edge_df instead of this loop
  for (ton in seq_along(xpdb_s)) {
    if (length(xpdb_s[[ton]]$parent)>0 && any(xpdb_s[[ton]]$parent %in% names(xpdb_s)))
      base_diag <- base_diag  %>%
        DiagrammeR::add_edge(from = which(names(xpdb_s) %in% xpdb_s[[ton]]$parent), to = ton)
  }

  base_diag
}
