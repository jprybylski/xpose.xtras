# This is just a rough implementation for now
# Final version would use df input
# color based on focus and base
# allow extra info to be shown (like exposed_props)
# Final version should have function that makes diagram
# and another that renders, so plot is not forced to generate
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

  # Should use edge_df instead of this loop
  for (ton in seq_along(xpdb_s)) {
    if (length(xpdb_s[[ton]]$parent)>0 && any(xpdb_s[[ton]]$parent %in% names(xpdb_s)))
      base_diag <- base_diag  %>%
        DiagrammeR::add_edge(from = which(names(xpdb_s) %in% xpdb_s[[ton]]$parent), to = ton)
  }
  base_diag %>%
    DiagrammeR::render_graph(layout="tree")
} # diagrammr
