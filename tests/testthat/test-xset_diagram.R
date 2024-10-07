test_that("diagram gets generated", {
  expect_no_error(
    diagram_lineage(pheno_set)
  )
  expect_true(
    DiagrammeR::is_graph_simple(diagram_lineage(pheno_set))
  )
})
