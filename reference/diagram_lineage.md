# Visualize `xpose_set`

**\[experimental\]**

In its current state, this function is intended to provide a simple
visual representation of an `xpose_set`. Functionality and aesthetic
enhancements are expected in future releases.

## Usage

``` r
diagram_lineage(xpdb_s, ...)
```

## Arguments

- xpdb_s:

  \<`xpose_set`\> object

- ...:

  For later expansion. Will be ignored.

## Value

A DiagrammeR-compliant graph object.

## Examples

``` r
diagram_lineage(pheno_set) %>%
  DiagrammeR::render_graph(layout="tree")

{"x":{"diagram":"digraph {\n\ngraph [layout = \"neato\",\n       outputorder = \"edgesfirst\",\n       bgcolor = \"white\"]\n\nnode [fontname = \"Helvetica\",\n      fontsize = \"10\",\n      shape = \"circle\",\n      fixedsize = \"true\",\n      width = \"0.5\",\n      style = \"filled\",\n      fillcolor = \"aliceblue\",\n      color = \"gray70\",\n      fontcolor = \"gray50\"]\n\nedge [fontname = \"Helvetica\",\n     fontsize = \"8\",\n     len = \"1.5\",\n     color = \"gray80\",\n     arrowsize = \"0.5\"]\n\n  \"1\" [label = \"run3\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"-1.5,6!\"] \n  \"2\" [label = \"run4\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"-1,5!\"] \n  \"3\" [label = \"run5\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"-1,5!\"] \n  \"4\" [label = \"run6\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"-1,4!\"] \n  \"5\" [label = \"run10\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"-3,3!\"] \n  \"6\" [label = \"run12\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"-3,2!\"] \n  \"7\" [label = \"run11\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"0,3!\"] \n  \"8\" [label = \"run13\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"0,2!\"] \n  \"9\" [label = \"run7\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"-1,3!\"] \n  \"10\" [label = \"run8\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"-2,3!\"] \n  \"11\" [label = \"run9\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"1,3!\"] \n  \"12\" [label = \"run14\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"1,2!\"] \n  \"13\" [label = \"run15\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"0.5,1!\"] \n  \"14\" [label = \"run16\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"1.5,1!\"] \n  \"1\"->\"2\" \n  \"1\"->\"3\" \n  \"3\"->\"4\" \n  \"4\"->\"5\" \n  \"5\"->\"6\" \n  \"4\"->\"7\" \n  \"7\"->\"8\" \n  \"4\"->\"9\" \n  \"4\"->\"10\" \n  \"4\"->\"11\" \n  \"11\"->\"12\" \n  \"12\"->\"13\" \n  \"12\"->\"14\" \n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
```
