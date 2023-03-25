
<!-- README.md is generated from README.Rmd. Please edit that file -->

# graphhighlight

This package highlights nodes and edges of `ggraph` plot.

``` r
library(ggraph)
library(igraph)
library(graphhighlight)
set.seed(1)
g <- random.graph.game(10,0.1,directed=TRUE)
E(g)$weight <- sample(1:10, length(E(g)), replace=TRUE)
V(g)$size <- sample(1:10, length(V(g)), replace=TRUE)
V(g)$name <- letters[1:length(V(g))]

ggraph(g, layout="nicely")+
  geom_edge_diagonal(aes(width=.data$weight,
                         color=.data$weight))+
  geom_node_point(aes(size=size))+
  geom_node_text(aes(label=name), repel=TRUE)+
  scale_edge_color_gradient(low="grey",high="red")+
  scale_edge_width(range=c(0.1,1))+
  highlight_edge(filter="weight>8",highlight_color = "blue")+
  highlight_node(filter="size>6",highlight_color = "green")+
  theme_graph()
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="768" style="display: block; margin: auto;" />

``` r
ggraph(g, layout="nicely")+
  geom_edge_diagonal(aes(width=.data$weight,
                         color=.data$weight))+
  geom_node_point(aes(size=size))+
  geom_node_text(aes(label=name), repel=TRUE)+
  scale_edge_color_gradient(low="blue",high="red")+
  highlight_node(node_name="a")+
  highlight_edge(filter="weight<5",
                 glow=TRUE,
                 glow_base_size=TRUE,
                 glow_edge_size=0.8)+
  theme_graph()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="768" style="display: block; margin: auto;" />
