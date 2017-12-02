a <- grid::arrow(type="closed",unit(0.15,"inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha=n),
                  show.legend = FALSE,
                  arrow=a,end_cap=circle(0.07,"inches") ) +
  geom_node_point(color="lightblue",size=5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) + theme_void()