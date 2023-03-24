highlight_edge <- function(edge_name, highlight_color="red") {
  structure(list(edge_name = edge_name,
                 highlight_color = highlight_color),
            class = "highlight_edge")
}

#' ggplot_add.highlight_edge
#' @param object An object to add to the plot
#' @param plot The ggplot object to add object to
#' @param object_name The name of the object to add
#' @export ggplot_add.highlight_edge
#' @export
ggplot_add.highlight_edge <- function(object, plot, object_name) {
  ## Plan 1
  ## `+theme_graph()` will not work after this modification
  ## Also slow
  if (is.vector(object$edge_name)) {
    frs <- object$edge_name[1]
    tos <- object$edge_name[2]
  }
  ed <- get_edges()(plot$data)
  ed <- ed[ ed$node1.name %in% frs, ]
  ed <- ed[ ed$node2.name %in% tos, ]
  if (dim(ed)[1]==0) {stop("There is no edge specified")}
  candidate_edge_id <- ed$edge.id
  # build <- ggplot_build(plot)
  # re <- build$data[[1]]
  # re[ re$group %in% candidate_edge_id, "edge_colour" ] <- object$highlight_color
  # build$data[[1]] <- re
  # ggplotify::as.ggplot(ggplot_gtable(build))
  
  ## Plan 2
  for (l in seq_along(plot$layers)) {
    st <- plot$layers[[l]]$stat
    if (sum(grepl("StatEdge", attributes(st)$class))>0){
      cl <- attributes(st)$class
      se <- cl[which(sapply(cl, function(x) grepl("StatEdge",x)))]
    }
  }
  se <- tolower(se)
  geom <- paste0("geom_",substr(se, 5, 8),"_",substr(se, 9, nchar(se)))
  plot + eval(parse(text = geom))(aes(filter=edge.id %in% candidate_edge_id),
                            color=object$highlight_color)
}