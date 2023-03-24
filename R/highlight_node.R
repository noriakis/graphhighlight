highlight_node <- function(node_name=NULL,
  highlight_color="red", filter=NULL) {
  structure(list(node_name = node_name,
                 highlight_color = highlight_color,
                 filter = filter),
            class = "highlight_node")
}

#' ggplot_add.highlight_node
#' @param object An object to add to the plot
#' @param plot The ggplot object to add object to
#' @param object_name The name of the object to add
#' @export ggplot_add.highlight_node
#' @export
ggplot_add.highlight_node <- function(object, plot, object_name) {

  nd <- get_nodes()(plot$data)
  ## TODO: no text evaluation
  if (!is.null(object$filter)) {
    nd <- subset(nd, eval(parse(text=object$filter)))
  }
  filter_nodes <- nd$name

  if (!is.null(object$node_name)) {
    nd <- nd[ nd$name %in% intersect(filter_nodes, object$node_name), ]
  } else {
    nd <- nd[ nd$name %in% filter_nodes, ]
  }

  candl <- NULL
  for (l in seq_along(plot$layers)) {
    st <- plot$layers[[l]]$geom
    if (sum(grepl("GeomPoint", attributes(st)$class))>0){
      candl <- l
    }
  }
  if (is.null(candl)) {stop("No geom_node_point found")}

  aes_list <- plot$layers[[candl]]$mapping
  plot + geom_node_point(c(aes_list,
                           aes(filter=.data$name %in% nd$name)),
                         color=object$highlight_color,
                         show.legend=FALSE)
}