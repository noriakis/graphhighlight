highlight_node <- function(node_name, highlight_color="red") {
  structure(list(node_name = node_name,
                 highlight_color = highlight_color),
            class = "highlight_node")
}

#' ggplot_add.highlight_node
#' @param object An object to add to the plot
#' @param plot The ggplot object to add object to
#' @param object_name The name of the object to add
#' @export ggplot_add.highlight_node
#' @export
ggplot_add.highlight_node <- function(object, plot, object_name) {
  ## Or, directly modify ggplot_build(plot) to change color
  ## and re-convert gtable to ggplot2
  candl <- NULL
  for (l in seq_along(plot$layers)) {
    st <- plot$layers[[l]]$geom
    if (sum(grepl("GeomPoint", attributes(st)$class))>0){
      candl <- l
    }
  }
  if (is.null(candl)) {stop("No node_point geom")}
  aes_list <- plot$layers[[candl]]$mapping
  plot + geom_node_point(c(aes_list,
                           aes(filter=.data$name %in% object$node_name)),
                         color=object$highlight_color,
                         show.legend=FALSE)
}