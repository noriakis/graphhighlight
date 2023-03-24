
highlight_edge <- function(edge_name=NULL,
  highlight_color="red", directed=TRUE, filter=NULL) {

  structure(list(edge_name = edge_name,
                 highlight_color = highlight_color,
                 directed = directed,
                 filter = filter),
            class = "highlight_edge")
}

#' ggplot_add.highlight_edge
#' @param object An object to add to the plot
#' @param plot The ggplot object to add object to
#' @param object_name The name of the object to add
#' @export ggplot_add.highlight_edge
#' @export
ggplot_add.highlight_edge <- function(object, plot, object_name) {
  ed <- get_edges()(plot$data)
  ## TODO: no text evaluation
  if (!is.null(object$filter)) {
    ed <- subset(ed, eval(parse(text=object$filter)))
  }


  if (!is.null(object$edge_name)) {

    if (is.vector(object$edge_name)) {
      frs <- object$edge_name[1]
      tos <- object$edge_name[2]
    } else {
      frs <- object$edge_name[,1]
      tos <- object$edge_name[,2]
    }

    if (object$directed) {
      ed <- ed[ ed$node1.name %in% frs, ]
      ed <- ed[ ed$node2.name %in% tos, ]
      candidate_edge_id <- ed$edge.id
    } else {
      candidate_edge_id <- NULL
      for (i in seq_len(length(frs))) {
        candidate_edge_id <- c(candidate_edge_id,
         ed[ ed$node1.name %in% c(frs[i], tos[i]) & 
               ed$node2.name %in% c(frs[i], tos[i]), ]$edge.id)
      }
    }

  } else {
    candidate_edge_id <- ed$edge.id
  }
  if (length(candidate_edge_id)==0) {stop("There is no edge specified")}
  
  ## Plan 1
  ## `+theme_graph()` will not work after this modification
  ## Also slow
  # build <- ggplot_build(plot)
  # re <- build$data[[1]]
  # re[ re$group %in% candidate_edge_id, "edge_colour" ] <- object$highlight_color
  # build$data[[1]] <- re
  # ggplotify::as.ggplot(ggplot_gtable(build))
  
  ## Plan 2
  for (l in seq_along(plot$layers)) {
    st <- plot$layers[[l]]$stat
    if (sum(grepl("StatEdge", attributes(st)$class))>0){
      candl <- l
      cl <- attributes(st)$class
      se <- cl[which(sapply(cl, function(x) grepl("StatEdge",x)))]
    }
  }
  se <- tolower(se)
  geom <- paste0("geom_",substr(se, 5, 8),"_",substr(se, 9, nchar(se)))
  aes_list <- plot$layers[[candl]]$mapping
  ## Do not reflect to legends
  plot + eval(parse(text = geom))(c(aes_list,
                                    aes(filter=edge.id %in% candidate_edge_id)),
                                  color=object$highlight_color,show.legend=FALSE)
  
}
