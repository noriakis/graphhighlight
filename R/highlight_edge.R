#' @export
highlight_edge <- function(edge_name=NULL,
  highlight_color="red", directed=TRUE, filter=NULL,
  change_label_color=TRUE, glow=FALSE) {

  structure(list(edge_name = edge_name,
                 highlight_color = highlight_color,
                 directed = directed,
                 filter = filter, glow=glow,
                 change_label_color = change_label_color),
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
  geom_param_list <- plot$layers[[candl]]$geom_params
  if (!object$glow) {
    geom_param_list[["color"]] <- object$highlight_color
  }
  geom_param_list[["show.legend"]] <- FALSE
  geom_param_list["na.rm"] <- NULL
  geom_param_list["interpolate"] <- NULL
  
  build <- ggplot_build(plot)
  current_end_cap <- unique(build$data[[candl]]$end_cap)
  minimum_edge_width <- min(build$data[[candl]]$edge_width)
  if (!is.na(current_end_cap)) {
    geom_param_list[["end_cap"]] <- current_end_cap
  }
  if (object$change_label_color) {
    geom_param_list[["label_colour"]] <- object$highlight_color
  }
  # plot + eval(parse(text = geom))(c(aes_list,
  #                                   aes(filter=edge.id %in% candidate_edge_id)),
  #                                 color=object$highlight_color,show.legend=FALSE)

  if (object$glow) {
    glow_edges(plot, geom, aes_list, candidate_edge_id, geom_param_list)
  } else {
    plot + do.call(eval(parse(text = geom)),
      c(list(mapping=c(aes_list,aes(filter=edge.id %in% candidate_edge_id))),
         geom_param_list))    
  }

  
}


glow_edges <- function (plot, geom, aes_list, candidate_edge_id, geom_param_list) {
  layers <- 10
  edge_size <- 0.01
  glow_edge_size <- 0.3
  geom_param_list[["position"]] <- "identity"
  aes_list["width"] <- NULL
  aes_list[["edge_alpha"]] <- 1
  for (i in seq_len(layers+1)){
      geom_param_list[["edge_width"]] <- edge_size+(glow_edge_size*i)
      plot <- plot + do.call(eval(parse(text = geom)),
      c(list(mapping=c(aes_list,
        aes(filter=edge.id %in% candidate_edge_id))),
        geom_param_list))
  }
  plot+scale_edge_alpha(range=c(0.01, 0.1),guide="none")
}