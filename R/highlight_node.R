
#' @export
highlight_node <- function(node_name=NULL,
                           highlight_color="red",
                           filter=NULL,
                           glow=FALSE,
                           glow_fixed_color=FALSE,
                           glow_base_size=FALSE) {
  structure(list(node_name = node_name,
                 highlight_color = highlight_color,
                 filter = filter,
                 glow = glow,
                 glow_fixed_color = glow_fixed_color,
                 glow_base_size = glow_base_size),
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
  geom_param_list <- plot$layers[[candl]]$geom_params
  if (!object$glow) {
    geom_param_list[["color"]] <- object$highlight_color
  }
  geom_param_list[["show.legend"]] <- FALSE
  geom_param_list["na.rm"] <- NULL
  aes_list <- plot$layers[[candl]]$mapping
  
  if (object$glow) {
    glow_nodes(plot, aes_list, nd$name, geom_param_list, object$glow_base_size, candl,
      object$glow_fixed_color, object$highlight_color)  
  } else {
    plot + do.call(geom_node_point,c(list(mapping=c(aes_list,
                                                    aes(filter=.data$name %in% nd$name))),
                                     geom_param_list))
  }
}

glow_nodes <- function(plot, aes_list, candidate_node_id,
                       geom_param_list, glow_base_size, candl,
                       glow_fixed_color, highlight_color) {
  layers <- 10
  size <- 8
  glow_size <- 1.2
  aes_list[["fill"]] <- NA
  geom_param_list[["fill"]] <- NA

  aes_list[["alpha"]] <- 1
  aes_list["size"] <- NULL

  if (glow_fixed_color) {
    aes_list["color"] <- NULL
    geom_param_list[["color"]] <- highlight_color
  }

  if (glow_base_size) {
    base_size <- ggplot_build(plot)$data[[candl]][plot$data$name %in% candidate_node_id,]$size
  }

  for (i in seq_len(layers+1)){
    if (glow_base_size) {
      geom_param_list[["size"]] <- base_size + (glow_size*i)
      plot <- plot + do.call(geom_node_point,
                             c(list(mapping=c(aes_list,
                                              aes(filter=.data$name %in% candidate_node_id))),
                               geom_param_list))
    } else {
      geom_param_list[["size"]] <- size+(glow_size*i)
      plot <- plot + do.call(geom_node_point,c(list(mapping=c(aes_list,
                                                              aes(filter=.data$name %in% candidate_node_id))),
                                               geom_param_list))
    }
  }
  plot+scale_alpha(range=c(0.01, 0.1),guide="none")
}