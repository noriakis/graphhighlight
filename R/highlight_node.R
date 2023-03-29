
#' highlight_node
#' highlight the nodes
#' @param node_name used to subset node data with `name` argument
#' @param highlight_color highlighting color
#' @param filter used to subset node data specified as string
#' @param glow use glow highlighting
#' @param glow_fixed_color use glow highlighting with fixed color,
#' specified by `highlight_color` (default: FALSE, use the raw color of the nodes)
#' @param glow_base_size use base size of the node for initial size of glowing
#' @param highlight_by_shape boolean, use shape highlighting
#' @param shape_number shape number
#' @param shape_size shape size
#' @param shape_color shape color 
#' @param shape_with_text if specified, show text along with shape (21 only)
#' using `geomtextpath`
#' @param glow_size argument to control how big the glowing will be
#' @param override_text after highlighting, stack the last geom_node_text layer
#' @importFrom rlang .data
#' @importFrom ggplot2 aes ggplot_build scale_alpha
#' @importFrom ggraph get_nodes get_edges geom_node_text geom_node_point
#' @importFrom ggraph geom_node_label scale_edge_alpha
#' @export
highlight_node <- function(node_name=NULL,
                           highlight_color=NULL,
                           filter=NULL,
                           glow=FALSE,
                           glow_fixed_color=FALSE,
                           glow_base_size=FALSE,
                           highlight_by_shape=FALSE,
                           shape_number=NULL,
                           shape_size=NULL,
                           shape_color=NULL,
                           shape_with_text=NULL,
                           glow_size=1.2,
                           override_text=FALSE) {
  structure(list(node_name = node_name,
                 highlight_color = highlight_color,
                 filter = filter,
                 glow = glow,
                 glow_fixed_color = glow_fixed_color,
                 glow_base_size = glow_base_size,
                 glow_size=glow_size,
                 highlight_by_shape = highlight_by_shape,
                 shape_number = shape_number,
                 shape_size = shape_size,
                 shape_color = shape_color,
                 shape_with_text = shape_with_text,
                 override_text = override_text),
            class = "highlight_node")
}

#' ggplot_add.highlight_node
#' @param object An object to add to the plot
#' @param plot The ggplot object to add object to
#' @param object_name The name of the object to add
#' @importFrom ggplot2 ggplot_add
#' @export ggplot_add.highlight_node
#' @export
ggplot_add.highlight_node <- function(object, plot, object_name) {
  
  if (!is.null(object$shape_with_text)) {object$shape_number <- 21}

  nd <- get_nodes()(plot$data)
  ## TODO: no text evaluation
  if (!is.null(object$filter)) {
    nd <- subset(nd, eval(parse(text=object$filter)))
  }
  if (!is.null(object$node_name)) {
    nd <- nd[ nd$name %in% object$node_name, ]
  }
  
  candidate_node_id <- nd$.ggraph.orig_index

  candl <- NULL
  for (l in seq_along(plot$layers)) {
    st <- plot$layers[[l]]$geom
    if (sum(grepl("GeomPoint", attributes(st)$class))>0){
      if (!is.null(candl)) {
        message("multiple geom_node_point found, taking the last layer")
      }
      candl <- l
    }
  }
  if (is.null(candl)) {stop("No geom_node_point found")}
  geom_param_list <- plot$layers[[candl]]$geom_params
  if (!object$glow) {
    if (!is.null(object$highlight_color)) {
      geom_param_list[["color"]] <- object$highlight_color
    }
  }
  geom_param_list[["show.legend"]] <- FALSE
  geom_param_list["na.rm"] <- NULL
  aes_list <- plot$layers[[candl]]$mapping
  aes_list["filter"] <- NULL

  if (object$glow) {
    plot <- glow_nodes(plot, aes_list, candidate_node_id, geom_param_list, object$glow_base_size, candl,
      object$glow_fixed_color, object$highlight_color, object$glow_size)  
  } else {
    if (object$highlight_by_shape) {
      if (is.null(object$shape_number)) {stop("Please specify shape")}
      if (is.null(object$shape_size)) {stop("Please specify shape size")}
      geom_param_list[["shape"]] <- object$shape_number
      base_size <- ggplot_build(plot)$data[[candl]][plot$data$.ggraph.orig_index
       %in% candidate_node_id,]$size
      geom_param_list[["size"]] <- base_size + object$shape_size
      geom_param_list[["color"]] <- object$shape_color
      plot <- plot + do.call(geom_node_point,c(list(mapping=c(aes_list,
                                                      aes(filter=.data$.ggraph.orig_index 
                                                        %in% candidate_node_id))),
                                       geom_param_list))
      if (!is.null(object$shape_with_text)) {
        build <- ggplot_build(plot)$data[[candl]][plot$data$.ggraph.orig_index
                                          %in% candidate_node_id,]
        t <- seq(1, -1, length.out = 1000) * pi
        build$ssize <- sqrt(build$size)/pi/.pt ## Better specification
        pos <- do.call(rbind,
                  sapply(seq_len(length(row.names(build))),
                        function(row) {
                          data.frame(
                          x=as.numeric(build[row,"x"]) + 
                            sin(t)*build[row,"ssize"],
                          y=as.numeric(build[row,"y"]) + 
                            cos(t)*build[row,"ssize"],
                          text=object$shape_with_text,
                          group=row
                          )
                        },
            simplify=FALSE)) |> data.frame()
        plot <- plot + geom_textpath(x=pos$x,
                                      y=pos$y,
                                      straight=FALSE,
                                      group=pos$group,
                                      label=pos$text,
                                      # color=object$highlight_color,
                                      data=pos,
                                      offset=unit(5,"mm"),
                                      text_only = TRUE,
                                      size=2)
      }
    } else {
      plot <- plot + do.call(geom_node_point,c(list(mapping=c(aes_list,
                                                      aes(filter=.data$.ggraph.orig_index 
                                                        %in% candidate_node_id))),
                                       geom_param_list))      
    }
  }
  if (object$override_text) {
    plot + highlight_node_text(highlight_color=NULL)
  } else {
    plot
  }
}

#' @noRd
glow_nodes <- function(plot, aes_list, candidate_node_id,
                       geom_param_list, glow_base_size, candl,
                       glow_fixed_color, highlight_color, glow_size) {
  layers <- 10
  size <- 8
  aes_list[["fill"]] <- NA
  geom_param_list[["fill"]] <- NA
  aes_list[["alpha"]] <- 1
  aes_list["size"] <- NULL

  if (glow_fixed_color) {
    aes_list["color"] <- NULL
    geom_param_list[["color"]] <- highlight_color
  }

  if (glow_base_size) {
    base_size <- ggplot_build(plot)$data[[candl]][plot$data$.ggraph.orig_index %in% candidate_node_id,]$size
  }

  for (i in seq_len(layers+1)){
    if (glow_base_size) {
      geom_param_list[["size"]] <- base_size + (glow_size*i)
    } else {
      geom_param_list[["size"]] <- size+(glow_size*i)
    }
    plot <- plot + do.call(geom_node_point,
                       c(list(mapping=c(aes_list,
                                        aes(filter=.data$.ggraph.orig_index %in% candidate_node_id))),
                         geom_param_list))
  }
  plot+scale_alpha(range=c(0.01, 0.1),guide="none")
}
