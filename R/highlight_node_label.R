#' highlight_node_label
#' 
#' This just stacks the layer changing the colour of 
#' specified label in previous layer's output.
#' Must specify seed to use with ggrepel, and should be 
#' used with shadowtext in some environments.
#' 
#' @param node_name text labels to highlight
#' @param filter text attributes to filter (in string)
#' @param highlight_color specify highlighting colour
#' @param highlight_bg_color specify highlighting colour
#' @param seed make repel reproducible
#' @export
highlight_node_label <- function(node_name=NULL,
                           highlight_color="red",
                           highlight_bg_color="yellow",
                           filter=NULL,
                           seed=42) {
  structure(list(node_name = node_name,
                 highlight_color = highlight_color,
                 highlight_bg_color=highlight_bg_color,
                 filter = filter,
                 seed=seed
                 ),
            class = "highlight_node_label")
}

#' ggplot_add.highlight_node_label
#' @param object An object to add to the plot
#' @param plot The ggplot object to add object to
#' @param object_name The name of the object to add
#' @export ggplot_add.highlight_node_label
#' @export
ggplot_add.highlight_node_label <- function(object, plot, object_name) {

  nd <- get_nodes()(plot$data)
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
    if (sum(grepl("GeomLabel", attributes(st)$class))>0){
      if (!is.null(candl)) {
        message("multiple geom_node_label found, taking the last layer")
      }
      candl <- l
      cl <- attributes(st)$class
      se <- cl[which(sapply(cl, function(x) grepl("GeomLabel",x)))]
      if (se=="GeomLabelRepel") {
        use_repel <- TRUE
      } else {
        use_repel <- FALSE
      }
    }
  }
  
  if (is.null(candl)) {stop("No geom_node_label found")}
  plot$layers[[candl]]$geom_params[["seed"]] <- object$seed
  geom_param_list <- plot$layers[[candl]]$geom_params

  if (is.null(object$highlight_color) & is.null(object$highlight_bg_color)) {
    stop("Please specify highlight_color or highlight_bg_color")
  }

  geom_param_list[["show.legend"]] <- FALSE
  geom_param_list["na.rm"] <- NULL
  
  build <- ggplot_build(plot)$data[[candl]]
  if (!is.null(object$highlight_color)) {
    build[ plot$data$.ggraph.orig_index %in% candidate_node_id, ]$colour <- object$highlight_color
  }
  if (!is.null(object$highlight_bg_color)) {  
    build[ plot$data$.ggraph.orig_index %in% candidate_node_id, ]$fill <- object$highlight_bg_color
  }
  aes_list <- plot$layers[[candl]]$mapping
  aes_list["filter"] <- NULL

  geom_param_list[["repel"]] <- use_repel
  if (use_repel) {
    geom_param_list[["seed"]] <- object$seed
  }
  geom_param_list["color"] <- NULL;

  plot + do.call(geom_node_label,
                 c(geom_param_list, list(
                   label=build$label,
                    color=build$colour,
                    fill=build$fill)))
}