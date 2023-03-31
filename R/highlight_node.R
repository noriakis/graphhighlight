
#' highlight_node
#' highlight the nodes
#' @param node_name used to subset node data with `name` argument
#' @param highlight_color highlighting color
#' @param filter used to subset node data specified as string
#' @param glow use glow highlighting
#' @param glow_base_size use base size of the node for initial size of glowing
#' @param shape_number shape number
#' @param shape_size shape size
#' @param shape_color shape color 
#' @param shape_with_text if specified, show text along with shape (21 only)
#' using `geomtextpath`
#' @param with_text if specified, show text around the nodes, not the shape
#' @param text_attribute if not NULL, graph attributes of this name will be used as text annotation
#' @param text_node_color if TRUE, node color will be used as text color
#' @param glow_size argument to control how big the glowing will be
#' @param override_text after highlighting, stack the last geom_node_text layer
#' @param use_ggfx use ggfx geom to highlight, default to NULL
#' @param ggfx_params ggfx parameters
#' @importFrom rlang .data
#' @importFrom ggplot2 aes ggplot_build scale_alpha
#' @importFrom ggraph get_nodes get_edges geom_node_text geom_node_point
#' @importFrom ggraph geom_node_label scale_edge_alpha
#' @export
highlight_node <- function(node_name=NULL,
                           highlight_color=NULL,
                           filter=NULL,
                           glow=FALSE,
                           glow_base_size=FALSE,
                           shape_number=NULL,
                           shape_size=NULL,
                           shape_color=NULL,
                           shape_with_text=NULL,
                           with_text=NULL,
                           text_attribute=NULL,
                           text_node_color=FALSE,
                           glow_size=1.2,
                           override_text=FALSE,
                           use_ggfx=NULL,
                           ggfx_params=list(),
                           textpath_params=list()) {
  structure(list(node_name = node_name,
                 highlight_color = highlight_color,
                 filter = filter,
                 glow = glow,
                 glow_base_size = glow_base_size,
                 glow_size=glow_size,
                 shape_number = shape_number,
                 shape_size = shape_size,
                 shape_color = shape_color,
                 shape_with_text = shape_with_text,
                 with_text = with_text,
                 text_attribute=text_attribute,
                 text_node_color=text_node_color,
                 override_text = override_text,
                 use_ggfx = use_ggfx,
                 ggfx_params = ggfx_params,
                 textpath_params = textpath_params),
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
  
  if (!is.null(object$with_text)) {object$shape_with_text <- NULL}
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
        message("multiple geom_node_point found, taking the first layer")
      } else {
        candl <- l
      }
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


  if (!is.null(object$use_ggfx)) {
    plot <- plot + do.call(
      eval(parse(text=object$use_ggfx)),
      c(list(
        x=do.call(geom_node_point,c(list(mapping=c(aes_list,
                                                      aes(filter=.data$.ggraph.orig_index 
                                                        %in% candidate_node_id))),
                                      geom_param_list))
        ),
      object$ggfx_params
      )
    )
  } else {
    if (object$glow) {
      plot <- glow_nodes(plot, aes_list, candidate_node_id, geom_param_list, object$glow_base_size, candl,
        object$highlight_color, object$glow_size)  
    } else {
      if (!is.null(object$with_text)) {
          plot <- append_textpath(plot=plot, candl=candl, candidate_node_id=candidate_node_id,
            shape_with_text=object$with_text, along_with="node", textpath_params=object$textpath_params,
            text_attribute=object$text_attribute, text_node_color=object$text_node_color)
      } else {
        if (!is.null(object$shape_number)) {
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
            plot <- append_textpath(plot=plot, candl=candl, candidate_node_id=candidate_node_id,
              shape_with_text=object$shape_with_text, along_with="shape",
              textpath_params=object$textpath_params,
              text_attribute=object$text_attribute,
              text_node_color=object$text_node_color)
          }
        } else {
          plot <- plot + do.call(geom_node_point,c(list(mapping=c(aes_list,
                                                          aes(filter=.data$.ggraph.orig_index 
                                                            %in% candidate_node_id))),
                                           geom_param_list))      
        }
      }
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
                       highlight_color, glow_size) {
  layers <- 10
  size <- 8
  aes_list[["fill"]] <- NA
  geom_param_list[["fill"]] <- NA
  aes_list[["alpha"]] <- 1
  aes_list["size"] <- NULL

  if (!is.null(highlight_color)) {
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

#' @noRd
append_textpath <- function(plot, candl, candidate_node_id,
            shape_with_text, along_with, textpath_params,
            text_attribute, text_node_color) {
  build <- ggplot_build(plot)$data
  if (along_with=="shape") {
    ## Take the last layer in which the shape has been added
    build <- build[[length(build)]]
  } else {
    build <- build[[candl]][plot$data$.ggraph.orig_index %in% candidate_node_id,]
  }
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
                    text=ifelse(is.null(text_attribute),
                      shape_with_text,
                      plot$data[plot$data$.ggraph.orig_index %in% candidate_node_id,]
                      [row, text_attribute]),
                    group=row,
                    color=ifelse(text_node_color,
                      build[row, "colour"],
                      ifelse(!is.null(textpath_params[["color"]]),
                        textpath_params[["color"]], "black"))
                    )
                  },
      simplify=FALSE)) |> data.frame()
  ## [TODO] change the colors according to some node params
  textpath_params[["x"]] <- pos$x
  textpath_params[["y"]] <- pos$y
  textpath_params[["group"]] <- pos$group
  textpath_params[["label"]] <- pos$text
  textpath_params[["data"]] <- pos
  textpath_params[["color"]] <- pos$color

  plot <- plot + do.call(geom_textpath, textpath_params)
  plot
}