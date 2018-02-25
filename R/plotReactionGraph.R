#' Plot reaction graph
#' 
#' Plot the reaction graph data produced by 
#' \code{\link{parseModuleReaction}} using the visNetwork
#' package.
#'
#' The \code{vertices} and \code{edges} data.frames may be passed directly
#' as they are produced by \code{\link{parseModuleReaction}} or additional
#' columns may be added as specified below. 
#' 
#' \strong{Vertices:}
#' \describe{
#'    \item{\code{title}}{Provides the mouseover text of a node. If unspecified 
#'        this is constructed from the node id or weight column if provided.}
#'    \item{\code{shape}}{Name of a shape. Default is 'box' for reactions and 
#'        'dot' for compounds.}
#'    \item{\code{label}}{The label to appear in the node. By default this is 
#'        the id for reactions and blank for compounds.}
#'    \item{\code{color}}{String representing color hex code (e.g. '#FFFFFF') 
#'        or color name (e.g. 'red'). If \code{vertexWeightColumn} is provided 
#'        and there is no 'color' column, the color will be determined by the 
#'        weight.}
#' }
#'
#' \strong{Edges:}
#' \describe{
#'    \item{\code{color}}{String representation of color as above. Default is 
#'        'black'.}
#'    \item{\code{arrows}}{Where to place arrowheads on edges. Default is 'to' 
#'        directed edges and 'undefined' otherwise. Valid values include 
#'        for 'to', 'from', 'middle' or any combination with any separating 
#'        symbol.}
#' }
#' 
#' @param vertices data.frame of vertex data
#' @param edges data.frame of edge data
#' @param vertexWeightColumn name of column in vertices to 
#'        use for assigning colors
#' @param palette any valid palette argument to 
#'        the \code{\link{col_numeric}} function
#' @return a visNetwork object
#' @seealso \code{\link{parseModuleReaction}}, \code{\link{col_numeric}},
#'          \code{\link{visNetwork}}
#' @export
plotReactionGraph <- function(vertices, edges, vertexWeightColumn=NA, 
                              palette="YlOrRd", title=NA) {
    require(RColorBrewer)
    require(visNetwork)
    require(scales)

	  cpal <- col_numeric(palette, c(0, 1), na.color="#FFFFFF")

    # Vertices formatting
    if (nrow(vertices) > 0) {
        if (!("label" %in% colnames(vertices))) {
            vertices$label <- vertices$id
            vertices$label[vertices$type == "compound"] <- ""
        }
        if (!("shape" %in% colnames(vertices))) {
            vertices$shape <- "box"
            vertices$shape[vertices$type == "compound"] <- "dot"
        }
        if (!("size" %in% colnames(vertices))) {
            vertices$size <- NA
            vertices$size[vertices$type == "compound"] <- 5
        }
        if (!("color" %in% colnames(vertices)) & !is.na(vertexWeightColumn)) {
            vertices$color <- cpal(vertices[, vertexWeightColumn])
        }
        if (!("title" %in% colnames(vertices))){
            vertices$title[vertices$type == "compound"] <- vertices$id[vertices$type == "compound"]
            if (!is.na(vertexWeightColumn)) {
                vertices$title[vertices$type == "reaction"] <- 
                    paste0(vertexWeightColumn, "=", 
                    sprintf("%1.3f", vertices[vertices$type == "reaction", vertexWeightColumn]))
            }
        }
    }
    
    # Edges formatting
    if (nrow(edges) > 0) {
        if (!("arrows" %in% colnames(edges))) {
            edges$arrows <- "to"
            edges$arrows[!edges$directed] <- "undefined"
        }
        if (!("color" %in% colnames(edges))) {
            edges$color <- "black"
        }
    }
    
    # add title as special root node
#    browser()
    if (!is.na(title)) {

        if (nrow(vertices) == 1 && !("level" %in% colnames(vertices))) {
            vertices$level <- 0
        } 
    
        curr.root <- vertices$id[which.min(vertices$level)[1]]
        
        new.vertex <- vertices[1, ]
        new.vertex[1,] <- NA
        new.vertex$id <- "title"
        new.vertex$label <- title
        new.vertex$shape <- "box"
        if ("color" %in% names(new.vertex)) new.vertex$color <- "#FFFFFF"
        new.vertex$level <- min(vertices$level)-1
        vertices <- rbind(new.vertex, vertices)
        vertices$level <- vertices$level+1
        
        # make alternate styling for title node
        vertices$borderWidth <- 1
        vertices$borderWidth[1] <- 0
        vertices$font <- "14px arial black"
        vertices$font[1] <- "18px arial black"
        
        new.edge <- edges
        new.edge[1, ] <- NA
        new.edge <- new.edge[1,]
        new.edge$from <- "title"
        new.edge$to <- curr.root
        
        new.edge$color <- "#FFFFFF"
        edges <- rbind(new.edge, edges)
        
    }
    
	p <- visNetwork(vertices, edges, height=800) %>% visEdges("to") %>% 
		visHierarchicalLayout(sortMethod="directed", 
			levelSeparation=75)%>%#, nodeSpacing=50, edgeMinimization=FALSE, blockShifting=FALSE) %>%
		visEvents(selectNode="function(properties) {
            nodenames = properties.nodes[0].split(' | ');
            for (var key in nodenames) {
            	url = \"http://www.kegg.jp/dbget-bin/www_bget?\"+nodenames[key];
            	window.open(url, '_blank');
			}
            }") %>%
		visInteraction(#dragNodes=FALSE, 
            selectConnectedEdges=FALSE) %>%
#		visNodes(font="14px arial black") %>%
		visPhysics(hierarchicalRepulsion=list(nodeDistance=50, springLength=50)) 
    
	return(p)
}