#' Parse module reaction string
#'
#' Parse the module reaction string and return
#' the vertices and edges of the resulting graph.
#' 
#' @param rxn.string String representation of the reaction
#' @param includeCompounds should compounds be included in 
#'        the resulting graph (FALSE by default) 
#' @return list containing 'vertices' and 'edges' data.frames
#' @export
parseModuleReaction <- function(rxn.string, includeCompounds=FALSE) {
#    require(stringr)
#    require(igraph)
    
    rxn.lines <- strsplit(rxn.string, "\n")[[1]]

    compounds <- character(0)
    rxns <- character(0)

    reactions <- data.frame(from.compound=character(0), to.compound=character(0), reaction=character(0))
    compounds <- character(0)
    rxns <- character(0)
    edges <- data.frame(from=character(0), to=character(0), directed=logical(0))
    vertices <- data.frame(id=character(0), type=character(0))

    for (ll in rxn.lines) {
        if (grepl("^[//]+$", ll)) next
        
        rxn.ids <- gsub("[[:space:]]+.*", "", ll)
        rxn.ids <- gsub(",", " | ", rxn.ids)
        #rxn.ids <- stringr::str_trim(strsplit(rxn.ids, ",")[[1]])
        rxns <- c(rxns, rxn.ids)
        
        compound.ids <- stringr::str_trim(gsub("^.+?[[:space:]]+", "", ll))
        compound.ids <- stringr::str_trim(strsplit(compound.ids, "->")[[1]])

        if (length(compound.ids) != 2) {
            stop(paste0("IDK what to do with this, there's not two sets of compounds to this reaction: ", ll))
        }
        
        from <- stringr::str_trim(strsplit(compound.ids[1], "\\+")[[1]])
        to <- stringr::str_trim(strsplit(compound.ids[2], "\\+")[[1]])
        
        compounds <- c(compounds, from, to)

        if (!includeCompounds) {
            for (ff in from) {
                for (tt in to) {
                    reactions <- rbind(reactions, data.frame(from.compound=ff, to.compound=tt, 
                        reaction=rxn.ids, stringsAsFactors=FALSE))
                    vertices <- rbind(vertices, 
                        data.frame(id=rxn.ids, type="reaction", stringsAsFactors=FALSE))
                }
            }
        } else {
            for (ff in from) {
                #from first compound to reactions
                edges <- rbind(edges, data.frame(from=ff, to=rxn.ids, 
                    directed=FALSE, stringsAsFactors=FALSE)) 
            }

            for (tt in to) {
                #from reactions to second compound 
                edges <- rbind(edges, data.frame(from=rxn.ids, to=tt, 
                    directed=TRUE, stringsAsFactors=FALSE)) 
            }
            vertices <- rbind(vertices, 
                data.frame(id=from, type="compound", stringsAsFactors=FALSE),
                data.frame(id=rxn.ids, type="reaction", stringsAsFactors=FALSE),
                data.frame(id=to, type="compound", stringsAsFactors=FALSE)
            )
        }
    }
    vertices <- unique(vertices)

    # if !includeCompounds then turn reactions data.frame into edges
    if (!includeCompounds) {
        if (nrow(reactions) > 1) {
            edges <- data.frame(from=character(0), to=character(0), directed=logical(0))
            for (i in 1:nrow(reactions)) {
                cc <- reactions[i, "to.compound"]
                ind <- reactions$from.compound == cc
                for (j in which (ind)) {
                    edges <- rbind(edges, 
                        data.frame(from=reactions[i, "reaction"], to=reactions[j, "reaction"], 
                        directed=TRUE, stringsAsFactors=FALSE))
                }
            }
        }
#        vertices <- data.frame(id=unique(rxns), type="reaction", stringsAsFactors=FALSE)
    } else {
#        vertices <- rbind(
#            data.frame(id=unique(rxns), type="reaction", stringsAsFactors=FALSE),
#            data.frame(id=unique(compounds), type="compound", stringsAsFactors=FALSE))
    }

    # Use igraph to get longest path from root for each node 
    # (for levels param to visNetwork)
    if (nrow(edges) > 0) {
        edges <- unique(edges)
        edges$weight <- -1
        g <- igraph::graph_from_data_frame(edges, vertices=vertices)
        g <- igraph::set.edge.attribute(g, "directed", value=TRUE)
        roots <- edges$from[1]
        
        #identify if there are multiple unconnected subgraphs
        connected.groups <- igraph::components(g, mode="weak")$membership
        if (length(unique(connected.groups)) > 1) {
            #NOTE: this assumes the "root" of each comoponent is the first
            #one listed in the reaction string, which is typically the case
            roots <- unlist(lapply(unique(connected.groups), function(i) {
                vv <- names(connected.groups[connected.groups==i])
                ind <- which.min(match(vv, vertices$id))
                root <- vv[ind]
                return(root)
            }))
        }
        
        #identify cycles (they break shortest path algorithm)
        if (!igraph::is.dag(g)) {
            g <- igraph::set.edge.attribute(g, "weight", value=1)
            d <- apply(igraph::distances(g, v=roots, to=vertices$id, mode="out"), 
                MARGIN=2, min, na.rm=TRUE)
           
            # give edges that create cycles a really large positive weight
            ind.from <- d[match(edges$from, vertices$id)]
            ind.to <- d[match(edges$to, vertices$id)]
            ind.back <- ind.to<ind.from
            #really a cycle?
            d2 <- unlist(lapply(which(ind.back), function(i) igraph::distances(g, edges$to[i], edges$from[i], mode="out")))
            ind.cycle <- which(ind.back)[which(is.finite(d2))]
            
            edges$weight <- -1
            edges$weight[ind.cycle] <- 800
            g <- igraph::graph_from_data_frame(edges, vertices=vertices)
            g <- igraph::set.edge.attribute(g, "directed", value=TRUE)
        }
        
        try ({
            level <- -apply(igraph::distances(g, v=roots, to=vertices$id, mode="out"), 
                MARGIN=2, min)
            vertices$level <- as.numeric(level)
            vertices$level[!is.finite(vertices$level)] <- 0
            
            edges <- dplyr::select(edges, -weight) #remove weight column
        })
    }
    return(list(vertices=vertices, edges=edges))
}