


###### Return links ########
#' zelda
#'
#' Takes two lists of genes and finds the receptor-ligand relations (links) between them.
#' Plots a bipartite plot of the found genes.
#' Returns a bipartite network of the connections.
#'
#' @param receptors A list of genes containing putative receptors
#' @param ligands A list of genes containg putative ligands
#' @param return_ggplot Flag to return a ggplot2 object instead of plotting. Defaults to FALSE.
#' @param plot_it Flag to plot bipartite network or not. Defaults to TRUE.
#' @param return_links Flag to return a dataframe with the links object instead of plotting. Defaults to FALSE.
#' @param database The database to be used to find the links. Defaults to "ramilowski_links".
#' Other options: "cellphone_db_links"; "all"
#' @import dplyr
#' @import ggplot2
#' @export
#' @examples
#'
#'  receptor_candidates <- c("CD3", "AIF", "CCR3", "CXCR3")
#'  ligand_candidates <- c("TP53", "NFKB1", "CXCL9")
#'  links <- return_links(receptors = receptor_candidates, ligands = ligand_candidates, database = "all")
#'  head(links)

return_links <-
  function(receptors, ligands, database = "ramilowski_links") {
    receptors <- unique(receptors)
    ligands <- unique(ligands)

    get_ramilowki_links <- function(receptors, ligands) {
      links <- ramilowski_links %>%
        filter(Receptor.ApprovedSymbol %in% receptors &
                 Ligand.ApprovedSymbol %in% ligands) %>%
        select(Receptor.ApprovedSymbol, Ligand.ApprovedSymbol)
      colnames(links) <- c("receptor", "ligand")
      links$database <- "ramilowski_links"
      return(links)
    }

    get_cellphone_db_links <- function(receptors, ligands) {
      links <-
        cellphone_db_links[cellphone_db_links$receptors %in% receptors &
                             cellphone_db_links$ligands %in% ligands,] %>%
        select(receptors, ligands)
      colnames(links) <- c("receptor", "ligand")
      links$database <- "cellphone_db_links"
      return(links)
    }

    if (database == "ramilowski_links") {
      links <- get_ramilowki_links(receptors, ligands)
    }
    if (database == 'cellphone_db_links') {
      links <- get_cellphone_db_links(receptors, ligands)
    }
    if (database == "all") {
      rami <- get_ramilowki_links(receptors, ligands)
      cellphone <- get_cellphone_db_links(receptors, ligands)
      links <- rbind(rami, cellphone)
    }

    if (nrow(links) < 1) {
      message('No links were found')
      return(NULL)
    }

    return(links)
  }




###### Plot bipartplot from link edgelist


add_types_to_graph <- function(graph_object) {
  adj <- as.matrix(get.adjacency(graph_object))
  adj <- adj[rowSums(adj[, -1]) != 0, ]
  adj <- adj[, colSums(adj) != 0]
  g <- graph.incidence(adj, weighted = T)
  return(g)
}

override_ggnetwork_layout <-
  function(ggnetwork_data_frame,
           igraph_plot_coordinates,
           igraph_plot_coordinates_as_edges
  ) {
    # add coordinates to ggnetwork generated data frame
    ggnetwork_data_frame[is.na(ggnetwork_data_frame$na.y), ]$x <-
      igraph_plot_coordinates$X1
    ggnetwork_data_frame[is.na(ggnetwork_data_frame$na.y), ]$xend <-
      igraph_plot_coordinates$X1
    ggnetwork_data_frame[is.na(ggnetwork_data_frame$na.y), ]$y <-
      igraph_plot_coordinates$X2
    ggnetwork_data_frame[is.na(ggnetwork_data_frame$na.y), ]$yend <-
      igraph_plot_coordinates$X2

    ggnetwork_data_frame[!is.na(ggnetwork_data_frame$na.y), ]$x <-
      igraph_plot_coordinates_as_edges$X1
    ggnetwork_data_frame[!is.na(ggnetwork_data_frame$na.y), ]$xend <-
      igraph_plot_coordinates_as_edges$X2
    ggnetwork_data_frame[!is.na(ggnetwork_data_frame$na.y), ]$y <-
      igraph_plot_coordinates_as_edges$Y1
    ggnetwork_data_frame[!is.na(ggnetwork_data_frame$na.y), ]$yend <-
      igraph_plot_coordinates_as_edges$Y2

    return(ggnetwork_data_frame)

  }

set_ggnetwork_node_names <- function(ggnetwork_data_frame, node_names){
  node_names_df <- data.frame(vertex.names = seq_along(node_names), node_names)
  ggnetwork_data_frame <- left_join(ggnetwork_data_frame, node_names_df)
  ggnetwork_data_frame$vertex.names <- ggnetwork_data_frame$node_names
  ggnetwork_data_frame$node_names <- NULL

  return(ggnetwork_data_frame)
}



#' plot_links
#'
#' Takes an edge list (from a bipartite network)
#' And plots a railway bipartite plot using ggplot2
#'
#' The link list can be regarded as a bipartite network.
#' Only the first two columns of the edgelist are used
#'
#' @param edgelist The edgelist corresponding to the bipartite network
#' @param return_ggplot Flag to return a ggplot2 object instead of plotting
#' @import igraph
#' @import network
#' @import ggplot2
#' @export
#' @examples
#'  receptor_candidates <- c("CD3", "AIF", "CCR3", "CXCR3")
#'  ligand_candidates <- c("TP53", "NFKB1", "CXCL9")
#'  links <-
#'    return_links(receptors = receptor_candidates,
#'                 ligands = ligand_candidates,
#'                 database = "all")
#'  head(links)
#'
#'  plot_links(links)
plot_links <- function(edgelist, return_ggplot = FALSE) {
  edgelist <- unique(as.matrix(edgelist[, c(1, 2)]))

  if (nrow(edgelist) == 1) {
    message('ggplot not available for 1 edge graphs in this version')
    if (return_ggplot == FALSE) {
      plot(graph_object)
      return(NULL)
    } else{
      stop('ggplot not available for 1 edge graphs in this version')
    }
  }

  receptors <- edgelist[, 1]
  ligands <- edgelist[, 2]
  igraph_object <- igraph::graph_from_edgelist(edgelist)
  igraph_object <- add_types_to_graph(igraph_object)
  igraph_plot_coordinates <-
    as.data.frame(layout_as_bipartite(igraph_object))

  colnames(igraph_plot_coordinates) = c("X1", "X2")

  node_numbers_as_edgelist <- igraph::get.edgelist(igraph_object)

  igraph_plot_coordinates_as_edges <-
    data.frame(igraph_plot_coordinates[node_numbers_as_edgelist[, 1], ],
               igraph_plot_coordinates[node_numbers_as_edgelist[, 2], ])
  colnames(igraph_plot_coordinates_as_edges) <-
    c("X1", "Y1", "X2", "Y2")

  node_names <- unique(c(receptors, ligands))
  igraph_plot_coordinates$label = node_names

  igraph_plot_coordinates$groups <-
    ifelse(igraph_plot_coordinates$X2 == 1, 'receptors', 'ligands')



  p <- ggplot(igraph_plot_coordinates) +
      geom_segment(data=igraph_plot_coordinates_as_edges, aes_(x=~X1, y=~Y1, xend=~X2, yend=~Y2),
                 size = 0.5, alpha=0.7, colour="black")+
    geom_point(aes_(x=~X1, y=~X2, size=10, alpha=.5), color='black') +
    geom_label(aes_(x=~X1, y=~X2, label =~label, colour =~groups)) +
    scale_alpha(guide = "none") +
    scale_size(guide = "none") +
    theme_blank() +
    labs(title="Receptors and ligands")

  p
  if (return_ggplot == FALSE) {
    plot(p)
  } else{
    return(p)
  }

}
