
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
#' Other options: "cellphone_db_links"
#' @import dplyr
#' @import ggplot2
#' @export
#' @examples
#' data(degs)
#' degs_alzheimer <- degs$degs_alzheimer
#' degs_periodontitis <- degs$degs_periodontitis
#' p <- zelda(receptors = degs_alzheimer, ligands = degs_periodontitis, return_ggplot = TRUE, plot_it = FALSE)
#' p
#' links <- p <- zelda(receptors = degs_alzheimer, ligands = degs_periodontitis, plot_it = FALSE, return_links = TRUE)
#' head(links)

zelda <- function(receptors, ligands, database = "ramilowski_links", return_ggplot = FALSE, plot_it = TRUE, return_links = FALSE){
  if (return_ggplot == TRUE & return_links == TRUE ){
    stop("You have to choose either return_ggplot or return_links to be true, but not both")
  }

  if (return_ggplot == FALSE & return_links == FALSE & plot_it == FALSE){
    stop("This function is doing nothing. Maybe you should set something to TRUE.")
  }

  receptors <- unique(receptors)
  ligands <- unique(ligands)
  if (database == "ramilowski_links"){
    links <- ramilowski_links %>%
      filter(Receptor.ApprovedSymbol %in% receptors & Ligand.ApprovedSymbol %in% ligands )
  }
  if (database == 'cellphone_db_links'){
    links <- cellphone_db_links[cellphone_db_links$receptors %in% receptors & cellphone_db_links$ligands %in% ligands, ]
  }

  if(nrow(links)<1){
    message('No links were found')
    return(NULL)
  }

  edgelist <- as.matrix(links[,c("Ligand.ApprovedSymbol", "Receptor.ApprovedSymbol")])
  if (plot_it == TRUE){
    bipartplot(edgelist, return_ggplot = FALSE)
  }
  if (return_ggplot == TRUE){
    return(bipartplot(edgelist, return_ggplot = TRUE))
  }
  if (return_links == TRUE){
    edgelist <- as.data.frame(edgelist)
    colnames(edgelist) <-c('receptors', 'ligands')
    return(edgelist)
  }


}

