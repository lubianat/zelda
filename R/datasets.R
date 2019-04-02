NULL

#' Ligand-receptor interaction dataframe
#'
#' Dataframe of ligand-receptor interactions curated by Ramilowski et al, 2015
#'Ref: https://www.nature.com/articles/ncomms8866
#'

#' @name ramilowski_links
#' @docType data
#' @usage data(ramilowski_links)
#' @format data frame
#' @keywords datasets, links, ramilowski
#' @references Ramilowski, Jordan A., et al. "A draft network of
#' ligand–receptor-mediated multicellular signalling in human."
#' Nature communications 6 (2015): 7866.
#' \href{https://www.ncbi.nlm.nih.gov/pubmed/26198319}{PubMed}
"ramilowski_links"



#' Sample genes for example
#'
#' Sets of differentially expressed genes (DEGs) from two
#' different studies: GSE110226 (Alzheimer's disease DEGs in the brain)
#' and GSE61490(Chronic Periodontitis DEGs in monocytes)
#'

#' @name degs
#' @docType data
#' @usage data(degs)
#' @format data frame
#' @keywords degs
"degs"


#' Simulation of zelda 100000 times
#'
#'A 100 000 simulation of randomly sampled genes from a 13000 gene pool to
#' obtain estimates of links by random selection of genes.
#' This estimate was done with two random lists of 1000 genes.
#'

#' @name kxk_simulation
#' @docType data
#' @usage data(kxk_simulation)
#' @format Large numeric
#' @keywords simulation
"kxk_simulation"
