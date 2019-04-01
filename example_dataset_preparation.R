degs_alzheimer <- GSE110226_ALZxCTR_limma_DEGS$Gene.symbol
degs_periodontitis <- GSE61490_PERIODONTITISxCTRl_BioJupies_DEGS_TOP_1000$gene_symbol

degs <- list(degs_alzheimer = degs_alzheimer, degs_periodontitis = degs_periodontitis)

save(degs, './data/degs.RData')
