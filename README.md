# zelda 

A package to find links between two lists of genes. One of them has receptors, the other has ligands. This package makes the match.


# Citation

Zelda uses datasets from two different publications. If you use it in your analysis, please cite the original publications. 

For the "ramilowski_links" dataset please cite "Ramilowski, Jordan A., et al. "A draft network of ligand–receptor-mediated multicellular signalling in human." Nature communications 6 (2015): 7866." 

For the "cellphone_db_links" dataset please cite "Efremova, Mirjana, et al. "CellPhoneDB v2. 0: Inferring cell-cell communication from combined expression of multi-subunit receptor-ligand complexes." bioRxiv (2019): 680926."





# Dataset preparation 

```
library(biomaRt)
library(readxl)
library(data.table)

###### Prepare table from Ramilowski et al, 2015 #####
file_url <- 'https://media.nature.com/original/nature-assets/ncomms/2015/150722/ncomms8866/extref/ncomms8866-s3.xlsx'
download.file(file_url,
              destfile = './data/bridges_from_ncomms15107.xlsx', method = 'wget')
ramilowski_links <- read_excel('./data/bridges_from_ncomms15107.xlsx', sheet = 2)
save('ramilowski_links', file = './data/ramilowski_links.RData')

####### Prepare table from CellPhoneDB ####

file_url <- "https://www.cellphonedb.org/downloads/interactions_cellphonedb.csv"
download.file(file_url,
              destfile = './data/bridges_from_cellphone_db', method = 'wget')
cellphone_db_links <- data.frame(fread('./data/bridges_from_cellphone_db'))
cellphone_db_links[] <- lapply(cellphone_db_links, function(x) gsub("simple:", "", x))
cellphone_db_links <- cellphone_db_links[grep('complex',cellphone_db_links$partner_a, invert = T), ]


listMarts()
ensembl=useMart("ensembl")
ensembl = useDataset("hsapiens_gene_ensembl",mart=ensembl)
attributes = listAttributes(ensembl)
uniprot_to_gene_symbol<- getBM(attributes = c('uniprot_gn','uniprotswissprot','hgnc_symbol'),   mart = ensembl)

cellphone_db_links$entry_name_a[!(cellphone_db_links$partner_a %in% uniprot_to_gene_symbol$uniprotswissprot)]
cellphone_db_links$ligands <- uniprot_to_gene_symbol$hgnc_symbol[match(cellphone_db_links$partner_a, uniprot_to_gene_symbol$uniprotswissprot)]
cellphone_db_links$receptors <- uniprot_to_gene_symbol$hgnc_symbol[match(cellphone_db_links$partner_b, uniprot_to_gene_symbol$uniprotswissprot)]

save('cellphone_db_links', file = './data/cellphone_db_links.RData')

```


#  List of works that use receptor-ligand databases 

### Applying databases

Database of Ligand-Receptor Partners (DLRP), IUPHAR and Human Plasma Membrane Receptome (HPMR) are included via Ramilowski db.,  but only until 2015. 

* In [https://www.sciencedirect.com/science/article/pii/S221112471831636X?via%3Dihub#bib33](Kumar, M. P. et al. Analysis of Single-Cell RNA-Seq Identifies Cell-Cell Communication Associated with Tumor Characteristics. Cell Rep. 25, 1458–1468.e4 (2018)), they use the Ramilowski db with addition of known B7 family member interactions from Southan et al., 2016.


* In [https://www.ncbi.nlm.nih.gov/pubmed/25704820](Transcriptome Analysis of Individual Stromal CellPopulations Identifies Stroma-Tumor Crosstalk in Mouse Lung Cancer Model) they claim to have established a database of 1433 interactions, but I was not able to find it anywhere. 


*  In [https://www.nature.com/articles/nature22796](Camp, J. G. et al. Multilineage communication regulates human liver bud development from pluripotency. Nature 546, 533–538 (2017).) they use the database by Ramilowski et al.

*  In [https://www.sciencedirect.com/science/article/pii/S2211124718315043?via%3Dihub](Joost, S. et al. Single-Cell Transcriptomics of Traced Epidermal and Hair Follicle Stem Cells Reveals Rapid Adaptations during Wound Healing. Cell Rep. 25, 585–597.e7 (2018).) they use the database by Ramilowski et al.

* In [https://www.nature.com/articles/s41592-018-0009-z#Sec8](Boisset, J.-C. et al. Mapping the physical network of cellular interactions. Nat. Methods 15, 547–553 (2018).
19.) they do not use receptor-ligand information to infer cell-cell interaction (but by its title, it could have been the case)


### Describing databases 

* [https://doi.org/10.1093/nar/gkv1037](The IUPHAR/BPS Guide to PHARMACOLOGY in 2016: towards curated quantitative interactions between 1300 protein targets and 6000 ligands)
(Southan et al, Nucleic Acids Research, Volume 44, Issue D1, 4 January 2016)

* [https://www.nature.com/articles/ncomms8866](Ramilowski, Jordan A., et al. "A draft network of ligand–receptor-mediated multicellular signalling in human." Nature communications 6 (2015): 7866.)

* [https://www.biorxiv.org/content/10.1101/680926v1](Efremova, Mirjana, et al. "CellPhoneDB v2. 0: Inferring cell-cell communication from combined expression of multi-subunit receptor-ligand complexes." bioRxiv (2019): 680926)