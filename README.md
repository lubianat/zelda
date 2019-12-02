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
