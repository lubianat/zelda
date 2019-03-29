#download stuff
file_url <- 'https://media.nature.com/original/nature-assets/ncomms/2015/150722/ncomms8866/extref/ncomms8866-s3.xlsx'
download.file(file_url,
              destfile = './data/bridges_from_ncomms15107.xlsx', method = 'wget')
library(readxl)
excel_sheets('./data/bridges_from_ncomms15107.xlsx')
ramilowski_links <- read_excel('./data/bridges_from_ncomms15107.xlsx', sheet = 2)

save('ramilowski_links', file = './data/ramilowski_links.RData')
