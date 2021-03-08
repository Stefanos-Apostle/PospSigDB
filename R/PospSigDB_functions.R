# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


## Function to load in tab delim gene sets from .gmt formatted file
load_gmt <- function(tab_delim_gmt) {
  set <- read_delim(tab_delim_gmt, delim = "\n", col_names = FALSE)
  gmt_geneset_list = list()
  unlisted <- c()
  i = 1
  while (i <= length(set[[1]])){
    x = set[[1]][i]
    y = strsplit(x, "\t")
    l = length(y[[1]])
    sets = y[[1]][3:l]
    unlisted <- c(unlisted, sets)
    name = y[[1]][1]
    gmt_geneset_list[[name]] = sets
    i = i + 1
  }
  return(gmt_geneset_list)
}


## Function to write .gmt files from gene sets to be moved to /secondary/projects/pospisilik/PospSigDB/
write_gmt <- function(title, description, genes, path=".") {
  nt <- toupper(title)
  nt <- strsplit(nt, split = " ")[[1]]
  nt <- paste(nt, collapse = "_")
  contents <- c(nt, paste(">", description, sep = " "), genes)
  write.table(x = t(as.data.frame(contents)), file = paste(paste(path, "/", sep=''), nt, ".gmt", sep = ""), sep = "\t",
              row.names = F, col.names = F, quote=F)
}




## Function to read .rnk file
## returns rnk list formatted for use within R.
read_RNK <- function(rnk_file) {
  x <- read.delim(file = rnk_file, sep = "\t")
  print(colnames(x)[1])
  rnk_stat <- x[,2]
  names(rnk_stat) <- x[,1]
  rnk_stat <- rnk_stat[order(rnk_stat, decreasing = T)]
  return(rnk_stat)
}



## Function from NixSEA to write .rnk file
## Writes description of gene set in first row since it will be ignored
write_RNK <- function (file, RNK_file, description="gene", species = "Human")
{
  if ((species %in% c("Human", "Mouse")) == FALSE) {
    stop("Species must be 'Human' or 'Mouse'.")
  }
  spec_dataset <- if (species == "Human") {
    "hsapiens_gene_ensembl"
  }
  else if (species == "Mouse") {
    "mmusculus_gene_ensembl"
  }
  symbol <- if (species == "Human") {
    "hgnc_symbol"
  }
  else if (species == "Mouse") {
    "mgi_symbol"
  }
  ensembl = useMart("ensembl", dataset = spec_dataset)
  genemap <- getBM(attributes = c("ensembl_gene_id", "entrezgene_id",
                                  symbol), filters = "entrezgene_id", values = names(RNK_file),
                   mart = ensembl)
  idx <- match(names(RNK_file), genemap$entrezgene_id)
  conv_syms <- genemap[, which(colnames(genemap) == symbol)][idx]
  wRNK <- data.frame(description = conv_syms, stat = RNK_file)
  write.table(wRNK, file, quote = F, sep = "\t", eol = "\r",
              row.names = F)
}



