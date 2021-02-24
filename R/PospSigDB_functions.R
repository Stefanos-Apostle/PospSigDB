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













