
args     <- commandArgs(trailingOnly = TRUE)
treePath <- args[1]

if(is.na(treePath)){ stop("Invalid path") }

# don't know tree name apriori
obj          <- load(treePath)
originalTree <- get(obj)
rm(list = obj)

if(as.character(class(originalTree)) != "BinaryTree"){
  stop("You must provide a BinaryTree to this process!")
}

t<- originalTree@tree
rm(originalTree)

treeMap <- createBinaryTreeMap(t)

# save
fileName <- tools::file_path_sans_ext(basename(treePath))
save(treeMap, file = gsub(fileName,  paste0(fileName,"_exported"), treePath))

