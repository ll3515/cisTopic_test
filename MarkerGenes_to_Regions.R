# hpc
suppressWarnings(library(cisTopic))
library(colorout)
source("/rds/general/user/ll3515/home/SCRATCH/ax4/tools/R_functions/000.R_functions.R")
source("/rds/general/user/ll3515/home/SCRATCH/ax4/tools/R_functions/Liisi_functions.R")

setwd("/rds/general/user/ll3515/home/DATA/scATACseq/atac_v1.2_mm_P50_brain_fresh_5k")

load("cisTopicObject.RData")
GeneRegions = cisTopicObject@region.data
head(GeneRegions)

load("Zeisel_CellType_MM.RData")
Head(hmscDF)

# Zeisel to gene names
markerZ = list()
for(i in names(hmscDF)){
  markerZ[[i]]<-names(hmscDF[[i]])
}
str(markerZ)

# Gene regions promoter
geneNames = unique(GeneRegions$SYMBOL)

# extract the region closest to TSS for all genes
regions = list()
for(i in 1:length(geneNames)){
  x=GeneRegions[which(GeneRegions$SYMBOL == geneNames[[i]]),c(1:3,15,17)]
  regions[[geneNames[i]]]=x[which(abs(x$distanceToTSS)==min(abs(x$distanceToTSS))),]
}
Head(regions)

# unlist into dataframe
tmp = rbind(regions[[1]],regions[[2]])
for(j in 3:length(regions)){
  tmp = rbind(tmp, regions[[j]])
}
Head(tmp)
# test
#GeneRegions[which(GeneRegions$SYMBOL == "Rp1"),c(1:3,15,17)]

closestGene = tmp
#save(closestGene, file = "closestGeneRegions.RData")
load("closestGeneRegions.RData")

# Marker genes to regions
for(m in names(markerZ)){
  regionList = closestGene[closestGene$SYMBOL %in% markerZ[[m]],]
  write.delim(markerZ, file =paste0("/Bulk_peaks/",m,".closestGene"))
}
