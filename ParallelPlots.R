# load in necessary packages
library(stringr)
library(ggplot2)
library(GGally)
# the code is the same for all, EAS, EUR, and shared SNPs
# adjust the paths and names of the figures according to 
# the specific SNP characteristic you are aiming for 

# read in data on derived alleles and regions of the face this is affecting

DerivedAlleles <-data.frame(read.csv("~/Desktop/EvolutionRedo/Data/CSVs/ALLsnpparaplot.csv"))
View(DerivedAlleles)

regions <- list("nose", "forehead", "maxillary", 
                "glabella", "lower mouth", "upper mouth", 
                "eye", "tempora", "zygoma", "mandible")


for(region in regions) {
  assign(paste0(("ALLDerParaPlot_"),region),data.frame(subset(DerivedAlleles, grepl(get("region"), DerivedAlleles$Associated.Regions))))
  #write.csv(get(paste0(("Age_"),region)), paste0("~/Documents/TotalAlleleDataTables/","Total_",region,"DataTA.csv"), row.names = FALSE)
}

# make parallel plots for all SNPs
ggparcoord(DerivedAlleles, columns = 10:12, scale = "globalminmax", showPoints = TRUE) + 
  ggtitle("All SNPs") + 
  ylab('Derived Allele Frequncy') +
  xlab('Population') +
  theme_linedraw()

# make parallel plots for each facial region

for (df in regions) {
  name_d <- paste0("ALLDerParaPlot_", df)
  plotname <- paste0("All SNPs affecting ", df, " expression")
  p <- ggparcoord(get(name_d), columns = 10:12, scale = "globalminmax", showPoints = TRUE) + 
    ggtitle(plotname) +
    ylab('Derived Allele Frequncy') +
    xlab('Population') +
    theme_linedraw()
  assign(name_d, p)
  print(p)
  #ggsave(filename = paste0(name_d, ".jpeg"),plot = p)
}

