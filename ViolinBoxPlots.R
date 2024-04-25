# load in necessary packages
library(stringr)
library(ggplot2)
library(ggthemes)
# the code is the same for all, EAS, EUR, and shared SNPs
# adjust the paths and names of the figures according to 
# the specific SNP characteristic you are aiming for 

# read in data on derived alleles and regions of the face this is affecting
DerivedAlleles <- data.frame(read.csv("/Path/to/ViolinBoxPlots.csv"))
View(DerivedAlleles)
regions <- list("nose", "forehead", "maxillary", 
                "glabella", "lower mouth", "upper mouth", 
                "eye", "tempora", "zygoma", "mandible")
#for loop to make data tables of SNPs for each facial region, can also create CSVs
for(region in regions) {
  assign(paste0(("ALLderived_"),region),data.frame(subset(DerivedAlleles, grepl(get("region"), DerivedAlleles$AssociatedRegions))))
  #write.csv(get(paste0(("derived_"),region)), paste0("~/DeskTop/Gibson Lab/EvolutionaryDataTables/","derived_",region,"DataTA.csv"), row.names = FALSE)
}

one.way <- aov(Der.Frequency ~ Population, data = DerivedAlleles)
summary(one.way)

# make histogram of anc allele frequencies
ggplot(DerivedAlleles, aes(x = Der.Frequency)) +
  ggtitle('Distribution of Derived Allele Frequencies for All Facial SNPs Across Populations') +
  geom_histogram(aes(fill = Population), color = 'black') +
  theme_linedraw() +
  scale_y_continuous(limit = c(0, 65), expand = c(0,0)) +
  scale_x_continuous(expand=c(0,0)) +
  xlab('Derived Allele Frequency') + ylab('Count') +
  scale_fill_manual(values = c('#D80551','#13A357','#0D69D0'))

# make violin box plots for each facial region
# All SNPs
ggplot(DerivedAlleles, aes(x = Population, y = Der.Frequency, color = Population)) +
  geom_violin() + ylab('Frequencies of Derived Allele') +
  ylim(0,1) +
  scale_color_manual(values = c('#D80551','#13A932','#0D69D0')) +
  ggtitle('All SNPs') + 
  geom_jitter() +
  geom_boxplot(width=0.3) + 
  theme_linedraw()

#for loop to make ggplots of each facial region
for (df in regions) {
  name_d <- paste0("ALLderived_", df)
  plotname <- paste0("All SNPs affecting ", df, " expression")
  p <- ggplot(get(name_d), aes(x = Population, y = Der.Frequency, color = Population)) +
    geom_violin() + ylab('Frequencies of Derived Allele') +
    ylim(0,1) +
    scale_color_manual(values = c('#D80551','#13A932','#0D69D0')) +
    ggtitle(plotname) + 
    geom_jitter() +
    geom_boxplot(width=0.3) + 
    theme_linedraw()
  print(p)
  assign(name_d, p)
  
  #ggsave(filename = paste0(name_d, ".jpeg"),plot = p)
}
