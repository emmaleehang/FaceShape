# Import Necessary Packages
library(readxl)
library(ggplot2)

#Import data with age of SNPs to create data visualization plots
EvolutionAgeSNPs <- "FinalDataWithAge.xlsx"
sheets <- excel_sheets(EvolutionAgeSNPs)


# Loop through each sheet name and read the sheet into a data frame
for (sheet in sheets) {
  assign(sheet, read_excel(EvolutionAgeSNPs, sheet = sheet))
}

# this ensures that the numbers on the axes are not in scientific format
options(scipen = 999)

# create histograms of the mode age of mutation for all, EAS-specific, EUR-specific, and shared variants
ggplot(ALL, aes(x = AgeMode_MutYears)) + 
  geom_histogram(color = "black", fill = "darkseagreen4") +
  theme_linedraw() +
  labs(title = "Distribution of Mode Age of Mutation of All Variants", x = "Mode Age of Mutation", y = "Frequency") +
  coord_cartesian(ylim = c(0, 30), xlim = c(0, 800000)) +
  scale_x_continuous(expand = c(0, 0)) + # Remove gap for x-axis
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.margin = margin(t = 10, r = 20, b = 10, l = 10, unit = "pt"))

ggplot(EAS, aes(x = AgeMode_MutYears)) + 
  geom_histogram(color = "black", fill = "darkseagreen3") +
  theme_linedraw() +
  labs(title = "Distribution of Mode Age of Mutation of EAS Variants", x = "Mode Age of Mutation", y = "Frequency") +
  coord_cartesian(ylim = c(0, 15), xlim = c(0, 800000)) +
  scale_x_continuous(expand = c(0, 0)) + # Remove gap for x-axis
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.margin = margin(t = 10, r = 20, b = 10, l = 10, unit = "pt"))


ggplot(EUR, aes(x = AgeMode_MutYears)) + 
  geom_histogram(color = "black", fill = "darkseagreen3") +
  theme_linedraw() +
  labs(title = "Distribution of Mode Age of Mutation of EUR Variants", x = "Mode Age of Mutation", y = "Frequency") +
  coord_cartesian(ylim = c(0, 15), xlim = c(0, 800000)) +
  scale_x_continuous(expand = c(0, 0)) + # Remove gap for x-axis
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.margin = margin(t = 10, r = 20, b = 10, l = 10, unit = "pt"))

ggplot(SHARE, aes(x = AgeMode_MutYears)) + 
  geom_histogram(color = "black", fill = "darkseagreen3") +
  theme_linedraw() +
  labs(title = "Distribution of Mode Age of Mutation of Shared Variants", x = "Mode Age of Mutation", y = "Frequency") +
  coord_cartesian(ylim = c(0, 15), xlim = c(0, 800000)) +
  scale_x_continuous(expand = c(0, 0)) + # Remove gap for x-axis
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.margin = margin(t = 10, r = 20, b = 10, l = 10, unit = "pt"))

face <- as.numeric(as.character(unlist(ALL['AgeMode_MutYears'])))
# read in ages of SNPs associated with bmi, breast cancer, ibd, neuroticism, prostate cancer, schizophrenia, and BMI adjusted waist-to-hip ratio
bmi <- as.numeric(as.character(unlist(read.csv('NonFaceAge.csv')['BMI'])))
brca <- as.numeric(as.character(unlist(read.csv('NonFaceAge.csv')['BrCa'])))
ibd <- as.numeric(as.character(unlist(read.csv('NonFaceAge.csv')['IBD'])))
neur <- as.numeric(as.character(unlist(read.csv('NonFaceAge.csv')['Neur'])))
prca <- as.numeric(as.character(unlist(read.csv('NonFaceAge.csv')['PrCa'])))
schiz <- as.numeric(as.character(unlist(read.csv('NonFaceAge.csv')['Schiz'])))
whr <- as.numeric(as.character(unlist(read.csv('NonFaceAge.csv')['WHR'])))

# kolmogorov-smirnov tests for distribution of face shape against other traits
ks.test(face, bmi)
# p-value = 0.5358

ks.test(face, brca)
# p-value = 0.9686

ks.test(face, ibd)
# p-value = 0.8436

ks.test(face, neur)
# p-value = 0.4284

ks.test(face, prca)
# p-value = 0.1103

ks.test(face, schiz)
# p-value = 0.1108

ks.test(face, whr)
# p-value = 0.009238

# create density plots of face against other traits
agedata <- read.csv('NonFaceAge.csv')
trait_colors <- c('Face' = 'black', 'BrCa' = 'lightpink', 
                  'BMI' = 'mediumpurple', 'IBD' = 'orange', 'Neur' = 'lightblue', 
                  'PrCa' = 'brown', 'Schiz' = '#FFFF00', 'WHR' = 'lightgreen')
ggplot(agedata, aes(x = Face)) +
  geom_density(aes(x = Face, color = 'Face'), size = 1.5) +
  geom_density(aes(x = BrCa, color = 'BrCa')) +
  geom_density(aes(x = BMI, color = 'BMI')) +
  geom_density(aes(x = IBD, color = 'IBD')) +
  geom_density(aes(x = Neur, color = 'Neur')) +
  geom_density(aes(x = PrCa, color = 'PrCa')) +
  geom_density(aes(x = Schiz, color = 'Schiz')) +
  geom_density(aes(x = WHR, color = 'WHR')) +
  labs(title = "Density plots of various common GWAS traits",
       x = "Age of Mutation of SNPs Affecting Traits",
       y = "Density",
       color = "Trait") +
  scale_color_manual(values = trait_colors, breaks = names(trait_colors), 
                     labels = names(trait_colors)) +  
  guides(color = guide_legend(override.aes = list(fill = trait_colors, color = trait_colors))) +
  scale_x_continuous(expand = c(0, 0), limits = c(0,1000000)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.000004)) +
  theme_linedraw()

# create density plots of EAS-specific variant age and EUR-specific variant age
trait_colors2 <- c('EAS Face' = 'seagreen', 'EUR Face' = 'blue')

options(scipen = 999)
ggplot(agedata, aes(x = FaceEAS)) +
  geom_density(aes(x = FaceEAS, color = 'EAS Face')) +
  geom_density(aes(x = FaceEUR, color = 'EUR Face')) +
  #geom_density(aes(x = FaceShare, color = 'Share')) +
  labs(title = "Density plots of East Asian and European Face Shape",
       x = "Age of Facial SNPs",
       y = "Density",
       color = "Trait") +
  scale_color_manual(values = trait_colors2, breaks = names(trait_colors2), 
                     labels = names(trait_colors2)) +  
  guides(color = guide_legend(override.aes = list(fill = trait_colors2, color = trait_colors2))) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1000000)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.000004)) +
  theme_linedraw()
