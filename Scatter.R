# import necessary packages
library(ggplot2)
library(cowplot)

# read scatter plot data
allscatter <- read.csv("facescatterdata.csv")
label_mapping <- c("black" = "Average substitution rate", "purple" = "Slower than average", "green" = "Faster than average")
color_mapping <- c(color_mapping <- c("black" = "black", "purple" = "purple", "green" = "green"))
options(scipen = 999)

# create 9 graphs based on coloring by population (EAS, EUR, AFR) and allele frequency by population (EAS, AFR, EUR)

#EAS EAS
EASEAS <- ggplot(allscatter, aes(y=AgeMode_MutYears, x=EAS.AncFreq, color = EAS.groups)) + 
  geom_point(size=5) +
  scale_color_manual(values = color_mapping, labels = label_mapping) +
  labs(title = "EAS Anc Allele Freq by Age of Mutation, Colored by EAS Rates", x = "EAS Ancestral Allele Frequency", y = "Mode Age of Mutation", , color = 'Derived Allele Substitution Rate in EAS') +
  theme_linedraw() +
  theme(legend.text = element_text(size = 10),  # Adjust text size in legend
                         legend.title = element_text(size = 10),
        legend.position = "bottom")
print(EASEAS)

#EAS EUR
EASEUR <- ggplot(allscatter, aes(y=AgeMode_MutYears, x=EUR.AncFreq, color = EAS.groups)) + 
  geom_point(size=5) +
  scale_color_manual(values = color_mapping, labels = label_mapping) +
  labs(title = "EUR Anc Allele Freq by Age of Mutation, Colored by EAS Rates", x = "EUR Ancestral Allele Frequency", y = "Mode Age of Mutation", color = 'Derived Allele Substitution Rate in EAS')  +
  theme_linedraw() +
  theme(legend.text = element_text(size = 10),  # Adjust text size in legend
        legend.title = element_text(size = 10),
        legend.position = "bottom")
print(EASEUR)

#EAS AFR
EASAFR <- ggplot(allscatter, aes(y=AgeMode_MutYears, x=AFR.AncFreq, color = EAS.groups)) +
  geom_point(size=5) +
  scale_color_manual(values = color_mapping, labels = label_mapping) +
  labs(title = "AFR Anc Allele Freq by Age of Mutation, Colored by EAS Rates", x = "AFR Ancestral Allele Frequency", y = "Mode Age of Mutation", color = 'Derived Allele Substitution Rate in EAS')  +
  theme_linedraw()  +
  theme(legend.text = element_text(size = 10),  # Adjust text size in legend
        legend.title = element_text(size = 10),
        legend.position = "bottom")
print(EASAFR)

#EAS combined
prow <- plot_grid(EASEAS + theme(legend.position="none"), EASEUR + theme(legend.position="none"), 
                      EASAFR + theme(legend.position="none"), ncol = 3, rel_widths = c(1, 1, 1),
                      align = 'vh',
                      labels = c("A", "B", "C"),  # Labels for the legend
                      hjust = -1, nrow = 1)

legend_b <- get_legend(
  EASEAS + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

combined <- plot_grid(prow, legend_b,  ncol = 1, rel_heights = c(1, .1))

print(combined)


#EUR EAS
EUREAS <- ggplot(allscatter, aes(y=AgeMode_MutYears, x=EAS.AncFreq, color = EUR.groups)) + 
  geom_point(size=5) +
  scale_color_manual(values = color_mapping, labels = label_mapping) +
  labs(title = "EAS Anc Allele Freq by Age of Mutation, Colored by EUR Rates", x = "EAS Ancestral Allele Frequency", y = "Mode Age of Mutation", color = 'Derived Allele Substitution Rate in EUR')  +
  theme_linedraw()  +
  theme(legend.text = element_text(size = 10),  # Adjust text size in legend
        legend.title = element_text(size = 10),
        legend.position = "bottom")
print(EUREAS)

#EUR EUR
EUREUR <- ggplot(allscatter, aes(y=AgeMode_MutYears, x=EUR.AncFreq, color = EUR.groups)) + 
  geom_point(size=5) +
  scale_color_manual(values = color_mapping, labels = label_mapping) +
  labs(title = "EUR Anc Allele Freq by Age of Mutation, Colored by EUR Rates", x = "EUR Ancestral Allele Frequency", y = "Mode Age of Mutation", color = 'Derived Allele Substitution Rate in EUR')  +
  theme_linedraw()  +
  theme(legend.text = element_text(size = 10),  # Adjust text size in legend
        legend.title = element_text(size = 10),
        legend.position = "bottom")
print(EUREUR)

#EUR AFR
EURAFR <- ggplot(allscatter, aes(y=AgeMode_MutYears, x=AFR.AncFreq, color = EUR.groups), labels = label_mapping) + 
  geom_point(size=5) +
  scale_color_manual(values = color_mapping, labels = label_mapping) +
  labs(title = "AFR Anc Allele Freq by Age of Mutation, Colored by EUR Rates", x = "AFR Ancestral Allele Frequency", y = "Mode Age of Mutation", color = 'Derived Allele Substitution Rate in EUR')  +
  theme_linedraw()  +
  theme(legend.text = element_text(size = 10),  # Adjust text size in legend
        legend.title = element_text(size = 10),
        legend.position = "bottom")
print(EURAFR)

#EUR combined
prow <- plot_grid(EUREAS + theme(legend.position="none"), EUREUR + theme(legend.position="none"), 
                  EURAFR + theme(legend.position="none"), ncol = 3, rel_widths = c(1, 1, 1),
                  align = 'vh',
                  labels = c("D", "E", "F"),  # Labels for the legend
                  hjust = -1, nrow = 1)

legend_b <- get_legend(
  EUREUR + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

combined <- plot_grid(prow, legend_b,  ncol = 1, rel_heights = c(1, .1))

print(combined)

#AFR EAS
AFREAS <- ggplot(allscatter, aes(y=AgeMode_MutYears, x=EAS.AncFreq, color = AFR.groups)) + 
  geom_point(size=5) +
  scale_color_manual(values = color_mapping, labels = label_mapping) +
  labs(title = "EAS Anc Allele Freq by Age of Mutation, Colored by AFR Rates", x = "EAS Ancestral Allele Frequency", y = "Mode Age of Mutation", color = 'Derived Allele Substitution Rate in AFR')  +
  theme_linedraw()  +
  theme(legend.text = element_text(size = 10),  # Adjust text size in legend
        legend.title = element_text(size = 10),
        legend.position = "bottom")
print(AFREAS)

#AFR EUR
AFREUR <- ggplot(allscatter, aes(y=AgeMode_MutYears, x=EUR.AncFreq, color = AFR.groups)) + 
  geom_point(size=5) +
  scale_color_manual(values = color_mapping, labels = label_mapping) +
  labs(title = "EUR Anc Allele Freq by Age of Mutation, Colored by AFR Rates", x = "EUR Ancestral Allele Frequency", y = "Mode Age of Mutation", color = 'Derived Allele Substitution Rate in AFR')  +
  theme_linedraw()  +
  theme(legend.text = element_text(size = 10),  # Adjust text size in legend
        legend.title = element_text(size = 10),
        legend.position = "bottom")
print(AFREUR)

#AFR AFR
AFRAFR <- ggplot(allscatter, aes(y=AgeMode_MutYears, x=AFR.AncFreq, color = AFR.groups), labels = label_mapping) + 
  geom_point(size=5) +
  scale_color_manual(values = color_mapping, labels = label_mapping) +
  labs(title = "AFR Anc Allele Freq by Age of Mutation, Colored by AFR Rates", x = "AFR Ancestral Allele Frequency", y = "Mode Age of Mutation", color = 'Derived Allele Substitution Rate in AFR')  +
  theme_linedraw()  +
  theme(legend.text = element_text(size = 10),  # Adjust text size in legend
        legend.title = element_text(size = 10),
        legend.position = "bottom")
print(AFRAFR)

#AFR combined
prow <- plot_grid(AFREAS + theme(legend.position="none"), AFREUR + theme(legend.position="none"), 
                  AFRAFR + theme(legend.position="none"), ncol = 3, rel_widths = c(1, 1, 1),
                  align = 'vh',
                  labels = c("G", "H", "I"),  # Labels for the legend
                  hjust = -1, nrow = 1)

legend_b <- get_legend(
  AFREUR + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

combined <- plot_grid(prow, legend_b,  ncol = 1, rel_heights = c(1, .1))

print(combined)

# for other traits, the code is identical but uses a different dataset. 
# Download the specific data set with 'full' in its title from the spreadsheet 'OtherTraitsAgeData.xlsx' and import the trait as CSV files
