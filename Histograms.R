# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)

# Define the path to FinalDataWithAge.xlsx
file_path <- "FinalDataWithAge.xlsx"

# List of sheet names to import
sheet_names <- c("ALL", "EAS", "EUR", "SHARE")

ALL <- read_excel(file_path, sheet = "ALL")
EAS <- read_excel(file_path, sheet = "EAS")
EUR <- read_excel(file_path, sheet = "EUR")
SHARE <- read_excel(file_path, sheet = "SHARE")

options(scipen = 999)

# create histograms of mode age of mutation for all, EAS-specific, EUR-specific, and shared variants
ggplot(ALL, aes(x = AgeMode_MutYears)) +
  geom_histogram(binwidth = 24000, fill = "darkseagreen4", color = "black") +  # Adjust binwidth as necessary
  labs(title = paste("Distribution of the mode age of mutation of all variants"),
       x = "Mode age of mutation",
       y = "Frequency") +
  xlim(0, 800000) +
  ylim(0, 30) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 800000)) +  # Set x-axis to start at 0
  scale_y_continuous(expand = c(0, 0), limits = c(0, 30)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1), 
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "lines"))

ggplot(EAS, aes(x = AgeMode_MutYears)) +
  geom_histogram(binwidth = 24000, fill = "darkseagreen", color = "black") +  # Adjust binwidth as necessary
  labs(title = paste("Distribution of the mode age of mutation of EAS specific variants"),
       x = "Mode age of mutation",
       y = "Frequency") +
  xlim(0, 800000) +
  ylim(0, 15) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 800000)) +  # Set x-axis to start at 0
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1), 
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "lines"))

ggplot(EUR, aes(x = AgeMode_MutYears)) +
  geom_histogram(binwidth = 24000, fill = "darkseagreen", color = "black") +  # Adjust binwidth as necessary
  labs(title = paste("Distribution of the mode age of mutation of EUR specific variants"),
       x = "Mode age of mutation",
       y = "Frequency") +
  xlim(0, 800000) +
  ylim(0, 15) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 800000)) +  # Set x-axis to start at 0
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1), 
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "lines"))

ggplot(SHARE, aes(x = AgeMode_MutYears)) +
  geom_histogram(binwidth = 24000, fill = "darkseagreen", color = "black") +  # Adjust binwidth as necessary
  labs(title = paste("Distribution of the mode age of mutation of shared variants"),
       x = "Mode age of mutation",
       y = "Frequency") +
  xlim(0, 800000) +
  ylim(0, 15) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 800000)) +  # Set x-axis to start at 0
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1), 
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "lines"))  # Add a border around the plot

# read in face scatter data.csv which has derived allele substitution rates
derived <- read.csv('facescatterdata.csv')

# create histograms of derived allele substitution rates in EAS, EUR, and AFR
ggplot(derived, aes(x = EAS.SubRate)) +
  geom_histogram(binwidth=0.0008, fill = "paleturquoise3", color = "black") +  # Adjust binwidth as necessary
  labs(title = paste("Distribution of the derived allele substitution rate in EAS"),
       x = "Mode age of mutation",
       y = "Frequency") +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 0.03)) +  # Set x-axis to start at 0
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1), 
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "lines"))  # Add a border around the plot

ggplot(derived, aes(x = EUR.SubRate)) +
  geom_histogram(binwidth=0.0008, fill = "paleturquoise3", color = "black") +  # Adjust binwidth as necessary
  labs(title = paste("Distribution of the derived allele substitution rate in EUR"),
       x = "Mode age of mutation",
       y = "Frequency") +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 0.03)) +  # Set x-axis to start at 0
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1), 
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "lines"))  # Add a border around the plot

ggplot(derived, aes(x = AFR.SubRate)) +
  geom_histogram(binwidth=0.0008, fill = "paleturquoise3", color = "black") +  # Adjust binwidth as necessary
  labs(title = paste("Distribution of the derived allele substitution rate in AFR"),
       x = "Mode age of mutation",
       y = "Frequency") +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 0.03)) +  # Set x-axis to start at 0
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1), 
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "lines"))  # Add a border around the plot
