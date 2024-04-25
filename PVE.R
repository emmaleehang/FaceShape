# Load ggplot2
library(ggplot2)
# Read in "FacePVEData.csv" and "PopulationCharacteristic.csv" which contains
# information on the sum of PCs, sum of beta squared of PCs, sum of effects, and sum of effect squared

facePVE = read.csv("FacePVEData.csv")
popchar = read.csv("PopulationCharacteristic.csv")

# merge two tables
full = merge(facePVE, popchar, by.x = "RSID", by.y = "SNP")

# Subset where pop is EAS and char is diff
df_eas_diff <- full[full$population == "EAS" & full$characteristic == "differentiated", ]

# Subset where pop is EAS and char is share
df_eas_share <- full[full$population == "EAS" & full$characteristic == "shared", ]

# model that contains info on the R^2 and p-value, which will be printed on the graph
model <- lm(sumPC ~ EASAFRDiff, data = full)
r_squared <- summary(model)$r.squared

# Extract p-value for the slope (x)
p_value <- summary(model)$coefficients[2,4] 

# plot the sum PC by the difference in the AFR and EAS allele frequencies
ggplot(full, aes(x = EASAFRDiff, y = sumPC))+ 
  geom_point() + # Scatterplot
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Line of best fit with standard error
  theme_minimal() + 
  ggtitle("sumPC by AFR - EAS for all facial variants") +
  xlab("EAS-AFR") + 
  ylab("sumPC") + 
  annotate("text", x = Inf, y = Inf, label = sprintf("R² = %.2f, p = %.2g", r_squared, p_value), 
           hjust = 1.1, vjust = 2, size = 5, color = "black") 

# create this model with only EAS specific SNPs
model <- lm(sumPC ~ EASAFRDiff, data = df_eas_diff)
r_squared <- summary(model)$r.squared

# Extract p-value for the slope (x)
p_value <- summary(model)$coefficients[2,4] 

# this plot does not result in a significant p-value
ggplot(df_eas_diff, aes(x = EASAFRDiff, y = sumPC)) + 
  geom_point() +                          # Add a line
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Line of best fit with standard error
  theme_minimal() + 
  ggtitle("sumPC by EAS-AFR for EAS specific variants") +
  xlab("EAS-AFR") + 
  ylab("sumPC") + 
  annotate("text", x = Inf, y = Inf, label = sprintf("R² = %.2f, p = %.2g", r_squared, p_value), 
           hjust = 1.1, vjust = 2, size = 5, color = "black") 

# create this model with shared SNPs between EUR and EAS
model <- lm(sumPC ~ EASAFRDiff, data = df_eas_share)
r_squared <- summary(model)$r.squared

# Extract p-value for the slope (x)
p_value <- summary(model)$coefficients[2,4]

# this plot has the highest R^2 value and p-value
ggplot(df_eas_share, aes(x = EASAFRDiff, y = sumPC)) + 
  geom_point()+                          # Add a line
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Line of best fit with standard error
  theme_linedraw() + 
  ggtitle("Sum of Betas for 72 PCs for Whole Face by difference between AFR AA and EAS AA for shared variants") +
  xlab("AFR-EAS") + 
  ylab("sumPC") + 
  annotate("text", x = Inf, y = Inf, label = sprintf("R² = %.2f, p = %.2g", r_squared, p_value), 
           hjust = 1.1, vjust = 2, size = 5, color = "black") 



hist(df_eas_diff$EURPVE, xlim=c(0,0.016))    
hist(df_eas_share$EURPVE, xlim=c(0,0.016)) 

# create histograms of percent variance explained in each population (EAS, EUR, AFR)
# histograms are separated by PVE by EAS-specific variants and shared variants

ggplot(full, aes(x = EASPVE, fill = characteristic)) + 
  geom_histogram(col=I('chartreuse4'), binwidth = 0.0003) +
  scale_fill_manual(values = c("differentiated" = "lightgreen", "shared" = "darkgreen")) + # Customize colors
  theme_linedraw() +
  coord_cartesian(ylim = c(0, 35), xlim=c(0,0.015)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "PVE in EAS", 
       x = "EAS PVE", 
       y = "Frequency",
       fill = "Characteristic") 

ggplot(full, aes(x = EURPVE, fill = characteristic)) + 
  geom_histogram(col=I('darkturquoise'), binwidth = 0.0003) +
  scale_fill_manual(values = c("differentiated" = "lightblue", "shared" = "darkblue")) + # Customize colors
  theme_linedraw() + 
  coord_cartesian(ylim = c(0, 35), xlim=c(0,0.015)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "PVE in EUR", 
       x = "EUR PVE", 
       y = "Frequency",
       fill = "Characteristic") 

ggplot(full, aes(x = AFRPVE, fill = characteristic)) + 
  geom_histogram(col=I('khaki'), binwidth = 0.0003) +
  scale_fill_manual(values = c("differentiated" = "darkgoldenrod1", "shared" = "darkgoldenrod4")) + # Customize colors
  theme_linedraw() + 
  coord_cartesian(ylim = c(0, 35), xlim=c(0,0.015)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "PVE in AFR", 
       x = "AFR PVE", 
       y = "Frequency",
       fill = "Characteristic") 
