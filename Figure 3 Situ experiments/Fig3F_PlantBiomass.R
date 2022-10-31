### Biomass
## packages
library(readxl)
library(reshape2)
library(dplyr)
library(ggplot2)
library(ggpubr)

## import data
CrackPlant <- read_excel("Spatial self-organization in the Yellow River Delta-jiaguo Yan & Johan Van de Koppel 20180404.xlsx", 'Full Factorial experiment')
CrackPlant <- CrackPlant[which(CrackPlant$Density != 0),]
#CrackPlant <- CrackPlant[which(CrackPlant$Treatments == 'Crack (+), plant (+)'),]
CrackPlant <- dplyr::select(CrackPlant, Treatments, Above_ground_biomass, Below_ground_biomass, Biomass)#, Crack_width, Hardness, Salinity, water)

CrackPlant$Treatments = factor(CrackPlant$Treatments, levels=c('Crack (+), plant (+)','Crack (-), plant (+)')) 
df <- melt(CrackPlant, id="Treatments", variable.name = "Attribute", value.name = "Size")
df$Attribute = factor(df$Attribute, levels=c('Biomass', 'Above_ground_biomass', 'Below_ground_biomass')) 

############################ p: Biomass ############################
library(tidyverse)
df<- df %>% filter(!is.na(df$Attribute))

p <- ggbarplot(df, x = "Attribute", y = "Size", add = "mean_se",
               color = "Treatments", fill = "Treatments", alpha = 0.1, palette = "jco", #add = "jitter",
               xlab = "Treatments", ylab = "Plant biomass (dry g)",
               position = position_dodge(0.8)) + 
  scale_x_discrete(labels=c("Biomass" = "Total",
                            "Above_ground_biomass" = "Above ground",
                            "Below_ground_biomass" = "Below ground")) +

  theme_bw() +
  theme(
    panel.border = element_rect(size = 1.0,colour = "black", linetype=1),
    axis.line=element_line(size = 0.6, colour = "black", linetype=1),
    legend.position = c(0.75,0.65),#"none",#
    legend.title = element_blank(),
    legend.text = element_text(size = 14,color = "black"),
    legend.background = element_rect(fill = NA,color = NA),
    axis.text.x = element_text(size=16, vjust = 0.5,hjust=0.5,color = 'black', angle=10),
    axis.text.y = element_text(size=16, hjust = 1,color = 'black'),
    #axis.title.x = element_text(size=14,margin = margin(t = 5, b = 0)),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=16,margin = margin(r = 2)))
# Add p-values comparing groups
# Specify the comparisons you want
#my_comparisons <- list( c("Crack (+), plant (+)", "Crack (-), plant (+)") )
p <- p + stat_compare_means(aes(group = Treatments), label = "p.signif",size=8, 
                            label.y = 20, method = "t.test") +
  scale_color_discrete(breaks=c("Crack (+), plant (+)", "Crack (-), plant (+)"),
                       labels=c("Plant (+), Crack (+)", "Plant (+), Crack (-)")) +
  scale_fill_discrete(breaks=c("Crack (+), plant (+)", "Crack (-), plant (+)"),
                      labels=c("Plant (+), Crack (+)", "Plant (+), Crack (-)"))
p
ggsave(p,filename = "p6Biomass.pdf",width = 5,height = 4)

#Add P-values and Significance Levels to ggplots
compare_means(Size ~ Treatments, data = df, 
              group.by = "Attribute", method = "anova") # "t.test"、"anova"、"kruskal.test"

res.aov3 <- aov(Size ~ Treatments, data = df)
summary(res.aov3)

# ==================Draw the summary table of Total_biomass============== 
#::::::::::::::::::::::::::::::::::::::
# Compute descriptive statistics by groups
stable <- desc_statby(CrackPlant, measure.var = "Biomass",
                      grps = "Treatments", ci = 0.95)
#stable <- stable[, c("Treatments", "length", "mean", "sd")]
stable
# Summary table plot, medium orange theme
stable.p <- ggtexttable(stable, rows = NULL, 
                        theme = ttheme("mOrange"))
# Plot
stable.p

# ==================Draw the summary table of Above_ground_biomass============== 
#::::::::::::::::::::::::::::::::::::::
# Compute descriptive statistics by groups
stable <- desc_statby(CrackPlant, measure.var = "Above_ground_biomass",
                      grps = "Treatments", ci = 0.95)
#stable <- stable[, c("Treatments", "length", "mean", "sd")]
stable
# Summary table plot, medium orange theme
stable.p <- ggtexttable(stable, rows = NULL, 
                        theme = ttheme("mOrange"))
# Plot
stable.p

# ==================Draw the summary table of Below_ground_biomass============== 
#::::::::::::::::::::::::::::::::::::::
# Compute descriptive statistics by groups
stable <- desc_statby(CrackPlant, measure.var = "Below_ground_biomass",
                      grps = "Treatments", ci = 0.95)
#stable <- stable[, c("Treatments", "length", "mean", "sd")]
stable
# Summary table plot, medium orange theme
stable.p <- ggtexttable(stable, rows = NULL, 
                        theme = ttheme("mOrange"))
# Plot
stable.p

