library(brms)
library(nlme)
library(vegan)

library(devtools)
#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis) 


meta.modsite
meta.aggregate
meta.aggmodsite

dist_modsite <- vegdist(modern.site[,c(3:62)], method="bray")
dist_aggsite <- vegdist(com.aggregate[,c(8:35)], method="bray")
dist_aggmodsite <- vegdist(agg.modsite[,c(5:64)], method="bray")

adonis_modsite <- adonis2(dist_modsite ~ Site * Year, data=modern.site)
adonis_aggsite <- adonis2(dist_aggsite ~ Site * AgeConsolidated500, data=com.aggregate)
adonis_aggmodsite <- adonis2(dist_aggmodsite ~ Site * AgeConsolidated500 * Group, data=agg.modsite)

pwadonis_modsite <- pairwise.adonis2(dist_modsite ~ Site * Year, data=modern.site)
pwadonis_aggsite <- pairwise.adonis2(dist_aggsite ~ Site * AgeConsolidated500, data=com.aggregate)
pwadonis_aggmodsite <- pairwise.adonis2(dist_aggmodsite ~ Site * AgeConsolidated500 * Group, data=agg.modsite)

#write.csv(pwadonis_modsite, file = "output/pwadonis_modsite.csv")
#write.csv(pwadonis_aggsite, file = "output/pwadonis_aggsite.csv")
#write.csv(pwadonis_aggmodsite, file = "output/pwadonis_aggmodsite.csv")

agg.modsite <- full_join(com.aggregate[,c(1:3,8:35)] |> mutate(Group = c("Core")), modern.site[,c(1,3:62,65,66,67)]) 
agg.modsite[is.na(agg.modsite)] <- 0

agg.modsite <- agg.modsite |> 
  select(1:3, 32, 5:31, 33:64, 4)

meta.aggmodsite <- metaMDS(agg.modsite[,c(5:64)], autotransform=F) #0.13
comp.aggmodsite.meta <- agg.modsite[,1:4] %>%
  mutate(., CoreDepth = paste(Core, AgeConsolidated500, sep = "-"))

data.aggmodsite.scores <- as.data.frame(scores(meta.aggmodsite, "sites"))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.aggmodsite.scores$sample <- comp.aggmodsite.meta$CoreDepth  # create a column of site names, from the rownames of data.scores
data.aggmodsite.scores$age <- factor(comp.aggmodsite.meta$AgeConsolidated500)  
data.aggmodsite.scores$core <- factor(comp.aggmodsite.meta$Core)
data.aggmodsite.scores$site <- factor(comp.aggmodsite.meta$Site)
data.aggmodsite.scores$group <- factor(comp.aggmodsite.meta$Group)

species.aggmodsite.scores <- as.data.frame(scores(meta.aggmodsite, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.aggmodsite.scores$genus <- rownames(species.aggmodsite.scores)

#### modelling ####
## diversity ##
agg.mod.meta <- agg.mod %>% select(., Core, Site, AgeConsolidated500) %>% 
  mutate(Group = case_when(AgeConsolidated500 == 0 ~ "Core",
                           AgeConsolidated500 >4000 ~ "Core",
                           AgeConsolidated500 <4000 & AgeConsolidated500 >1000 ~ "Survey"))
agg.mod.comm <- agg.mod %>% select(., -Core, -Site, -AgeConsolidated500)

agg.mod.comm$sr <- rowSums(agg.mod.comm!=0)
agg.mod.comm$h <- diversity(agg.mod.comm[,c(1:58)], "shannon")
agg.mod.comm$simp <- diversity(agg.mod.comm[,c(1:58)], "simpson")
agg.mod.comm$j <- agg.mod.comm$h/log(agg.mod.comm$sr)

agg.mod.div <- agg.mod.meta %>%
  cbind(., agg.mod.comm$sr) %>%
  cbind(., agg.mod.comm$h) %>%
  cbind(., agg.mod.comm$simp) %>%
  cbind(., agg.mod.comm$j) %>%
  rename(., sr = "agg.mod.comm$sr", h = "agg.mod.comm$h", simp = "agg.mod.comm$simp", j = "agg.mod.comm$j")

rm(agg.mod.meta);rm(agg.mod.comm)

agg.modsite.meta <- agg.modsite %>% select(., Core, Site, AgeConsolidated500, Group)
agg.modsite.comm <- agg.modsite %>% select(., -Core, -Site, -AgeConsolidated500, -Group)

agg.modsite.comm$sr <- rowSums(agg.modsite.comm!=0)
agg.modsite.comm$h <- diversity(agg.modsite.comm[,c(1:59)], "shannon")
agg.modsite.comm$simp <- diversity(agg.modsite.comm[,c(1:59)], "simpson")
agg.modsite.comm$j <- agg.modsite.comm$h/log(agg.modsite.comm$sr)

agg.modsite.div <- agg.modsite.meta %>%
  cbind(., agg.modsite.comm$sr) %>%
  cbind(., agg.modsite.comm$h) %>%
  cbind(., agg.modsite.comm$simp) %>%
  cbind(., agg.modsite.comm$j) %>%
  rename(., sr = "agg.modsite.comm$sr", h = "agg.modsite.comm$h", simp = "agg.modsite.comm$simp", j = "agg.modsite.comm$j")

rm(agg.modsite.meta);rm(agg.modsite.comm)

agg.modsite.div$fAge <- factor(agg.modsite.div$AgeConsolidated500)
agg.modsite.div$AgeSite <- factor()

ggplot(agg.modsite.div) + geom_boxplot(aes(fAge, sr)) + facet_grid(~Site) 
ggplot(agg.modsite.div) + geom_boxplot(aes(fAge, h)) + facet_grid(~Site) 
ggplot(agg.modsite.div) + geom_boxplot(aes(fAge, simp)) + facet_grid(~Site) 
ggplot(agg.modsite.div) + geom_boxplot(aes(fAge, j)) + facet_grid(~Site) 



lm.sr <- lm(sr ~ Site * AgeConsolidated500, data=agg.mod.div)
summary(lme.sr)
