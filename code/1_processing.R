library(tidyverse)

#### Settings
set.seed(13)
theme_base <- theme_set(theme_bw(base_size = 10) + theme(legend.position="bottom"))
theme_noleg <- theme_set(theme_bw(base_size = 10) + theme(legend.position="none"))

sixcolour <- c("CY"="#66c2a5","HA"="#fc8d62","KS"="#8da0cb","SL"="#e78ac3","SM"="#a6d854")
twocolour <- c("East"="#d73027", "West"="#4575b4")
#fc8d59 low
#fee090 mid
#91bfdb high

#66c2a5 CY
#fc8d62 HA
#8da0cb SL
#e78ac3 SM
#a6d854 KS
#ffd92f

#4575b4 West
#d73027 East

#### Processing cores
Core <- read.csv("data_csv/Full_core.csv")

Core.Grouped <- Core |> group_by(Core, DepthConsolidated)

Core.GF <- Core.Grouped |>  pivot_wider(id_cols= c(Core, DepthConsolidated), names_from=GrowthForm, values_from=Weight, values_fn = sum)
Core.GE <- Core.Grouped |>  pivot_wider(id_cols= c(Core, DepthConsolidated), names_from=Genus, values_from=Weight, values_fn = sum)

#write.csv(Core.GF, file="output/Core GrowthForm.csv")
#write.csv(Core.GE, file="output/Core Genus.csv")

Core.Grouped$GFGen <- str_c(Core.Grouped$Genus, ".", Core.Grouped$GrowthForm)
Core.GEF <- Core.Grouped |> pivot_wider(id_cols= c(Core, DepthConsolidated), names_from=GFGen, values_from=Weight, values_fn = sum)

#write.csv(Core.GEF, file="output/Core GenusGF.csv")

rm(Core);rm(Core.Grouped);rm(Core.GE);rm(Core.GEF);rm(Core.GF)

#### Processing modern
Survey <- read.csv("data_csv/modgenera.t.csv") |> 
  select(Location, Site, Day, Month, Year, Depth, Acanthastrea:Unknown) |> 
  pivot_longer(cols = Acanthastrea:Unknown,
               names_to = "Genus",
               values_to = "Cover") |> 
  group_by(Location, Site, Day, Month, Year, Depth, Genus) |> 
  summarise(Cover = sum(Cover)) |> 
  pivot_wider(names_from = Genus,
              values_from = Cover) |> 
  mutate(Code = paste(Site, paste(c("D"), Depth, sep=""), Year, Month, Day, sep="-")) 

write.csv(Survey, file="output/survey.site.csv")

Transect <- read.csv("data_csv/modgenera.t.csv") |> 
  select(Location, Site, Day, Month, Year, Depth, Replicate, Acanthastrea:Unknown) |> 
  pivot_longer(cols = Acanthastrea:Unknown,
               names_to = "Genus",
               values_to = "Cover") |> 
  group_by(Location, Site, Day, Month, Year, Depth, Genus, Replicate) |> 
  summarise(Cover = sum(Cover)) |> 
  pivot_wider(names_from = Genus,
              values_from = Cover) |> 
  mutate(Code = paste(Site, paste(c("D"), Depth, sep=""), Year, Month, Day, paste(c("T"), Replicate, sep=""), sep="-")) 
  
write.csv(Transect, file="output/survey.transect.csv")
