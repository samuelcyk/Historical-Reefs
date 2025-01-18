library(tidyverse)
library(vegan)
library(nlme)
library(ggExtra)
library(dendextend)
library(factoextra)
library(ggh4x)
library(patchwork)
library(devtools)
#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis) 

#### Load packages
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



#### FIG 3 PREP ####

community.prop.genw0 <- read.csv("data_csv/Core prop meru 0.csv", header=T)

com.prop.gen.topgen <- community.prop.genw0 |>
  select(Core, Site, DepthConsolidated, AgeConsolidated500, Age, 
         Acropora, Montipora, Euphyllia, Goniopora, Porites, Merulinidae) |>
  cbind(com.prop.gen.others$Total) |>
  rename(Others = "com.prop.gen.others$Total")

cy01.prop.gen.long <- com.prop.gen.topgen |> filter(Core == "CY01") |>
  pivot_longer(cols=6:12, names_to="Genus", values_to="ProportionalCover")
cy02.prop.gen.long <- com.prop.gen.topgen |> filter( Core == "CY02") |>
  pivot_longer( cols=6:12, names_to="Genus", values_to="ProportionalCover")
cy03.prop.gen.long <- com.prop.gen.topgen |> filter( Core == "CY03") |>
  pivot_longer( cols=6:12, names_to="Genus", values_to="ProportionalCover")
ha02.prop.gen.long <- com.prop.gen.topgen |> filter( Core == "HA02") |>
  pivot_longer( cols=6:12, names_to="Genus", values_to="ProportionalCover")
ha03.prop.gen.long <- com.prop.gen.topgen |> filter( Core == "HA03") |>
  pivot_longer( cols=6:12, names_to="Genus", values_to="ProportionalCover")
ha04.prop.gen.long <- com.prop.gen.topgen |> filter( Core == "HA04") |>
  pivot_longer( cols=6:12, names_to="Genus", values_to="ProportionalCover")
ha05.prop.gen.long <- com.prop.gen.topgen |> filter( Core == "HA05") |>
  pivot_longer( cols=6:12, names_to="Genus", values_to="ProportionalCover")
ha06.prop.gen.long <- com.prop.gen.topgen |> filter( Core == "HA06") |>
  pivot_longer( cols=6:12, names_to="Genus", values_to="ProportionalCover")
sm01.prop.gen.long <- com.prop.gen.topgen |> filter( Core == "SM01") |>
  pivot_longer( cols=6:12, names_to="Genus", values_to="ProportionalCover")
sm02.prop.gen.long <- com.prop.gen.topgen |> filter( Core == "SM02") |>
  pivot_longer( cols=6:12, names_to="Genus", values_to="ProportionalCover")
sm03.prop.gen.long <- com.prop.gen.topgen |> filter( Core == "SM03") |>
  pivot_longer( cols=6:12, names_to="Genus", values_to="ProportionalCover")
sm04.prop.gen.long <- com.prop.gen.topgen |> filter( Core == "SM04") |>
  pivot_longer( cols=6:12, names_to="Genus", values_to="ProportionalCover")
sm05.prop.gen.long <- com.prop.gen.topgen |> filter( Core == "SM05") |>
  pivot_longer( cols=6:12, names_to="Genus", values_to="ProportionalCover")
sm06.prop.gen.long <- com.prop.gen.topgen |> filter( Core == "SM06") |>
  pivot_longer( cols=6:12, names_to="Genus", values_to="ProportionalCover")
sl01.prop.gen.long <- com.prop.gen.topgen |> filter( Core == "SL01") |>
  pivot_longer( cols=6:12, names_to="Genus", values_to="ProportionalCover")
sl02.prop.gen.long <- com.prop.gen.topgen |> filter( Core == "SL02") |>
  pivot_longer( cols=6:12, names_to="Genus", values_to="ProportionalCover")
sl03.prop.gen.long <- com.prop.gen.topgen |> filter( Core == "SL03") |>
  pivot_longer( cols=6:12, names_to="Genus", values_to="ProportionalCover")
ks01.prop.gen.long <- com.prop.gen.topgen |> filter( Core == "KS01") |>
  pivot_longer( cols=6:12, names_to="Genus", values_to="ProportionalCover")
ks02.prop.gen.long <- com.prop.gen.topgen |> filter( Core == "KS02") |>
  pivot_longer( cols=6:12, names_to="Genus", values_to="ProportionalCover")
ks03.prop.gen.long <- com.prop.gen.topgen |> filter( Core == "KS03") |>
  pivot_longer( cols=6:12, names_to="Genus", values_to="ProportionalCover")

core.prop.gen.long <- bind_rows(cy01.prop.gen.long,cy02.prop.gen.long,cy03.prop.gen.long,
                                ha02.prop.gen.long,ha03.prop.gen.long,ha04.prop.gen.long,ha05.prop.gen.long,ha06.prop.gen.long,
                                sm01.prop.gen.long,sm02.prop.gen.long,sm03.prop.gen.long,sm04.prop.gen.long,sm05.prop.gen.long,sm06.prop.gen.long,
                                sl01.prop.gen.long,sl02.prop.gen.long,sl03.prop.gen.long,
                                ks01.prop.gen.long,ks02.prop.gen.long,ks03.prop.gen.long)

core.prop.gen.long$Core <- factor(core.prop.gen.long$Core, 
                                  levels = c("CY01", "CY02", "CY03", 
                                             "HA02", "HA03", "HA04", 
                                             "HA05", "HA06",
                                             "SM01", "SM02", "SM03",
                                             "SM06", "SM05", "SM04",
                                             "SL03", "SL02", "SL01",
                                             "KS01", "KS02", "KS03"), 
                                  ordered = TRUE)

core.prop.gen.long <- core.prop.gen.long |> 
  filter(Core != "KS02")

rm(community.prop.genw0);rm(com.prop.gen.others);rm(com.prop.gen.topgen)
rm(cy01.prop.gen.long);rm(cy02.prop.gen.long);rm(cy03.prop.gen.long);
rm(ha02.prop.gen.long);rm(ha03.prop.gen.long);rm(ha04.prop.gen.long);rm(ha05.prop.gen.long);rm(ha06.prop.gen.long);
rm(sm01.prop.gen.long);rm(sm02.prop.gen.long);rm(sm03.prop.gen.long);rm(sm04.prop.gen.long);rm(sm05.prop.gen.long);rm(sm06.prop.gen.long);
rm(sl01.prop.gen.long);rm(sl02.prop.gen.long);rm(sl03.prop.gen.long);
rm(ks01.prop.gen.long);rm(ks02.prop.gen.long);rm(ks03.prop.gen.long)

#### FIG 3 PARTS ####

CY01a <- core.prop.gen.long |> 
  filter(Core == "CY01") |> 
  ggplot() +
  geom_smooth(aes(Age, ProportionalCover, colour=Genus), se=FALSE) +
  coord_flip() + 
  scale_x_reverse(name = "Age (years before present)") + 
  scale_y_continuous(name = NULL, 
                     limits = c(0,1), 
                     breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("Acropora" = "#66c2a5", 
                                 "Montipora" = "#fc8d62",
                                 "Euphyllia" = "#8da0cb",
                                 "Goniopora" = "#e78ac3",
                                 "Porites" = "#a6d854",
                                 "Merulinidae" = "#ffd92f",
                                 "Others" = "grey")) +
  ggtitle("CY01")

CY02a <- core.prop.gen.long |> 
  filter(Core == "CY02") |> 
  ggplot() +
  geom_smooth(aes(Age, ProportionalCover, colour=Genus), se=FALSE) +
  coord_flip() + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     limits = c(0,1), 
                     breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("Acropora" = "#66c2a5", 
                                 "Montipora" = "#fc8d62",
                                 "Euphyllia" = "#8da0cb",
                                 "Goniopora" = "#e78ac3",
                                 "Porites" = "#a6d854",
                                 "Merulinidae" = "#ffd92f",
                                 "Others" = "grey")) +
  ggtitle("CY02")

CY03a <- core.prop.gen.long |> 
  filter(Core == "CY03") |> 
  ggplot() +
  geom_smooth(aes(Age, ProportionalCover, colour=Genus), se=FALSE) +
  coord_flip() + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     limits = c(0,1), 
                     breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("Acropora" = "#66c2a5", 
                                 "Montipora" = "#fc8d62",
                                 "Euphyllia" = "#8da0cb",
                                 "Goniopora" = "#e78ac3",
                                 "Porites" = "#a6d854",
                                 "Merulinidae" = "#ffd92f",
                                 "Others" = "grey")) +
  ggtitle("CY03")


HA02a <- core.prop.gen.long |> 
  filter(Core == "HA02") |> 
  ggplot() +
  geom_smooth(aes(Age, ProportionalCover, colour=Genus), se=FALSE) +
  coord_flip() + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     limits = c(0,1), 
                     breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("Acropora" = "#66c2a5", 
                                 "Montipora" = "#fc8d62",
                                 "Euphyllia" = "#8da0cb",
                                 "Goniopora" = "#e78ac3",
                                 "Porites" = "#a6d854",
                                 "Merulinidae" = "#ffd92f",
                                 "Others" = "grey")) +
  ggtitle("HA02")

HA03a <- core.prop.gen.long |> 
  filter(Core == "HA03") |> 
  ggplot() +
  geom_smooth(aes(Age, ProportionalCover, colour=Genus), se=FALSE) +
  coord_flip() + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     limits = c(0,1), 
                     breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("Acropora" = "#66c2a5", 
                                 "Montipora" = "#fc8d62",
                                 "Euphyllia" = "#8da0cb",
                                 "Goniopora" = "#e78ac3",
                                 "Porites" = "#a6d854",
                                 "Merulinidae" = "#ffd92f",
                                 "Others" = "grey")) +
  ggtitle("HA03")

HA04a <- core.prop.gen.long |> 
  filter(Core == "HA04") |> 
  ggplot() +
  geom_smooth(aes(Age, ProportionalCover, colour=Genus), se=FALSE) +
  coord_flip() + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     limits = c(0,1), 
                     breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("Acropora" = "#66c2a5", 
                                 "Montipora" = "#fc8d62",
                                 "Euphyllia" = "#8da0cb",
                                 "Goniopora" = "#e78ac3",
                                 "Porites" = "#a6d854",
                                 "Merulinidae" = "#ffd92f",
                                 "Others" = "grey")) +
  ggtitle("HA04")

HA05a <- core.prop.gen.long |> 
  filter(Core == "HA05") |> 
  ggplot() +
  geom_smooth(aes(Age, ProportionalCover, colour=Genus), se=FALSE) +
  coord_flip() + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     limits = c(0,1), 
                     breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("Acropora" = "#66c2a5", 
                                 "Montipora" = "#fc8d62",
                                 "Euphyllia" = "#8da0cb",
                                 "Goniopora" = "#e78ac3",
                                 "Porites" = "#a6d854",
                                 "Merulinidae" = "#ffd92f",
                                 "Others" = "grey")) +
  ggtitle("HA05")

HA06a <- core.prop.gen.long |> 
  filter(Core == "HA06") |> 
  ggplot() +
  geom_smooth(aes(Age, ProportionalCover, colour=Genus), se=FALSE) +
  coord_flip() + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     limits = c(0,1), 
                     breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("Acropora" = "#66c2a5", 
                                 "Montipora" = "#fc8d62",
                                 "Euphyllia" = "#8da0cb",
                                 "Goniopora" = "#e78ac3",
                                 "Porites" = "#a6d854",
                                 "Merulinidae" = "#ffd92f",
                                 "Others" = "grey")) +
  ggtitle("HA06")

SM01a <- core.prop.gen.long |> 
  filter(Core == "SM01") |> 
  ggplot() +
  geom_smooth(aes(Age, ProportionalCover, colour=Genus), se=FALSE) +
  coord_flip() + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     limits = c(0,1), 
                     breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("Acropora" = "#66c2a5", 
                                 "Montipora" = "#fc8d62",
                                 "Euphyllia" = "#8da0cb",
                                 "Goniopora" = "#e78ac3",
                                 "Porites" = "#a6d854",
                                 "Merulinidae" = "#ffd92f",
                                 "Others" = "grey")) +
  ggtitle("SM01")

SM02a <- core.prop.gen.long |> 
  filter(Core == "SM02") |> 
  ggplot() +
  geom_smooth(aes(Age, ProportionalCover, colour=Genus), se=FALSE) +
  coord_flip() + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = "Cover", 
                     limits = c(0,1), 
                     breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("Acropora" = "#66c2a5", 
                                 "Montipora" = "#fc8d62",
                                 "Euphyllia" = "#8da0cb",
                                 "Goniopora" = "#e78ac3",
                                 "Porites" = "#a6d854",
                                 "Merulinidae" = "#ffd92f",
                                 "Others" = "grey")) +
  ggtitle("SM02")

SM03a <- core.prop.gen.long |> 
  filter(Core == "SM03") |> 
  ggplot() +
  geom_smooth(aes(Age, ProportionalCover, colour=Genus), se=FALSE) +
  coord_flip() + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     limits = c(0,1), 
                     breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("Acropora" = "#66c2a5", 
                                 "Montipora" = "#fc8d62",
                                 "Euphyllia" = "#8da0cb",
                                 "Goniopora" = "#e78ac3",
                                 "Porites" = "#a6d854",
                                 "Merulinidae" = "#ffd92f",
                                 "Others" = "grey")) +
  ggtitle("SM03")

SM06a <- core.prop.gen.long |> 
  filter(Core == "SM06") |> 
  ggplot() +
  geom_smooth(aes(Age, ProportionalCover, colour=Genus), se=FALSE) +
  coord_flip() + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     limits = c(0,1), 
                     breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("Acropora" = "#66c2a5", 
                                 "Montipora" = "#fc8d62",
                                 "Euphyllia" = "#8da0cb",
                                 "Goniopora" = "#e78ac3",
                                 "Porites" = "#a6d854",
                                 "Merulinidae" = "#ffd92f",
                                 "Others" = "grey")) +
  ggtitle("SM06")

SM05a <- core.prop.gen.long |> 
  filter(Core == "SM05") |> 
  ggplot() +
  geom_smooth(aes(Age, ProportionalCover, colour=Genus), se=FALSE) +
  coord_flip() + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     limits = c(0,1), 
                     breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("Acropora" = "#66c2a5", 
                                 "Montipora" = "#fc8d62",
                                 "Euphyllia" = "#8da0cb",
                                 "Goniopora" = "#e78ac3",
                                 "Porites" = "#a6d854",
                                 "Merulinidae" = "#ffd92f",
                                 "Others" = "grey")) +
  ggtitle("SM05")

SM04a <- core.prop.gen.long |> 
  filter(Core == "SM04") |> 
  ggplot() +
  geom_smooth(aes(Age, ProportionalCover, colour=Genus), se=FALSE) +
  coord_flip() + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     limits = c(0,1), 
                     breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("Acropora" = "#66c2a5", 
                                 "Montipora" = "#fc8d62",
                                 "Euphyllia" = "#8da0cb",
                                 "Goniopora" = "#e78ac3",
                                 "Porites" = "#a6d854",
                                 "Merulinidae" = "#ffd92f",
                                 "Others" = "grey")) +
  ggtitle("SM04")

SL03a <- core.prop.gen.long |> 
  filter(Core == "SL03") |> 
  ggplot() +
  geom_smooth(aes(Age, ProportionalCover, colour=Genus), se=FALSE) +
  coord_flip() + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     limits = c(0,1), 
                     breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("Acropora" = "#66c2a5", 
                                 "Montipora" = "#fc8d62",
                                 "Euphyllia" = "#8da0cb",
                                 "Goniopora" = "#e78ac3",
                                 "Porites" = "#a6d854",
                                 "Merulinidae" = "#ffd92f",
                                 "Others" = "grey")) +
  ggtitle("SL03")

SL02a <- core.prop.gen.long |> 
  filter(Core == "SL02") |> 
  ggplot() +
  geom_smooth(aes(Age, ProportionalCover, colour=Genus), se=FALSE) +
  coord_flip() + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     limits = c(0,1), 
                     breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("Acropora" = "#66c2a5", 
                                 "Montipora" = "#fc8d62",
                                 "Euphyllia" = "#8da0cb",
                                 "Goniopora" = "#e78ac3",
                                 "Porites" = "#a6d854",
                                 "Merulinidae" = "#ffd92f",
                                 "Others" = "grey")) +
  ggtitle("SL02")

SL01a <- core.prop.gen.long |> 
  filter(Core == "SL01") |> 
  ggplot() +
  geom_smooth(aes(Age, ProportionalCover, colour=Genus), se=FALSE) +
  coord_flip() + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     limits = c(0,1), 
                     breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("Acropora" = "#66c2a5", 
                                 "Montipora" = "#fc8d62",
                                 "Euphyllia" = "#8da0cb",
                                 "Goniopora" = "#e78ac3",
                                 "Porites" = "#a6d854",
                                 "Merulinidae" = "#ffd92f",
                                 "Others" = "grey")) +
  ggtitle("SL01")

KS01a <- core.prop.gen.long |> 
  filter(Core == "KS01") |> 
  ggplot() +
  geom_smooth(aes(Age, ProportionalCover, colour=Genus), se=FALSE) +
  coord_flip() + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     limits = c(0,1), 
                     breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("Acropora" = "#66c2a5", 
                                 "Montipora" = "#fc8d62",
                                 "Euphyllia" = "#8da0cb",
                                 "Goniopora" = "#e78ac3",
                                 "Porites" = "#a6d854",
                                 "Merulinidae" = "#ffd92f",
                                 "Others" = "grey")) +
  ggtitle("KS01")

KS03a <- core.prop.gen.long |> 
  filter(Core == "KS03") |> 
  ggplot() +
  geom_smooth(aes(Age, ProportionalCover, colour=Genus), se=FALSE) +
  coord_flip() + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     limits = c(0,1), 
                     breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("Acropora" = "#66c2a5", 
                                 "Montipora" = "#fc8d62",
                                 "Euphyllia" = "#8da0cb",
                                 "Goniopora" = "#e78ac3",
                                 "Porites" = "#a6d854",
                                 "Merulinidae" = "#ffd92f",
                                 "Others" = "grey")) +
  ggtitle("KS03")

#### FIG 3 ####

FIG3 <- CY01a + CY02a + CY03a +
  HA02a + HA03a + HA04a +
  HA05a + HA06a + 
  SM01a + SM02a + SM03a +
  SM06a + SM05a + SM04a +
  SL03a + SL02a + SL01a +
  KS01a + KS03a + plot_layout(nrow = 1, guides = "collect") & theme_classic() & theme(axis.text.x = element_text(angle = 40, hjust=1),
                                                                    axis.text.y = element_text(angle = 50, hjust=1),
                                                                    legend.position = "bottom") 
ggsave(FIG3, file = "figs/coreplot.age.sm.tiff", units = "cm", height = 12, width = 45)
ggsave(FIG3, file = "figs/coreplot.age.sm.pdf", units = "cm", height = 12, width = 45)
ggsave(FIG3, file = "figs/coreplot.age.sm.svg", units = "cm", height = 12, width = 45)

#### FIG 4 PREP ####
sediment_sec5 <- read.csv("data_csv/section_data_5t.csv", header=T, check.names=F)

sediment_sec5_long <- sediment_sec5 |> 
  pivot_longer(cols = 3:6,
               names_to = "Size",
               values_to = "Proportion") |> 
  replace_na(list(Proportion = 0))
  

sediment_sec5_long$Size <- factor(sediment_sec5_long$Size, levels = c("0", "125", "500", "2000"))

sediment_sec5_long$Core <- factor(sediment_sec5_long$Core, 
                                  levels = c("CY01", "CY02", "CY03", 
                                             "HA02", "HA03", "HA04", 
                                             "HA05", "HA06",
                                             "SM01", "SM02", "SM03",
                                             "SM06", "SM05", "SM04",
                                             "SL03", "SL02", "SL01",
                                             "KS01", "KS02", "KS03"), 
                                  ordered = TRUE)

#### FIG 4 PARTS ####

CY01b <- sediment_sec5_long |> 
  filter(Core == "CY01") |> 
  ggplot(aes(x = Depth, y = Proportion/100, fill = Size)) +
  geom_area(alpha = 0.8, linewidth = 0.5) +
  coord_flip(xlim = c(350, 0)) + 
  scale_x_reverse(name = "Depth (cm)") + 
  scale_y_continuous(name = NULL, 
                     breaks = c(0, 0.5, 1)) +
  ggtitle("CY01")

CY02b <- sediment_sec5_long |> 
  filter(Core == "CY02") |> 
  ggplot(aes(x = Depth, y = Proportion/100, fill = Size)) +
  geom_area(alpha = 0.8, linewidth = 0.5) +
  coord_flip(xlim = c(350, 0)) + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     breaks = c(0, 0.5, 1)) +
  ggtitle("CY02")

CY03b <- sediment_sec5_long |> 
  filter(Core == "CY03") |> 
  ggplot(aes(x = Depth, y = Proportion/100, fill = Size)) +
  geom_area(alpha = 0.8, linewidth = 0.5) +
  coord_flip(xlim = c(350, 0)) + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     breaks = c(0, 0.5, 1)) +
  ggtitle("CY03")

HA02b <- sediment_sec5_long |> 
  filter(Core == "HA02") |> 
  ggplot(aes(x = Depth, y = Proportion/100, fill = Size)) +
  geom_area(alpha = 0.8, linewidth = 0.5) +
  coord_flip(xlim = c(350, 0)) + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     breaks = c(0, 0.5, 1)) +
  ggtitle("HA02")

HA03b <- sediment_sec5_long |> 
  filter(Core == "HA03") |> 
  ggplot(aes(x = Depth, y = Proportion/100, fill = Size)) +
  geom_area(alpha = 0.8, linewidth = 0.5) +
  coord_flip(xlim = c(350, 0)) + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     breaks = c(0, 0.5, 1)) +
  ggtitle("HA03")

HA04b <- sediment_sec5_long |> 
  filter(Core == "HA04") |> 
  ggplot(aes(x = Depth, y = Proportion/100, fill = Size)) +
  geom_area(alpha = 0.8, linewidth = 0.5) +
  coord_flip(xlim = c(350, 0)) + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     breaks = c(0, 0.5, 1)) +
  ggtitle("HA04")

HA05b <- sediment_sec5_long |> 
  filter(Core == "HA05") |> 
  ggplot(aes(x = Depth, y = Proportion/100, fill = Size)) +
  geom_area(alpha = 0.8, linewidth = 0.5) +
  coord_flip(xlim = c(350, 0)) + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     breaks = c(0, 0.5, 1)) +
  ggtitle("HA05")

HA06b <- sediment_sec5_long |> 
  filter(Core == "HA06") |> 
  ggplot(aes(x = Depth, y = Proportion/100, fill = Size)) +
  geom_area(alpha = 0.8, linewidth = 0.5) +
  coord_flip(xlim = c(350, 0)) + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     breaks = c(0, 0.5, 1)) +
  ggtitle("HA06")

SM01b <- sediment_sec5_long |> 
  filter(Core == "SM01") |> 
  ggplot(aes(x = Depth, y = Proportion/100, fill = Size)) +
  geom_area(alpha = 0.8, linewidth = 0.5) +
  coord_flip(xlim = c(350, 0)) + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     breaks = c(0, 0.5, 1)) +
  ggtitle("SM01")


SM02b <- sediment_sec5_long |> 
  filter(Core == "SM02") |> 
  ggplot(aes(x = Depth, y = Proportion/100, fill = Size)) +
  geom_area(alpha = 0.8, linewidth = 0.5) +
  coord_flip(xlim = c(350, 0)) + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     breaks = c(0, 0.5, 1)) +
  ggtitle("SM02")

SM03b <- sediment_sec5_long |> 
  filter(Core == "SM03") |> 
  ggplot(aes(x = Depth, y = Proportion/100, fill = Size)) +
  geom_area(alpha = 0.8, linewidth = 0.5) +
  coord_flip(xlim = c(350, 0)) + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = "Cover", 
                     breaks = c(0, 0.5, 1)) +
  ggtitle("SM03")

SM04b <- sediment_sec5_long |> 
  filter(Core == "SM04") |> 
  ggplot(aes(x = Depth, y = Proportion/100, fill = Size)) +
  geom_area(alpha = 0.8, linewidth = 0.5) +
  coord_flip(xlim = c(350, 0)) + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     breaks = c(0, 0.5, 1)) +
  ggtitle("SM04")

SM05b <- sediment_sec5_long |> 
  filter(Core == "SM05") |> 
  ggplot(aes(x = Depth, y = Proportion/100, fill = Size)) +
  geom_area(alpha = 0.8, linewidth = 0.5) +
  coord_flip(xlim = c(350, 0)) + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     breaks = c(0, 0.5, 1)) +
  ggtitle("SM05")

SM06b <- sediment_sec5_long |> 
  filter(Core == "SM06") |> 
  ggplot(aes(x = Depth, y = Proportion/100, fill = Size)) +
  geom_area(alpha = 0.8, linewidth = 0.5) +
  coord_flip(xlim = c(350, 0)) + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     breaks = c(0, 0.5, 1)) +
  ggtitle("SM06")

SL01b <- sediment_sec5_long |> 
  filter(Core == "SL01") |> 
  ggplot(aes(x = Depth, y = Proportion/100, fill = Size)) +
  geom_area(alpha = 0.8, linewidth = 0.5) +
  coord_flip(xlim = c(350, 0)) + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     breaks = c(0, 0.5, 1)) +
  ggtitle("SL01")

SL02b <- sediment_sec5_long |> 
  filter(Core == "SL02") |> 
  ggplot(aes(x = Depth, y = Proportion/100, fill = Size)) +
  geom_area(alpha = 0.8, linewidth = 0.5) +
  coord_flip(xlim = c(350, 0)) + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     breaks = c(0, 0.5, 1)) +
  ggtitle("SL02")

SL03b <- sediment_sec5_long |> 
  filter(Core == "SL03") |> 
  ggplot(aes(x = Depth, y = Proportion/100, fill = Size)) +
  geom_area(alpha = 0.8, linewidth = 0.5) +
  coord_flip(xlim = c(350, 0)) + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     breaks = c(0, 0.5, 1)) +
  ggtitle("SL03")

KS01b <- sediment_sec5_long |> 
  filter(Core == "KS01") |> 
  ggplot(aes(x = Depth, y = Proportion/100, fill = Size)) +
  geom_area(alpha = 0.8, linewidth = 0.5) +
  coord_flip(xlim = c(350, 0)) + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     breaks = c(0, 0.5, 1)) +
  ggtitle("KS01")


KS02b <- sediment_sec5_long |> 
  filter(Core == "KS02") |> 
  ggplot(aes(x = Depth, y = Proportion/100, fill = Size)) +
  geom_area(alpha = 0.8, linewidth = 0.5) +
  coord_flip(xlim = c(350, 0)) + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     breaks = c(0, 0.5, 1)) +
  ggtitle("KS02")

KS03b <- sediment_sec5_long |> 
  filter(Core == "KS03") |> 
  ggplot(aes(x = Depth, y = Proportion/100, fill = Size)) +
  geom_area(alpha = 0.8, linewidth = 0.5) +
  coord_flip(xlim = c(350, 0)) + 
  scale_x_reverse(name = NULL) + 
  scale_y_continuous(name = NULL, 
                     breaks = c(0, 0.5, 1)) +
  ggtitle("KS03")

#### FIG 4 ####

FIG4 <- CY01b + CY02b + CY03b +
  HA02b + HA03b + HA04b +
  HA05b + HA06b + 
  SM01b + SM02b + SM03b +
  SM06b + SM05b + SM04b +
  SL03b + SL02b + SL01b +
  KS01b + KS02b + KS03b + 
  plot_layout(nrow = 1, guides = "collect") & 
  scale_fill_manual(values = c("2000" = "#ffffcc",
                               "500" =  "#a1dab4",
                               "125" = "#41b6c4",
                               "0" = "#225ea8")) & 
  theme_classic() & theme(axis.text.x = element_text(angle = 40, hjust=1),
                          axis.text.y = element_text(angle = 50, hjust=1),
                          legend.position = "bottom") 


ggsave(FIG4, file = "figs/sediplot.dep.sm.tiff", units = "cm", height = 12, width = 45)
ggsave(FIG4, file = "figs/sediplot.dep.sm.pdf", units = "cm", height = 12, width = 45)

#### FIG 5/6 PREP ####
com.prop <- read.csv("data_csv/Community Data ProportionCover.csv")

agedepth <- read.csv("data_csv/Age Depth Accumulation.csv") |> 
  mutate(Var.age = (max-min)/2, Mean.age = mean, DepthConsolidated = depth) |>
  select(-min, -max, -median, -mean, -depth)

com.total <- left_join(com.prop, agedepth) |>
  select(1:3, 33, 32, 4:31)
rm(com.prop)
rm(com.propfull)

meta.total.prop <- metaMDS(com.total[,c(6:33)], autotransform=F) #0.069
comp.total.meta <- com.total[,1:5] |>
  mutate(CoreDepth = paste(Core, DepthConsolidated, sep = "-"))

data.total.scores <- as.data.frame(scores(meta.total.prop, "sites"))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.total.scores$sample <- comp.total.meta$CoreDepth  # create a column of site names, from the rownames of data.scores
data.total.scores$depth <- comp.total.meta$DepthConsolidated  
data.total.scores$agemean <- comp.total.meta$Mean.age
data.total.scores$agevar <- comp.total.meta$Var.age
data.total.scores$core <- factor(comp.total.meta$Core)
data.total.scores$site <- factor(substr(data.total.scores$core,1,2))
data.total.scores$location <- factor(comp.total.meta$Location)

species.total.scores <- as.data.frame(scores(meta.total.prop, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.total.scores$genus <- rownames(species.total.scores)

rm(comp.total.meta)

#### FIG 5 PARTS ####
com.aggregate <- read.csv("data_csv/Summarised Community by age prop.csv", header=T)

meta.aggregate <- metaMDS(com.aggregate[,c(8:35)], autotransform=F) #0.15
comp.aggregate.meta <- com.aggregate[,1:7]  |> 
  mutate(CoreDepth = paste(Core, AgeConsolidated500, sep = "-"))

data.agg.scores <- as.data.frame(scores(meta.aggregate,"sites"))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.agg.scores$sample <- comp.aggregate.meta$CoreDepth  # create a column of site names, from the rownames of data.scores
data.agg.scores$age <- factor(comp.aggregate.meta$AgeConsolidated500)  
data.agg.scores$core <- factor(comp.aggregate.meta$Core)
data.agg.scores$site <- factor(comp.aggregate.meta$Site)

species.agg.scores <- as.data.frame(scores(meta.aggregate, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.agg.scores$genus <- rownames(species.agg.scores)

ell.site <- data.frame()
for(g in levels(data.agg.scores$site)){
  ell.site <- rbind(ell.site, cbind(as.data.frame(with(data.agg.scores[data.agg.scores$site==g,],
                                                       veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),wt=rep(1/length(NMDS1),length(NMDS1)))$cov,center=c(mean(NMDS1),mean(NMDS2)))))
                                    ,site=g))
}
#### FIG 5 PLOT ####

plotnmds.agg.site <- ggplot()+ 
  geom_text(data=species.agg.scores |>  
              filter(genus != "Unknown"),
            aes(x=NMDS1,y=NMDS2,label=genus),size=3) +  # add the species labels
  geom_point(data=data.agg.scores,aes(x=NMDS1,y=NMDS2,colour=site),size=2, alpha=0.7) + # add the point markers
  #  geom_path(data=ell.site, aes(x=NMDS1, y=NMDS2,colour=site), size=1.2, linetype=1, alpha=0.7) +
  annotate(geom="text", x=-2.9, y=-2.5, label="Stress = 0.15") + 
  coord_equal(xlim = c(-3.5, 2.25), ylim = c(-2.5, 2.5)) + scale_colour_manual(values=sixcolour, name="site")
#plotnmdsmarg.core.site <- ggMarginal(plotnmds.agg.site, groupColour = T, groupFill = T)

plotnmds.agg.site.el <- ggplot()+ 
  geom_text(data=species.agg.scores |>  
              filter(genus != "Unknown"),
            aes(x=NMDS1,y=NMDS2,label=genus),size=3) +  # add the species labels
  geom_point(data=data.agg.scores,aes(x=NMDS1,y=NMDS2,colour=site),size=2, alpha=0.7) + # add the point markers
  geom_path(data=ell.site, aes(x=NMDS1, y=NMDS2,colour=site), size=1.2, linetype=1, alpha=0.7) +
  annotate(geom="text", x=-2.9, y=-2.5, label="Stress = 0.15") + 
  coord_equal(xlim = c(-3.5, 2.25), ylim = c(-2.5, 2.5)) + scale_colour_manual(values=sixcolour, name="site")
#plotnmdsmarg.core.site <- ggMarginal(plotnmds.agg.site, groupColour = T, groupFill = T)

plotnmds.agg.age <- ggplot()+ 
  geom_text(data=species.agg.scores |> 
              filter(genus != "Unknown"),
            aes(x=NMDS1,y=NMDS2,label=genus),size=3) +  # add the species labels
  geom_point(data=data.agg.scores,aes(x=NMDS1,y=NMDS2,colour=age),size=2, alpha=0.7) + # add the point markers
  #  geom_path(data=ell.site, aes(x=NMDS1, y=NMDS2,colour=site), size=1.2, linetype=1, alpha=0.7) +
  annotate(geom="text", x=-2.9, y=-2.5, label="Stress = 0.15") + 
  coord_equal(xlim = c(-3.5, 2.25), ylim = c(-2.5, 2.5)) + scale_colour_manual(values=c('#7d0000', '#c2271f', '#f06754', '#fcb09e', '#99c3f9', '#618fcb', '#2e5e9a', '#003167'))
#plotnmdsmarg.core.age <- ggMarginal(plotnmds.agg.age, groupColour = T, groupFill = T)

#### FIG 6 PREP ####

modern.site <- read.csv("data_csv/survey.site.csv", header=T) |> 
  rename(Reef = Location) |> 
  filter(Reef=="Cyrene Reef" | Reef=="Pulau Hantu" | Reef=="Hantu Patch" | Reef=="Kusu Island" | Reef=="Pulau Subar Darat" | Reef=="Pulau Subar Laut" | Reef=="Pulau Semakau") |> 
  select(Code, Site, Year, Acanthastrea:Total) |> 
  mutate(Reef = factor(substr(Site,1,2))) |> 
  mutate(Site = recode(Reef, CY = "CY", HA = "HA", HP = "HA", KU = "KS", PS = "SL", SE = "SM"))  |> 
  mutate(AgeConsolidated500 = -50) |> 
  mutate(Core = Code) |> 
  column_to_rownames("Code") |> 
  mutate(Group = factor(c("Survey")))

meta.modsite <- metaMDS(modern.site[,c(3:62)], autotransform=F) #0.18
comp.modsite.meta <- modern.site[,1:2]
data.modsite.scores <- as.data.frame(scores(meta.modsite)$sites)  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.modsite.scores$year <- comp.modsite.meta$Year  
data.modsite.scores$site <- factor(comp.modsite.meta$Site)
data.modsite.scores$fyear <- factor(comp.modsite.meta$Year)  

ell.site2 <- data.frame()
for(g in levels(data.modsite.scores$site)){
  ell.site2 <- rbind(ell.site2, cbind(as.data.frame(with(data.modsite.scores[data.modsite.scores$site==g,],
                                                       veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),wt=rep(1/length(NMDS1),length(NMDS1)))$cov,center=c(mean(NMDS1),mean(NMDS2)))))
                                    ,site=g))
}

rm(comp.modsite.meta)

#### FIG 6 PLOT ####

plotnmds.modsite.site <- ggplot()+ 
  #  geom_text(data=species.modsite.scores,aes(x=NMDS1,y=NMDS2,label=genus),size=3) +  # add the species labels
  geom_point(data=data.modsite.scores,aes(x=NMDS1,y=NMDS2,colour=site),size=2, alpha=0.7) + # add the point markers
  #  geom_path(data=ell.mod2site, aes(x=NMDS1, y=NMDS2,colour=site), size=1.2, linetype=1, alpha=0.7) +
  annotate(geom="text", x=-1.8, y=-1.95, label="Stress = 0.18") + 
  coord_equal(xlim = c(-2.2, 1.2)) + scale_colour_manual(values=sixcolour, name="site")

plotnmds.modsite.site.el <- ggplot()+ 
  #  geom_text(data=species.modsite.scores,aes(x=NMDS1,y=NMDS2,label=genus),size=3) +  # add the species labels
  geom_point(data=data.modsite.scores,aes(x=NMDS1,y=NMDS2,colour=site),size=2, alpha=0.7) + # add the point markers
  geom_path(data=ell.site2, aes(x=NMDS1, y=NMDS2,colour=site), size=1.2, linetype=1, alpha=0.7) +
  annotate(geom="text", x=-1.8, y=-1.95, label="Stress = 0.18") + 
  coord_equal(xlim = c(-2.2, 1.2)) + scale_colour_manual(values=sixcolour, name="site")

plotnmds.modsite.year <- ggplot()+ 
  #  geom_text(data=species.modsite.scores,aes(x=NMDS1,y=NMDS2,label=genus),size=3) +  # add the species labels
  geom_point(data=data.modsite.scores,aes(x=NMDS1,y=NMDS2,colour=year),size=2, alpha=0.7) + # add the point markers
  #  geom_path(data=ell.mod2site, aes(x=NMDS1, y=NMDS2,colour=site), size=1.2, linetype=1, alpha=0.7) +
  annotate(geom="text", x=-1.8, y=-1.95, label="Stress = 0.18") + 
  coord_equal(xlim = c(-2.2, 1.2)) + scale_colour_gradient2(low="#d73027", mid="#fee090", high="#4575b4", midpoint=2000)

#### FIG 5 + FIG 6 #### 

FIG56.el <- plotnmds.agg.site.el + plotnmds.agg.age + plotnmds.modsite.site.el + plotnmds.modsite.year + 
  plot_annotation(tag_levels = "a") &
  theme_classic() &
  theme(legend.position = "right") + plot_layout(ncol =2 )

FIG56 <- plotnmds.agg.site + plotnmds.agg.age + plotnmds.modsite.site + plotnmds.modsite.year + 
  plot_annotation(tag_levels = "a") &
  theme_classic() &
  theme(legend.position = "right")+ plot_layout(ncol =2 )

ggsave(FIG56, width=36, height=22, units="cm", filename="figs/nmds.modsite.tiff")
ggsave(FIG56, width=36, height=22, units="cm", filename="figs/nmds.modsite.pdf")
ggsave(FIG56.el, width=36, height=22, units="cm", filename="figs/nmds.el.modsite.tiff")
ggsave(FIG56.el, width=36, height=22, units="cm", filename="figs/nmds.el.modsite.pdf")


#(wrap_elements(plotnmdsmarg.core.site) + plotnmds.modsite.site) / (wrap_elements(plotnmdsmarg.core.age) + plotnmds.modsite.year) + plot_annotation(tag_levels = "a")


## functions
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}
