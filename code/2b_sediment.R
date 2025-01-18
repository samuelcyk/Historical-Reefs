library(G2Sd)
library(viridis)

sediment_com5 <- read.csv("data_csv/complete_data_5t.csv", header=T, check.names=F) %>%
  rename(., Core = 1)
sediment_sec5 <- read.csv("data_csv/section_data_5t.csv", header=T, check.names=F) %>%
  rename(., Core = 1)
sediment_sec <- read.csv("data_csv/section_data_t.csv", header=T, check.names=F) %>%
  rename(., Core = 1)

sediment_com5_long <- sediment_com5 %>% 
  pivot_longer(cols = 3:8,
               names_to = "Size",
               values_to = "Proportion")
sediment_com5_long$Size <- factor(sediment_com5_long$Size, levels = c("0", "63", "125", "250", "500", "2000"))

sediment_sec5_long <- sediment_sec5 %>% 
  pivot_longer(cols = 3:6,
               names_to = "Size",
               values_to = "Proportion")
sediment_sec5_long$Size <- factor(sediment_sec5_long$Size, levels = c("0", "125", "500", "2000"))

coreorder <- c("CY01","CY02","CY03", 
               "HA02", "HA03", "HA04", "HA05", "HA06", 
               "SM01", "SM02", "SM03", "SM06", "SM05", "SM04",
               "SL03", "SL02", "SL01",
               "KS01", "KS02", "KS03")
sediment_sec5_long <- arrange(mutate(sediment_sec5_long, Core=factor(Core,levels=coreorder)),Core)

sediment_sec_long <- sediment_sec %>% 
  pivot_longer(cols = 3:6,
               names_to = "Size",
               values_to = "Proportion")
sediment_sec_long$Size <- factor(sediment_sec_long$Size, levels = c("0", "125", "500", "2000"))

cy01_com5_long <- filter(sediment_com5_long, Core == "CY-01A")
ha02_com5_long <- filter(sediment_com5_long, Core == "HA-02A")
ha03_com5_long <- filter(sediment_com5_long, Core == "HA-03A")
ha04_com5_long <- filter(sediment_com5_long, Core == "HA-04A")
ha05_com5_long <- filter(sediment_com5_long, Core == "HA-05A")
ks03_com5_long <- filter(sediment_com5_long, Core == "KS-03A")
sl01_com5_long <- filter(sediment_com5_long, Core == "SL-01A")
sm01_com5_long <- filter(sediment_com5_long, Core == "SM-01A")
sm02_com5_long <- filter(sediment_com5_long, Core == "SM-02A")
sm04_com5_long <- filter(sediment_com5_long, Core == "SM-04A")

sed_com5_plot <- ggplot(sediment_com5_long, aes(x=Depth, y=Proportion, fill=Size)) + facet_wrap(~ Core, ncol= 10)  + 
  geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse() + 
  theme(axis.text.x = element_text(angle = 50, hjust=1))

#ggsave(sed_com5_plot, width = 30, height = 20, units="cm", dpi=300, filename="figures/sedprop.complete5.tiff" )
#ggsave(sed_com5_plot, width = 30, height = 20, units="cm", dpi=300, filename="figures/sedprop.complete5.svg" )

sed_sec5_plot <- ggplot(sediment_sec5_long, aes(x=Depth, y=Proportion, fill=Size)) + facet_wrap(~ Core, ncol=20) + 
  geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse() +
  theme(axis.text.x = element_text(angle = 50, hjust=1))
ggsave(sed_sec5_plot, width = 30, height = 16, units="cm", dpi=300, filename="newfigs/sedprop.sections5.tiff" )

#ggsave(sed_sec5_plot, width = 30, height = 35, units="cm", dpi=300, filename="figures/sedprop.sections5.tiff" )
#ggsave(sed_sec5_plot, width = 30, height = 35, units="cm", dpi=300, filename="figures/sedprop.sections5.svg" )

cy01_com5_plot <- ggplot(cy01_com5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
ha02_com5_plot <- ggplot(ha02_com5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
ha03_com5_plot <- ggplot(ha03_com5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
ha04_com5_plot <- ggplot(ha04_com5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
ha05_com5_plot <- ggplot(ha05_com5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
ks03_com5_plot <- ggplot(ks03_com5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
sl01_com5_plot <- ggplot(sl01_com5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
sm01_com5_plot <- ggplot(sm01_com5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
sm02_com5_plot <- ggplot(sm02_com5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
sm04_com5_plot <- ggplot(sm04_com5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()

cy01_sec5_long <- filter(sediment_sec5_long, Core == "CY-01A")
cy02_sec5_long <- filter(sediment_sec5_long, Core == "CY-02A")
cy03_sec5_long <- filter(sediment_sec5_long, Core == "CY-03A")
ha02_sec5_long <- filter(sediment_sec5_long, Core == "HA-02A")
ha03_sec5_long <- filter(sediment_sec5_long, Core == "HA-03A")
ha04_sec5_long <- filter(sediment_sec5_long, Core == "HA-04A")
ha05_sec5_long <- filter(sediment_sec5_long, Core == "HA-05A")
ha06_sec5_long <- filter(sediment_sec5_long, Core == "HA-06A")
ks01_sec5_long <- filter(sediment_sec5_long, Core == "KS-01A")
ks02_sec5_long <- filter(sediment_sec5_long, Core == "KS-02A")
ks03_sec5_long <- filter(sediment_sec5_long, Core == "KS-03A")
sl01_sec5_long <- filter(sediment_sec5_long, Core == "SL-01A")
sl02_sec5_long <- filter(sediment_sec5_long, Core == "SL-02A")
sl03_sec5_long <- filter(sediment_sec5_long, Core == "SL-03A")
sm01_sec5_long <- filter(sediment_sec5_long, Core == "SM-01A")
sm02_sec5_long <- filter(sediment_sec5_long, Core == "SM-02A")
sm03_sec5_long <- filter(sediment_sec5_long, Core == "SM-03A")
sm04_sec5_long <- filter(sediment_sec5_long, Core == "SM-04A")
sm05_sec5_long <- filter(sediment_sec5_long, Core == "SM-05A")
sm06_sec5_long <- filter(sediment_sec5_long, Core == "SM-06A")

cy01_sec5_plot <- ggplot(cy01_sec5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
cy02_sec5_plot <- ggplot(cy02_sec5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
cy03_sec5_plot <- ggplot(cy03_sec5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
ha02_sec5_plot <- ggplot(ha02_sec5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
ha03_sec5_plot <- ggplot(ha03_sec5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
ha04_sec5_plot <- ggplot(ha04_sec5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
ha05_sec5_plot <- ggplot(ha05_sec5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
ha06_sec5_plot <- ggplot(ha06_sec5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
ks01_sec5_plot <- ggplot(ks01_sec5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
ks02_sec5_plot <- ggplot(ks02_sec5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
ks03_sec5_plot <- ggplot(ks03_sec5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
sl01_sec5_plot <- ggplot(sl01_sec5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
sl02_sec5_plot <- ggplot(sl02_sec5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
sl03_sec5_plot <- ggplot(sl03_sec5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
sm01_sec5_plot <- ggplot(sm01_sec5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
sm02_sec5_plot <- ggplot(sm02_sec5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
sm03_sec5_plot <- ggplot(sm03_sec5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
sm04_sec5_plot <- ggplot(sm04_sec5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
sm05_sec5_plot <- ggplot(sm05_sec5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()
sm06_sec5_plot <- ggplot(sm06_sec5_long, aes(x=Depth, y=Proportion, fill=Size)) + geom_area(alpha=0.8, size=0.5) + 
  scale_fill_viridis(discrete=T) + 
  coord_flip() + scale_x_reverse()