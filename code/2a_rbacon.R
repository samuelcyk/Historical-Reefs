#### library loading ####
library(rbacon)
?Bacon
##lab ID Age Error Depth cc=2 for marine20 cal

#### delta.R and delta.STD calculations ####
## from http://calib.org/marine/index.html?npoints=21&clat=1.2557758872809899&clon=103.75599994287137
## 411, 412, 414, 399, 421, 409, 410, 1846, 612, 501
## 10 closest: -140, 58 (all)
## 9 closest: -161, 44 (all on same shelf - cocoskeeling, <1500km)
## 5 closest: -181, 63 (half <1000km)
## 2 closest: -193, 84 (labelled sg)
## 1: -286, 60 (actual sg)
## 20: -155, 69 (<2000km, except for 490; add 607, 2076, 2077, 2078, 2079, 2080, 2081, 406, 407, 408)
## Southern 2002 -25 ± 20 for SCS
## −235 ± 104 from Chua et al 2020
## chua and southern average = ??

#### run all cores ####
#allcores <- list.files("Bacon_runs")
#for(i in allcores) {
#  try(Bacon(i, delta.R = -235, delta.STD =104, d.min = 0, d.by = 0.5, accept.suggestions=TRUE))
#}
Bacon("CY01", delta.R = -235, delta.STD = 104, d.by = 0.5, d.min = 0, d.max = 93)
Bacon("CY02", delta.R = -235, delta.STD = 104, d.by = 0.5, d.min = 0, d.max = 65)
Bacon("CY03", delta.R = -235, delta.STD = 104, d.by = 0.5, d.min = 0, d.max = 150)
Bacon("HA02", delta.R = -235, delta.STD = 104, d.by = 0.5, d.min = 0, d.max = 149.5)
Bacon("HA03", delta.R = -235, delta.STD = 104, d.by = 0.5, d.min = 0, d.max = 206)
Bacon("HA04", delta.R = -235, delta.STD = 104, d.by = 0.5, d.min = 1, d.max = 35)
Bacon("HA05", delta.R = -235, delta.STD = 104, d.by = 0.5, d.min = 5, d.max = 70)
Bacon("HA06", delta.R = -235, delta.STD = 104, d.by = 0.5, d.min = 0, d.max = 125)
Bacon("KS01", delta.R = -235, delta.STD = 104, d.by = 0.5, d.min = 15, d.max = 45)
Bacon("KS02", postbomb = 3, d.by = 0.5, d.min = 38, d.max = 50) #using calibomb dates
Bacon("KS03", delta.R = -235, delta.STD = 104, d.by = 0.5, d.min = 31, d.max = 90)
Bacon("SL01", delta.R = -235, delta.STD = 104, d.by = 0.5, d.min = 5, d.max = 91)
Bacon("SL02", delta.R = -235, delta.STD = 104, d.by = 0.5, d.min = 6, d.max = 136)
Bacon("SL03", delta.R = -235, delta.STD = 104, d.by = 0.5, d.min = 15, d.max = 130)
Bacon("SM01", delta.R = -235, delta.STD = 104, d.by = 0.5, d.min = 0, d.max = 241)
Bacon("SM02", delta.R = -235, delta.STD = 104, d.by = 0.5, d.min = 15, d.max = 100)
Bacon("SM03", delta.R = -235, delta.STD = 104, d.by = 0.5, d.min = 3, d.max = 201)
Bacon("SM04", delta.R = -235, delta.STD = 104, d.by = 0.5, d.min = 10, d.max = 205)
Bacon("SM05", delta.R = -235, delta.STD = 104, d.by = 0.5, d.min = 0, d.max = 50)
Bacon("SM06", delta.R = -235, delta.STD = 104, d.by = 0.5, d.min = 20, d.max = 260)

#### Coral depths
## CY-01A 0-93 (1cm)
## CY-02A 0-65 (1cm)
## CY-03A 5-150 (5cm)
## HA-02A 0-149.5 (1cm)
## HA-03A 0-206 (1cm)
## HA-04A 1-35 (1cm)
## HA-05A 10-70 (5cm)
## HA-06A 5-125 (5cm)
## KS-01A 20-45 (5cm)
## KS-02A 38-50 (1cm)
## KS-03A 31-90 (1cm)
## SL-01A 5-91 (1cm)
## SL-02A 6-136 (1cm)
## SL-03A 15-130 (1cm)
## SM-01A 0-241 (1cm)
## SM-02A 15-100(1cm)
## SM-03A 18-216 (1cm) -15
## SM-04A 25-215 (5cm) -10
## SM-05A 5-50 (5cm)
## SM-06A 25-260 (5cm)