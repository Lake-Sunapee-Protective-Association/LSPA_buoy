library(tidyverse)
library(ggthemes)
library(readxl)

# ##functions
# substrRight <- function(x, n){
#   substr(x, nchar(x)-n+1, nchar(x))
# }

##ggplot grom
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5))


##create lists of sensors
alltemp <- c("TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m")
alltemp2011 <- c("TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m")
alltemp2007 <- c("TempC_0m", "TempC_0p5m", "TempC_1m", "TempC_1p5m", "TempC_2m", "TempC_2p5m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m", "TempC_11m", "TempC_13m")
alltemp2007LMP <- c("TempC_0m", "TempC_0p5m", "TempC_1m", "TempC_1p5m", "TempC_2m", "TempC_2p5m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m", "TempC_11m", "TempC_12m", "TempC_13m", "TempC_14m")
alltemp2016 <- c("TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m")
upDO <- c("DOTempC", "DOSat", "DOppm")
lowDO <- c("DOLowTempC", "DOLowSat", "DOLowPPM")
chla <- c('Chlor_RFU', 'Chlor_UGL', 'SpecCond')
hobotemp <- c('TempC_1m_hobo', 'TempC_2m_hobo', 'TempC_3m_hobo', 'TempC_4m_hobo', 'TempC_5m_hobo', 'TempC_6m_hobo', 'TempC_7m_hobo', 'TempC_8m_hobo', 'TempC_9m_hobo')
air <-  c('AirTempC', 'RelHum')


