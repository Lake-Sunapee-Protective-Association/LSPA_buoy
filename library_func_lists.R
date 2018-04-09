library(tidyverse)
library(ggthemes)
library(readxl)

##functions
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

##ggplot grom
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5))


##create lists of sensors
alltemp <- c("TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m")
alltemp2013 <- c("TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m")
alltemp2011 <- c("TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m")
alltemp2007 <- c("TempC_0m", "TempC_0p5m", "TempC_1m", "TempC_1p5m", "TempC_2m", "TempC_2p5m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m", "TempC_11m", "TempC_13m")
alltemp2007LMP <- c("TempC_0m", "TempC_0p5m", "TempC_1m", "TempC_1p5m", "TempC_2m", "TempC_2p5m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m", "TempC_11m", "TempC_12m", "TempC_13m", "TempC_14m")
alltemp2016 <- c("TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m")
upDO <- c("DOTempC", "DOSat", "DOppm")
lowDO <- c("DOLoTempC", "DOLowSat", "DOLowPPM")
allwatersensors <- c("TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m", 
                     "DOTempC", "DOSat", "DOppm", "DOLoTempC", "DOLowSat", "DOLowPPM")
allmet <- c("AirTempC", "RelHum", "PAR", "WindSpd", "CorrWind", "WindSpdAv", "WindVect", "MaxWind", "MaxWindDir")
allmet2016 <- c("AirTempC", "RelHum", "PAR", "WindSpdAv", "WindVect", "MaxWind", "MaxWindDir")
wind <- c('WindSpdAv', 'WindVect', 'MaxWind', 'MaxWindDir', 'CorrWind', 'WindSpd')
wind2016 <-  c('WindSpdAv', 'WindVect', 'MaxWind', 'MaxWindDir')
chla <- c('Chlor_RFU', 'Chlor_UGL', 'SpecCond')
hobotemp <- c('TempC_1m_hobo', 'TempC_2m_hobo', 'TempC_3m_hobo', 'TempC_4m_hobo', 'TempC_5m_hobo', 'TempC_6m_hobo', 'TempC_7m_hobo', 'TempC_8m_hobo', 'TempC_9m_hobo')
air <-  c('AirTempC', 'RelHum')


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
