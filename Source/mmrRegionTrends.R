# mmrRegionTrends.R              damiancclarke             yyyy-mm-dd:2015-06-26
#---|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8
#
#
# Take regional MMR estimates and plot the using lattice plots.
#

library(foreign)
library(lattice)

DAT <- '/home/damiancclarke/investigacion/2013/WorldMMR/Data/'
OUT <- '/home/damiancclarke/investigacion/2013/WorldMMR/Results/Graphs/region/'

mmr <- read.dta(paste(DAT,'mmrRegions.dta',sep=""))
names(mmr) <- c("regCode","country","regName","years","MMR","MMRate","fert")



subset <- "Zimbabwe"
postscript(paste(OUT,subset,".eps",sep=""),
               horizontal = FALSE, onefile = FALSE, paper = "special",
               height=7, width=9)

xyplot(MMR ~ years | regName, data=mmr[mmr$country==subset,], xlab="Years",
       ylab="MMR",main = subset,scales = list(x = list(rot = 55)),
       panel = function(x, y) {
           panel.grid(h = -1, v = 2)
           panel.xyplot(x, y)
           llines(lowess(x, y))
       })
dev.off()



