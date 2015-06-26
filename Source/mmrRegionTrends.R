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



countryPlot <- function(country) {
    postscript(paste(OUT,country,".eps",sep=""),
               horizontal = FALSE, onefile = FALSE, paper = "special",
               height=7, width=9)

    print(xyplot(MMR ~ years | regName, data=mmr[mmr$country==country,],
                 xlab="Years",ylab="MMR",main = country,
                 scales = list(x = list(rot = 55)),
                 panel = function(x, y) {
                     panel.grid(h = -1, v = 2)
                     panel.xyplot(x, y)
                     llines(lowess(x, y))
                 }))
    dev.off()
}

countryPlot(country="Benin")
countryPlot(country="Bolivia")
countryPlot(country="Brazil")
countryPlot(country="Burkina-Faso")
countryPlot(country="Burundi")
countryPlot(country="Cambodia")
countryPlot(country="Cameroon")
countryPlot(country="Chad")
countryPlot(country="Congo-Brazzaville")
countryPlot(country="Congo-Democratic-Republic")
countryPlot(country="Cote-d-Ivoire")
countryPlot(country="Dominican-Republic")
countryPlot(country="Ethiopia")
countryPlot(country="Gabon")
countryPlot(country="Indonesia")
countryPlot(country="Madagascar")
countryPlot(country="Mali")
countryPlot(country="Morocco")
countryPlot(country="Mozambique")
countryPlot(country="Namibia")
countryPlot(country="Niger")
countryPlot(country="Peru")
countryPlot(country="Philippines")
countryPlot(country="Rwanda")
countryPlot(country="Senegal")
countryPlot(country="South-Africa")
countryPlot(country="Tanzania")
countryPlot(country="Uganda")
countryPlot(country="Zimbabwe")

