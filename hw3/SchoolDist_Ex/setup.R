library(rgdal)
library(rgeos)

x = readOGR("/home/vis/cr173/Sta523/data/parking/pluto/Manhattan/","MNMapPLUTO",stringsAsFactors=FALSE)

s = data.frame( gCentroid(x,byid=TRUE), SchoolDist = x@data$SchoolDist, stringsAsFactors=FALSE)
s$SchoolDist = as.integer(as.character(s$SchoolDist))

s = na.omit(s)

n_scramble = round(0.1*nrow(s))
to = sample(1:nrow(s),n_scramble,replace=TRUE)
from = sample(1:nrow(s),n_scramble,replace=TRUE)

s$SchoolDist[to] = s$SchoolDist[from]

s = s[sample(1:nrow(s),round(0.3*nrow(s)),replace=TRUE),]

cols = c("#7fc97f","#beaed4","#fdc086","#ffff99","#386cb0","#f0027f","#bf5b17")
cols[10] = "#bf5b17"

save(s, cols, file="data.Rdata")


