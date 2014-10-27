suppressMessages(library(rgdal))

tz = readOGR(path.expand("../world/"),"tz_world_mp",verbose=FALSE)

cols = c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5","#ffed6f")

png("tz.png", width=2000, height=1100)
plot(tz, col=cols)
dev.off()