## Data
load(file="data.Rdata")
cols = adjustcolor(cols, alpha=0.5)

library(rgdal)
nybb = readOGR(path.expand("~/Sta523/data/parking/nybb/"),"nybb",stringsAsFactors=FALSE)
manh = nybb[2,]



## Convex Hull

data = s

o = order(data$SchoolDist)
pts = as.matrix( data[o, c("x","y")] )
rownames(pts) = data$SchoolDist[o]

spts = SpatialPoints(pts)
ch = gConvexHull(spts, byid=TRUE)

data = data.frame(SchoolDist = as.integer(sapply(ch@polygons, function(x) x@ID), stringsAsFactors=FALSE))
chd = SpatialPolygonsDataFrame(ch, data, match.ID=FALSE)


## SVM

library(e1071)
k=svm(as.factor(SchoolDist)~.,data=s,cross=10)
#plot(k,data=s)

library(raster)
r = rasterize(manh, raster(ncols=500,nrows=1000,ext=extent(bbox(manh))))

cells = which(!is.na(r[]))
crds = xyFromCell(r,cells)

z = predict(k,crds)

r[cells] = as.numeric(as.character(z))

dist = sort(unique(s$SchoolDist))

l=list()
for(i in seq_along(dist))
{
  l[[i]] = rasterToPolygons(r, function(x) x==dist[i], dissolve=TRUE)
  l[[i]]@polygons[[1]]@ID = as.character(dist[i])
  rownames(l[[i]]@data) = dist[i]
  colnames(l[[i]]@data) = "SchoolDist"
}

pd = do.call(rbind, l)

writeOGR(pd, "./out", "", driver="GeoJSON")
file.rename("./out", "./district_svm.json")




## Truth

nysd = readOGR("nysd/","nysd")
manh_sd = nysd[nysd$SchoolDist %in% dist,]
manh_sd = manh_sd

manh_sd = spTransform(manh_sd, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))



par(mfrow=c(1,4), mar=c(1,1,4,1))
default_plot = function(main="")
  plot(0,0,type='n', 
       xlim=c(-74.01812,-73.90853), 
       ylim=c(40.70196, 40.87733),
       main = main, axes=FALSE,
       xlab="", ylab="")

# Data

default_plot("Data")
points(s$x,s$y,col=cols[s$SchoolDist],pch=16, cex=0.4)
plot(manh,add=TRUE)

# Convex Hull
default_plot("Convex Hull")
plot(chd, col = cols[chd@data$SchoolDist], add=TRUE)

# SVM Prediction
default_plot("SVM")
plot(pd, col = cols[pd@data$SchoolDist], add=TRUE)

# Truth
default_plot("Truth")
plot(manh_sd, col = cols[manh_sd@data$SchoolDist], add=TRUE)


