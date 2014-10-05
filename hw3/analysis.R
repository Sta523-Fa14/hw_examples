library(rgdal)

# To read GeoJSON must use OGRGeoJSON as layer
p = readOGR("precinct_in.json","OGRGeoJSON") 

# Include precinct IDs as data with column named `precinct`
p@data


# Write to GeoJSON
writeOGR(p, "./out", "", driver="GeoJSON") # Creates out file, current version of 
                                           # GDAL does not allow . in file names 
                                           # so we have to rename the file afterwards
file.rename("./out", "./precinct.json")

p2 = readOGR("precinct.json","OGRGeoJSON") 

p2@data


# Compare results
pdf("plot.pdf",width=10,height=5)

par(mfrow=c(1,2))

# alpha affects alpha blending (makes things transparent), useful if polys may overlap
plot(p,main = "precinct_in.json", axes=TRUE, col=adjustcolor(2:6,alpha=0.5))
plot(p2,main = "precinct.json", axes=TRUE, col=adjustcolor(2:6,alpha=0.5))

dev.off()