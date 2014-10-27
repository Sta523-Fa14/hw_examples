---
title: "Analyze data"
date: "September 29, 2014"
output: html_document
---


```{r}
# Load Required packages
source("check_packages.R")
check_packages(c("fields","maptools","stringr"))


load(file = "NC_Starbucks.Rdata")

r = r[,c("id", "name", "address", "lat", "long")]
```


```{r}
find_cluster = function(r, rads=c(0.1, 0.25, 0.5, 0.75, 1))
{
    locs = r[,c("long","lat")]

    d = rdist.earth(locs, miles = TRUE)

    for(rad in rads)
    {   
        z = t(apply(d,1, function(x) x<rad))
        counts = apply(z,1,sum)

        l = z[which.max(counts),]

        cat("\n\nWithin",rad,"miles ...\n\n")
        
        print(r[l,])
    }
}
```


# Find cluster
```{r}
find_cluster(r)

plot(0,0, type='n', xlab="",ylab="",main="",
     xlim=c(-80.8,-81), ylim=c(35.1,35.3),
     mar=c(3,3,2,2))

nc_shp = readShapeSpatial(system.file("shapes/sids.shp", package="maptools")[1])

plot(nc_shp, add=TRUE)
rect(-80.92,35.2,-80.95,35.25, border='red')
points(locs)
```


```{r}
r[str_detect(r$name,"CLT"),]

find_cluster(r[!str_detect(r$name,"CLT"),])
```