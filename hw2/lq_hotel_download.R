# Load Required packages
source("check_packages.R")
check_packages(c("httr"))

# Load list
load(file="lq/list.Rdata")

dir.create("lq/hotels/", showWarnings = FALSE)

# Loop over hotels and download their pages
for(i in 1:nrow(d))
{
    url = d[i,"url"]
    page = GET(url)

    s = content(page, as="text")

    write(s, file=paste0("lq/hotels/",1,".html"))
 
    Sys.sleep(1) # wait before grabbing the next page
}