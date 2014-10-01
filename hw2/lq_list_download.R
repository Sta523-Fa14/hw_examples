# Load Required packages
source("check_packages.R")
check_packages(c("httr"))

url = "http://www.lq.com/en/findandbook/hotel-listings.html"

page = GET(url)

s = content(page, as="text")

dir.create("lq/", showWarnings = FALSE)

write(s, file="lq/list.html")