# Do parsing stuff

# ...

# df of Hotel names and urls
d = data.frame(name= "La Quinta Inn & Suites Flagstaff",
               url = "http://www.lq.com/en/findandbook/hotel-details.flagstaff.address.html",
               stringsAsFactors = FALSE) 

# ...
 

# Save results as Rdata file
save(d, file="lq/list.Rdata")
