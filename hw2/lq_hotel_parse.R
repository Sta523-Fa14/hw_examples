# Loop over hotel files

for(file in dir("lq/hotels",full.names=TRUE))
{
    # Do parsing stuff
    # ...
}

lq = data.frame("DATA!!!") # df of La Quinta Data

# ...
# Do any additional parsing to get Lat/Long etc.
# ... 

# Save results as Rdata file
save(lq, file="lq/lq_data.Rdata")
