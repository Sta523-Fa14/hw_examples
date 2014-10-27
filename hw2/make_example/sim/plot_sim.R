load(file = "sim_results.Rdata")

png("sim.png")

x = x[1:100]
y = y[1:100]

plot(x[d<=1], y[d<=1], pch=16, col='green', xlab="", ylab="")
points(x[d>1], y[d>1], pch=16)

lx = seq(0, 1, length.out = 300)
ly = sqrt(1-lx^2)

lines(lx, ly, col="blue", lwd=3)

dev.off()