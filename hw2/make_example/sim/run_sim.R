# Run our pi simulator

n_sim = 1e6

x = runif(n_sim)
y = runif(n_sim)

d = sqrt(x^2+y^2)

save(x,y,d,file="sim_results.Rdata")

n_tab_rows = log10(n_sim)

tab = t(sapply( 10^(1:n_tab_rows), 
                function(x) 
                {
                    n = sum(d[1:x]<1)

                    c(x, n, n/x, 4*n/x)
                }))

tab = data.frame(tab)

tab[,1] = as.integer(tab[,1])
tab[,2] = as.integer(tab[,2])

colnames(tab) = c("1:n", "$d_i<1$", "$\\pi/4$", "$\\pi$")

save(tab, file="sim_tab_results.Rdata")
