library("tsbugs")
par(mfrow=c(2,1)) 
precio <- ts(semantic$precio_promedio, start=c(2001, 1), end=c(2016, 11), frequency=12) 
plot(precio, main="Level (ft)") 
plot(diff(precio), main="Differenced Level")
ar1 <- ar.bugs(y=diff(precio), ar.order=1)
print(ar1)

ar2 <- ar.bugs(y=diff(precio), ar.order=2, ar.prior="dunif(-1,1)", 
               var.prior="dgamma(0.001,0.001)", k = 10, 
               mean.centre = TRUE)
print(ar2)

writeLines(ar2$bug, "ar2.txt") 

library("R2OpenBUGS")

inits<-function(){list(phi0=0,phi1=0,phi2=0,sigma2=0,y.new=rep(0,191))}

ar2.bug <- bugs(data = ar2$data,
                inits = inits,
                param = c(nodes(ar2, "prior")$name, "y.new"),
                model = "ar2.txt", 
                n.iter = 11000, n.burnin = 1000, n.chains = 1)
