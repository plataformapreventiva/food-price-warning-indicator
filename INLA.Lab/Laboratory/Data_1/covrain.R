source("lattice.txt")
elev=read.table("elevdata.txt",header=T)
elevmat=matrix(elev[,3],ncol=101,byrow=T)

zgrad3=scan("zgrad3.out")
zgrad3mean=zgrad3-mean(zgrad3)

oomit=elev$x!=1000 & elev$y!=500
z1=elev$elev[oomit]
mz1=mean(z1)
z1=z1-mean(z1)
z=cbind(rep(1,20000),z1,zgrad3mean[elev$x!=1000 & elev$y!=500])

grid=expand.grid((c(0:159)-30)*5,(c(0:259)-30)*5)
crule=list(x=grid[,2],y=grid[,1],w=rep(500*1000/(200*100),260*160))

m=dim(elev)[1]
rule=list(x=elev$x[oomit],y=elev$y[oomit],w=rep(500*1000/length(elev$x[oomit]),length(elev$x[oomit])))

elevim=im(t(elevmat-mz1),xcol=10*c(0:200)/2,yrow=10*c(0:100)/2)

gradim=im(t(matrix(zgrad3mean,ncol=101,byrow=T)),xcol=10*c(0:200)/2,yrow=10*c(0:100)/2)

## grid the environmental covariates
dummy<-default.dummy(data.pp, ntile=50)
all.pp<-superimpose(data.pp, dummy)
grid= janine.grid(rotate(all.pp),nrow=nrow, ncol=ncol, edge=0.00)

elevation<-lookup.im(elevim, all.pp$x, all.pp$y)
elev.grid = (janine.gridit(rotate(all.pp), elevation, grid))

gradient<-lookup.im(gradim, all.pp$x, all.pp$y)
grad.grid = (janine.gridit(rotate(all.pp), gradient, grid))


