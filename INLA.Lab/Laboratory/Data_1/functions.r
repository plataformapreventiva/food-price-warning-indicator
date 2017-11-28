inla.squishplot = function (xlim, ylim, asp = 1, newplot = TRUE)
{
## This function is a copy from package TeachingDemos

if (length(xlim) < 2)
stop("xlim must be a vector of length 2")
if (length(ylim) < 2)
stop("ylim must be a vector of length 2")
if (newplot)
plot.new()
tmp <- par(c("plt", "pin", "xaxs", "yaxs"))
if (tmp$xaxs == "i") {
xlim <- range(xlim)
} else {
tmp.r <- diff(range(xlim))
xlim <- range(xlim) + c(-1, 1) * 0.04 * tmp.r
}
if (tmp$yaxs == "i") {
ylim <- range(ylim)
} else {
tmp.r <- diff(range(ylim))
ylim <- range(ylim) + c(-1, 1) * 0.04 * tmp.r
}
tmp2 <- (ylim[2] - ylim[1])/(xlim[2] - xlim[1])
tmp.y <- tmp$pin[1] * tmp2 * asp
if (tmp.y < tmp$pin[2]) {
par(pin = c(tmp$pin[1], tmp.y))
par(plt = c(tmp$plt[1:2], par("plt")[3:4]))
} else {
tmp.x <- tmp$pin[2]/tmp2/asp
par(pin = c(tmp.x, tmp$pin[2]))
par(plt = c(par("plt")[1:2], tmp$plt[3:4]))
}
return(invisible(tmp["plt"]))
}

inla.display.matrix = function(x, wrap=TRUE, xaxt=FALSE, yaxt=FALSE, col=gray(seq(0, 1, len=256)), ...)
{
## display a matrix as an image with correct layout and autoscaling

x = as.matrix(x)
n = dim(x)
y = x
if (wrap) {
ii = 1L:n[1L]
jj = n[1L]:1L
for(j in 1L:n[2L])
y[ii, j] = x[jj, j]
}

## use the image.plot-function in package fields; its much better...
#inla.squishplot(c(0, 1), c(0, 1), n[1]/n[2])
image(t(y), col=col, bty="n", xaxt="n", yaxt="n", ...)

box()
if (xaxt) {
title(xlab="")
nn = (n[2]+1)%/%2
axis(1, at=c(0,(nn-1)/(n[2]-1), 1), labels=as.character(c(1, nn, n[2])))
}
if (yaxt) {
title(ylab="")
nn = (n[1]+1)%/%2
axis(2, at=c(0,(nn-1)/(n[1]-1), 1), labels=as.character(c(1, nn, n[1])))
}
}

midpoints.func=function(x.win,nrow,ncol)
{
eps=c((max(x.win$x)-min(x.win$x))/ncol/2,(max(x.win$y)-min(x.win$y))/nrow/2)
x.x=matrix(rep(seq(min(x.win$x)+eps[1],max(x.win$x)-eps[1],length=ncol),nrow),nrow,ncol,byrow=T)
x.y=matrix(rep(seq(max(x.win$y)-eps[2],min(x.win$y)+eps[2],length=nrow),ncol),nrow,ncol)
x.ppp=ppp(x.x,x.y,window=x.win)
return(x.ppp)
}


plot.cc<-function(res.cc)
{
f.ex=splinefun(res.cc[[1]],res.cc[[2]],method="natural")
r=res.cc[[1]]
mat=matrix(0,nrow=3,ncol=length(r))
for (i in 1:3)
	mat[i,]=res.cc[[(2*i)]]
matplot(r,t(mat),type="lll",lty=c(1,2,2),lwd=c(2,1,1),col=c(1),xlab="Distance d in meters",
		font=1,ylab=expression(f[cc](d)),cex.lab=1.4,cex.axis=1.4,main="")
#matplot(r,t(mat),type="lll",lty=c(1,2,2),lwd=c(2,1,1),col=c(1),xlab="Distance d in meters",
#		font=1,ylab=expression(hat(f)[cc](d)),cex.lab=1.1,cex.axis=1.1,main="")
}

im.matrix<-function (x, wrap = TRUE, xaxt = FALSE, yaxt = FALSE) 
{
	#if (!is.matrix(x)) 
    #	stop("First argument must be a matrix")
    n = dim(x)
    y = x
    if (wrap) {
        for (j in 1:n[2]) y[1:n[1], j] = x[n[1]:1, j]}
    inla.squishplot(c(0, 1), c(0, 1), n[1]/n[2])
    image.plot(t(y), col = gray(seq(0, 1, len = 256)), bty = "n",xaxt = "n", yaxt = "n",cex.axis=2)
}


func.im.area<-function(vec,ns,index.in,bg)
{
	x.mat<-rep(bg,ns)
	x.mat[index.in]<-vec
	im.matrix(matrix(x.mat,nrow,ncol))
}

func.im.area2<-function(vec,ns,index.in,bg)
{
	x.mat<-rep(bg,ns)
	x.mat[index.in]<-vec
	inla.squishplot(c(0,1),c(0,1),nrow/ncol)
	im.matrix(matrix(x.mat,nrow,ncol))
}

### Two alternatives for the NN-covariate   

### Alt. 1: Accurate distance to nearest point outside cell 

func.nn.exact<-function(x.ppp,x.p,y.count)
{
b<-nncross(x.ppp,x.p)$dist

index<-seq(1:(x.ppp$n))[y.count>0]
i.seq<-seq(1:length(index))

a<-x.ppp[y.count>0]
a.count<-y.count[y.count>0]
for (i in i.seq)
{
	pp.super<-superimpose(a[i],x.p)
	b[index[i]]<-nndist(pp.super,k=(a.count[i]+1))[1]
}
return(b)
}	

### Alt. 2: Distance between all midpoints to midpoints of cells containing points.

func.nn.mid=function(x.ppp,x.p,y.count)
{
a<-x.ppp[y.count>0]
b<-nncross(x.ppp,a)$dist
b[y.count>0]=nndist(a)
return(b)
}


### Assign covariate value to first order neighbours
func.grid.cov2=function(cov.pp,x.grid,mid.p)
{
	c1=rep(NA,mid.p$n)
	c1.values=cov.pp$marks
	cdist=nncross(mid.p,cov.pp)$which
	c1=c1.values[cdist]
	return(c1)
}	




