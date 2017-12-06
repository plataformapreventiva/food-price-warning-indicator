#
#
#	Date: 4 June 2016
#
#	R version: 3.2.2
#

rm(list=ls())

# install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/stable")
# install.packages('sp')
# install.packages('spatstat')
# install.packages('mvtnorm')
# install.packages('lattice')
# install.packages('mgcv')
# install.packages('pixmap')
# install.packages('numDeriv')
# install.packages('fields')

wpath <- 'C:/JCMO.Trabajo/Seminars,Visits&Talks/16-06.BUC4/Laboratory/'

library('sp')
library('INLA')
library('spatstat')
library('mvtnorm')
library('lattice')
library('mgcv')
library('pixmap')
library('numDeriv')
library('fields')

source(paste(wpath,'Data_1/functions.r',sep=''))

# 2. A simple point process

# Reading in and gridding the data
# We read in the data as:
paul <- read.delim(paste(wpath,'Data_1/paul.txt',sep=''))

# type 5 is Andersonia heterophylla
data <- paul[paul$type=="5",]
x <- data$x/10
y <- data$y/10

# We transform the data into a point pattern object (using several commands from the library
# spatstat, for details check the library help files). Ignore the warning about duplicated
# points.
x.area <- 22
x.win <- owin(c(0, x.area),c(0, x.area))

data.pp=ppp(x,y,window=x.win)
plot(data.pp, main= " Andersonia heterophylla")

# We now need to transform the data, i.e. construct a grid with 30 x 30 cells
nrow <- 30
ncol <- nrow
x.grid <- quadrats(x.win,ncol,nrow)

# and count the number of points in each grid cell; note that this will be our response variable.
count.grid <- quadratcount(data.pp, tess=x.grid)
plot(count.grid)

#----------------------------------------------------
# I.- First model (no covariates)

# We have to transform the grid of counts into a vector (and we now use the notation from the
# slides for the response variable):
Y <- as.vector(count.grid)

# The number of grid cells
n <- ncol*nrow

# And calculate the area of the grid cells:
cell.area <- x.area^2/n
E <- rep(cell.area, n)
# INLA requires separate indices for each of the spatial vector and the error term.
I <- 1:n
J <- 1:n

# We have to specify a prior for the spatial vector
prior.spat <- c(1,0.00005) #Default!
hyper.spat <- list(prec=list(param=prior.spat))

# We can no specify the model formula
formula <- Y ~ 1+ 
				f(I, model="rw2d", nrow=nrow, ncol=ncol, hyper=hyper.spat)+
				f(J, model="iid")

# and run the model (this should take only a few seconds at most)
result <- inla(	formula,data=data.frame(Y,I,J),
				family="poisson",E=E, verbose=TRUE,
				control.compute=list(dic=TRUE))

# We can look at a summary and a plot of the results
summary(result)
plot(result)

# The estimated intercept
result$summary.fixed

# the (posterior mean of the) spatial eect
f.spat <- result$summary.random$I$mean

# plot it
im.matrix(matrix(f.spat, ncol, nrow))

# the error term
f.unstruct <- result$summary.random$J$mean
im.matrix(matrix(f.unstruct, nrow, ncol))

# the resulting fit (compare with original pattern) scaled up by the area.
fit <- E*result$summary.fitted.values$mean
im.matrix(matrix(fit, nrow, ncol))

#----------------------------------------------------
# II.- Second model (with covariates)

# Again we start by reading in the data and gridding it; 
# read in the data it is a big data set and
# takes a while to load up.
nrow <- 50
ncol <- 100
n <- nrow*ncol
BCIData <- read.delim(paste(wpath,'Data_1/Routput.txt',sep=''))

species <- BCIData[BCIData$sp=="beilpe",]
spec.string <- "Beilschmiedia p.L."
x <- species$gx[is.na(species$gx)==F]
y <- species$gy[is.na(species$gy)==F]
x.win <- owin(c(0,1000),c(0,500))
data.pp <- ppp(x,y, window=x.win)
plot(data.pp, main=spec.string)

Area <- rep(1,nrow*ncol)

# Generate the count grid
x.grid <- quadrats(x.win,ncol,nrow)
count.grid <- quadratcount(data.pp, tess=x.grid)

# Find midpoints of each cell
mid.p <- midpoints.func(x.win,nrow,ncol)

# We also need to read in the covariate data, elevation and gradient. 
# This is done in the file 'read_cov.txt' 
# (check details if you are interested but it depends on the format of 
# the data and is hence not that interesting here):
# source(paste(wpath,'Data_1/read_cov.txt',sep=''))
elev <- read.table(paste(wpath,'Data_1/elevdata.txt',sep=''),header=T)

elevmat <- matrix(elev[,3],ncol=101,byrow=T)
oomit <- elev$x!=1000 & elev$y!=500
z1 <- elev$elev[oomit]
mz1 <- mean(z1)
elevim <- im(t(elevmat-mz1),xcol=10*c(0:200)/2,yrow=10*c(0:100)/2)
elev.mean <-lookup.im(elevim,mid.p$x,mid.p$y)

zgrad3 <- scan(paste(wpath,'Data_1/zgrad3.out',sep=''))
zgrad3mean <- zgrad3-mean(zgrad3)
gradim <- im(t(matrix(zgrad3mean,ncol=101,byrow=T)),xcol=10*c(0:200)/2,yrow=10*c(0:100)/2)
grad.mean <- lookup.im(gradim,mid.p$x,mid.p$y)

Soil <- read.csv(paste(wpath,'Data_1/bci.block20.data.csv',sep=''),dec=",")
soil.x <- Soil$x
soil.y <- Soil$y
soil.pp <- ppp(x=soil.x,y=soil.y,window=x.win)

soil.index <- c(3:15)
no.cov <- length(soil.index) # Number of covariates
cov.mat<-matrix(0,nrow=n,ncol=no.cov)
for (i in 1:no.cov){
  soil.pp <- ppp(x=soil.x, y=soil.y, window=x.win,marks=Soil[,soil.index[i]])
  # Assign covariate values to the four neighbouring cells. 
  cov.mat[,i] <- func.grid.cov2(soil.pp,x.grid,mid.p) 
  }

# Log-transform all covariates except N.min
cov.mat.log <- cov.mat
cov.mat.log[,-12] <- log(cov.mat[,-12])

for(i in 1:no.cov){
  cov.mat.log[,i]=cov.mat.log[,i]-mean(cov.mat.log[,i])
  }

cov.string <- c("Elev.","Slope",names(Soil)[3:(no.cov+2)])
cov.all <- cbind(elev.mean,grad.mean,cov.mat.log)

# Plot the centred elevation (as stored in elev.mean)
im.matrix(matrix(elev.mean,nrow,ncol))

# Plot the centred gradient (as stored in grad.mean)
dev.new()
im.matrix(matrix(grad.mean,nrow,ncol))

# The response
Y <- as.vector(count.grid)

# the explanatory variables
Z1 <- as.vector(elev.mean)
Z2 <- as.vector(grad.mean)

# the grid cell index for the spatial eects
I <- 1:n
J <- 1:n

# Use the log-gamma prior for the precision
param.spat <- list(prec=list(prior="pc.prec",param=c(u=1,alpha=0.01)))

# And we fit a model with the two covariates, a spatially structured 
# vector and an error term, as before.

formula <- Y ~ 	1 + 
				Z1 + 
				Z2 + 
				f(I, model="rw2d", nrow=nrow, ncol=ncol, 
				  hyper=param.spat, scale.model=T)+ 
				f(J, model="iid")

result.rain <- inla(formula, data=data.frame(Y,Z1,Z2, I, J), 
					family = "poisson",
					E=Area, verbose = TRUE , control.compute=list(dic=TRUE))

# This might take a few minutes (96 seconds when I last ran it). 
# If you want to use a quick and dirty (i.e. slightly less exact) method use

result.rain2 <- inla(formula, data=data.frame(Y,Z1,Z2, I,J), 
					family = "poisson",
					E=Area, verbose = TRUE , control.compute=list(dic=TRUE),
					control.inla = list(strategy = "gaussian", int.strategy = "eb"))

# Posterior estimates
# Compare result.rain and result.rain2
summary(result.rain)

# The structured and unstructured spatial effects
im.matrix((matrix(result.rain$summary.random$I$mean, nrow, ncol)))
dev.new()
im.matrix((matrix(result.rain$summary.random$J$mean, nrow, ncol)))

# -------------------------------------------------------
# -------------------------------------------------------
# Spatial modelling with INLA - Part 2
# -------------------------------------------------------
# -------------------------------------------------------

# In this practical we will look at some more complex spatial models.
# The aims of this practical are to get a general understanding of how to fit
# - joint models/ models with several likelihoods
# - marked point processes (qualitatively marked point patterns, quantitatively marked point pat-
# terns)
# - models of replicated point patterns

# -------------------------------------------------------
# 1. Third model: A joint model for to species
rm(list=ls())

library('spatstat')
library('mvtnorm')
library('lattice')
library('mgcv')
library('pixmap')
library('numDeriv')
library('fields')
library('INLA')

# specify the working directory (the path will be dierent on your machine; change accordingly):
wpath <- 'C:/JCMO.Trabajo/Seminars,Visits&Talks/16-06.BUC4/Laboratory/'

setwd(paste(wpath,'Data_1/',sep=''))

# read in some functions that we will need later
source("functions.r")

# (a) Reading in and gridding the data
# We read in the data as:
nrow <- 50
ncol <- 100
n <- nrow*ncol
BCIData <- read.delim(paste(wpath,'Data_1/Routput.txt',sep=''))
species1 <- BCIData[BCIData$sp=="protte",]
spec.string1 <- "Protium tenuifolium"
species2 <- BCIData[BCIData$sp=="protpa",]
spec.string2 <- "Protium panamense"

# We transform the data into two point pattern objects before
species <- species1
x <- species$gx[is.na(species$gx)==F]
y <- species$gy[is.na(species$gy)==F]
x.win <- owin(c(0,1000),c(0,500))
cell.area <- 100
data1.pp <- ppp(x,y, window=x.win)
E1 <- rep(cell.area, n)
species <- species2
x <- species$gx[is.na(species$gx)==F]
y <- species$gy[is.na(species$gy)==F]
x.win <- owin(c(0,1000),c(0,500))
data2.pp <- ppp(x,y, window=x.win)
E2 <- rep(cell.area, n)
E <- c(E1,E2)

# and count the number of points in each grid cell; 
# note that these will be our two response variables.
x.grid <- quadrats(x.win,ncol,nrow)
count1.grid <- quadratcount(data1.pp, tess=x.grid)
count2.grid <- quadratcount(data2.pp, tess=x.grid)

# The midpoints
mid.p <- midpoints.func(x.win,nrow,ncol)

# We read in the soil covariate data along with elevation and gradient.
# source(paste(wpath,'Data_1/read_cov.txt',sep=''))
elev <- read.table(paste(wpath,'Data_1/elevdata.txt',sep=''),header=T)

elevmat <- matrix(elev[,3],ncol=101,byrow=T)
oomit <- elev$x!=1000 & elev$y!=500
z1 <- elev$elev[oomit]
mz1 <- mean(z1)
elevim <- im(t(elevmat-mz1),xcol=10*c(0:200)/2,yrow=10*c(0:100)/2)
elev.mean <-lookup.im(elevim,mid.p$x,mid.p$y)

zgrad3 <- scan(paste(wpath,'Data_1/zgrad3.out',sep=''))
zgrad3mean <- zgrad3-mean(zgrad3)
gradim <- im(t(matrix(zgrad3mean,ncol=101,byrow=T)),xcol=10*c(0:200)/2,yrow=10*c(0:100)/2)
grad.mean <- lookup.im(gradim,mid.p$x,mid.p$y)

Soil <- read.csv(paste(wpath,'Data_1/bci.block20.data.csv',sep=''),dec=",")
soil.x <- Soil$x
soil.y <- Soil$y
soil.pp <- ppp(x=soil.x,y=soil.y,window=x.win)

soil.index <- c(3:15)
no.cov <- length(soil.index) # Number of covariates
cov.mat<-matrix(0,nrow=n,ncol=no.cov)
for (i in 1:no.cov){
  soil.pp <- ppp(x=soil.x, y=soil.y, window=x.win,marks=Soil[,soil.index[i]])
  # Assign covariate values to the four neighbouring cells. 
  cov.mat[,i] <- func.grid.cov2(soil.pp,x.grid,mid.p) 
  }

# In this model we work with the principal components of the covariates (just for simplicity)
pc <- princomp(cov.all,cor=T)
pc.log <- pc$scores[,1:3]

# Log-transform all covariates except N.min
cov.mat.log <- cov.mat
cov.mat.log[,-12] <- log(cov.mat[,-12])

for(i in 1:no.cov){
  cov.mat.log[,i]=cov.mat.log[,i]-mean(cov.mat.log[,i])
  }

cov.string <- c("Elev.","Slope",names(Soil)[3:(no.cov+2)])
cov.all <- cbind(elev.mean,grad.mean,cov.mat.log)

# Plot the centred elevation (as stored in elev.mean)
im.matrix(matrix(elev.mean,nrow,ncol))

#
# -- END: BUC4.Laboratory.R --