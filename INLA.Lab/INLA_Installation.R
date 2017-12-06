# Install INLA

# We have created a standard R-repository, so that 'install.packages' and 'update.packages' will work as expected. You need to add the address to the INLA-repository, as

install.packages("INLA", repos="https://inla.r-inla-download.org/R/stable", dep=TRUE)

# for the stable version, OR


install.packages("INLA", repos="https://inla.r-inla-download.org/R/testing", dep=TRUE)

# for the testing version. Do simular using 'update.packages()'.

# You can also just append the INLA-repos to the  global 'repos' variable, like

options(repos = c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"))

# and then you can do

update.packages("INLA", dep=TRUE)

# etc, to install and update the package, without specifying the repos. 
