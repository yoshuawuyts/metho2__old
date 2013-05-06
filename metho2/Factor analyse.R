##------------------------------------------------------------------------------------------------------------------------------------
## starting up
#
# Set up working directory
#setwd("H:/Methodologie 3/Research Proposal")
#setwd("F:/Research Proposal")
setwd("C:/Users/djai/Documents/Studie/B-these")

# Download package OpenMx
source('http://openmx.psyc.virginia.edu/getOpenMx.R')
# Load the R package OpenMx
require(OpenMx)

# Load package foreign
#library("foreign", lib.loc="Q:/LNC0HQDB/r-project-r-2-15-1-en/library")
library("foreign", lib.loc="C:/Program Files/R/R-2.15.2/library")

# Run the source file GenEpiHelperFunctions.R
# This file contains helper functions that are not included in OpenMx
# And will be used to have a detailed look at the output
# Note that the location of this file is on the internet
source("http://www.vipbg.vcu.edu/~vipbg/Tc24/GenEpiHelperFunctions.R")

##--------------------------------------------------------------------------------------------------------------------------------------
## Read in data
#
# Read in spss file
dataset <- read.spss(".sav", use.value.labels = F, to.data.frame = T)
head(dataset, 2)

##---------------------------------------------------------------------------------------------------------------------------------------
## Factor Analysis
# info: http://openmx.psyc.virginia.edu/docs/openmx/latest/FactorAnalysis_Path.html
# 
##---------------------------------------------------------------------------------------------------------------------------------------
## Assign observed/manifest variables to objects
#
names(dataset)
FactorRaw <- dataset[,c()]
PMall <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28")
PMa <- c("5","12","19","26")
PMextex <- c("1","8","15","22")
PMextint <- c("7","14","21","28")
PMextid <- c("3","10","17","24")
PMintexp <- c("4","11","18","25")
PMintacc <- c("6","13","20","27")
PMintkno <- c("2","9","16","23")
latentvars <- c("PMa","PMextex","PMextint","PMextid","PMintexp","PMintacc","PMintkno")

##--------------------------------------------------------------------------------------------------------------------------------------
##----------------------------------------------------------------------------------------------------------------------------------------
## Model 1
## 
#
PMfactormodel <-mxModel("Factor Model Path Specification",
                       type="RAM",
                       mxData(
                         observed=FactorRaw,
                         type="raw"
                       ),
                       manifestVars=PMall,
                       latentVars=latentvars,
                       # residual variances
                       mxPath(
                         from=PMall,
                         arrows=2,
                         free=TRUE,
                         values=1,
                         labels=paste("e",1:28,sep="")
                       ),
                       # latent variances
                       mxPath(
                         from=latentvars,
                         arrows=2,
                         connect="single",
                         free=FALSE,                        
                         values=1,
                         labels=paste("var",1:7,sep="")
                       ),                    
                       # latent covariances
                       mxPath(
                         from=latentvars,
                         arrows=2,
                         connect="unique.bivariate",
                         free=TRUE,
                         values=1,
                         labels=paste("cov",1:21,sep="")
                       ),
                       # factor loadings for PM variables 
                       mxPath(
                         from="PMa",
                         to=PMa,
                         arrows=1,
                         free=TRUE,
                         values=1,
                         labels=paste("l",1:4,sep="")
                       ),
                        mxPath(
                          from="PMextex",
                          to=PMextex,
                          arrows=1,
                          free=TRUE,
                          values=1,
                          labels=paste("l",5:8,sep="")
                        ),
                        mxPath(
                          from="PMextint",
                          to=PMextint,
                          arrows=1,
                          free=TRUE,
                          values=1,
                          labels=paste("l",9:12,sep="")
                        ),
                        mxPath(
                          from="PMextid",
                          to=PMextid,
                          arrows=1,
                          free=TRUE,
                          values=1,
                          labels=paste("l",13:16,sep="")
                        ),
                        mxPath(
                          from="PMintexp",
                          to=PMintexp,
                          arrows=1,
                          free=TRUE,
                          values=1,
                          labels=paste("l",17:20,sep="")
                        ),
                        mxPath(
                          from="PMintacc",
                          to=PMintacc,
                          arrows=1,
                          free=TRUE,
                          values=1,
                          labels=paste("l",21:24,sep="")
                        ),
                        mxPath(
                          from="PMintkno",
                          to=PMintkno,
                          arrows=1,
                          free=TRUE,
                          values=1,
                          labels=paste("l",25:28,sep="")
                        ),
                       #means
                       mxPath(
                         from="one",
                         to=c(PMall,latentvars),
                         arrows=1,
                         free=c(rep(TRUE, 28),rep(FALSE, 7)),
                         values=c(rep(1, 28), rep(0, 7)),
                         labels=c(paste("m",1:28,sep=""), rep(NA, 7))
                       )
)
#model fit
PMfactormodelfit <- mxRun(PMfactormodel)
summary(PMfactormodelfit)
tableFitStatistics(PMfactormodelfit)
