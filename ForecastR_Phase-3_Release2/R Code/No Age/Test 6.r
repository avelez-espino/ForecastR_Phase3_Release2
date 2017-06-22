
#===============================================================================
# Set working directory
#===============================================================================

## setwd("K:\\Phase 3\\TR - No Age")

## setwd("F:/Phase 3/TR - No Age")
## homeDir <- "F:/Phase 3/TR - No Age"

## homeDir <- "F:\\Phase 3\\TR - No Age"

homeDir <- "N:\\Phase 3\\TR + No Age + meboot + stlboot"

guiDir <- homeDir

rm(list=ls())

#===============================================================================
# Specify data file name
#===============================================================================

filename <- "SPR_Terminal_Run_Without_Age_Specific_Information"


filename_original <- paste0(filename, ".csv")

#===============================================================================
# Read Data
#===============================================================================

datafile_original <- read.csv(filename_original, header=TRUE, as.is=TRUE)

## ind <- apply(datafile_original, 1, function(x) all(is.na(x)))
## datafile_original <- datafile_original[!ind, ]       ## R may read an extra final line

datafile_original$Stock_Name[datafile_original$Stock_Name==""] <- NA
datafile_original$Stock_Species[datafile_original$Stock_Species==""] <- NA
datafile_original$Stock_Abundance[datafile_original$Stock_Abundance==""] <- NA
datafile_original$Forecasting_Year[datafile_original$Forecasting_Year==""] <- NA


datafile_original <- datafile_original[rowSums(is.na(datafile_original))!=ncol(datafile_original), ]

datafile <- datafile_original  ## Add this GUI command for stocks with NO AGE information, 
                               ## otherwise the test R code will NOT work 

noagemodelnaiveone <- TRUE  ## Add this GUI command for stocks with NO AGE information,
                            ## otherwise the test R code will NOT work 

noagemodelavgthree <- TRUE  ## Add this GUI command for stocks with NO AGE information,
                            ## otherwise the test R code will NOT work 

noagemodelavgfive <- TRUE  ## Add this GUI command for stocks with NO AGE information,
                           ## otherwise the test R code will NOT work 
                           
noagemodelarima <- TRUE   ## Add this GUI command for stocks with NO AGE information,
                          ## otherwise the test R code will NOT work 
                          
noagemodelexpsmooth <- TRUE  ## Add this GUI command for stocks with NO AGE information,
                             ## otherwise the test R code will NOT work
                             
                             
retromeasureMRE <- TRUE    ## Add this GUI command for stocks with NO AGE information,
                           ## otherwise the test R code will NOT work
                
retromeasureMAE <- TRUE ## Add this GUI command for stocks with NO AGE information,
                        ## otherwise the test R code will NOT work
                  
retromeasureMPE <- TRUE ## Add this GUI command for stocks with NO AGE information,   
                        ## otherwise the test R code will NOT work
                        
retromeasureMAPE <- TRUE   ## Add this GUI command for stocks with NO AGE information, 
                           ## otherwise the test R code will NOT work
                         
retromeasureMASE <- TRUE ## Add this GUI command for stocks with NO AGE information,
                         ## otherwise the test R code will NOT work

retromeasureRMSE <- TRUE ## Add this GUI command for stocks with NO AGE information,
                         ## otherwise the test R code will NOT work

head(datafile_original)
tail(datafile_original)

#===============================================================================
# Helper Function for Installing R Packages
#===============================================================================

## function for installing and/or loading R packages
usePackage <- function(p) {
    if (!is.element(p, installed.packages()[,1]))
        install.packages(p, dep = TRUE)
        require(p, character.only = TRUE)
}

usePackage("rJava")

#===============================================================================
# Source Helper Functions for Creating Word Reports
#===============================================================================

## source("startReport.R")

## source("endReport.R")


#=============================================================================
# Source Code for Bias Coefficient Computation and Visualization 
#=============================================================================

source("Bias Coefficient Code.R")

#=============================================================================
# Source Code for Loess Bootstrapping for Time Series 
# (Imported from package ‘TStools’ version 1.90)
#=============================================================================

## usePackage("Rcpp")

source("stlboot.R")


#=============================================================================
# Test the code with various settings
#=============================================================================


boxcoxtransform <- TRUE   # use Box Cox transformation  

# bootmethod <- "meboot"  # use maximum entropy bootstrapping   

bootmethod <- "stlboot"   # use loess bootstrapping 

index <- 15 

B <- 500  

#=============================================================================
# Source Code for ARIMA Modeling 
#=============================================================================
                             
source("1. Produce Point and Interval Forecasts.R", echo=TRUE)

source("2. Evaluate Point Forecast Performance.R", echo=TRUE)

source("3. Produce Model and Forecast Diagnostics.R", echo=TRUE)

source("4. Display Results for Best Forecasting Model.R", echo=TRUE)
    
#===============================================================================
# Produce full report
#===============================================================================

source("5. Create ForecastR Report.R", echo=TRUE)


#===============================================================================
# Produce executive report
#===============================================================================

source("6. Create Executive Summary for ForecastR Report.R", echo=TRUE)
         
         
         
