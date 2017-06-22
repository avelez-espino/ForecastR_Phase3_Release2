#===============================================================================
# Clean up working space
#===============================================================================

rm(list=ls())

#===============================================================================
# Set working directory
#===============================================================================

homeDir <- "C:\\Users\\Velez-EspinoA\\Desktop\\ForecastR\\ForecastR_Phase 3\\Phase-3_Tests\\ForecastR_Phase-3_Release2"

setwd(homeDir)

#===============================================================================
# Specify data file name (without .csv extension!) 
#===============================================================================

filename <- "NTH_TR_No_Age"

#===============================================================================
# Choose forecasting models 
#
# e.g.:
#      noagemodelnaiveone <- TRUE  (use naiveone model)        OR 
#      noagemodelnaiveone <- FALSE  (do NOT use naiveone model)
#===============================================================================


noagemodelnaiveone <- TRUE  # naive model (previous year)

noagemodelavgthree <- TRUE  # naive model (average of previous 3 years)

noagemodelavgfive <- TRUE  # naive model (average of previous 5 years)
                           
noagemodelarima <- TRUE    # ARIMA model 
                          
noagemodelexpsmooth <- TRUE # exponential smoothing model  


#===============================================================================
# Choose measures of retrospective point forecast evaluation  
# 
# e.g.:    
#         retromeasureMRE <- TRUE  (use MRE as a measure)                OR        
#         retromeasureMRE <- FALSE (do NOT use MRE as a measure)
#===============================================================================

retromeasureMRE <- TRUE    
                
retromeasureMAE <- TRUE 
                  
retromeasureMPE <- TRUE 

retromeasureMAPE <- TRUE   
                         
retromeasureMASE <- TRUE 

retromeasureRMSE <- TRUE 


#=============================================================================
# Test the code with various settings
#=============================================================================

boxcoxtransform <- TRUE   # use Box Cox transformation  

# bootmethod <- "meboot"  # use maximum entropy bootstrapping   

bootmethod <- "stlboot"   # use loess bootstrapping 

index <- 15     ## how many years to use when initializing the retrospective point forecast evaluation 

B <- 5000 

#===============================================================================
# Read Data
#===============================================================================

filename_original <- paste0(filename, ".csv")

datafile_original <- read.csv(paste0("Data//",filename_original), header=TRUE, as.is=TRUE)

## ind <- apply(datafile_original, 1, function(x) all(is.na(x)))
## datafile_original <- datafile_original[!ind, ]       ## R may read an extra final line

datafile_original$Stock_Name[datafile_original$Stock_Name==""] <- NA
datafile_original$Stock_Species[datafile_original$Stock_Species==""] <- NA
datafile_original$Stock_Abundance[datafile_original$Stock_Abundance==""] <- NA
datafile_original$Forecasting_Year[datafile_original$Forecasting_Year==""] <- NA


datafile_original <- datafile_original[rowSums(is.na(datafile_original))!=ncol(datafile_original), ]

datafile <- datafile_original  ## Add this GUI command for stocks with NO AGE information, 
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


#=============================================================================
# Source Code for Bias Coefficient Computation and Visualization 
#=============================================================================

source(paste0("R Code","\\","No Age","\\","Bias Coefficient Code.R"), echo=TRUE)

#=============================================================================
# Source Code for Loess Bootstrapping for Time Series 
# (Imported from package ‘TStools’ version 1.90)
#=============================================================================

## usePackage("Rcpp")

source(paste0("R Code","\\","No Age","\\","stlboot.R"), echo=TRUE)


#=============================================================================
# Source Code for Naive and Time Series Modeling 
#=============================================================================
                             
source(paste0("R Code","\\","No Age","\\","1. Produce Point and Interval Forecasts.R"), echo=TRUE)

source(paste0("R Code","\\","No Age","\\","2. Evaluate Point Forecast Performance.R"), echo=TRUE)

source(paste0("R Code","\\","No Age","\\","3. Produce Model and Forecast Diagnostics.R"), echo=TRUE)

source(paste0("R Code","\\","No Age","\\","4. Display Results for Best Forecasting Model.R"), echo=TRUE)
    

#===============================================================================
# Source Helper Functions for Creating Word Reports
#===============================================================================

source(paste0("R Code","\\","No Age","\\","startReport.R"), echo=TRUE)

source(paste0("R Code","\\","No Age","\\","endReport.R"), echo=TRUE)

#===============================================================================
# Produce full report
#===============================================================================

docx.file <- paste0("ForecastR Reports","\\","Full_Report_", filename,"_", bootmethod,".docx")

startReport(template=paste0("Template Report","\\","Template_ReporteRs.docx"),
            plotwidth=6, plotheight=7)
            
source(paste0("R Code","\\","No Age","\\","5. Create ForecastR Report.R"), echo=TRUE)

endReport(docx.file=docx.file)
  
usePackage("stringr")
  
cat("\n\n\n")
cat("=============================================================================","\n")
cat("Full ForecastR report successfully produced and stored in the following directory:","\n")
cat(homeDir, "\n\n")
cat("Full ForecastR report has the following name:", "\n")
cat(str_replace(docx.file, "ForecastR Reports\\\\",""), "\n\n")
  
cat("Population/Stock Name: ", stockname, "\n")
cat("Population/Stock Species: ", stockspecies, "\n")
cat("Population/Stock Abundance: ", stockabundance, "\n")
cat("Forecasting Year: ", forecastingyear, "\n")

forecastingmodels <- c(ifelse(isTRUE(noagemodelnaiveone), "naiveone", "\n"), 
                       ifelse(isTRUE(noagemodelavgthree), "avgthree", "\n"),
                       ifelse(isTRUE(noagemodelavgfive), "avgfive", "\n"), 
                       ifelse(isTRUE(noagemodelarima), "arima", "\n"), 
                       ifelse(isTRUE(noagemodelexpsmooth), "expsmooth", "\n"))


cat("Forecasting Model(s): ", forecastingmodels, "\n")
cat("Bootstrap Method: ", bootmethod, "\n")
cat("=============================================================================","\n")
  
browseURL(docx.file)
     
graphics.off() 


#===============================================================================
# Produce executive report
#===============================================================================


docx.file <- paste0("ForecastR Reports","\\","Executive_Report_", filename,"_", bootmethod,".docx")

startReport(template=paste0("Template Report","\\","Template_ReporteRs.docx"),
            plotwidth=6, plotheight=7)
            
source(paste0("R Code","\\","No Age","\\", "6. Create Executive Summary for ForecastR Report.R"), echo=TRUE)

endReport(docx.file=docx.file)
  
usePackage("stringr")

cat("\n\n\n")
cat("=============================================================================","\n")
cat("Executive ForecastR report successfully produced and stored in the following directory:","\n")
cat(homeDir, "\n\n")
cat("Executive ForecastR report has the following name:", "\n")
cat(str_replace(docx.file, "ForecastR Reports\\\\",""), "\n\n")
  
cat("Population/Stock Name: ", stockname, "\n")
cat("Population/Stock Species: ", stockspecies, "\n")
cat("Population/Stock Abundance: ", stockabundance, "\n")
cat("Forecasting Year: ", forecastingyear, "\n")

forecastingmodels <- c(ifelse(isTRUE(noagemodelnaiveone), "naiveone", "\n"), 
                       ifelse(isTRUE(noagemodelavgthree), "avgthree", "\n"),
                       ifelse(isTRUE(noagemodelavgfive), "avgfive", "\n"), 
                       ifelse(isTRUE(noagemodelarima), "arima", "\n"), 
                       ifelse(isTRUE(noagemodelexpsmooth), "expsmooth", "\n"))


cat("Forecasting Model(s): ", forecastingmodels, "\n")
cat("Bootstrap Method: ", bootmethod, "\n")
cat("=============================================================================","\n")
  
browseURL(docx.file)
     
graphics.off() 


         
         
         
