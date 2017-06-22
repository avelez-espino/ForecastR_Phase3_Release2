#===============================================================================
# Clean up all objects in R's current session
#===============================================================================

rm(list=ls())

options(warn=1)

#===============================================================================
# Select home directory
#===============================================================================

homeDir <- "C:\\Users\\Velez-EspinoA\\Desktop\\ForecastR\\ForecastR_Phase 3\\Phase-3_Tests\\ForecastR_Phase-3_Release2"

#===============================================================================
# Specify name of comma delimited data file (without .csv extension) 
#===============================================================================

## filename <- "SPR_Terminal_Run_Ages_2_to_6"

filename <- "WCVI_TR_upto2016"

#===============================================================================
# Select Models
#===============================================================================

n1.model <- TRUE
n3.model <- TRUE
n5.model <- TRUE
ARIMA.model <- TRUE
EXPSMOOTH.model <- TRUE
SIMPLESIBREG.model <- TRUE
SIMPLELOGPOWER.model <- TRUE


#===============================================================================
# User Input  (No need to distinguish among models - the code will do that)
#===============================================================================

set.seed(1700)

bootmethod <- "meboot"      # use maximum entropy bootstrapping for time series bootstrapping  

# bootmethod <- "stlboot"   # use loess bootstrapping for time series bootstrapping

index.year <- 15    # minimum number of years to use for retrospective forecast evaluation 

B <- 2500            # number of bootstrap samples to use when constructing forecast intervals

boxcoxtransform <- TRUE  # whether or not to use Box Cox transformation for data used to fit time series models
                         # (i.e., ARIMA, Exponential Smoothing)  ---> set to FALSE if no transformation is desired

#===============================================================================
# Initialize Model Environments
#===============================================================================

if (isTRUE(n1.model)) { n1 <- new.env() }

if (isTRUE(n3.model)) { n3 <- new.env() }

if (isTRUE(n5.model)) { n5 <- new.env() }

if (isTRUE(ARIMA.model)) { ARIMA <- new.env() }

if (isTRUE(EXPSMOOTH.model)) { EXPSMOOTH <- new.env() }

if (isTRUE(SIMPLESIBREG.model)) { SIMPLESIBREG <- new.env() }

if (isTRUE(SIMPLELOGPOWER.model)) { SIMPLELOGPOWER <- new.env() }


#===============================================================================
# Distribute User Input to Various Models
#===============================================================================


if (isTRUE(n1.model)) { 
 
    n1$index.year <- index.year 
    n1$bootmethod <- bootmethod
    n1$B <- B
    n1$boxcoxtransform <- boxcoxtransform

}


if (isTRUE(n3.model)) { 
 
    n3$index.year <- index.year 
    n3$bootmethod <- bootmethod
    n3$B <- B
    n3$boxcoxtransform <- boxcoxtransform

}


if (isTRUE(n5.model)) { 
 
    n5$index.year <- index.year 
    n5$bootmethod <- bootmethod
    n5$B <- B
    n5$boxcoxtransform <- boxcoxtransform

}


if (isTRUE(ARIMA.model)) { 
 
    ARIMA$index.year <- index.year 
    ARIMA$bootmethod <- bootmethod
    ARIMA$B <- B
    ARIMA$boxcoxtransform <- boxcoxtransform

}



if (isTRUE(EXPSMOOTH.model)) { 
 
    EXPSMOOTH$index.year <- index.year 
    EXPSMOOTH$bootmethod <- bootmethod
    EXPSMOOTH$B <- B
    EXPSMOOTH$boxcoxtransform <- boxcoxtransform

}

if (isTRUE(SIMPLESIBREG.model)) { 
 
    SIMPLESIBREG$index.year <- index.year 
    ## SIMPLESIBREG$bootmethod <- bootmethod   ## Prediction intervals for youngest age 
                                               ## were hard-coded to use stlboot 
    SIMPLESIBREG$B <- B
    SIMPLESIBREG$boxcoxtransform <- boxcoxtransform

}


if (isTRUE(SIMPLELOGPOWER.model)) { 
 
    SIMPLELOGPOWER$index.year <- index.year 
    ## SIMPLELOGPOWER$bootmethod <- bootmethod   ## Prediction intervals for youngest age 
                                                 ## were hard-coded to use stlboot 
    SIMPLELOGPOWER$B <- B
    SIMPLELOGPOWER$boxcoxtransform <- boxcoxtransform

}

#===============================================================================
# Helper Function for Installing R Packages
#===============================================================================

## function for installing and/or loading R packages
usePackage <- function(p) {
    if (!is.element(p, installed.packages()[,1]))
        install.packages(p, dep = TRUE)
        require(p, character.only = TRUE)
}



#===============================================================================
# Helper Function for Capitalizing Words
#===============================================================================

capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                  {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

## capwords(c("using AIC for model selection"))

#===============================================================================
# Helper Function for Wrapping Text
#===============================================================================

wrapper <- function(x, ...){
      paste(strwrap(x, width=50, ...), collapse = "\n")
}

#===============================================================================
# Read abundance data file from user-specified file name 
#===============================================================================

## filename <- "SPR_Terminal_Run_Ages_2_to_6"

filename_original <- paste0(filename, ".csv")    

datafile_original <- read.csv(paste0("Data","\\",filename_original), header=TRUE, as.is=TRUE)

## ind <- apply(datafile_original, 1, function(x) all(is.na(x)))
## datafile_original <- datafile_original[!ind, ]       ## R may read an extra final line

datafile_original$Stock_Name[datafile_original$Stock_Name==""] <- NA
datafile_original$Stock_Species[datafile_original$Stock_Species==""] <- NA
datafile_original$Stock_Abundance[datafile_original$Stock_Abundance==""] <- NA
datafile_original$Forecasting_Year[datafile_original$Forecasting_Year==""] <- NA


datafile_original <- datafile_original[rowSums(is.na(datafile_original))!=ncol(datafile_original), ]

head(datafile_original)
tail(datafile_original)

#===============================================================================
# Source Code for Selected Models
#===============================================================================


if (isTRUE(n1.model)) { 

    source(paste0(homeDir, "\\", "R Code", "\\", "n1", "\\", "Bias Coefficient Code - Naive One", ".r"))
    
    source(paste0(homeDir, "\\", "R Code", "\\", "n1", "\\","stlboot",".r"))
    
    source(paste0(homeDir, "\\", "R Code", "\\", "n1", "\\","Review Code - Naive One",".r"))
    
}

if (isTRUE(n3.model)) {  

     source(paste0(homeDir, "\\", "R Code", "\\", "n3", "\\", "Bias Coefficient Code - Average Three", ".r"))
     
     source(paste0(homeDir, "\\", "R Code", "\\", "n3", "\\","stlboot",".r"))
     
     source(paste0(homeDir, "\\", "R Code", "\\", "n3", "\\","Review Code - Average Three",".r"))
}

if (isTRUE(n5.model)) {  

    source(paste0(homeDir, "\\", "R Code", "\\", "n5", "\\", "Bias Coefficient Code - Average Five", ".r"))

    source(paste0(homeDir, "\\", "R Code", "\\", "n5", "\\","stlboot",".r"))
    
    source(paste0(homeDir, "\\", "R Code", "\\", "n5", "\\","Review Code - Average Five",".r"))
}

if (isTRUE(ARIMA.model)) {

    source(paste0(homeDir, "\\", "R Code", "\\", "ARIMA", "\\", "Bias Coefficient Code - ARIMA", ".r"))
    
    source(paste0(homeDir, "\\", "R Code", "\\", "ARIMA", "\\","stlboot",".r"))
    
    #---
    
    source(paste0(homeDir, "\\", "R Code", "\\", "ARIMA", "\\","Review Code - ARIMA",".r"))
        
}


if (isTRUE(EXPSMOOTH.model)) { 

    source(paste0(homeDir, "\\", "R Code", "\\", "EXPSMOOTH", "\\", "Bias Coefficient Code - Exponential Smoothing", ".r"))
    
    source(paste0(homeDir, "\\", "R Code", "\\", "EXPSMOOTH", "\\","stlboot",".r"))
    
    #---
    
    source(paste0(homeDir, "\\", "R Code", "\\", "EXPSMOOTH", "\\","Review Code - Exponential Smoothing",".r"))

}

if (isTRUE(SIMPLESIBREG.model)) { 

    cat("==================================================================", "\n\n")

    cat("Source file: ", "Bias Coefficient Code - Simple Sibling Regression", ".r", "\n\n")
    source(paste0(homeDir, "\\", "R Code", "\\", "SIMPLESIBREG", "\\", "Bias Coefficient Code - Simple Sibling Regression", ".r"))
    
    cat("==================================================================", "\n\n")
    
    cat("Source file: ", "stlboot", ".r", "\n\n")
    source(paste0(homeDir, "\\", "R Code", "\\", "SIMPLESIBREG", "\\","stlboot",".r"))
    
    cat("==================================================================", "\n\n")
    
    cat("Source file: ", "Review Code - Simple Sibling Regression", ".r", "\n\n")
    source(paste0(homeDir, "\\", "R Code", "\\", "SIMPLESIBREG", "\\","Review Code - Simple Sibling Regression",".r"))

    cat("==================================================================", "\n\n")

    cat("Source file: ", "Youngest Age - Simple Sibling Regression", ".r", "\n\n")
    source(paste0(homeDir, "\\", "R Code", "\\", "SIMPLESIBREG", "\\","Youngest Age - Simple Sibling Regression",".r"))

    cat("==================================================================", "\n\n")

    cat("Source file: ", "RETRO - Simple Sibling Regression", ".r", "\n\n")
    source(paste0(homeDir, "\\", "R Code", "\\", "SIMPLESIBREG", "\\","RETRO - Simple Sibling Regression",".r"))
    
    cat("==================================================================", "\n\n")
    
    cat("Source file: ", "Effects - Simple Sibling Regression", ".r", "\n\n")
    source(paste0(homeDir, "\\", "R Code", "\\", "SIMPLESIBREG", "\\","Effects - Simple Sibling Regression",".r"))
    
    cat("==================================================================", "\n\n")
    
    cat("Source file: ", "Misc - Simple Sibling Regressio", ".r", "\n\n")
    source(paste0(homeDir, "\\", "R Code", "\\", "SIMPLESIBREG", "\\","Misc - Simple Sibling Regression",".r"))
    
    cat("==================================================================", "\n\n")
    
    cat("Source file: ", "Bias Coefficient Computation - Simple Sibling Regression", ".r", "\n\n")
    source(paste0(homeDir, "\\", "R Code", "\\", "SIMPLESIBREG", "\\", "Bias Coefficient Computation - Simple Sibling Regression", ".r"))

    cat("==================================================================", "\n\n")
    
}

if (isTRUE(SIMPLELOGPOWER.model)) { 

    cat("==================================================================", "\n\n")

    cat("Source file: ", "Bias Coefficient Code - Simple Log Power Regression", ".r", "\n\n")
    source(paste0(homeDir, "\\", "R Code", "\\", "SIMPLELOGPOWER", "\\", "Bias Coefficient Code - Simple Log Power Regression", ".r"))
    
    cat("==================================================================", "\n\n")
 
    cat("Source file: ", "stlboot", ".r", "\n\n")
    source(paste0(homeDir, "\\", "R Code", "\\", "SIMPLELOGPOWER", "\\","stlboot",".r"))
    
    cat("==================================================================", "\n\n")
    
    cat("Source file: ", "Review Code - Simple Log Power Regression", ".r", "\n\n")  
    source(paste0(homeDir, "\\", "R Code", "\\", "SIMPLELOGPOWER", "\\","Review Code - Simple Log Power Regression",".r"))

    cat("==================================================================", "\n\n")
    
    cat("Source file: ", "Youngest Age - Simple Log Power Regression", ".r", "\n\n")  
    source(paste0(homeDir, "\\", "R Code", "\\", "SIMPLELOGPOWER", "\\","Youngest Age - Simple Log Power Regression",".r"))

    cat("==================================================================", "\n\n")

    cat("Source file: ", "RETRO - Simple Log Power Regression", ".r", "\n\n")
    source(paste0(homeDir, "\\", "R Code", "\\", "SIMPLELOGPOWER", "\\","RETRO - Simple Log Power Regression",".r"))
    
    cat("==================================================================", "\n\n")
      
    cat("Source file: ", "Effects - Simple Log Power Regression", ".r", "\n\n")
    source(paste0(homeDir, "\\", "R Code", "\\", "SIMPLELOGPOWER", "\\","Effects - Simple Log Power Regression",".r"))
    
    cat("==================================================================", "\n\n")
      
    cat("Source file: ", "Misc - Simple Log Power Regression", ".r", "\n\n")
    source(paste0(homeDir, "\\", "R Code", "\\", "SIMPLELOGPOWER", "\\","Misc - Simple Log Power Regression",".r"))
    
    cat("==================================================================", "\n\n")
      
    cat("Source file: ", "Bias Coefficient Computation - Simple Log Power Regression", ".r", "\n\n")  
    source(paste0(homeDir, "\\", "R Code", "\\", "SIMPLELOGPOWER", "\\", "Bias Coefficient Computation - Simple Log Power Regression", ".r"))
    
    cat("==================================================================", "\n\n")  


}

#===============================================================================
# Source Helper Functions for Creating Word Reports
#===============================================================================

usePackage("ReporteRs")

source("startReport.R")

source("endReport.R")

#===============================================================================
# Produce individual reports:  Naive Model (Previous Year)
#===============================================================================

if (isTRUE(n1.model)) {

  if (n1$bootmethod=="meboot") {

     docx.file <- paste0("ForecastR Reports","\\","Report_Naive_Modeling_Previous_Year", "_", filename,"_",n1$bootmethod,".docx")

     startReport(template=paste0("Template Report","\\","Template_ReporteRs.docx"),
            plotwidth=6, plotheight=7)
            

     source(paste0("R Code","\\","n1","\\","Report - Naive One.R"),echo=TRUE)

     endReport(docx.file=docx.file)
  
     usePackage("stringr")
  
     cat("\n\n\n")
     cat("=============================================================================","\n")
     cat("ForecastR report successfully produced and stored in the following directory:","\n")
     cat(homeDir, "\n\n")
     cat("ForecastR report has the following name:", "\n")
     cat(str_replace(docx.file, "ForecastR Reports\\\\",""), "\n\n")
  
     cat("Population/Stock Name", n1$stockname, "\n")
     cat("Population/Stock Species", n1$stockspecies, "\n")
     cat("Population/Stock Abundance", n1$stockabundance, "\n")
     cat("Forecasting Year", n1$forecastingyear, "\n")
     cat("Forecasting Model:", "Naive Model (Previous Year)", "\n")
     cat("Bootstrap Method:", n1$bootmethod, "\n")
     cat("=============================================================================","\n")
  
     browseURL(docx.file)
     
     graphics.off() 
  
  } # end  if (n1$bootmethod=="meboot")

  if (n1$bootmethod=="stlboot") {

     docx.file <- paste0("ForecastR Reports","\\", "Report_Naive_Modeling_Previous_Year", "_", filename, "_",bootmethod,".docx")
     startReport(template=paste0("Template Report","\\","Template_ReporteRs.docx"),
            plotwidth=6, plotheight=7)
            
     ## source("Report - Naive One.R", echo=TRUE)
     
     source(paste0("R Code","\\","n1","\\","Report - Naive One.R"),echo=TRUE)
     
     endReport(docx.file=docx.file)
     
     usePackage("stringr")

     cat("\n\n\n")
     cat("=============================================================================","\n")
     cat("ForecastR report successfully produced and stored in the following directory:","\n")
     cat(homeDir, "\n\n")
     cat("ForecastR report has the following name:", "\n")
     cat(str_replace(docx.file, "ForecastR Reports\\\\",""), "\n\n")
     cat("=============================================================================","\n")
  
     cat("Population/Stock Name", n1$stockname, "\n")
     cat("Population/Stock Species", n1$stockspecies, "\n")
     cat("Population/Stock Abundance", n1$stockabundance, "\n")
     cat("Forecasting Year", n1$forecastingyear, "\n")
     cat("Forecasting Model:", "Naive Model (Previous Year)", "\n")
     cat("Bootstrap Method:", n1$bootmethod, "\n")
     cat("=============================================================================","\n")
  
     browseURL(docx.file)
  
     graphics.off() 
  }

} 


#===============================================================================
# Produce individual reports:  Naive Model (Average of Previous Three Years)
#===============================================================================

if (isTRUE(n3.model)) {

  if (n3$bootmethod=="meboot") {
  
     docx.file <- paste0("ForecastR Reports","\\","Report_Naive_Modeling_Average_of_Previous_Three_Years", "_", filename,"_",n3$bootmethod,".docx")

     startReport(template=paste0("Template Report","\\","Template_ReporteRs.docx"),
            plotwidth=6, plotheight=7)
            
     source(paste0("R Code","\\","n3","\\","Report - Average Three.R"),echo=TRUE)

     endReport(docx.file=docx.file)
  
     usePackage("stringr")
  
     cat("\n\n\n")
     cat("=============================================================================","\n")
     cat("ForecastR report successfully produced and stored in the following directory:","\n")
     cat(homeDir, "\n\n")
     cat("ForecastR report has the following name:", "\n")
     cat(str_replace(docx.file, "ForecastR Reports\\\\",""), "\n\n")
  
     cat("Population/Stock Name", n3$stockname, "\n")
     cat("Population/Stock Species", n3$stockspecies, "\n")
     cat("Population/Stock Abundance", n3$stockabundance, "\n")
     cat("Forecasting Year", n3$forecastingyear, "\n")
     cat("Forecasting Model:", "Naive Model (Average of Previous Three Years)", "\n")
     cat("Bootstrap Method:", n3$bootmethod, "\n")
     cat("=============================================================================","\n")
  
     browseURL(docx.file)
     
     graphics.off() 

  } 
  
  if (n3$bootmethod=="stlboot") {
  
      docx.file <- paste0("ForecastR Reports","\\","Report_Naive_Modeling_Average_of_Previous_Three_Years", "_", filename, "_",n3$bootmethod,".docx")
  
      startReport(template=paste0("Template Report","\\","Template_ReporteRs.docx"),
            plotwidth=6, plotheight=7)
  
      source(paste0("R Code","\\","n3","\\","Report - Average Three.R"), echo=TRUE)
  
      endReport(docx.file=docx.file)
      
      usePackage("stringr")

      cat("\n\n\n")
      cat("=============================================================================","\n")
      cat("ForecastR report successfully produced and stored in the following directory:","\n")
      cat(homeDir, "\n\n")
      cat("ForecastR report has the following name:", "\n")
      cat(str_replace(docx.file, "ForecastR Reports\\\\",""), "\n\n")
      cat("=============================================================================","\n")
  
      cat("Population/Stock Name", n3$stockname, "\n")
      cat("Population/Stock Species", n3$stockspecies, "\n")
      cat("Population/Stock Abundance", n3$stockabundance, "\n")
      cat("Forecasting Year", n3$forecastingyear, "\n")
      cat("Forecasting Model:", "Naive Model (Average of Previous Three Years)", "\n")
      cat("Bootstrap Method:", n3$bootmethod, "\n")
      cat("=============================================================================","\n")
  
      browseURL(docx.file)
      
      graphics.off() 
  }
  
} 


                                     
#===============================================================================
# Produce individual reports:  Naive Model (Average of Previous Five Years)
#===============================================================================

if (isTRUE(n5.model)) {

  if (n5$bootmethod=="meboot") {

     docx.file <- paste0("ForecastR Reports","\\","Report_Naive_Modeling_Average_of_Previous_FIve_Years", "_", filename,"_",n5$bootmethod,".docx")

     startReport(template=paste0("Template Report","\\","Template_ReporteRs.docx"),
            plotwidth=6, plotheight=7)
            

     source(paste0("R Code","\\","n5","\\","Report - Average Five.R"),echo=TRUE)

     endReport(docx.file=docx.file)
  
     usePackage("stringr")
  
     cat("\n\n\n")
     cat("=============================================================================","\n")
     cat("ForecastR report successfully produced and stored in the following directory:","\n")
     cat(homeDir, "\n\n")
     cat("ForecastR report has the following name:", "\n")
     cat(str_replace(docx.file, "ForecastR Reports\\\\",""), "\n\n")
  
     cat("Population/Stock Name", n5$stockname, "\n")
     cat("Population/Stock Species", n5$stockspecies, "\n")
     cat("Population/Stock Abundance", n5$stockabundance, "\n")
     cat("Forecasting Year", n5$forecastingyear, "\n")
     cat("Forecasting Model:", "Naive Model (Average of Previous Five Years)", "\n")
     cat("Bootstrap Method:", n5$bootmethod, "\n")
     cat("=============================================================================","\n")
  
     browseURL(docx.file)
     
     graphics.off() 
  
  }
  
  if (n5$bootmethod=="stlboot") {
  
     docx.file <- paste0("ForecastR Reports","\\","Report_Naive_Modeling_Average_of_Previous_Five_Years", "_", filename, "_",n5$bootmethod,".docx")
  
     startReport(template=paste0("Template Report","\\","Template_ReporteRs.docx"),
            plotwidth=6, plotheight=7)
  
     source(paste0("R Code","\\","n5","\\","Report - Average Five.R"), echo=TRUE)
  
     endReport(docx.file=docx.file)

     usePackage("stringr")

     cat("\n\n\n")
     cat("=============================================================================","\n")
     cat("ForecastR report successfully produced and stored in the following directory:","\n")
     cat(homeDir, "\n\n")
     cat("ForecastR report has the following name:", "\n")
     cat(str_replace(docx.file, "ForecastR Reports\\\\",""), "\n\n")
     cat("=============================================================================","\n")
  
     cat("Population/Stock Name", n5$stockname, "\n")
     cat("Population/Stock Species", n5$stockspecies, "\n")
     cat("Population/Stock Abundance", n5$stockabundance, "\n")
     cat("Forecasting Year", n5$forecastingyear, "\n")
     cat("Forecasting Model:", "Naive Model (Average of Previous Five Years)", "\n")
     cat("Bootstrap Method:", n5$bootmethod, "\n")
     cat("=============================================================================","\n")
  
     browseURL(docx.file)
     
     graphics.off() 
  
  }

} 


#===============================================================================
# Produce individual reports:  ARIMA Model
#===============================================================================

## ARIMA$forecast.arima.modified.meboot

if (isTRUE(ARIMA.model)) {

  if (ARIMA$bootmethod=="meboot") {

     docx.file <- paste0("ForecastR Reports","\\","Report_ARIMA_Modeling", "_", filename,"_",ARIMA$bootmethod,".docx")

     startReport(template=paste0("Template Report","\\","Template_ReporteRs.docx"),
            plotwidth=6, plotheight=7)
            
     source(paste0("R Code","\\","ARIMA","\\","Report - ARIMA - meboot.R"), echo=TRUE)

     endReport(docx.file=docx.file)
  
     usePackage("stringr")
  
     cat("\n\n\n")
     cat("=============================================================================","\n")
     cat("ForecastR report successfully produced and stored in the following directory:","\n")
     cat(homeDir, "\n\n")
     cat("ForecastR report has the following name:", "\n")
     cat(str_replace(docx.file, "ForecastR Reports\\\\",""), "\n\n")
  
     cat("Population/Stock Name", ARIMA$stockname, "\n")
     cat("Population/Stock Species", ARIMA$stockspecies, "\n")
     cat("Population/Stock Abundance", ARIMA$stockabundance, "\n")
     cat("Forecasting Year", ARIMA$forecastingyear, "\n")
     cat("Forecasting Model:", "ARIMA Model", "\n")
     cat("Bootstrap Method:", ARIMA$bootmethod, "\n")
     cat("=============================================================================","\n")
  
     browseURL(docx.file)
     
     graphics.off() 

  }
  
  if (ARIMA$bootmethod=="stlboot") {
  
     docx.file <- paste0("ForecastR Reports","\\","Report_ARIMA_Modeling", "_", filename,"_",ARIMA$bootmethod,".docx")

     startReport(template=paste0("Template Report","\\","Template_ReporteRs.docx"),
            plotwidth=6, plotheight=7)
            
     source(paste0("R Code","\\","ARIMA","\\","Report - ARIMA - stlboot.R"), echo=TRUE)

     endReport(docx.file=docx.file)
  
     usePackage("stringr")
  
     cat("\n\n\n")
     cat("=============================================================================","\n")
     cat("ForecastR report successfully produced and stored in the following directory:","\n")
     cat(homeDir, "\n\n")
     cat("ForecastR report has the following name:", "\n")
     cat(str_replace(docx.file, "ForecastR Reports\\\\",""), "\n\n")
  
     cat("Population/Stock Name", ARIMA$stockname, "\n")
     cat("Population/Stock Species", ARIMA$stockspecies, "\n")
     cat("Population/Stock Abundance", ARIMA$stockabundance, "\n")
     cat("Forecasting Year", ARIMA$forecastingyear, "\n")
     cat("Forecasting Model:", "ARIMA Model", "\n")
     cat("Bootstrap Method:", ARIMA$bootmethod, "\n")
     cat("=============================================================================","\n")
  
     browseURL(docx.file)
     
     graphics.off() 
  
  }

}


#===============================================================================
# Produce individual reports:  Exponential Smoothing Model
#===============================================================================

## EXPSMOOTH$forecast.expsmooth.modified.meboot

if (isTRUE(EXPSMOOTH.model)) {

  if (EXPSMOOTH$bootmethod=="meboot") {

     docx.file <- paste0("ForecastR Reports","\\","Report_EXPSMOOTH_Modeling", "_", filename,"_",EXPSMOOTH$bootmethod,".docx")

     startReport(template=paste0("Template Report","\\","Template_ReporteRs.docx"),
            plotwidth=6, plotheight=7)
            
     source(paste0("R Code","\\","EXPSMOOTH","\\","Report - Exponential Smoothing - meboot.R"), echo=TRUE)

     endReport(docx.file=docx.file)
  
     usePackage("stringr")
  
     cat("\n\n\n")
     cat("=============================================================================","\n")
     cat("ForecastR report successfully produced and stored in the following directory:","\n")
     cat(homeDir, "\n\n")
     cat("ForecastR report has the following name:", "\n")
     cat(str_replace(docx.file, "ForecastR Reports\\\\",""), "\n\n")
  
     cat("Population/Stock Name", EXPSMOOTH$stockname, "\n")
     cat("Population/Stock Species", EXPSMOOTH$stockspecies, "\n")
     cat("Population/Stock Abundance", EXPSMOOTH$stockabundance, "\n")
     cat("Forecasting Year", EXPSMOOTH$forecastingyear, "\n")
     cat("Forecasting Model:", "Exponential Smoothing Model", "\n")
     cat("Bootstrap Method:", EXPSMOOTH$bootmethod, "\n")
     cat("=============================================================================","\n")
  
     browseURL(docx.file)
     
     graphics.off() 

  }
  
  if (EXPSMOOTH$bootmethod=="stlboot") {
  
     docx.file <- paste0("ForecastR Reports","\\","Report_EXPSMOOTH_Modeling", "_", filename,"_",EXPSMOOTH$bootmethod,".docx")

     startReport(template=paste0("Template Report","\\","Template_ReporteRs.docx"),
            plotwidth=6, plotheight=7)
            
     source(paste0("R Code","\\","EXPSMOOTH","\\","Report - Exponential Smoothing - stlboot.R"), echo=TRUE)

     endReport(docx.file=docx.file)
  
     usePackage("stringr")
  
     cat("\n\n\n")
     cat("=============================================================================","\n")
     cat("ForecastR report successfully produced and stored in the following directory:","\n")
     cat(homeDir, "\n\n")
     cat("ForecastR report has the following name:", "\n")
     cat(str_replace(docx.file, "ForecastR Reports\\\\",""), "\n\n")
  
     cat("Population/Stock Name", EXPSMOOTH$stockname, "\n")
     cat("Population/Stock Species", EXPSMOOTH$stockspecies, "\n")
     cat("Population/Stock Abundance", EXPSMOOTH$stockabundance, "\n")
     cat("Forecasting Year", EXPSMOOTH$forecastingyear, "\n")
     cat("Forecasting Model:", "Exponential Smoothing Model", "\n")
     cat("Bootstrap Method:", EXPSMOOTH$bootmethod, "\n")
     cat("=============================================================================","\n")
  
     browseURL(docx.file)
     
     graphics.off() 

  }

}

              
#===============================================================================
# Produce individual reports:  Simple Sibling Regression  
#===============================================================================

if (isTRUE(SIMPLESIBREG.model)) {

     docx.file <- paste0("ForecastR Reports","\\","Report_Simple_Sibling_Regression_Modeling", "_", filename,".docx")

     startReport(template=paste0("Template Report","\\","Template_ReporteRs.docx"),
            plotwidth=6, plotheight=7)
            
     source(paste0("R Code","\\","SIMPLESIBREG","\\","Report - Simple Sibling Regression.R"), echo=TRUE)

     endReport(docx.file=docx.file)
  
     usePackage("stringr")
  
     cat("\n\n\n")
     cat("=============================================================================","\n")
     cat("ForecastR report successfully produced and stored in the following directory:","\n")
     cat(homeDir, "\n\n")
     cat("ForecastR report has the following name:", "\n")
     cat(str_replace(docx.file, "ForecastR Reports\\\\",""), "\n\n")
  
     cat("Population/Stock Name", SIMPLESIBREG$stockname, "\n")
     cat("Population/Stock Species", SIMPLESIBREG$stockspecies, "\n")
     cat("Population/Stock Abundance", SIMPLESIBREG$stockabundance, "\n")
     cat("Forecasting Year", SIMPLESIBREG$forecastingyear, "\n")
     cat("Forecasting Model:", "Simple Sibling Regression Model", "\n")
     cat("Bootstrap Method:", SIMPLESIBREG$bootmethod, "\n")
     cat("=============================================================================","\n")
  
     browseURL(docx.file)
     
     graphics.off() 

}


#===============================================================================
# Produce individual reports:  Simple Log Power Regression  
#===============================================================================

if (isTRUE(SIMPLELOGPOWER.model)) {

     docx.file <- paste0("ForecastR Reports","\\","Report_Simple_Log_Power_Regression_Modeling", "_", filename,".docx")

     startReport(template=paste0("Template Report","\\","Template_ReporteRs.docx"),
            plotwidth=6, plotheight=7)
            
     source(paste0("R Code","\\","SIMPLELOGPOWER","\\","Report - Simple Log Power Regression.R"), echo=TRUE)

     endReport(docx.file=docx.file)
  
     usePackage("stringr")
  
     cat("\n\n\n")
     cat("=============================================================================","\n")
     cat("ForecastR report successfully produced and stored in the following directory:","\n")
     cat(homeDir, "\n\n")
     cat("ForecastR report has the following name:", "\n")
     cat(str_replace(docx.file, "ForecastR Reports\\\\",""), "\n\n")
  
     cat("Population/Stock Name", SIMPLELOGPOWER$stockname, "\n")
     cat("Population/Stock Species", SIMPLELOGPOWER$stockspecies, "\n")
     cat("Population/Stock Abundance", SIMPLELOGPOWER$stockabundance, "\n")
     cat("Forecasting Year", SIMPLELOGPOWER$forecastingyear, "\n")
     cat("Forecasting Model:", "Simple Log Power Regression Model", "\n")
     cat("Bootstrap Method:", SIMPLELOGPOWER$bootmethod, "\n")
     cat("=============================================================================","\n")
  
     browseURL(docx.file)
     
     graphics.off() 

}







