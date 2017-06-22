usePackage("ReporteRs")

docx.file <- paste0("ForecastR Reports","\\","Combined_Models_Report",".docx")

startReport(template=paste0("Template Report","\\","Template_ReporteRs.docx"),
            plotwidth=6, plotheight=7)

#========================================================================================================
# pred.args 
#========================================================================================================


pred.args <- NULL

if (exists("n1", mode="environment")) {

   pred.args.tmp <- mget( ls( pattern = "^pred.int.individual.age", env = n1) , env = n1)

   pred.args <- c(pred.args, pred.args.tmp)

   rm(pred.args.tmp)

}


if (exists("n3", mode="environment")) {

   pred.args.tmp <- mget( ls( pattern = "^pred.int.individual.age", env = n3) , env = n3)

   pred.args <- c(pred.args, pred.args.tmp)

}

if (exists("n5", mode="environment")) {

   pred.args.tmp <- mget( ls( pattern = "^pred.int.individual.age", env = n5) , env = n5)

   pred.args <- c(pred.args, pred.args.tmp)

}

if (exists("ARIMA", mode="environment")) {

   pred.args.tmp <- mget( ls( pattern = "^pred.int.individual.age", env = ARIMA) , env = ARIMA)

   pred.args <- c(pred.args, pred.args.tmp)

}


if (exists("EXPSMOOTH", mode="environment")) {

   pred.args.tmp <- mget( ls( pattern = "^pred.int.individual.age", env = EXPSMOOTH) , env = EXPSMOOTH)

   pred.args <- c(pred.args, pred.args.tmp)

}


if (exists("SIMPLESIBREG", mode="environment")) {

   pred.args.tmp <- mget( ls( pattern = "^pred.int.individual.ages.simple.sibling.regression$", env = SIMPLESIBREG) , env = SIMPLESIBREG)

   pred.args <- c(pred.args, pred.args.tmp)

}


if (exists("SIMPLELOGPOWER", mode="environment")) {

   pred.args.tmp <- mget( ls( pattern = "^pred.int.individual.ages.simplelogpower.regression$", env = SIMPLELOGPOWER) , env = SIMPLELOGPOWER)

   pred.args <- c(pred.args, pred.args.tmp)

}



## all.pred.args <- c("pred.int.individual.stock.naiveone.no.age",
##                     "pred.int.individual.stock.avgthree.no.age",
##                       "pred.int.individual.stock.avgfive.no.age",
##                         "pred.int.individual.stock.arima.no.age",
##                           "pred.int.individual.stock.expsmooth.no.age")

all.pred.args <- names(pred.args)

all.pred.args

pred.args <-  pred.args[all.pred.args]

## Source: https://stat.ethz.ch/pipermail/r-help/2006-August/111896.html
delete.NULLs  <-  function(x.list){   # delele null/empty entries in a list
    x.list[unlist(lapply(x.list, length) != 0)]
}

pred.args <- delete.NULLs(pred.args)

pred.args

names(pred.args)

#========================================================================================================
# Model Ranking
#========================================================================================================


rank.rmse.results.individual.age  <- function(table.rmse.results.individual.age, pred.args, all.pred.args, j){  # j = cycles through ages
    
     ## need to work with absolute values of MRE and MPE (should these be selected by users)
     
     table.rmse.results.individual.age.tmp <- as.data.frame(table.rmse.results.individual.age[[j]])
     table.rmse.results.individual.age.tmp <- apply(table.rmse.results.individual.age.tmp, 2, abs)

        
     pred.int.widths <- NULL 
     pred.int.models <- NULL 
     pred.int.lower.limit <- NULL 
     pred.int.upper.limit <- NULL 
     pred.int.center <- NULL 
     for (i in 1:length(pred.args)){   # cycle through each forecasting model using the index i; 
                                       # cycle through each individual age using the index j

          model.info.tmp <- pred.args[all.pred.args[i]] # information for i-th model 
                
          age.info.tmp <- model.info.tmp[[1]][[j]]                                         # information for j-th age 
  
          pred.int.lower.limit.tmp <- round(age.info.tmp$PI.lwr)
          pred.int.upper.limit.tmp <- round(age.info.tmp$PI.upr)
          pred.int.width.tmp <- pred.int.upper.limit.tmp - pred.int.lower.limit.tmp 
          pred.int.widths <- c(pred.int.widths, pred.int.width.tmp)

          class.tmp <- all.pred.args[i] 

          usePackage("stringr")
          class.tmp <- str_replace_all(class.tmp, "pred.int.individual.ages.", "")
          class.tmp

          pred.int.models <- c(pred.int.models, class.tmp)
          pred.int.lower.limit <- c(pred.int.lower.limit, pred.int.lower.limit.tmp)
          pred.int.upper.limit <- c(pred.int.upper.limit, pred.int.upper.limit.tmp)
          
          pred.int.center.tmp <- round(age.info.tmp$PI.ctr)
          pred.int.center <- c(pred.int.center, pred.int.center.tmp)
     
     }

     pred.int <- data.frame(model=pred.int.models, center=pred.int.center,
                            lower=pred.int.lower.limit, upper=pred.int.upper.limit, 
                            width=pred.int.widths)

     if (ncol(table.rmse.results.individual.age.tmp) > 1) {   # more than one retro measure 

         table.rmse.results.individual.age.tmp[is.infinite(table.rmse.results.individual.age.tmp)] <- NA
         table.rmse.results.individual.age.tmp[is.nan(table.rmse.results.individual.age.tmp)] <- NA
      
         ranks <- apply(table.rmse.results.individual.age.tmp,2,rank, na.last="keep")

         avg.ranks <- round(apply(ranks,1,mean, na.rm=TRUE),2) 
         pred.int.and.avg.ranks <- cbind.data.frame(pred.int, avg.ranks) 
         ## index.min.avg.rank <- as.numeric(which(avg.ranks == min(avg.ranks))[1])
         index.min.avg.rank <- as.numeric(which(avg.ranks == min(avg.ranks)))
         index.min.avg.rank <- index.min.avg.rank[which(pred.int.widths[avg.ranks == min(avg.ranks)]==min(pred.int.widths[avg.ranks == min(avg.ranks)]))]  
         model.min.avg.rank <- rownames(table.rmse.results.individual.age.tmp)[index.min.avg.rank]
     }

     ## when there is only one forecasting model, ranking is not necessary

     if (ncol(table.rmse.results.individual.age.tmp)>1) {
        out <- list()
        out$age <- names(table.rmse.results.individual.age)[j]
        ## out$ranking.method <- ranking.method
        out$ranking.measures <- colnames(table.rmse.results.individual.age.tmp)
        out$measures <- table.rmse.results.individual.age[[j]]  ## should you have absolute values here for MRE and MPE? 
        out$absolute.measures <- apply(table.rmse.results.individual.age[[j]],2,abs) 
        out$ranks <- ranks
        out$avg.ranks <- avg.ranks
        out$pred.int.and.avg.ranks <- pred.int.and.avg.ranks
        out$index.min.avg.rank <- index.min.avg.rank
        out$model.min.avg.rank <- index.min.avg.rank
        out$all.models <- rownames(table.rmse.results.individual.age.tmp)
        out$best.model <- rownames(table.rmse.results.individual.age.tmp)[index.min.avg.rank] 
     }
     
     
     if(ncol(table.rmse.results.individual.age.tmp)==1) {
        
        model.tmp <- names(pred.args)
        usePackage("stringr")
        model.tmp <- str_replace_all(model.tmp,"pred.int.individual.ages.","")
        ## model.tmp <- str_replace_all(model.tmp,".individual.age","")
     
     
        out <- list()
        out$age <- names(table.rmse.results.individual.age)[j]
        ## out$ranking.method <- ranking.method
        out$ranking.measures <- rownames(table.rmse.results.individual.age.tmp)
        out$measures <- table.rmse.results.individual.age[[j]]  ## should you have absolute values here for MRE and MPE? 
        out$absolute.measures <- abs(table.rmse.results.individual.age[[j]])  ## will this work for a vector? 
        out$ranks <- 1
        out$avg.ranks <- 1
        tmpdat <- cbind.data.frame(pred.int, 1)
        colnames(tmpdat)[colnames(tmpdat)=="1"] <- "Rank"  
        ## out$pred.int.and.avg.ranks <-    ## What should be listed here???
        out$index.min.avg.rank <- 1
        out$model.min.avg.rank <- 1
        out$all.models <- model.tmp
        out$best.model <- model.tmp 
     }
     

     return(out) 
     
}



## j = 1  # Age 2 
## j = 2  # Age 3 
j = 3  # Age 4 

# j = 4  # Age 5 

## Why did I comment out the two lines of code below?!

table.rank.rmse.results.individual.age <- rank.rmse.results.individual.age(table.rmse.results.individual.age, pred.args, all.pred.args, j)
table.rank.rmse.results.individual.age
 
#========================================================================================================

Age <- NULL

if (exists("n1", mode="environment")) {
    Age <- c(Age, names(n1$MIA)[-c(1,2)])
} else if (exists("n3", mode="environment")) {
    Age <- c(Age, names(n3$MIA)[-c(1,2)])
} else if (exists("n5", mode="environment")) {
    Age <- c(Age, names(n5$MIA)[-c(1,2)])
} else if (exists("ARIMA", mode="environment")) {
    Age <- c(Age, names(ARIMA$MIA)[-c(1,2)])
} else if (exists("EXPSMOOTH", mode="environment")) {
    Age <- c(Age, names(EXPSMOOTH$MIA)[-c(1,2)])
} else if (exists("SIMPLESIBREG", mode="environment")) {
   usePackage("operator.tools")
   Age <- c(Age, names(SIMPLESIBREG$M) %!in% c("Measure","Total"))
} else if (exists("SIMPLESIBREG", mode="environment")) {
   usePackage("operator.tools")
   Age <- c(Age, names(SIMPLELOGPOWER$M) %!in% c("Measure","Total"))
}

Age <- unique(Age)


#========================================================================================================
# CoverPage
#========================================================================================================


## doc = addParagraph(doc, "ForecastR Output", stylename = "Normal" )

pot1 = pot("ForecastR Output Report", textProperties(font.weight="bold", font.size = 20) )
my.pars = set_of_paragraphs(pot1)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )

pot1 = pot(" ", textProperties(font.weight="bold", font.size = 10) )
my.pars = set_of_paragraphs(pot1)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )

pot2 =  pot("Stock Name: ", textProperties(font.weight="bold", font.size = 10) ) + 
        pot(paste(stockname), textProperties(font.size = 10) )
my.pars = set_of_paragraphs(pot2)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )

pot3 =  pot("Stock Species: ", textProperties(font.weight="bold", font.size = 10) ) + 
        pot(paste(stockspecies), textProperties(font.size = 10) )
my.pars = set_of_paragraphs(pot3)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )

pot4 =  pot("Abundance Measure: ", textProperties(font.weight="bold", font.size = 10) ) + 
        pot(paste(stockabundance), textProperties(font.size = 10) )
my.pars = set_of_paragraphs(pot4)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )


if (exists("n1", mode="environment")) {
    n1_hist_run_years <- NULL 
    for (i in 1:length(n1$datalist)) {
    a <- names(n1$datalist)[i]
    a <- capwords(a)
    usePackage("stringr")
    a <- str_replace_all(a, "Age", "Age ")
    m <- min(range(n1$datalist[[i]]$CY))
    M <- max(range(n1$datalist[[i]]$CY))
    n1_hist_run_years <- c(n1_hist_run_years, paste0(m, " - ", M, " (", a, ")"))
   }
   n1_hist_run_years <- paste(n1_hist_run_years, collapse="; ")
}


if (exists("n3", mode="environment")) {
    n3_hist_run_years <- NULL 
    for (i in 1:length(n3$datalist)) {
    a <- names(n3$datalist)[i]
    a <- capwords(a)
    usePackage("stringr")
    a <- str_replace_all(a, "Age", "Age ")
    m <- min(range(n3$datalist[[i]]$CY))
    M <- max(range(n3$datalist[[i]]$CY))
    n3_hist_run_years <- c(n3_hist_run_years, paste0(m, " - ", M, " (", a, ")"))
   }
   n3_hist_run_years <- paste(n3_hist_run_years, collapse="; ")
}

if (exists("n5", mode="environment")) {
    n5_hist_run_years <- NULL 
    for (i in 1:length(n5$datalist)) {
    a <- names(n5$datalist)[i]
    a <- capwords(a)
    usePackage("stringr")
    a <- str_replace_all(a, "Age", "Age ")
    m <- min(range(n5$datalist[[i]]$CY))
    M <- max(range(n5$datalist[[i]]$CY))
    n5_hist_run_years <- c(n5_hist_run_years, paste0(m, " - ", M, " (", a, ")"))
   }
   n5_hist_run_years <- paste(n5_hist_run_years, collapse="; ")
}


if (exists("ARIMA", mode="environment")) {
    ARIMA_hist_run_years <- NULL 
    for (i in 1:length(ARIMA$datalist)) {
    a <- names(ARIMA$datalist)[i]
    a <- capwords(a)
    usePackage("stringr")
    a <- str_replace_all(a, "Age", "Age ")
    m <- min(range(ARIMA$datalist[[i]]$CY))
    M <- max(range(ARIMA$datalist[[i]]$CY))
    ARIMA_hist_run_years <- c(ARIMA_hist_run_years, paste0(m, " - ", M, " (", a, ")"))
   }
   ARIMA_hist_run_years <- paste(ARIMA_hist_run_years, collapse="; ")
}

if (exists("EXPSMOOTH", mode="environment")) {
    EXPSMOOTH_hist_run_years <- NULL 
    for (i in 1:length(EXPSMOOTH$datalist)) {
    a <- names(EXPSMOOTH$datalist)[i]
    a <- capwords(a)
    usePackage("stringr")
    a <- str_replace_all(a, "Age", "Age ")
    m <- min(range(EXPSMOOTH$datalist[[i]]$CY))
    M <- max(range(EXPSMOOTH$datalist[[i]]$CY))
    EXPSMOOTH_hist_run_years <- c(EXPSMOOTH_hist_run_years, paste0(m, " - ", M, " (", a, ")"))
   }
   EXPSMOOTH_hist_run_years <- paste(EXPSMOOTH_hist_run_years, collapse="; ")
}

if (exists("SIMPLESIBREG", mode="environment")) {
    SIMPLESIBREG_hist_run_years <- NULL 
    for (i in 1:length(SIMPLESIBREG$datalist)) {
    a <- names(SIMPLESIBREG$datalist)[i]
    a <- capwords(a)
    usePackage("stringr")
    a <- str_replace_all(a, "Age", "Age ")
    m <- min(range(SIMPLESIBREG$datalist[[i]]$CY))
    M <- max(range(SIMPLESIBREG$datalist[[i]]$CY))
    SIMPLESIBREG_hist_run_years <- c(SIMPLESIBREG_hist_run_years, paste0(m, " - ", M, " (", a, ")"))
   }
   SIMPLESIBREG_hist_run_years <- paste(SIMPLESIBREG_hist_run_years, collapse="; ")
}


if (exists("SIMPLELOGPOWER", mode="environment")) {
    SIMPLELOGPOWER_hist_run_years <- NULL 
    for (i in 1:length(SIMPLELOGPOWER$datalist)) {
    a <- names(SIMPLELOGPOWER$datalist)[i]
    a <- capwords(a)
    usePackage("stringr")
    a <- str_replace_all(a, "Age", "Age ")
    m <- min(range(SIMPLELOGPOWER$datalist[[i]]$CY))
    M <- max(range(SIMPLELOGPOWER$datalist[[i]]$CY))
    SIMPLELOGPOWER_hist_run_years <- c(SIMPLELOGPOWER_hist_run_years, paste0(m, " - ", M, " (", a, ")"))
   }
   SIMPLELOGPOWER_hist_run_years <- paste(SIMPLELOGPOWER_hist_run_years, collapse="; ")
}



hist_run_years <- c( ifelse(!is.null(n1_hist_run_years),n1_hist_run_years,NULL),
                     ifelse(!is.null(n3_hist_run_years),n3_hist_run_years,NULL), 
                     ifelse(!is.null(n5_hist_run_years),n5_hist_run_years,NULL), 
                     ifelse(!is.null(ARIMA_hist_run_years),ARIMA_hist_run_years,NULL),
                     ifelse(!is.null(EXPSMOOTH_hist_run_years),EXPSMOOTH_hist_run_years,NULL), 
                     ifelse(!is.null(SIMPLESIBREG_hist_run_years),SIMPLESIBREG_hist_run_years,NULL), 
                     ifelse(!is.null(SIMPLELOGPOWER_hist_run_years),SIMPLELOGPOWER_hist_run_years,NULL)
                    )

hist_run_years <- unique(hist_run_years)

hist_run_years


pot5a =  pot("Historical Run Years: ", textProperties(font.weight="bold", font.size = 10) ) + 
        pot(paste(hist_run_years), textProperties(font.size = 10) )
my.pars = set_of_paragraphs(pot5a)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )

pot5b =  pot("Forecasting Year: ", textProperties(font.weight="bold", font.size = 10) ) + 
        pot(paste(forecastingyear), textProperties(font.size = 11) )
my.pars = set_of_paragraphs(pot5b)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )


if (length(pred.args)>1) {
  pot6 =  pot("Forecasting Models: ", textProperties(font.weight="bold", font.size = 10)) 
  my.pars = set_of_paragraphs(pot6)
  doc = addParagraph( doc, value = my.pars, stylename="Normal" )
} else if (length(pred.args)==1) {
  pot6 =  pot("Forecasting Model: ", textProperties(font.weight="bold", font.size = 10)) 
  my.pars = set_of_paragraphs(pot6)
  doc = addParagraph( doc, value = my.pars, stylename="Normal" )
}



for (k in 1:length(pred.args)) {

    tmplabel <- names(pred.args)[k]  
    
    usePackage("stringr")
    
    tmplabel <- str_replace_all(tmplabel, "pred.int.individual.ages.", "")
    
    tmplabel
    
    if (tmplabel=="naiveone") {
              mypot <- "Naive Model (Previous Year)"
    } else if (tmplabel=="avgthree") {
              mypot <- "Naive Model (Average of Previous 3 Years)"
    } else if (tmplabel=="avgfive") {
              mypot <- "Naive Model (Average of Previous 5 Years)"
    } else if (tmplabel=="arima") {
              mypot <- "ARIMA Model"
    } else if (tmplabel=="expsmooth") {
              mypot <- "Exponential Smoothing Model"
    } else if (tmplabel=="sibreg") {
            mypot <- "Sibling Regression Model"
    } else if (tmplabel=="logpower") {
            mypot <- "Log Power Regression Model"
    }

    pot.model =  pot(mypot, textProperties(font.size = 10) ) 
    my.pars = set_of_paragraphs(pot.model)
    doc = addParagraph( doc, value = my.pars, stylename="BulletList" )

}


if (length(names(retromeasure.args))>1) {
   pot6 =  pot("Forecast Performance Measures: ", textProperties(font.weight="bold", font.size = 10)) 
   my.pars = set_of_paragraphs(pot6)
   doc = addParagraph( doc, value = my.pars, stylename="Normal" )
} else if (length(names(retromeasure.args))==1) {
   pot6 =  pot("Forecast Performance Measure: ", textProperties(font.weight="bold", font.size = 10)) 
   my.pars = set_of_paragraphs(pot6)
   doc = addParagraph( doc, value = my.pars, stylename="Normal" )
}


usePackage("stringr")   
measures <- str_replace_all(string=names(retromeasure.args), pattern="retromeasure", replacement="")

## for (k in 1:length(retromeasure.args)) {

pot.measures =  pot(paste(measures, collapse="; "), textProperties(font.size = 10) ) 
my.pars = set_of_paragraphs(pot.measures)
doc = addParagraph( doc, value = my.pars, stylename="BulletList" )

## }

if (length(pred.args)>1) {

    pottmp =  pot(paste0("Best Forecasting Model for Total ", stockabundance), textProperties(font.weight="bold", font.size = 10)) 
    my.pars = set_of_paragraphs(pottmp)
    doc = addParagraph( doc, value = my.pars, stylename="Normal" ) 
   
    
    BestForecastingModel <- table.rank.rmse.results.total.age$best.model
    
    if (BestForecastingModel=="naiveone") {
        BestForecastingModel <- "Naive Model (Previous Year)"
    } else if (BestForecastingModel=="avgthree") {
        BestForecastingModel <- "Naive Model (Average of Previous 3 Years)"
    } else if (BestForecastingModel=="avgfive") {
        BestForecastingModel <- "Naive Model (Average of Previous 5 Years)"
    } else if (BestForecastingModel=="arima") {
        BestForecastingModel <- "ARIMA Model"
    } else if (BestForecastingModel=="expsmooth") {
        BestForecastingModel <- "Exponential Smoothing Model"
    } else if (BestForecastingModel=="sibreg") {
        BestForecastingModel <- "Sibling Regression Model"
    } else if (BestForecastingModel=="sibreg") {
        BestForecastingModel <- "Log Power Regression Model"
    }
    

    pot.best =  pot(BestForecastingModel, textProperties(font.size = 10) ) 
    my.pars = set_of_paragraphs(pot.best)
    doc = addParagraph( doc, value = my.pars, stylename="BulletList" )

    rm(BestForecastingModel)

}


if (length(pred.args)>1) {

    pottmp =  pot(paste0("Best Forecasting Model for Age-Specific ", stockabundance), textProperties(font.weight="bold", font.size = 10)) 
    my.pars = set_of_paragraphs(pottmp)
    doc = addParagraph( doc, value = my.pars, stylename="Normal" ) 
   
    for (i in 1:length(Age)){
    
        table.rank.rmse.results.individual.age <- rank.rmse.results.individual.age(table.rmse.results.individual.age, pred.args, all.pred.args, i)
    
        BestForecastingModel <- table.rank.rmse.results.individual.age$best.model
        
        if (BestForecastingModel=="naiveone") {
        BestForecastingModel <- paste(Age[i]," - ", "Naive Model (Previous Year)")
        } else if (BestForecastingModel=="avgthree") {
        BestForecastingModel <- paste(Age[i]," - ", "Naive Model (Average of Previous 3 Years)")
        } else if (BestForecastingModel=="avgfive") {
        BestForecastingModel <- paste(Age[i]," - ", "Naive Model (Average of Previous 5 Years)")
        } else if (BestForecastingModel=="arima") {
        BestForecastingModel <- paste(Age[i]," - ", "ARIMA Model")
        } else if (BestForecastingModel=="expsmooth") {
        BestForecastingModel <- paste(Age[i]," - ", "Exponential Smoothing Model")
        } else if (BestForecastingModel=="sibreg") {
        BestForecastingModel <- paste(Age[i]," - ", "Sibling Regression Model")
        } else if (BestForecastingModel=="logpower") {
        BestForecastingModel <- paste(Age[i]," - ", "Log Power Regression Model")  
        }
    
        pot.best =  pot(BestForecastingModel, textProperties(font.size = 10) ) 
        my.pars = set_of_paragraphs(pot.best)
        doc = addParagraph( doc, value = my.pars, stylename="BulletList" )
    
        rm(BestForecastingModel)
    
    }


}

     
pot7 =  pot("Date: ", textProperties(font.weight="bold", font.size = 10) ) + 
        pot(paste(Sys.Date()), textProperties(font.size = 11) )
my.pars = set_of_paragraphs(pot7)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )




#===============================================================================
# Point forecasts for age-specific and total abundance
#===============================================================================



n1$results.point.forecast.naiveone
n3$results.point.forecast.avgthree
n5$results.point.forecast.avgfive 
ARIMA$results.point.forecast.arima
EXPSMOOTH$results.point.forecast.expsmooth

#-------------------------------------------

SIMPLESIBREG$results.point.forecast.sibreg <- SIMPLESIBREG$point.and.interval.forecasts.complex.sibling.regression
 
SIMPLESIBREG$results.point.forecast.sibreg <- SIMPLESIBREG$results.point.forecast.sibreg[-nrow(SIMPLESIBREG$results.point.forecast.sibreg),]

SIMPLESIBREG$results.point.forecast.sibreg <- SIMPLESIBREG$results.point.forecast.sibreg[ ,-ncol(SIMPLESIBREG$results.point.forecast.sibreg)]

names(SIMPLESIBREG$results.point.forecast.sibreg) <- c("Age", "Model", "RY", "p")

SIMPLESIBREG$results.point.forecast.sibreg$RY <- as.numeric(SIMPLESIBREG$results.point.forecast.sibreg$RY)
SIMPLESIBREG$results.point.forecast.sibreg$p <- as.numeric(replacecomma(SIMPLESIBREG$results.point.forecast.sibreg$p))

SIMPLESIBREG$results.point.forecast.sibreg$Age <- as.factor(SIMPLESIBREG$results.point.forecast.sibreg$Age)

#-------------------------------------------

#-------------------------------------------

SIMPLELOGPOWER$results.point.forecast.logpower <- SIMPLELOGPOWER$point.and.interval.forecasts.logpower.regression
 
SIMPLELOGPOWER$results.point.forecast.logpower <- SIMPLELOGPOWER$results.point.forecast.logpower[-nrow(SIMPLELOGPOWER$results.point.forecast.logpower),]

SIMPLELOGPOWER$results.point.forecast.logpower <- SIMPLELOGPOWER$results.point.forecast.logpower[ ,-ncol(SIMPLELOGPOWER$results.point.forecast.logpower)]

names(SIMPLELOGPOWER$results.point.forecast.logpower) <- c("Age", "Model", "RY", "p")

SIMPLELOGPOWER$results.point.forecast.logpower$RY <- as.numeric(SIMPLELOGPOWER$results.point.forecast.logpower$RY)
SIMPLELOGPOWER$results.point.forecast.logpower$p <- as.numeric(replacecomma(SIMPLELOGPOWER$results.point.forecast.logpower$p))

SIMPLELOGPOWER$results.point.forecast.logpower$Age <- as.factor(SIMPLELOGPOWER$results.point.forecast.logpower$Age)

#-------------------------------------------



ft_point_forecast <- NULL 

if (exists("n1", mode="environment")) {
   ft_tmp_1 <- n1$results.point.forecast.naiveone
   ft_tmp_2 <- c(unique(ft_tmp_1$Model), ft_tmp_1$p, sum(ft_tmp_1$p))
   ## colnames(ft_tmp_2) <- c("Model", as.character(ft_tmp_1$Age), "Total")
   ft_point_forecast <- rbind.data.frame(ft_point_forecast, ft_tmp_2, stringsAsFactors = FALSE) 
  
   names(ft_point_forecast) <- c("Model", as.character(ft_tmp_1$Age), "Total")
   
   rm(ft_tmp_1, ft_tmp_2)
}


if (exists("n3", mode="environment")) {
   ft_tmp_1 <- n3$results.point.forecast.avgthree
   ft_tmp_2 <- c(unique(ft_tmp_1$Model), ft_tmp_1$p, sum(ft_tmp_1$p)) 
   
   ft_tmp_2[-1] <- as.numeric(ft_tmp_2[-1])
   
   ft_point_forecast <- rbind.data.frame(ft_point_forecast, ft_tmp_2, stringsAsFactors = FALSE) 
  
   colnames(ft_point_forecast) <- c("Model", as.character(ft_tmp_1$Age), "Total")
   rm(ft_tmp_1, ft_tmp_2)
}


if (exists("n5", mode="environment")) {
   ft_tmp_1 <- n5$results.point.forecast.avgfive
   ft_tmp_2 <- c(unique(ft_tmp_1$Model), ft_tmp_1$p, sum(ft_tmp_1$p)) 
   
   ft_tmp_2[-1] <- as.numeric(ft_tmp_2[-1])
   
   ft_point_forecast <- rbind.data.frame(ft_point_forecast, ft_tmp_2, stringsAsFactors = FALSE) 
  
   colnames(ft_point_forecast) <- c("Model", as.character(ft_tmp_1$Age), "Total")
   rm(ft_tmp_1, ft_tmp_2)
}


if (exists("ARIMA", mode="environment")) {
   ft_tmp_1 <- ARIMA$results.point.forecast.arima
   ft_tmp_2 <- c("ARIMA", ft_tmp_1$p, sum(ft_tmp_1$p)) 
   
   ft_tmp_2[-1] <- as.numeric(ft_tmp_2[-1])
   
   ft_point_forecast <- rbind.data.frame(ft_point_forecast, ft_tmp_2, stringsAsFactors = FALSE) 
  
   colnames(ft_point_forecast) <- c("Model", as.character(ft_tmp_1$Age), "Total")
   rm(ft_tmp_1, ft_tmp_2)
}


if (exists("EXPSMOOTH", mode="environment")) {
   ft_tmp_1 <- EXPSMOOTH$results.point.forecast.expsmooth
   ft_tmp_2 <- c("Exponential Smoothing", ft_tmp_1$p, sum(ft_tmp_1$p)) 
   
   ft_tmp_2[-1] <- as.numeric(ft_tmp_2[-1])
   
   ft_point_forecast <- rbind.data.frame(ft_point_forecast, ft_tmp_2, stringsAsFactors = FALSE) 
  
   colnames(ft_point_forecast) <- c("Model", as.character(ft_tmp_1$Age), "Total")
   rm(ft_tmp_1, ft_tmp_2)
}

if (exists("SIMPLESIBREG", mode="environment")) {
   ft_tmp_1 <- SIMPLESIBREG$results.point.forecast.sibreg
   ft_tmp_2 <- c("Sibling Regression", ft_tmp_1$p, sum(ft_tmp_1$p)) 
   
   ft_tmp_2[-1] <- as.numeric(ft_tmp_2[-1])
   
   ft_point_forecast <- rbind.data.frame(ft_point_forecast, ft_tmp_2, stringsAsFactors = FALSE) 
  
   colnames(ft_point_forecast) <- c("Model", as.character(ft_tmp_1$Age), "Total")
   rm(ft_tmp_1, ft_tmp_2)
}

if (exists("SIMPLELOGPOWER", mode="environment")) {
   ft_tmp_1 <- SIMPLELOGPOWER$results.point.forecast.logpower
   ft_tmp_2 <- c("Log Power Regression", ft_tmp_1$p, sum(ft_tmp_1$p)) 
   
   ft_tmp_2[-1] <- as.numeric(ft_tmp_2[-1])
   
   ft_point_forecast <- rbind.data.frame(ft_point_forecast, ft_tmp_2, stringsAsFactors = FALSE) 
  
   colnames(ft_point_forecast) <- c("Model", as.character(ft_tmp_1$Age), "Total")
   rm(ft_tmp_1, ft_tmp_2)
}



usePackage("scales")

str(ft_point_forecast)

ft_point_forecast[,-1] <- apply(ft_point_forecast[,-1], 2, as.numeric)
ft_point_forecast[,-1] <- apply(ft_point_forecast[,-1], 2, comma)

ft_point_forecast

#=================================================================================

doc = addPageBreak(doc)

doc = addSection(doc)

mytitle <- ifelse(length(table.rank.rmse.results.total.age$all.models) > 1, 
                  paste0("Point and Interval Forecasts Produced by the User-Specified Forecasting Models"), 
                  paste0("Point and Interval Forecasts Produced by the User-Specified Forecasting Model"))

doc = addTitle(doc,value=mytitle, level=1) # Add titles

tablecaption <- paste0("Point forecasts of ",
                      forecastingyear, " age-specific and total ",
                      paste0(tolower(stockabundance),"s"),
                      " associated with the ",
                      stockname, " ",
                      tolower(stockspecies), " stock, ", 
                      "which were produced by the forecasting model(s) considered for this stock.")

doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")

#=================================================================================

MyFTable = FlexTable(data = ft_point_forecast,
                     body.cell.props = cellProperties( padding = 4, border.color="darkgrey"),  ## MANGO1
                     header.cell.props= cellProperties( padding = 4, border.color="darkgrey"),
                     header.text.props = textBold(), header.columns = FALSE)

MyFTable = addHeaderRow(MyFTable, text.properties = textBold(),
                           value = colnames(ft_point_forecast),
                           colspan = rep(1, ncol(ft_point_forecast)),
                           cell.properties = cellProperties( padding = 4, background.color="lightgrey", border.color="darkgrey" ))

if (exists("ARIMA", mode="environment")) {

    MyFTable = addFooterRow(MyFTable, 
                        value = 'ARIMA Modeling Details',
                        colspan = ncol(ft_point_forecast),
                        text.properties = textProperties(color = "darkblue", font.weight="bold", font.size = 8),
                        cell.properties = cellProperties(background.color = "lightgrey", padding = 4, border.color="darkgrey") )
                        
    ft_tmp_1 <- ARIMA$results.point.forecast.arima
                       
    for (k in 1:nrow(ft_tmp_1)){

         value_tmp <- c(paste0("   ", ft_tmp_1$Age[k], "   "), ft_tmp_1$Model[k]) 

         MyFTable = addFooterRow(MyFTable, 
                        value = value_tmp,
                        colspan = c(1, ncol(ft_point_forecast)-1),
                        text.properties = textProperties(color = "darkblue", font.size = 8),
                        cell.properties = cellProperties(padding = 4, border.color="darkgrey") ) 

         rm(value_tmp)
    }                   
    
    rm(ft_tmp_1)
}
    

if (exists("EXPSMOOTH", mode="environment")) {

    MyFTable = addFooterRow(MyFTable, 
                        value = 'Exponential Smoothing Modeling Details',
                        colspan = ncol(ft_point_forecast),
                        text.properties = textProperties(color = "darkblue", font.weight="bold", font.size = 8),
                        cell.properties = cellProperties(background.color = "lightgrey", padding = 4, border.color="darkgrey") )
                        
    ft_tmp_1 <- EXPSMOOTH$results.point.forecast.expsmooth
                       
    for (k in 1:nrow(ft_tmp_1)){

         value_tmp <- c(paste0("   ", ft_tmp_1$Age[k], "   "), ft_tmp_1$Model[k]) 

         MyFTable = addFooterRow(MyFTable, 
                        value = value_tmp,
                        colspan = c(1, ncol(ft_point_forecast)-1),
                        text.properties = textProperties(color = "darkblue", font.size = 8),
                        cell.properties = cellProperties(padding = 4, border.color="darkgrey") ) 

         rm(value_tmp)
    }       
    
    rm(ft_tmp_1)            
}
    
if (exists("SIMPLESIBREG", mode="environment")) {

    MyFTable = addFooterRow(MyFTable, 
                        value = 'Sibling Regression Modeling Details',
                        colspan = ncol(ft_point_forecast),
                        text.properties = textProperties(color = "darkblue", font.weight="bold", font.size = 8),
                        cell.properties = cellProperties(background.color = "lightgrey", padding = 4, border.color="darkgrey") )
                        
    ft_tmp_1 <- SIMPLESIBREG$results.point.forecast.sibreg
                       
    for (k in 1:nrow(ft_tmp_1)){

         value_tmp <- c(paste0("   ", ft_tmp_1$Age[k], "   "), ft_tmp_1$Model[k]) 

         MyFTable = addFooterRow(MyFTable, 
                        value = value_tmp,
                        colspan = c(1, ncol(ft_point_forecast)-1),
                        text.properties = textProperties(color = "darkblue", font.size = 8),
                        cell.properties = cellProperties(padding = 4, border.color="darkgrey") ) 

         rm(value_tmp)
    }       
    
    rm(ft_tmp_1)            
}
    
    
if (exists("SIMPLELOGPOWER", mode="environment")) {

    MyFTable = addFooterRow(MyFTable, 
                        value = 'Log Power Regression Modeling Details',
                        colspan = ncol(ft_point_forecast),
                        text.properties = textProperties(color = "darkblue", font.weight="bold", font.size = 8),
                        cell.properties = cellProperties(background.color = "lightgrey", padding = 4, border.color="darkgrey") )
                        
    ft_tmp_1 <- SIMPLELOGPOWER$results.point.forecast.logpower
                       
    for (k in 1:nrow(ft_tmp_1)){

         value_tmp <- c(paste0("   ", ft_tmp_1$Age[k], "   "), ft_tmp_1$Model[k]) 

         MyFTable = addFooterRow(MyFTable, 
                        value = value_tmp,
                        colspan = c(1, ncol(ft_point_forecast)-1),
                        text.properties = textProperties(color = "darkblue", font.size = 8),
                        cell.properties = cellProperties(padding = 4, border.color="darkgrey") ) 

         rm(value_tmp)
    }       
    
    rm(ft_tmp_1)            
}
    
    

#### MyFTable[1, 1, to = 'header'] = parProperties(text.align = 'left')
                        
ft_point_forecast
                        
MyFTable[1:nrow(ft_point_forecast) , 2:ncol(ft_point_forecast)] = parProperties( text.align = 'center', padding = 4)

doc = addFlexTable(doc, MyFTable)

rm(MyFTable)
  
rm(ft_point_forecast)
  
  
## PANETTONE  

#===============================================================================
# Point and interval forecasts for age-specific and total abundance
#===============================================================================


#--- Ages and Models
n1$results.point.forecast.naiveone
n3$results.point.forecast.avgthree
n5$results.point.forecast.avgfive 
ARIMA$results.point.forecast.arima
EXPSMOOTH$results.point.forecast.expsmooth

SIMPLESIBREG$results.point.forecast.sibreg
SIMPLELOGPOWER$results.point.forecast.logpower
  
#--- Point and Interval Forecasts
n1$PI.individual.ages.naiveone
n3$PI.individual.ages.avgthree
n5$PI.individual.ages.avgfive
ARIMA$PI.individual.ages.arima
EXPSMOOTH$PI.individual.ages.expsmooth

SIMPLESIBREG$PI.individual.ages.sibreg
SIMPLELOGPOWER$PI.individual.ages.logpower
  
#-------------------------------------------------------------------------------


ft_n1 <- NULL 

if (exists("n1", mode="environment")) {
     
   for (j in 1:nrow(n1$PI.individual.ages.naiveone)) {
   
        ft_tmp_1 <- n1$results.point.forecast.naiveone
        ft_tmp_11 <- n1$PI.individual.ages.naiveone
   
        ft_n1 <- c(ft_n1, ft_tmp_11$PI.ctr[j], paste0(ft_tmp_11$PI.lwr[j] ," - ", ft_tmp_11$PI.upr[j]))
   }
   
   ft_n1 <- c(unique(n1$results.point.forecast.naiveone$Model),ft_n1)

   rm(ft_tmp_1, ft_tmp_11)
}


ft_n3 <- NULL

if (exists("n3", mode="environment")) {
     
   for (j in 1:nrow(n3$PI.individual.ages.avgthree)) {
   
        ft_tmp_1 <- n3$results.point.forecast.avgthree
        ft_tmp_11 <- n3$PI.individual.ages.avgthree
   
        ft_n3 <- c(ft_n3, ft_tmp_11$PI.ctr[j], paste0(ft_tmp_11$PI.lwr[j] ," - ", ft_tmp_11$PI.upr[j]))
   }
   
   ft_n3 <- c(unique(n3$results.point.forecast.avgthree$Model),ft_n3)

   rm(ft_tmp_1, ft_tmp_11)

}



ft_n5 <- NULL 

if (exists("n5", mode="environment")) {
   
   for (j in 1:nrow(n5$PI.individual.ages.avgfive)) {
   
        ft_tmp_1 <- n5$results.point.forecast.avgfive
        ft_tmp_11 <- n5$PI.individual.ages.avgfive
   
        ft_n5 <- c(ft_n5, ft_tmp_11$PI.ctr[j], paste0(ft_tmp_11$PI.lwr[j] ," - ", ft_tmp_11$PI.upr[j]))
   }
   
   ft_n5 <- c(unique(n5$results.point.forecast.avgfive$Model),ft_n5)

   rm(ft_tmp_1, ft_tmp_11)

}


ft_ARIMA <- NULL 

if (exists("ARIMA", mode="environment")) {
   
   for (j in 1:nrow(ARIMA$PI.individual.ages.arima)) {
   
        ft_tmp_1 <- ARIMA$results.point.forecast.arima
        ft_tmp_11 <- ARIMA$PI.individual.ages.arima
   
        ft_ARIMA <- c(ft_ARIMA, ft_tmp_11$PI.ctr[j], paste0(ft_tmp_11$PI.lwr[j] ," - ", ft_tmp_11$PI.upr[j]))
   }
   
   ft_ARIMA <- c("ARIMA",ft_ARIMA)

   rm(ft_tmp_1, ft_tmp_11)

}





ft_EXPSMOOTH <- NULL 

if (exists("EXPSMOOTH", mode="environment")) {
   
   for (j in 1:nrow(EXPSMOOTH$PI.individual.ages.expsmooth)) {
   
        ft_tmp_1 <- EXPSMOOTH$results.point.forecast.expsmooth
        ft_tmp_11 <- EXPSMOOTH$PI.individual.ages.expsmooth
   
        ft_EXPSMOOTH <- c(ft_EXPSMOOTH, ft_tmp_11$PI.ctr[j], paste0(ft_tmp_11$PI.lwr[j] ," - ", ft_tmp_11$PI.upr[j]))
   }
   
   ft_EXPSMOOTH <- c("Exponential Smoothing",ft_EXPSMOOTH)

   rm(ft_tmp_1, ft_tmp_11)

}




ft_SIMPLESIBREG <- NULL 

if (exists("SIMPLESIBREG", mode="environment")) {
   
   for (j in 1:nrow(SIMPLESIBREG$PI.individual.ages.sibreg)) {
   
        ft_tmp_1 <- SIMPLESIBREG$results.point.forecast.sibreg
        ft_tmp_11 <- SIMPLESIBREG$PI.individual.ages.sibreg
   
        ft_SIMPLESIBREG <- c(ft_SIMPLESIBREG, ft_tmp_11$PI.ctr[j], paste0(ft_tmp_11$PI.lwr[j] ," - ", ft_tmp_11$PI.upr[j]))
   }
   
   ft_SIMPLESIBREG <- c("Sibling Regression",ft_SIMPLESIBREG)

   rm(ft_tmp_1, ft_tmp_11)

}



ft_SIMPLELOGPOWER <- NULL 

if (exists("SIMPLELOGPOWER ", mode="environment")) {
   
   for (j in 1:nrow(SIMPLELOGPOWER $PI.individual.ages.logpower)) {
   
        ft_tmp_1 <- SIMPLELOGPOWER$results.point.forecast.logpower
        ft_tmp_11 <- SIMPLELOGPOWER$PI.individual.ages.logpower
   
        ft_SIMPLELOGPOWER <- c(ft_SIMPLELOGPOWER, ft_tmp_11$PI.ctr[j], paste0(ft_tmp_11$PI.lwr[j] ," - ", ft_tmp_11$PI.upr[j]))
   }
   
   ft_SIMPLELOGPOWER  <- c("Log Power Regression",ft_SIMPLELOGPOWER)

   rm(ft_tmp_1, ft_tmp_11)

}




ft_n1

ft_n3

ft_n5
  
ft_ARIMA

ft_EXPSMOOTH

ft_SIMPLESIBREG

ft_SIMPLELOGPOWER


ft <- rbind.data.frame(ft_n1, ft_n3, ft_n5, ft_ARIMA, ft_EXPSMOOTH, ft_SIMPLESIBREG, ft_SIMPLELOGPOWER)  

colnames(ft) <- NULL   
  
#===============================================================================

doc = addSection(doc, landscape=TRUE, ncol=1)

tablecaption <- paste0("Point forecasts and 80% interval forecasts for the ",
                      forecastingyear, " age-specific ",
                      paste0(tolower(stockabundance),"s"),
                      " associated with the ",
                      stockname, " ",
                      tolower(stockspecies), " stock, which were ", 
                      "produced by the forecasting model(s) considered for this stock.")

doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")

#=================================================================================

MyFTable = FlexTable(data = ft,
                     body.cell.props = cellProperties( padding = 1, border.color="darkgrey"),  ## MANGO1
                     body.text.props = textProperties(font.size = 9), 
                     header.cell.props= cellProperties( padding = 1, border.color="darkgrey"),
                     header.text.props = textProperties(font.size = 9, font.weight = "bold"), header.columns = FALSE)


## value_tmp <- c(" ", rep(Age,each=2))

value_tmp <- c(" ", Age)

MyFTable = addHeaderRow(MyFTable,
                           value = value_tmp, 
                           colspan = c(1, rep(2, (ncol(ft)-1)/2)),
                           cell.properties = cellProperties( padding = 1, background.color="lightgrey", border.color="darkgrey" ), 
                           text.properties = textProperties( font.size = 9, font.weight="bold"), 
                           par.properties = parProperties(text.align = "center"))

rm(value_tmp)


value_tmp <- c("Model", rep(c("Point Forecast","80% Interval Forecast"),length(Age)))

MyFTable = addHeaderRow(MyFTable, 
                           value = value_tmp, 
                           colspan = rep(1,ncol(ft)),
                           cell.properties = cellProperties( padding = 1, background.color="lightgrey", border.color="darkgrey" ), 
                           text.properties = textProperties( font.size = 9, font.weight = "bold" ), 
                           par.properties = parProperties(text.align = "center"))

rm(value_tmp)


## MyFTable = spanFlexTableColumns( MyFTable, i = 1, from = 2, to = Nmeasures+1)
## MyFTable[1, 1, text.properties = textBold(color = "black"), newpar = FALSE] = "Model"
## MyFTable[1, 2, text.properties = textBold(color = "black"), newpar = FALSE] = "Measure"
## MyFTable[1, Nmeasures+1, text.properties = textBold(color = "black"), newpar = FALSE] = ""

MyFTable[ ,2:ncol(ft)] = parCenter() # parRight()

doc = addFlexTable(doc, MyFTable)

rm(MyFTable)

## doc = addSection(doc, landscape=FALSE)
  
## BOOBOO
  
#===============================================================================
# Ranking of models for age-specific components of abundance
#===============================================================================


for (i in 1:length(Age)) {


  Measure <- NULL

  if (exists("n1", mode="environment")) {
      Measure <- c(Measure, as.character(n1$MIA$Measure))
  } else if (exists("n3", mode="environment")) {
      Measure <- c(Measure, as.character(n3$MIA$Measure))
  } else if (exists("n5", mode="environment")) {
      Measure <- c(Measure, as.character(n5$MIA$Measure))
  } else if (exists("ARIMA", mode="environment")) {
      Measure <- c(Measure, as.character(ARIMA$MIA$Measure))
  } else if (exists("EXPSMOOTH", mode="environment")) {
      Measure <- c(Measure, as.character(EXPSMOOTH$MIA$Measure))
  } else if (exists("SIMPLESIBREG", mode="environment")) {
  
      Measure <- c(Measure, as.character(SIMPLESIBREG$M$Measure))     # Note the use of M instead of MIA !!!
  
  } else if (exists("SIMPLELOGPOWER", mode="environment")) {
  
      Measure <- c(Measure, as.character(SIMPLELOGPOWER$M$Measure))     # Note the use of M instead of MIA !!!
  }


  Measure <- unique(Measure)


  #===============================================================================

  ## f_tt_0 <- c("", "", rep("",ncol(table.rank.rmse.results.no.age$measure)))

  Model <- NULL

  f_tt  <- c("Model", Measure, "")

  if (exists("n1", mode="environment")) {
    Model <- c(Model, "Naive Model (Previous Year)")
    f_tt <- rbind(f_tt, c("Naive Model (Previous Year)", n1$MIA[,2+i], " "))
  }

  if (exists("n3", mode="environment")) {
    Model <- c(Model, "Naive Model (Average of Previous Three Years)")
    f_tt <- rbind(f_tt, c("Naive Model (Average of Previous Three Years)", n3$MIA[,2+i]," "))
  }

  if (exists("n5", mode="environment")) {
    Model <- c(Model, "Naive Model (Average of Previous Five Years)")
    f_tt <- rbind(f_tt, c("Naive Model (Average of Previous Five Years)", n5$MIA[,2+i]," "))
  }

  if (exists("ARIMA", mode="environment")) {
    Model <- c(Model, "ARIMA Model")
    f_tt <- rbind(f_tt, c("ARIMA Model", ARIMA$MIA[,2+i]," "))
  }

  if (exists("EXPSMOOTH", mode="environment")) {
    Model <- c(Model, "Exponential Smoothing Model")
    f_tt <- rbind(f_tt, c("Exponential Smoothing Model", EXPSMOOTH$MIA[,2+i]," "))
  }

  if (exists("SIMPLESIBREG", mode="environment")) {
    Model <- c(Model, "Sibling Regression Model")
    f_tt <- rbind(f_tt, c("Sibling Regression Model", SIMPLESIBREG$M[,1+i]," "))
  }

  if (exists("SIMPLELOGPOWER", mode="environment")) {
    Model <- c(Model, "Log Power Regression Model")
    f_tt <- rbind(f_tt, c("Log Power Regression Model", SIMPLELOGPOWER$M[,1+i]," "))
  }


  Model 
  f_tt

  f_tt[f_tt=="NaN"] <- "NA"
  f_tt[f_tt=="-Inf"] <- "NA"
  f_tt[f_tt=="Inf"] <- "NA"

  #=================================================================================

  doc = addSection(doc, landscape=TRUE, ncol=1)
  
  mytitle <- ifelse(length(table.rank.rmse.results.total.age$all.models) > 1, 
                  paste0("Ranking of the User-Specified Forecasting Models Based on Measures of Retrospective Point Forecast Performance"), 
                  paste0("Measures of Retrospective Point Forecast Performance Corresponding to the User-Specified Forecasting Model"))

  doc = addTitle(doc,value=mytitle, level=1) # Add titles

  tablecaption <- paste0("Ranking of models used for forecasting the ",
                      forecastingyear, " ",
                      tolower(stockabundance),
                      " associated with the ",
                      tolower(Age[i]), " component of the ",
                      stockname, " ",
                      tolower(stockspecies), " stock. ",
                      "For each measure, models are first ranked with respect to each of the reported ",
                      "retrospective point forecast performance measures, ",
                      "such that the model which achieves the lowest absolute value for that measure ",
                      "receives the smallest rank. ",
                      "For each model, the obtained ranks are then averaged across all performance measures to obtain a ",
                      "model-specific average rank. ",
                      "The best forecasting model is the one with the smallest ", 
                      "model-specific average rank.")


   doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")

  #=================================================================================

  Nmeasures <- length(Measure)

  Nmodels <- length(Model)

  MyFTable = FlexTable(data = f_tt,
                     body.cell.props = cellProperties( padding = 4, border.color="darkgrey"),  ## MANGO1
                     header.cell.props= cellProperties( padding = 4, border.color="darkgrey"),
                     header.text.props = textBold(), header.columns = FALSE)

  MyFTable = addHeaderRow( MyFTable, text.properties = textBold(),
                           value = paste0(Age[i]," component of total ", tolower(stockabundance), " for the ", stockname, " ", stockspecies, " stock"),
                           colspan = ncol(f_tt),
                           cell.properties = cellProperties( padding = 4, background.color="lightgrey", border.color="darkgrey" ))

  MyFTable = addHeaderRow( MyFTable, text.properties = textBold(),
                           value = c("Model","Measure", " "),
                           colspan = c(1, Nmeasures, 1),
                           cell.properties = cellProperties(padding = 4, border.color="darkgrey"))


  table.rank.rmse.results.individual.age <- rank.rmse.results.individual.age(table.rmse.results.individual.age, pred.args, all.pred.args, i)

  table.avg.rank.rmse.results.individual.age <- cbind.data.frame(table.rank.rmse.results.individual.age$ranks, 
                                                                 sprintf("%.2f",table.rank.rmse.results.individual.age$avg.ranks))
                                                                 
  table.avg.rank.rmse.results.individual.age <- cbind.data.frame(rownames(table.avg.rank.rmse.results.individual.age),table.avg.rank.rmse.results.individual.age)
                                                                 
  names(table.avg.rank.rmse.results.individual.age)[1] <- "Model"
  
  rownames(table.avg.rank.rmse.results.individual.age) <- NULL

  names(table.avg.rank.rmse.results.individual.age)[length(names(table.avg.rank.rmse.results.individual.age))] <- 
        paste(names(table.avg.rank.rmse.results.individual.age)[-c(1,length(names(table.avg.rank.rmse.results.individual.age)))], collapse=", ") 

  table.avg.rank.rmse.results.individual.age <- data.frame(lapply(table.avg.rank.rmse.results.individual.age, as.character), stringsAsFactors=FALSE)

  names(table.avg.rank.rmse.results.individual.age)[length(names(table.avg.rank.rmse.results.individual.age))] <- 
      str_replace_all(names(table.avg.rank.rmse.results.individual.age)[length(names(table.avg.rank.rmse.results.individual.age))],"\\..",", ")

  if (exists("n1", mode="environment")) {  
  table.avg.rank.rmse.results.individual.age$Model  <- str_replace_all(table.avg.rank.rmse.results.individual.age$Model, 
                                                                       "naiveone", "Naive Model (Previous Year)")
  } 
  
  if (exists("n3", mode="environment")) {  
  table.avg.rank.rmse.results.individual.age$Model  <- str_replace_all(table.avg.rank.rmse.results.individual.age$Model, 
                                                                       "avgthree", "Naive Model (Average of Previous Three Years)") 
  }
                
  if (exists("n5", mode="environment")) {                                                         
  table.avg.rank.rmse.results.individual.age$Model  <- str_replace_all(table.avg.rank.rmse.results.individual.age$Model, 
                                                                       "avgfive", "Naive Model (Average of Previous Five Years)") 
  }

  if (exists("ARIMA", mode="environment")) {  
  table.avg.rank.rmse.results.individual.age$Model  <- str_replace_all(table.avg.rank.rmse.results.individual.age$Model, 
                                                                       "arima", "ARIMA Model") 
  }
  
  if (exists("EXPSMOOTH", mode="environment")) {  
  table.avg.rank.rmse.results.individual.age$Model  <- str_replace_all(table.avg.rank.rmse.results.individual.age$Model, 
                                                                       "expsmooth", "Exponential Smoothing Model") 
  }
  if (exists("SIMPLESIBREG", mode="environment")) {  
  table.avg.rank.rmse.results.individual.age$Model  <- str_replace_all(table.avg.rank.rmse.results.individual.age$Model, 
                                                                       "sibreg", "Sibling Regression Model") 
  }

  ## WASHINGTON
  
  MyFTable = addFooterRow(MyFTable,  text.properties = textProperties(font.weight = "bold"),
                           value = c("Model","Rank", "Average Rank"),
                           colspan = c(1, Nmeasures, 1),
                           cell.properties = cellProperties(padding = 4, border.color="darkgrey"))

  MyFTable = addFooterRow(MyFTable, text.properties = textProperties(font.weight = "normal"), 
                           value = names(table.avg.rank.rmse.results.individual.age),
                           colspan = c(1, rep(1, Nmeasures), 1),
                           cell.properties = cellProperties(padding = 4, border.color="darkgrey"))
                           
  for (j in 1:nrow(table.avg.rank.rmse.results.individual.age)) {
  
        if (j != table.rank.rmse.results.individual.age$index.min.avg.rank) {
        
           MyFTable = addFooterRow(MyFTable, text.properties = textProperties(font.weight = "normal"),
                           value = as.character(table.avg.rank.rmse.results.individual.age[j, ]),
                           colspan = c(1, rep(1, Nmeasures), 1),
                           cell.properties = cellProperties(padding = 4, border.color="darkgrey"))
  
        }
        
        if (j == table.rank.rmse.results.individual.age$index.min.avg.rank) {
        
            MyFTable = addFooterRow(MyFTable, text.properties = textProperties(font.weight = "normal"),
                           value = as.character(table.avg.rank.rmse.results.individual.age[j, ]),
                           colspan = c(1, rep(1, Nmeasures), 1),
                           cell.properties = cellProperties(padding = 4, border.color="darkgrey",  background.color = "yellow"))
        
        }
        
  
  }
                             
  ## MyFTable = addFooterRow( MyFTable, value = paste0('Note: Ranking across models for a given performance measure is based on the absolute values of that measure.',
  ##                                                  " The model with the smallest absolute value of the measure receives the smallest rank."),
  ##                         colspan = ncol(f_tt),
  ##                         text.properties = textProperties(color = "red", font.weight="bold", font.size = 9),
  ##                         cell.properties = cellProperties(padding = 4, border.color="darkgrey") )

  
  doc = addFlexTable(doc, MyFTable)

  rm(MyFTable)
  
  rm(f_tt)
  
  rm(Model)

  ## doc = addSection(doc, landscape=FALSE)
}


## CHRISTMAS

################################################################################
#===============================================================================
# Ranking of models for total abundance
#===============================================================================
################################################################################

## doc = addPageBreak(doc)

Measure <- NULL

if (exists("n1", mode="environment")) {
      Measure <- c(Measure, as.character(n1$M.naiveone$Measure))
} else if (exists("n3", mode="environment")) {
      Measure <- c(Measure, as.character(n3$M.avgthree$Measure))
} else if (exists("n5", mode="environment")) {
      Measure <- c(Measure, as.character(n5$M.avgfive$Measure))
} else if (exists("ARIMA", mode="environment")) {
      Measure <- c(Measure, as.character(ARIMA$M.arima$Measure))
} else if (exists("EXPSMOOTH", mode="environment")) {
      Measure <- c(Measure, as.character(EXPSMOOTH$M.expsmooth$Measure))
} else if (exists("SIMPLESIBREG", mode="environment")) {
      Measure <- c(Measure, as.character(SIMPLESIBREG$M.sibreg$Measure))
} else if (exists("SIMPLELOGPOWER", mode="environment")) { 
      Measure <- c(Measure, as.character(SIMPLELOGPOWER$M.logpower$Measure))
}

Measure <- unique(Measure)


#===============================================================================

Model <- NULL

f_tt  <- c("Model", Measure, "")

if (exists("n1", mode="environment")) {
    Model <- c(Model, "Naive Model (Previous Year)")
    f_tt <- rbind(f_tt, c("Naive Model (Previous Year)", n1$M.naiveone[,"Total"], " "))
}

if (exists("n3", mode="environment")) {
    Model <- c(Model, "Naive Model (Average of Previous Three Years)")
    f_tt <- rbind(f_tt, c("Naive Model (Average of Previous Three Years)", n3$M.avgthree[,"Total"]," "))
}

if (exists("n5", mode="environment")) {
    Model <- c(Model, "Naive Model (Average of Previous Five Years)")
    f_tt <- rbind(f_tt, c("Naive Model (Average of Previous Five Years)", n5$M.avgfive[,"Total"]," "))
}

if (exists("ARIMA", mode="environment")) {
    Model <- c(Model, "ARIMA Model")
    f_tt <- rbind(f_tt, c("ARIMA Model", ARIMA$M.arima[,"Total"]," "))
}

if (exists("EXPSMOOTH", mode="environment")) {
    Model <- c(Model, "Exponential Smoothing Model")
    f_tt <- rbind(f_tt, c("Exponential Smoothing Model", EXPSMOOTH$M.expsmooth[,"Total"]," "))
}

if (exists("SIMPLESIBREG", mode="environment")) {

    usePackage("scales")
    
    Model <- c(Model, "Sibling Regression Model")
    f_tt <- rbind(f_tt, c("Sibling Regression Model", comma(SIMPLESIBREG$M[,"Total"])," "))
}

if (exists("SIMPLELOGPOWER", mode="environment")) {

    usePackage("scales")
    
    Model <- c(Model, "Log Power Regression Model")
    f_tt <- rbind(f_tt, c("Log Power Regression Model", comma(SIMPLELOGPOWER$M[,"Total"])," "))
}


Model 
f_tt

f_tt[f_tt=="NaN"] <- "NA"
f_tt[f_tt=="-Inf"] <- "NA"
f_tt[f_tt=="Inf"] <- "NA"

#=================================================================================

doc = addSection(doc, landscape=TRUE, ncol=1)

tablecaption <- paste0("Ranking of models used for forecasting the ",
                      forecastingyear, " total ",
                      tolower(stockabundance),
                      " associated with the ",
                      stockname, " ",
                      tolower(stockspecies), " stock. ",
                      "For each measure, models are first ranked with respect to each of the reported ",
                      "retrospective point forecast performance measures, ",
                      "such that the model which achieves the lowest absolute value for that measure ",
                      "receives the smallest rank. ",
                      "For each model, the obtained ranks are then averaged across all performance measures to obtain a ",
                      "model-specific average rank. ",
                      "The best forecasting model is the one with the smallest ", 
                      "model-specific average rank.")


doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")

#=================================================================================

Nmeasures <- length(Measure)

Nmodels <- length(Model)

MyFTable = FlexTable(data = f_tt,
                     body.cell.props = cellProperties( padding = 4, border.color="darkgrey"),  ## MANGO1
                     header.cell.props= cellProperties( padding = 4, border.color="darkgrey"),
                     header.text.props = textBold(), header.columns = FALSE)

MyFTable = addHeaderRow( MyFTable, text.properties = textBold(),
                           value = paste0(Age[i]," component of total ", tolower(stockabundance), " for the ", stockname, " ", stockspecies, " stock"),
                           colspan = ncol(f_tt),
                           cell.properties = cellProperties( padding = 4, background.color="lightgrey", border.color="darkgrey" ))

MyFTable = addHeaderRow( MyFTable, text.properties = textBold(),
                           value = c("Model","Measure", " "),
                           colspan = c(1, Nmeasures, 1),
                           cell.properties = cellProperties(padding = 4, border.color="darkgrey"))


table.rank.rmse.results.total.age <- rank.rmse.results.total.age(table.rmse.results.total.age, pred.args.total, all.pred.args.total)

table.avg.rank.rmse.results.total.age <- cbind.data.frame(table.rank.rmse.results.total.age$ranks, 
                                                                 sprintf("%.2f",table.rank.rmse.results.total.age$avg.ranks))
                                                                 
table.avg.rank.rmse.results.total.age <- cbind.data.frame(rownames(table.avg.rank.rmse.results.total.age),table.avg.rank.rmse.results.total.age)
                                                                 
names(table.avg.rank.rmse.results.total.age)[1] <- "Model"
  
rownames(table.avg.rank.rmse.results.total.age) <- NULL

names(table.avg.rank.rmse.results.total.age)[length(names(table.avg.rank.rmse.results.total.age))] <- 
paste(names(table.avg.rank.rmse.results.total.age)[-c(1,length(names(table.avg.rank.rmse.results.total.age)))], collapse=", ") 

table.avg.rank.rmse.results.total.age <- data.frame(lapply(table.avg.rank.rmse.results.total.age, as.character), stringsAsFactors=FALSE)

names(table.avg.rank.rmse.results.total.age)[length(names(table.avg.rank.rmse.results.total.age))] <- 
str_replace_all(names(table.avg.rank.rmse.results.total.age)[length(names(table.avg.rank.rmse.results.total.age))],"\\..",", ")

if (exists("n1", mode="environment")) {  
  table.avg.rank.rmse.results.total.age$Model  <- str_replace_all(table.avg.rank.rmse.results.total.age$Model, 
                                                                       "naiveone", "Naive Model (Previous Year)")
} 
  
if (exists("n3", mode="environment")) {  
  table.avg.rank.rmse.results.total.age$Model  <- str_replace_all(table.avg.rank.rmse.results.total.age$Model, 
                                                                       "avgthree", "Naive Model (Average of Previous Three Years)") 
}
                
if (exists("n5", mode="environment")) {                                                         
  table.avg.rank.rmse.results.total.age$Model  <- str_replace_all(table.avg.rank.rmse.results.total.age$Model, 
                                                                       "avgfive", "Naive Model (Average of Previous Five Years)") 
}

if (exists("ARIMA", mode="environment")) {  
  table.avg.rank.rmse.results.total.age$Model  <- str_replace_all(table.avg.rank.rmse.results.total.age$Model, 
                                                                       "arima", "ARIMA Model") 
}
  
if (exists("EXPSMOOTH", mode="environment")) {  
  table.avg.rank.rmse.results.total.age$Model  <- str_replace_all(table.avg.rank.rmse.results.total.age$Model, 
                                                                       "expsmooth", "Exponential Smoothing Model") 
}

if (exists("SIMPLESIBREG", mode="environment")) {  
  table.avg.rank.rmse.results.total.age$Model  <- str_replace_all(table.avg.rank.rmse.results.total.age$Model, 
                                                                       "sibreg", "Sibling Regression Model") 
}

if (exists("SIMPLELOGPOWER", mode="environment")) {  
  table.avg.rank.rmse.results.total.age$Model  <- str_replace_all(table.avg.rank.rmse.results.total.age$Model, 
                                                                       "logpower", "Log Power Regression Model") 
}

## WASHINGTON2
  
MyFTable = addFooterRow(MyFTable,  text.properties = textProperties(font.weight = "bold"),
                           value = c("Model","Rank", "Average Rank"),
                           colspan = c(1, Nmeasures, 1),
                           cell.properties = cellProperties(padding = 4, border.color="darkgrey"))

MyFTable = addFooterRow(MyFTable, text.properties = textProperties(font.weight = "normal"), 
                           value = names(table.avg.rank.rmse.results.individual.age),
                           colspan = c(1, rep(1, Nmeasures), 1),
                           cell.properties = cellProperties(padding = 4, border.color="darkgrey"))
                           
for (j in 1:nrow(table.avg.rank.rmse.results.total.age)) {
  
        if (j != table.rank.rmse.results.total.age$index.min.avg.rank) {
        
           MyFTable = addFooterRow(MyFTable, text.properties = textProperties(font.weight = "normal"),
                           value = as.character(table.avg.rank.rmse.results.total.age[j, ]),
                           colspan = c(1, rep(1, Nmeasures), 1),
                           cell.properties = cellProperties(padding = 4, border.color="darkgrey"))
  
        }
        
        if (j == table.rank.rmse.results.total.age$index.min.avg.rank) {
        
            MyFTable = addFooterRow(MyFTable, text.properties = textProperties(font.weight = "normal"),
                           value = as.character(table.avg.rank.rmse.results.individual.age[j, ]),
                           colspan = c(1, rep(1, Nmeasures), 1),
                           cell.properties = cellProperties(padding = 4, border.color="darkgrey",  background.color = "yellow"))
        
        }
        
  
}
                           

## MyFTable = addFooterRow( MyFTable, value = paste0('Note: Ranking across models for a given performance measure is based on the absolute values of that measure.',
##                                                    " The model with the smallest absolute value of the measure receives the smallest rank."),
##                                   colspan = ncol(f_tt),
##                                   text.properties = textProperties(color = "red", font.weight="bold", font.size = 9),
##                                   cell.properties = cellProperties(padding = 4, border.color="darkgrey") )


doc = addFlexTable(doc, MyFTable)

rm(MyFTable)
  
rm(f_tt)
  
rm(Model)

## doc = addSection(doc, landscape=FALSE)

## PENTATONIX

#================================================================================================
#
#  Side-by-side visual comparison of point and interval forecasts for total age
# 
#================================================================================================


## pred.args.total[[k]]$PI.lwr
## pred.args.total[[k]]$PI.upr
## pred.args.total[[k]]$PI.ctr



usePackage("stringr")

PI <- NULL
PI.desc <- NULL
for (k in 1:length(pred.args.total)) {
    PI$PI.ctr <- c( PI$PI.ctr, as.numeric(replacecomma(pred.args.total[[k]]$PI.ctr)) )
    PI$PI.lwr <- c( PI$PI.lwr, as.numeric(replacecomma(pred.args.total[[k]]$PI.lwr)) )
    PI$PI.upr <- c( PI$PI.upr, as.numeric(replacecomma(pred.args.total[[k]]$PI.upr)) )

    PI.desc.temp <- names(pred.args.total)[k]
    PI.desc.temp <- str_replace_all(PI.desc.temp, "pred.int.total.age.", "")
    

    PI.desc <- c(PI.desc, PI.desc.temp)
}



PI.desc <- str_replace(PI.desc, "naiveone", "Naive (1 yr)") 
PI.desc <- str_replace(PI.desc, "avgthree", "Naive (3 yrs)")
PI.desc <- str_replace(PI.desc, "avgfive", "Naive (5 yrs)")  
PI.desc <- str_replace(PI.desc, "arima", "ARIMA")  
PI.desc <- str_replace(PI.desc, "expsmooth", "Exponential Smoothing")

PI <- as.data.frame(PI)

PI <- cbind.data.frame(PI.desc, PI, AvgRank = table.rank.rmse.results.total.age$avg.rank)

usePackage("dplyr")

PI <- arrange(PI, AvgRank)

PI$PI.desc <- factor(PI$PI.desc, levels=PI$PI.desc)


if (length(pred.args.total) > 1) {

  ## doc = addPageBreak(doc)

  ## doc = addSection(doc, landscape=TRUE)

  doc = addSection(doc, landscape=TRUE, ncol=1)
  

	figurecaption <- paste0("Side-by-side comparison of point forecasts and 80% forecast intervals of total ", tolower(stockabundance), " ",
                      "corresponding to the ", 
                      forecastingyear, " forecasting year for the ",
                      stockname, " ", 
                      stockspecies, " stock. ", 
                      "The numbers listed in red at the top of the display represent the model-specific average ranks. ", 
                      "Models were ordered based on their average rank, from the model with the lowest average rank (\"best model\") ", 
                      "to the one with the highest average rank (\"worst model\").")


	myplot <- ggplot(PI, aes(x=PI.desc, y=PI.ctr)) + 
 		     geom_errorbar(aes(ymin=PI.lwr, 
                   ymax=PI.upr), width=0.1, 
                 colour="black") + 
  		     geom_point() + 
                 annotate("text", x = PI$PI.desc, y = PI$PI.ctr, label = comma(round(PI$PI.ctr)), hjust = -0.3, size=3) + 
                 annotate("text", x = PI$PI.desc, y = PI$PI.lwr, label = comma(round(PI$PI.lwr)), hjust = -0.3, size=3) + 
                 annotate("text", x = PI$PI.desc, y = PI$PI.upr, label = comma(round(PI$PI.upr)), hjust = -0.3, size=3) + 
                 annotate("text", x= PI$PI.desc, y = rep(1.05*max(PI$PI.upr), length(PI$PI.upr)), 
                          label=paste0(PI$AvgRank), col="red", size=3) + 
                 scale_y_continuous(paste0("Point Forecast and 80% Forecast Interval of ", stockabundance), labels = scales::comma) + 
                 xlab("Model") + 
                 theme_bw() +
                 theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))
                
         
                 
                 
    doc = addPlot(doc, 
        fun = plot, # print,
        x = myplot,
        width=plotheight+2, height=plotwidth-1)

  doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

  rm(myplot)

  ## doc = addSection(doc, landscape=FALSE)

}




#====================================================================================                      
# Results for the "Best" Forecasting Model for Individual Ages: 
#   Point Forecasts 
#====================================================================================

doc = addSection(doc, landscape=FALSE, ncol=1)

mytitle0 <- ifelse(length(table.rank.rmse.results.total.age$all.models) > 1, 
                   "Produced by the Best Forecasting Models", 
                   "Produced by the User-Selected Forecasting Model")
mytitle <- paste0("Point Forecasts of Age-Specific and Total ", paste0(stockabundance,"s",collapse=""), " ", mytitle0)

doc <- addTitle(doc, mytitle, level = 1)

## LAUREN 1


Age_tmp <- NULL 
Model_tmp <- NULL 
PointForecast_tmp <- NULL 
for (i in 1:length(Age)) {

     cat("i = ",i, "\n")

     table.rank.rmse.results.individual.age <- rank.rmse.results.individual.age(table.rmse.results.individual.age, pred.args, all.pred.args, i)

     Age_tmp <- c(Age_tmp, table.rank.rmse.results.individual.age$age)

     Model_tmp <- c(Model_tmp, table.rank.rmse.results.individual.age$best.model)
     
     if (table.rank.rmse.results.individual.age$best.model=="naiveone") {
     
            PointForecast_tmp <- c(PointForecast_tmp, n1$PI.individual.ages.naiveone[i,"PI.ctr"]) 

     }
     
     if (table.rank.rmse.results.individual.age$best.model=="avgthree") {
     
            PointForecast_tmp <- c(PointForecast_tmp, n3$PI.individual.ages.avgthree[i,"PI.ctr"]) 

     }
     
     if (table.rank.rmse.results.individual.age$best.model=="avgfive") {
     
            PointForecast_tmp <- c(PointForecast_tmp, n5$PI.individual.ages.avgfive[i,"PI.ctr"]) 

     }
   
     if (table.rank.rmse.results.individual.age$best.model=="arima") {
     
            PointForecast_tmp <- c(PointForecast_tmp, ARIMA$PI.individual.ages.arima[i,"PI.ctr"]) 

     }
   
     if (table.rank.rmse.results.individual.age$best.model=="expsmooth") {
     
            PointForecast_tmp <- c(PointForecast_tmp, EXPSMOOTH$PI.individual.ages.expsmooth[i,"PI.ctr"]) 

     }
     
     if (table.rank.rmse.results.individual.age$best.model=="sibreg") {
     
            PointForecast_tmp <- c(PointForecast_tmp, SIMPLESIBREG$PI.individual.ages.sibreg[i,"PI.ctr"]) 

     }
     
     
}

PointForecast_tmp

Age_tmp <- c(Age_tmp, "Total")
Model_tmp <- c(Model_tmp, table.rank.rmse.results.total.age$best.model)

Age_tmp 
Model_tmp

if (table.rank.rmse.results.total.age$best.model=="naiveone") {
     
            PointForecast_tmp <- c(PointForecast_tmp, n1$PI.total.age.naiveone["PI.ctr"]) 

}
     
if (table.rank.rmse.results.total.age$best.model=="avgthree") {
     
            PointForecast_tmp <- c(PointForecast_tmp, n3$PI.total.age.avgthree["PI.ctr"]) 

}
     
if (table.rank.rmse.results.total.age$best.model=="avgfive") {
     
            PointForecast_tmp <- c(PointForecast_tmp, n5$PI.total.age.avgfive["PI.ctr"]) 

}
   
if (table.rank.rmse.results.total.age$best.model=="arima") {
     
            PointForecast_tmp <- c(PointForecast_tmp, ARIMA$PI.total.age.arima["PI.ctr"]) 

}
   
if (table.rank.rmse.results.total.age$best.model=="expsmooth") {
     
            PointForecast_tmp <- c(PointForecast_tmp, EXPSMOOTH$PI.total.age.expsmooth["PI.ctr"]) 

}

if (table.rank.rmse.results.total.age$best.model=="sibreg") {
     
            PointForecast_tmp <- c(PointForecast_tmp, as.character(SIMPLESIBREG$PI.total.age.sibreg$PI.ctr)) 

}



length(Age_tmp)
length(PointForecast_tmp)
length(Model_tmp)

tt_ff <- cbind(Age_tmp, PointForecast_tmp, Model_tmp)
rownames(tt_ff) <- NULL 
tt_ff

tt_ff <- as.data.frame(tt_ff)

tt_ff$Model_tmp <- as.character(tt_ff$Model_tmp)

tt_ff$Model_tmp[tt_ff$Model_tmp == "naiveone"] <- "Naive Model (Previous Year)"
tt_ff$Model_tmp[tt_ff$Model_tmp == "avgthree"] <- "Naive Model (Average of Previous 3 Years)"
tt_ff$Model_tmp[tt_ff$Model_tmp == "avgfive"] <- "Naive Model (Average of Previous 5 Years)"
tt_ff$Model_tmp[tt_ff$Model_tmp == "arima"] <-  "ARIMA Model"
tt_ff$Model_tmp[tt_ff$Model_tmp == "expsmooth"] <- "Exponential Smoothing Model" 
tt_ff$Model_tmp[tt_ff$Model_tmp == "sibreg"] <- "Sibling Regression Model" 

tt_ff

colnames(tt_ff) <- c("Stock Component", "Point Forecast", 
                     ifelse(length(table.rank.rmse.results.total.age) > 1,"Best Forecasting Model","Forecasting Model"))



tablecaption <- paste0("Point forecasts for the ",
                      forecastingyear,
                      " age-specific and total ",
                      paste(tolower(stockabundance),"s",collapse="", sep=""),
                      " associated with the ",
                      stockname, " ",
                      tolower(stockspecies), " stock. ", 
                      "For each stock component, the point forecast was produced by the ", 
                      ifelse(length(table.rank.rmse.results.total.age) > 1,"best forecasting model for that component.",
                             "user-specified forecasting model for that component.")
                      )

tabledata <- tt_ff

doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")

MyFTable = FlexTable(data=tabledata,
                     header.columns = TRUE,
                     add.rownames = FALSE,
                     header.cell.props = cellProperties(padding=7, background.color="lightgrey", border.color="darkgrey"),
                     body.cell.props = cellProperties(padding=7),
                     body.text.props = textProperties(font.weight = "normal",
                                                      font.size = 10,
                                                      font.family = "Calibri"),
                     header.text.props = textProperties(font.weight = "normal",
                                                        font.size = 10,
                                                        font.family = "Calibri"),
                     body.par.props = parProperties(text.align = "right"),
                     header.par.props = parProperties(text.align = "right"))

# applies a border grid on table
MyFTable <- setFlexTableBorders(MyFTable,
                                inner.vertical = borderProperties(color = "darkgray", style = "solid"),
                                inner.horizontal = borderProperties(color = "darkgray", style = "solid"),
                                outer.vertical = borderProperties(color = "darkgray", style = "solid"),
                                outer.horizontal = borderProperties(color = "darkgray", style = "solid")
)


doc = addFlexTable(doc, MyFTable)




## LAUREN 2

## doc = addSection(doc, landscape=TRUE, ncol=1)

for (i in 1:length(Age)) {

     cat("i = ",i, "\n")
     
     
     doc = addSection(doc, landscape=FALSE, ncol=1)

     table.rank.rmse.results.individual.age <- rank.rmse.results.individual.age(table.rmse.results.individual.age, pred.args, all.pred.args, i)

      if (table.rank.rmse.results.individual.age$best.model=="naiveone") {
      
         figurecaption0 <- ifelse(length(table.rank.rmse.results.individual.age$all.models) > 1,
                                  paste0("The point forecast was obtained from the best forecasting model of ", 
                                         tolower(table.rank.rmse.results.individual.age$age), " ",     
                                         tolower(n1$stockabundance), " ", 
                                         "among all considered forecasting models, ", 
                                         "namely the naive model (previous year)."),
                                  "The point forecast was obtained from a naive model (previous year).")
          
         figurecaption <- paste0("Historical values of ", 
                                tolower(table.rank.rmse.results.individual.age$age), " ",     
                                tolower(n1$stockabundance),
                                " and corresponding ",
                                forecastingyear,
                                " point forecast of ",
                                tolower(table.rank.rmse.results.individual.age$age), " ",     
                                tolower(n1$stockabundance),
                                " for the ",
                                paste(n1$stockname,tolower(n1$stockspecies), "stock.", figurecaption0))
     
         n1$pointforecasts <- n1$point.forecast.naiveone(n1$datalist, n1$naiveone.model.fits)
         myplot <- n1$barplot.forecasted.values.individual.ages.naiveone(
                        n1$fits, 
                        n1$pointforecasts,
                        n1$stockabundance, 
                        n1$stockname, 
                        n1$stockspecies,
                        i)
         
         doc = addPlot(doc,
                       fun = plot, # print,
                       x = myplot,
                       width=6.5, height=6)

         doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

         rm(myplot)
         rm(figurecaption0)
         rm(figurecaption)
             
     }

     if (table.rank.rmse.results.individual.age$best.model=="avgthree") {
     
         figurecaption0 <- ifelse(length(table.rank.rmse.results.individual.age$all.models) > 1,
                                  paste0("The point forecast was obtained from the best forecasting model of ", 
                                         tolower(table.rank.rmse.results.individual.age$age), " ",     
                                         tolower(n1$stockabundance), " ", 
                                         "among all considered forecasting models, ", 
                                         "namely the naive model (average of previous 3 years)."),
                                  "The point forecast was obtained from a naive model (average of previous 3 years).")
          
          
         figurecaption <- paste0("Historical values of ", 
                                tolower(table.rank.rmse.results.individual.age$age), " ",     
                                tolower(n3$stockabundance),
                                " and corresponding ",
                                forecastingyear,
                                " point forecast of ",
                                tolower(table.rank.rmse.results.individual.age$age), " ",     
                                tolower(n3$stockabundance),
                                " for the ",
                                paste(n3$stockname,tolower(n3$stockspecies), " stock. ", figurecaption0))
     
         n3$pointforecasts <- n3$point.forecast.avgthree(n3$datalist, n3$avgthree.model.fits)
         myplot <- n3$barplot.forecasted.values.individual.ages.avgthree(
                           n3$fits, 
                           n3$pointforecasts, 
                           n1$stockabundance, 
                           n1$stockname, 
                           n1$stockspecies,
                           i)
         
         doc = addPlot(doc,
                       fun = plot, # print,
                       x = myplot,
                       width=6.5, height=6)

         doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

         rm(myplot)
         rm(figurecaption0)
         rm(figurecaption)
             
     }

     if (table.rank.rmse.results.individual.age$best.model=="avgfive") {
     
         figurecaption0 <- ifelse(length(table.rank.rmse.results.individual.age$all.models) > 1,
                                  paste0("The point forecast was obtained from the best forecasting model of ", 
                                         tolower(table.rank.rmse.results.individual.age$age), " ",     
                                         tolower(n1$stockabundance), " ", 
                                         "among all considered forecasting models, ", 
                                         "namely the naive model (average of previous 5 years)."),
                                  "The point forecast was obtained from a naive model (average of previous 5 years).")
          
          
         figurecaption <- paste0("Historical values of ", 
                                tolower(table.rank.rmse.results.individual.age$age), " ",     
                                tolower(n5$stockabundance),
                                " and corresponding ",
                                forecastingyear,
                                " point forecast of ",
                                tolower(table.rank.rmse.results.individual.age$age), " ",     
                                tolower(n5$stockabundance),
                                " for the ",
                                paste(n5$stockname,tolower(n5$stockspecies), " stock. ", figurecaption0))
     
         n5$pointforecasts <- n5$point.forecast.avgfive(n5$datalist, n5$avgfive.model.fits)
         myplot <- n5$barplot.forecasted.values.individual.ages.avgfive(
                       n5$fits, 
                       n5$pointforecasts, 
                       n1$stockabundance, 
                       n1$stockname, 
                       n1$stockspecies,              
                       i)
         
         doc = addPlot(doc,
                       fun = plot, # print,
                       x = myplot,
                       width=6.5, height=6)

         doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

         rm(myplot)
         rm(figurecaption0)
         rm(figurecaption)
             
     }
     
     if (table.rank.rmse.results.individual.age$best.model=="arima") {
     
         figurecaption0 <- ifelse(length(table.rank.rmse.results.individual.age$all.models) > 1,
                                  paste0("The point forecast was obtained from the best forecasting model of ", 
                                         tolower(table.rank.rmse.results.individual.age$age), " ",     
                                         tolower(n1$stockabundance), " ", 
                                         "among all considered forecasting models, ", 
                                         "namely the ARIMA model."),
                                  "The point forecast was obtained from an ARIMA model.")
           
         figurecaption <- paste0("Historical values of ", 
                                tolower(table.rank.rmse.results.individual.age$age), " ",     
                                tolower(ARIMA$stockabundance),
                                " and corresponding ",
                                forecastingyear,
                                " point forecast of ",
                                tolower(table.rank.rmse.results.individual.age$age), " ",     
                                tolower(ARIMA$stockabundance),
                                " for the ",
                                paste(ARIMA$stockname,tolower(EXPSMOOTH$stockspecies), " stock. ", figurecaption0))
     
         ARIMA$pointforecasts <- ARIMA$point.forecast.arima(ARIMA$datalist, ARIMA$arima.model.fits, ARIMA$boxcoxtransform)
         myplot <- ARIMA$barplot.forecasted.values.individual.ages.arima(
                       ARIMA$arima.model.fits, 
                       ARIMA$boxcoxtransform, 
                       ARIMA$pointforecasts, 
                       n1$stockabundance, 
                       n1$stockname, 
                       n1$stockspecies,
                       i)
         
         doc = addPlot(doc,
                       fun = plot, # print,
                       x = myplot,
                       width=6.5, height=6)

         doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

         rm(myplot)
         rm(figurecaption0)
         rm(figurecaption)
             
     }
     
     if (table.rank.rmse.results.individual.age$best.model=="expsmooth") {
     
         figurecaption0 <- ifelse(length(table.rank.rmse.results.individual.age$all.models) > 1,
                                  paste0("The point forecast was obtained from the best forecasting model of ", 
                                         tolower(table.rank.rmse.results.individual.age$age), " ",     
                                         tolower(n1$stockabundance), " ", 
                                         "among all considered forecasting models, ", 
                                         "namely the exponential smoothing model."),
                                  "The point forecast was obtained from an exponential smoothing model.")
           
         figurecaption <- paste0("Historical values of ", 
                                tolower(table.rank.rmse.results.individual.age$age), " ",     
                                tolower(EXPSMOOTH$stockabundance),
                                " and corresponding ",
                                forecastingyear,
                                " point forecast of ",
                                tolower(table.rank.rmse.results.individual.age$age), " ",     
                                tolower(EXPSMOOTH$stockabundance),
                                " for the ",
                                paste(EXPSMOOTH$stockname,tolower(EXPSMOOTH$stockspecies), " stock. ", figurecaption0))
     
         EXPSMOOTH$pointforecasts <- EXPSMOOTH$point.forecast.expsmooth(EXPSMOOTH$datalist, EXPSMOOTH$expsmooth.model.fits, EXPSMOOTH$boxcoxtransform)
         myplot <- EXPSMOOTH$barplot.forecasted.values.individual.ages.expsmooth(
                      EXPSMOOTH$fits, 
                      EXPSMOOTH$pointforecasts, 
                      n1$stockabundance, 
                      n1$stockname, 
                      n1$stockspecies,
                      i)
         
         doc = addPlot(doc,
                       fun = plot, # print,
                       x = myplot,
                       width=6.5, height=6)

         doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

         rm(myplot)
         rm(figurecaption0)
         rm(figurecaption)
             
     }
     
     
     if (table.rank.rmse.results.individual.age$best.model=="sibreg") {
      
         if ( i == 1) {
         
             tmp_model <- ifelse(SIMPLESIBREG$total.index==1, "avgfive", 
                                 ifelse(SIMPLESIBREG$total.index==2, "arima", "expsmooth"))
         
             tmp_model[tmp_model=="avgfive"] <- "naive model (average of previous 5 years)" 
             
             tmp_model[tmp_model=="arima"] <- "ARIMA model"
             
             tmp_model[tmp_model=="expsmooth"] <- "exponential smoothing model"
         
             figurecaption0 <- ifelse(length(table.rank.rmse.results.individual.age$all.models) > 1,
                                  paste0("The point forecast was obtained from the best forecasting model of ", 
                                         tolower(table.rank.rmse.results.individual.age$age), " ",     
                                         tolower(SIMPLESIBREG$stockabundance), " ", 
                                         "among all considered forecasting models for the youngest age, namely the ", 
                                         tmp_model, 
                                         "."),
                                  "The point forecast was obtained from a sibling regression model without environmental covariates.")
          
            figurecaption <- paste0("Historical values of ", 
                                tolower(table.rank.rmse.results.individual.age$age), " ",     
                                tolower(SIMPLESIBREG$stockabundance),
                                " and corresponding ",
                                forecastingyear,
                                " point forecast of ",
                                tolower(table.rank.rmse.results.individual.age$age), " ",     
                                tolower(SIMPLESIBREG$stockabundance),
                                " for the ",
                                paste(SIMPLESIBREG$stockname,tolower(SIMPLESIBREG$stockspecies), "stock.", figurecaption0))
     
         
             

             myplot <- SIMPLESIBREG$barplot.forecasted.values.youngest.age.complexsib(SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest,
                                                         SIMPLESIBREG$pred.int.individual.ages.arima.youngest,
                                                         SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest,
                                                         SIMPLESIBREG$result.avgfive.youngest,
                                                         SIMPLESIBREG$result.arima.youngest,
                                                         SIMPLESIBREG$result.expsmooth.youngest,
                                                         SIMPLESIBREG$forecastingyear,
                                                         SIMPLESIBREG$total.index,
                                                         SIMPLESIBREG$stockabundance,
                                                         SIMPLESIBREG$stockname,
                                                         SIMPLESIBREG$stockspecies)
                                                         
            doc = addPlot(doc,
                       fun = plot, # print,
                       x = myplot,
                       width=6.5, height=6)

            doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

            rm(myplot)
            rm(figurecaption0)
            rm(figurecaption)

         
         }
         
         #---- older age
         
         if ( i > 1 ) {
         
            figurecaption0 <- ifelse(length(table.rank.rmse.results.individual.age$all.models) > 1,
                                  paste0("The point forecast was obtained from the best forecasting model of ", 
                                         tolower(table.rank.rmse.results.individual.age$age), " ",     
                                         tolower(SIMPLESIBREG$stockabundance), " ", 
                                         "among all considered forecasting models, ", 
                                         "namely the sibling regression model without environmental covariates."),
                                  "The point forecast was obtained from a sibling regression model without environmental covariates.")
          
            figurecaption <- paste0("Historical values of ", 
                                tolower(table.rank.rmse.results.individual.age$age), " ",     
                                tolower(SIMPLESIBREG$stockabundance),
                                " and corresponding ",
                                forecastingyear,
                                " point forecast of ",
                                tolower(table.rank.rmse.results.individual.age$age), " ",     
                                tolower(SIMPLESIBREG$stockabundance),
                                " for the ",
                                paste(SIMPLESIBREG$stockname,tolower(SIMPLESIBREG$stockspecies), "stock.", figurecaption0))
     
            myplot <- SIMPLESIBREG$barplot.forecasted.values.individual.ages.complexsib(
                                 SIMPLESIBREG$point_forecast_best_model_for_each_age_class,
                                 SIMPLESIBREG$forecastingyear,
                                 SIMPLESIBREG$stockabundance,
                                 SIMPLESIBREG$stockname,
                                 SIMPLESIBREG$stockspecies, 
                                 i)
                                                            
            doc = addPlot(doc,
                       fun = plot, # print,
                       x = myplot,
                       width=6.5, height=6)

            doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

            rm(myplot)
            rm(figurecaption0)
            rm(figurecaption)
             
         } 
             
     }

}



## ELAINE


#====================================================================================                      
# Results for the "Best" Forecasting Model for Total Age: 
#   Point Forecasts 
#====================================================================================

doc = addSection(doc, landscape=FALSE, ncol=1)

## mytitle0 <- ifelse(length(table.rank.rmse.results.total.age$all.models) > 1, 
##                   "Produced by the Best Forecasting Model", 
##                   "Produced by the User-Selected Forecasting Model")
## mytitle <- paste0("Point Forecasts of Total ", stockabundance, " ", mytitle0)
## 
## doc <- addTitle(doc, mytitle, level = 1)

if (table.rank.rmse.results.total.age$best.model=="naiveone") {

    figurecaption0 <- ifelse(length(table.rank.rmse.results.total.age$all.models) > 1,
                                  paste0("This model was identified as the best forecasting model among all considered models for total ",
                                         tolower(n1$stockabundance), "."), 
                             "") 

    figurecaption <- paste0("Historical total ",
                       tolower(n1$stockabundance),
                       " values",
                       " and corresponding ",
                       max(n1$results.afe.total.age.retro.naiveone$CY)+1,
                       " point forecast ",
                       "for the ",
                       paste(n1$stockname,tolower(n1$stockspecies), "stock."),
                       " The point forecast of total ",
                       tolower(n1$stockabundance),
                       " was obtained by totaling the point forecasts of age-specific ",
                       paste0(tolower(n1$stockabundance),"s",collapse=" "),
                       " produced by ",
                       "the naive model (previous year). ", 
                       figurecaption0, 
                       sep="")
    
    n1$pointforecasts <- n1$point.forecast.naiveone(n1$datalist, n1$naiveone.model.fits)

    myplot <- n1$barplot.forecasted.values.total.age.naiveone(n1$results.total.age.retro.predictive.performance.naiveone,
                                                              n1$pointforecasts,
                                                              n1$stockabundance, 
                                                              n1$stockname, 
                                                              n1$stockspecies)

    
    doc = addPlot(doc,
              fun = plot, # print,
              x = myplot,
              width=6.5, height=6)


    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)
    rm(figurecaption0)
    rm(figurecaption)

}



if (table.rank.rmse.results.total.age$best.model=="avgthree") {

    figurecaption0 <- ifelse(length(table.rank.rmse.results.total.age$all.models) > 1,
                                  paste0("This model was identified as the best forecasting model among all considered models for total ",
                                         tolower(n3$stockabundance), "."), 
                             "") 

    figurecaption <- paste0("Historical total ",
                       tolower(n3$stockabundance),
                       " values",
                       " and corresponding ",
                       max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                       " point forecast ",
                       "for the ",
                       paste(n3$stockname,tolower(n3$stockspecies), "stock."),
                       " The point forecast of total ",
                       tolower(n3$stockabundance),
                       " was obtained by totaling the point forecasts of age-specific ",
                       paste0(tolower(n3$stockabundance),"s",collapse=" "),
                       " produced by ",
                       "the naive model (average of previous 3 years). ", 
                       figurecaption0, 
                       sep="")
    
    n3$pointforecasts <- n3$point.forecast.avgthree(n3$datalist, n3$avgthree.model.fits)

    myplot <- n3$barplot.forecasted.values.total.age.avgthree(n3$results.total.age.retro.predictive.performance.avgthree,
                                                              n3$pointforecasts,
                                                              n3$stockabundance, 
                                                              n3$stockname, 
                                                              n3$stockspecies)

    
    doc = addPlot(doc,
              fun = plot, # print,
              x = myplot,
              width=6.5, height=6)


    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)
    rm(figurecaption0)
    rm(figurecaption)

}


if (table.rank.rmse.results.total.age$best.model=="avgfive") {

    
    figurecaption0 <- ifelse(length(table.rank.rmse.results.total.age$all.models) > 1,
                                  paste0("This model was identified as the best forecasting model among all considered models for total ",
                                         tolower(n5$stockabundance), "."), 
                             "") 

    figurecaption <- paste0("Historical total ",
                       tolower(n5$stockabundance),
                       " values",
                       " and corresponding ",
                       max(n5$results.afe.total.age.retro.avgfive$CY)+1,
                       " point forecast ",
                       "for the ",
                       paste(n5$stockname,tolower(n5$stockspecies), "stock."),
                       " The point forecast of total ",
                       tolower(n5$stockabundance),
                       " was obtained by totaling the point forecasts of age-specific ",
                       paste0(tolower(n5$stockabundance),"s",collapse=" "),
                       " produced by ",
                       "the naive model (average of previous 5 years). ", 
                       figurecaption0, 
                       sep="")
    
    n5$pointforecasts <- n5$point.forecast.avgfive(n5$datalist, n5$avgfive.model.fits)

    myplot <- n5$barplot.forecasted.values.total.age.avgfive(n5$results.total.age.retro.predictive.performance.avgfive,
                                                             n5$pointforecasts,
                                                             n5$stockabundance, 
                                                             n5$stockname, 
                                                             n5$stockspecies)

    
    doc = addPlot(doc,
              fun = plot, # print,
              x = myplot,
              width=6.5, height=6)


    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)
    rm(figurecaption0)
    rm(figurecaption)

}



if (table.rank.rmse.results.total.age$best.model=="arima") {


    figurecaption0 <- ifelse(length(table.rank.rmse.results.total.age$all.models) > 1,
                                  paste0("This model was identified as the best forecasting model among all considered models for total ",
                                         tolower(ARIMA$stockabundance), "."), 
                             "") 

    figurecaption <- paste0("Historical total ",
                       tolower(ARIMA$stockabundance),
                       " values",
                       " and corresponding ",
                       max(ARIMA$results.afe.total.age.retro.arima$CY)+1,
                       " point forecast ",
                       "for the ",
                       paste(ARIMA$stockname,tolower(ARIMA$stockspecies), "stock."),
                       " The point forecast of total ",
                       tolower(ARIMA$stockabundance),
                       " was obtained by totaling the point forecasts of age-specific ",
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       " produced by ",
                       "the ARIMA model. ", 
                       figurecaption0, 
                       sep="")

    ARIMA$pointforecasts <- ARIMA$point.forecast.arima(ARIMA$datalist, ARIMA$arima.model.fits, ARIMA$boxcoxtransform)

    myplot <- ARIMA$barplot.forecasted.values.total.age.arima(ARIMA$results.total.age.retro.predictive.performance.arima,
                                                        ARIMA$pointforecasts,
                                                        ARIMA$stockabundance, 
                                                        ARIMA$stockname, 
                                                        ARIMA$stockspecies)

    
    doc = addPlot(doc,
              fun = plot, # print,
              x = myplot,
              width=6.5, height=6)


    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)
    rm(figurecaption)

}



if (table.rank.rmse.results.total.age$best.model=="expsmooth") {

    
    figurecaption0 <- ifelse(length(table.rank.rmse.results.total.age$all.models) > 1,
                                  paste0("This model was identified as the best forecasting model among all considered models for total ",
                                         tolower(EXPSMOOTH$stockabundance), "."), 
                             "") 

    figurecaption <- paste0("Historical total ",
                       tolower(EXPSMOOTH$stockabundance),
                       " values",
                       " and corresponding ",
                       max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                       " point forecast ",
                       "for the ",
                       paste(EXPSMOOTH$stockname,tolower(EXPSMOOTH$stockspecies), "stock."),
                       " The point forecast of total ",
                       tolower(EXPSMOOTH$stockabundance),
                       " was obtained by totaling the point forecasts of age-specific ",
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       " produced by ",
                       "the exponential smoothing model. ", 
                       figurecaption0, 
                       sep="")

    EXPSMOOTH$pointforecasts <- EXPSMOOTH$point.forecast.expsmooth(EXPSMOOTH$datalist, EXPSMOOTH$expsmooth.model.fits, EXPSMOOTH$boxcoxtransform)

    myplot <- EXPSMOOTH$barplot.forecasted.values.total.age.expsmooth(EXPSMOOTH$results.total.age.retro.predictive.performance.expsmooth,
                                                        EXPSMOOTH$pointforecasts,
                                                        EXPSMOOTH$stockabundance, 
                                                        EXPSMOOTH$stockname, 
                                                        EXPSMOOTH$stockspecies)

    
    doc = addPlot(doc,
              fun = plot, # print,
              x = myplot,
              width=6.5, height=6)


    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)
    rm(figurecaption0)
    rm(figurecaption)

}

## DAIGLE 

#====================================================================================                      
# Results for the "Best" Forecasting Model for Individual Ages: 
#   Interval Forecasts 
#====================================================================================


doc = addSection(doc, landscape=FALSE, ncol=1)

mytitle0 <- ifelse(length(table.rank.rmse.results.total.age$all.models) > 1, 
                   "Produced by the Best Forecasting Models", 
                   "Produced by the User-Selected Forecasting Model")
mytitle <- paste0("Interval Forecasts of Age-Specific and Total ", paste0(stockabundance,"s",collapse=""), " ", mytitle0)

doc <- addTitle(doc, mytitle, level = 1)

## LAUREN 3

Age_tmp <- NULL 
Model_tmp <- NULL 
PointForecast_tmp <- NULL 
IntervalForecast_tmp <- NULL 
for (i in 1:length(Age)) {

     cat("i = ",i, "\n")

     table.rank.rmse.results.individual.age <- rank.rmse.results.individual.age(table.rmse.results.individual.age, pred.args, all.pred.args, i)

     Age_tmp <- c(Age_tmp, table.rank.rmse.results.individual.age$age)

     Model_tmp <- c(Model_tmp, table.rank.rmse.results.individual.age$best.model)
     
     if (table.rank.rmse.results.individual.age$best.model=="naiveone") {
     
            PointForecast_tmp <- c(PointForecast_tmp, n1$PI.individual.ages.naiveone[i,"PI.ctr"]) 
            
            IntervalForecast_tmp <- c(IntervalForecast_tmp, paste0(n1$PI.individual.ages.naiveone[i,"PI.lwr"]," - ", n1$PI.individual.ages.naiveone[i,"PI.upr"])) 

     }
     
     if (table.rank.rmse.results.individual.age$best.model=="avgthree") {
     
            PointForecast_tmp <- c(PointForecast_tmp, n3$PI.individual.ages.avgthree[i,"PI.ctr"]) 
            
            IntervalForecast_tmp <- c(IntervalForecast_tmp, paste0(n3$PI.individual.ages.avgthree[i,"PI.lwr"]," - ", n3$PI.individual.ages.avgthree[i,"PI.upr"])) 

     }
     
     if (table.rank.rmse.results.individual.age$best.model=="avgfive") {
     
            PointForecast_tmp <- c(PointForecast_tmp, n5$PI.individual.ages.avgfive[i,"PI.ctr"]) 
            
            IntervalForecast_tmp <- c(IntervalForecast_tmp, paste0(n5$PI.individual.ages.avgfive[i,"PI.lwr"]," - ", n5$PI.individual.ages.avgfive[i,"PI.upr"])) 

     }
   
     if (table.rank.rmse.results.individual.age$best.model=="arima") {
     
            PointForecast_tmp <- c(PointForecast_tmp, ARIMA$PI.individual.ages.arima[i,"PI.ctr"]) 
            
            IntervalForecast_tmp <- c(IntervalForecast_tmp, paste0(ARIMA$PI.individual.ages.arima[i,"PI.lwr"]," - ", ARIMA$PI.individual.ages.arima[i,"PI.upr"])) 

     }
   
     if (table.rank.rmse.results.individual.age$best.model=="expsmooth") {
     
            PointForecast_tmp <- c(PointForecast_tmp, EXPSMOOTH$PI.individual.ages.expsmooth[i,"PI.ctr"]) 
            
            IntervalForecast_tmp <- c(IntervalForecast_tmp, paste0(EXPSMOOTH$PI.individual.ages.expsmooth[i,"PI.lwr"]," - ", EXPSMOOTH$PI.individual.ages.expsmooth[i,"PI.upr"])) 

     }
     
     
}


Age_tmp <- c(Age_tmp, "Total")
Model_tmp <- c(Model_tmp, table.rank.rmse.results.total.age$best.model)

Age_tmp 
Model_tmp

if (table.rank.rmse.results.total.age$best.model=="naiveone") {
     
            PointForecast_tmp <- c(PointForecast_tmp, n1$PI.total.age.naiveone["PI.ctr"]) 
            
            IntervalForecast_tmp <- c(IntervalForecast_tmp, paste0(n1$PI.total.age.naiveone["PI.lwr"]," - ",n1$PI.total.age.naiveone["PI.upr"]))

}
     
if (table.rank.rmse.results.total.age$best.model=="avgthree") {
     
            PointForecast_tmp <- c(PointForecast_tmp, n3$PI.total.age.avgthree["PI.ctr"]) 
            
            IntervalForecast_tmp <- c(IntervalForecast_tmp, paste0(n3$PI.total.age.avgthree["PI.lwr"]," - ",n3$PI.total.age.avgthree["PI.upr"]))


}
     
if (table.rank.rmse.results.total.age$best.model=="avgfive") {
     
            PointForecast_tmp <- c(PointForecast_tmp, n5$PI.total.age.avgfive["PI.ctr"]) 
            
            IntervalForecast_tmp <- c(IntervalForecast_tmp, paste0(n5$PI.total.age.avgfive["PI.lwr"]," - ",n5$PI.total.age.avgfive["PI.upr"]))


}
   
if (table.rank.rmse.results.total.age$best.model=="arima") {
     
            PointForecast_tmp <- c(PointForecast_tmp, ARIMA$PI.total.age.arima["PI.ctr"]) 
            
            IntervalForecast_tmp <- c(IntervalForecast_tmp, paste0(ARIMA$PI.total.age.arima["PI.lwr"]," - ",ARIMA$PI.total.age.arima["PI.upr"]))


}
   
if (table.rank.rmse.results.total.age$best.model=="expsmooth") {
     
            PointForecast_tmp <- c(PointForecast_tmp, EXPSMOOTH$PI.total.age.expsmooth["PI.ctr"]) 
            
            IntervalForecast_tmp <- c(IntervalForecast_tmp, paste0(EXPSMOOTH$PI.total.age.expsmooth["PI.lwr"]," - ",EXPSMOOTH$PI.total.age.expsmooth["PI.upr"]))


}

length(Age_tmp)
length(PointForecast_tmp)
length(Model_tmp)

tt_ff <- cbind(Age_tmp, PointForecast_tmp, IntervalForecast_tmp, Model_tmp)
rownames(tt_ff) <- NULL 
tt_ff

tt_ff <- as.data.frame(tt_ff)

tt_ff$Model_tmp <- as.character(tt_ff$Model_tmp)

tt_ff$Model_tmp[tt_ff$Model_tmp == "naiveone"] <- "Naive Model (Previous Year)"
tt_ff$Model_tmp[tt_ff$Model_tmp == "avgthree"] <- "Naive Model (Average of Previous 3 Years)"
tt_ff$Model_tmp[tt_ff$Model_tmp == "avgfive"] <- "Naive Model (Average of Previous 5 Years)"
tt_ff$Model_tmp[tt_ff$Model_tmp == "arima"] <-  "ARIMA Model"
tt_ff$Model_tmp[tt_ff$Model_tmp == "expsmooth"] <- "Exponential Smoothing Model" 
tt_ff$Model_tmp[tt_ff$Model_tmp == "sibreg"] <- "Sibling Regression Model" 

tt_ff

colnames(tt_ff) <- c("Stock Component", "Point Forecast", "80% Interval Forecast", 
                     ifelse(length(table.rank.rmse.results.total.age) > 1,"Best Forecasting Model","Forecasting Model"))



tablecaption <- paste0("Point and interval forecasts for the ",
                      forecastingyear,
                      " age-specific and total ",
                      paste(tolower(stockabundance),"s",collapse="", sep=""),
                      " associated with the ",
                      stockname, " ",
                      tolower(stockspecies), " stock. ", 
                      "For each stock component, the point and interval forecasts were produced by the ", 
                      ifelse(length(table.rank.rmse.results.total.age) > 1,"best forecasting model for that component.",
                             "user-specified forecasting model for that component.")
                      )

tabledata <- tt_ff

doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")

MyFTable = FlexTable(data=tabledata,
                     header.columns = TRUE,
                     add.rownames = FALSE,
                     header.cell.props = cellProperties(padding=7, background.color="lightgrey", border.color="darkgrey"),
                     body.cell.props = cellProperties(padding=7),
                     body.text.props = textProperties(font.weight = "normal",
                                                      font.size = 10,
                                                      font.family = "Calibri"),
                     header.text.props = textProperties(font.weight = "normal",
                                                        font.size = 10,
                                                        font.family = "Calibri"),
                     body.par.props = parProperties(text.align = "right"),
                     header.par.props = parProperties(text.align = "right"))

# applies a border grid on table
MyFTable <- setFlexTableBorders(MyFTable,
                                inner.vertical = borderProperties(color = "darkgray", style = "solid"),
                                inner.horizontal = borderProperties(color = "darkgray", style = "solid"),
                                outer.vertical = borderProperties(color = "darkgray", style = "solid"),
                                outer.horizontal = borderProperties(color = "darkgray", style = "solid")
)


doc = addFlexTable(doc, MyFTable)


## RAINBOW


#-----------------------------------------------------------------------------------------
# naiveone: Visual representation of interval forecast of age-specific abundance (age-specific, naiveone)
#-----------------------------------------------------------------------------------------

for (i in 1:length(Age)){

    doc = addSection(doc, landscape=FALSE, ncol=1)

     cat("i = ",i, "\n")

     table.rank.rmse.results.individual.age <- rank.rmse.results.individual.age(table.rmse.results.individual.age, pred.args, all.pred.args, i)

     #--- naiveone ---
     
     if (table.rank.rmse.results.individual.age$best.model=="naiveone") {
     
          figurecaption0 <- ifelse(length(table.rank.rmse.results.individual.age$all.models) > 1,
                                  paste0("This model was identified as the best forecasting model among all considered models for ",
                                         tolower(table.rank.rmse.results.individual.age$age), " ",  
                                         tolower(n1$stockabundance), "."), 
                             "") 

          figurecaption <- paste0("Historical values of ", 
                                  tolower(table.rank.rmse.results.individual.age$age), " ",  
                                  tolower(n1$stockabundance),
                                  " along with the corresponding ",
                                  max(n1$results.afe.total.age.retro.naiveone$CY)+1,
                                  " point forecast and 80% interval forecast ",
                                  "for the ",
                                  paste(n1$stockname,tolower(n1$stockspecies), "stock."),
                                  " The point and interval forecasts of total ",
                                  tolower(n1$stockabundance),
                                  " were produced by ",
                                  "the naive model (previous year). ", 
                                  figurecaption0, 
                                  sep="")
                              
         n1$pointforecasts <- n1$point.forecast.naiveone(n1$datalist, n1$naiveone.model.fits)
                              
         myplot <- n1$scatterplot.forecasted.values.and.forecast.intervals.individual.ages.naiveone(
                                n1$naiveone.model.fits,
                                n1$pointforecasts,
                                n1$PI.individual.ages.naiveone.no.comma,
                                n1$stockabundance,
                                n1$stockname,
                                n1$stockspecies,
                                i)
                                
          doc = addPlot(doc,
              fun = plot, # print,
              x = myplot,
              width=6.5, height=6)


          doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

          rm(myplot)
          rm(figurecaption0)
          rm(figurecaption)

     }
     
     
     #--- avgthree ---

     if (table.rank.rmse.results.individual.age$best.model=="avgthree") {
     
          figurecaption0 <- ifelse(length(table.rank.rmse.results.individual.age$all.models) > 1,
                                  paste0("This model was identified as the best forecasting model among all considered models for ",
                                         tolower(table.rank.rmse.results.individual.age$age), " ", 
                                         tolower(n3$stockabundance), "."), 
                             "") 

          figurecaption <- paste0("Historical values of ", 
                                  tolower(table.rank.rmse.results.individual.age$age), " ",  
                                  tolower(n3$stockabundance),
                                  " along with the corresponding ",
                                  max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                                  " point forecast and 80% interval forecast ",
                                  "for the ",
                                  paste(n3$stockname,tolower(n3$stockspecies), "stock."),
                                  " The point and interval forecasts of total ",
                                  tolower(n3$stockabundance),
                                  " were produced by ",
                                  "the naive model (average of previous 3 years). ", 
                                  figurecaption0, 
                                  sep="")
                              
         n3$pointforecasts <- n3$point.forecast.avgthree(n3$datalist, n3$avgthree.model.fits)
                              
         myplot <- n3$scatterplot.forecasted.values.and.forecast.intervals.individual.ages.avgthree(
                                n3$avgthree.model.fits,
                                n3$pointforecasts,
                                n3$PI.individual.ages.avgthree.no.comma,
                                n3$stockabundance,
                                n3$stockname,
                                n3$stockspecies,
                                i)
                                
          doc = addPlot(doc,
              fun = plot, # print,
              x = myplot,
              width=6.5, height=6)


          doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

          rm(myplot)
          rm(figurecaption0)
          rm(figurecaption)

     }
     

     #--- avgfive ----

     if (table.rank.rmse.results.individual.age$best.model=="avgthree") {
     
          figurecaption0 <- ifelse(length(table.rank.rmse.results.individual.age$all.models) > 1,
                                  paste0("This model was identified as the best forecasting model among all considered models for ",
                                         tolower(table.rank.rmse.results.individual.age$age), " ", 
                                         tolower(n5$stockabundance), "."), 
                             "") 

          figurecaption <- paste0("Historical values of ", 
                                  tolower(table.rank.rmse.results.individual.age$age), " ",  
                                  tolower(n5$stockabundance),
                                  " along with the corresponding ",
                                  max(n5$results.afe.total.age.retro.avgfive$CY)+1,
                                  " point forecast and 80% interval forecast ",
                                  "for the ",
                                  paste(n5$stockname,tolower(n5$stockspecies), "stock."),
                                  " The point and interval forecasts of total ",
                                  tolower(n5$stockabundance),
                                  " were produced by ",
                                  "the naive model (average of previous 5 years). ", 
                                  figurecaption0, 
                                  sep="")
                              
         n5$pointforecasts <- n5$point.forecast.expsmooth(n5$datalist, n5$expsmooth.model.fits)
                              
         myplot <- n5$scatterplot.forecasted.values.and.forecast.intervals.individual.ages.avgfive(
                                n5$avgfive.model.fits,
                                n5$pointforecasts,
                                n5$PI.individual.ages.avgfive.no.comma,
                                n5$stockabundance,
                                n5$stockname,
                                n5$stockspecies,
                                i)
                                
          doc = addPlot(doc,
              fun = plot, # print,
              x = myplot,
              width=6.5, height=6)


          doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

          rm(myplot)
          rm(figurecaption0)
          rm(figurecaption)

     }
     

     #--- arima -------

     if (table.rank.rmse.results.individual.age$best.model=="arima") {
     
          figurecaption0 <- ifelse(length(table.rank.rmse.results.individual.age$all.models) > 1,
                                  paste0("This model was identified as the best forecasting model among all considered models for ",
                                         tolower(table.rank.rmse.results.individual.age$age), " ", 
                                         tolower(ARIMA$stockabundance), "."), 
                             "") 

          figurecaption <- paste0("Historical values of ", 
                                  tolower(table.rank.rmse.results.individual.age$age), " ",  
                                  tolower(ARIMA$stockabundance),
                                  " along with the corresponding ",
                                  max(ARIMA$results.afe.total.age.retro.arima$CY)+1,
                                  " point forecast and 80% interval forecast ",
                                  "for the ",
                                  paste(ARIMA$stockname,tolower(ARIMA$stockspecies), "stock."),
                                  " The point and interval forecasts of total ",
                                  tolower(ARIMA$stockabundance),
                                  " were produced by ",
                                  "the ARIMA model. ", 
                                  figurecaption0, 
                                  sep="")
                              
         ARIMA$pointforecasts <- ARIMA$point.forecast.arima(ARIMA$datalist, ARIMA$arima.model.fits, ARIMA$boxcoxtransform)
                              
         myplot <- ARIMA$scatterplot.forecasted.values.and.forecast.intervals.individual.ages.arima(
                                fits = ARIMA$arima.model.fits,
                                boxcoxtransform = ARIMA$boxcoxtransform, 
                                pointforecasts = ARIMA$pointforecasts,
                                intervalforecasts = ARIMA$PI.individual.ages.arima.no.comma,
                                stockabundance = ARIMA$stockabundance,
                                stockname = ARIMA$stockname,
                                stockspecies = ARIMA$stockspecies,
                                i)
                                
          doc = addPlot(doc,
              fun = plot, # print,
              x = myplot,
              width=6.5, height=6)


          doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

          rm(myplot)
          rm(figurecaption0)
          rm(figurecaption)

     }
     
     #--- expsmooth --- 

     if (table.rank.rmse.results.individual.age$best.model=="expsmooth") {
     
          figurecaption0 <- ifelse(length(table.rank.rmse.results.individual.age$all.models) > 1,
                                  paste0("This model was identified as the best forecasting model among all considered models for ",
                                         tolower(table.rank.rmse.results.individual.age$age), " ", 
                                         tolower(EXPSMOOTH$stockabundance), "."), 
                             "") 

          figurecaption <- paste0("Historical values of ", 
                                  tolower(table.rank.rmse.results.individual.age$age), " ",  
                                  tolower(EXPSMOOTH$stockabundance),
                                  " along with the corresponding ",
                                  max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                                  " point forecast and 80% interval forecast ",
                                  "for the ",
                                  paste(EXPSMOOTH$stockname,tolower(EXPSMOOTH$stockspecies), "stock."),
                                  " The point and interval forecasts of total ",
                                  tolower(EXPSMOOTH$stockabundance),
                                  " were produced by ",
                                  "the exponential smoothing model. ", 
                                  figurecaption0, 
                                  sep="")
                              
        
         #---
         
         ## graphics.off()  # POLAR 
         
         EXPSMOOTH$pointforecasts <- EXPSMOOTH$point.forecast.expsmooth(EXPSMOOTH$datalist, EXPSMOOTH$expsmooth.model.fits, EXPSMOOTH$boxcoxtransform)

         myplot <- EXPSMOOTH$scatterplot.forecasted.values.and.forecast.intervals.individual.ages.expsmooth(
                       fits = EXPSMOOTH$expsmooth.model.fits,
                       boxcoxtransform = EXPSMOOTH$boxcoxtransform,
                       pointforecasts = EXPSMOOTH$pointforecasts,
                       intervalforecasts = EXPSMOOTH$PI.individual.ages.expsmooth.no.comma,
                       stockabundance = EXPSMOOTH$stockabundance,
                       stockname = EXPSMOOTH$stockname,
                       stockspecies = EXPSMOOTH$stockspecies,
                       i)
    
          doc = addPlot(doc,
              fun = plot, # print,
              x = myplot,
              width=6.5, height=6)


          doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

          rm(myplot)
          rm(figurecaption0)
          rm(figurecaption)

     }
     

}


## BOOGIE

#-----------------------------------------------------------------------------------------
# naiveone: Visual representation of interval forecast of total abundance (total age, naiveone)
#-----------------------------------------------------------------------------------------

doc = addSection(doc, landscape=FALSE, ncol=1)

if (table.rank.rmse.results.total.age$best.model=="naiveone") {

    
    figurecaption0 <- ifelse(length(table.rank.rmse.results.total.age$all.models) > 1,
                                  paste0("This model was identified as the best forecasting model among all considered models for total ",
                                         tolower(n1$stockabundance), "."), 
                             "") 

    figurecaption <- paste0("Historical total ",
                       tolower(n1$stockabundance),
                       " values",
                       " along with the corresponding ",
                       max(n1$results.afe.total.age.retro.naiveone$CY)+1,
                       " point forecast and 80% interval forecast ",
                       "for the ",
                       paste(n1$stockname,tolower(n1$stockspecies), "stock."),
                       " The point and interval forecasts of total ",
                       tolower(n1$stockabundance),
                       " were produced by ",
                       "the naive model (previous year). ", 
                       figurecaption0, 
                       sep="")

    n1$pointforecasts <- n1$point.forecast.naiveone(n1$datalist, n1$naiveone.model.fits)

    ## graphics.off() 

    myplot <- n1$scatterplot.forecasted.values.and.forecast.intervals.total.age.naiveone(
                     n1$results.total.age.retro.predictive.performance.naiveone,
                     n1$pointforecasts,
                     n1$PI.total.age.naiveone, 
                     n1$stockabundance,
                     n1$stockname,
                     n1$stockspecies)

    doc = addPlot(doc,
              fun = plot, # print,
              x = myplot,
              width=6.5, height=6)


    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)
    rm(figurecaption0)
    rm(figurecaption)

}


#-----------------------------------------------------------------------------------------
# avgthree: Visual representation of interval forecast of total abundance (total age, avgthree)
#-----------------------------------------------------------------------------------------

if (table.rank.rmse.results.total.age$best.model=="avgthree") {

    
    figurecaption0 <- ifelse(length(table.rank.rmse.results.total.age$all.models) > 1,
                                  paste0("This model was identified as the best forecasting model among all considered models for total ",
                                         tolower(n3$stockabundance), "."), 
                             "") 

    figurecaption <- paste0("Historical total ",
                       tolower(n3$stockabundance),
                       " values",
                       " along with the corresponding ",
                       max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                       " point forecast and 80% interval forecast ",
                       "for the ",
                       paste(n3$stockname,tolower(n3$stockspecies), "stock."),
                       " The point and interval forecasts of total ",
                       tolower(n3$stockabundance),
                       " were produced by ",
                       "the naive model (average of previous 3 years). ", 
                       figurecaption0, 
                       sep="")

    n3$pointforecasts <- n3$point.forecast.avgthree(n3$datalist, n3$avgthree.model.fits)

    ## graphics.off() 

    myplot <- n3$scatterplot.forecasted.values.and.forecast.intervals.total.age.avgthree(
                     n3$results.total.age.retro.predictive.performance.avgthree,
                     n3$pointforecasts,
                     n3$PI.total.age.avgthree,
                     n3$stockabundance,
                     n3$stockname,
                     n3$stockspecies)

    doc = addPlot(doc,
              fun = plot, # print,
              x = myplot,
              width=6.5, height=6)


    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)
    rm(figurecaption0)
    rm(figurecaption)

}



#-----------------------------------------------------------------------------------------
# avgfive: Visual representation of interval forecast of total abundance (total age, avgfive)
#-----------------------------------------------------------------------------------------

if (table.rank.rmse.results.total.age$best.model=="avgfive") {

    
    figurecaption0 <- ifelse(length(table.rank.rmse.results.total.age$all.models) > 1,
                                  paste0("This model was identified as the best forecasting model among all considered models for total ",
                                         tolower(n5$stockabundance), "."), 
                             "") 

    figurecaption <- paste0("Historical total ",
                       tolower(n5$stockabundance),
                       " values",
                       " along with the corresponding ",
                       max(n5$results.afe.total.age.retro.avgfive$CY)+1,
                       " point forecast and 80% interval forecast ",
                       "for the ",
                       paste(n5$stockname,tolower(n5$stockspecies), "stock."),
                       " The point and interval forecasts of total ",
                       tolower(n5$stockabundance),
                       " were produced by ",
                       "the naive model (average of previous 5 years). ", 
                       figurecaption0, 
                       sep="")

    n5$pointforecasts <- n5$point.forecast.avgfive(n5$datalist, n5$avgfive.model.fits)

    ## graphics.off() 

    myplot <- n5$scatterplot.forecasted.values.and.forecast.intervals.total.age.avgfive(
                     n5$results.total.age.retro.predictive.performance.avgfive,
                     n5$pointforecasts,
                     n5$PI.total.age.avgfive, 
                     n5$stockabundance,
                     n5$stockname,
                     n5$stockspecies)

    doc = addPlot(doc,
              fun = plot, # print,
              x = myplot,
              width=6.5, height=6)


    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)
    rm(figurecaption0)
    rm(figurecaption)

}


#-----------------------------------------------------------------------------------------
# arima: Visual representation of interval forecast of total abundance (total age, arima)
#-----------------------------------------------------------------------------------------
                                                                                        
if (table.rank.rmse.results.total.age$best.model=="arima") {

    
    figurecaption0 <- ifelse(length(table.rank.rmse.results.total.age$all.models) > 1,
                                  paste0("This model was identified as the best forecasting model among all considered models for total ",
                                         tolower(ARIMA$stockabundance), "."), 
                             "") 

    figurecaption <- paste0("Historical total ",
                       tolower(ARIMA$stockabundance),
                       " values",
                       " along with the corresponding ",
                       max(ARIMA$results.afe.total.age.retro.arima$CY)+1,
                       " point forecast and 80% interval forecast ",
                       "for the ",
                       paste(ARIMA$stockname,tolower(ARIMA$stockspecies), "stock."),
                       " The point and interval forecasts of total ",
                       tolower(ARIMA$stockabundance),
                       " were produced by ",
                       "the ARIMA model. ", 
                       figurecaption0, 
                       sep="")

    ARIMA$pointforecasts <- ARIMA$point.forecast.arima(ARIMA$datalist, ARIMA$arima.model.fits, ARIMA$boxcoxtransform)

    ## graphics.off() 

    myplot <- ARIMA$scatterplot.forecasted.values.and.forecast.intervals.total.age.arima(
                     ARIMA$results.total.age.retro.predictive.performance.arima,
                     ARIMA$pointforecasts,
                     ARIMA$PI.total.age.arima, 
                     ARIMA$stockabundance,
                     ARIMA$stockname,
                     ARIMA$stockspecies)

    doc = addPlot(doc,
              fun = plot, # print,
              x = myplot,
              width=6.5, height=6)


    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)
    rm(figurecaption0)
    rm(figurecaption)

}

#-----------------------------------------------------------------------------------------
# expsmooth: Visual representation of interval forecast of total abundance (total age, expsmooth)
#-----------------------------------------------------------------------------------------

                                                                                        
if (table.rank.rmse.results.total.age$best.model=="expsmooth") {

    
    figurecaption0 <- ifelse(length(table.rank.rmse.results.total.age$all.models) > 1,
                                  paste0("This model was identified as the best forecasting model among all considered models for total ",
                                         tolower(EXPSMOOTH$stockabundance), "."), 
                             "") 

    figurecaption <- paste0("Historical total ",
                       tolower(EXPSMOOTH$stockabundance),
                       " values",
                       " along with the corresponding ",
                       max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                       " point forecast and 80% interval forecast ",
                       "for the ",
                       paste(EXPSMOOTH$stockname,tolower(EXPSMOOTH$stockspecies), "stock."),
                       " The point and interval forecasts of total ",
                       tolower(EXPSMOOTH$stockabundance),
                       " were produced by ",
                       "the exponential smoothing model. ", 
                       figurecaption0, 
                       sep="")

    EXPSMOOTH$pointforecasts <- EXPSMOOTH$point.forecast.expsmooth(EXPSMOOTH$datalist, EXPSMOOTH$expsmooth.model.fits, EXPSMOOTH$boxcoxtransform)

    ## graphics.off() 
    
    myplot <- EXPSMOOTH$scatterplot.forecasted.values.and.forecast.intervals.total.age.expsmooth(
                     EXPSMOOTH$results.total.age.retro.predictive.performance.expsmooth,
                     EXPSMOOTH$pointforecasts,
                     EXPSMOOTH$PI.total.age.expsmooth, 
                     EXPSMOOTH$stockabundance,
                     EXPSMOOTH$stockname,
                     EXPSMOOTH$stockspecies)

    doc = addPlot(doc,
              fun = plot, # print,
              x = myplot,
              width=6.5, height=6)


    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)
    rm(figurecaption0)
    rm(figurecaption)

}









#====================================================================================                      
# Results for the "Best" Forecasting Model for Total Age: 
#   Empirical Probabilities for Total Age
#====================================================================================

## doc = addPageBreak(doc)

doc = addSection(doc, landscape=FALSE, ncol=1)

doc <- addTitle(doc, "Empirical Probabilities Concerning Total Abundance", level=1)               
               
if (table.rank.rmse.results.total.age$best.model=="naiveone") {

    ## doc = addPageBreak(doc)
     
    n1$emp.prob.naiveone.total.age <- empirical.probability.yboot.model.total.age(n1$PI.total.age.naiveone, n1$PI.total.age.naiveone.sim, stockabundance)
   
    tablecaption <- paste0("Estimated probabilities that the actual total ",
                       tolower(stockabundance)," ",
                       "value yet to be observed in ",
                       forecastingyear, " for the ",
                       stockname," ",
                       stockspecies, " stock",
                       " is less than or equal to a specific threshold, ",
                       " greater than a specific threshold ",
                       " or falls between two specific thresholds ",
                       " (i.e., the threshold located on the previous row and the one located on the current row of the table). ",  
                       "Probabilities are derived from the distribution of bootstrapped point forecasts of ",
                       "total ",
                       tolower(stockabundance), 
                       ifelse(length(table.rank.rmse.results.total.age$all.models)>1, 
                       " corresponding to the best-ranked forecasting model among all considered forecasting models, namely the naive model (previous year).", 
                       " corresponding to the naive model (previous year)."))
                       

    doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")

    tt_1 <- n1$emp.prob.naiveone.total.age$prob.thresholds

    tt_2 <- n1$emp.prob.naiveone.total.age$prob.point.forecast

    tt_1_and_2 <- rbind.data.frame(tt_1, tt_2)

    usePackage("plyr")

    tt_arrange <- arrange(tt_1_and_2, prob.threshold)

    # set cell padding defaut to 2
    baseCellProp = cellProperties( padding = 2)

    from_tmp = which(tt_arrange[,1] == n1$emp.prob.naiveone.total.age$prob.point.forecast$prob.threshold)

    tt_arrange[from_tmp, 4] <- tt_arrange[from_tmp + 1, 4]

    tt_arrange[,1] <- comma(tt_arrange[,1])
    tt_arrange[,2] <- paste0(sprintf("%.1f", tt_arrange[,2]),"%")
    tt_arrange[,3] <- paste0(sprintf("%.1f", tt_arrange[,3]),"%")
    tt_arrange[,4] <- paste0(sprintf("%.1f", tt_arrange[,4]),"%")

    names(tt_arrange)[1] <- "Threshold"

    names(tt_arrange)[2] <- "Prob(Actual <= Threshold)"
    names(tt_arrange)[3] <- "Prob(Actual > Threshold)"

    ## names(tt_arrange)[4] <- "Prob(Previous Threshold < Actual <= Current Threshold)"

    names(tt_arrange)[4] <- "Interval Probability"

    tt_arrange[1,4] <- "-"

    my_ft <- FlexTable( data = tt_arrange,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right"),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=2)
             )

    # overwrites some paragraph formatting properties
    my_ft[, 1:ncol(tt_arrange)] = parProperties(text.align = "right")


    my_ft = spanFlexTableRows(my_ft, j=4, from = from_tmp, to = from_tmp + 1)

    my_ft[tt_arrange$Threshold %in% comma(n1$emp.prob.naiveone.total.age$prob.point.forecast[1]), 1:3] = 
             cellProperties( background.color = "orange",  padding=2)

    doc = addFlexTable(doc, flextable=my_ft)
    
    rm(my_ft)
    rm(tt_arrange)
    rm(from_tmp)
    rm(tt_1)
    rm(tt_2)
    rm(tt_1_and_2)
    rm(tablecaption)

}  ## end if (naiveone)



if (table.rank.rmse.results.total.age$best.model=="avgthree") {

    ## doc = addPageBreak(doc)
    
    n3$emp.prob.avgthree.total.age <- empirical.probability.yboot.model.total.age(n3$PI.total.age.avgthree, n3$PI.total.age.avgthree.sim, stockabundance)
   
    tablecaption <- paste0("Estimated probabilities that the actual total ",
                       tolower(stockabundance)," ",
                       "value yet to be observed in ",
                       forecastingyear, " for the ",
                       stockname," ",
                       stockspecies, " stock",
                       " is less than or equal to a specific threshold, ",
                       " greater than a specific threshold ",
                       " or falls between two specific thresholds ",
                       " (i.e., the threshold located on the previous row and the one located on the current row of the table). ",  
                       "Probabilities are derived from the distribution of bootstrapped point forecasts of ",
                       "total ",
                       tolower(stockabundance), 
                       ifelse(length(table.rank.rmse.results.total.age$all.models)>1, 
                       " corresponding to the best-ranked forecasting model among all considered forecasting models, namely the naive model (average of previous 3 years).", 
                       " corresponding to the naive model (average of previous 3 years)."))
                       
    doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")

    tt_1 <- n3$emp.prob.avgthree.total.age$prob.thresholds

    tt_2 <- n3$emp.prob.avgthree.total.age$prob.point.forecast

    tt_1_and_2 <- rbind.data.frame(tt_1, tt_2)

    usePackage("plyr")

    tt_arrange <- arrange(tt_1_and_2, prob.threshold)

    # set cell padding defaut to 2
    baseCellProp = cellProperties( padding = 2)

    from_tmp = which(tt_arrange[,1] == n3$emp.prob.avgthree.total.age$prob.point.forecast$prob.threshold)

    tt_arrange[from_tmp, 4] <- tt_arrange[from_tmp + 1, 4]

    tt_arrange[,1] <- comma(tt_arrange[,1])
    tt_arrange[,2] <- paste0(sprintf("%.1f", tt_arrange[,2]),"%")
    tt_arrange[,3] <- paste0(sprintf("%.1f", tt_arrange[,3]),"%")
    tt_arrange[,4] <- paste0(sprintf("%.1f", tt_arrange[,4]),"%")

    names(tt_arrange)[1] <- "Threshold"

    names(tt_arrange)[2] <- "Prob(Actual <= Threshold)"
    names(tt_arrange)[3] <- "Prob(Actual > Threshold)"

    ## names(tt_arrange)[4] <- "Prob(Previous Threshold < Actual <= Current Threshold)"

    names(tt_arrange)[4] <- "Interval Probability"

    tt_arrange[1,4] <- "-"

    my_ft <- FlexTable( data = tt_arrange,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right"),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=2)
             )

    # overwrites some paragraph formatting properties
    my_ft[, 1:ncol(tt_arrange)] = parProperties(text.align = "right")


    my_ft = spanFlexTableRows(my_ft, j=4, from = from_tmp, to = from_tmp + 1)

    my_ft[tt_arrange$Threshold %in% comma(n3$emp.prob.avgthree.total.age$prob.point.forecast[1]), 1:3] = 
             cellProperties( background.color = "orange",  padding=2)

    doc = addFlexTable(doc, flextable=my_ft)

    rm(my_ft)
    rm(tt_arrange)
    rm(from_tmp)
    rm(tt_1)
    rm(tt_2)
    rm(tt_1_and_2)
    rm(tablecaption)

} ## end if (avgthree)


if (table.rank.rmse.results.total.age$best.model=="avgfive") {

   
    ## doc = addPageBreak(doc)
    
    n5$emp.prob.avgfive.total.age <- empirical.probability.yboot.model.total.age(n5$PI.total.age.avgfive, n5$PI.total.age.avgfive.sim, stockabundance)
   
    tablecaption <- paste0("Estimated probabilities that the actual total ",
                       tolower(stockabundance)," ",
                       "value yet to be observed in ",
                       forecastingyear, " for the ",
                       stockname," ",
                       stockspecies, " stock",
                       " is less than or equal to a specific threshold, ",
                       " greater than a specific threshold ",
                       " or falls between two specific thresholds ",
                       " (i.e., the threshold located on the previous row and the one located on the current row of the table). ",  
                       "Probabilities are derived from the distribution of bootstrapped point forecasts of ",
                       "total ",
                       tolower(stockabundance), 
                       ifelse(length(table.rank.rmse.results.total.age$all.models)>1, 
                       " corresponding to the best-ranked forecasting model among all considered forecasting models, namely the naive model (average of previous 5 years).", 
                       " corresponding to the naive model (average of previous 5 years)."))
                       
    doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")

    tt_1 <- n5$emp.prob.avgfive.total.age$prob.thresholds

    tt_2 <- n5$emp.prob.avgfive.total.age$prob.point.forecast

    tt_1_and_2 <- rbind.data.frame(tt_1, tt_2)

    usePackage("plyr")

    tt_arrange <- arrange(tt_1_and_2, prob.threshold)

    # set cell padding defaut to 2
    baseCellProp = cellProperties( padding = 2)

    from_tmp = which(tt_arrange[,1] == n5$emp.prob.avgfive.total.age$prob.point.forecast$prob.threshold)

    tt_arrange[from_tmp, 4] <- tt_arrange[from_tmp + 1, 4]

    tt_arrange[,1] <- comma(tt_arrange[,1])
    tt_arrange[,2] <- paste0(sprintf("%.1f", tt_arrange[,2]),"%")
    tt_arrange[,3] <- paste0(sprintf("%.1f", tt_arrange[,3]),"%")
    tt_arrange[,4] <- paste0(sprintf("%.1f", tt_arrange[,4]),"%")

    names(tt_arrange)[1] <- "Threshold"

    names(tt_arrange)[2] <- "Prob(Actual <= Threshold)"
    names(tt_arrange)[3] <- "Prob(Actual > Threshold)"

    ## names(tt_arrange)[4] <- "Prob(Previous Threshold < Actual <= Current Threshold)"

    names(tt_arrange)[4] <- "Interval Probability"

    tt_arrange[1,4] <- "-"

    my_ft <- FlexTable( data = tt_arrange,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right"),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=2)
             )

    # overwrites some paragraph formatting properties
    my_ft[, 1:ncol(tt_arrange)] = parProperties(text.align = "right")


    my_ft = spanFlexTableRows(my_ft, j=4, from = from_tmp, to = from_tmp + 1)

    my_ft[tt_arrange$Threshold %in% comma(n5$emp.prob.avgfive.total.age$prob.point.forecast[1]), 1:3] = 
             cellProperties( background.color = "orange",  padding=2)

    doc = addFlexTable(doc, flextable=my_ft)

    rm(my_ft)
    rm(tt_arrange)
    rm(from_tmp)
    rm(tt_1)
    rm(tt_2)
    rm(tt_1_and_2)
    rm(tablecaption)


} ## end if (avgfive)


if (table.rank.rmse.results.total.age$best.model=="arima") {

    
    ## doc = addPageBreak(doc)
    
    ARIMA$emp.prob.arima.total.age <- empirical.probability.yboot.model.total.age(ARIMA$PI.total.age.arima, ARIMA$PI.total.age.arima.sim, stockabundance)
   
    tablecaption <- paste0("Estimated probabilities that the actual total ",
                       tolower(stockabundance)," ",
                       "value yet to be observed in ",
                       forecastingyear, " for the ",
                       stockname," ",
                       stockspecies, " stock",
                       " is less than or equal to a specific threshold, ",
                       " greater than a specific threshold ",
                       " or falls between two specific thresholds ",
                       " (i.e., the threshold located on the previous row and the one located on the current row of the table). ",  
                       "Probabilities are derived from the distribution of bootstrapped point forecasts of ",
                       "total ",
                       tolower(stockabundance), 
                       ifelse(length(table.rank.rmse.results.total.age$all.models)>1, 
                       " corresponding to the best-ranked forecasting model among all considered forecasting models, namely the ARIMA model.", 
                       " corresponding to the ARIMA model."))
                       
    doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")

    tt_1 <- ARIMA$emp.prob.arima.total.age$prob.thresholds

    tt_2 <- ARIMA$emp.prob.arima.total.age$prob.point.forecast

    tt_1_and_2 <- rbind.data.frame(tt_1, tt_2)

    usePackage("plyr")

    tt_arrange <- arrange(tt_1_and_2, prob.threshold)

    # set cell padding defaut to 2
    baseCellProp = cellProperties( padding = 2)

    from_tmp = which(tt_arrange[,1] == ARIMA$emp.prob.arima.total.age$prob.point.forecast$prob.threshold)

    tt_arrange[from_tmp, 4] <- tt_arrange[from_tmp + 1, 4]

    tt_arrange[,1] <- comma(tt_arrange[,1])
    tt_arrange[,2] <- paste0(sprintf("%.1f", tt_arrange[,2]),"%")
    tt_arrange[,3] <- paste0(sprintf("%.1f", tt_arrange[,3]),"%")
    tt_arrange[,4] <- paste0(sprintf("%.1f", tt_arrange[,4]),"%")

    names(tt_arrange)[1] <- "Threshold"

    names(tt_arrange)[2] <- "Prob(Actual <= Threshold)"
    names(tt_arrange)[3] <- "Prob(Actual > Threshold)"

    ## names(tt_arrange)[4] <- "Prob(Previous Threshold < Actual <= Current Threshold)"

    names(tt_arrange)[4] <- "Interval Probability"

    tt_arrange[1,4] <- "-"

    my_ft <- FlexTable( data = tt_arrange,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right"),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=2)
             )

    # overwrites some paragraph formatting properties
    my_ft[, 1:ncol(tt_arrange)] = parProperties(text.align = "right")


    my_ft = spanFlexTableRows(my_ft, j=4, from = from_tmp, to = from_tmp + 1)

    my_ft[tt_arrange$Threshold %in% comma(ARIMA$emp.prob.arima.total.age$prob.point.forecast[1]), 1:3] = 
             cellProperties( background.color = "orange",  padding=2)

    doc = addFlexTable(doc, flextable=my_ft)

    rm(my_ft)
    rm(tt_arrange)
    rm(from_tmp)
    rm(tt_1)
    rm(tt_2)
    rm(tt_1_and_2)
    rm(tablecaption)



} ## end if (arima)

if (table.rank.rmse.results.total.age$best.model=="expsmooth") {

  
    ## doc = addPageBreak(doc)
    
    EXPSMOOTH$emp.prob.expsmooth.total.age <- empirical.probability.yboot.model.total.age(EXPSMOOTH$PI.total.age.expsmooth, EXPSMOOTH$PI.total.age.expsmooth.sim, stockabundance)
   
    tablecaption <- paste0("Estimated probabilities that the actual total ",
                       tolower(stockabundance)," ",
                       "value yet to be observed in ",
                       forecastingyear, " for the ",
                       stockname," ",
                       stockspecies, " stock",
                       " is less than or equal to a specific threshold, ",
                       " greater than a specific threshold ",
                       " or falls between two specific thresholds ",
                       " (i.e., the threshold located on the previous row and the one located on the current row of the table). ",  
                       "Probabilities are derived from the distribution of bootstrapped point forecasts of ",
                       "total ",
                       tolower(stockabundance), 
                       ifelse(length(table.rank.rmse.results.total.age$all.models)>1, 
                       " corresponding to the best-ranked forecasting model among all considered forecasting models, namely the naive model (average of previous 5 years).", 
                       " corresponding to the naive model (average of previous 5 years)."))
                       
    doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")

    tt_1 <- EXPSMOOTH$emp.prob.expsmooth.total.age$prob.thresholds

    tt_2 <- EXPSMOOTH$emp.prob.expsmooth.total.age$prob.point.forecast

    tt_1_and_2 <- rbind.data.frame(tt_1, tt_2)

    usePackage("plyr")

    tt_arrange <- arrange(tt_1_and_2, prob.threshold)

    # set cell padding defaut to 2
    baseCellProp = cellProperties( padding = 2)

    from_tmp = which(tt_arrange[,1] == EXPSMOOTH$emp.prob.expsmooth.total.age$prob.point.forecast$prob.threshold)

    tt_arrange[from_tmp, 4] <- tt_arrange[from_tmp + 1, 4]

    tt_arrange[,1] <- comma(tt_arrange[,1])
    tt_arrange[,2] <- paste0(sprintf("%.1f", tt_arrange[,2]),"%")
    tt_arrange[,3] <- paste0(sprintf("%.1f", tt_arrange[,3]),"%")
    tt_arrange[,4] <- paste0(sprintf("%.1f", tt_arrange[,4]),"%")

    names(tt_arrange)[1] <- "Threshold"

    names(tt_arrange)[2] <- "Prob(Actual <= Threshold)"
    names(tt_arrange)[3] <- "Prob(Actual > Threshold)"

    ## names(tt_arrange)[4] <- "Prob(Previous Threshold < Actual <= Current Threshold)"

    names(tt_arrange)[4] <- "Interval Probability"

    tt_arrange[1,4] <- "-"

    my_ft <- FlexTable( data = tt_arrange,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right"),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=2)
             )

    # overwrites some paragraph formatting properties
    my_ft[, 1:ncol(tt_arrange)] = parProperties(text.align = "right")


    my_ft = spanFlexTableRows(my_ft, j=4, from = from_tmp, to = from_tmp + 1)

    my_ft[tt_arrange$Threshold %in% comma(EXPSMOOTH$emp.prob.expsmooth.total.age$prob.point.forecast[1]), 1:3] = 
             cellProperties( background.color = "orange",  padding=2)

    doc = addFlexTable(doc, flextable=my_ft)

    rm(my_ft)
    rm(tt_arrange)
    rm(from_tmp)
    rm(tt_1)
    rm(tt_2)
    rm(tt_1_and_2)
    rm(tablecaption)

  


} ## end if (expsmooth)


if (table.rank.rmse.results.total.age$best.model=="sibreg") {

    ## doc = addPageBreak(doc)
     
    SIMPLESIBREG$emp.prob.sibreg.total.age <- empirical.probability.yboot.model.total.age(SIMPLESIBREG$PI.total.age.sibreg, SIMPLESIBREG$PI.total.age.sibreg.sim, stockabundance)
   
    tablecaption <- paste0("Estimated probabilities that the actual total ",
                       tolower(stockabundance)," ",
                       "value yet to be observed in ",
                       forecastingyear, " for the ",
                       stockname," ",
                       stockspecies, " stock",
                       " is less than or equal to a specific threshold, ",
                       " greater than a specific threshold ",
                       " or falls between two specific thresholds ",
                       " (i.e., the threshold located on the previous row and the one located on the current row of the table). ",  
                       "Probabilities are derived from the distribution of bootstrapped point forecasts of ",
                       "total ",
                       tolower(stockabundance), 
                       ifelse(length(table.rank.rmse.results.total.age$all.models)>1, 
                       " corresponding to the best-ranked forecasting model among all considered forecasting models, namely the sibling regression model without environmental covariates.", 
                       " corresponding to the sibling regression model without environmental covariates."))
                       

    doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")

    tt_1 <- SIMPLESIBREG$emp.prob.sibreg.total.age$prob.thresholds

    tt_2 <- SIMPLESIBREG$emp.prob.sibreg.total.age$prob.point.forecast

    tt_1_and_2 <- rbind.data.frame(tt_1, tt_2)

    usePackage("plyr")

    tt_arrange <- arrange(tt_1_and_2, prob.threshold)

    # set cell padding defaut to 2
    baseCellProp = cellProperties( padding = 2)

    from_tmp = which(tt_arrange[,1] == SIMPLESIBREG$emp.prob.sibreg.total.age$prob.point.forecast$prob.threshold)

    tt_arrange[from_tmp, 4] <- tt_arrange[from_tmp + 1, 4]

    tt_arrange[,1] <- comma(tt_arrange[,1])
    tt_arrange[,2] <- paste0(sprintf("%.1f", tt_arrange[,2]),"%")
    tt_arrange[,3] <- paste0(sprintf("%.1f", tt_arrange[,3]),"%")
    tt_arrange[,4] <- paste0(sprintf("%.1f", tt_arrange[,4]),"%")

    names(tt_arrange)[1] <- "Threshold"

    names(tt_arrange)[2] <- "Prob(Actual <= Threshold)"
    names(tt_arrange)[3] <- "Prob(Actual > Threshold)"

    ## names(tt_arrange)[4] <- "Prob(Previous Threshold < Actual <= Current Threshold)"

    names(tt_arrange)[4] <- "Interval Probability"

    tt_arrange[1,4] <- "-"

    my_ft <- FlexTable( data = tt_arrange,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right"),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=2)
             )

    # overwrites some paragraph formatting properties
    my_ft[, 1:ncol(tt_arrange)] = parProperties(text.align = "right")


    my_ft = spanFlexTableRows(my_ft, j=4, from = from_tmp, to = from_tmp + 1)

    my_ft[tt_arrange$Threshold %in% comma(SIMPLESIBREG$emp.prob.sibreg.total.age$prob.point.forecast[1]), 1:3] = 
             cellProperties( background.color = "orange",  padding=2)

    doc = addFlexTable(doc, flextable=my_ft)
    
    rm(my_ft)
    rm(tt_arrange)
    rm(from_tmp)
    rm(tt_1)
    rm(tt_2)
    rm(tt_1_and_2)
    rm(tablecaption)

}  ## end if (sibreg)

                      
#====================================================================================                      
# Results for the "Best" Forecasting Model for Total Age: 
#   Distribution of Bootstrapped Point Forecasts 
#====================================================================================

## doc = addPageBreak(doc)

doc = addSection(doc, landscape=FALSE, ncol=1)

doc <- addTitle(doc, "Distribution of Bootstrapped Point Forecasts of Total Abundance", level=2)

if (table.rank.rmse.results.total.age$best.model=="naiveone") {

    figurecaption <- paste("Distribution of bootstrapped forecasted values ",
                       "for the total ",
                       tolower(stockabundance),
                       " corresponding to the ",
                       paste(stockname," ", tolower(stockspecies),
                       " stock, derived on the basis of maximum entropy bootstrapping for the forecasting year ",
                       forecastingyear,
                       ".",
                       " The dashed red line indicates the position of the point forecast of total terminal run on the horizontal axis,",
                       "while the blue segment indicates the 80% forecast interval.", sep=""),
                       " The point forecast for the total ",
                       tolower(stockabundance),
                       " was obtained by adding up the point forecasts for the age-specific components of ",
                       tolower(stockabundance),
                       " produced by naive modeling (previous year).",
                       sep="")


   myplot <- n1$plot.distribution.bootstrapped.point.forecasts.total.age.naiveone(n1$PI.total.age.naiveone.sim,
      n1$PI.total.age.naiveone.no.comma, n1$stockabundance)
      
   doc = addPlot(doc,
            fun=print,
            x=myplot,
            width=plotwidth, height=plotheight-3)

    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)
    rm(figurecaption)

}


if (table.rank.rmse.results.total.age$best.model=="avgthree") {

     figurecaption <- paste("Distribution of bootstrapped forecasted values ",
                       "for the total ",
                       tolower(stockabundance),
                       " corresponding to the ",
                       paste(stockname," ", tolower(stockspecies),
                       " stock, derived on the basis of maximum entropy bootstrapping for the forecasting year ",
                       forecastingyear,
                       ".",
                       " The dashed red line indicates the position of the point forecast of total terminal run on the horizontal axis,",
                       "while the blue segment indicates the 80% forecast interval.", sep=""),
                       " The point forecast for the total ",
                       tolower(stockabundance),
                       " was obtained by adding up the point forecasts for the age-specific components of ",
                       tolower(stockabundance),
                       " produced by naive modeling (average of previous 3 years).",
                       sep="")


   myplot <- n3$plot.distribution.bootstrapped.point.forecasts.total.age.avgthree(n3$PI.total.age.avgthree.sim,
                                                                  n3$PI.total.age.avgthree.no.comma,
                                                                  n3$stockabundance)
                                                                  
   doc = addPlot(doc,
            fun=print,
            x=myplot,
            width=plotwidth, height=plotheight-3)

    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)
    rm(figurecaption)
}



if (table.rank.rmse.results.total.age$best.model=="avgfive") {

   figurecaption <- paste("Distribution of bootstrapped forecasted values ",
                       "for the total ",
                       tolower(stockabundance),
                       " corresponding to the ",
                       paste(stockname," ", tolower(stockspecies),
                       " stock, derived on the basis of maximum entropy bootstrapping for the forecasting year ",
                       forecastingyear,
                       ".",
                       " The dashed red line indicates the position of the point forecast of total terminal run on the horizontal axis,",
                       "while the blue segment indicates the 80% forecast interval.", sep=""),
                       " The point forecast for the total ",
                       tolower(stockabundance),
                       " was obtained by adding up the point forecasts for the age-specific components of ",
                       tolower(stockabundance),
                       " produced by naive modeling (average of previous 5 years).",
                       sep="")


   myplot <- n5$plot.distribution.bootstrapped.point.forecasts.total.age.avgfive(n5$PI.total.age.avgfive.sim,
                                                                    n5$PI.total.age.avgfive.no.comma,
                                                                    n5$stockabundance)
                                                                    
   doc = addPlot(doc,
            fun=print,
            x=myplot,
            width=plotwidth, height=plotheight-3)

    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)
    rm(figurecaption)

}


if (table.rank.rmse.results.total.age$best.model=="arima") {

    figurecaption <- paste("Distribution of bootstrapped forecasted values ",
                       "for the total ",
                       tolower(stockabundance),
                       " corresponding to the ",
                       paste(stockname," ", tolower(stockspecies),
                       " stock, derived on the basis of loess bootstrapping for the forecasting year ",
                       forecastingyear,
                       ".",
                       " The dashed red line indicates the position of the point forecast of total terminal run on the horizontal axis,",
                       "while the blue segment indicates the 80% forecast interval.", sep=""),
                       " The point forecast for the total ",
                       tolower(stockabundance),
                       " was obtained by adding up the point forecasts for the age-specific components of ",
                       tolower(stockabundance),
                       " produced by ARIMA modeling.",
                       sep="")


    myplot <- ARIMA$plot.distribution.bootstrapped.point.forecasts.total.age.arima(ARIMA$PI.total.age.arima.sim,
                                                               ARIMA$PI.total.age.arima.no.comma,
                                                               ARIMA$stockabundance)
    
    doc = addPlot(doc,
            fun=print,
            x=myplot,
            width=plotwidth, height=plotheight-3)

    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)
    rm(figurecaption)
    
}

if (table.rank.rmse.results.total.age$best.model=="expsmooth") {

    figurecaption <- paste("Distribution of bootstrapped forecasted values ",
                       "for the total ",
                       tolower(stockabundance),
                       " corresponding to the ",
                       paste(stockname," ", tolower(stockspecies),
                       " stock, derived on the basis of loess bootstrapping for the forecasting year ",
                       forecastingyear,
                       ".",
                       " The dashed red line indicates the position of the point forecast of total terminal run on the horizontal axis,",
                       "while the blue segment indicates the 80% forecast interval.", sep=""),
                       " The point forecast for the total ",
                       tolower(stockabundance),
                       " was obtained by adding up the point forecasts for the age-specific components of ",
                       tolower(stockabundance),
                       " produced by exponential smoothing.",
                       sep="")


    myplot <- EXPSMOOTH$plot.distribution.bootstrapped.point.forecasts.total.age.expsmooth(EXPSMOOTH$PI.total.age.expsmooth.sim,
                                                               EXPSMOOTH$PI.total.age.expsmooth.no.comma,
                                                               EXPSMOOTH$stockabundance)


    doc = addPlot(doc,
            fun=print,
            x=myplot,
            width=plotwidth, height=plotheight-3)

    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)
    rm(figurecaption)


}

           
#====================================================================================                      
# Results for the "Best" Forecasting Model for Individual Ages: 
#   Super-Imposed Time Series Plots of Retrospectively Forecasted and Actual Age-Specific Abundance Values 
#====================================================================================

## doc = addPageBreak(doc)


## doc <- addTitle(doc, "Forecast Diagnostics for Age-Specific Abundance", level=2)

## COME BACK HERE

#====================================================================================                      
# Results for the "Best" Forecasting Model for Total Age: 
#   Super-Imposed Time Series Plots of Retrospectively Forecasted and Actual Total Abundance Values 
#=====================================================================================

## doc = addPageBreak(doc)

doc = addSection(doc, landscape=TRUE, ncol=1)

doc <- addTitle(doc, "Forecast Diagnostics Pertaining to Total Abundance", level=2)

if (table.rank.rmse.results.total.age$best.model=="naiveone") {

   figurecaption <- paste("Super-imposed time series plots of retrospectively forecasted and actual total ",
                       # terminal runs
                       paste0(tolower(n1$stockabundance),"s",collapse=" "),
                       " corresponding to the return years ",
                       min(n1$results.afe.total.age.retro.naiveone$CY),
                       " - ",
                       max(n1$results.afe.total.age.retro.naiveone$CY),
                       ". ", ## "Forecasted total terminal runs were derived via naive time series modeling.",
                       "The retrospectively forecasted total ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " for a given return year was derived by adding up the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs
                       paste0(tolower(n1$stockabundance),"s",collapse=" "),
                       " for that year.",
                       " ARIMA modeling ",
                       " was used to obtain the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs.",
                       paste0(tolower(n1$stockabundance),"s",collapse=" "),
                       ".",
                       sep="") 
                               
   myplot <- n1$timeseries.plot.results.afe.total.age.retro.naiveone(n1$results.total.age.retro.predictive.performance.naiveone, 
                                                                     n1$stockabundance)
                                                                     
   doc = addPlot(doc,
            fun=print,
            x=myplot,
            width=plotwidth+1, height=plotheight-2)

   doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

   rm(myplot)

   rm(figurecaption)
                                                                                                                   
}                                                             

if (table.rank.rmse.results.total.age$best.model=="avgthree") {

   figurecaption <- paste("Super-imposed time series plots of retrospectively forecasted and actual total ",
                       # terminal runs
                       paste0(tolower(n3$stockabundance),"s",collapse=" "),
                       " corresponding to the return years ",
                       min(n3$results.afe.total.age.retro.avgthree$CY),
                       " - ",
                       max(n3$results.afe.total.age.retro.avgthree$CY),
                       ". ", ## "Forecasted total terminal runs were derived via naive time series modeling.",
                       "The retrospectively forecasted total ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " for a given return year was derived by adding up the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs
                       paste0(tolower(n3$stockabundance),"s",collapse=" "),
                       " for that year.",
                       " Naive modeling (average of previous 3 years)",
                       " was used to obtain the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs.",
                       paste0(tolower(n3$stockabundance),"s",collapse=" "),
                       ".",
                       sep="") 
                               
   myplot <- n3$timeseries.plot.results.afe.total.age.retro.avgthree(n3$results.total.age.retro.predictive.performance.avgthree, 
                                                                     n3$stockabundance)
                                                                     
   doc = addPlot(doc,
            fun=print,
            x=myplot,
            width=plotwidth+1, height=plotheight-2)

   doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

   rm(myplot)

   rm(figurecaption)
                            
                                                 
}     

if (table.rank.rmse.results.total.age$best.model=="avgfive") {

   figurecaption <- paste("Super-imposed time series plots of retrospectively forecasted and actual total ",
                       # terminal runs
                       paste0(tolower(n5$stockabundance),"s",collapse=" "),
                       " corresponding to the return years ",
                       min(n5$results.afe.total.age.retro.avgfive$CY),
                       " - ",
                       max(n5$results.afe.total.age.retro.avgfive$CY),
                       ". ", ## "Forecasted total terminal runs were derived via naive time series modeling.",
                       "The retrospectively forecasted total ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " for a given return year was derived by adding up the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs
                       paste0(tolower(n5$stockabundance),"s",collapse=" "),
                       " for that year.",
                       " Naive modeling (average of previous 5 years) ",
                       " was used to obtain the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs.",
                       paste0(tolower(n5$stockabundance),"s",collapse=" "),
                       ".",
                       sep="") 
                               
   myplot <- n5$timeseries.plot.results.afe.total.age.retro.avgfive(n5$results.total.age.retro.predictive.performance.avgfive, 
                                                                    n5$stockabundance)
                                                                    
   doc = addPlot(doc,
            fun=print,
            x=myplot,
            width=plotwidth+1, height=plotheight-2)

   doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

   rm(myplot)

   rm(figurecaption)
                            
   
}     

if (table.rank.rmse.results.total.age$best.model=="arima") {

   
   figurecaption <- paste("Super-imposed time series plots of retrospectively forecasted and actual total ",
                       # terminal runs
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       " corresponding to the return years ",
                       min(ARIMA$results.afe.total.age.retro.arima$CY),
                       " - ",
                       max(ARIMA$results.afe.total.age.retro.arima$CY),
                       ". ", ## "Forecasted total terminal runs were derived via naive time series modeling.",
                       "The retrospectively forecasted total ",
                       # terminal run
                       tolower(ARIMA$stockabundance),
                       " for a given return year was derived by adding up the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       " for that year.",
                       " ARIMA modeling ",
                       " was used to obtain the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs.",
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       ".",
                       sep="")
                               
   myplot <- ARIMA$timeseries.plot.results.afe.total.age.retro.arima(ARIMA$results.total.age.retro.predictive.performance.arima, 
                                                                     ARIMA$stockabundance)
                                                                     
   doc = addPlot(doc,
            fun=print,
            x=myplot,
            width=plotwidth+1, height=plotheight-2)

   doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

   rm(myplot)

   rm(figurecaption)
                                              
}     

if (table.rank.rmse.results.total.age$best.model=="expsmooth") {

   figurecaption <- paste("Super-imposed time series plots of retrospectively forecasted and actual total ",
                       # terminal runs
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       " corresponding to the return years ",
                       min(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY),
                       " - ",
                       max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY),
                       ". ", ## "Forecasted total terminal runs were derived via naive time series modeling.",
                       "The retrospectively forecasted total ",
                       # terminal run
                       tolower(EXPSMOOTH$stockabundance),
                       " for a given return year was derived by adding up the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       " for that year.",
                       " Exponential smoothing modeling ",
                       " was used to obtain the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs.",
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       ".",
                       sep="")
                               
   myplot <- EXPSMOOTH$timeseries.plot.results.afe.total.age.retro.expsmooth(EXPSMOOTH$results.total.age.retro.predictive.performance.expsmooth, 
                                                                             EXPSMOOTH$stockabundance)
                                                 
   doc = addPlot(doc,
            fun=print,
            x=myplot,
            width=plotwidth+1, height=plotheight-2)

   doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

   rm(myplot)

   rm(figurecaption)
                                                 
}     



#====================================================================================                      
# Results for the "Best" Forecasting Model for Total Age: 
#   Scatter Plots of Retrospectively Forecasted versus Actual Total Abundance Values 
#=====================================================================================

## doc = addPageBreak(doc)

doc = addSection(doc, landscape=FALSE, ncol=1)

if (table.rank.rmse.results.total.age$best.model=="naiveone") {

    figurecaption <- paste("Scatterplot of retrospectively forecasted total ",
                       # terminal runs
                       paste0(tolower(n1$stockabundance),"s",collapse=" "),
                       " versus actual total ",
                       # terminal runs ",
                       paste0(tolower(n1$stockabundance),"s",collapse=" "),
                       " corresponding to the return years ",
                       min(n1$results.afe.total.age.retro.naiveone$CY),
                       " - ",
                       max(n1$results.afe.total.age.retro.naiveone$CY),
                       ". ", ## "Forecasted total terminal runs were derived via naive time series modeling.",
                       "Observations are labeled according to the last two digits of the associated historical return years.",
                       " The retrospectively forecasted total ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " for a given return year was derived by adding up the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs
                       paste0(tolower(n1$stockabundance),"s",collapse=" "),
                       " for that year.",
                       " Naive modeling (previous year) ",
                       " was used to obtain the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs.",
                       paste0(tolower(n1$stockabundance),"s",collapse=" "),
                       ".",
                       sep="")

   myplot <- n1$scatter.plot.results.afe.total.age.retro.naiveone(n1$results.total.age.retro.predictive.performance.naiveone, n1$naiveone)

   doc = addPlot(doc,
            fun=print,
            x=myplot,
            width=plotwidth+1, height=plotheight-2)

   doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

   rm(myplot)

   rm(figurecaption)


}


if (table.rank.rmse.results.total.age$best.model=="avgthree") {

    figurecaption <- paste("Scatterplot of retrospectively forecasted total ",
                       # terminal runs
                       paste0(tolower(n3$stockabundance),"s",collapse=" "),
                       " versus actual total ",
                       # terminal runs ",
                       paste0(tolower(n3$stockabundance),"s",collapse=" "),
                       " corresponding to the return years ",
                       min(n3$results.afe.total.age.retro.avgthree$CY),
                       " - ",
                       max(n3$results.afe.total.age.retro.avgthree$CY),
                       ". ", ## "Forecasted total terminal runs were derived via naive time series modeling.",
                       "Observations are labeled according to the last two digits of the associated historical return years.",
                       " The retrospectively forecasted total ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " for a given return year was derived by adding up the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs
                       paste0(tolower(n3$stockabundance),"s",collapse=" "),
                       " for that year.",
                       " ARIMA modeling ",
                       " was used to obtain the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs.",
                       paste0(tolower(n3$stockabundance),"s",collapse=" "),
                       ".",
                       sep="")

   myplot <- n3$scatter.plot.results.afe.total.age.retro.avgthree(n3$results.total.age.retro.predictive.performance.avgthree, n3$stockabundance)

   doc = addPlot(doc,
            fun=print,
            x=myplot,
            width=plotwidth+1, height=plotheight-2)

   doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

   rm(myplot)

   rm(figurecaption)

}


if (table.rank.rmse.results.total.age$best.model=="avgfive") {

    figurecaption <- paste("Scatterplot of retrospectively forecasted total ",
                       # terminal runs
                       paste0(tolower(n5$stockabundance),"s",collapse=" "),
                       " versus actual total ",
                       # terminal runs ",
                       paste0(tolower(n5$stockabundance),"s",collapse=" "),
                       " corresponding to the return years ",
                       min(n5$results.afe.total.age.retro.avgfive$CY),
                       " - ",
                       max(n5$results.afe.total.age.retro.avgfive$CY),
                       ". ", ## "Forecasted total terminal runs were derived via naive time series modeling.",
                       "Observations are labeled according to the last two digits of the associated historical return years.",
                       " The retrospectively forecasted total ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " for a given return year was derived by adding up the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs
                       paste0(tolower(n5$stockabundance),"s",collapse=" "),
                       " for that year.",
                       " ARIMA modeling ",
                       " was used to obtain the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs.",
                       paste0(tolower(n5$stockabundance),"s",collapse=" "),
                       ".",
                       sep="")

   myplot <- n5$scatter.plot.results.afe.total.age.retro.avgfive(n5$results.total.age.retro.predictive.performance.avgfive, n5$stockabundance)

   doc = addPlot(doc,
            fun=print,
            x=myplot,
            width=plotwidth+1, height=plotheight-2)

   doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

   rm(myplot)

   rm(figurecaption)


}


if (table.rank.rmse.results.total.age$best.model=="arima") {

    figurecaption <- paste("Scatterplot of retrospectively forecasted total ",
                       # terminal runs
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       " versus actual total ",
                       # terminal runs ",
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       " corresponding to the return years ",
                       min(ARIMA$results.afe.total.age.retro.arima$CY),
                       " - ",
                       max(ARIMA$results.afe.total.age.retro.arima$CY),
                       ". ", ## "Forecasted total terminal runs were derived via naive time series modeling.",
                       "Observations are labeled according to the last two digits of the associated historical return years.",
                       " The retrospectively forecasted total ",
                       # terminal run
                       tolower(ARIMA$stockabundance),
                       " for a given return year was derived by adding up the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       " for that year.",
                       " ARIMA modeling ",
                       " was used to obtain the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs.",
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       ".",
                       sep="")

   myplot <- ARIMA$scatter.plot.results.afe.total.age.retro.arima(ARIMA$results.total.age.retro.predictive.performance.arima, ARIMA$stockabundance)

   doc = addPlot(doc,
            fun=print,
            x=myplot,
            width=plotwidth+1, height=plotheight-2)

   doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

   rm(myplot)

   rm(figurecaption)


}


if (table.rank.rmse.results.total.age$best.model=="expsmooth") {

   figurecaption <- paste("Scatterplot of retrospectively forecasted total ",
                       # terminal runs
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       " versus actual total ",
                       # terminal runs ",
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       " corresponding to the return years ",
                       min(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY),
                       " - ",
                       max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY),
                       ". ", ## "Forecasted total terminal runs were derived via naive time series modeling.",
                       "Observations are labeled according to the last two digits of the associated historical return years.",
                       " The retrospectively forecasted total ",
                       # terminal run
                       tolower(EXPSMOOTH$stockabundance),
                       " for a given return year was derived by adding up the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       " for that year.",
                       " Exponential smoothing modeling ",
                       " was used to obtain the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs.",
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       ".",
                       sep="")

   myplot <- EXPSMOOTH$scatter.plot.results.afe.total.age.retro.expsmooth(EXPSMOOTH$results.total.age.retro.predictive.performance.expsmooth, 
                                                                          EXPSMOOTH$stockabundance)

   doc = addPlot(doc,
            fun=print,
            x=myplot,
            width=plotwidth+1, height=plotheight-2)

   doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

   rm(myplot)

   rm(figurecaption)

}



#====================================================================================                      
# Results for the "Best" Forecasting Model for Total Age: 
#   Bias Coefficient Plot 
#=====================================================================================

doc = addSection(doc, landscape=FALSE, ncol=1)

if (table.rank.rmse.results.total.age$best.model=="naiveone") {
    
    figurecaption <- paste0("Bias coefficient plot constructed from the retrospective forecast errors produced by the naive models (previous year) involved in forecasting ",
                       "the ",
                       max(n1$results.afe.total.age.retro.naiveone$CY)+1,
                       " total ",
                       # " terminal run for the ",
                       tolower(n1$stockabundance),
                       " for the ",
                       paste0(n1$stockname," ", tolower(n1$stockspecies), " stock."), 
                       " The bias coefficient is estimated to be ", sprintf("%.4f", n1$bias.coeff.afe.total.age.retro.naiveone), ".",
                       sep="")


    usePackage("gridGraphics")

    myplot <- n1$bias.coef.plot.total.age.retro.naiveone

    doc = addPlot(doc,
            fun=grid.draw,
            x=myplot,
            width=plotwidth, height=plotheight)

    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)

    rm(figurecaption)

}


if (table.rank.rmse.results.total.age$best.model=="avgthree") {

    figurecaption <- paste0("Bias coefficient plot constructed from the retrospective forecast errors produced by the naive models (average of previous 3 years) involved in forecasting ",
                       "the ",
                       max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                       " total ",
                       # " terminal run for the ",
                       tolower(n3$stockabundance),
                       " for the ",
                       paste0(n3$stockname," ", tolower(n3$stockspecies), " stock."), 
                       " The bias coefficient is estimated to be ", sprintf("%.4f", n3$bias.coeff.afe.total.age.retro.avgthree), ".",
                       sep="")


    usePackage("gridGraphics")

    myplot <- n3$bias.coef.plot.total.age.retro.avgthree

    doc = addPlot(doc,
            fun=grid.draw,
            x=myplot,
            width=plotwidth, height=plotheight)

    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)

    rm(figurecaption)
 

}

if (table.rank.rmse.results.total.age$best.model=="avgfive") {

    figurecaption <- paste0("Bias coefficient plot constructed from the retrospective forecast errors produced by the naive models (average of previous 5 years) involved in forecasting ",
                       "the ",
                       max(n5$results.afe.total.age.retro.avgfive$CY)+1,
                       " total ",
                       # " terminal run for the ",
                       tolower(n5$stockabundance),
                       " for the ",
                       paste0(n5$stockname," ", tolower(n5$stockspecies), " stock."), 
                       " The bias coefficient is estimated to be ", sprintf("%.4f", n5$bias.coeff.afe.total.age.retro.avgfive), ".",
                       sep="")

    usePackage("gridGraphics")

    myplot <- n5$bias.coef.plot.total.age.retro.avgfive

    doc = addPlot(doc,
            fun=grid.draw,
            x=myplot,
            width=plotwidth, height=plotheight)

    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)

    rm(figurecaption)

}


if (table.rank.rmse.results.total.age$best.model=="arima") {

     figurecaption <- paste0("Bias coefficient plot constructed from the retrospective forecast errors produced by the ARIMA models involved in forecasting ",
                       "the ",
                       max(ARIMA$results.afe.total.age.retro.arima$CY)+1,
                       " total ",
                       # " terminal run for the ",
                       tolower(ARIMA$stockabundance),
                       " for the ",
                       paste0(ARIMA$stockname," ", tolower(ARIMA$stockspecies), " stock."), 
                       " The bias coefficient is estimated to be ", sprintf("%.4f", ARIMA$bias.coeff.afe.total.age.retro.arima), ".",
                       sep="")


    usePackage("gridGraphics")
    
    myplot <- ARIMA$bias.coef.plot.total.age.retro.arima

    doc = addPlot(doc,
            fun=grid.draw,
            x=myplot,
            width=plotwidth, height=plotheight)

    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)

    rm(figurecaption)

}


if (table.rank.rmse.results.total.age$best.model=="expsmooth") {

    figurecaption <- paste0("Bias coefficient plot constructed from the retrospective forecast errors produced by the exponential smoothing models involved in forecasting ",
                       "the ",
                       max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                       " total ",
                       # " terminal run for the ",
                       tolower(EXPSMOOTH$stockabundance),
                       " for the ",
                       paste0(EXPSMOOTH$stockname," ", tolower(EXPSMOOTH$stockspecies), " stock."), 
                       " The bias coefficient is estimated to be ", sprintf("%.4f", EXPSMOOTH$bias.coeff.afe.total.age.retro.expsmooth), ".",
                       sep="")

    usePackage("gridGraphics")
    
    myplot <- EXPSMOOTH$bias.coef.plot.total.age.retro.expsmooth

    doc = addPlot(doc,
            fun=grid.draw,
            x=myplot,
            width=plotwidth, height=plotheight)

    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)

    rm(figurecaption)

}






#====================================================================================

endReport(docx.file=docx.file)