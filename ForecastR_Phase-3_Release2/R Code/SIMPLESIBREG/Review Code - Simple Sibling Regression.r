######################################################################################################
# Simple Sibling Regression Models - Prediction Results
# Date Stamp: January 25, 2016
######################################################################################################

cat("Review Code - Simple Sibling Regression.R", "\n\n")

SIMPLESIBREG <- new.env()

######################################################################################################
# setwd("C:\\Documents and Settings\\dv6110ca\\Desktop\\Demo R")

## setwd("L:\\IMPORTANT\\Demo R")

## setwd("F:\\IMPORTANT\\Demo R")

## datafile <- read.csv("SPR_thousands.csv", as.is=TRUE)

SIMPLESIBREG$datafile <- datafile_original

SIMPLESIBREG$stockabundance <- SIMPLESIBREG$datafile$Stock_Abundance[1]
SIMPLESIBREG$stockname <- SIMPLESIBREG$datafile$Stock_Name[1]
SIMPLESIBREG$stockspecies <- SIMPLESIBREG$datafile$Stock_Species[1]
SIMPLESIBREG$forecastingyear <- SIMPLESIBREG$datafile$Forecasting_Year[1]


usePackage("stringr")
SIMPLESIBREG$forecastingyear <- str_replace_all(SIMPLESIBREG$forecastingyear, "\n","")
SIMPLESIBREG$forecastingyear <- as.numeric(SIMPLESIBREG$forecastingyear)


#======================================================================================================

## function for installing and/or loading R packages 
usePackage <- function(p) {
    if (!is.element(p, installed.packages()[,1]))
        install.packages(p, dep = TRUE)
    require(p, character.only = TRUE)
}



wrapper <- function(x, ...){
      paste(strwrap(x, width=50, ...), collapse = "\n")
}


#=======================================================================================================

SIMPLESIBREG$datafile_extract_age_class <- function(datafile, stockabundance){

    forecastingyear <- as.numeric(datafile$Forecasting_Year[1])
    datafilesub <- datafile

    stockname <- datafile$Stock_Name[1]
    extract_ages <- sort(unique(datafilesub$Age_Class))

    tmpvar <- list()
    for (i in 1:length(extract_ages)){
         if (stockabundance=="Terminal Run"){
           tmpvar[[i]] <- subset(datafilesub, Age_Class==extract_ages[i])[,c("Run_Year","Brood_Year","Average_Terminal_Run")]
           names(tmpvar[[i]])[names(tmpvar[[i]])=="Average_Terminal_Run"] <- paste0("Age","_",extract_ages[i])
         }
         
         if (stockabundance=="Escapement"){
           tmpvar[[i]] <- subset(datafilesub, Age_Class==extract_ages[i])[,c("Run_Year","Brood_Year","Average_Escapement")]
            names(tmpvar[[i]])[names(tmpvar[[i]])=="Average_Escapement"] <- paste0("Age","_",extract_ages[i])
         }
         
         if (stockabundance=="Production"){
           tmpvar[[i]] <- subset(datafilesub, Age_Class==extract_ages[i])[,c("Run_Year","Brood_Year","Average_Production")]
            names(tmpvar[[i]])[names(tmpvar[[i]])=="Average_Production"] <- paste0("Age","_",extract_ages[i])
         }
         
    }
    
    names(tmpvar) <- paste("Age", extract_ages)
    
    return(tmpvar)

}


SIMPLESIBREG$datafile_variables <- SIMPLESIBREG$datafile_extract_age_class(SIMPLESIBREG$datafile, SIMPLESIBREG$stockabundance)

## names(datafile_variables)

SIMPLESIBREG$prepare_data_and_model_formulas <- function(datafile_variables){
   ## http://stackoverflow.com/questions/4951442/formula-with-dynamic-number-of-variables


   ## mylist.names <- names(datafile_variables)
   
   ## mylist <- vector("list", length(mylist.names)-1)
   ## names(mylist) <- mylist.names[-1]
   
   mylist <- rep(list(list()),length(names(datafile_variables))-1)

   for (i in 2:length(datafile_variables)){

        outcome_data <- datafile_variables[[i]]
        
        names(outcome_data)[names(outcome_data)=="Run_Year"] <- 
            paste0("Run_Year","_",names(outcome_data)[length(names(outcome_data))])
        
        predictor_data <- list()
        for (j in 1:(i-1)){
          predictor_data[[j]] <- datafile_variables[[j]]
          names(predictor_data[[j]])[names(predictor_data[[j]])=="Run_Year"] <- 
            paste0("Run_Year","_",names(predictor_data[[j]])[length(names(predictor_data[[j]]))])   
        }
        
        mylist[[i-1]]$outcome_data <- outcome_data
        
        mylist[[i-1]]$predictor_data <- predictor_data
        
    }

    names(mylist) <- names(datafile_variables)[-1]

    ## build model formulas for each applicable age class,
    ## as well as provide data set to be used for each formula

    ## myres <- list(list())
    
    myres <- rep(list(list()),length(mylist))

    for (k in 1:length(mylist)){

         mylength <- length(mylist[[k]]$predictor_data)
         
         outcome_name <- names(mylist[[k]]$outcome_data)[3]
         
         predictor_names <-  unlist(lapply(mylist[[k]]$predictor_data, function(ll) {names(ll)[3]}))

         predictor_names  <- rev(predictor_names)


         for (l in 1:length(predictor_names)){
             
             model_formulas <- list()
             for (j in 1:l){
                 model_formulas[[j]] <- as.formula(paste(outcome_name,"~","-1","+",paste(predictor_names[1:(l-j+1)],collapse="+")))
             }
             
         }
         
         model_formulas <- rev(model_formulas)
         
         model_formulas
         
         usePackage("BBmisc")
         
         model_formulas <- getFirst(model_formulas)   # extract first formula only for simple sibling regression
 
         str(model_formulas)
         
         
         ## if (k >= 2) {
         ## for (kk in 2:length(predictor_names)){
         ##   model_formulas[[length(predictor_names)+kk-1]] <- 
         ##   as.formula(paste(outcome_name,"~","-1","+","I(",paste(predictor_names[1:kk],collapse="+"),")"))
         ## }
         ## }
         
         print(model_formulas)     # lapply(model_formulas, `[[`, 1)
                              
         myres[[k]]$model_formulas <- model_formulas

         ## come back here: http://stackoverflow.com/questions/22955617/linear-models-in-r-with-different-combinations-of-variables
         ## predictor_variables <- c("factor1","factor2")
         ## as.formula(paste("y~",paste(listofvariables,collapse="+")))
        
    }

    names(myres) <- names(datafile_variables)[-1]

    mymerge <- list()
    for (i in 1:length(mylist)){
        tmpmerge <- mylist[[i]]$outcome_data
        for (j in length(mylist[[i]]$predictor_data):1) {
            tmpmerge <- merge(tmpmerge, mylist[[i]]$predictor_data[[j]], by="Brood_Year", all=FALSE)
        }
        mymerge[[i]] <- tmpmerge
    }
    
    names(mymerge) <- names(datafile_variables)[-1]
    
    results <- list(model_data=mymerge, model_formulas=myres)
    
    str(results)
    
    return(results)

}




SIMPLESIBREG$datafile_variables <- SIMPLESIBREG$datafile_extract_age_class(SIMPLESIBREG$datafile, SIMPLESIBREG$stockabundance)

SIMPLESIBREG$data_and_model_formulas <-  SIMPLESIBREG$prepare_data_and_model_formulas(SIMPLESIBREG$datafile_variables)

SIMPLESIBREG$data_and_model_formulas 

SIMPLESIBREG$sibling_regression_model_fits <- function(data_and_model_formulas){


    ## fits <- rep(list(list()),length(data_and_model_formulas[[1]]))
    
     fits <- vector("list", length(data_and_model_formulas[[1]]))         
    
     names(fits) <- names(data_and_model_formulas[[1]])
    
     for (i in 1:length(data_and_model_formulas[[1]])){   
         
          fits[[i]] <- lm(formula=data_and_model_formulas$model_formulas[[i]]$model_formulas,
                          data=data_and_model_formulas$model_data[[i]])
    
    }
    
       
    res <- list(model_data=data_and_model_formulas$model_data, 
                model_formulas=data_and_model_formulas$model_formulas, 
                model_fits=fits)

    return(res)

}


SIMPLESIBREG$fits <-  SIMPLESIBREG$sibling_regression_model_fits(SIMPLESIBREG$data_and_model_formulas) 

SIMPLESIBREG$fits 

SIMPLESIBREG$fits$model_data
SIMPLESIBREG$fits$model_formulas
SIMPLESIBREG$fits$model_fits

## Important Reference on working with AIC:
## http://theses.ulaval.ca/archimede/fichiers/21842/apa.html 

## Reference on differences in computing AIC values and on AICcmodavg package  
## http://r.789695.n4.nabble.com/R-s-AIC-values-differ-from-published-values-td4384952.html


SIMPLESIBREG$model_selection_results <- function(fits){
    
    ## model formulas 
    ## myformulas <- rep(list(list()),length(fits))
    myformulas <- NULL
    
    for (i in 1:length(fits$model_fits)){
         myformulas[[i]] <- fits$model_formulas[[i]]
    }
    
    names(myformulas) <-  names(fits$model_formulas)
    
    ## coefficients
    mycoefs <- rep(list(),length(fits$model_fits))
    
    for (i in 1:length(fits$model_fits)){
         mycoefs[[i]] <- round(coef(fits$model_fits[[i]]),4)
    }
    
    names(mycoefs) <-  names(fits$model_formulas)

    ## sigma values
    mysigma <- rep(list(),length(fits$model_fits))
    
    for (i in 1:length(fits$model_fits)){
         mysigma[[i]] <- round(summary(fits$model_fits[[i]])$sigma,4)
    }
    
    names(mysigma) <-  names(fits$model_formulas)


    ## R-squared values
    myrsq <- rep(list(),length(fits$model_fits))
    
    for (i in 1:length(fits$model_fits)){
         myrsq[[i]] <- round(summary(fits$model_fits[[i]])$r.squared,4)
    }
    
    names(myrsq) <-  names(fits$model_formulas)

    ## Adjusted R-squared values
    myrsqadj <- rep(list(),length(fits$model_fits))
    
    for (i in 1:length(fits$model_fits)){
         myrsqadj[[i]] <- round(summary(fits$model_fits[[i]])$adj.r.squared,4)
    }
    
    names(myrsqadj) <-  names(fits$model_formulas)

    ## AICc values
    usePackage("AICcmodavg")
    myaicc <- rep(list(),length(fits$model_fits))
    
    for (i in 1:length(fits$model_fits)){
         myaicc[[i]] <- round(AICc(fits$model_fits[[i]]),2)
    }
    
    
    ## log likelihood (logLik)   
    myloglik <- rep(list(),length(fits$model_fits))
     
    for (i in 1:length(fits$model_fits)){
         myloglik[[i]] <- round(as.numeric(logLik(fits$model_fits[[i]])),2)
    }
    
    ## number of parameters
    myparams <- rep(list(),length(fits$model_fits))
     
    for (i in 1:length(fits$model_fits)){
          
         myparams[[i]] <- attr(logLik(fits$model_fits[[i]]),"df") # subtract one df for estimating the error variance                                    
                                                                  # if trying to compute number of degrees of freedom for 
                                                                  # the model (i.e., number of regression coefficients)
    }
    
    
    names(myaicc) <-  names(fits$model_formulas)

    res <- list(formulas=myformulas,coefs=mycoefs,sigma=mysigma,rsq=myrsq, rsqadj=myrsqadj,params=myparams,loglik=myloglik,aicc=myaicc)
    
    return(res)
}
                                     

SIMPLESIBREG$results_model_selection <-  SIMPLESIBREG$model_selection_results(SIMPLESIBREG$fits)

## SIMPLESIBREG$results_model_selection 

## str(SIMPLESIBREG$results_model_selection)


## eliminate original function model_selection_table()?

SIMPLESIBREG$model_selection_table_updated <- function(results_model_selection){

   ## dims <- unlist(lapply(results_model_selection$coefs, function(ll){length(ll)}))
   
   tmpres <- matrix(NA, ncol = 8, nrow=length(results_model_selection$formulas))
   
   colnames(tmpres) <- c("Age_Class","Model","sigma","Rsq","Adj_Rsq","Num_Param","Log_Lik","AICc")  
   
   tmpres <- as.data.frame(tmpres)
        
    
   for (i in 1:length(results_model_selection$formulas)) {
   
        ## fill tmpres with ages 
        tmpres[i ,"Age_Class"] <- names(results_model_selection$coefs)[i]
        
        
        ## fill tmpres with model formulas
        tmp <- as.character(unlist(results_model_selection$formulas[[i]]))
        tmp <- paste(tmp[2],tmp[1],tmp[3])
        usePackage("stringr")
        tmp <- str_replace_all(tmp, "NA", "")
        tmp <- trimws(tmp, "both")
        tmpres[i,"Model"] <- tmp   
        
        ## fill tmpres with sigma values 
        tmpres[i,"sigma"] <-  unlist(results_model_selection$sigma[[i]])
        
        ## fill tmpres with R squared values 
        tmpres[i,"Rsq"] <-  unlist(results_model_selection$rsq[[i]])
        
        ## fill tmpres with adjusted R squared values 
        tmpres[i,"Adj_Rsq"] <-  unlist(results_model_selection$rsqadj[[i]])
        
   
        ## fill tmpres with Num_Param values (i.e., number of parameters)
        tmpres[i,"Num_Param"] <-  unlist(results_model_selection$params[[i]])
        
        
        ## fill tmpres with the Log Likelihood values 
        tmpres[i,"Log_Lik"] <-  unlist(results_model_selection$loglik[[i]])
        
   
        ## fill tmpres with AICc values
        tmpres[i,"AICc"] <-  unlist(results_model_selection$aicc[[i]])
        
   }
   
   return(tmpres)
}



SIMPLESIBREG$model_selection_table_updated(SIMPLESIBREG$results_model_selection)



SIMPLESIBREG$model_selection_delta_AIC_and_Akaike_weights <- function(results_model_selection){
  
      myres <- SIMPLESIBREG$model_selection_table_updated(results_model_selection)

      ages <- sort(unique(myres$Age_Class))

      res <- NULL

      for (i in 1:length(ages)){
      
           tmp <- subset(myres, Age_Class==ages[i])
           tmp <- as.data.frame(tmp)
           
           ## compute delta AICc for each model
           tmp$Delta_AICc <- tmp$AICc - min(tmp$AICc)
           
           ## compute Akaike weight for each model
           
           tmp$Akaike_Weight <-  exp(-tmp$Delta_AICc/2)/sum(exp(-tmp$Delta_AICc/2))
           
           tmp$Akaike_Weight <- round(tmp$Akaike_Weight, 2)
           
           tmp
           
           res <- rbind(res, tmp)
           
      }
      
      return(res)
}


## not sure yet how to handle model selection - single best model or model averaging?
## For now: choose the model with Delta AIC = 0 and with largest Akaike weight

SIMPLESIBREG$model_weights <- SIMPLESIBREG$model_selection_delta_AIC_and_Akaike_weights(SIMPLESIBREG$results_model_selection)

SIMPLESIBREG$model_weights

## usePackage("stringr")
## model_weights$Model <- str_replace_all(model_weights$Model,"NA ","")
## model_weights$Model <- str_replace_all(model_weights$Model," NA","")


SIMPLESIBREG$select_best_fitting_model_for_each_age_class <- function(results_model_selection){

   mysel <- SIMPLESIBREG$model_selection_delta_AIC_and_Akaike_weights(results_model_selection)

   ages <- sort(unique(mysel$Age_Class))

   best_model_for_each_age_class <- NULL
   for (i in 1:length(ages)){
      
           tmp <- subset(mysel, Age_Class==ages[i])
           tmp <- as.data.frame(tmp) 
    
           ## best_tmp <- subset(tmp, Delta_AICc==0 & Akaike_Weight==max(Akaike_Weight) & Num_Param==min(Num_Param))
          
           best_tmp <- subset(tmp, Delta_AICc==0)  
          
           if(nrow(best_tmp)>1) { best_tmp <- best_tmp[1,]}  ## choose the first model stored in best_tmp 
                                                             ## in case several other models are competing for 
                                                             ## the role of "best fitting model"    
        
        best_model_for_each_age_class <- rbind(best_model_for_each_age_class, best_tmp) 
    
    }
                                                                                      
    best_model_for_each_age_class 
    
    return(best_model_for_each_age_class)
    
}

SIMPLESIBREG$best_fitting_model_for_each_age_class <-  SIMPLESIBREG$select_best_fitting_model_for_each_age_class(SIMPLESIBREG$results_model_selection)

## model summary for "best fitting model" for each age class 

SIMPLESIBREG$summary_output_best_fitting_model_for_each_age_class <- function(fits, results_model_selection){

      best_fitting_model_for_each_age_class <-  SIMPLESIBREG$select_best_fitting_model_for_each_age_class(results_model_selection)

      ages <- best_fitting_model_for_each_age_class$Age_Class

      results <- list()
      counter <- 0 
      for (i in 1:length(ages)){
           
           mydata <-  fits$model_data[[ages[i]]]
           myformula <- subset(best_fitting_model_for_each_age_class, Age_Class==ages[i])$Model
           usePackage("stringr")
           myformula <- str_replace_all(myformula,"NA ","")
           myformula <- str_replace_all(myformula," NA","")
           myformula <- as.formula(myformula)
           mymodel <- lm(formula=myformula, data=mydata)
           mysummary <- summary(mymodel)
           
           mylist <- list(data=mydata, formula=myformula, model=mymodel, summary=mysummary)
               
           results[[counter <- counter + 1]] <- mylist    
               
      }

      results
      
      names(results) <- ages
      
      results
      
}


SIMPLESIBREG$results_best_fitting_model_for_each_age_class <- SIMPLESIBREG$summary_output_best_fitting_model_for_each_age_class(SIMPLESIBREG$fits, SIMPLESIBREG$results_model_selection)


SIMPLESIBREG$format_results_best_fitting_model_for_each_age_class <- function(results_best_fitting_model_for_each_age_class,i) {

    model <- results_best_fitting_model_for_each_age_class[[i]]$summary
   
    s = model$coefficients
    s = as.data.frame(s)

    # get significance codes for the P-values reported in the model summary 
    signif.codes = cut(s[, "Pr(>|t|)"], 
                                    breaks = c( -Inf, 0.001, 0.01, 0.05, Inf), 
                                    labels= c("***", "**", "*", "" ) )

   # format the numerical values in the model summary 
   s[, "Estimate"] = formatC(s[, "Estimate"], digits=3, format = "f")
   s[, "Std. Error"] = formatC(s[, "Std. Error"], digits=3, format = "f")
   s[, "t value"] = formatC(s[, "t value"], digits=3, format = "f")
   s[, "Pr(>|t|)"] = ifelse(s[, "Pr(>|t|)"] < 0.001, "< 0.001", 
                                             formatC(s[, "Pr(>|t|)"], digits=5, format = "f"))
   # add signif codes to the P-values reported in the model summary 
   s$Signif = signif.codes
   
   
   ## extract predictor variables
   ## usePackage("formula.tools")
   ## var <- attr(model$terms, "term.labels")
   ## vars <- rhs.vars(formula(model))
   ## extract response variable
   ## resp <- lhs.vars(formula(model))
   ## form <- as.character(formula(model))
   
   ## s$Age_Class <- resp 
   ## s$Best_Model <- form
   
   ## ss <- cbind.data.frame(subset(s, select=c(Age_Class,Best_Model)),
   ##                       subset(s, select=-c(Age_Class,Best_Model)))
   ## rownames(ss) <- NULL
   
   ## ss
   
   ss1 <- data.frame(Variable=rownames(s))
   ss2 <- s
   ss <- cbind.data.frame(ss1,ss2)
   rownames(ss) <- NULL
   
   ss

}


SIMPLESIBREG$format_results_best_fitting_model_for_each_age_class(SIMPLESIBREG$results_best_fitting_model_for_each_age_class,i=1)


 
#--------- point forecast for each individual age and for the total age ----------------------

## http://science.nature.nps.gov/im/datamgmt/statistics/r/advanced/ReproducibleReporting.cfm

SIMPLESIBREG$point.forecast.best.fitting.model.for.each.age.class <- 
    function(results_best_fitting_model_for_each_age_class, forecastingyear, datafile_variables){
    
     PSY <- forecastingyear
     myfits <- results_best_fitting_model_for_each_age_class
    
     original.data <-   datafile_variables   # include just the predictors
     
     ll <- original.data

     for (k in 1:length(ll)){
    
        tmpdf <- do.call(rbind.data.frame, ll[k])
        rownames(tmpdf) <- 1:nrow(tmpdf)

        names(tmpdf)[names(tmpdf)=="Run_Year"] <- paste0("Run_Year_",names(tmpdf)[length(names(tmpdf))])

        tmpdf1 <- subset(tmpdf, select=Brood_Year)
        tmpdf2 <- subset(tmpdf, select=-Brood_Year)
        tmpdf3 <- cbind.data.frame(tmpdf1, tmpdf2)

        df <- tmpdf3

        if (k == 1){

          mergedf <- df

        } else {

          mergedf <- merge(mergedf, df, by="Brood_Year", all=TRUE)

      }

      }
       
      original.data <- mergedf
    
    
     output <- vector("list",length(myfits))
          
     nms <- NULL
     for (j in 1:length(myfits)) { 

         ## fits[[j]]$model

         model <- myfits[[j]]$model
         
         ## x <- myfits[[j]]$data 
         x <- original.data

         # extract predictor variables
         usePackage("formula.tools")
         ## var <- attr(model$terms, "term.labels")
         vars <- rhs.vars(formula(model))
         ## extract response variable
         resp <- lhs.vars(formula(model))
    
         ## paste0("Run_Year_",resp)
    
         ## Hadley Wickham: http://adv-r.had.co.nz/Computing-on-the-language.html#subset
         subset2 <- function(x, condition) {
            condition_call <- substitute(condition)
            r <- eval(condition_call, x, parent.frame())
            x[r, ]
         }

         condition <- paste(paste0("Run_Year_",vars), "==", PSY-1)     # Does this work as intended?  
         
         mylist <- list()
         for (i in 1:length(condition)){
               sub <- subset2(x=x,eval(parse(text=condition[i])))
               sub <- subset(sub, select=vars[i])
               sub <- na.omit(sub)
               sub
               mylist[[i]] <- sub  
         }
         
         mydf <- as.data.frame(mylist)
         
          
         ## sub <- as.numeric(sub)

         ## new <- eval(parse(text=paste("data.frame(",var,"=",sub,")")))    

         form <- as.character(formula(model))

         
         
         pst <- paste("Brood_Year",paste0("Run_Year_",resp), resp, paste0("Run_Year_",vars),vars)
         pstspl <- unlist(strsplit(pst," "))  
         
         xsub <- x[,colnames(x) %in% pstspl]
          
         output[[j]]$Data <- xsub
          
         ## xsub <- subset(x,eval(parse(text=pstspl)))
          
         output[[j]]$Model <- form
         
         ## output[[j]]$p <- predict(model, newdata=new) 
         output[[j]]$PointForecast <- predict(model, newdata=mydf) 

         ## nms <- c(nms, var <- names(model.frame(fits[[j]]$model))[1])          

         nms <- c(nms, resp)
     }

     names(output) <- nms

     return(output)
}

 
SIMPLESIBREG$point_forecast_best_model_for_each_age_class <- 
SIMPLESIBREG$point.forecast.best.fitting.model.for.each.age.class(SIMPLESIBREG$results_best_fitting_model_for_each_age_class, 
                                                            SIMPLESIBREG$forecastingyear, SIMPLESIBREG$datafile_variables)
 



#*******************************************************************************
#  barplot forecasted values (specific ages)
#
#*******************************************************************************


SIMPLESIBREG$barplot.forecasted.values.individual.ages.simplesib <- function(point_forecast_best_model_for_each_age_class, 
                                                                 forecastingyear, i){
    
        .e <- environment()
    
        myfits <- point_forecast_best_model_for_each_age_class 
        
        ages <- names(myfits) 
    
        ### starts here

         form <- myfits[[i]]$Model
         form <- as.formula(form)
         # extract predictor variables
         usePackage("formula.tools")
         ## var <- attr(model$terms, "term.labels")
         vars <- rhs.vars(form)
         ## extract response variable
         resp <- lhs.vars(form)
        
         age <- resp
    
         usePackage("stringr")
         usePackage("scales")
        
         age <- str_replace(age, "_"," ")
        
         age 
        
         ## years <- myfits[[i]]$Data[,2]
         
         years <- subset(myfits[[i]]$Data, select=eval(parse(text=paste0("Run_Year_",resp))))

         years <- na.omit(years)
        

         ## retropointforecasts <-  myfits[[i]]$Data[,3]  # historic data ?
         
         retropointforecasts <-  subset(myfits[[i]]$Data, select=eval(parse(text=paste0(resp))))
         retropointforecasts <- na.omit(retropointforecasts)
        
        
         p <- myfits[[i]]$PointForecast

         p <- as.numeric(p)
        
         p <- ifelse(p>0,p,0)

         p <- round(p)

        ## p <- round(p)
        
         pointforecast <- p

         forecastingyear <- forecastingyear

       
        dfretro <- data.frame(years,retropointforecasts)
        names(dfretro) <- c("years","retropointforecasts")
      
        dffor <- data.frame(forecastingyear,pointforecast)
     
        usePackage("ggplot2")
       
        gp1 <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts), environment=.e) + 
             geom_rect(aes(x=NULL, y=NULL, xmax=years-1/3,
               xmin=years+1/3,
               ymax=retropointforecasts,
                ymin=0),fill="dodgerblue3") +
        geom_text(data=dfretro,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(dfretro$retropointforecasts)))),
               colour="dodgerblue3",angle=90,size=2.5,hjust=0,vjust=0.2) +
        geom_rect(data=dffor, aes(x=NULL, y=NULL, xmax=forecastingyear-1/3,
               xmin=forecastingyear+1/3,
               ymax=pointforecast,
                ymin=0),fill="violetred2") +
        geom_text(data=dffor,aes(x=forecastingyear,
               y=pointforecast,
               label=paste(comma(round(dffor$pointforecast)))),
               colour="violetred2",angle=90,size=2.5,hjust=0,vjust=0.2) +
          geom_segment(data=dfretro,aes(x = years[1]-0.5, 
                     y = mean(dfretro$retropointforecasts),
                     xend = forecastingyear+0.5, 
                     yend = mean(dfretro$retropointforecasts)),
                     colour="grey15",linetype=1) +
        annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retropointforecasts,dffor$pointforecast)),
               label=paste("Historical Average =",comma(round(mean(dfretro$retropointforecasts)))), 
               size=2.5, colour="grey15",hjust=0,vjust=0) + 
        scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +
        ## scale_y_continuous("Terminal Run",label=comma, breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
        scale_y_continuous(paste(SIMPLESIBREG$stockabundance),label=comma, 
        breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
        ## labs(title=paste(age)) +  
        ggtitle(label=paste(age)) + 
        theme(plot.title=element_text(size=12, hjust=0.5),
              axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5), 
              axis.text.y=element_text(size=8), 
              axis.title.x=element_text(size=10,vjust=-0.5),
              axis.title.y=element_text(size=10,vjust=1.5),      
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))  +
             expand_limits(y=1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast)))

       
      gp2 <- ggplot(data=dfretro, aes(years,retropointforecasts),environment=.e) + 
      geom_line(data=dfretro, aes(years,retropointforecasts),col="dodgerblue3") + 
      geom_segment(data=dfretro, aes(x = years[length(years)], 
                     y = retropointforecasts[length(retropointforecasts)],
                     xend = dffor$forecastingyear, 
                     yend = dffor$pointforecast),
                     colour="violetred2",linetype=2,size=0.7) +
      geom_point(data=dfretro, aes(years,y=retropointforecasts),colour="dodgerblue3",size=2) +
      geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="violetred2",size=2) + 
      geom_text(data=dffor, aes(x=forecastingyear,
                   y=pointforecast,
                   label=paste(comma(round(pointforecast)))),
                   # colour="red",angle=90,vjust=2,size=3) + 
                   colour="violetred2",angle=90,hjust=-0.5,vjust=0,size=2.5) +
      geom_segment(data=dfretro,aes(x=years[1],
                       y=mean(retropointforecasts),
                       xend=forecastingyear,
                       yend=mean(retropointforecasts)), colour="grey15") +
      ## scale_y_continuous("Terminal Run",labels=comma, breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
      scale_y_continuous(paste(SIMPLESIBREG$stockabundance),labels=comma, 
      breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +  
         scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) + 
         ## geom_text(data=dfretro,aes(x=years[1],
         ##              y=max(dfretro$retropointforecasts),
         ##              label=paste("Historical Average = ",comma(round(mean(dfretro$retropointforecasts),2)))),
         ##          colour="burlywood4",size=3,hjust=-1.2,vjust=1) +        
     annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retropointforecasts,dffor$pointforecast)),
               label=paste("Historical Average =",comma(round(mean(dfretro$retropointforecasts)))), 
               size=2.5,colour="grey15",hjust=0,vjust=0) +
     ## labs(title=paste(age)) +
     ggtitle(label=paste(age)) +
     theme(plot.title=element_text(size=12, hjust=0.5),  
           axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5), 
           axis.text.y=element_text(size=8),  
           axis.title.x=element_text(size=10,vjust=-0.5),
           axis.title.y=element_text(size=10,vjust=1.5),    
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50")) +
     expand_limits(y=1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast)))
     
     ## print(gp2)
     
     usePackage("grid")
     usePackage("gridExtra")
     usePackage("ggplot2")
     
     ## plotlist <- gList(ggplotGrob(gp1),ggplotGrob(gp2))
     ## do.call("grid.arrange", c(plotlist,ncol=1))
      
     p <- arrangeGrob(gp1,gp2) 
     
     p
     
}


## i <- 2

SIMPLESIBREG$myplot <- SIMPLESIBREG$barplot.forecasted.values.individual.ages.simplesib(
                         SIMPLESIBREG$point_forecast_best_model_for_each_age_class, SIMPLESIBREG$forecastingyear, i=2)

plot(SIMPLESIBREG$myplot)


#====================================================================================
# Compute Interval Forecasts
#====================================================================================


SIMPLESIBREG$results_best_fitting_model_for_each_age_class <- SIMPLESIBREG$summary_output_best_fitting_model_for_each_age_class(SIMPLESIBREG$fits, 
                                                              SIMPLESIBREG$results_model_selection)


SIMPLESIBREG$bootstrap.prediction.interval.simple.sibling.regression.no.covariates.updated <- function(results_best_fitting_model_for_each_age_class,j,x0,B){

       print(j)

       olddata <- results_best_fitting_model_for_each_age_class[[j]]$data
       n <- nrow(olddata)
       myformula <- results_best_fitting_model_for_each_age_class[[j]]$formula
       mod.fit <- lm(formula=myformula, data=olddata) ## original model fit

       model.lm <- glm(formula=myformula, data=olddata)

       usePackage("boot")

       model.diag <- glm.diag(model.lm)
       model.res <- model.diag$res * model.diag$sd
       model.res <- model.res - mean(model.res)

       model.data <- data.frame(olddata,resid=model.res,fit=fitted(model.lm))

       new.data <- data.frame(x0)
       
       
       pred.var <-  as.character(myformula[[3]])[3]
       
       names(new.data) <-  pred.var

       new.fit <- predict(model.lm, new.data)


       model.fun <- function(dat, inds, i.pred, fit.pred, x.pred, pred.var){

          assign(".inds", inds, envir=.GlobalEnv)
          
          lm.form <- as.formula(paste("fit+resid[.inds]","~","-1 + ",pred.var))
          
          ## lm.b <- glm(fit+resid[.inds] ~ x, data=dat)

          lm.b <- glm(lm.form, data=dat)

          pred.b <- predict(lm.b,x.pred)
          remove(".inds", envir=.GlobalEnv)
          c(coef(lm.b), pred.b-(fit.pred+dat$resid[i.pred]))

      }


       model.boot <- boot(model.data, model.fun, R=B, m=1,fit.pred=new.fit, x.pred=new.data, pred.var = pred.var)

       q1 <- B*0.10      # 975
       q2 <- B*(1-0.10)  # 25
       lwr.upr.boot <- new.fit - sort(model.boot$t[,2])[c(q1,q2)]

       lwr.boot <- min(lwr.upr.boot)
       upr.boot <- max(lwr.upr.boot)


       y.star.boot <- new.fit - sort(model.boot$t[,2])

       return(list(y.star.boot=y.star.boot, lwr.boot=lwr.boot, upr.boot=upr.boot))

}


## SIMPLESIBREG$bootstrap.prediction.interval.simple.sibling.regression.no.covariates <- function(results_best_fitting_model_for_each_age_class,j,x0,B){
##
##       print(j)
##
##       olddata <- results_best_fitting_model_for_each_age_class[[j]]$data
##       n <- nrow(olddata)
##       myformula <- results_best_fitting_model_for_each_age_class[[j]]$formula
##       mod.fit <- lm(formula=myformula, data=olddata) ## original model fit
##
##       r <- mod.fit$residuals  ## residuals
##       influence.stat <- lm.influence(mod.fit)
##       h.j <- influence.stat$hat
##       r.j <- mod.fit$residuals/sqrt(1-h.j) ## modified residuals
##       res <- r.j
##
##       y.hat <- coef(mod.fit)%*%t(x0)
##
##       delta <- NULL
##       
##       while (length(delta) < B){ 
##       ## for (i in 1:B){
##
##          ## resample data (case resampling)
##          index <- sample(1:n, size=n, replace = TRUE)
##          bootdata <- olddata[index,]  # case resampling
##
##          ## fit model to bootstrap data obtained by resampling cases
##          mod.fit.star <- lm(myformula, data = bootdata)
##
##          ## fitted value obtained from mod.fit.star
##          y.hat.star <- coef(mod.fit.star)%*%t(x0)
##
##          ## resample residual
##          i.pred <- sample(1:n, size=1)
##          epsilon.plus.star <- res[i.pred]
##
##          ## calculate prediction error delta (delta is the prediction error)
##          ## delta <- c(delta,
##          ##           y.hat.star - (y.hat + epsilon.plus.star))
##
##          if (y.hat.star - (y.hat + epsilon.plus.star) <= y.hat) {
##              delta <- c(delta,
##                     y.hat.star - (y.hat + epsilon.plus.star))
##          }
##
##
##       } # end for loop
##
##
##       ## hist(delta)
##
##       delta.quant <- quantile(delta,  probs = c(0.10, 0.90), type = 1)
##
##       ## rev(delta.quant)
##
##       ## PI.int <- as.numeric(pred.newdata$fit[1] - rev(delta.quant))
##       PI.int <- as.numeric(y.hat - rev(delta.quant))
##
##       ## hist(pred.newdata$fit[1] - delta)
##
##       ## round(PI.int,2) # 1.54 - 2.75
##
##       y.star.boot <- y.hat - delta
##       lwr.boot <- PI.int[1]
##       upr.boot <- PI.int[2]
##       delta <- delta
##
##       return(list(y.star.boot=y.star.boot, lwr.boot=lwr.boot, upr.boot=upr.boot, delta=delta))
##
##}


## SIMPLESIBREG$bootstrap.prediction.interval.simple.sibling.regression.no.covariates(SIMPLESIBREG$results_best_fitting_model_for_each_age_class,j=1,x0=500,B)


## SIMPLESIBREG$bootstrap.prediction.interval.simple.sibling.regression.no.covariates.updated(SIMPLESIBREG$results_best_fitting_model_for_each_age_class,j=3,x0=1000,B)



#---- "best: sibling regression model forecast -----------------------------------------------------------------



SIMPLESIBREG$best.sibling.regression.model.forecast <- function(datafile_variables, results_best_fitting_model_for_each_age_class, forecastingyear, B){

     myfits <- results_best_fitting_model_for_each_age_class
  
     original.data <-   datafile_variables   # include just the predictors
     
     ll <- original.data

     for (k in 1:length(ll)){
    
        tmpdf <- do.call(rbind.data.frame, ll[k])
        rownames(tmpdf) <- 1:nrow(tmpdf)

        names(tmpdf)[names(tmpdf)=="Run_Year"] <- paste0("Run_Year_",names(tmpdf)[length(names(tmpdf))])

        tmpdf1 <- subset(tmpdf, select=Brood_Year)
        tmpdf2 <- subset(tmpdf, select=-Brood_Year)
        tmpdf3 <- cbind.data.frame(tmpdf1, tmpdf2)

        df <- tmpdf3

        if (k == 1){

          mergedf <- df

        } else {

          mergedf <- merge(mergedf, df, by="Brood_Year", all=TRUE)

      }

      }
       
      original.data <- mergedf
     
     
     ## usePackage("stringr") 
     ## names(original.data) <- str_replace_all(names(original.data)," ","_")
     


     output <- vector("list", length(myfits)) # start to build the S3 class storing the output 
     ## class(output) <- "asslr"  # create a new class
   
     ## PSY <- forecastingyear

     nms <- NULL
     for (j in 1:length(myfits)) { 

         model <- myfits[[j]]$model
          
         model.data <- myfits[[j]]$data

         # names(model.frame(myfits[[j]]$model))

         # extract names of outcome and predictor variable(s) 
         allvars <- names(model.frame(myfits[[j]]$model))
         allvars <-  c(allvars,paste0("Run_Year_",allvars))
         # extract name(s) of predictor variable(s) 
         vars <- names(model.frame(myfits[[j]]$model))[-1]
    
         # extract outcome + predictor variables from model data
         subset.model.data <- subset(model.data, select=c("Brood_Year",allvars))  
         
         # extract just the predictor variable(s) from original data
         subset.original.data <- subset(original.data,select=c("Brood_Year",vars))   
         
    
         ## last.observed.brood.year <- subset.original.data$Brood_Year[length(subset.original.data$Brood_Year)] 
         age.for.outcome.variable <- names(model.frame(myfits[[j]]$model))[1]
         ## split string at non-digits
         age.for.outcome.variable <- strsplit(age.for.outcome.variable, "[^[:digit:]]")
         ## convert strings to numeric ("" become NA)
         age.for.outcome.variable <- as.numeric(unlist(age.for.outcome.variable))
         ## remove NA and duplicates
         age.for.outcome.variable <- unique(age.for.outcome.variable[!is.na(age.for.outcome.variable)])
         mybroodyear <- forecastingyear - age.for.outcome.variable
         
         newdata <- subset(subset.original.data, Brood_Year== mybroodyear)
         newdata
         newdata <- subset(newdata, select=vars)
         newdata
    
         x0 <- newdata
    
         newdata <- eval(parse(text=paste("data.frame(",vars,"=",newdata,")")))    

         form <- as.character(formula(model))

         output[[j]]$Model <- paste(form)

         output[[j]]$p <- as.numeric(predict(model, newdata=newdata)) 
         
       
         ## tmp.boot <- SIMPLESIBREG$bootstrap.prediction.interval.simple.sibling.regression.no.covariates(results_best_fitting_model_for_each_age_class,j,x0,B)

         
         tmp.boot <- SIMPLESIBREG$bootstrap.prediction.interval.simple.sibling.regression.no.covariates.updated(results_best_fitting_model_for_each_age_class,j,x0,B)

         output[[j]]$p.lwr <- tmp.boot$lwr.boot
       
         output[[j]]$p.upr <- tmp.boot$upr.boot
                                                                  
         output[[j]]$y.star.boot <- tmp.boot$y.star.boot
         output[[j]]$delta <- tmp.boot$delta   # forecast error (defined as predicted minus actual)
         
         output[[j]]$x0 <- x0
         output[[j]]$model.boot <- tmp.boot$model.boot ## save object produced by Boot() function in car package
         
         output[[j]]$model.formula <- form
         output[[j]]$model.data <- subset.model.data
        
     }

      
     return(output)

}


## B <- 9999

SIMPLESIBREG$B <- B

SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression <- SIMPLESIBREG$best.sibling.regression.model.forecast(SIMPLESIBREG$datafile_variables, 
                                                                                              SIMPLESIBREG$results_best_fitting_model_for_each_age_class, 
                                                                                              SIMPLESIBREG$forecastingyear, 
                                                                                              B=SIMPLESIBREG$B)

SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression[[2]]$p



###
### Histogram of Bootstrap Predictions: Best Fitting Models
###

SIMPLESIBREG$plot.yboot.simple.sibling.regression.best.fitting.models <- function(best.fits, pred.int.individual.ages.simple.sibling.regression){

    .e = environment()

    y.star.boot.stacked <- NULL
    labels.stacked <- NULL

      for (j in 1:length(pred.int.individual.ages.simple.sibling.regression)){

           y.star.boot.stacked <- c(y.star.boot.stacked, pred.int.individual.ages.simple.sibling.regression[[j]]$y.star.boot)
           mylabel <-  as.character(formula(best.fits[[j]]$model))
           labels.stacked <- c( labels.stacked, rep(mylabel, length(pred.int.individual.ages.simple.sibling.regression[[j]]$y.star.boot)) )

          }

    data.stacked <- data.frame(y.star.boot=y.star.boot.stacked,
                               labels=labels.stacked)

    usePackage("plyr")

    data.stacked <- ddply(data.stacked, c('labels'))
    data.stacked$labels <- as.factor(data.stacked$labels)

    usePackage("ggplot2")
    usePackage("scales")


    d <- data.stacked

    l <- levels(d$labels)

    breaks <- NULL
    for (j in 1:length(best.fits)){
        h <- hist(data.stacked$y.star.boot[data.stacked$labels==levels(data.stacked$labels)[j]],plot=FALSE, breaks = "Freedman-Diaconis")
        ## breaks[[j]] <- h$breaks
        h.tmp <- seq(from=min(h$breaks), to=max(h$breaks), by = unique(diff(h$breaks))) 
        breaks[[j]] <- h.tmp
    }

    ## g <- ggplot(d, aes(x=residuals), environment=.e) +
     g <- ggplot(d, aes(x=y.star.boot),environment=.e) +
           mapply(function(d, b) {geom_histogram(data=d, breaks=b, fill="wheat",colour="black")},
            split(d, d$labels), breaks) +
             facet_wrap(~ labels,  scales="free", ncol=1) +
               expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Frequency"),labels=comma) +
                   scale_x_continuous(paste("Bootstrapped Point Forecasts"),labels=comma)

      g = g + theme_bw() +
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))
                
     clim <- NULL
     for (j in 1:length(pred.int.individual.ages.simple.sibling.regression)){

        tmp <- pred.int.individual.ages.simple.sibling.regression[[j]]$p
        tmp <- round(tmp)
        clim <- c(clim, tmp)

     }

     dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))

     g = g + geom_vline(aes(xintercept = z), data=dummy2, linetype="dashed",col="red", size=1)

     x <- NULL
     xend <- NULL
     y <- NULL
     yend <- NULL
     for (j in 1:length(pred.int.individual.ages.simple.sibling.regression)){
         x <- c(x, max(0,pred.int.individual.ages.simple.sibling.regression[[j]]$p.lwr))
         xend <- c(xend, round(pred.int.individual.ages.simple.sibling.regression[[j]]$p.upr))
         y <- c(y,0)
         yend <- c(yend,0)
     }

     dummy3 <- data.frame(x=x,xend=xend,y=y,yend=yend, labels = levels(data.stacked$labels))
     g = g + geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data=dummy3, linetype="solid",col="blue", size=1)



     return(g)
}



SIMPLESIBREG$results_best_fitting_model_for_each_age_class <- SIMPLESIBREG$summary_output_best_fitting_model_for_each_age_class(SIMPLESIBREG$fits, 
                                                              SIMPLESIBREG$results_model_selection)

SIMPLESIBREG$best.fits <-  SIMPLESIBREG$results_best_fitting_model_for_each_age_class

SIMPLESIBREG$plot.yboot.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits, SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression)



#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------



SIMPLESIBREG$PI.ctr <- NULL
SIMPLESIBREG$PI.lwr <- NULL
SIMPLESIBREG$PI.upr <- NULL 
# PI.med <- NULL
SIMPLESIBREG$PI.sim <- NULL
SIMPLESIBREG$nms <- NULL

for (k in 1:length(SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression)){

     SIMPLESIBREG$PI.ctr <- c(SIMPLESIBREG$PI.ctr, 
                 SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression[[k]]$p) 

     SIMPLESIBREG$PI.lwr <- c(SIMPLESIBREG$PI.lwr, 
                 SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression[[k]]$p.lwr) 
       
     SIMPLESIBREG$PI.upr <- c(SIMPLESIBREG$PI.upr,
                 SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression[[k]]$p.upr)
                 
     # PI.med <- c(PI.med,
     #            pred.int.individual.ages.expsmooth[[k]]$PI.median)            

     SIMPLESIBREG$PI.sim <- cbind(SIMPLESIBREG$PI.sim, SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression[[k]]$y.star.boot)
     
     SIMPLESIBREG$form <-  as.formula(SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression[[k]]$model.formula)
     usePackage("formula.tools")
     
     SIMPLESIBREG$nms <- c(SIMPLESIBREG$nms, lhs.vars(SIMPLESIBREG$form))
     
}

colnames(SIMPLESIBREG$PI.sim) <- SIMPLESIBREG$nms


SIMPLESIBREG$PI.lwr[SIMPLESIBREG$PI.lwr < 0] <- 0 
SIMPLESIBREG$PI.upr[SIMPLESIBREG$PI.upr < 0] <- 0 
## PI.med[PI.med < 0] <- 0 

SIMPLESIBREG$PI.ctr <- round(SIMPLESIBREG$PI.ctr)
SIMPLESIBREG$PI.lwr <- round(SIMPLESIBREG$PI.lwr)
SIMPLESIBREG$PI.upr <- round(SIMPLESIBREG$PI.upr)
## PI.med <- round(PI.med)


SIMPLESIBREG$PI.individual.ages.simple.sibling.regression <- data.frame(PI.ctr=SIMPLESIBREG$PI.ctr, PI.lwr=SIMPLESIBREG$PI.lwr, PI.upr=SIMPLESIBREG$PI.upr)

## PI.individual.ages.expsmooth.no.comma <- data.frame(PI.ctr=PI.ctr, PI.med=PI.med, PI.lwr=PI.lwr, PI.upr=PI.upr)

SIMPLESIBREG$PI.individual.ages.simple.sibling.regression.no.comma <- data.frame(PI.ctr=SIMPLESIBREG$PI.ctr, PI.lwr=SIMPLESIBREG$PI.lwr, PI.upr=SIMPLESIBREG$PI.upr)


## PI.individual.ages.simple.sibling.regression

usePackage("scales")

SIMPLESIBREG$PI.individual.ages.simple.sibling.regression <- comma(SIMPLESIBREG$PI.individual.ages.simple.sibling.regression)

## PI.individual.ages.simple.sibling.regression

SIMPLESIBREG$PI.individual.ages.simple.sibling.regression.sim <- SIMPLESIBREG$PI.sim



##########################################################################################
#
#  Plot distribution of bootstrapped point forecasts - individual ages 
#
##########################################################################################

SIMPLESIBREG$plot.distribution.bootstrapped.point.forecasts.individual.ages.simple.sibling.regression  <- function(PI.individual.ages.simple.sibling.regression.sim,
                                                                                   PI.individual.ages.simple.sibling.regression.no.comma){

   PI.individual.ages.simple.sibling.regression.sim <- as.data.frame(PI.individual.ages.simple.sibling.regression.sim)
  
   par(mfrow=c(2, ceiling(ncol(PI.individual.ages.simple.sibling.regression.sim)/2)), mar=c(4,4,4,3))

   for (i in 1:ncol(PI.individual.ages.simple.sibling.regression.sim)){

        h <- hist(PI.individual.ages.simple.sibling.regression.sim[,i], plot=FALSE, breaks = "Freedman-Diaconis")
        hist(PI.individual.ages.simple.sibling.regression.sim[,i], prob=FALSE, col="papayawhip", main="", xaxt="n",
             xlab="Bootstrapped Point Forecast", las=1, breaks = "Freedman-Diaconis")     
        # lines(density(PI.individual.ages.arima.sim[,i], adjust=2), col="red")

        abline(v=PI.individual.ages.simple.sibling.regression.no.comma[i,"PI.ctr"],col="red",lty=2,lwd=2)
        segments(PI.individual.ages.simple.sibling.regression.no.comma[i,"PI.lwr"],
                 0,
                 PI.individual.ages.simple.sibling.regression.no.comma[i,"PI.upr"],
                 0,col="blue",lwd=4)

        age <- names(PI.individual.ages.simple.sibling.regression.sim)[i]
        usePackage("scales")
        age <- str_replace_all(age,"_"," ")
        
        ## age <- gregexpr("[0-9]+", age)[[1]][1]

        usePackage("scales")
        axis(1, at=pretty(h$breaks), labels=comma(pretty(h$breaks)))
        
        title(main=paste("Distribution of Bootstrapped Point Forecasts \n for the ",age,
               " Component of Terminal Run", sep=""),cex.main=1)
               
        box()

   }
  
}

SIMPLESIBREG$plot.distribution.bootstrapped.point.forecasts.individual.ages.simple.sibling.regression(SIMPLESIBREG$PI.individual.ages.simple.sibling.regression.sim,
                                                                    SIMPLESIBREG$PI.individual.ages.simple.sibling.regression.no.comma)


###################################################################################################################################
#**********************************************************************************************************************************
#
#---- plot forecasted values & forecast intervals:  scatterplot (individual ages) -------------------------------------------------
#
#**********************************************************************************************************************************
###################################################################################################################################


SIMPLESIBREG$scatterplot.forecasted.values.and.forecast.intervals.individual.ages.simple.sibling.regression <- function(bestfits, pointforecasts, intervalforecasts, forecastingyear, i){
  
     .e <- environment()
    
     ages <- names(bestfits) 
  
     ## par(mfrow=c(length(fits),1),mar=c(4,4,3,2), oma=c(2,2,2,6),cex.main=1.1)

     ## for (i in 1:length(fits)) {
    
        ### starts here
      
        age <- ages[i]
    
       
    
        usePackage("stringr")
        
        ## age <- str_replace(age, "_"," ")
        
        agetmp <- str_replace(age, " ","_")
        
        numeric_age <- as.numeric(gsub("[^\\d]+", "", age, perl=TRUE)) 
        
        years <- bestfits[[i]]$data$Brood_Year  + numeric_age

        ## retropointforecasts <-  fits[[i]]$model.data[,3]  # historic data ?
        retropointforecasts <- subset(bestfits[[i]]$data, select=agetmp)
        retropointforecasts <- as.numeric(unlist(retropointforecasts))

        p <-  as.numeric(pointforecasts[[i]]$PointForecast)

        ## p <- round(p)
        
        pointforecast <- round(p)

        forecastingyear <- forecastingyear

       
        dfretro <- data.frame(years=years,retropointforecasts=retropointforecasts)
      
      
        upper <- as.numeric(intervalforecasts[i,"PI.upr"])
        lower <- as.numeric(intervalforecasts[i,"PI.lwr"])
      
        dffor <- data.frame(forecastingyear,pointforecast,upper,lower)
    
      
    gp <- ggplot(data=dfretro, aes(years,retropointforecasts), environment=.e) + 
          ## ggplot(data=dfretro, aes(years,retropointforecasts)) +
      geom_line(aes(years,retropointforecasts), stat="identity", colour="dodgerblue3") + 
      geom_rect(data=dffor, aes(x=NULL, y=NULL, xmax=forecastingyear+1/3,
                xmin=forecastingyear-1/3,
                ymax=upper,
                ymin=lower),fill="lightgrey") + 
      geom_segment(data=dffor, aes(x = forecastingyear, 
                     y = lower,
                     xend = forecastingyear, 
                     yend = upper),
                     colour="violetred2",linetype=1,size=0.7) + 
      geom_segment(data=dffor, aes(x = forecastingyear-1/3, 
                     y = lower,
                     xend = forecastingyear+1/3, 
                     yend = lower),
                     colour="violetred2",linetype=1,size=0.7) + 
      geom_segment(data=dffor, aes(x = forecastingyear-1/3, 
                     y = upper,
                     xend = forecastingyear+1/3, 
                     yend = upper),
                     colour="violetred2",linetype=1,size=0.7) +
      geom_segment(data=dfretro, aes(x = dfretro$years[length(dfretro$years)], 
                     y = dfretro$retropointforecasts[length(dfretro$retropointforecasts)],
                     xend = dffor$forecastingyear, 
                     yend = dffor$pointforecast),
                     colour="violetred2",linetype=3,size=0.7) +
      geom_point(data=dfretro,aes(x=years,y=retropointforecasts),colour="dodgerblue3",size=2) +
      geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2) + 
      geom_text(data=dffor, aes(x=forecastingyear+1/2,
                   y=pointforecast,
                   ## label=paste(comma(sprintf("%.2f",pointforecast)))),
                   label=paste(comma(round(pointforecast)))),
                   colour="violetred2",angle=0,hjust=0,size=2.5) +
      geom_text(data=dffor, aes(x=forecastingyear+1/2,     # lower end annotation
                   y=lower,
                   ## label=paste(comma(sprintf("%.2f",lower)))),
                   label=paste(comma(round(lower)))),
                   colour="violetred2",angle=0,hjust=0,size=2.5)+
      geom_text(data=dffor, aes(x=forecastingyear+1/2,     # upper end annotation
                   y=upper,
                   ## label=paste(comma(sprintf("%.2f",upper)))),
                   label=paste(comma(round(upper)))),
                   colour="violetred2",angle=0,hjust=0,size=2.5) +
      geom_segment(data=dfretro, aes(x=years[1],
                       y=mean(retropointforecasts),
                       xend=dffor$forecastingyear,
                       yend=mean(retropointforecasts)), colour="grey15") + 
      # scale_y_continuous("Terminal Run",labels=comma) +
      scale_y_continuous(paste(SIMPLESIBREG$stockabundance),labels=comma) +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear), 
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0)) + 
         geom_text(data=dfretro, aes(x=years[1]+1,
                       y=1.1*max(dfretro$retropointforecasts,dffor$upper),
                       label=paste("Historical Average = ",comma(round(mean(dfretro$retropointforecasts))))),
                   colour="grey15",size=2.5,hjust=0.1,vjust=0.1) +
      ## labs(title=paste(age)) +
      ggtitle(label=paste(age)) + 
      theme( plot.title=element_text(size=12, hjust=0.5),
             axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
             axis.text.y=element_text(size=8),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50")) 

     ## print(gp)
  
     return(gp)
  
}





SIMPLESIBREG$bestfits <- SIMPLESIBREG$results_best_fitting_model_for_each_age_class
SIMPLESIBREG$pointforecasts <- SIMPLESIBREG$point_forecast_best_model_for_each_age_class 
SIMPLESIBREG$intervalforecasts <-   SIMPLESIBREG$PI.individual.ages.simple.sibling.regression.no.comma


SIMPLESIBREG$scatterplot.forecasted.values.and.forecast.intervals.individual.ages.simple.sibling.regression(
            SIMPLESIBREG$bestfits, SIMPLESIBREG$pointforecasts, 
            SIMPLESIBREG$intervalforecasts, SIMPLESIBREG$forecastingyear,i=1)


#*******************************************************************************
#  scatterplot matrix of abundance data (specific ages)
#
#*******************************************************************************


SIMPLESIBREG$scatterplot.matrix.individual.ages.simplesib <- function(fits){
    
    .e <- environment()
    
    ## i <- length(fits)  
    ## tmpdata <- fits$model_data[[i]]

    # Begin Brownyn update ------------------------------------------------#     
    usePackage("rlist")
    fits.List <- List(fits)
    tmpdata.List <- fits.List['model_data'] 
    i <- length(tmpdata.List$data$model_data)
    tmpdata <- tmpdata.List$data$model_data[[i]]
    # End Brownyn update ------------------------------------------------#
    
    usePackage("stringr")

    string <- names(tmpdata)
    pattern <- "^Age_" 

    extract.ages <- grepl(pattern, string, ignore.case=T)

    ## string[extract.ages]

    ## extract data 
    tmpdatasub <- subset(tmpdata, select=string[extract.ages])

    ## head(tmpdatasub)

    ## Source for makePairs(): 
    ## http://gastonsanchez.wordpress.com/2012/08/27/scatterplot-matrices-with-ggplot/
    ## Also: http://www.r-bloggers.com/ggplot2-cheatsheet-for-visualizing-distributions/

    # another option
    makePairs <- function(data){
      grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
      grid <- subset(grid, x != y)
      all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
      xcol <- grid[i, "x"]
      ycol <- grid[i, "y"]
      data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol], 
               x = data[, xcol], y = data[, ycol], data)
      }))
      all$xvar <- factor(all$xvar, levels = names(data))
      all$yvar <- factor(all$yvar, levels = names(data))
      densities <- do.call("rbind", lapply(1:ncol(data), function(i) {
      data.frame(xvar = names(data)[i], yvar = names(data)[i], x = data[, i])
      }))
      list(all=all, densities=densities)
    }
 
    # expand tmpdatasub data frame for pairs plot
    gg1 = makePairs(tmpdatasub)
 
    # new data frame mega_gg1
    mega_gg1 = gg1$all
 
    usePackage("scales")

    p <- ggplot(mega_gg1, aes_string(x = "x", y = "y")) + 
          facet_grid(xvar ~ yvar, scales = "free") + 
            geom_point(na.rm = TRUE, alpha=0.5, colour="black") +
              scale_y_continuous("",labels=comma) +
                scale_x_continuous("",labels=comma) +
                  ## stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)), 
                    ## data = gg1$densities, position = "identity", 
                      ## colour = "grey20", geom = "line") + 
                        stat_boxplot(aes(x = x, y=x), data = gg1$densities, colour="violet", alpha=0.9) +
                          theme(plot.title=element_text(size=12),
                                axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
                                axis.text.y=element_text(size=8),
                                axis.title.x=element_text(size=10,vjust=-0.5),
                                axis.title.y=element_text(size=10,vjust=1.5),
                                panel.grid = element_blank(),
                                panel.border=element_blank(),
                                panel.grid.major=element_blank(),
                                panel.grid.minor=element_blank(),
                                axis.text=element_text(colour="black"),
                                panel.background = element_rect(fill='white',colour="grey50")) 

     p
     
}



SIMPLESIBREG$scatterplot.matrix.individual.ages.simplesib(SIMPLESIBREG$fits)




#*******************************************************************************
#  correlation matrix of abundance data (specific ages)
#
#*******************************************************************************

SIMPLESIBREG$correlation.matrix.individual.ages.simplesib <- function(fits){ 
 
    ## i <- length(fits)   
    ## tmpdata <- fits$model_data[[i]]

    # Begin Brownyn update ------------------------------------------------#     
    usePackage("rlist")
    fits.List <- List(fits)
    tmpdata.List <- fits.List['model_data'] 
    i <- length(tmpdata.List$data$model_data)
    tmpdata <- tmpdata.List$data$model_data[[i]]
    # End Brownyn update ------------------------------------------------#

    usePackage("stringr")

    string <- names(tmpdata)
    pattern <- "^Age_" 

    extract.ages <- grepl(pattern, string, ignore.case=T)

    ## string[extract.ages]

    ## extract data 
    tmpdatasub <- subset(tmpdata, select=string[extract.ages])

    cor.matrix <- cor(tmpdatasub) 
    
    cor.matrix.rounded <- round(cor.matrix, 2) 

    usePackage("stringr")
    rownames(cor.matrix.rounded) <-  str_replace_all(rownames(cor.matrix.rounded),"_"," ")
    colnames(cor.matrix.rounded) <-  str_replace_all(colnames(cor.matrix.rounded),"_"," ")
    
    return(cor.matrix.rounded)

}


SIMPLESIBREG$correlation.matrix.individual.ages.simplesib(SIMPLESIBREG$fits)

#######################################################################################################
#######################################################################################################
##
## Model Diagnostics for "Best" Fitting Models
##
#######################################################################################################
#######################################################################################################

## best.fits <- results_best_fitting_model_for_each_age_class


###
### Plot of Residuals vs. Fitted Values: Best Fitting Models
###

SIMPLESIBREG$plot.residuals.simple.sibling.regression.best.fitting.models <- function(best.fits){

    .e = environment()

    residuals.stacked <- NULL
    fitted.stacked <- NULL
    labels.stacked <- NULL

    for (j in 1:length(best.fits)){
           
           residuals.stacked <- c( residuals.stacked, residuals(best.fits[[j]]$model) )
           fitted.stacked <- c( fitted.stacked, fitted(best.fits[[j]]$model) )
           mylabel <-  as.character(formula(best.fits[[j]]$model))
           labels.stacked <- c( labels.stacked, rep(mylabel, length(fitted(best.fits[[j]]$model))))

    }

    data.stacked <- data.frame(residuals=residuals.stacked,
                               fitted=fitted.stacked,
                               labels=labels.stacked)

    usePackage("plyr")

    data.stacked <- ddply(data.stacked, c('labels'))
    data.stacked$labels <- as.factor(data.stacked$labels)

    usePackage("ggplot2")
    usePackage("scales")

    ## g <- ggplot(data.stacked, aes(fitted,residuals),environment=.e)  +
      g <- ggplot(data.stacked, aes(fitted,residuals),environment=.e) +
           geom_abline(intercept=0,slope=0,colour="red",size=0.8) +
           geom_point(colour="blue",alpha=0.5) +
              facet_wrap(~labels, scales="free", ncol = 1) +
                expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Residuals"),labels=comma) +
                   scale_x_continuous(paste("Fitted Values"),labels=comma)



      g = g + theme_bw() +
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))

      ## Source: http://trinkerrstuff.wordpress.com/2012/09/01/add-text-annotations-to-ggplot2-faceted-plot/

        for (j in 1:length(best.fits)){

          m <- best.fits[[j]]$model
          f <- as.character(formula(m))

          index <- as.numeric(which(abs(rstudent(m)) > 2))

          if (length(index) > 0) {

          by <- best.fits[[j]]$data$Brood_Year
          by <- substr(by, start=3, stop=4)

          ann_text <- data.frame(fitted = fitted(m)[index], residuals = residuals(m)[index],
                             labels = factor(f, levels = levels(data.stacked$labels)),
                             text = by[index])

          ## g = g + geom_text(aes(x=fitted,y=residuals,label=text),data = ann_text,hjust=-0.2,vjust=-0.1,size=2.5,colour="magenta")

          g = g + geom_text(aes(x=fitted,y=residuals,label=text),data = ann_text,
                            hjust="inward",vjust="inward",size=2.5,colour="magenta")

          } 

        }

     return(g)
}

SIMPLESIBREG$plot.residuals.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits)


###
### Histogram of Residuals: Best Fitting Models 
###

SIMPLESIBREG$plot.histresid.simple.sibling.regression.best.fitting.models <- function(best.fits){

    .e = environment()

    residuals.stacked <- NULL
    labels.stacked <- NULL

      for (j in 1:length(best.fits)){

           residuals.stacked <- c(residuals.stacked, residuals(best.fits[[j]]$model) )
           mylabel <-  as.character(formula(best.fits[[j]]$model))
           labels.stacked <- c( labels.stacked, rep(mylabel, length(fitted(best.fits[[j]]$model))))

          }
    
    data.stacked <- data.frame(residuals=residuals.stacked,
                               labels=labels.stacked)

    usePackage("plyr")

    data.stacked <- ddply(data.stacked, c('labels'))
    data.stacked$labels <- as.factor(data.stacked$labels)

    usePackage("ggplot2")
    usePackage("scales")


    d <- data.stacked

    l <- levels(d$labels)

    breaks <- NULL
    for (j in 1:length(best.fits)){
        h <- hist(data.stacked$residuals[data.stacked$labels==levels(data.stacked$labels)[j]],plot=FALSE, breaks = "Freedman-Diaconis")
        ## breaks[[j]] <- h$breaks
        h.tmp <- seq(from=min(h$breaks), to=max(h$breaks), by = unique(diff(h$breaks))) 
        breaks[[j]] <- h.tmp
    }

    g <- ggplot(d, aes(x=residuals), environment=.e) +
           mapply(function(d, b) {geom_histogram(data=d, breaks=b, fill="lightblue",colour="black")},
            split(d, d$labels), breaks) +
             facet_wrap(~ labels,  scales="free", ncol=1) +
               expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Frequency"),labels=comma) +
                   scale_x_continuous(paste("Residuals"),labels=comma)

      g = g + theme_bw() +
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))

     return(g)
}

SIMPLESIBREG$plot.histresid.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits)




###
### Density Plots of Residuals: Best Fitting Models
###

## best.fits

SIMPLESIBREG$plot.densresid.simple.sibling.regression.best.fitting.models <- function(best.fits){

    .e = environment()

    residuals.stacked <- NULL
    labels.stacked <- NULL

    ## for (i in 1:length(fits)){  # gives you ages
      for (j in 1:length(best.fits)){

           residuals.stacked <- c(residuals.stacked, residuals(best.fits[[j]]$model) )
           mylabel <-  as.character(formula(best.fits[[j]]$model))
           labels.stacked <- c( labels.stacked, rep(mylabel, length(fitted(best.fits[[j]]$model))))

          }
    ## }

    data.stacked <- data.frame(residuals=residuals.stacked,
                               labels=labels.stacked)

    usePackage("plyr")

    data.stacked <- ddply(data.stacked, c('labels'))
    data.stacked$labels <- as.factor(data.stacked$labels)

    usePackage("ggplot2")
    usePackage("scales")


    d <- data.stacked

    l <- levels(d$labels)

    breaks <- NULL
    for (j in 1:length(best.fits)){
        h <- hist(data.stacked$residuals[data.stacked$labels==levels(data.stacked$labels)[j]],plot=FALSE)
        breaks[[j]] <- h$breaks
    }

    g <- ggplot(d, aes(x=residuals)) +
           mapply(function(d, b) {geom_density(data=d, fill="lightblue",colour="lightblue")},
            split(d, d$labels)) +
             facet_wrap(~ labels,  scales="free", ncol=1) +
               expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Density"),labels=comma) +
                   scale_x_continuous(paste("Residuals"),labels=comma)



      ## print(g)

      g = g + theme_bw() +
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))

     ## print(g)

     return(g)
}

SIMPLESIBREG$plot.densresid.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits)



###
### ACF and PACF Functions of Residuals: Best Fitting Models 
###

SIMPLESIBREG$plot.acfpacf.simple.sibling.regression.best.fitting.models <- function(best.fits){

    .e = environment()

    residuals.stacked <- NULL
    labels.stacked <- NULL

      for (j in 1:length(best.fits)){

           residuals.stacked <- c(residuals.stacked, residuals(best.fits[[j]]$model) )
           mylabel <-  as.character(formula(best.fits[[j]]$model))
           labels.stacked <- c( labels.stacked, rep(mylabel, length(fitted(best.fits[[j]]$model))))

          }
    
    data.stacked <- data.frame(residuals=residuals.stacked,
                               labels=labels.stacked)

    usePackage("plyr")

    data.stacked <- ddply(data.stacked, c('labels'))
    data.stacked$labels <- as.factor(data.stacked$labels)

    usePackage("ggplot2")
    usePackage("scales")

    usePackage("gridExtra")
    
    
    plotlist <- vector("list",2*length(best.fits))
    for (j in 1:length(best.fits)) {
     
          resid.acf <- acf(data.stacked$residuals[data.stacked$labels==levels(data.stacked$labels)[j]], plot=FALSE)
          resid.pacf <- pacf(data.stacked$residuals[data.stacked$labels==levels(data.stacked$labels)[j]], plot=FALSE)

          ci <- 0.95 # Indicates 95% confidence level
          clim0 <- qnorm((1 + ci)/2)/sqrt(resid.acf$n.used)
          clim <- c(-clim0,clim0)

          hline.data <- data.frame(z=c(0,clim), type=c("base","ci","ci"))

          # ACF plot of residuals
          acfPlot <- ggplot(data.frame(lag=resid.acf$lag,acf=resid.acf$acf)) +
            geom_hline(aes(yintercept=z,colour=type,linetype=type),hline.data) +
              geom_linerange(aes(x=lag,ymin=0,ymax=acf)) +
               
                scale_colour_manual(values = c("black","blue")) +
                  scale_linetype_manual(values =c("solid","dashed")) +
                    ggtitle("Autocorrelations") + 
                      theme_bw() +
                        theme(axis.title.x=element_text(size=10,vjust=-0.5),
                          axis.title.y=element_text(size=10,vjust=1.5),
                            axis.text.x=element_text(size=8),
                              axis.text.y=element_text(size=8),
                                strip.text.x = element_text(size = 8, colour = "black", angle = 0))

          # PACF plot of residuals
          pacfPlot <- ggplot(data.frame(lag=resid.pacf$lag,pacf=resid.pacf$acf)) + 
          geom_hline(aes(yintercept=z,colour=type,linetype=type),hline.data) +
            geom_linerange(aes(x=lag,ymin=0,ymax=pacf)) +
              scale_colour_manual(values = c("black","blue")) +
                scale_linetype_manual(values =c("solid","dashed")) +
                  ggtitle("Partial Autocorrelations") + 
                      theme_bw() +
                        theme(axis.title.x=element_text(size=10,vjust=-0.5),
                          axis.title.y=element_text(size=10,vjust=1.5),
                            axis.text.x=element_text(size=8),
                              axis.text.y=element_text(size=8),
                                strip.text.x = element_text(size = 8, colour = "black", angle = 0))

          
          plotlist[[2*j-1]] <- acfPlot  
          plotlist[[2*j]] <- pacfPlot

     }
     
     
     usePackage("gridExtra")
     g <- do.call(arrangeGrob, plotlist)

     return(g)
}


SIMPLESIBREG$plot.acfpacf.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits)


###
### ACF Plots of Residuals: Best Fitting Models
###

SIMPLESIBREG$plot.acfresid.simple.sibling.regression.best.fitting.models <- function(best.fits){

    .e = environment()

    ## residuals.stacked <- NULL
    lag.stacked <- NULL
    acf.stacked <- NULL
    labels.stacked <- NULL
    

    ## for (i in 1:length(fits)){  # gives you ages
     for (j in 1:length(best.fits)){

           ## residuals.stacked <- c(residuals.stacked, residuals(best.fits[[j]]$model) )
           
           acftmp <- acf( residuals(best.fits[[j]]$model) , plot=FALSE)
           
           mylabel <-  as.character(formula(best.fits[[j]]$model))
           labels.stacked <- c( labels.stacked, rep(mylabel,  dim(acftmp$lag)[1])  )
           
           lag.stacked <- c( lag.stacked , as.numeric(acftmp$lag) )
           acf.stacked  <- c(acf.stacked , as.numeric(acftmp$acf))

          }
    ## }

    data.stacked <- data.frame(lag=lag.stacked, acf=acf.stacked, labels=labels.stacked)

    usePackage("plyr")

    data.stacked <- ddply(data.stacked, c('labels'))
    data.stacked$labels <- as.factor(data.stacked$labels)

    usePackage("ggplot2")
    usePackage("scales")

    # px <- pretty(data.stacked$index)
    # py <- pretty(data.stacked$leverage)

    ## N <- ceiling(length(fits$model_fits[[i]])/2)

    ## g <- ggplot(data.stacked, aes(fitted,residuals),environment=.e)  +
      g <- ggplot(data.stacked, aes(lag,acf),environment=.e) +
           geom_linerange(aes(x=lag, ymin=0, ymax=acf), colour="red", size=0.3) +
           ## geom_hline(yintercept=2*mean(data.stacked$leverage)) +   ## doesn't work
          # geom_text(aes(label=labels),col="blue",size=3) +
            # coord_fixed(ratio=1) +
              facet_wrap(~labels, scales="free", ncol = 1) +
                expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("ACF"),labels=comma) +
                   scale_x_continuous(paste("Lag"),labels=comma)
                   ## coord_fixed(ratio=1)

      ## print(g)

      g = g + theme_bw() +
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))

     ## Source: http://trinkerrstuff.wordpress.com/2012/09/01/add-text-annotations-to-ggplot2-faceted-plot/


     clim <- NULL
     for (j in 1:length(best.fits)){
     
        acftmp <- acf( residuals(best.fits[[j]]$model) , plot=FALSE)
         
        ci <- 0.95 # Indicates 95% confidence level
        clim0 <- qnorm((1 + ci)/2)/sqrt(acftmp$n.used)
        clim <- c(clim, clim0)

     }
     
      dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))
      g = g + geom_hline(aes(yintercept = z), dummy2, linetype="dashed",col="blue", size=0.3)
      dummy2 <- data.frame(z = -clim, labels = levels(data.stacked$labels))
      g = g + geom_hline(aes(yintercept = z), data = dummy2, linetype="dashed",col="blue", size=0.3)
      dummy2 <- data.frame(z = 0, labels = levels(data.stacked$labels))
      g = g + geom_hline(aes(yintercept = z), data = dummy2, linetype="solid",col="black", size=0.3)



     return(g)
}

SIMPLESIBREG$plot.acfresid.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits)



###
### PACF Plots of Residuals : Best Fitting Models
###

SIMPLESIBREG$plot.pacfresid.simple.sibling.regression.best.fitting.models <- function(best.fits){

    .e = environment()

    ## residuals.stacked <- NULL
    lag.stacked <- NULL
    acf.stacked <- NULL
    labels.stacked <- NULL
    

    ## for (i in 1:length(fits)){  # gives you ages
     for (j in 1:length(best.fits)){

           ## residuals.stacked <- c(residuals.stacked, residuals(best.fits[[j]]$model) )
           
           acftmp <- acf( residuals(best.fits[[j]]$model) , type="partial", plot=FALSE)
           
           mylabel <-  as.character(formula(best.fits[[j]]$model))
           labels.stacked <- c( labels.stacked, rep(mylabel,  dim(acftmp$lag)[1])  )
           
           lag.stacked <- c( lag.stacked , as.numeric(acftmp$lag) )
           acf.stacked  <- c(acf.stacked , as.numeric(acftmp$acf))

          }
    ## }

    data.stacked <- data.frame(lag=lag.stacked, acf=acf.stacked, labels=labels.stacked)

    usePackage("plyr")

    data.stacked <- ddply(data.stacked, c('labels'))
    data.stacked$labels <- as.factor(data.stacked$labels)

    usePackage("ggplot2")
    usePackage("scales")

    # px <- pretty(data.stacked$index)
    # py <- pretty(data.stacked$leverage)

    ## N <- ceiling(length(fits$model_fits[[i]])/2)

    ## g <- ggplot(data.stacked, aes(fitted,residuals),environment=.e)  +
      g <- ggplot(data=data.stacked, aes(lag,acf), environment=.e) +
           geom_linerange(aes(x=lag, ymin=0, ymax=acf), colour="red", size=0.3) +
           ## geom_hline(yintercept=2*mean(data.stacked$leverage)) +   ## doesn't work
          # geom_text(aes(label=labels),col="blue",size=3) +
            # coord_fixed(ratio=1) +
              facet_wrap(~labels, scales="free", ncol = 1) +
                expand_limits(x=1, y=0) +
                  scale_y_continuous(paste("PACF"),labels=comma) +
                   scale_x_continuous(paste("Lag"),labels=comma)
                  

      ## print(g)

      g = g + theme_bw() +
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))

     ## Source: http://trinkerrstuff.wordpress.com/2012/09/01/add-text-annotations-to-ggplot2-faceted-plot/


     clim <- NULL
     for (j in 1:length(best.fits)){
     
        acftmp <- acf( residuals(best.fits[[j]]$model) , type="partial", plot=FALSE)
         
        ci <- 0.95 # Indicates 95% confidence level
        clim0 <- qnorm((1 + ci)/2)/sqrt(acftmp$n.used)
        clim <- c(clim, clim0)

     }
     

      dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))
      g = g + geom_hline(aes(yintercept = z), dummy2, linetype="dashed",col="blue", size=0.3)
      dummy2 <- data.frame(z = -clim, labels = levels(data.stacked$labels))
      g = g + geom_hline(aes(yintercept = z), data = dummy2, linetype="dashed",col="blue", size=0.3)
      dummy2 <- data.frame(z = 0, labels = levels(data.stacked$labels))
      g = g + geom_hline(aes(yintercept = z), data = dummy2, linetype="solid",col="black", size=0.3)


     return(g)
}

SIMPLESIBREG$plot.pacfresid.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits)


SIMPLESIBREG$best.fits <- SIMPLESIBREG$results_best_fitting_model_for_each_age_class
## http://docs.ggplot2.org/0.9.3.1/geom_vline.html


###
### Time Series Plot of Residuals: Best Fitting Models
###

SIMPLESIBREG$plot.timeresid.simple.sibling.regression.best.fitting.models <- function(best.fits){

    .e = environment()

    ## residuals.stacked <- NULL
    index.stacked <- NULL
    residuals.stacked <- NULL
    labels.stacked <- NULL
    

    ## for (i in 1:length(fits)){  # gives you ages
     for (j in 1:length(best.fits)){

           ## residuals.stacked <- c(residuals.stacked, residuals(best.fits[[j]]$model) )
           
           residuals.stacked <- c( residuals.stacked , residuals(best.fits[[j]]$model) )
           index.stacked <- c( index.stacked , 1:length(residuals(best.fits[[j]]$model)) )
          
           
           mylabel <-  as.character(formula(best.fits[[j]]$model))
           labels.stacked <- c( labels.stacked, rep(mylabel,  length(residuals(best.fits[[j]]$model)))  )     

          }
    ## }

    data.stacked <- data.frame(index=index.stacked, residuals=residuals.stacked, labels=labels.stacked)

    usePackage("plyr")

    data.stacked <- ddply(data.stacked, c('labels'))
    data.stacked$labels <- as.factor(data.stacked$labels)

    usePackage("ggplot2")
    usePackage("scales")

    g <- ggplot(data.stacked, aes(index,residuals), environment=.e) +
          geom_point(colour="black", alpha=0.8) +
           geom_line(linetype="dotted") + 
            facet_wrap(~labels, scales="free", ncol = 1) +
             expand_limits(x=0, y=0) +
              scale_y_continuous(paste("Residuals"),labels=comma) +
               scale_x_continuous(paste("Observation Index"))
              
    ## print(g)

    g = g + theme_bw() +
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))

     ## Source: http://trinkerrstuff.wordpress.com/2012/09/01/add-text-annotations-to-ggplot2-faceted-plot/

     clim <- NULL
     for (j in 1:length(best.fits)){
     
        clim <- c(clim, 0)

     }
     
     dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))
     g = g + geom_hline(aes(yintercept = z), dummy2, linetype="solid",col="red", size=0.3)
    
     return(g)
}


SIMPLESIBREG$plot.timeresid.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits)




###
### Index Plot of Leverage Values: Best Fitting Models
###

SIMPLESIBREG$plot.hatvalues.simple.sibling.regression.best.fitting.models <- function(best.fits){
 
    .e = environment()
 
    leverage.stacked <- NULL
    index.stacked <- NULL
    labels.stacked <- NULL
    
    for (j in 1:length(best.fits)){     

           leverage.stacked <- c(leverage.stacked, hatvalues(best.fits[[j]]$model) )
           index.stacked <- c(index.stacked, 1:length(fitted(best.fits[[j]]$model)) )
           mylabel <-  as.character(formula(best.fits[[j]]$model) ) 
           labels.stacked <- c( labels.stacked, rep(mylabel, length(fitted(best.fits[[j]]$model))) )

    }
 
    data.stacked <- data.frame(leverage=leverage.stacked,
                               index=index.stacked, 
                               labels=labels.stacked)

    usePackage("plyr")
    
    data.stacked <- ddply(data.stacked, c('labels'))     
    data.stacked$labels <- as.factor(data.stacked$labels)    
 
    usePackage("ggplot2")
    usePackage("scales")
    
  
    ## g <- ggplot(data.stacked, aes(fitted,residuals),environment=.e)  +
      g <- ggplot(data.stacked, aes(index,leverage), environment=.e) +
           geom_point(colour="blue",alpha=0.5) +  
           ## geom_hline(yintercept=2*mean(data.stacked$leverage)) +   ## doesn't work
          # geom_text(aes(label=labels),col="blue",size=3) + 
            # coord_fixed(ratio=1) +
              facet_wrap(~labels, scales="free", ncol = 1) + 
                expand_limits(x=0, y=0) + 
                  scale_y_continuous(paste("Leverage Values"),labels=comma) + 
                   scale_x_continuous(paste("Observation Index"),labels=comma) 
                   ## coord_fixed(ratio=1) 
                           
      ## print(g)

      g = g + theme_bw() + 
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8), 
                strip.text.x = element_text(size = 8, colour = "black", angle = 0)) 
     
      ## Source: http://trinkerrstuff.wordpress.com/2012/09/01/add-text-annotations-to-ggplot2-faceted-plot/ 

    
      ## for (i in 1:length(fits)){  # gives you ages
        mean.hatvalues <- NULL
        index.hatvalues <- NULL
        for (j in 1:length(best.fits)){     

          m <- best.fits[[j]]$model
          f <- as.character(formula(m))
          
          mean.hatvalues <- c(mean.hatvalues, mean(hatvalues(m)))
          index.hatvalues <- c(index.hatvalues, length(fitted(m)))
          
          index <- as.numeric(which(hatvalues(m) > 3*mean(hatvalues(m)))) 

          by <- best.fits[[j]]$data$Brood_Year
          by <- substr(by, start=3, stop=4) 
                 
          if (length(index)>0) {         
                 
          ann_text <- data.frame(index = (1:length(fitted(m)))[index], leverage = hatvalues(m)[index], 
                             labels = factor(f, levels = levels(data.stacked$labels)), 
                             text = by[index])

          ## g = g + geom_text(aes(x=index,y=leverage,label=text),data = ann_text,hjust=-0.2,vjust=-0.1,size=2.5,colour="magenta")
          g = g + geom_text(aes(x=index,y=leverage,label=text),data = ann_text,
                            hjust="inward",vjust="inward",size=2.5,colour="magenta")
     
          }         
    
        } 
      
      
     clim <- NULL
     for (j in 1:length(best.fits)){
     
        clim <- c(clim, 3*mean.hatvalues[j])

     }
     
     dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))
     g = g + geom_hline(aes(yintercept = z), dummy2, linetype="dashed",col="green", size=0.7)
    

     ## print(g)

     return(g)
}

SIMPLESIBREG$plot.hatvalues.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits)


###
### Index Plots of Studentized Residuals: Best Fitting Models
###

SIMPLESIBREG$plot.studentresid.simple.sibling.regression.best.fitting.models <- function(best.fits){

    .e = environment()

    ## residuals.stacked <- NULL
    index.stacked <- NULL
    residuals.stacked <- NULL
    labels.stacked <- NULL
    
    for (j in 1:length(best.fits)){

           residuals.stacked <- c(residuals.stacked, rstudent(best.fits[[j]]$model) )
           index.stacked <- c(index.stacked, 1:length(rstudent(best.fits[[j]]$model)) )
           
           mylabel <-  as.character(formula(best.fits[[j]]$model))
           labels.stacked <- c( labels.stacked, rep(mylabel,  length(rstudent(best.fits[[j]]$model)))  )
           
    }
    
    data.stacked <- data.frame(index=index.stacked, residuals=residuals.stacked, labels=labels.stacked)

    usePackage("plyr")

    data.stacked <- ddply(data.stacked, c('labels'))
    data.stacked$labels <- as.factor(data.stacked$labels)

    usePackage("ggplot2")
    usePackage("scales")

    g <- ggplot(data.stacked, aes(index,residuals), environment=.e) +
           geom_linerange(aes(x=index, ymin=0, ymax=residuals), colour="steelblue3", size=0.5) +
              facet_wrap(~labels, scales="free", ncol = 1) +   
                expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Studentized Residuals"),labels=comma) +
                   scale_x_continuous(paste("Observation Index"))   ## + 
                    ## expand_limits(y = c(min( pretty(data.stacked$residuals) )-1 , max(  pretty(data.stacked$residuals) )+ 1 ))
                   

     g = g + theme_bw() +
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))

     ## Source: http://trinkerrstuff.wordpress.com/2012/09/01/add-text-annotations-to-ggplot2-faceted-plot/

     index.residuals <- NULL
     for (j in 1:length(best.fits)){     

          m <- best.fits[[j]]$model
          f <- as.character(formula(m))
          
          ## cutoff.residuals <- c(cutoff.residuals, 2)
          index.residuals <- c(index.residuals, length(rstudent(m)))
          
          index <- as.numeric(which(rstudent(m) > 2 | rstudent(m) < -2)) 

          if (length(index) > 0) {

          by <- best.fits[[j]]$data$Brood_Year
          by <- substr(by, start=3, stop=4) 
                 
          ann_text <- data.frame(index = (1:length(rstudent(m)))[index], residuals = rstudent(m)[index], 
                             labels = factor(f, levels = levels(data.stacked$labels)), 
                             text = by[index])

          ## g = g + geom_text(aes(x=index,y=residuals,label=text),data = ann_text,hjust=-0.2,vjust=-0.1,size=2.5,colour="magenta")
          g = g + geom_text(aes(x=index,y=residuals,label=text),data = ann_text,
                            hjust="inward",vjust="inward", size=2.5, colour="magenta")
     
          } 
    } 
    
      
    clim <- NULL
    for (j in 1:length(best.fits)){
     
        clim <- c(clim, 2)

    }
     
    dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))
    g = g + geom_hline(aes(yintercept = z), dummy2, linetype="dashed",col="salmon3", size=0.3)
    dummy2 <- data.frame(z = -clim, labels = levels(data.stacked$labels))
    g = g + geom_hline(aes(yintercept = z), data = dummy2, linetype="dashed",col="salmon3", size=0.3)
    dummy2 <- data.frame(z = 0, labels = levels(data.stacked$labels))
    g = g + geom_hline(aes(yintercept = z), data = dummy2, linetype="solid",col="black", size=0.3)

    return(g)
}


SIMPLESIBREG$plot.studentresid.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits)




###
### Index Plot of Cook's Distances:  Best Fitting Models
###

SIMPLESIBREG$plot.cooks.simple.sibling.regression.best.fitting.models <- function(best.fits){
 
    .e = environment()
 
    cooks.stacked <- NULL
    index.stacked <- NULL
    labels.stacked <- NULL
    
    ## for (i in 1:length(fits)){  # gives you ages
      for (j in 1:length(best.fits)){     

           cooks.stacked <- c(cooks.stacked, cooks.distance(best.fits[[j]]$model) )
           index.stacked <- c(index.stacked, 1:length(fitted(best.fits[[j]]$model)) )
           mylabel <-  as.character(formula(best.fits[[j]]$model)) 
           labels.stacked <- c( labels.stacked, rep(mylabel, length(fitted(best.fits[[j]]$model))))

          }
    ## }

    data.stacked <- data.frame(cooks=cooks.stacked,
                               index=index.stacked, 
                               labels=labels.stacked)

    usePackage("plyr")
    
    data.stacked <- ddply(data.stacked, c('labels'))     
    data.stacked$labels <- as.factor(data.stacked$labels)    
 
    usePackage("ggplot2")
    usePackage("scales")
    
    
    ## g <- ggplot(data.stacked, aes(fitted,residuals),environment=.e)  +
      g <- ggplot(data.stacked, aes(index,cooks,ymin=0,ymax=cooks)) +
            geom_linerange(colour="grey") +   
           geom_point(colour="darkgreen",alpha=0.5) + 
           ## geom_hline(yintercept=2*mean(data.stacked$leverage)) +   ## doesn't work
          # geom_text(aes(label=labels),col="blue",size=3) + 
            # coord_fixed(ratio=1) +
              facet_wrap(~labels, scales="free", ncol = 1) + 
                expand_limits(x=0, y=0) + 
                  scale_y_continuous(paste("Cook's Distance"),labels=comma) + 
                   scale_x_continuous(paste("Observation Index"),labels=comma) 
                   ## coord_fixed(ratio=1) 
                           
      ## print(g)

      g = g + theme_bw() + 
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8), 
                strip.text.x = element_text(size = 8, colour = "black", angle = 0)) 
     
      ## Source: http://trinkerrstuff.wordpress.com/2012/09/01/add-text-annotations-to-ggplot2-faceted-plot/ 

    
    
        for (j in 1:length(best.fits)){     

          m <- best.fits[[j]]$model
          f <- as.character(formula(m))
          
          ## index.hatvalues <- c(index.hatvalues, length(fitted(m)))
          
          n <-  nrow(model.matrix(m)) 
          
          ## Cook's Distance Cut-Off: http://en.wikipedia.org/wiki/Cook's_distance
          index <- as.numeric(which(cooks.distance(m) > 4/n)) 

          by <- best.fits[[j]]$data$Brood_Year
          by <- substr(by, start=3, stop=4) 
               
              
          if (length(index) > 0) {
             
          ann_text <- data.frame(index = (1:length(fitted(m)))[index], cooks = cooks.distance(m)[index], 
                             labels = factor(f, levels = levels(data.stacked$labels)), 
                             text = by[index])

          ## g = g + geom_text(aes(x=index,y=cooks,label=text),data = ann_text,hjust=-0.2,vjust=-0.1,size=2.5,colour="magenta")

          g = g + geom_text(aes(x=index,y=cooks,label=text),data = ann_text,
                            hjust="inward",vjust="inward",size=2.5,colour="magenta")

          }        
    
        } 
    

    clim <- NULL
    for (j in 1:length(best.fits)){
     
        m <- best.fits[[j]]$model
        n <-  nrow(model.matrix(m))
        
        ## Cook's Distance Cut-Off: http://en.wikipedia.org/wiki/Cook's_distance
        clim <- c(clim, 4/n)

    }
     
    dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))
    g = g + geom_hline(aes(yintercept = z), dummy2, linetype="dashed",col="darkblue", size=0.3)
    dummy2 <- data.frame(z = -clim, labels = levels(data.stacked$labels))
    g = g + geom_hline(aes(yintercept = z), data = dummy2, linetype="dashed",col="darkblue", size=0.3)
    dummy2 <- data.frame(z = 0, labels = levels(data.stacked$labels))
    g = g + geom_hline(aes(yintercept = z), data = dummy2, linetype="solid",col="black", size=0.3)


     return(g)
}

SIMPLESIBREG$plot.cooks.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits)


### 
### Influence Plots:  Best Fitting Models
###


SIMPLESIBREG$plot.cooks.bubble.simple.sibling.regression.best.fitting.models <- function(best.fits){
 
    .e = environment()
 
    best.fits <- best.fits
 
    residuals.stacked <- NULL 
    leverage.stacked <- NULL 
    cooks.stacked <- NULL
    index.stacked <- NULL
    labels.stacked <- NULL
    # by.stacked <- NULL 
    
    ## for (i in 1:length(fits)){  # gives you ages
      for (j in 1:length(best.fits)){     

           residuals.stacked <- c(residuals.stacked, rstudent(best.fits[[j]]$model) )
           leverage.stacked <- c(leverage.stacked, hatvalues(best.fits[[j]]$model) ) 
           cooks.stacked <- c(cooks.stacked, cooks.distance(best.fits[[j]]$model) )
           index.stacked <- c(index.stacked, 1:length(fitted(best.fits[[j]]$model)) )
           mylabel <-  as.character(formula(best.fits[[j]]$model)) 
           labels.stacked <- c( labels.stacked, rep(mylabel, length(fitted(best.fits[[j]]$model))))
           
           # by <- best.fits[[j]]$data$Brood_Year
           # by <- substr(by, start=3, stop=4) 

           # by.stacked <- c(by.stacked, by)

          }
    ## }

    data.stacked <- data.frame(residuals=residuals.stacked,
                               leverage=leverage.stacked,
                               cooks=cooks.stacked,
                               index=index.stacked, 
                               labels=labels.stacked 
                               # , by=by.stacked
                               )

    usePackage("plyr")
    
    data.stacked <- ddply(data.stacked, c('labels'))     
    data.stacked$labels <- as.factor(data.stacked$labels)    
 
    data.stacked <<- data.stacked
 
    usePackage("ggplot2")
    usePackage("scales")
    
    
    # px <- pretty(data.stacked$index)
    # py <- pretty(data.stacked$leverage)

    ## N <- ceiling(length(fits$model_fits[[i]])/2)
    
    usePackage("plyr")
    
    ## g <- ggplot(data.stacked, aes(fitted,residuals),environment=.e)  +
      g <- ggplot(data.stacked, aes(x=leverage,y=residuals,size=cooks, guide=FALSE),environment=.e) +
            geom_point(colour="red", fill="red", shape=21, alpha=0.5)+ 
             geom_vline(data = ddply(data.stacked, "labels", summarise, v = 3*mean(leverage)), 
             aes(xintercept=v), linetype="dashed", colour="springgreen") +
              ## geom_text(size=2.5) + 
            
           ## geom_hline(yintercept=2*mean(data.stacked$leverage)) +   ## doesn't work
          # geom_text(aes(label=labels),col="blue",size=3) + 
            # coord_fixed(ratio=1) +
              facet_wrap(~labels, scales="free", ncol = 1) + 
                expand_limits(x=0, y=0) + 
                  scale_y_continuous(paste("Studentized Residual"),labels=comma) + 
                   scale_x_continuous(paste("Leverage"),labels=comma)  + 
                     scale_size_continuous(name = "Cook's Distance") 
                   ## coord_fixed(ratio=1) 
                           
      ## print(g)

      g = g + theme_bw() + 
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8), 
                strip.text.x = element_text(size = 8, colour = "black", angle = 0),
                legend.position="top") 
     
      ## Source: http://trinkerrstuff.wordpress.com/2012/09/01/add-text-annotations-to-ggplot2-faceted-plot/ 


        mean.hatvalues <- NULL
        ## index.hatvalues <- NULL
        for (j in 1:length(best.fits)){     

          m <- best.fits[[j]]$model
          f <- as.character(formula(m))
          
          ## index.hatvalues <- c(index.hatvalues, length(fitted(m)))
          mean.hatvalues <- c(mean.hatvalues, mean(hatvalues(m)))
          
          n <-  nrow(model.matrix(m)) 
          
          index <- as.numeric(which( (hatvalues(m) > 3*mean(hatvalues(m)) | (abs(rstudent(m)) > 2) ))) 
 
          by2 <- best.fits[[j]]$data$Brood_Year
          by2 <- substr(by2, start=3, stop=4) 
               
              
          if (length(index) > 0) {
             
          ann_text <<- data.frame(leverage = hatvalues(m)[index], residuals = rstudent(m)[index], 
                             labels = factor(f, levels = levels(data.stacked$labels)), 
                             text = by2[index])

          ## g = g + geom_text(aes(x=leverage,y=residuals,label=text),data = ann_text,hjust=-0.2,vjust=-0.1,size=2.5,colour="black")
           
          g = g + geom_text(aes(x=leverage,y=residuals,label=text),data = ann_text,
                                hjust="inward",vjust="inward",size=2.5,colour="black") 
           
          rm(list = ls(envir=globalenv())[
             grep("ann_text", ls(envir=globalenv()))], 
             envir = globalenv())    # remove ann_text from global environment
              
          }        
    
        } 
      ## }

        
      dummy2 <- data.frame(X = levels(data.stacked$labels), Z = rep(2,nlevels(data.stacked$labels)))
      g = g + geom_hline(data = dummy2, aes(yintercept = Z), linetype="dashed",col="blue")
      
      dummy3 <- data.frame(X = levels(data.stacked$labels), Z = rep(-2,nlevels(data.stacked$labels)))
      g = g + geom_hline(data = dummy3, aes(yintercept = Z), linetype="dashed", col="blue")
      
      dummy4 <- data.frame(X = levels(data.stacked$labels), Z = rep(0,nlevels(data.stacked$labels)))
      g = g + geom_hline(data = dummy4, aes(yintercept = Z), linetype="solid",col="blue")
                
      ## dummy5 <- data.frame(X = levels(data.stacked$labels)[1], Z = 3*mean.hatvalues[1])
      ## g = g + geom_vline(data = dummy5, aes(xintercept = Z), linetype="solid",col="green")
      
      rm(list = ls(envir=globalenv())[
             grep("data.stacked", ls(envir=globalenv()))], 
             envir = globalenv())    # remove data.stacked from global environment
          
         
      ## print(g)

      return(g)
}


SIMPLESIBREG$plot.cooks.bubble.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits)



