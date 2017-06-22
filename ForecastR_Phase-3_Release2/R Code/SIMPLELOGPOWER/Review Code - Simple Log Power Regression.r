######################################################################################################
# Simple Log Power Regression Models - Prediction Results
# Date Stamp: February 10, 2017
######################################################################################################

cat("Review Code - Simple Log Power Regression.R", "\n\n")

######################################################################################################

## SIMPLELOGPOWER <- new.env()

## SIMPLELOGPOWER$datafile_original <- 

## SIMPLELOGPOWER$datafile <- SIMPLELOGPOWER$datafile_original

## SIMPLELOGPOWER$stockabundance <- SIMPLELOGPOWER$datafile$Stock_Abundance[1]
## SIMPLELOGPOWER$stockname <- SIMPLELOGPOWER$datafile$Stock_Name[1]
## SIMPLELOGPOWER$stockspecies <- SIMPLELOGPOWER$datafile$Stock_Species[1]
## SIMPLELOGPOWER$forecastingyear <- SIMPLELOGPOWER$datafile$Forecasting_Year[1]

## usePackage("stringr")
## SIMPLELOGPOWER$forecastingyear <- str_replace_all(SIMPLELOGPOWER$forecastingyear, "\n","")
## SIMPLELOGPOWER$forecastingyear <- as.numeric(SIMPLELOGPOWER$forecastingyear)


#======================================================================================================

## function for installing and/or loading R packages
## usePackage <- function(p) {
##    if (!is.element(p, installed.packages()[,1]))
##        install.packages(p, dep = TRUE)
##    require(p, character.only = TRUE)
## }

## wrapper <- function(x, ...){
##       paste(strwrap(x, width=50, ...), collapse = "\n")
## }


#=======================================================================================================

SIMPLELOGPOWER$datafile <- datafile_original

SIMPLELOGPOWER$stockabundance <- SIMPLELOGPOWER$datafile$Stock_Abundance[1]
SIMPLELOGPOWER$stockname <- SIMPLELOGPOWER$datafile$Stock_Name[1]
SIMPLELOGPOWER$stockspecies <- SIMPLELOGPOWER$datafile$Stock_Species[1]
SIMPLELOGPOWER$forecastingyear <- SIMPLELOGPOWER$datafile$Forecasting_Year[1]

#=======================================================================================================

SIMPLELOGPOWER$datafile_extract_age_class <- function(datafile, stockabundance){

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


SIMPLELOGPOWER$datafile_variables <- SIMPLELOGPOWER$datafile_extract_age_class(SIMPLELOGPOWER$datafile, SIMPLELOGPOWER$stockabundance)


#=======================================================================================================


# prepare data and model formulas for simple log power regressions without environmental covariates


SIMPLELOGPOWER$prepare_data_and_model_formulas_simplelogpower <- function(datafile_variables){
   ## http://stackoverflow.com/questions/4951442/formula-with-dynamic-number-of-variables


   ## mylist.names <- names(datafile_variables)

   ## mylist <- vector("list", length(mylist.names)-1)
   ## names(mylist) <- mylist.names[-1]

   mylist <- rep(list(list()),length(names(datafile_variables))-1)

   for (i in 2:length(datafile_variables)){   # i is used to index the outcome variables (so it excludes the youngest age outcome)

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

    } # end for i

    names(mylist) <- names(datafile_variables)[-1]

    ## build model formulas for each applicable age class,
    ## as well as provide data set to be used for each formula

    ## myres <- list(list())

    myres <- rep(list(list()),length(mylist))

    for (k in 1:length(mylist)){    # k indexes the outcome variables, so it excludes the youngest age outcome

         mylength <- length(mylist[[k]]$predictor_data)

         outcome_name <- names(mylist[[k]]$outcome_data)[3]

         predictor_names <-  unlist(lapply(mylist[[k]]$predictor_data, function(ll) {names(ll)[3]}))

         predictor_names  <- rev(predictor_names)


         for (l in 1:length(predictor_names)){

             model_formulas <- list()
             for (j in 1:l){

                 outcome_variable <- subset(mylist[[k]]$outcome_data, select=outcome_name)
                 zeroes_in_outcome_variable <- length(outcome_variable[outcome_variable==0]) # equals zero when we have no zeroes in the outcome variable
                 ln_outcome_variable <- ifelse(zeroes_in_outcome_variable==0,"log", "log1p")

                 tmp <- predictor_names[1:(l-j+1)]

                 ln_predictor_variables <- NULL
                 for (v in 1:length(tmp)){

                      A <- sapply(mylist[[k]]$predictor_data,"[[",tmp[v])
                      B <- A[!sapply(A, is.null)]
                      C <- unlist(B)
                      C
                      zeroes_in_predictor_variable <- length(C[C==0])  # equals zero when we have no zeroes in the predictor variable

                      D <- ifelse(zeroes_in_predictor_variable==0,"log", "log1p")
                      ln_predictor_variables <- c(ln_predictor_variables,D)
                 } # end for v



                 model_formulas[[j]] <- as.formula(paste(paste0(ln_outcome_variable,"(",outcome_name,")"),"~","1","+",
                                                   paste(paste0(ln_predictor_variables ,"(",predictor_names[1:(l-j+1)],")"),collapse="+")))
             } # end for j

         }  # end for l

         model_formulas <- rev(model_formulas)

         model_formulas

         usePackage("BBmisc")

         model_formulas <- getFirst(model_formulas)  # extract first formula only for simple log power regression

         str(model_formulas)

         #### if (k >= 2) {
         #### for (kk in 2:length(predictor_names)){


         ####   tmp <- predictor_names[1:kk]
         ####   MM <- data.frame(Brood_Year=forecastingyear-1)
         ####   for (v in 1:length(tmp)){
         ####        print(v)
         ####        AA <- sapply(mylist[[k]]$predictor_data,"[[",tmp[v])
         ####        AA <- unlist(AA[!sapply(AA, is.null)])
         ####         AA
         ####        BB <- sapply(rev(mylist[[k]]$predictor_data),"[[","Brood_Year")[v]
         ####        BB <- unlist(BB)
         ####
         ####        CC <- cbind.data.frame(BB,AA)
         ####        names(CC) <- c("Brood_Year", tmp[v])
         ####        print(head(CC))
         ####        print(tail(CC))
         ####        MM <- merge(MM,CC, by="Brood_Year",all=TRUE)
         ####   }
         ####
         ####   MM # stores all of the predictors needed for the I() transformation;
         ####      # now we need to compute the sum of these predictors and
         ####      # see if any of the sum values are equal to zero
         ####
         ####   SS <- rowSums(MM[,-1]) # sum of predictor variables included in the I() transformation
         ####   SS <- SS[!is.na(SS)]
         ####
         ####   zeroes_in_sum_of_predictor_variable <- length(SS[SS==0])  # equals zero when we have no zeroes in the sum of predictor variables
         ####
         ####   DD <- ifelse(zeroes_in_sum_of_predictor_variable==0,"log", "log1p")
         ####   ln_sum_of_predictor_variables <- DD
         ####
         ####   model_formulas[[length(predictor_names)+kk-1]] <-
         ####   as.formula(paste(paste0(ln_outcome_variable,"(",outcome_name,")"),"~","1","+",
         ####             ln_sum_of_predictor_variables,"(",paste(predictor_names[1:kk],collapse="+"),")"))
         ####
         ####             # paste0(ln_predictor_variables
         #### }
         #### }


         #### ## model_formulas <- rev(model_formulas)

         print(model_formulas)

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

    return(results)

}




SIMPLELOGPOWER$datafile_variables <- SIMPLELOGPOWER$datafile_extract_age_class(SIMPLELOGPOWER$datafile, SIMPLELOGPOWER$stockabundance)

SIMPLELOGPOWER$data_and_model_formulas_simplelogpower <-  SIMPLELOGPOWER$prepare_data_and_model_formulas_simplelogpower(SIMPLELOGPOWER$datafile_variables)

SIMPLELOGPOWER$data_and_model_formulas_simplelogpower

#================================================================================


SIMPLELOGPOWER$simplelogpower_regression_model_fits <- function(data_and_model_formulas_simplelogpower){

    data_and_model_formulas <- data_and_model_formulas_simplelogpower

    ## fits <- rep(list(list()),length(data_and_model_formulas[[1]]))

    fits <- vector("list",length(data_and_model_formulas[[1]]))

    names(fits) <- names(data_and_model_formulas[[1]])

    for (i in 1:length(data_and_model_formulas[[1]])){

          fits[[i]] <-  lm(formula=data_and_model_formulas$model_formulas[[i]]$model_formulas,
                             data=data_and_model_formulas$model_data[[i]])

    }


    res <- list(model_data=data_and_model_formulas$model_data,
                model_formulas=data_and_model_formulas$model_formulas,
                model_fits=fits)

    return(res)

}


SIMPLELOGPOWER$fits.simplelogpower <-  SIMPLELOGPOWER$simplelogpower_regression_model_fits(SIMPLELOGPOWER$data_and_model_formulas_simplelogpower)

#===============================================================================


## Important Reference on working with AIC:
## http://theses.ulaval.ca/archimede/fichiers/21842/apa.html

## Reference on differences in computing AIC values and on AICcmodavg package
## http://r.789695.n4.nabble.com/R-s-AIC-values-differ-from-published-values-td4384952.html



SIMPLELOGPOWER$model_selection_results_simplelogpower <- function(fits.simplelogpower){

    ## model formulas
    ## myformulas <- rep(list(list()),length(fits$model_fits))

    fits <- fits.simplelogpower

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


SIMPLELOGPOWER$results_model_selection_simplelogpower <-  SIMPLELOGPOWER$model_selection_results_simplelogpower(SIMPLELOGPOWER$fits.simplelogpower)

SIMPLELOGPOWER$results_model_selection_simplelogpower


#=================================================================================================


SIMPLELOGPOWER$model_selection_table_updated_simplelogpower <- function(results_model_selection_simplelogpower){

   results_model_selection <- results_model_selection_simplelogpower

   ## dims <- unlist(lapply(results_model_selection$coefs, function(ll){length(ll)}))

   tmpres <- matrix(NA, ncol = 8, nrow=length(results_model_selection$formulas))

   colnames(tmpres) <- c("Age_Class","Model","sigma","Rsq","Adj_Rsq","Num_Param","Log_Lik","AICc")

   tmpres <- as.data.frame(tmpres)


   for (i in 1:length(results_model_selection$formulas)) {

        ## fill tmpres with ages
        tmpres[i,"Age_Class"] <- names(results_model_selection$coefs)[i]


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

   ## names(myres) <- names(results_model_selection$coefs)

   ## return(myres)

   return(tmpres)
}



SIMPLELOGPOWER$model_selection_table_updated_simplelogpower(SIMPLELOGPOWER$results_model_selection_simplelogpower)


#============================================================================================

SIMPLELOGPOWER$model_selection_delta_AIC_and_Akaike_weights_simplelogpower <- function(results_model_selection_simplelogpower){

      results_model_selection <- results_model_selection_simplelogpower

      myres <- SIMPLELOGPOWER$model_selection_table_updated_simplelogpower(results_model_selection)

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

SIMPLELOGPOWER$model_weights_simplelogpower <- SIMPLELOGPOWER$model_selection_delta_AIC_and_Akaike_weights_simplelogpower(SIMPLELOGPOWER$results_model_selection_simplelogpower)

## usePackage("stringr")
## SIMPLELOGPOWER$model_weights_simplelogpower$Model <- str_replace_all(SIMPLELOGPOWER$model_weights_simplelogpower$Model,"NA ","")
## SIMPLELOGPOWER$model_weights_simplelogpower$Model <- str_replace_all(SIMPLELOGPOWER$model_weights_simplelogpower$Model," NA","")

#===============================================================================================

## select the best log power regression model for each age class

## not sure yet how to handle model selection - single best model or model averaging?
## For now: choose the model with Delta AIC = 0 and with largest Akaike weight

SIMPLELOGPOWER$select_best_fitting_model_for_each_age_class_simplelogpower <- function(results_model_selection_simplelogpower){

   results_model_selection <- results_model_selection_simplelogpower

   mysel <- SIMPLELOGPOWER$model_selection_delta_AIC_and_Akaike_weights_simplelogpower(results_model_selection_simplelogpower)

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

SIMPLELOGPOWER$best_fitting_model_for_each_age_class_simplelogpower <-  SIMPLELOGPOWER$select_best_fitting_model_for_each_age_class_simplelogpower(SIMPLELOGPOWER$results_model_selection_simplelogpower)

SIMPLELOGPOWER$best_fitting_model_for_each_age_class_simplelogpower

#====================================================================================


## model summary for "best fitting model" for each age class

SIMPLELOGPOWER$summary_output_best_fitting_model_for_each_age_class_simplelogpower <- function(fits.simplelogpower, results_model_selection_simplelogpower){

      fits <- fits.simplelogpower

      results_model_selection <- results_model_selection_simplelogpower

      best_fitting_model_for_each_age_class <-  SIMPLELOGPOWER$select_best_fitting_model_for_each_age_class_simplelogpower(results_model_selection_simplelogpower)

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


SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower <- SIMPLELOGPOWER$summary_output_best_fitting_model_for_each_age_class_simplelogpower(
                                                                            SIMPLELOGPOWER$fits.simplelogpower,
                                                                            SIMPLELOGPOWER$results_model_selection_simplelogpower)


SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower

SIMPLELOGPOWER$format_results_best_fitting_model_for_each_age_class_simplelogpower <- function(results_best_fitting_model_for_each_age_class_simplelogpower,i) {

    results_best_fitting_model_for_each_age_class <- results_best_fitting_model_for_each_age_class_simplelogpower

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

SIMPLELOGPOWER$format_results_best_fitting_model_for_each_age_class_simplelogpower(SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower,i=1)

SIMPLELOGPOWER$best.fits.simplelogpower <- SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower




#--------- point forecast for each individual age and for the total age ----------------------

## http://science.nature.nps.gov/im/datamgmt/statistics/r/advanced/ReproducibleReporting.cfm


SIMPLELOGPOWER$point.forecast.best.fitting.model.for.each.age.class.simplelogpower <-
    function(results_best_fitting_model_for_each_age_class_simplelogpower, forecastingyear, datafile_variables){


     usePackage("stringr")

     PSY <- forecastingyear
     results_best_fitting_model_for_each_age_class <- results_best_fitting_model_for_each_age_class_simplelogpower
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

         if ( sum(str_count(vars, "\\+")) >= 1 ) {

             vars <- unlist(strsplit(vars, " \\+ "))

             vars <- gsub("log\\(", "log", x=vars)

         }



         ## extract response variable

         resp <- lhs.vars(formula(model))
         resp <- attr(resp,"term.labels")
         resp

         ## paste0("Run_Year_",resp)

         ## Hadley Wickham: http://adv-r.had.co.nz/Computing-on-the-language.html#subset
         subset2 <- function(x, condition) {
            condition_call <- substitute(condition)
            r <- eval(condition_call, x, parent.frame())
            x[r, ]
         }

         ## condition <- paste(paste0("Run_Year_",vars), "==", PSY-1)
         condition <- paste(paste0("Run_Year_",vars), "==", PSY-1:length(vars))


         usePackage("stringr")

         condition <- gsub("\\(|\\)", "", x=condition)

         condition <- gsub(pattern="log|log1p", replacement="", x=condition)


         vars <- gsub("\\(|\\)", "", x=vars)

         vars <- gsub(pattern="log|log1p", replacement="", x=vars)

         ## extract abundance values for forecasting

         subs1 <- subset( x, eval(parse(text=paste(condition, collapse=" & "))) )

         brood_year <- subs1$Brood_Year

         vars_without_logs <- vars    # vars stores the predictor variables, which are logged via log() or log1p() transformations
         vars_without_logs <- gsub("\\(|\\)", "", x=vars_without_logs)
         vars_without_logs <- gsub(pattern="log|log1p", replacement="", x=vars_without_logs)

         head(subs1)

         ## if ( grepl("\\+", vars_without_logs) ) {
         ##     vars_without_logs <- unlist(strsplit(vars_without_logs, " \\+ "))
         ## }



         subs2 <- subset(subs1, select=vars_without_logs)


         ## IGNORE mylist
         ## mylist <- list()
         ## for (i in 1:length(condition)){
         ##      sub <- subset2(x=x,eval(parse(text=condition[i])))
         ##      sub <- subset(sub, select=vars[i])
         ##      sub <- na.omit(sub)
         ##      sub
         ##      mylist[[i]] <- sub
         ## }
         ##
         ## mydf <- as.data.frame(mylist)

         mydf <- subs2

         ## sub <- as.numeric(sub)

         ## new <- eval(parse(text=paste("data.frame(",var,"=",sub,")")))

         form <- as.character(formula(model))

         output[[j]]$Model <- form

         sigma.ols <- summary(model)$sigma
         n <- nrow(model.frame(model))
         sigma.squared.mle <- sigma.ols^2 * ((n-2)/n)

         p <- predict(model, newdata=mydf)

         output[[j]]$PointForecastLogScale <- p
         output[[j]]$PointForecast <- round(exp(p + (sigma.squared.mle/2)))

         resp_without_logs <- resp
         resp_without_logs <- gsub("\\(|\\)", "", x=resp_without_logs)
         resp_without_logs <- gsub(pattern="log|log1p", replacement="", x=resp_without_logs)



         #   pst <- paste("Brood_Year",paste0("Run_Year_",resp), resp, paste0("Run_Year_",vars),vars)
         pst <- paste("Brood_Year",paste0("Run_Year_",resp_without_logs), resp_without_logs, paste0("Run_Year_",vars_without_logs),vars_without_logs)
         pstspl <- unlist(strsplit(pst," "))

         xsub <- x[,colnames(x) %in% pstspl]


         output[[j]]$Data <- xsub

         output[[j]]$DataForecast <- cbind.data.frame(Brood_Year=brood_year,mydf)

         ## output[[j]]$Model <- form

         ## output[[j]]$PointForecast <- predict(model, newdata=mydf)

         ## nms <- c(nms, var <- names(model.frame(fits[[j]]$model))[1])

         nms <- c(nms, resp)
     }

     names(output) <- nms

     return(output)
}


SIMPLELOGPOWER$point_forecast_best_model_for_each_age_class_simplelogpower <- SIMPLELOGPOWER$point.forecast.best.fitting.model.for.each.age.class.simplelogpower(
                                                                     SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower,
                                                                     SIMPLELOGPOWER$forecastingyear,
                                                                     SIMPLELOGPOWER$datafile_variables)


SIMPLELOGPOWER$point_forecast_best_model_for_each_age_class_simplelogpower



#-----------------------------------------------------------------------------------------
# Barplot and time series plot representation of point forecast of age-specific abundance (individual age, simplelogpower)
#-----------------------------------------------------------------------------------------

SIMPLELOGPOWER$barplot.forecasted.values.individual.ages.simplelogpower <- function(point_forecast_best_model_for_each_age_class_simplelogpower,
                                                                 forecastingyear, i){

        .e <- environment()

        myfits <- point_forecast_best_model_for_each_age_class_simplelogpower

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
         resp <- attr(resp,"term.labels")
         resp
         resp <- gsub("\\(|\\)", "", x=resp)
         resp <- gsub(pattern="log|log1p", replacement="", x=resp)
         resp


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
             geom_rect(data=dfretro,aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="dodgerblue3") +
        geom_text(data=dfretro,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(dfretro$retropointforecasts)))),
               colour="dodgerblue3",angle=90,size=2.5,hjust=0,vjust=0.2) +
        # scale_y_continuous(expand=c(0.01, 0.5)) +
        geom_rect(data=dffor, aes(xmax=forecastingyear-1/3,
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
               label=paste("Historical Average =",comma(round(mean(dfretro$retropointforecasts)))), size=2.5, colour="grey15",hjust=0,vjust=0) +
        scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +
        ## scale_y_continuous("Terminal Run",label=comma, breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
        scale_y_continuous(paste(SIMPLELOGPOWER$stockabundance),label=comma, breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
        labs(title=paste(age)) +
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
      scale_y_continuous(paste(SIMPLELOGPOWER$stockabundance),labels=comma, breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
         scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +
         ## geom_text(data=dfretro,aes(x=years[1],
         ##              y=max(dfretro$retropointforecasts),
         ##              label=paste("Historical Average = ",comma(round(mean(dfretro$retropointforecasts),2)))),
         ##          colour="burlywood4",size=3,hjust=-1.2,vjust=1) +
     annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retropointforecasts,dffor$pointforecast)),
               label=paste("Historical Average =",comma(round(mean(dfretro$retropointforecasts)))), size=2.5,colour="grey15",hjust=0,vjust=0) +
     labs(title=paste(age)) +
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

     ## p <- arrangeGrob(gp1,gp2)

     p <- grid.arrange(gp1,gp2)

     p

}



SIMPLELOGPOWER$barplot.forecasted.values.individual.ages.simplelogpower(SIMPLELOGPOWER$point_forecast_best_model_for_each_age_class_simplelogpower,
                                                                 SIMPLELOGPOWER$forecastingyear, i=2)




#====================================================================================
# Compute Interval Forecasts
#====================================================================================


SIMPLELOGPOWER$bootstrap.prediction.interval.simplelogpower.regression.no.covariates <- function(results_best_fitting_model_for_each_age_class_simplelogpower,j,x0,B){

       print(j)

       results_best_fitting_model_for_each_age_class <- results_best_fitting_model_for_each_age_class_simplelogpower

       olddata <- results_best_fitting_model_for_each_age_class[[j]]$data
       n <- nrow(olddata)
       myformula <- results_best_fitting_model_for_each_age_class[[j]]$formula
       mod.fit <- lm(formula=myformula, data=olddata) ## original model fit

       r <- mod.fit$residuals  ## residuals
       influence.stat <- lm.influence(mod.fit)
       h.j <- influence.stat$hat
       r.j <- mod.fit$residuals/sqrt(1-h.j) ## modified residuals
       res <- r.j


       ## y.hat <- coef(mod.fit)%*%t(x0)
       y.hat <- predict(mod.fit,newdata=x0)
       y.hat <- as.numeric(y.hat)

       delta <- NULL
       sigma.squared.mle.star <- NULL

       #### while (length(delta) < B){

       for (i in 1:B){

          ## resample data (case resampling)
          index <- sample(1:n, size=n, replace = TRUE)
          bootdata <- olddata[index,]  # case resampling

          ## fit model to bootstrap data obtained by resampling cases
          mod.fit.star <- lm(myformula, data = bootdata)

          ## fitted value obtained from mod.fit.star
          ## y.hat.star <- coef(mod.fit.star)%*%t(x0)
          y.hat.star <- predict(mod.fit.star,newdata=x0)
          y.hat.star <- as.numeric(y.hat.star)
          ## resample residual
          i.pred <- sample(1:n, size=1)
          epsilon.plus.star <- res[i.pred]

          ## calculate prediction error delta (delta is the prediction error)
          delta <- c(delta,
                     y.hat.star - (y.hat + epsilon.plus.star))

          ## if (y.hat.star - (y.hat + epsilon.plus.star) <= y.hat) {
          ##    delta <- c(delta,
          ##           y.hat.star - (y.hat + epsilon.plus.star))
          ## }

          sigma.ols.star <- summary(mod.fit.star)$sigma
          n.star <- nrow(model.frame(mod.fit.star))
          sigma.squared.mle.star <- c(sigma.squared.mle.star, sigma.ols.star^2 * ((n.star-2)/n.star))


       ## } # end while loop
       } # end for loop


       delta.quant <- quantile(delta,  probs = c(0.10, 0.90), type = 1)

       ## PI.int <- as.numeric(y.hat - rev(delta.quant))

       y.star.boot <- y.hat - delta

       y.star.boot.back.transformed <- round(exp(y.star.boot + sigma.squared.mle.star/2))


       ## lwr.boot <- PI.int[1]
       ## upr.boot <- PI.int[2]

       y.star.boot.back.transformed.quant <- quantile(y.star.boot.back.transformed,  probs = c(0.10, 0.90), type = 1)

       lwr.boot <- y.star.boot.back.transformed.quant[1]
       upr.boot <- y.star.boot.back.transformed.quant[2]

       delta <- delta

       return(list(y.star.boot=y.star.boot.back.transformed, lwr.boot=lwr.boot, upr.boot=upr.boot, delta=delta))

}



#---- "best: sibling regression model forecast -----------------------------------------------------------------


SIMPLELOGPOWER$best.simplelogpower.regression.model.forecast <- function(datafile_variables, results_best_fitting_model_for_each_age_class_simplelogpower, forecastingyear, B){

     results_best_fitting_model_for_each_age_class <- results_best_fitting_model_for_each_age_class_simplelogpower
     myfits <- results_best_fitting_model_for_each_age_class_simplelogpower

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

         usePackage("stringr")

         if ( sum(str_count(allvars, "\\+")) >= 1 ) {

            allvars <- unlist(strsplit(allvars, " \\+ "))

            allvars <- gsub("log\\(", "log", x=allvars)

         }

         allvars <- gsub("\\(|\\)", "", x=allvars)
         allvars <- gsub(pattern="log|log1p", replacement="", x=allvars)

         allvars <-  c(allvars,paste0("Run_Year_",allvars))



         # extract name(s) of predictor variable(s)
         vars <- names(model.frame(myfits[[j]]$model))[-1]

         if ( sum(str_count(vars, "\\+")) >= 1 ) {

            vars <- unlist(strsplit(vars, " \\+ "))

            vars <- gsub("log\\(", "log", x=vars)

         }


         vars <- gsub("\\(|\\)", "", x=vars)
         vars <- gsub(pattern="log|log1p", replacement="", x=vars)


         subset.model.data <- subset(model.data, select=c("Brood_Year",allvars))  # extract outcome + predictor variables from model data

         subset.original.data <- subset(original.data,select=c("Brood_Year",vars))   # extract just the predictor variable(s) from original data

         ## last.observed.brood.year <- subset.original.data$Brood_Year[length(subset.original.data$Brood_Year)]
         age.for.outcome.variable <- names(model.frame(myfits[[j]]$model))[1]


         age.for.outcome.variable <- gsub("\\(|\\)", "", x=age.for.outcome.variable)
         age.for.outcome.variable <- gsub(pattern="log|log1p", replacement="", x=age.for.outcome.variable)

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

         ## newdata <- eval(parse(text=paste("data.frame(",vars,"=",newdata,")")))

         form <- as.character(formula(model))

         output[[j]]$Model <- paste(form)

         ## output[[j]]$p <- as.numeric(predict(model, newdata=newdata))


         ## GHOMESHI

         p <- as.numeric(predict(model, newdata=x0))
         sigma.ols <- summary(model)$sigma
         n <- nrow(model.frame(model))
         sigma.squared.mle <- sigma.ols^2 * ((n-2)/n)

         output[[j]]$p <- round(exp(p + (sigma.squared.mle/2)))       # back-transformed point forecast

         tmp.boot <- SIMPLELOGPOWER$bootstrap.prediction.interval.simplelogpower.regression.no.covariates(results_best_fitting_model_for_each_age_class,j,x0,B)

         output[[j]]$p.lwr <- tmp.boot$lwr.boot

         output[[j]]$p.upr <- tmp.boot$upr.boot

         output[[j]]$y.star.boot <- tmp.boot$y.star.boot
         output[[j]]$delta <- tmp.boot$delta   # forecast error (defined as predicted minus actual) - expressed on log scale!!!

         output[[j]]$x0 <- x0
         output[[j]]$model.boot <- tmp.boot$model.boot ## save object produced by Boot() function in car package

         output[[j]]$model.formula <- form
         output[[j]]$model.data <- subset.model.data

     }


     return(output)

}


## B <- 9999

SIMPLELOGPOWER$B <- B

SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression <- SIMPLELOGPOWER$best.simplelogpower.regression.model.forecast(SIMPLELOGPOWER$datafile_variables,
                                                                                                 SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower,
                                                                                                 SIMPLELOGPOWER$forecastingyear,
                                                                                                 B = SIMPLELOGPOWER$B)


#================================================================================================



SIMPLELOGPOWER$PI.ctr <- NULL
SIMPLELOGPOWER$PI.lwr <- NULL
SIMPLELOGPOWER$PI.upr <- NULL
# PI.med <- NULL
SIMPLELOGPOWER$PI.sim <- NULL
SIMPLELOGPOWER$nms <- NULL

for (k in 1:length(SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression)){

     SIMPLELOGPOWER$PI.ctr <- c(SIMPLELOGPOWER$PI.ctr,
                          SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression[[k]]$p)

     SIMPLELOGPOWER$PI.lwr <- c(SIMPLELOGPOWER$PI.lwr,
                          SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression[[k]]$p.lwr)

     SIMPLELOGPOWER$PI.upr <- c(SIMPLELOGPOWER$PI.upr,
                          SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression[[k]]$p.upr)

     # PI.med <- c(PI.med,
     #            pred.int.individual.ages.expsmooth[[k]]$PI.median)

     SIMPLELOGPOWER$PI.sim <- cbind(SIMPLELOGPOWER$PI.sim, SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression[[k]]$y.star.boot)

     SIMPLELOGPOWER$form <-  as.formula(SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression[[k]]$model.formula)
     usePackage("formula.tools")

     SIMPLELOGPOWER$nms <- c(SIMPLELOGPOWER$nms, lhs.vars(SIMPLELOGPOWER$form))

}

colnames(SIMPLELOGPOWER$PI.sim) <- SIMPLELOGPOWER$nms


SIMPLELOGPOWER$PI.lwr[SIMPLELOGPOWER$PI.lwr < 0] <- 0
SIMPLELOGPOWER$PI.upr[SIMPLELOGPOWER$PI.upr < 0] <- 0

SIMPLELOGPOWER$PI.ctr <- round(SIMPLELOGPOWER$PI.ctr)
SIMPLELOGPOWER$PI.lwr <- round(SIMPLELOGPOWER$PI.lwr)
SIMPLELOGPOWER$PI.upr <- round(SIMPLELOGPOWER$PI.upr)


SIMPLELOGPOWER$PI.individual.ages.simplelogpower.regression <- data.frame(PI.ctr = SIMPLELOGPOWER$PI.ctr,
                                                              PI.lwr = SIMPLELOGPOWER$PI.lwr,
                                                              PI.upr = SIMPLELOGPOWER$PI.upr)

SIMPLELOGPOWER$PI.individual.ages.simplelogpower.regression.no.comma <- data.frame(PI.ctr = SIMPLELOGPOWER$PI.ctr,
                                                                       PI.lwr = SIMPLELOGPOWER$PI.lwr,
                                                                       PI.upr = SIMPLELOGPOWER$PI.upr)


## PI.individual.ages.complex.sibling.regression

usePackage("scales")

SIMPLELOGPOWER$PI.individual.ages.simplelogpower.regression <- comma(SIMPLELOGPOWER$PI.individual.ages.simplelogpower.regression)

## PI.individual.ages.complex.sibling.regression

SIMPLELOGPOWER$PI.individual.ages.simplelogpower.regression.sim <- SIMPLELOGPOWER$PI.sim



#=============================================================================================================
# simplelogpower: Visual representation of interval forecast of age-specific abundance (individual age, sibreg)
#==============================================================================================================

capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                  {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


SIMPLELOGPOWER$scatterplot.forecasted.values.and.forecast.intervals.individual.ages.simplelogpower.regression <- function(bestfits, pointforecasts, intervalforecasts, forecastingyear, i){

    .e <- environment()

    ages <- names(bestfits)

    modelsimplelogpower <- as.character(bestfits[[i]]$formula)

    modelsimplelogpower <- paste("Simple Log Power Regression Model:", modelsimplelogpower)

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
          ## geom_rect(data=dffor, aes(x=NULL, y=NULL, xmax=forecastingyear+1/3,
          ##      xmin=forecastingyear-1/3,
          ##      ymax=upper,
          ##      ymin=lower),fill="lightgrey") +
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
          scale_y_continuous(paste(age, SIMPLELOGPOWER$stockabundance),labels=comma) +
          scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0)) +
          geom_text(data=dfretro, aes(x=years[1]+1,
                       y=1.1*max(dfretro$retropointforecasts,dffor$upper),
                       label=paste("Historical Average = ",comma(round(mean(dfretro$retropointforecasts))))),
                   colour="grey15",size=2.5,hjust=0.1,vjust=0.1) +
          ## labs(title=paste(age)) +
          ggtitle(label=capwords(paste(SIMPLELOGPOWER$stockname, SIMPLELOGPOWER$stockspecies, "Stock")), subtitle=paste(modelsimplelogpower)) +
          theme(plot.title=element_text(size=12, hjust=0.5),
                plot.subtitle=element_text(hjust=0.5),
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


## bestfits <- results_best_fitting_model_for_each_age_class
## pointforecasts <- point_forecast_best_model_for_each_age_class
## intervalforecasts <-   PI.individual.ages.complex.sibling.regression.no.comma

SIMPLELOGPOWER$scatterplot.forecasted.values.and.forecast.intervals.individual.ages.simplelogpower.regression(
   bestfits = SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower,
   pointforecasts = SIMPLELOGPOWER$point_forecast_best_model_for_each_age_class_simplelogpower,
   intervalforecasts = SIMPLELOGPOWER$PI.individual.ages.simplelogpower.regression.no.comma,
   forecastingyear = SIMPLELOGPOWER$forecastingyear,
   i=1)


#######################################################################################################
#######################################################################################################
##
## Model Diagnostics for "Best" Fitting Models
##
#######################################################################################################
#######################################################################################################



###
### Plot of Residuals vs. Fitted Values: Best Fitting Models
###

SIMPLELOGPOWER$plot.residuals.simplelogpower.regression.best.fitting.models <- function(best.fits.simplelogpower){

    .e = environment()

    best.fits <- best.fits.simplelogpower

    residuals.stacked <- NULL
    fitted.stacked <- NULL
    labels.stacked <- NULL

    for (j in 1:length(best.fits)){

           ## residuals.stacked <- c( residuals.stacked, residuals(best.fits[[j]]$model) )
           residuals.stacked <- c( residuals.stacked, residuals(best.fits[[j]]$model) )
           ## fitted.stacked <- c( fitted.stacked, fitted(best.fits[[j]]$model) )
           fitted.stacked <- c( fitted.stacked, fitted(best.fits[[j]]$model) )
           ## mylabel <-  as.character(formula(best.fits[[j]]$model))
           mylabel <-  as.character(formula(best.fits[[j]]$model))
           labels.stacked <- c( labels.stacked, rep(mylabel, length(fitted(best.fits[[j]]$model))))

    }

    options(stringsAsFactors = FALSE)

    data.stacked <- data.frame(residuals=residuals.stacked,
                               fitted=fitted.stacked,
                               labels=labels.stacked)

    options(stringsAsFactors = TRUE)

    usePackage("plyr")

    ## data.stacked <- ddply(data.stacked, c('labels'))

    data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))

    usePackage("ggplot2")
    usePackage("scales")

    dummy.min <- aggregate(cbind(residuals, fitted) ~ labels, FUN=min, data=data.stacked)
    dummy.min$residuals <- dummy.min$residuals - 0.2
    dummy.min

    dummy.max <- aggregate(cbind(residuals, fitted) ~ labels, FUN=max, data=data.stacked)
    dummy.max$residuals <- dummy.max$residuals + 0.2
    dummy.max

    dummy <- rbind.data.frame(dummy.min, dummy.max)

    dummy

    dummy <- ddply(dummy, c('labels'))

    dummy <- dummy[names(data.stacked)]


    ## g <- ggplot(data.stacked, aes(fitted,residuals),environment=.e)  +
      g <- ggplot(data.stacked, aes(fitted,residuals),environment=.e) +
           geom_abline(intercept=0,slope=0,colour="red",size=0.8) +
           geom_point(colour="blue",alpha=0.5) +
              facet_wrap(~labels, scales="free", ncol = 1) +
                geom_blank(data=dummy) +
                ## expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Residuals"),labels=comma) +
                   scale_x_continuous(paste("Fitted Values"),labels=comma)




      g = g + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5), 
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))

      ## Source: http://trinkerrstuff.wordpress.com/2012/09/01/add-text-annotations-to-ggplot2-faceted-plot/

        for (j in 1:length(best.fits)){

          m <- best.fits[[j]]$model
          f <- as.character(formula(m))

          index <- as.numeric(which(abs(rstudent(m)) > 2))
          
          
          if (length(index) > 0 ) {
          
             print(index)

             by2 <- best.fits[[j]]$data$Brood_Year
             by2 <- substr(by2, start=3, stop=4)

             ann_text <<- data.frame(fitted = fitted(m)[index], residuals = residuals(m)[index],
                             labels = factor(f, levels = levels(data.stacked$labels)),
                             text = by2[index])

             g = g + geom_text(aes(x=fitted,y=residuals,label=text),data = ann_text,
                            hjust="inward",vjust="inward",size=2.5,colour="magenta")

             rm(list = ls(envir=globalenv())[
                grep("ann_text", ls(envir=globalenv()))],
                envir = globalenv())    # remove ann_text from global environment

         } 

        }

     return(g)
}


SIMPLELOGPOWER$plot.residuals.simplelogpower.regression.best.fitting.models(SIMPLELOGPOWER$best.fits.simplelogpower)


###
### Histogram of Residuals: Best Fitting Models
###

SIMPLELOGPOWER$plot.histresid.simplelogpower.regression.best.fitting.models <- function(best.fits.simplelogpower){

    .e = environment()

    best.fits <- best.fits.simplelogpower

    residuals.stacked <- NULL
    labels.stacked <- NULL

      for (j in 1:length(best.fits)){

           residuals.stacked <- c(residuals.stacked, residuals(best.fits[[j]]$model) )
           mylabel <-  as.character(formula(best.fits[[j]]$model))
           labels.stacked <- c( labels.stacked, rep(mylabel, length(fitted(best.fits[[j]]$model))))

          }


    options(stringsAsFactors = FALSE)

    data.stacked <- data.frame(residuals=residuals.stacked,
                               labels=labels.stacked)

    options(stringsAsFactors = TRUE)

    usePackage("plyr")

    ## data.stacked <- ddply(data.stacked, c('labels'))

    data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))

    usePackage("ggplot2")
    usePackage("scales")

    d <- data.stacked

    l <- levels(d$labels)

    breaks <- vector("list", length=length(best.fits))
    for (j in 1:length(best.fits)){
        h <- hist(data.stacked$residuals[data.stacked$labels==levels(data.stacked$labels)[j]],plot=FALSE, breaks="Freedman-Diaconis")
        #### breaks[[j]] <- h$breaks
        ## h.tmp <- seq(from=min(h$breaks), to=max(h$breaks), by = unique(diff(h$breaks)))
        #### breaks[[j]] <- h.tmp
        h.tmp <- h$breaks
        print(h.tmp)
        breaks[[j]] <- h.tmp
    }

    breaks

    g <- ggplot(d, aes(x=residuals), environment=.e) +  # environment=.e
           mapply(function(d, b) {geom_histogram(data=d, breaks=b, fill="lightblue",colour="black")},
            split(d, d$labels), breaks) +
             facet_wrap(~ labels,  scales="free", ncol=1) +
               ## expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Frequency"),labels=comma) +
                   scale_x_continuous(paste("Residuals"),labels=comma)

      g = g + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5), 
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))


      g <- g + geom_vline(xintercept = 0, size=0.8, colour="red")

     return(g)
}

SIMPLELOGPOWER$plot.histresid.simplelogpower.regression.best.fitting.models(SIMPLELOGPOWER$best.fits.simplelogpower)




###
### Density Plots of Residuals: Best Fitting Models
###


SIMPLELOGPOWER$plot.densresid.simplelogpower.regression.best.fitting.models <- function(best.fits.simplelogpower){

    .e = environment()

    best.fits <- best.fits.simplelogpower

    residuals.stacked <- NULL
    labels.stacked <- NULL

    ## for (i in 1:length(fits)){  # gives you ages
      for (j in 1:length(best.fits)){

           residuals.stacked <- c(residuals.stacked, residuals(best.fits[[j]]$model) )
           mylabel <-  as.character(formula(best.fits[[j]]$model))
           labels.stacked <- c( labels.stacked, rep(mylabel, length(fitted(best.fits[[j]]$model))))

          }
    ## }

    ## data.stacked <- data.frame(residuals=residuals.stacked,
    ##                           labels=labels.stacked)

    ## usePackage("plyr")

    ## data.stacked <- ddply(data.stacked, c('labels'))
    ## data.stacked$labels <- as.factor(data.stacked$labels)

    options(stringsAsFactors = FALSE)

    data.stacked <- data.frame(residuals=residuals.stacked,
                               labels=labels.stacked)

    options(stringsAsFactors = TRUE)

    ## usePackage("plyr")

    ## data.stacked <- ddply(data.stacked, c('labels'))

    data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))


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
          theme(plot.title=element_text(size=12, hjust=0.5), 
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))

      g <- g + geom_vline(xintercept = 0, size=0.8, colour="red")

     ## print(g)

     return(g)
}

SIMPLELOGPOWER$plot.densresid.simplelogpower.regression.best.fitting.models(SIMPLELOGPOWER$best.fits.simplelogpower)




###
### ACF Plots of Residuals: Best Fitting Models
###

SIMPLELOGPOWER$plot.acfresid.simplelogpower.regression.best.fitting.models <- function(best.fits.simplelogpower){

    .e = environment()

    best.fits <- best.fits.simplelogpower

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

    options(stringsAsFactors = FALSE)

    data.stacked <- data.frame(lag=lag.stacked, acf=acf.stacked, labels=labels.stacked)

    unique(data.stacked$labels)

    options(stringsAsFactors = TRUE)

    data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))

    ## usePackage("plyr")
    ## data.stacked <- ddply(data.stacked, c('labels'))
    ## data.stacked$labels <- as.factor(data.stacked$labels)



    usePackage("ggplot2")
    usePackage("scales")


    # px <- pretty(data.stacked$index)
    # py <- pretty(data.stacked$leverage)

    ## N <- ceiling(length(fits$model_fits[[i]])/2)

    ## g <- ggplot(data.stacked, aes(fitted,residuals),environment=.e)  +
      g <- ggplot(data.stacked, aes(lag,acf), environment=.e) +
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
          theme(plot.title=element_text(size=12, hjust=0.5), 
                axis.title.x=element_text(size=10,vjust=-0.5),
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

SIMPLELOGPOWER$plot.acfresid.simplelogpower.regression.best.fitting.models(SIMPLELOGPOWER$best.fits.simplelogpower)



###
### PACF Plots of Residuals : Best Fitting Models
###

SIMPLELOGPOWER$plot.pacfresid.simplelogpower.regression.best.fitting.models <- function(best.fits.simplelogpower){

    .e = environment()

    best.fits <- best.fits.simplelogpower

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

    options(stringsAsFactors = FALSE)

    data.stacked <- data.frame(lag=lag.stacked, acf=acf.stacked, labels=labels.stacked)

    unique(data.stacked)

    options(stringsAsFactors = TRUE)

    data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))


    ## usePackage("plyr")

    ## data.stacked <- ddply(data.stacked, c('labels'))
    ## data.stacked$labels <- as.factor(data.stacked$labels)

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
          theme(plot.title=element_text(size=12, hjust=0.5), 
                axis.title.x=element_text(size=10,vjust=-0.5),
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

SIMPLELOGPOWER$plot.pacfresid.simplelogpower.regression.best.fitting.models(SIMPLELOGPOWER$best.fits.simplelogpower)

## http://docs.ggplot2.org/0.9.3.1/geom_vline.html

###
### Time Series Plot of Residuals: Best Fitting Models
###

SIMPLELOGPOWER$plot.timeresid.simplelogpower.regression.best.fitting.models <- function(best.fits.simplelogpower){

    .e = environment()

    best.fits <- best.fits.simplelogpower

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

     options(stringsAsFactors = FALSE)

     data.stacked <- data.frame(index=index.stacked, residuals=residuals.stacked, labels=labels.stacked)

     unique(data.stacked$labels)

     options(stringsAsFactors = TRUE)


     data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))

    ## usePackage("plyr")

    ## data.stacked <- ddply(data.stacked, c('labels'))
    ## data.stacked$labels <- as.factor(data.stacked$labels)

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
          theme(plot.title=element_text(size=12, hjust=0.5), 
                axis.title.x=element_text(size=10,vjust=-0.5),
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


SIMPLELOGPOWER$plot.timeresid.simplelogpower.regression.best.fitting.models(SIMPLELOGPOWER$best.fits.simplelogpower)




###
### Index Plot of Leverage Values: Best Fitting Models
###

SIMPLELOGPOWER$plot.hatvalues.simplelogpower.regression.best.fitting.models <- function(best.fits.simplelogpower){

    .e = environment()

    best.fits <- best.fits.simplelogpower

    leverage.stacked <- NULL
    index.stacked <- NULL
    labels.stacked <- NULL

    for (j in 1:length(best.fits)){

           leverage.stacked <- c(leverage.stacked, hatvalues(best.fits[[j]]$model) )
           index.stacked <- c(index.stacked, 1:length(fitted(best.fits[[j]]$model)) )
           mylabel <-  as.character(formula(best.fits[[j]]$model) )
           labels.stacked <- c( labels.stacked, rep(mylabel, length(fitted(best.fits[[j]]$model))) )

    }

    options(stringsAsFactors = FALSE)


    data.stacked <- data.frame(leverage=leverage.stacked,
                               index=index.stacked,
                               labels=labels.stacked)

    options(stringsAsFactors = TRUE)


    data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))

    ## usePackage("plyr")
    ## data.stacked <- ddply(data.stacked, c('labels'))
    ## data.stacked$labels <- as.factor(data.stacked$labels)

    usePackage("ggplot2")
    usePackage("scales")

    myrange <- function(x) {diff(range(x))}
    aggrange <- aggregate(cbind(leverage, index) ~ labels, FUN=myrange, data=data.stacked)


    dummy.min <- aggregate(cbind(leverage, index) ~ labels, FUN=min, data=data.stacked)
    dummy.min$leverage <- dummy.min$leverage -  aggrange$leverage/5
    dummy.min

    dummy.max <- aggregate(cbind(leverage, index) ~ labels, FUN=max, data=data.stacked)
    dummy.max$leverage <- dummy.max$leverage + aggrange$leverage/5
    dummy.max

    dummy <- rbind.data.frame(dummy.min, dummy.max)

    dummy

    dummy <- ddply(dummy, c('labels'))

    dummy <- dummy[names(data.stacked)]


    ## g <- ggplot(data.stacked, aes(fitted,residuals),environment=.e)  +
      g <- ggplot(data.stacked, aes(index,leverage)) +
           geom_point(colour="blue",alpha=0.5) +
           ## geom_hline(yintercept=2*mean(data.stacked$leverage)) +   ## doesn't work
          # geom_text(aes(label=labels),col="blue",size=3) +
            # coord_fixed(ratio=1) +
              facet_wrap(~labels, scales="free", ncol = 1) +
                geom_blank(data=dummy) +
                ## expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Leverage Values"),labels=comma) +
                   scale_x_continuous(paste("Observation Index"),labels=comma)
                   ## coord_fixed(ratio=1)

      ## print(g)

      g = g + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5), 
                axis.title.x=element_text(size=10,vjust=-0.5),
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

          by2 <- best.fits[[j]]$data$Brood_Year
          by2 <- substr(by2, start=3, stop=4)

          if (length(index)>0) {

          ann_text <<- data.frame(index = (1:length(fitted(m)))[index], leverage = hatvalues(m)[index],
                             labels = factor(f, levels = levels(data.stacked$labels)),
                             text = by2[index])

          g = g + geom_text(aes(x=index,y=leverage,label=text),data = ann_text,
                                hjust="inward",vjust="inward",size=2.5,colour="magenta")

          rm(list = ls(envir=globalenv())[
             grep("ann_text", ls(envir=globalenv()))],
             envir = globalenv())    # remove ann_text from global environment


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

SIMPLELOGPOWER$plot.hatvalues.simplelogpower.regression.best.fitting.models(SIMPLELOGPOWER$best.fits.simplelogpower)


###
### Index Plots of Studentized Residuals: Best Fitting Models
###

SIMPLELOGPOWER$plot.studentresid.simplelogpower.regression.best.fitting.models <- function(best.fits.simplelogpower){

    .e = environment()

    best.fits <- best.fits.simplelogpower

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

    options(stringsAsFactors = FALSE)

    data.stacked <- data.frame(index=index.stacked, residuals=residuals.stacked, labels=labels.stacked)

    unique(data.stacked$labels.stacked)

    options(stringsAsFactors = TRUE)

    data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))

    ## usePackage("plyr")
    ## data.stacked <- ddply(data.stacked, c('labels'))
    ## data.stacked$labels <- as.factor(data.stacked$labels)

    usePackage("ggplot2")
    usePackage("scales")

    g <- ggplot(data.stacked, aes(index,residuals), environment=.e) +
           geom_linerange(aes(x=index, ymin=0, ymax=residuals), colour="steelblue3", size=0.5) +
              facet_wrap(~labels, scales="free", ncol = 1) +
                expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Studentized Residuals"),labels=comma) +
                   scale_x_continuous(paste("Observation Index"))   +
                    expand_limits(y = c(min( pretty(data.stacked$residuals) )-1 , max(  pretty(data.stacked$residuals) )+ 1 ))


     g = g + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5), 
                axis.title.x=element_text(size=10,vjust=-0.5),
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
           
          by2 <- best.fits[[j]]$data$Brood_Year
          by2 <- substr(by2, start=3, stop=4)

          ann_text <<- data.frame(index = (1:length(rstudent(m)))[index], residuals = rstudent(m)[index],
                             labels = factor(f, levels = levels(data.stacked$labels)),
                             text = by2[index])

          g = g + geom_text(aes(x=index,y=residuals,label=text),data = ann_text,
                                hjust="inward",vjust="inward",size=2.5,colour="magenta")

           rm(list = ls(envir=globalenv())[
             grep("ann_text", ls(envir=globalenv()))],
             envir = globalenv())    # remove ann_text from global environment

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

SIMPLELOGPOWER$plot.studentresid.simplelogpower.regression.best.fitting.models(SIMPLELOGPOWER$best.fits.simplelogpower)


###
### Index Plot of Cook's Distances:  Best Fitting Models
###

SIMPLELOGPOWER$plot.cooks.simplelogpower.regression.best.fitting.models <- function(best.fits.simplelogpower){

    .e = environment()

    best.fits <- best.fits.simplelogpower

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

    options(stringsAsFactors = FALSE)

    data.stacked <- data.frame(cooks=cooks.stacked,
                               index=index.stacked,
                               labels=labels.stacked)

    options(stringsAsFactors = TRUE)

    data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))

    ## usePackage("plyr")
    ## data.stacked <- ddply(data.stacked, c('labels'))
    ## data.stacked$labels <- as.factor(data.stacked$labels)

    usePackage("ggplot2")
    usePackage("scales")

    myrange <- function(x) {diff(range(x))}
    aggrange <- aggregate(cbind(cooks, index) ~ labels, FUN=myrange, data=data.stacked)


    dummy.max <- aggregate(cbind(cooks, index) ~ labels, FUN=max, data=data.stacked)
    dummy.max$cooks <- dummy.max$cooks + aggrange$cooks/10

    dummy <- dummy.max
    dummy <- ddply(dummy, c('labels'))
    dummy <- dummy[names(data.stacked)]


    ## g <- ggplot(data.stacked, aes(fitted,residuals),environment=.e)  +
      g <- ggplot(data.stacked, aes(index,cooks,ymin=0,ymax=cooks),environment=.e) +
            geom_linerange(colour="grey") +
           geom_point(colour="darkgreen",alpha=0.5) +
           ## geom_hline(yintercept=2*mean(data.stacked$leverage)) +   ## doesn't work
          # geom_text(aes(label=labels),col="blue",size=3) +
            # coord_fixed(ratio=1) +
              facet_wrap(~labels, scales="free", ncol = 1) +
                geom_blank(data=dummy) +
                # expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Cook's Distance"),labels=comma) +
                   scale_x_continuous(paste("Observation Index"),labels=comma)
                   ## coord_fixed(ratio=1)

      ## print(g)

      g = g + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5), 
                axis.title.x=element_text(size=10,vjust=-0.5),
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


          by2 <- best.fits[[j]]$data$Brood_Year
          by2 <- substr(by2, start=3, stop=4)


          if (length(index) > 0) {

          ann_text <<- data.frame(index = (1:length(fitted(m)))[index], cooks = cooks.distance(m)[index],
                             labels = factor(f, levels = levels(data.stacked$labels)),
                             text = by2[index])

          g = g + geom_text(aes(x=index,y=cooks,label=text),data = ann_text,
                            hjust="inward",vjust="inward",size=2.5,colour="magenta")

          rm(list = ls(envir=globalenv())[
             grep("ann_text", ls(envir=globalenv()))],
             envir = globalenv())    # remove ann_text from global environment

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
    ## dummy2 <- data.frame(z = -clim, labels = levels(data.stacked$labels))
    ## g = g + geom_hline(aes(yintercept = z), data = dummy2, linetype="dashed",col="darkblue", size=0.3)
    dummy2 <- data.frame(z = 0, labels = levels(data.stacked$labels))
    g = g + geom_hline(aes(yintercept = z), data = dummy2, linetype="solid",col="black", size=0.3)


     return(g)
}

SIMPLELOGPOWER$plot.cooks.simplelogpower.regression.best.fitting.models(SIMPLELOGPOWER$best.fits.simplelogpower)


###
### Influence Plots:  Best Fitting Models
###


SIMPLELOGPOWER$plot.cooks.bubble.simplelogpower.regression.best.fitting.models <- function(best.fits){

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
             geom_vline(data = ddply(data.stacked, "labels", summarise, v = 3*mean(leverage)), aes(xintercept=v), linetype="dashed", colour="springgreen") +
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
          theme(plot.title=element_text(size=12, hjust=0.5), 
                axis.title.x=element_text(size=10,vjust=-0.5),
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


SIMPLELOGPOWER$plot.cooks.bubble.simplelogpower.regression.best.fitting.models(SIMPLELOGPOWER$best.fits.simplelogpower)








