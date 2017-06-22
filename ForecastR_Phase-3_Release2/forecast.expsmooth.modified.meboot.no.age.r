
forecast.expsmooth.modified.meboot.no.age <- function(fit, boxcoxtransform, level=80, npaths=B){

    series <- fit$model.data[,ncol(fit$model.data)]

    mean(series)

    usePackage("meboot")
    usePackage("forecast")

    trim.values <- seq(0, 0.4, by=0.05)

    mean.series.meboot2 <- NULL
    for (k in 1:length(trim.values)) {
        set.seed(k)
        series.meboot2 <- meboot2(series, reps=npaths, trim=trim.values[k])$ensemble
        mean.series.meboot2 <- c(mean.series.meboot2, mean(series.meboot2))

    }

    diff.mean.series.meboot2 <- mean.series.meboot2 - mean(series)

    abs.diff.mean.series.meboot2 <- abs(diff.mean.series.meboot2)

    trim.optimal <- trim.values[which.min(abs.diff.mean.series.meboot2)]

    ## series.meboot2 <- meboot2(series, reps=npaths, trim=trim)$ensemble

    series.meboot2 <- meboot2(series, reps=npaths, trim=trim.optimal)$ensemble

    mean(series.meboot2)

    expsmoothfit <- fit$model

    sink("expsmoothfit.txt")
    print(expsmoothfit)
    sink()

    out <- readLines("expsmoothfit.txt")

    usePackage("stringr")

    out.pattern <- str_detect(string=out, pattern="ETS")

    modelexpsmooth <- out[out.pattern==TRUE]
    usePackage("stringr")
    modelexpsmooth <- str_trim(modelexpsmooth)

    ## usePackage("stringr")

    ## modelexpsmooth <- modelexpsmooth

    #---

    modelexpsmooth.damped <- function(modelexpsmooth) {

        modelexpsmooth.damped.tmp <- strsplit(modelexpsmooth,",")[[1]][2]

        usePackage("stringi")

        modelexpsmooth.damped.tmp <- stri_detect_fixed(modelexpsmooth.damped.tmp,c("d"))

        damped <- modelexpsmooth.damped.tmp

        #---

        modelexpsmooth <- str_replace_all(modelexpsmooth, "ETS", "")
        modelexpsmooth <- str_replace_all(modelexpsmooth, "\\(", "")
        modelexpsmooth <- str_replace_all(modelexpsmooth, "\\)", "")
        modelexpsmooth <- str_replace_all(modelexpsmooth, ",", "")

        usePackage("stringr")

        model <- modelexpsmooth
        model <- str_replace(model,"d","")
        model

        list(model=model, damped=damped)

    }

    #---

    ## modelexpsmooth <- str_replace_all(modelexpsmooth, "ETS", "")
    ## modelexpsmooth <- str_replace_all(modelexpsmooth, "\\(", "")
    ## modelexpsmooth <- str_replace_all(modelexpsmooth, "\\)", "")
    ## modelexpsmooth <- str_replace_all(modelexpsmooth, ",", "")

    if (boxcoxtransform == TRUE) {

             out.lambda <- str_detect(string=out, pattern="lambda")
             mystr <- out[out.lambda==TRUE]
             mystr <- mystr[2]
             lambda.char <- regmatches(mystr,gregexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?",mystr))
             lambda <- as.numeric(lambda.char)

         } else {

             lambda <- NULL

    }


    fn <- "expsmoothfit.txt"
    if (file.exists(fn)) file.remove(fn)

    ####

    ## usePackage("foreach")

    expsmoothfcast <- function(x, modelexpsmooth, lambda){

          if (!is.null(lambda)) {
              series.boot <- x
          } else {

             series.boot <- x
             series.boot[series.boot==0] <- 0.001 # add small constant to zero counts

          }

          ## model0 <- forecast::ets(series.boot, model=expsmoothfit)

          damped.flag <-  modelexpsmooth.damped(modelexpsmooth)$damped
          model <- modelexpsmooth.damped(modelexpsmooth)$model

          model0 <- forecast::ets(series.boot, model=model, lambda=lambda, damped=damped.flag)


          ## p0 <- as.numeric(forecast::forecast(model0, h=1, level=0.80, lambda=lambda, biasadj=FALSE)$mean)
          p0 <- as.numeric(forecast::forecast(model0, h=1, biasadj=FALSE)$mean)
          p0 <- round(p0)
          return(p0)

    }


   series.boot.forecast.expsmooth <- apply(series.meboot2, 2, expsmoothfcast, modelexpsmooth, lambda)  ## one-year ahead forecasts for all time series in the ensemble

   y.paths <- series.boot.forecast.expsmooth   ## one-year ahead forecasts


   lower <- as.numeric(quantile(y.paths, 0.5 - level/200, type = 8))
   upper <- as.numeric(quantile(y.paths, 0.5 + level/200, type = 8))

   out <- NULL

   ## out$mean <-  as.numeric(rwf(series,h=1, drift=FALSE, level=level)$mean)

   ## model00 <- forecast::ets(series, model=modelexpsmooth, lambda=lambda)
   out$mean <- as.numeric(forecast::forecast(expsmoothfit , h=1, lambda=lambda, biasadj=FALSE)$mean)

   out$lower <- round(lower)
   out$upper <- round(upper)

   out$sim <- y.paths
   out$series <- out$series
   out$ensemble <- series.meboot2

   out$trim <- trim.optimal

   return(out)

}

