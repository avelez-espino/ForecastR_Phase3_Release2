cat("Effects - Sibling Regression.R", "\n\n")

usePackage("effects")

SIMPLESIBREG$Fixup.model.matrix.old <- function(mod, mod.matrix, mod.matrix.all, X.mod, factor.cols, 
    cnames, focal.predictors, excluded.predictors, typical, given.values, 
    partial.residuals = FALSE, mod.matrix.all.rounded) 
{

   has.intercept <- function (model, ...) { 
      any(names(coefficients(model)) == "(Intercept)")
   }

    vars <- as.character(attr(terms(mod), "variables"))[-(1:2)]
    attr(mod.matrix, "assign") <- attr(mod.matrix.all, "assign")
    if (length(excluded.predictors) > 0) {
        sel <- apply(sapply(excluded.predictors, matchVarName, 
            expressions = vars), 1, any)
        strangers <- Strangers(mod, focal.predictors, excluded.predictors)
        stranger.cols <- apply(outer(strangers, attr(mod.matrix, 
            "assign"), "=="), 2, any)
    }
    else stranger.cols <- rep(FALSE, ncol(mod.matrix))
    if (has.intercept(mod)) 
        stranger.cols[1] <- TRUE
    if (any(stranger.cols)) {
        facs <- factor.cols & stranger.cols
        covs <- (!factor.cols) & stranger.cols
        if (any(facs)) {
            mod.matrix[, facs] <- matrix(apply(as.matrix(X.mod[, 
                facs]), 2, mean), nrow = nrow(mod.matrix), ncol = sum(facs), 
                byrow = TRUE)
            if (partial.residuals) {
                mod.matrix.all.rounded[, facs] <- mod.matrix.all[, 
                  facs] <- matrix(apply(as.matrix(X.mod[, facs]), 
                  2, mean), nrow = nrow(mod.matrix.all), ncol = sum(facs), 
                  byrow = TRUE)
            }
        }
        if (any(covs)) {
            mod.matrix[, covs] <- matrix(apply(as.matrix(X.mod[, 
                covs]), 2, typical), nrow = nrow(mod.matrix), 
                ncol = sum(covs), byrow = TRUE)
            if (partial.residuals) {
                mod.matrix.all.rounded[, covs] <- mod.matrix.all[, 
                  covs] <- matrix(apply(as.matrix(X.mod[, covs]), 
                  2, typical), nrow = nrow(mod.matrix.all), ncol = sum(covs), 
                  byrow = TRUE)
            }
        }
        if (!is.null(given.values)) {
            stranger.names <- cnames[stranger.cols]
            given <- stranger.names %in% names(given.values)
            if (any(given)) {
                mod.matrix[, stranger.names[given]] <- matrix(given.values[stranger.names[given]], 
                  nrow = nrow(mod.matrix), ncol = length(stranger.names[given]), 
                  byrow = TRUE)
                if (partial.residuals) {
                  mod.matrix.all.rounded[, stranger.names[given]] <- mod.matrix.all[, 
                    stranger.names[given]] <- matrix(given.values[stranger.names[given]], 
                    nrow = nrow(mod.matrix.all), ncol = length(stranger.names[given]), 
                    byrow = TRUE)
                }
            }
        }
        for (name in cnames) {
            components <- unlist(strsplit(name, ":"))
            components <- components[components %in% cnames]
            if (length(components) > 1) {
                mod.matrix[, name] <- apply(mod.matrix[, components], 
                  1, prod)
                if (partial.residuals) {
                  mod.matrix.all[, name] <- apply(mod.matrix.all[, 
                    components], 1, prod)
                  mod.matrix.all.rounded[, name] <- apply(mod.matrix.all.rounded[, 
                    components], 1, prod)
                }
            }
        }
    }
    if (partial.residuals) 
        list(mod.matrix = mod.matrix, mod.matrix.all = mod.matrix.all, 
            mod.matrix.all.rounded = mod.matrix.all.rounded)
    else mod.matrix
}



SIMPLESIBREG$my.Effect.lm <- function(focal.predictors, mod, xlevels = list(), default.levels = NULL,
                                 given.values, se = TRUE, confidence.level = 0.95,
                                 transformation = list(link = family(mod)$linkfun, inverse = family(mod)$linkinv),
                                 typical = mean, offset = mean, partial.residuals = FALSE,
                                 quantiles = seq(0.2, 0.8, by = 0.2), x.var = NULL, ...){
    if (partial.residuals) {
        all.vars <- all.vars(formula(mod))
        data <- expand.model.frame(mod, all.vars)[, all.vars]
    } else { NULL }

    if (missing(given.values)){
        given.values <- NULL
    } else if (!all(which <- names(given.values) %in% names(coef(mod)))) {
        stop("given.values (", names(given.values[!which]), ") not in the model")
    }

    off <- if (is.numeric(offset) && length(offset) == 1) {
        offset
    } else if (is.function(offset)) {
        mod.off <- model.offset(model.frame(mod))
        if (is.null(mod.off)) {
            0
        } else { offset(mod.off) }
    } else stop("offset must be a function or a number")


    formula.rhs <- formula(mod)[[3]]

    ## predict.data

    model.components <- effects:::Analyze.model(focal.predictors, mod,
        xlevels, default.levels, formula.rhs, partial.residuals = partial.residuals,
        quantiles = quantiles, x.var = x.var, data = data)

    excluded.predictors <- model.components$excluded.predictors

    predict.data <- model.components$predict.data

    predict.data.all.rounded <- predict.data.all <- if (partial.residuals) {
       na.omit(data[, all.vars(formula(mod))])
    } else { NULL }


    factor.levels <- model.components$factor.levels
    factor.cols <- model.components$factor.cols
    n.focal <- model.components$n.focal
    x <- model.components$x
    X.mod <- model.components$X.mod
    cnames <- model.components$cnames
    X <- model.components$X
    x.var <- model.components$x.var
    formula.rhs <- formula(mod)[c(1, 3)]
    Terms <- delete.response(terms(mod))
    mf <- model.frame(Terms, predict.data, xlev = factor.levels,
        na.action = NULL)
    mod.matrix <- model.matrix(formula.rhs, data = mf, contrasts.arg = mod$contrasts)
    factors <- sapply(predict.data, is.factor)
    if (partial.residuals) {
        for (predictor in focal.predictors[-x.var]) {
            if (!factors[predictor]) {
                values <- unique(predict.data[, predictor])
                predict.data.all.rounded[, predictor] <- values[apply(outer(predict.data.all[,
                  predictor], values, function(x, y) (x - y)^2),
                  1, which.min)]
            }
        }
        mf.all <- model.frame(Terms, predict.data.all, xlev = factor.levels,
            na.action = NULL)
        mod.matrix.all <- model.matrix(formula.rhs, data = mf.all,
            contrasts.arg = mod$contrasts)
        mf.all.rounded <- model.frame(Terms, predict.data.all.rounded,
            xlev = factor.levels, na.action = NULL)
        mod.matrix.all.rounded <- model.matrix(formula.rhs, data = mf.all.rounded,
            contrasts.arg = mod$contrasts)
    } else { mod.matrix.all <- model.matrix(mod) }
    wts <- weights(mod)
    if (is.null(wts))
        wts <- rep(1, length(residuals(mod)))
        
    has.intercept <- function (model, ...) { 
        any(names(coefficients(model)) == "(Intercept)")
    }
        
    res <- SIMPLESIBREG$Fixup.model.matrix.old(mod=mod, mod.matrix=mod.matrix, mod.matrix.all=mod.matrix.all,
                                        X.mod=X.mod, factor.cols=factor.cols, cnames=cnames, 
                                        focal.predictors=focal.predictors, 
                                        excluded.predictors=excluded.predictors,
                                        typical=mean, given.values=given.values, 
                                        partial.residuals = partial.residuals,
                                        mod.matrix.all.rounded)
                                        
    mod.matrix <- if (partial.residuals){
     res$mod.matrix
    } else { res }
    mod.matrix.cases <- if (partial.residuals) {
        res$mod.matrix.all
    } else { NULL }
    mod.matrix.cases.rounded <- if (partial.residuals) {
        res$mod.matrix.all.rounded
    } else { NULL }
    
    mod.matrix <- mod.matrix[, !is.na(mod$coefficients), drop = FALSE]
    effect <- off + mod.matrix %*% mod$coefficients[!is.na(mod$coefficients)]


    if (partial.residuals) {
        mod.matrix.cases <- na.omit(mod.matrix.cases[, !is.na(mod$coefficients), drop = FALSE])
        fitted <- as.vector(off + mod.matrix.cases %*% mod$coefficients[!is.na(mod$coefficients)])
        ## mod.matrix.cases.rounded doesn't have the correct dimensions! it currently has a NULL dimension,
        ## when it fact it should have an n x 1 dimension (where n is the number of regression observations)
        dim(mod.matrix.cases.rounded)  <- dim(mod.matrix.cases) # force dimension of mod.matrix.cases.rounded
                                                                # to be the same as that of mod.matrix.cases, i.e., n x 1
        mod.matrix.cases.rounded <- na.omit(mod.matrix.cases.rounded[, !is.na(mod$coefficients), drop=FALSE])
        # quote(mod.matrix <- mod.matrix[, !is.na(mod$coefficients), drop=FALSE])
        fitted.rounded <- as.vector(off + mod.matrix.cases.rounded %*%
            mod$coefficients[!is.na(mod$coefficients)])
        res <- na.omit(residuals(mod, type = "working"))
        partial.residuals.raw <- fitted + res
        partial.residuals.adjusted <- fitted.rounded + res

    } else {
        partial.residuals.raw <- partial.residuals.adjusted <- fitted.rounded <- fitted <- NULL
    }

    result <- list(term = paste(focal.predictors, collapse = "*"),
        formula = formula(mod), response = effects:::response.name(mod),
        variables = x, fit = effect, x = predict.data[, 1:n.focal,
            drop = FALSE], x.all = predict.data.all.rounded,
        model.matrix = mod.matrix, mod.matrix.all = mod.matrix.cases,
        data = X, discrepancy = 0, offset = off, fitted.rounded = fitted.rounded,
        fitted = fitted, partial.residuals.raw = partial.residuals.raw,
        partial.residuals.adjusted = partial.residuals.adjusted,
        x.var = x.var)
        
    whichFact <- unlist(lapply(result$variables, function(x) x$is.factor))
    
    zeroes <- NULL
    
    if (sum(whichFact) > 1) {
        nameFact <- names(whichFact)[whichFact]
        counts <- xtabs(as.formula(paste("~", paste(nameFact,
            collapse = "+"))), model.frame(mod))
        zeroes <- which(counts == 0)
    }
    
    if (length(zeroes) > 0) {
        levs <- expand.grid(lapply(result$variables, function(x) x$levels))
        good <- rep(TRUE, dim(levs)[1])
        for (z in zeroes) {
            good <- good & apply(levs, 1, function(x) !all(x ==
                levs[z, whichFact]))
        }
        result$fit[!good] <- NA
    }
    
    if (se) {
        if (any(family(mod)$family == c("binomial", "poisson"))) {
            dispersion <- 1
            z <- qnorm(1 - (1 - confidence.level)/2)
        }
        else {
            dispersion <- sum(wts * (residuals(mod))^2, na.rm = TRUE)/mod$df.residual  # dispersion is squared residual standard error?
            z <- qt(1 - (1 - confidence.level)/2, df = mod$df.residual)
        }
        V2 <- dispersion * summary.lm(mod)$cov
        V1 <- vcov(mod)
        V <- if (inherits(mod, "fakeglm")) {
             V1
        } else { V2 }
        vcov <- mod.matrix %*% V %*% t(mod.matrix)
        rownames(vcov) <- colnames(vcov) <- NULL
        var <- diag(vcov)
        result$vcov <- vcov
        result$se <- sqrt(var)
        result$predictor <- mod.matrix[ ,focal.predictors]  ## report value of predictor used for computation of confidence bands
        result$lower <- effect - z * result$se
        result$upper <- effect + z * result$se
        result$confidence.level <- confidence.level
        if (length(zeroes) > 0) {
            result$se[!good] <- NA
            result$lower[!good] <- NA
            result$upper[!good] <- NA
        }
    }
    
    if (is.null(transformation$link) && is.null(transformation$inverse)) {
        transformation$link <- I
        transformation$inverse <- I
    }
    result$transformation <- transformation
    ## print(result)

    ## report the value of the predictor variable used to computed the raw partial residual
    result$predictor.partial.residual.raw <- predict.data.all.rounded[,focal.predictors]
    result$excluded.predictors <- excluded.predictors
    result$focal.predictors <- focal.predictors
    result$outcome <- all.vars[1]
    class(result) <- "eff"
    result
}



SIMPLESIBREG$my.effect.plot.multiple.predictor <- function(results_best_fitting_model_for_each_age_class, stockabundance, i){ # i is for ith age class

    .e <- environment()

    usePackage("scales")

    mod <- results_best_fitting_model_for_each_age_class[[i]]
    mod.data <- mod$data
    mod.formula <- mod$formula
    usePackage("stringr")
    mod.formula <- str_replace(as.character(mod.formula),"-1","0")
    mod.formula <- as.formula(mod.formula)
    mod.formula

    mod.data <- mod.data[ , names(mod.data) %in% all.vars(mod.formula)]
    mod <- lm(mod.formula, mod.data)

    my.result <- SIMPLESIBREG$my.Effect.lm(all.vars(mod.formula)[2], mod=mod, se=TRUE, partial.residuals=TRUE)

    if (length(my.result$excluded.predictors)==0) {

    # 51 x 2

    	my.result <- SIMPLESIBREG$my.Effect.lm(all.vars(mod.formula)[2], mod=mod, se=TRUE, partial.residuals=TRUE)

    	my.df <- data.frame(predictor.partial.residual.raw = my.result$predictor.partial.residual.raw,
                        fitted=my.result$fitted, partial.residuals.raw=my.result$partial.residuals.raw)

    	# 100 x 2
    	my.df.bands <- data.frame(predictor=my.result$predictor,lower=my.result$lower, upper=my.result$upper)

    	gg <- ggplot(data=my.df.bands, environment=.e) +
               geom_ribbon(data=my.df.bands, aes(x=predictor, ymin=lower, ymax=upper), fill="pink") +
                geom_point(data=my.df,aes(predictor.partial.residual.raw, partial.residuals.raw),
                           colour="darkblue", alpha=0.5) +
              geom_line(data=my.df, aes(predictor.partial.residual.raw, fitted),col="red", size=0.8) +
                ## labs(x=paste(my.result$focal.predictors)) +
                  ## labs(y=paste(my.result$outcome))  +
                   ggtitle(paste(my.result$focal.predictors,"Effect Plot")) +
                    scale_y_continuous(paste(my.result$outcome, stockabundance),labels=comma) +
                     scale_x_continuous(paste(my.result$focal.predictors, stockabundance),labels=comma) +
                       theme_bw() +
                        theme(axis.title.x=element_text(size=10,vjust=-0.5),
                         axis.title.y=element_text(size=10,vjust=1.5),
                          axis.text.x=element_text(size=8),
                           axis.text.y=element_text(size=8),
                            strip.text.x = element_text(size = 8, colour = "black", angle = 0))




     }


    if (length(my.result$excluded.predictors)>0) {

    # 51 x 2


      for (k in 1:length(all.vars(mod.formula)[-1])) {

        allvars <-  all.vars(mod.formula)[-1]
    		my.result <- SIMPLESIBREG$my.Effect.lm(focal.predictors=allvars[k], mod=mod, se=TRUE, partial.residuals=TRUE)

        my.df <- data.frame(predictor.partial.residual.raw = my.result$predictor.partial.residual.raw,
                        fitted=my.result$fitted, partial.residuals.raw=my.result$partial.residuals.raw)

        my.result$focal.predictors
        my.result$excluded.predictors


    		# 100 x 2
    		my.df.bands <- data.frame(predictor=my.result$predictor,lower=my.result$lower, upper=my.result$upper)

        assign(paste("p", k, sep=""),

               ggplot(data=my.df.bands, environment=.e) +
                        geom_ribbon(data=my.df.bands, aes(x=predictor, ymin=lower, ymax=upper), fill="pink") +
                         geom_point(data=my.df,aes(predictor.partial.residual.raw, partial.residuals.raw),
                                    colour="darkblue", alpha=0.5) +
                          geom_line(data=my.df, aes(predictor.partial.residual.raw, fitted),col="red", size=0.8) +
                           ggtitle(paste(my.result$focal.predictors,"Effect Plot")) +
                            scale_y_continuous(paste(my.result$outcome, stockabundance),labels=comma) +
                             scale_x_continuous(paste(my.result$focal.predictors, stockabundance),labels=comma) +
                              theme_bw() +
                               theme(axis.title.x=element_text(size=10,vjust=-0.5),
                                axis.title.y=element_text(size=10,vjust=1.5),
                                 axis.text.x=element_text(size=8),
                                  axis.text.y=element_text(size=8),
                                   strip.text.x = element_text(size = 8, colour = "black", angle = 0))
                )


      }  # end for

       ## gList(ggplotGrob(gp1),ggplotGrob(gp2))

      ## gg
      ## do.call("grid.arrange", c(gg,ncol=1))

      usePackage("gridExtra")

      tmptext <- paste0("gg <- arrangeGrob(")
      for (k in 1:length(all.vars(mod.formula)[-1])) {
           if (k < length(all.vars(mod.formula)[-1])){
               tmptext <- paste0(tmptext, "p", k, ",")
           } else {
               tmptext <- paste0(tmptext,"p",k,")")
           }
      }


      eval(parse(text=paste(tmptext,sep="")))




     } # end if


    return(gg)

}


SIMPLESIBREG$my.effect.plot.multiple.predictor(
   SIMPLESIBREG$results_best_fitting_model_for_each_age_class, 
   SIMPLESIBREG$stockabundance, i=1)



