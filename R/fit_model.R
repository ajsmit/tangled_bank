#' fit_model
#'
#' an internal function for Hdotplot
#' @import ggplot2
#' @import lsmeans
#' @import Hmisc
#' @import broom
#' @import lsmeans
#' @import car
#' @import data.table
#' @export
fit_model <- function(
  x,
  y,
  g,
  covcols=NULL,
  rintcols=NULL,
  rslopecols=NULL,
  dt,
  fit.model='lm', # lm, glm
  error='Normal', # normal, lognormal, logistic, poisson
  add_interaction=FALSE,
  interaction.group=FALSE,
  interaction.treatment=TRUE,
  mean_intervals.method='raw', # model for CI of mean
  conf.mean=0.95, # confidence level for CI of mean
  contrasts.method='trt.vs.ctrl1', # which contrasts to show
  contrasts.scaling='raw',
  conf.contrast=0.95,
  adjust=FALSE
){
  if(g=='dummy_g'){
    xcols <- x
    grouping <- FALSE
  }else{
    xcols <- c(x,g)
    grouping <- TRUE
  }

  if(add_interaction==TRUE){
    icols <- c(x,g)
  }else{
    icols <- NULL
  }

  model_formula <- formula(make_formula_str(y, xcols, rintcols, rslopecols, icols, covcols))

  if(fit.model=='lm'){
    fit <- lm(model_formula, data=dt)
  }
  if(fit.model=='lmm'){
    fit <- lmer(model_formula, data=dt)
  }
  lsm <- lsmeans(fit, specs=xcols)

  # save global
  tables <- list(NULL)
  tables$fit <- fit
  tables$form_str <- model_formula
  tables$coeffs <- coefficients(summary(fit))
  tables$summary <- glance(fit)
  tables$summary.raw <- summary(fit)
  tables$means.raw <- lsm
  # anova tables
  if(fit.model=='lm'){
    tables$anova.1 <- anova(fit)
    tables$anova.2 <- Anova(fit, type='II')
    options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
    tables$anova.3 <- Anova(lm(model_formula, data=dt), type='III')
    options(contrasts=c(unordered="contr.treatment", ordered="contr.poly"))
  }
  if(fit.model=='lmm'){
    # tables$anova.3 <- Anova(lmer(model_formula, data=dt), type='III')
    tables$anova.1 <- anova(lmer(model_formula, data=dt), type=1)
    tables$anova.2 <- anova(lmer(model_formula, data=dt), type=2)
    tables$anova.3 <- anova(lmer(model_formula, data=dt), type=3)
  }


  #     Bayes model
  #  mad <- median(abs(dt[,y] - mean(dt[,y])))
  #  #dt[,y.mad:=y/mad]
  #  y.mad <- dt[,y]/mad
  #  fit.mcmc <- MCMCregress(y.mad ~ x, data=dt, b0 = 0, B0 = 0.1, c0=2, d0=0.11)
  #  post.lsm <- lsmeans(fit.mcmc, specs='x')
  # # dt <- dt[, .SD, .SDcols=c('x','y')] #drop y.mad because need to bind ci later

  # means intervals
  if(mean_intervals.method=='lm'){
    tables$means <- confint(lsm, level=conf.mean)
    ci_means <- data.table(tables$means) # mean intervals not adjusted
    ci_means <- ci_means[, .SD, .SDcols=c(xcols,'lsmean','lower.CL','upper.CL')]
  }
  if(mean_intervals.method=='raw'){
    conf.tail <- conf.mean + (1-conf.mean)/2
    tables$means <- dt[, .(
      mean=mean(get(y)),
      sem=sd(get(y))/sqrt(.N),
      lower=mean(get(y))-sd(get(y))/sqrt(.N)*qt(conf.tail,(.N-1)),
      upper=mean(get(y))+sd(get(y))/sqrt(.N)*qt(conf.tail,(.N-1))),
      by=xcols]
    ci_means <- tables$means[, .SD, .SDcols=c(xcols,'mean', 'lower','upper')]
  }
  if(mean_intervals.method=='boot'){
    dt_boot <- data.table(dt[, smean.cl.boot(get(y),conf.int=conf.mean), by=xcols])
    dt_boot[, tile:=c('a','lower','upper')]
    form <- formula(paste(paste(xcols,collapse='+'),'tile',sep='~'))
    ci_means <- dcast(dt_boot, form, value.var='V1') #**** change x+g to formula
  }
  # if(mean_intervals.method=='bayes'){
  #   conf.tail <- conf.mean + (1-conf.mean)/2
  #   res <- summary(as.mcmc(post.lsm), quantiles = c(0.5, (1-conf.tail), conf.tail))$quantiles*mad
  #   ci_means <- data.table(x=row.names(res),res)
  #   ci_means[, (x):=factor(substr(x,3,nchar(x)))]
  # }
  if(grouping==FALSE){
    ci_means[, (g):='dummy']
    setnames(ci_means, old=colnames(ci_means), new=c(x, y, 'lower', 'upper', g))
    ci_means <- ci_means[,.SD, .SDcols=c(x, g, y,'lower','upper')]
  }else{
    setnames(ci_means, old=colnames(ci_means), new=c(xcols, y, 'lower','upper'))
  }

  #     contrast intervals
  x_levels <- levels(dt[, get(x)]) # levels for means plot
  g_levels <- levels(dt[, get(g)])
  n_levels <- length(x_levels)
  n_groups <- length(g_levels)

  if(contrasts.method=='coefficients'){
    ci_diffs <- coefficients(summary(fit))

    # get rid of df column from lmer fit
    if(fit.model=='lmm'){
      ci_diffs <- ci_diffs[,-which(colnames(ci_diffs)=='df')]
    }

    x_names <- c(x_levels[-1], g_levels[-1])
    if(add_interaction==TRUE){
      temp <- expand.grid(x_levels[-1], g_levels[-1])
      x_names <- c(x_names, paste(temp$Var1, temp$Var2, sep=':'))
    }
    # confint
    ci_ci <- confint(fit, level=conf.contrast)[row.names(ci_diffs),]
    ci_diffs <- cbind(ci_diffs, ci_ci)
    tables$contrasts <- data.table(contrast=x_names, ci_diffs[-1,])
    ci_diffs <- data.table(contrast=x_names, g='dummy', ci_diffs[-1,])
    setnames(ci_diffs, old=colnames(ci_diffs), new = c('contrast', 'g', 'estimate', 'Std. Error', 't value', 'Pr(>|t|)', 'lower', 'upper'))
    ci_diffs <- ci_diffs[, .SD, .SDcols=c('contrast','g','estimate','lower','upper')]
    ci_diffs[, contrast:=factor(contrast, levels=x_names)]
  }
  if(contrasts.method!='coefficients'){
    ci.adjust <- 'none'
    if(adjust==TRUE){
      ci.adjust <- ifelse(contrasts.method=='trt.vs.ctrl1', 'dunnettx','tukey')
    }
    if(grouping==FALSE | add_interaction==TRUE){
      ci_diffs <- summary(contrast(lsm, method=contrasts.method), adjust=ci.adjust, level=conf.contrast, infer=c(TRUE,TRUE))
      tables$contrasts.raw <- ci_diffs
      if(grouping==TRUE & contrasts.method=='revpairwise'){ # subset into pairwise within each group
        # another method
        # fread(paste(as.character(ci_diffs$contras), collapse='\n'), sep='-')
        inc <- NULL
        split1 <- data.frame(t(do.call("cbind", strsplit(as.character(ci_diffs$contrast)," - "))))
        split2a <- data.frame(t(do.call("cbind", strsplit(as.character(split1$X1),","))))
        colnames(split2a) <- c('x1','g1')
        split2b <- data.frame(t(do.call("cbind", strsplit(as.character(split1$X2),","))))
        colnames(split2b) <- c('x2','g2')
        splits <- data.table(split2a, split2b)
        # splits[, x1:=as.character(x1)]
        # splits[, x2:=as.character(x2)]
        # splits[, g1:=as.character(g1)]
        # splits[, g2:=as.character(g2)]
        if(interaction.group==TRUE){
          inc <- c(inc, which(splits[,g1]==splits[,g2]))
        }
        if(interaction.treatment==TRUE){
          inc.x <- which(splits[,x1]==splits[,x2])
          t.x <- factor(splits[inc.x, x1], levels(dt[, get(x)]))
          inc <- c(inc, inc.x[order(t.x)])
        }
        ci_diffs <- ci_diffs[inc,]
      }
      tables$contrasts <- ci_diffs
      ci_diffs <- data.table(ci_diffs, g='dummy')
    }
    if(grouping==TRUE & add_interaction==FALSE){
      if(contrasts.method=='revpairwise'){
        p_levels <- n_levels*(n_levels-1)/2
        p_groups <- n_groups*(n_groups-1)/2
      }else{
        p_levels <- n_levels-1
        p_groups <- n_groups-1
      }
      diffs.x <- summary(contrast(lsm, method=contrasts.method, by=g), adjust=ci.adjust, level=conf.contrast, infer=c(TRUE,TRUE))
      ci_diffs.x <- data.table(diffs.x)[1:p_levels]
      setnames(ci_diffs.x, old=c(g), new='by')
      diffs.g <- summary(contrast(lsm, method=contrasts.method, by=x), adjust=ci.adjust, level=conf.contrast, infer=c(TRUE,TRUE))
      ci_diffs.g <- data.table(diffs.g)[1:p_groups]
      setnames(ci_diffs.g, old=c(x), new='by')
      # save to tables
      tables$contrasts.raw <- list(by_treatment=diffs.x, by_grouping=diffs.g)
      # combine
      ci_diffs <- data.table(NULL)
      if(interaction.treatment==TRUE){ci_diffs <- rbind(ci_diffs, ci_diffs.x)}
      if(interaction.group==TRUE){ci_diffs <- rbind(ci_diffs, ci_diffs.g)}
      tables$contrasts <- copy(ci_diffs)
      setnames(ci_diffs, old=c('by'), new='g')
      # ci_diffs.x[, g:='x']
      # ci_diffs.g[, g:='g']
    }
    ci_diffs <- ci_diffs[, .SD, .SDcols=c('contrast','g','estimate','lower.CL','upper.CL')]
  }
  if(fit.model=='bayes'){
    conf.tail <- conf.contrast + (1-conf.contrast)/2
    res <- summary(as.mcmc(contrast(post.lsm, method=contrasts.method)), quantiles = c(0.5, (1-conf.tail), conf.tail))$quantiles*mad
    ci_diffs <- data.table(x=row.names(res),res)
    ci_diffs[, x:=factor(substr(x,10,nchar(x)))]
  }

  # make sure factor order of ci_diffs is in order they appear in the table
  ci_diffs[, contrast:=factor(contrast, ci_diffs$contrast)]

  yscale <- 1 # default
  if(contrasts.scaling=='standardized'){
    yscale <- summary(fit)$sigma
    ci_diffs[, estimate:=estimate/yscale]
    ci_diffs[, lower.CL:=lower.CL/yscale]
    ci_diffs[, upper.CL:=upper.CL/yscale]

    # scale tables$contrasts
    tables$contrasts <- data.table(tables$contrasts)
    tables$contrasts[, estimate:=estimate/yscale]
    tables$contrasts[, SE:=SE/yscale]
    tables$contrasts[, lower.CL:=lower.CL/yscale]
    tables$contrasts[, upper.CL:=upper.CL/yscale]
  }
  if(contrasts.scaling=='percent'){
    scale.o <- ci_diffs[1, estimate]
    group_names <- contrast.groups(ci_diffs, grouping, add_interaction)
    x1 <- group_names[, X2]
    x2 <- ci_means[, get(x)]
    if(grouping==TRUE & add_interaction==FALSE){
      g1 <- group_names[, X1]
      g2 <- group_names[, X2]
      xmean <- ci_means[, .(mean=mean(get(y))), by=get(x)]
      gmean <- ci_means[, .(mean=mean(get(y))), by=get(g)]
      inc.g1.x <- na.omit(match(g1, xmean$get))
      inc.g2.x <- na.omit(match(g2, xmean$get))
      num.x <- xmean[inc.g1.x, mean]
      denom.x <- xmean[inc.g2.x, mean]
      inc.g1.g <- na.omit(match(g1, gmean$get))
      inc.g2.g <- na.omit(match(g2, gmean$get))
      num.g <- gmean[inc.g1.g, mean]
      denom.g <- gmean[inc.g2.g, mean]
      denom <- c(denom.x, denom.g)
    }else{
      if(grouping==TRUE & add_interaction==TRUE){
        x1 <- paste(x1, group_names[, G2])
        x2 <- paste(x2, ci_means[, get(g)])
      }
      inc <- match(x1, x2)
      denom <- ci_means[inc, get(y)]
    }
    ci_diffs[, estimate:=100*estimate/denom]
    ci_diffs[, lower.CL:=100*lower.CL/denom]
    ci_diffs[, upper.CL:=100*upper.CL/denom]

    # # rescale back to scale.o
    # yscale <- scale.o/ci_diffs[1, estimate]
    # ci_diffs[, estimate:=estimate*yscale]
    # ci_diffs[, lower.CL:=lower.CL*yscale]
    # ci_diffs[, upper.CL:=upper.CL*yscale]

    # scale tables$contrasts
    tables$contrasts <- data.table(tables$contrasts)
    tables$contrasts[, estimate:=100*estimate/denom]
    tables$contrasts[, SE:=100*SE/denom]
    tables$contrasts[, lower.CL:=100*lower.CL/denom]
    tables$contrasts[, upper.CL:=100*upper.CL/denom]

  }

  setnames(ci_diffs, old=colnames(ci_diffs), new=c(x, g, y,'lower','upper'))
  return(list(fit=fit, ci_means=ci_means, ci_diffs=ci_diffs, tables=tables, yscale=yscale))
}
