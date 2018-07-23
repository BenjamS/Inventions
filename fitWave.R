fitWave <- function(ts, t, t_proj = NA, n_max_periods = 15, pval_thresh = 0.05, quietly = T){
  #---------------------
  #Make sure required packages are loaded
  #---------------------
  required_packages <- c("stats")
  lapply(required_packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
  #---------------------
  ssp <- spectrum(ts)
  df_per <- data.frame(per = 1 / ssp$freq, spec = ssp$spec)
  df_per <- df_per[order(df_per$spec, decreasing = T), ]
  per <- df_per$per
  #---------------------
  n_max_periods <- min(length(per), n_max_periods)
  #---------------------
  regrsrs_sin <- paste0("sin(2 * pi / per[", c(1:n_max_periods), "] * t)", collapse = " + ")
  regrsrs_cos <- paste0("cos(2 * pi / per[", c(1:n_max_periods), "] * t)", collapse = " + ")
  regrsrs <- paste(regrsrs_sin, regrsrs_cos, sep = " + ")
  this_formula <- as.formula(paste("ts ~ -1 +", regrsrs)[1])
  linmod <- lm(this_formula)
  #---------------------
  summod <- summary(linmod)
  #print(summod)
  #---------------------
  pvals <- as.numeric(summod$coefficients[, 4])
  ind_rm <- which(pvals > pval_thresh)
  round <- 0
  while(length(ind_rm) > 0){
    round <- round + 1
    print(paste("round ", round))
    if(round == 7){
      print("Too many rounds, aborting.")
      break
    }
    regrsrs_char <- strsplit(as.character(regrsrs), " \\+ ")[[1]]
    regrsrs_char <- regrsrs_char[-ind_rm]
    regrsrs <- paste(regrsrs_char, collapse = " + ")
    this_formula <- as.formula(paste("ts ~ -1 +", regrsrs)[1])
    linmod <- lm(this_formula)
    #---------------------
    summod <- summary(linmod)
    #print(summod)
    pvals <- as.numeric(summod$coefficients[, 4])
    ind_rm <- which(pvals > pval_thresh)
  }
  #-----------------
  #Which frequencies contribute to the fit?
  v <- c()
  for(i in 1:length(regrsrs_char)){
    v[i] <- unlist(strsplit(regrsrs_char[i], " "))[5]
    }
  ind_fitted_periods <- as.numeric(gsub("\\D", "", unique(v)))
  fitted_periods <- sort(round(per[ind_fitted_periods], 2), decreasing = T)
  #----------------
  if(is.numeric(t_proj)){
      wavproj <- predict(linmod, newdata = data.frame(t = t_proj))
  }
  #----------------
  if(quietly == F){
    print(summod)
    print(paste("Number periods fitted:", length(fitted_periods)))
    print(fitted_periods)
    print(df_per[1:n_max_periods, ])
    }
  wavfit <- fitted(linmod)
  if(is.numeric(t_proj)){
    outlist <- list(wavfit, wavproj)
    return(outlist)
  }else{
    return(wavfit)
  }
}
