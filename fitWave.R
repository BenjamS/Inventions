fitWave <- function(ts, t, t_proj = NULL, q_prob = 0.99,
                    pval_thresh = 0.01, nper_fit = NULL, quietly = T){
  #---------------------
  #Make sure required packages are loaded
  #---------------------
  required_packages <- c("stats", "ggplot2")
  lapply(required_packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
  #---------------------
  ssp <- spectrum(ts, plot = F)
  df_per_raw <- data.frame(per = 1 / ssp$freq, spectrum = ssp$spec)
  df_per_raw <- df_per_raw[order(df_per_raw$spec, decreasing = T), ]
  #--
  df_per <- df_per_raw
  q <- quantile(df_per$spec, probs = q_prob)
  ind_keep <- which(df_per$spec > q)
  minspec_keep <- min(df_per$spectrum[ind_keep])
  df_per <- df_per[ind_keep, ]
  ind_rm <- which(df_per_raw$per >= length(ts))
  if(length(ind_rm) != 0){
    df_per <- df_per[-ind_rm, ]
  }
  maxper_keep <- max(df_per$per)
  #--
  gg_per <- ggplot(df_per_raw, aes(x = per, y = spectrum)) + geom_line() +
    geom_vline(xintercept = maxper_keep * 1.2, color = "orange") +
    geom_hline(yintercept = minspec_keep, color = "orange")
  print(gg_per)
  #--
  per <- df_per$per
  spec <- df_per$spectrum
  #---------------------
  #n_max_periods <- min(length(per), n_max_periods)
  n_max_periods <- length(per)
  if(is.null(nper_fit) == F){
    n_max_periods <- nper_fit
  }
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
  # fitted_periods <- sort(round(spec[ind_fitted_periods], 2), decreasing = T)
  df_per_final <- df_per[ind_fitted_periods, ]
  n_pers_fitted <- nrow(df_per_final)
  #----------------
  wavfit <- as.numeric(fitted(linmod))
  #----------------
  if(quietly == F){
    print(summod)
    print(paste("Number periods fitted:", n_pers_fitted))
    print(df_per_final)
    }
  if(is.null(t_proj) == F){
    t_future <- length(ts) + t_proj
    wavproj <- as.numeric(predict(linmod, newdata = data.frame(t = t_future)))
    outlist <- list(wavfit, df_per_final, n_pers_fitted, wavproj)
  }else{
    outlist <- list(wavfit, df_per_final, n_pers_fitted)
  }
  return(outlist)
}
