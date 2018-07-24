collectiveModes <- function(mat_diff, datevec, df_group = NULL,
                            Contrib_as_ModeSq = T,
                            AggregateContributions = T,
                            plot_eigenportfolios = F){
  
  col_order <- colnames(mat_diff)
  cormat <- cor(mat_diff)
  image(cormat)
  #-----------------------------
  eig_vectors <- eigen(cormat)$vectors
  lam_cor <- eigen(cormat)$values
  lamcor_max <- max(lam_cor)
  N_t <- nrow(mat_diff)
  N_c <- ncol(mat_diff)
  Q <- N_t / N_c
  s_sq <- 1 - lamcor_max / N_c
  #s_sq <- 1
  lamrand_max <- s_sq * (1 + 1 / Q + 2 / sqrt(Q))
  lamrand_min <- s_sq * (1 + 1 / Q - 2 / sqrt(Q))
  lam <- seq(lamrand_min, lamrand_max, 0.001)
  dens_rand <- Q / (2 * pi * s_sq) * sqrt((lamrand_max - lam) * (lam - lamrand_min)) / lam
  df_e <- data.frame(values = lam_cor)
  #--
  gg <- ggplot() +
    geom_density(data = df_e, aes(x = values, color = "Correlation Matrix")) +
    #geom_histogram(data = df_e, aes(x = values), alpha = 0.2) +
    geom_line(data = data.frame(x = lam, y = dens_rand), aes(x = x, y = y, color = "Random matrix")) +
    coord_cartesian(xlim = c(0, ceiling(lamcor_max))) +
    scale_colour_manual(name = "Eigenvalue density", 
                        values = c(`Correlation Matrix` = "blue", `Random matrix` = "orange"))
  
  print(gg)
  #-----------------------------
  # How many collective modes?
  ind_deviating_from_noise <- which(lam_cor > lamrand_max)
  collModes <- as.matrix(eig_vectors[, ind_deviating_from_noise])
  n_collModes <- ncol(collModes)
  print(paste("Number of collective modes: ", n_collModes))
  if(ncol(collModes) > 6){
    collModes <- collModes[, 1:6]
    n_collModes <- 6
  }
  #-----------------------------
  # Contributions of items to each mode
  if(Contrib_as_ModeSq == T){
    Contribution <- (collModes)^2
  }else{
    Contribution <- collModes
  }
  df_collModes <- data.frame(ts_id = col_order, Contribution)
  mode_id <- c(1:n_collModes)
  colnames(df_collModes)[2:(n_collModes + 1)] <- mode_id
  gathercols <- as.character(mode_id)
  df_collModes <- gather_(df_collModes, "Mode", "Contribution", gathercols)
  n_ts <- ncol(mat_diff)
  #-----------------------------
  if(is.null(df_group) == F){
    n_group_types <- ncol(df_group) - 1
    group_types <- colnames(df_group)[2:(n_group_types + 1)]
    ind_group_types <- (ncol(df_collModes) + 1):(ncol(df_collModes) + n_group_types)
    df_plot <- cbind(df_collModes, df_group[, c(2:ncol(df_group))])
    colnames(df_plot)[ind_group_types] <- group_types
    for(i in 1:n_group_types){
      ind_i <- i + 1
      this_group_label <- colnames(df_group)[ind_i]
      #df_plot <- df_plot[order(df_plot[, ind_group_types[i]]), ]
      group_label <- paste0("`", this_group_label, "`")
      #---
      if(AggregateContributions == F){
        #-----------------------
        xx <- df_plot[, ind_group_types[i]]
        df_plot$ts_id <- factor(df_plot$ts_id, levels = unique(df_plot$ts_id[order(xx)]))
        gg <- ggplot(df_plot, aes_string(x = "ts_id", y = "Contribution", fill = group_label)) +
          geom_bar(stat = "identity", position = "dodge") +
          #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
          facet_wrap(~ Mode, nrow = round(n_collModes / 2))
        if(n_ts <= 50){
          gg <- gg + theme(axis.text.x = element_text(angle = 60, hjust = 1))
        }else{
          gg <- gg + theme(axis.text.x = element_blank(),
                           axis.title.x = element_blank())
        }
        print(gg)
        #-----------------------
      }else{
        #-----------------------
        N_in_group <- c()
        Pvec_list <- list()
        these_groups <- unique(df_group[, ind_i])
        n_groups <- length(these_groups)
        for(j in 1:n_groups){
          this_group <- these_groups[j]
          Pvec <- rep(0, n_ts)
          ind_this_group <- which(df_group[, ind_i] == this_group)
          ts_in_group <- df_group[ind_this_group, 1]
          n_in_group <- length(ts_in_group)
          Pvec[ind_this_group] <- 1 / n_in_group
          Pvec_list[[j]] <- Pvec
          N_in_group[j] <- n_in_group
        }
        Pmat <- as.matrix(do.call(cbind, Pvec_list))
        #--------------------------
        mat_groupContrib <- t(Pmat) %*% Contribution
        #--------------------------
        df_groupContrib <- as.data.frame(mat_groupContrib)
        colnames(df_groupContrib) <- c(1:n_collModes)
        df_groupContrib$Group <- these_groups
        gathercols <- colnames(df_groupContrib)[1:n_collModes]
        df_plot <- gather_(df_groupContrib, "Mode", "Contribution", gathercols)
        if(n_collModes == 1){
          df_plot <- df_groupContrib
          colnames(df_plot)[1] <- "Value"
          df_plot$Mode <- 1
        }
        gg <- ggplot(df_plot, aes(x = Group, y = Contribution)) +
          geom_bar(stat = "identity") +
          facet_wrap(~ Mode, nrow = floor(n_collModes / 2)) +
          theme(axis.text.x = element_text(angle = 60, hjust = 1))
        print(gg)
        #-----------------------
      }
    }
    
  }else{
    #-----------------------
    df_plot <- df_collModes
    gg <- ggplot(df_plot, aes_string(x = "ts_id", y = "Contribution")) +
      geom_bar(stat = "identity", position = "dodge") +
      #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      facet_wrap(~ Mode, nrow = round(n_collModes / 2))
    if(n_ts <= 50){
      gg <- gg + theme(axis.text.x = element_text(angle = 60, hjust = 1))
    }else{
      gg <- gg + theme(axis.text.x = element_blank(),
                       axis.title.x = element_blank())
    }
    print(gg)
    #-----------------------
    
  }
  #-----------------------------
  # Main contributors to each mode
  select_main_contributors <- function(colvec, col_order, p_thresh){
    abs_colvec <- abs(colvec)
    q <- quantile(abs_colvec, p = p_thresh)
    ind_main <- which(abs_colvec >= q)
    top_contrib <- sort(abs_colvec[ind_main], decreasing = T)
    ind_match <- match(top_contrib, abs_colvec)
    main_contribtrs <- col_order[ind_match]
    return(main_contribtrs)
  }
  p_thresh <- .90
  Main_contributors <- apply(collModes[, 2:ncol(collModes)], 2,
                             function(x) select_main_contributors(x, col_order, p_thresh))
  print("Main contributors to each non-leading mode:")
  print(Main_contributors)
  #-----------------------------
  # Collective mode time series
  mat_cmts <- mat_diff %*% collModes
  ts_avg <- mat_diff %*% rep(1, n_ts) * 1 / n_ts
  # class(mat_cmts)
  # class(ts_avg)
  df_plot <- as.data.frame(mat_cmts)
  colnames(df_plot) <- mode_id
  df_plot$`ts Avg.` <- ts_avg
  df_plot$Date <- date_vec
  if(class(df_plot$Date) == "character"){
    df_plot$Date <- as.Date(df_plot$Date)
  }
  df_cmts <- df_plot
  gathercols <- colnames(df_plot)[c(1:(ncol(df_plot) - 1))]
  df_plot <- df_plot %>% gather_("Mode", "Value", gathercols)
  if(plot_eigenportfolios == T){
    zdf_plot <- df_plot %>% group_by(Mode) %>% mutate(Value = scale(Value))
    #--
    zdf_plot1 <- subset(zdf_plot, Mode %in% c("1", "ts Avg."))
    gg <- ggplot(zdf_plot1, aes(x = Date, y = zValue,
                                group = Mode, color = Mode))
    gg <- gg + geom_line()
    print(gg)
    zdf_plot2 <- subset(zdf_plot, !(Mode %in% c("1", "ts Avg.")))
    gg <- ggplot(zdf_plot2, aes(x = Date, y = zValue)) + geom_line() +
      facet_wrap(~Mode, ncol = 2)
    print(gg)
    
  }
  #-----------------------------
  ind_avg <- which(df_plot$Mode == "ts Avg.")
  df_plot$Value[ind_avg] <- rnorm(length(ind_avg))
  df_plot$Mode[ind_avg] <- "Porter-Thomas"
  gg <- ggplot(df_plot, aes(x = Value, group = Mode, color = Mode)) +
    geom_density()
  print(gg)
  #-----------------------------
  outlist <- list(as.data.frame(collModes), df_cmts)
  return(outlist)
}
