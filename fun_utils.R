get_pve <- function(fit, pc) {
  fit$eigenvalues[pc] / sum(fit$eigenvalues)*100
}

plot_subject_trajectories <- function(fit, scores, tnew, list_data, 
                                      subjects_to_show, 
                                      vec_var, df_comb, 
                                      ylim_offset = 3.2,
                                      var_disp_names = NULL,
                                      bool_trunc = FALSE,
                                      bool_par = TRUE,
                                      main_plus = NULL,
                                      sub = NULL,
                                      ylab = NULL,
                                      cex_main = 1.5,
                                      cex_lab = 1) {
  
  require(mfaces)
  
  if (is.data.frame(list_data)) {
    data <- list("y1" = list_data) # in case of univariate fpca
  } else {
    data <- list_data # in case of multivariate fpca
  }
  
  if (bool_trunc) {
    df_comb$time <- df_comb$hospital_outcome_days_from_start_sx_or_first_pos_swab
    df_comb$time[!(df_comb$hospital_outcome %in% "dead")] <- df_comb$last_observation_days_from_start_sx_or_first_pos_swab[!(df_comb$hospital_outcome %in% "dead")]
    
    df_comb$hospital_outcome[df_comb$severity %in% c("HC", "A", "B")] <- "alive"
    
    df_comb$status <- ifelse(df_comb$hospital_outcome == "dead", 2, 1) # 1 = cencored, 2 = dead
  }
  
  if (bool_par) {
    par(mar=mar, mfrow = c(length(subjects_to_show), length(vec_var)))
  }

  # for y-axis limits
  sel_subj_to_show <- lapply(data, function(x){which(x$subj %in% subjects_to_show)})
  # print(sel_subj_to_show)
  dat_subj_to_show <- mapply(function(data, sel_subj_to_show){data[sel_subj_to_show,]}, 
                  data = data, sel_subj_to_show = sel_subj_to_show, SIMPLIFY = FALSE)
  
  for(i in subjects_to_show){
    
    sev <- unique(df_comb$severity[df_comb$subject_id %in% i])
    sel <- lapply(data, function(x){which(x$subj==i)})
    dat_i <- mapply(function(data, sel){data[sel,]}, 
                    data = data, sel = sel, SIMPLIFY = FALSE)
    dat_i_pred <- lapply(dat_i, function(x){
      data.frame(subj=rep(x$subj[1],nrow(x) + length(tnew)),
                 argvals = c(rep(NA,nrow(x)),tnew),
                 y = rep(NA,nrow(x) + length(tnew)))
    })
    for(j in 1:length(dat_i)){
      dat_i_pred[[j]][1:nrow(dat_i[[j]]), ] <- dat_i[[j]]
    }
    
    if (is.data.frame(list_data)) {
      dat_i_pred <- dat_i_pred[[1]]
    }
    
    pred <- predict(fit, dat_i_pred)
    y_pred <- mapply(function(pred_y.pred, dat_i){
      pred_y.pred[nrow(dat_i)+1:length(tnew)]}, pred_y.pred = pred$y.pred, 
      dat_i = dat_i, SIMPLIFY = TRUE)
    
    pre <- pred
    
    for (k in seq_along(data)) {
      
      vv <- vec_var[k]
      
      if (is.null(var_disp_names)) {
        var_disp_name <- vv
      } else {
        var_disp_name <- var_disp_names[k]
      }

      if (is.data.frame(list_data)) {
        y_pred_k <- pre$y.pred
        se_pred_k <- pre$se.pred
      } else {
        y_pred_k <- pre$y.pred[[paste0("y", k)]] 
        se_pred_k <- pre$se.pred[[paste0("y", k)]]
      }

      if (any(df_comb$severity == "HC")) {
        lo <- quantile(df_comb[df_comb$severity == "HC", vv], probs = 0.25, na.rm = TRUE)
        up <- quantile(df_comb[df_comb$severity == "HC", vv], probs = 0.75, na.rm = TRUE)
        
        mmax <- max(max(dat_subj_to_show[[k]]$y, na.rm = T), up, na.rm = T)
        mmin <- min(min(dat_subj_to_show[[k]]$y, na.rm = T), lo, na.rm = T)
      } else {
        mmax <- max(dat_subj_to_show[[k]]$y, na.rm = T)
        mmin <- min(dat_subj_to_show[[k]]$y, na.rm = T)
      }

      Ylim = c(max(0.5, mmin) - ylim_offset, mmax + ylim_offset)
      Xlim = c(0,max(data[[k]]$argvals)+1)
      Ylab = ifelse(is.null(ylab), paste0(var_disp_name, " (log-scale)"), ylab)#bquote(y^(1))
      Xlab = "Days from symptom onset"
      
      main <- var_disp_name
      if (!is.null(main_plus)) {
        main <- paste0(main, main_plus)
      } else {
        main <- paste0(main, ", subj. ", i, "\n (sev.: ", sev, 
                       ", gender: ", unique(df_comb$gender[df_comb$subject_id %in% i]), 
                       ", age: ", unique(df_comb$age[df_comb$subject_id %in% i]), 
                       ")")
      }
      
      if (is.null(sub)) {
        sub <- paste0("Scores 1st EF: ", format(scores$EF1[scores$subject_id %in% i], digits =2),
                      ", 2nd EF: ", format(scores$EF2[scores$subject_id %in% i], digits =2))
      }

      idx = (nrow(dat_i[[k]])+1):(nrow(dat_i[[k]])+length(tnew))
      plot(dat_i[[k]][,"argvals"],dat_i[[k]][,"y"],ylim=Ylim,xlim=Xlim,ylab=Ylab,xlab=Xlab,
           main=main,cex.lab=cex_lab,cex.axis = 1.0,cex.main = cex_main,pch=1, 
           sub = sub)

      if (any(df_comb$severity == "HC")) {
        days_thres <- tnew[length(tnew)] + 1
        rect(-5, lo, days_thres+5, up, col = adjustcolor("gray", alpha.f=0.5), border = "gray")
      }
      
      status <- unique(df_comb$status[df_comb$subject_id %in% i])
      time <- unique(df_comb$time[df_comb$subject_id %in% i])
      time <- time[!is.na(time)]

      stopifnot(length(status) == 1 & length(time) == 1)
      
      if (status == 2 & time < 50) { # cut prediction if patient died within the analysis window
        lines(tnew[tnew<=time], y_pred_k[idx][tnew<=time],col="red",lwd=2)
        points(time,  y_pred_k[idx][time], pch = 4, cex = 1.25, lwd = 1.5)
        lines(tnew[tnew<=time], y_pred_k[idx][tnew<=time]-1.96*se_pred_k[idx][tnew<=time],
              col="blue",lwd=2,lty=2)
        lines(tnew[tnew<=time], y_pred_k[idx][tnew<=time]+1.96*se_pred_k[idx][tnew<=time],
              col="blue",lwd=2,lty=2)
      } else {
        lines(tnew, y_pred_k[idx],col="red",lwd=2)
        lines(tnew,y_pred_k[idx]-1.96*se_pred_k[idx],col="blue",lwd=2,lty=2)
        lines(tnew,y_pred_k[idx]+1.96*se_pred_k[idx],col="blue",lwd=2,lty=2)
        # lines(tnew,fit$fit[[paste0("y", k)]]$mu.new,lwd=2,lty=3,col="black") # population average
      }
      
    }
    
  }
  
}


plot_variance <- function(fit, list_data, tnew, vec_var, disp_param = TRUE, 
                          var_disp_names = NULL) {
  
  if (is.data.frame(list_data)) {
    data <- list("y1" = list_data) # in case of univariate fpca
  } else {
    data <- list_data # in case of multivariate fpca
  }
  
  Cov <- as.matrix(fit$Chat.new)
  Cov_diag <- diag(Cov)
  
  if (disp_param) {
    par(mfrow=c(1, length(vec_var)),mar=c(4.5,4.1,3,4.5))
  }

  Xlab = "Days from symptom onset"
  
  if (!is.null(var_disp_names)) {
    vec_var <- var_disp_names
  }
  
  for (k in seq_along(data)) {
    vv <- vec_var[k]
    plot(tnew,Cov_diag[seq_along(tnew)+length(tnew)*(k-1)],type="l",
         xlab = Xlab, ylab="variance",main=vv,
         cex.axis=1.25,cex.lab=1.25,cex.main=1.25,lwd=2)
  }
  
}

plot_correlation <- function(fit, list_data, tnew, vec_var, nb_spaces = 3, nb_ticks = 4,
                             var_disp_names = NULL, sub_var = NULL) {
  
  if (is.data.frame(list_data)) {
    data <- list("y1" = list_data) # in case of univariate fpca
  } else {
    data <- list_data # in case of multivariate fpca
  }
  
  if (!is.null(var_disp_names)) {
    vec_var <- var_disp_names
  }
  
  if (!is.null(sub_var)) {
    data <- data[sub_var]
    ind_cor <- Reduce(c, sapply(sub_var, function(id) (id-1)*length(tnew) + (tnew+1)))
    fit$Cor.new <- fit$Cor.new[ind_cor, ind_cor, drop = FALSE]
    vec_var <- vec_var[sub_var]
  }
  
  Cor <- as.matrix(fit$Cor.new)

  Xlab = "Days from symptom onset"
  
  require(fields)
  par(mar=c(5,4.5,4,7))
  par(mfrow = c(1,1))
  mycols <- colorRampPalette(colors = c("blue","white", "red"))(200)
  
  nb_var <- length(data)
  vec_trunc <- stringr::str_trunc(vec_var, 25)
  vec_spaces <- ceiling(nb_spaces / length(vec_var) - nchar(vec_trunc))

  main <- paste0(sapply(1:length(vec_var), function(i) paste0(paste0(rep(" ", max(2, ceiling(vec_spaces[i]/2))), collapse = ""),
                                                     vec_trunc[i], paste0(rep(" ", max(2, ceiling(vec_spaces[i]/2))), collapse = ""))),
                 collapse = "")

  image(Cor,axes=F, col=mycols, xlab=Xlab, ylab = Xlab,
        main = main, cex.main = 0.95)
  
  ax <- as.vector(sapply(seq_along(data), function(k) {
    seq(0, 1/nb_var, l = nb_ticks) + (k-1)/nb_var
  }))
  axis(1,at=ax,labels=format(rep(seq(0, max(tnew), l = nb_ticks), times = nb_var), digits = 2))
  axis(2,at=ax,labels=format(rep(seq(0, max(tnew), l = nb_ticks), times = nb_var), digits = 2))
  
  image.plot(1:(nb_var*length(tnew)), 
             1:(nb_var*length(tnew)), 
             as.matrix(Cor),
             col=mycols,
             cex.axis=1.25,cex.lab=1,cex.main=1,
             # lab.breaks = rep(tnew, times = nb_var),
             axis.args = list(at = c(-1.0, -0.5, 0, 0.5,1.0)),
             legend.shrink=0.75,legend.line=-1.5, legend.only = T)
  
  
}

