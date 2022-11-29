
devtools::install_github(repo = "https://github.com/ChuanHong/ssROCtesting")
library(ssROCtesting)
n <- 300; N <- 5000 - 300; p <- 0.3; boot <- FALSE; nbt <- 2; setting <- 1; seed <- 1
m2 <- 0.3; m1 <- 0.7; s2 <- 0.2; s1 <- 0.3
my_data <- data_generation(n, N, m1, s1, m2, s2, p, misspec)
Y <- my_data[, 'Y_miss']
S <- my_data[, 'S']

# auc(Y,S)
# plot(density(S))
# lines(density(S[my_data$Y==1]), col = "red")
# lines(density(S[my_data$Y==0]), col = "blue")
# summary(S)
# summary(expit(S))
#
# plot(density(expit(S)))
# lines(density(expit(S)[my_data$Y==1]), col = "red")
# lines(density(expit(S)[my_data$Y==0]), col = "blue")
#

# Labeled data.
labeled_ind <- which(!is.na(Y))
Yt <- Y[labeled_ind]
St <- S[labeled_ind]

#### Run ROC estimation. ####

# Truth.
roc.sl0 <- supervised_ROC(S, my_data[, 'Y'])
# roc.sl0$roc

# Supervised.
roc.sl <- supervised_ROC(St, Yt)

# Semi-supervised via imputation.
roc.IMP <- ss_imp_ROC(S, Y)

# Semi-supervised via EM.
roc.PARAM <- ss_param_ROC(S, Y, method = param_method )
#roc.semi.superv.EM(S, Y, Wt = NULL, fpr0 = seq(0.01, 0.99, by = 0.01))

# Semi-supervised via combined method.
roc.COM <- roc.semi.superv.COM(S, Y, Wt = NULL, roc.IMP, roc.PARAM,
                               fpr0 = seq(0.01, 0.99, by = 0.01))


#### Run ROC std error estimation. ####
if(boot == TRUE){

  roc.sl.bt=roc.IMP.bt=roc.PARAM.bt=roc.COM.bt=list()
  for(ibt in 1:nbt){
    #print(ibt)
    tryCatch({
      # Need to just sample the labeled data.
      idx = sample(1:n, n, replace = T)
      Yt.bt = Yt[idx]
      St.bt = St[idx]
      Y.bt = c(Yt.bt, Yv)
      S.bt = c(St.bt, Sv)

      roc.sl.bt[[ibt]] = supervised_ROC(St.bt, Yt.bt, Wt = weights,
                                        interp_metric = my_interp_metric)

      roc.IMP.bt[[ibt]]=ss_imp_ROC(S.bt, Y.bt, Wt = NULL,
                                   fpr0 = seq(0.01, 0.99, by = 0.01),
                                   bw = NULL,
                                   interp_metric = my_interp_metric)

      roc.PARAM.bt[[ibt]]=ss_param_ROC(S.bt, Y.bt, Wt = NULL,
                                       fpr0 = seq(0.01, 0.99, by = 0.01),
                                       interp_metric = my_interp_metric)

      roc.COM.bt[[ibt]]=ss_com_ROC(S.bt, Y.bt, roc.IMP.bt[[ibt]],
                                   roc.PARAM.bt[[ibt]],
                                   fpr0 = seq(0.01, 0.99, by = 0.01),
                                   interp_metric = my_interp_metric)

      roc.IMP.bt[[ibt]]=roc.IMP.bt[[ibt]][setdiff(ls(roc.IMP.bt[[ibt]]), "mhat")]
      roc.PARAM.bt[[ibt]]=roc.PARAM.bt[[ibt]][setdiff(ls(roc.PARAM.bt[[ibt]]), "Yhat")]

    },
    error=function(e) NA)
  }

}else{

  roc.sl.bt=roc.IMP.bt=roc.PARAM.bt=roc.COM.bt=list()
  for(ibt in 1:nbt){
    tryCatch({

      # Perturbation weight.
      ptb_wgt <- 4*rbeta(length(St), 1/2, 3/2)
      ptb_wgt_unlabeled <- 4*rbeta(sum(is.na(my_data[, 'Y_miss'] )),
                                   1/2, 3/2)

      # Perturbed estimates.
      roc.sl.bt[[ibt]] <- supervised_ROC(St, Yt, W_labeled = ptb_wgt)

      roc.IMP.bt[[ibt]] <- ss_imp_ROC(S, Y, W_labeled = ptb_wgt,
                                      W_unlabeled = ptb_wgt_unlabeled)

      roc.PARAM.bt[[ibt]] <-  ss_param_ROC(S, Y, W_labeled = ptb_wgt,
                                           W_unlabeled = ptb_wgt_unlabeled,
                                           method = param_method )
      #roc.semi.superv.EM(S, Y, Wt = ptb_wgt, fpr0 = seq(0.01, 0.99, by = 0.01))

      roc.COM.bt[[ibt]] <- roc.semi.superv.COM(S, Y,  Wt = ptb_wgt,
                                               Wv =ptb_wgt_unlabeled,
                                               roc.IMP.bt[[ibt]],
                                               roc.PARAM.bt[[ibt]],
                                               fpr0 =
                                                 seq(0.01, 0.99, by = 0.01))
      #roc.semi.superv.COM(S, Y, Wt = ptb_wgt,
      # roc.IMP.bt[[ibt]],
      # roc.PARAM.bt[[ibt]],
      # fpr0 = seq(0.01, 0.99, by = 0.01))

      roc.IMP.bt[[ibt]]=roc.IMP.bt[[ibt]][setdiff(ls(roc.IMP.bt[[ibt]]), "mhat")]
      roc.PARAM.bt[[ibt]]=roc.IMPbt[[ibt]][setdiff(ls(roc.IMPbt[[ibt]]), "Yhat")]

    },
    error=function(e) NA)
  }


}

# roc.sl0$auc
# roc.sl$auc
# roc.IMP$auc
# roc.PARAM$auc
# head(roc.PARAM$roc)
# head(roc.IMP$roc)
# head(roc.sl0$roc)

save(roc.sl0, roc.sl, roc.IMP, roc.PARAM, roc.COM,
     roc.sl.bt, roc.IMP.bt, roc.PARAM.bt, roc.COM.bt,
     file=paste0("~/Desktop/results/res-N", N+n, "-nv", n, "-setting",setting, "-prev", p, "-seed", seed, ".Rdata"))


