
devtools::install_github(repo = "https://github.com/ChuanHong/ssROCtesting")
library(ssROC)
n <- 300; N <- 5000 - 300; p <- 0.3; boot <- FALSE; nbt <- 2; setting <- 1; seed <- 1
m2 <- 0.3; m1 <- 0.7; s2 <- 0.2; s1 <- 0.3
my_data <- data_generation(n, N, m1, s1, m2, s2, p, misspec=F)
Y <- my_data[, 'Y_miss']
S <- my_data[, 'S']
labeled_ind <- which(!is.na(Y))
Yt <- Y[labeled_ind]
St <- S[labeled_ind]

## point estimates
roc.sl0 <- supervised(S, my_data[, 'Y'])
roc.sl <- supervised(St, Yt)
roc.ssROC <- ssROC(S, Y)

## pertubation
roc.ssROC.pert=pertubation(nbt=200, S_labeled=St,Y_labeled=Yt, S=S, Y=S, method="ssROC")



save(roc.sl0, roc.sl, roc.IMP, roc.PARAM, roc.COM,
     roc.sl.bt, roc.IMP.bt, roc.PARAM.bt, roc.COM.bt,
     file=paste0("~/Desktop/results/res-N", N+n, "-nv", n, "-setting",setting, "-prev", p, "-seed", seed, ".Rdata"))


