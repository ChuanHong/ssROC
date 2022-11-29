pertubation=function(nbt, S_labeled,Y_labeled, S, Y, method){
res.bt=NULL
for(ibt in 1:nbt){
ptb_wgt <- 4*rbeta(length(S_labeled), 1/2, 3/2)
ptb_wgt_unlabeled <- 4*rbeta(sum(is.na(Y)),
                             1/2, 3/2)

if(method=="supervised"){res.bt[[ibt]] <- tryCatch(supervised(S_labeled,Y_labeled, W_labeled = ptb_wgt), error=function(e) NA)
}
if(method=="ssROC"){res.bt[[ibt]] <- tryCatch(ssROC(S, Y, W_labeled = ptb_wgt,
                                W_unlabeled = ptb_wgt_unlabeled),error=function(e) NA)

res.bt[[ibt]]=tryCatch(res.bt[[ibt]][setdiff(ls(res.bt[[ibt]]), "mhat")],error=function(e) NA)
}
}
res.bt
}
