#------------------------------------------------------------------------------#
# Compare different models
#------------------------------------------------------------------------------#

rm(list=ls())

library("loo")

models<-("RWsimple")


for (model in models){
  modname<-paste0("param_rec_", model,".Rdata")
  load(paste0("output_files/",modname))
  
  assign(model, dat)
}

# # compute log like]
# for (model in models){
# assign(paste0("log_lik_", model), extract_log_lik(get(model)$model))
# assign(paste0("loo_", model), loo(get(model)$))
# }

model<-models[1]

log_lik_l<-extract_log_lik(get(model)$model)
loo<-loo(log_lik_l)

print(loo)
