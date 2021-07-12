## Dimensionality analysis (EE and IV)


# Read libraries ----------------------------------------------------------

library(Dimensionality)


# dimensionality analysis  ------------------------------------------------

EE_metric <- Dimensionality::EvennessEigen(matrix.M = as.matrix(div_metrics),
                                           scale = TRUE,
                                           method = "standardize",
                                           evenness = "Camargo")
IVs_diversity <- Dimensionality::ImportanceVal(matrix.M = as.matrix(div_metrics),
                                               scale = TRUE,
                                               method = "max",
                                               stopRule = TRUE)

IVs_diversity$IV.obs_stopRule


# by realm ----------------------------------------------------------------

reino <- metric_matrix$reino
dimensionlity_all_realms <-
  matrix(data = unlist(lapply(unique(reino),
                              function(x){
                                EE_metric_realm <- Dimensionality::EvennessEigen(matrix.M = as.matrix(div_metrics[which(metric_matrix$reino == x), ]),
                                                                                 scale = TRUE,
                                                                                 method = "standardize",
                                                                                 evenness = "Camargo")
                                IVs_diversity_realm <- Dimensionality::ImportanceVal(matrix.M = as.matrix(div_metrics[which(metric_matrix$reino == x), ]),
                                                                                     scale = TRUE,
                                                                                     method = "max",
                                                                                     stopRule = TRUE)
                                if(is.matrix(IVs_diversity_realm$IV.obs_stopRule) == TRUE){
                                  IVs_total_realm <- colSums(IVs_diversity_realm$IV.obs_stopRule)
                                } else {
                                  IVs_total_realm<- IVs_diversity_realm$IV.obs_stopRule
                                }
                                dimensionality_obs <- c(EE_metric_realm, IVs_total_realm)
                                return(dimensionality_obs)
                              }
  )
  ),
  nrow = length(unique(reino)),
  ncol = ncol(div_metrics) + 1,
  byrow = TRUE,
  dimnames = list(unique(reino), c("EE", colnames(div_metrics)
  )
  )
  )
