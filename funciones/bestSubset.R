bestSubset <- function(modelo_completo) {
  todos <- ols_step_all_possible(modelo_completo)$result
  df <- as.data.frame(todos)
  
  f_r2      <- which.max(df$rsquare)
  f_adjr    <- which.max(df$adjr)
  f_rmse    <- which.min(df$rmse)
  f_predr2  <- which.max(df$predrsq)
  f_cp      <- which.min(df$cp)
  f_aic     <- which.min(df$aic)
  f_sbic    <- which.min(df$sbic)
  f_sbc     <- which.min(df$sbc)
  f_msep    <- which.min(df$msep)
  f_fpe     <- which.min(df$fpe)
  f_apc     <- which.min(df$apc)
  f_hsp     <- which.min(df$hsp)
  
  criterios <- list(
    rsquare = df$predictors[f_r2],
    adjr    = df$predictors[f_adjr],
    rmse    = df$predictors[f_rmse],
    predrsq = df$predictors[f_predr2],
    cp      = df$predictors[f_cp],
    aic     = df$predictors[f_aic],
    sbic    = df$predictors[f_sbic],
    sbc     = df$predictors[f_sbc],
    msep    = df$predictors[f_msep],
    fpe     = df$predictors[f_fpe],
    apc     = df$predictors[f_apc],
    hsp     = df$predictors[f_hsp]
  )
  
  modelos_ganadores <- unlist(criterios)
  conteos <- table(modelos_ganadores)
  string_modelo_ganador <- names(conteos)[which.max(conteos)]
  
  metricas_cumplidas <- names(criterios)[which(modelos_ganadores == string_modelo_ganador)]
  
  cat("\n=========================================================\n")
  cat("  EL MEJOR MODELO POR MAYORÍA DE CRITERIOS (12 MÉTRICAS)  \n")
  cat("=========================================================\n")
  cat("Variables:", string_modelo_ganador, "\n")
  cat("Total de métricas cumplidas:", length(metricas_cumplidas), "de 12\n")
  cat("Criterios que lo eligieron:", paste(metricas_cumplidas, collapse = ", "), "\n")
  cat("=========================================================\n\n")
  
  variable_y <- as.character(formula(modelo_completo)[2])
  predictores_formateados <- gsub(" ", " + ", string_modelo_ganador)
  formula_final <- as.formula(paste(variable_y, "~", predictores_formateados))
  
  modelo_lm_final <- update(modelo_completo, formula = formula_final)
  
  modelo_lm_final$rsquare <- df$rsquare[f_r2]
  modelo_lm_final$adjr    <- df$adjr[f_adjr]
  modelo_lm_final$rmse    <- df$rmse[f_rmse]
  modelo_lm_final$predrsq <- df$predrsq[f_predr2]
  modelo_lm_final$cp      <- df$cp[f_cp]
  modelo_lm_final$aic     <- df$aic[f_aic]
  modelo_lm_final$sbic    <- df$sbic[f_sbic]
  modelo_lm_final$sbc     <- df$sbc[f_sbc]
  modelo_lm_final$msep    <- df$msep[f_msep]
  modelo_lm_final$fpe     <- df$fpe[f_fpe]
  modelo_lm_final$apc     <- df$apc[f_apc]
  modelo_lm_final$hsp     <- df$hsp[f_hsp]
  modelo_lm_final$metricas_ganadas <- metricas_cumplidas
  
  class(modelo_lm_final) <- c("mejor_lm", class(modelo_lm_final))
  
  return(modelo_lm_final)
}