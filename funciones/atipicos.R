atipicos <- function(modelo) {
  p <- sum(hatvalues(modelo))
  n <- nrow(modelo$model)
  
  criterios <- list(
    leverage      = abs(hatvalues(modelo)) > 2*p/n,
    rstandard     = abs(rstandard(modelo)) > 2,
    rstudent      = abs(rstudent(modelo)) > 2,
    cooks         = cooks.distance(modelo) > qf(0.5, p, n - p),
    dffits        = abs(dffits(modelo)) > 2*sqrt(p/n),
    dfbetas       = apply(abs(dfbetas(modelo)[,-1]) > 2/sqrt(n), 1, sum) > 2,
    covratio      = covratio(modelo) > 1 + 3*p/n | covratio(modelo) < 1 - 3*p/n
  )
  
  # Convertir lista lógica a matriz
  M <- do.call(cbind, criterios)
  colnames(M) <- names(criterios)
  
  # Contar cuántos criterios cumple cada observación
  conteo <- rowSums(M)
  
  # Filtrar solo observaciones que cumplen ≥1 criterio
  idx <- which(conteo > 0)
  
  # Crear tabla final
  resultado <- data.frame(
    observacion = idx,
    criterios_cumplidos = conteo[idx],
    M[idx, , drop = FALSE]
  )
  
  # Ordenar por número de criterios cumplidos
  resultado[order(-resultado$criterios_cumplidos), ]
}
