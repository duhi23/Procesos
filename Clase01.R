# Simulaciòn del juego consistente en lanzar una moneda
# Enunciado: Suponga que un jugador accede al juego de lanzar 
# una moneda y por cada sello que obtenga en el lanzamiento 
# cobra una unidad monetaria, asuma que el jugador inicia el juego
# con 0 unidades monetarias.

# Simulación de una trayectoria de 6 pasos
tr1 <- cumsum(sample(c(0,1), size = 6, replace = TRUE))
plot(1:length(tr1), tr1, type = "b", col = 2, xlab = 'Número de lanzamientos', ylab = 'Capital acumulado')

# Funciòn que genera una trayectoria tras realizar n lanzamientos
try <- function(n){
  return(cumsum(sample(c(0,1), size = n, replace = TRUE))) # 0:= cara y 1:= sello
} 
# Función que genera m replicas de trayectorias
rpl <- function(m,n){
  res <- matrix(0,ncol = n, nrow = m)
  for(j in 1:m){
    res[j,] <- try(n)
  }
  # Obtención de la ley de probabilidad
  pr <- table(res[,n])/m
  return(list(Matriz=res, Vector = pr, 
              CapitalEsperado = sum((0:n)*pr))) # Valor Esperado al cabo de n lanzamientos
}

system.time(T1 <- rpl(10000000,20))
T1$Vector
T1$CapitalEsperado
