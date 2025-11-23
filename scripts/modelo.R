##########        MODELO         ##########
SEIAQRV <- function (t, estado, parametros) {
  with (as.list (c(estado, parametros)) , {
    dS <- 
    dE <- 
    dI <- 
    dA <-
    dQ <- 
    dR <- 
    dV <- 
    list(c(dS, dE, dI, dA, dQ, dR, dV))
  })
}


pars <- c ( )
condiciones <- c (S= 100, E=2, I=5, A=1 , Q=0, R = , V =)
tiempo <- seq (0, 100, by = 0.05)
out <- ode(condiciones, tiempo, SEIAQRV, pars) 

matplot(out [ , 1], out [ , 2:6], type = "l", xlab = "tiempo", ylab = "Población", 
        main = "SEIAQRV", lwd = 2)
legend ("topright", c("Susceptibles", "Expuestos", "I_Graves", "I_Leves", "Cuarentena", "Recuperados"), 
        col = 1:4, lty = 1:3, cex= 0.5)


