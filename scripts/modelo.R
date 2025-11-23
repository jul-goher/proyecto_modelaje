##########        MODELO         ##########
library(deSolve)

SEIHRV <- function (t, estado, parametros) {
  with (as.list (c(estado, parametros)) , {
    dS <- mu - beta*S - gamma*R + rho*V - mu*S
    dE <- beta*S - sigma*rho*E - sigma*E*(1 - rho) - mu*E
    dI <- sigma*(1-rho)*E - alpha*I - mu*I
    dG <- sigma*rho*E - delta*G - mu*G
    dH <- delta*G - epsilon*H - mu*H
    dR <- epsilon*H + alpha*I - gamma*R - mu*R - lambda*R
    dV <- lambda*R - rho*V - mu*V
    list(c(dS, dE, dI, dG, dH, dR, dV))
  })
}

#### DENV 1 
pars_d1 <- c ( mu =0.0000169 , beta = 0.048, gamma = 0.00063, rho = 0.4, sigma = , alpha = 0.657 , delta = , epsilon = 0.657, lambda =  )
condiciones_d1 <- c (S= 100,000, E=50, I=10, G=2, H=3, R=3 , V = 2)
tiempo_d1 <- seq (0, 100, by = 0.05)
out_d1 <- ode(condiciones_d1, tiempo_d1, SEIHRV, pars_d1) 

matplot(out [ , 1], out [ , 2:8], type = "l", xlab = "tiempo", ylab = "Población", 
        main = "Dengue Serotipo 1", lwd = 2)
legend ("topright", c("Susceptibles", "Expuestos", "I_Leves", "I_Graves", "Hospitalizados", "Recuperados", "Vacunados"), 
        col = 1:4, lty = 1:3, cex= 0.5)

##### DENV 2 

pars_d2 <- c ( mu = 0.0000169, rho = 0.4, beta = 0.056, gamma = 0.00042, sigma = , alpha = 0.552 , delta = , epsilon = 0.552, lambda =  )
condiciones_d2 <- c (S= 100,000, E=50, I=10, G=2, H=3, R=3 , V = 2)
tiempo_d2 <- seq (0, 100, by = 0.05)
out_d2 <- ode(condiciones_d2, tiempo_d2, SEIHRV, pars_d2) 

matplot(out [ , 1], out [ , 2:8], type = "l", xlab = "tiempo", ylab = "Población", 
        main = "Dengue Serotipo 2", lwd = 2)
legend ("topright", c("Susceptibles", "Expuestos", "I_Leves", "I_Graves", "Hospitalizados", "Recuperados", "Vacunados"), 
        col = 1:4, lty = 1:3, cex= 0.5)



##### DENV 3
pars_d3 <- c ( mu =0.0000169 , beta = 0.11, gamma = 0.00056, rho = 0.4, sigma = , alpha = 0.657 , delta = , epsilon = 0.882, lambda =  )
condiciones_d3 <- c (S= 100,000, E=50, I=10, G=2, H=3, R=3 , V = 2)
tiempo_d3 <- seq (0, 100, by = 0.05)
out_d3 <- ode(condiciones_d3, tiempo_d3, SEIHRV, pars_d3) 

matplot(out [ , 1], out [ , 2:8], type = "l", xlab = "tiempo", ylab = "Población", 
        main = "Dengue Serotipo 3", lwd = 2)
legend ("topright", c("Susceptibles", "Expuestos", "I_Leves", "I_Graves", "Hospitalizados", "Recuperados", "Vacunados"), 
        col = 1:4, lty = 1:3, cex= 0.5)




##### DENV 4 
pars_d4 <- c ( mu =0.0000169 , beta = 0.081, gamma = 0.00053, rho = 0.4, sigma = , alpha = 0.687 , delta = , epsilon = 0.687, lambda = 0.9 )
condiciones_d4 <- c (S= 100,000, E=50, I=10, G=2, H=3, R=3 , V = 2)
tiempo_d4 <- seq (0, 100, by = 0.05)
out_d4 <- ode(condiciones_d4, tiempo_d4, SEIHRV, pars_d4) 

matplot(out [ , 1], out [ , 2:8], type = "l", xlab = "tiempo", ylab = "Población", 
        main = "Dengue Serotipo 4", lwd = 2)
legend ("topright", c("Susceptibles", "Expuestos", "I_Leves", "I_Graves", "Hospitalizados", "Recuperados", "Vacunados"), 
        col = 1:4, lty = 1:3, cex= 0.5)








