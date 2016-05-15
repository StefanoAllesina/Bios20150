# Plot four cases
source("LV.R")


A <- matrix(c(0.5, 2.5, 1.0, 4), 2, 2, byrow = TRUE)
r <- c(1, 1)
pars <- list(r = r, A = A)
x0 <- c(x1 = 0.1, x2 = 0.1)
times <- seq(0, 100, by = 0.05)
out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
out[out < 0] <- 0
pl1 <- (plotresults(out, pars, 
                    plotEQ = TRUE,
                    plotN1 = TRUE,
                    plotN2 = TRUE,
                    plotDYN = FALSE
))
show(pl1)
ggsave("../images/case1.jpg", pl1)

A <- matrix(c(2, 2.5, 1.0, 0.75), 2, 2, byrow = TRUE)
r <- c(1, 1)
pars <- list(r = r, A = A)
x0 <- c(x1 = 0.1, x2 = 0.1)
times <- seq(0, 100, by = 0.05)
out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
out[out < 0] <- 0
pl2 <- (plotresults(out, pars, 
                    plotEQ = TRUE,
                    plotN1 = TRUE,
                    plotN2 = TRUE,
                    plotDYN = FALSE
))
show(pl2)
ggsave("../images/case2.jpg", pl2)

A <- matrix(c(2, 2.5, 1.0, 4), 2, 2, byrow = TRUE)
r <- c(1, 1)
pars <- list(r = r, A = A)
x0 <- c(x1 = 0.1, x2 = 0.1)
times <- seq(0, 100, by = 0.05)
out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
out[out < 0] <- 0
pl3 <- (plotresults(out, pars, 
                 plotEQ = TRUE,
                 plotN1 = TRUE,
                 plotN2 = TRUE,
                 plotDYN = FALSE
))
show(pl3)
ggsave("../images/case3.jpg", pl3)


A <- matrix(c(0.6, 2.5, 1.0, 0.8), 2, 2, byrow = TRUE)
r <- c(1, 1)
pars <- list(r = r, A = A)
x0 <- c(x1 = 0.1, x2 = 0.1)
times <- seq(0, 100, by = 0.05)
out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
out[out < 0] <- 0
pl4 <- (plotresults(out, pars, 
                    plotEQ = TRUE,
                    plotN1 = TRUE,
                    plotN2 = TRUE,
                    plotDYN = FALSE
))
show(pl4)
ggsave("../images/case4.jpg", pl4)


