require(deSolve)
require(ggplot2)
require(scales)

LotkaVolterra <- function(time, state, params){
  # state:
  # x1, x2
  # params
  # r[1,2], interactions A
  with(as.list(c(state, params)), {
    x1 <- max(x1, 0)
    x2 <- max(x2, 0)
    dx1dt <- x1 * (r[1] - A[1,1] * x1 - A[1,2] * x2)
    dx2dt <- x2 * (r[2] - A[2,2] * x2 - A[2,1] * x1)
    return(list(c(dx1dt, dx2dt)))
    })
  
}

plotresults <- function(out, pars, plotEQ = TRUE, plotN1 = TRUE, plotN2 = TRUE, plotDYN = TRUE){
  r <- pars$r
  A <- pars$A
  eq1 <- c(0,0)
  eq2 <- c(0, r[2]/A[2,2])
  eq3 <- c(r[1]/A[1,1], 0)
  eq4 <- c((A[2,2] * r[1] - A[1,2] * r[2]) / det(A),
           (A[1,1] * r[2] - A[2,1] * r[1]) / det(A)
           )
  dfeq <- as.data.frame(rbind(eq1, eq2, eq3, eq4))
  if (eq4[1] < 0 | eq4[2] < 0) dfeq <- as.data.frame(rbind(eq1, eq2, eq3))
  colnames(dfeq) <- c("x1", "x2")
  
  line1 <- data.frame(
    x1 = seq(0, eq3[1], length.out = 3),
    x2 = (r[1] - A[1,1] * seq(0, eq3[1], length.out = 3)) / A[1,2]
    )
  line2 <- data.frame(
    x2 = seq(0, eq2[2], length.out = 3),
    x1 = (r[2] - A[2,2] * seq(0, eq2[2], length.out = 3)) / A[2,1]
  )
  
  pl <- ggplot(data = dfeq, aes(x1, x2)) 
  if (plotEQ == TRUE){
    pl <- pl + geom_point(size = 4)
  } else {
    pl <- pl + geom_point(size = 4, colour = NA)
  }
  if (plotN1 == TRUE){
    pl <- pl + geom_line(data = line1, aes(x1, x2), linetype = "44")
  }
  if (plotN2 == TRUE){
    pl <- pl + geom_line(data = line2, aes(x1, x2), linetype = "13" )
  }
  if (plotDYN == TRUE){
    pl <- pl + geom_point(data = out, aes(x1, x2, colour =time)) + 
      scale_colour_gradient2(low = muted("blue"), mid = muted("red"),
                             high = muted("green"))
  }
  pl <- pl + theme_bw() + theme(axis.text = element_text(size = 14, face = "bold"), axis.title = element_text(size=14, face="bold")) +
    theme(legend.position = "bottom") 
  return(pl)
}
# 
# # test competition
# r <- c(0.5,0.5)
# A <- matrix(c(1.4, 2.7, 1.6, 2.1), 2, 2, byrow = TRUE)
# pars <- list(r = r, A = A)
# x0 <- c(x1 = 0.1, x2 = 0.3)
# times <- seq(0, 2000, by = 0.1)
# out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
# out[out < 0] <- 0