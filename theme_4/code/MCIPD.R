library("ggplot2")

GetPayoffMC <- function(p, q, ep, C){
  p <- (1- ep) * p + ep * (1 - p)
  q <- (1- ep) * q + ep * (1 - q)
  M <- matrix(0, 4, 4)
  M[1,] <- c(p[1] * q[1], p[1] * (1 - q[1]), (1 - p[1]) * q[1], (1 - p[1]) * (1 - q[1]))
  M[2,] <- c(p[2] * q[3], p[2] * (1 - q[3]), (1 - p[2]) * q[3], (1 - p[2]) * (1 - q[3]))
  M[3,] <- c(p[3] * q[2], p[3] * (1 - q[2]), (1 - p[3]) * q[2], (1 - p[3]) * (1 - q[2]))
  M[4,] <- c(p[4] * q[4], p[4] * (1 - q[4]), (1 - p[4]) * q[4], (1 - p[4]) * (1 - q[4]))
  v <- Re(eigen(t(M))$vectors[,1])
  v <- v / sum(v)
  h1 <- c(1, 0, 1 + C, C)
  h2 <- c(1, 1 + C, 0, C)
  P1 <- v %*% h1
  P2 <- v %*% h2
  return(c(P1, P2))
}

PlotPayOffs <- function(NameP, NameQ, ep, C){
  if (NameP == "ALLD") p <- c(0,0,0,0)
  if (NameQ == "ALLD") q <- c(0,0,0,0)
  if (NameP == "ALLC") p <- c(1,1,1,1)
  if (NameQ == "ALLC") q <- c(1,1,1,1)
  if (NameP == "TFT") p <- c(1,0,1,0)
  if (NameQ == "TFT") q <- c(1,0,1,0)
  if (NameP == "GTFT") p <- c(1,1-C,1,1-C)
  if (NameQ == "GTFT") q <- c(1,1-C,1,1-C)
  if (NameP == "GRIM") p <- c(1,0,0,0)
  if (NameQ == "GRIM") q <- c(1,0,0,0)
  if (NameP == "WSLS") p <- c(1,0,0,1)
  if (NameQ == "WSLS") q <- c(1,0,0,1)
  if (NameP == "RND") p <- c(0.5,0.5,0.5,0.5)
  if (NameQ == "RND") q <- c(0.5,0.5,0.5,0.5)
  results <- data.frame()
  # P1 vs P1
  tmp <- GetPayoffMC(p, p, ep, C)
  results <- rbind(results, data.frame(P1 = NameP, P2 = NameP, Payoff = 1, Value = tmp[1]))
  results <- rbind(results, data.frame(P1 = NameP, P2 = NameP, Payoff = 2, Value = tmp[2]))
  # P1 vs P2
  tmp <- GetPayoffMC(p, q, ep, C)
  results <- rbind(results, data.frame(P1 = NameP, P2 = NameQ, Payoff = 1, Value = tmp[1]))
  results <- rbind(results, data.frame(P1 = NameP, P2 = NameQ, Payoff = 2, Value = tmp[2]))
  # P2 vs P2
  tmp <- GetPayoffMC(q, q, ep, C)
  results <- rbind(results, data.frame(P1 = NameQ, P2 = NameQ, Payoff = 1, Value = tmp[1]))
  results <- rbind(results, data.frame(P1 = NameQ, P2 = NameQ, Payoff = 2, Value = tmp[2]))
  pl <- ggplot(results, 
               aes(x = paste(P1, "vs.", P2), 
                   y = Value, 
                   fill = as.factor(Payoff))) +
    geom_bar(stat = "identity", position = "dodge") + theme_bw() + xlab("") + ylab("Average Payoff") + 
    theme(legend.position = "none", axis.text = element_text(size = 14, face = "bold"), axis.title = element_text(size=14, face="bold"))
  
  return(pl)
}
