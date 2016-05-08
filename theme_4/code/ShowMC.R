BuildMC <- function(p, q, ep, k){
  p <- (1- ep) * p + ep * (1 - p)
  q <- (1- ep) * q + ep * (1 - q)
  M <- matrix(0, 4, 4)
  M[1,] <- c(p[1] * q[1], p[1] * (1 - q[1]), (1 - p[1]) * q[1], (1 - p[1]) * (1 - q[1]))
  M[2,] <- c(p[2] * q[3], p[2] * (1 - q[3]), (1 - p[2]) * q[3], (1 - p[2]) * (1 - q[3]))
  M[3,] <- c(p[3] * q[2], p[3] * (1 - q[2]), (1 - p[3]) * q[2], (1 - p[3]) * (1 - q[2]))
  M[4,] <- c(p[4] * q[4], p[4] * (1 - q[4]), (1 - p[4]) * q[4], (1 - p[4]) * (1 - q[4]))
  return(M)
}

BuildMCFromStrategy <- function(NameP, NameQ, ep, k){
  if (NameP == "ALLD") p <- c(0,0,0,0)
  if (NameQ == "ALLD") q <- c(0,0,0,0)
  if (NameP == "ALLC") p <- c(1,1,1,1)
  if (NameQ == "ALLC") q <- c(1,1,1,1)
  if (NameP == "TFT") p <- c(1,0,1,0)
  if (NameQ == "TFT") q <- c(1,0,1,0)
  if (NameP == "GTFT") p <- c(1,1-k,1,1-k)
  if (NameQ == "GTFT") q <- c(1,1-k,1,1-k)
  if (NameP == "GRIM") p <- c(1,0,0,0)
  if (NameQ == "GRIM") q <- c(1,0,0,0)
  if (NameP == "WSLS") p <- c(1,0,0,1)
  if (NameQ == "WSLS") q <- c(1,0,0,1)
  if (NameP == "RND") p <- c(0.5,0.5,0.5,0.5)
  if (NameQ == "RND") q <- c(0.5,0.5,0.5,0.5)
  return(BuildMC(p, q, ep, C))
}

ShowSimulation <- function(NameP, NameQ, ep, k, initial = c(0,1,0,0)){
  M <- BuildMCFromStrategy(NameP, NameQ, ep, k)
  colnames(M) <- c("CC", "CD", "DC", "DD")
  rownames(M) <- colnames(M)
  state <- (initial)
  results <- data.frame()
  for (t in 1:50){
    for (i in 1:4){
      results <- rbind(results, data.frame(t = t, State = colnames(M)[i], value = state[i]))
    }
    state <- state %*% M
  }
  pl <- ggplot(results, aes(x = t, y = value, colour = State)) +  scale_y_continuous("Probability") + 
    geom_line(size = 2, alpha = 0.8) + theme_bw() + xlab("time") + 
    theme(axis.text = element_text(size = 14, face = "bold"), axis.title = element_text(size=14, face="bold")) +
    theme(legend.position = "bottom") 
  return(pl)
}
  
