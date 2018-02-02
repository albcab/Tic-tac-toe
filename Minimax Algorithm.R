## Minimax Algorithm ##

minimax <- function(juego, player) {
  free <- which(juego == 0)
  if(length(free) == 1) {
    juego[free] <- player
    return(list(move = free, U = ganador(juego, player)))
  }
  # if(length(free) == 9){
  #   ran <- sample(c(1, 3, 7, 9), 1)
  #   juego[ran] <- player
  #   return(list(move = ran, U = ganador(juego, player)))
  # }
  poss.results <- rep(0, 9)
  for(i in free) {
    game <- juego
    game[i] <- player
    poss.results[i] <- ganador(game, player)
  }
  mm <- ifelse(player == -1, "which.min", "which.max")
  if(any(poss.results == (player*10))) {
    move <- do.call(mm, list(poss.results))
    return(list(move = move, U = poss.results[move]))
  }
  for(i in free) {
    game <- juego
    game[i] <- player
    poss.results[i] <- minimax(game, -player)$U
  }
  random <- runif(9, 0, 0.1)
  poss.results[-free] <- 100 * -player
  poss.results <- poss.results + (player * random)
  move <- do.call(mm, list(poss.results))
  return(list(move = move, U = poss.results[move]))
}

machine.play <- function(juego, player) {
  free <- which(juego == 0)
  look.fwd <- matrix(ncol = 9, nrow = 10, data = 0)
  for(i in free) {
    juego1.5 <- juego
    juego1.5[i] <- player
    look.fwd[1, i] <- ganador2(juego1.5, player)
    free.fwd <- which(juego1.5 == 0)
    for(k in free.fwd) {
      juego2 <- juego1.5
      juego2[k] <- -player
      look.fwd[(k+1), i] <- ganador2(juego2, -player)
    }
  }
  if(!any(abs(look.fwd[1,]) == 9)) {
    rev.maxmin <- ifelse(player == -1, "max", "min")
    best.opp <- apply(look.fwd[-1,], 1, rev.maxmin)
    look.fwd[1,] <- look.fwd[1,] * -player * best.opp
  }
  num.wins <- c(3,2,3,2,4,2,3,2,3)
  # juego.fix <- juego
  # juego.fix[5] <- 0
  # game <- matrix(juego.fix, nrow = 3, byrow = T)
  # hor.ver <- c(rowSums(abs(game))[2], colSums(abs(game))[2])
  # diag <- c(sum(diag(game)), sum(diag(apply(game, 1, rev))))
  # if(2 %in% abs(diag) & 0 == sum(hor.ver))
  #   num.wins <- c(0,3,0,3,4,3,0,3,0)
  board <- sapply(num.wins, function(x) runif(1, 0.1*x, 0.1*x+0.1))
  look.fwd[1, free] <- look.fwd[1, free] + player*board[free]
  # if(2 %in% abs(diag) & 0 == sum(hor.ver))
  #   look.fwd[1,free] <- player*board[free]
  maxmin <- ifelse(player == -1, "which.min", "which.max")
  move <- do.call(maxmin, list(look.fwd[1,]))
  return(move)
}
