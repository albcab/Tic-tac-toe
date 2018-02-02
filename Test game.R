## Test game ##

tic.tac.toe <- function(player1 = "human", player2 = "computer", plays = 1) {
  if(!player1 %in% c("human", "computer") | !player2 %in% c("human", "computer"))
    stop("Players can only be humans or computers")
  for(i in 1:plays) {
    juego <- rep(0, 9)
    vic <- 0
    jugar(juego)
    player <- -1
    while(any(juego == 0) & abs(vic) < 9) {
      if(player1 == "human" & player == -1) 
        play <- human.play(juego)
      if(player1 == "computer" & player == -1)
        play <- minimax(juego, player)$move
      if(player2 == "human" & player == 1)
        play <- human.play(juego)
      if(player2 == "computer" & player == 1)
        play <- minimax(juego, player)$move
      juego[play] <- player
      jugar(juego)
      vic <- ganador(juego, player)
      player <- -player
    }
    if (vic == -10) text(15, 15, "PLAYER1 WINS", col = "red", cex = 3)
    if (vic == 10) text(15, 15, "PLAYER2 WINS", col = "red", cex = 3)
    if (vic < 10 & vic > -10) text(15, 15, "It's a DRAW", col = "red", cex = 4)
    locator(1)
  }
}

tic.tac.toe("human", "computer")