
# Simulación de tres en linea ---------------------------------------------

## Creamos dos funciones, una que crea el panel del juego
## la otra define el ganador del juego.
## Finalmente hacemos un loop de juegos al azar

## Main Setup ##

jugar <- function(juego) { # un juego siempre tiene 9 jugadas
  # Primera parte dibuja el tablero y juegadas
  xo <- c("X", " ", "O")
  par(mar = rep(0.3, 4))
  plot.new()
  plot.window(xlim = c(0, 30), ylim = c(0,30))
  abline(v = c(10, 20), h = c(10,20), col = "black", lwd = 4)
  game <- xo[juego + 2]
  text(rep(c(5, 15, 25), 3), c(rep(25, 3), rep(15, 3), rep(5, 3)), game, 
       cex = 5, col = "darkgreen")
  # Segunda parte identifica ganador y traza linea
  game <- matrix(juego, nrow = 3, byrow = T)
  hor <- abs(rowSums(game))
  if (any(hor == 3))
    hor <- (4-which(hor == 3))*10 - 5
  else
    hor <- 0
  ver <- abs(colSums(game))
  if (any(ver == 3))
    ver <- which(ver == 3)*10 - 5
  else
    ver <- 0
  diag1 <- abs(sum(diag(game)))
  diag2 <- abs(sum(diag(apply(game, 1, rev))))
  if (hor > 0) lines(c(0, 30), rep(hor, 2), col = "red", lwd = 8)
  if (ver > 0) lines(rep(ver, 2), c(0, 30), col = "red", lwd = 8)
  if (diag1 == 3) lines(c(0, 30), c(30, 0), col = "red", lwd = 8)
  if (diag2 == 3) lines(c(30, 0), c(30, 0), col = "red", lwd = 8)
}

ganador <- function(juego, player) {
  game <- matrix(juego, nrow = 3, byrow = T)
  hor <- rowSums(game)
  ver <- colSums(game)
  diag <- c(sum(diag(game)), sum(diag(apply(game, 1, rev))))
  if (-3 %in% c(hor, ver, diag))
    return(-10)
  if (3 %in% c(hor, ver, diag))
    return(10)
  else
    return(0)
  # opt <- c(hor, ver, diag)
  # minmax <- ifelse(player == -1, "min", "max")
  # whichminmax <- ifelse(player == -1, "which.min", "which.max")
  # best <- do.call(minmax, list(opt))
  # which.best <- do.call(whichminmax, list(opt))
  # if(best %in% opt[-which.best]) best <- abs(best)*best
  # if(abs(best) == 3) best <- best * 3
  # return(best)
}

human.play <- function(juego) {
  #text(15, 30, "Elije tu jugada", col = "salmon")
  empty <- which(juego == 0)
  move <- 0
  while (!move %in% empty) {
    coord <- locator(1)
    x <- floor(abs(coord$x)/10) + 1
    y <- floor(abs(coord$y)/10) + 1
    move <- x + 3 * (3-y)
    if(!move %in% empty) print("No puedes jugar donde ya han jugado")
  }
  return(move)
}
