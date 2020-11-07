#CONNECT 4 TIME BABY

#use readline(prompt="") for user input 

## the check functions could be optimized, especially for various board sizes, but work for now

#function for checking columns
colcheck <- function(board){
  nc <- ncol(board)
  check.temp <- matrix(rep(0,3*nc),nrow=3,ncol =nc)
  for(i in 1:nc){
    #right now, if else returns 4 values, i dont want it to
    check.temp[1,i] <- ifelse((board[1,i] == 1 & board[2,i] == 1 & board[3,i] == 1 & board[4,i] == 1)| (board[1,i] == 2 & board[2,i] == 2 & board[3,i] == 2 & board[4,i] == 2), 1, 0)
    check.temp[2,i] <- ifelse((board[2,i] == 1 & board[3,i] == 1 & board[4,i] == 1 & board[5,i] == 1)| (board[2,i] == 2 & board[3,i] == 2 & board[4,i] == 2 & board[5,i] == 2), 1, 0)
    check.temp[3,i] <- ifelse((board[3,i] == 1 & board[4,i] == 1 & board[5,i] == 1 & board[6,i] == 1)| (board[3,i] == 2 & board[4,i] == 2 & board[5,i] == 2 & board[6,i] == 2), 1, 0)
  }
  win <-ifelse(length(check.temp[(check.temp == 1)]) >= 1 ,1,0)
  return(win)
  #0 means game is still going
  #1 means game over
}

#function for checking rows
rowcheck <- function(board){
  nr <- nrow(board)
  check.temp <- matrix(rep(0,4*nr), nrow=4, ncol=nr)
  for(i in 1:nr){
    check.temp[1,i] <- ifelse((board[i,1] == 1 & board[i,2] == 1 & board[i,3] == 1 & board[i,4] == 1)| (board[i,1] == 2 & board[i,2] == 2 & board[i,3] == 2 & board[i,4] == 2), 1, 0)
    check.temp[2,i] <- ifelse((board[i,2] == 1 & board[i,3] == 1 & board[i,4] == 1 & board[i,5] == 1)| (board[i,2] == 2 & board[i,3] == 2 & board[i,4] == 2 & board[i,5] == 2), 1, 0)
    check.temp[3,i] <- ifelse((board[i,3] == 1 & board[i,4] == 1 & board[i,5] == 1 & board[i,6] == 1)| (board[i,3] == 2 & board[i,4] == 2 & board[i,5] == 2 & board[i,6] == 2), 1, 0)
    check.temp[4,i] <- ifelse((board[i,4] == 1 & board[i,5] == 1 & board[i,6] == 1 & board[i,7] == 1)| (board[i,4] == 2 & board[i,5] == 2 & board[i,6] == 2 & board[i,7] == 2), 1, 0)
  }
  win <-ifelse(length(check.temp[(check.temp == 1)]) >= 1 ,1,0)
  return(win)
  #0 means no win
  #1 is win
}

#combine the two checks for ease of use
wincheck <- function(board){
  win<-ifelse(rowcheck(board)== 1 | colcheck(board) ==1,1,0)
  return(win)
  #1 is win present, 
  #0 is no win
}

c4placepiece <- function(board,column, p){
  c <- as.numeric(column)
  col <- board[,c]
  l <- length(col)
  for(i in 0:(l-1)){
    if(col[l-i] == 0){
      board[l-i,c] <- p
      break()
    }
  }
  return(board)
}

aimove <- function(board){
  #check board cols
  temp <- rep(0,ncol(board))
  p <- rep(0,ncol(board))
  for(i in 1:ncol(board)){
    temp[i] <- length(which(board[,i] ==0))
  }
  s <- sum(temp)
  p <- temp/s
  return(sample(1:7, 1, prob= p))
}

connectfour <- function(){
  #initialize board
  board <-matrix(0, nrow=6, ncol=7)
  #collect player names
  p1<-readline(prompt= "Player 1 name:")
  p2<-readline(prompt= "Player 2 name:")
  if(p2 != "AI"){
     repeat{
      #show board
      prmatrix(board, collab = c("c1","C2","C3","C4","C5","C6","C7"), rowlab = rep("",6), quote=FALSE)
      #Player 1 turn
      turn <- p1
      #intake move
      p1move <- readline(prompt= "Player 1 move:")
      #update board
      board <- c4placepiece(board, p1move,1)
    #check for a p1 win
    if(wincheck(board == 1)){
      break}
    #show new board
    prmatrix(board, collab = c("c1","C2","C3","C4","C5","C6","C7"), rowlab = rep("",6), quote=FALSE)
    #Player 2 turn
    turn <- p2
    #intake move
    p2move <- readline(prompt= "Player 2 move:")
    #update board
    board <- c4placepiece(board, p2move,2)
    #board is shown here if win, or shown upon repeat!
    if(wincheck(board == 1)){
      break}
     }
  }else{
    repeat{
      #show board
      prmatrix(board, collab = c("c1","C2","C3","C4","C5","C6","C7"), rowlab = rep("",6), quote=FALSE)
      #Player 1 turn
      turn <- p1
      #intake move
      p1move <- readline(prompt= "Player 1 move:")
      #update board
      board <- c4placepiece(board, p1move,1)
      #check for a p1 win
      if(wincheck(board == 1)){
        break}
      #show new board
      #Player 2 turn
      turn <- p2
      #intake AI move
      p2move <- aimove(board)
      #update board
      board <- c4placepiece(board, p2move,2)
      #board is shown here if win, or shown upon repeat!
      if(wincheck(board == 1)){
        break}
    }
  }
  prmatrix(board, collab = c("c1","C2","C3","C4","C5","C6","C7"), rowlab = rep("",6), quote=FALSE)
  cat(turn, "has won!")
}

#ai v ai, will add as option to normal func when both player names are AI

aifight<-function(){
  #initialize board
  board <-matrix(0, nrow=6, ncol=7)
  #collect player names
  p1<-"AI"
  p2<- "AI"
  repeat{
    #show board
    prmatrix(board, collab = c("c1","C2","C3","C4","C5","C6","C7"), rowlab = rep("",6))
    #Player 1 turn
    turn <- "AI 1"
    #intake move
    p1move <- aimove(board)
    #update board
    board <- c4placepiece(board, p1move,1)
    #check for a p1 win
      if(wincheck(board == 1)){
      break}
    #show new board
    #Player 2 turn
    turn <- "AI 2"
    #intake AI move
    p2move <- aimove(board)
    #update board
    board <- c4placepiece(board, p2move,2)
    #board is shown here if win, or shown upon repeat!
    if(wincheck(board == 1)){
      break}
    rl<-readline(prompt= "Continue? : ")
    if(rl == "Y"){
    }else(break)
  }
prmatrix(board, collab = c("c1","C2","C3","C4","C5","C6","C7"), rowlab = rep("",6))
cat(turn, "has won!")
}

#library(gt)
#gt makes nicer tables, might be worth switching

