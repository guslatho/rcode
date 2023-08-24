#############################
# Welcome to SnakeNom v.01! #
#############################

# SnakeNom is a self-running snake game program. After activating, a snake
# will spawn and automatically move towards an apple before repeating the process.
# This function was mostly created as a curiosity, to see if a game-like
# program could be implemented using R's native libraries.
#
# To run SnakeNom, the Beepr sound library is required.
# It can be installed by using the following syntax:
#
# install.packages("beepr")
# library(beepr)
# 
# To run SnakeNom, type SnakeNom(gamespeed). Default gamespeed is 2, 
# any value from 1-4 should work depending on preference.
#
# Given drawing speed is slow, flashing will probably occur. Better results
# may be produced given different plotting software or perhaps different code
# approach but I opted for the current one since this was mostly made for fun anway.
#
# Feedback or comments can be written to guslathouwers@Gmail.com


################
# Program Code #
################

#~~~~~~~~~~~#
# Functions #
#~~~~~~~~~~~#

#Food and the snake are encoded as a 2 column matrix containing values 
#relating to snake on the grid (1-6 for both x and y)

#First function sets up game space, creates empty plot, adds lines
createSpace <- function(openingText=FALSE,scoreCounter=0) {
	
	#Create an empty plot to draw the game on
	plot(c(-2, 27), c(-4, 30), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n', main = "Score")

	#Draw fancy lines around the game area
	lines(c(-3,25),c(-5,-5),lwd=4)
	lines(c(-3,25),c(30,30),lwd=4)
	lines(c(-3,-3),c(-5,30),lwd=4)
	lines(c(25,25),c(-5,30),lwd=4)

	#For the opening text, display message
	if(openingText==TRUE){
		text(x=11,y=20,labels='Welcome to SnakeNom v0.1!\nPress [enter] to start.',cex=2)
		beep(sound=10)
	} else {
		legend("topright",legend=c("Score",scoreCounter),cex=0.63)
	}
}

#Function for drawing the player space
drawSpace<- function(snake, food, scoreCounter){
	
	#Create empty plot first
	createSpace(openingText=FALSE,scoreCounter)

	#Create quick function for drawing individual characters
	drawChar <- function (xChar,yChar,type='#'){
		xPos <- switch(xChar,1,5,9,13,17,21)
		yPos <- switch(yChar,25,20,15,10,5,0)
		text(x=xPos,y=yPos,labels=type,cex=6)
	}

	#Creating a temporary grid that will be used to output the final player area
	tempGrid<-matrix(rep('#',times=36),ncol=6)
	for(s in 1:length(snake[,1])){
		tempGrid[snake[s,1],snake[s,2]]<-'o'
	}
	tempGrid[snake[1,1],snake[1,2]]<-'0'	#Overwrite head of the snake with '0' character

	tempGrid[food[1,1],food[1,2]]<-'.'		#Write food character to the grid

	#For every character in the grid, draw appropriate symbol
	for(x in 1:6){
		drawChar(x,1,type=tempGrid[x,1])
		drawChar(x,2,type=tempGrid[x,2])
		drawChar(x,3,type=tempGrid[x,3])
		drawChar(x,4,type=tempGrid[x,4])
		drawChar(x,5,type=tempGrid[x,5])
		drawChar(x,6,type=tempGrid[x,6])
	}
}

#Create a quick starting position for the snake by generating random x/y position which returns as matrix
startPositionSnake <- function(){
	startX<-sample(1:6)[1]
	startY<-sample(1:6)[1]
	returnd<-matrix(c(startX,startY),ncol=2)
	return(returnd)
}

#Generate food in a new position that is not occupied by the snake
generateFood <- function(snake){
	foodPossible<-matrix(c(rep(c(1,2,3,4,5,6),times=6),rep(c(1,2,3,4,5,6),each=6)),ncol=2)
	for(s in 1:length(snake[,1])){
		removeInteger<-which(foodPossible[,1]==snake[s,1]&foodPossible[,2]==snake[s,2])
		foodPossible<-foodPossible[-removeInteger,]
	}	
	food<-foodPossible[sample(length(foodPossible[,1]))[1],]
	return(matrix(food,ncol=2))
}

#Function for retrieving relative position of the food, output as 4-value vector
foodPosition2 <- function (snake,food){
	relativeXY<-food-snake[1,]
	foodDir<-c(1,1,1,1) #foodDirection, order is LEFT,UP,DOWN,RIGHT. 1 Means in that direction is present food, 0 means no food there
	if(relativeXY[1]>0){foodDir[1]<-0;foodDir[4]<-1;}
	if(relativeXY[1]<0){foodDir[1]<-1;foodDir[4]<-0;}
	if(relativeXY[1]==0){foodDir[1]<-0;foodDir[4]<-0;}
	if(relativeXY[2]>0){foodDir[2]<-0;foodDir[3]<-1;}
	if(relativeXY[2]<0){foodDir[2]<-1;foodDir[3]<-0;}
	if(relativeXY[2]==0){foodDir[2]<-0;foodDir[3]<-0;}
	return(foodDir)
}

#Test to see if directions around the snake are free to move to (i.e. not inhabited by other part of the snake)
directionFree2 <- function(snake){
	#Look at whether the spaces left, up, down, or right to the snake are occupied by another part of the snake.
	#Also look if there is a wall in any direction of the snake's current position.
	posFree<-c(1,1,1,1) #order is LEFT,UP,DOWN,RIGHT
	if( (any((snake[1,1]+1)==snake[,1]&snake[1,2]==snake[,2])) |(snake[1,1]+1==7) ){posFree[4]<-0}
	if( (any((snake[1,1]-1)==snake[,1]&snake[1,2]==snake[,2])) |(snake[1,1]-1==0) ){posFree[1]<-0}
	if( (any(snake[1,1]==snake[,1]&(snake[1,2]-1)==snake[,2])) |(snake[1,2]-1==0) ){posFree[2]<-0}	
	if( (any(snake[1,1]==snake[,1]&(snake[1,2]+1)==snake[,2])) |(snake[1,2]+1==7) ){posFree[3]<-0}
	return(posFree)
}	

#Function to evaluate which direction the snake will move
snakeAi <- function(foodElig, directionElig){
	#Multiply matrixes to see which direction is both a food present plus snake not occupying
	moveOptions<-foodElig*directionElig
	#If there are any options...
	if(sum(moveOptions>0)){
		#Chose random option from those available. "rep" added since sample automatically produces 1:x instead of just single digit in case of 1 character
		optionChosen<-sample(rep(which(moveOptions==1),times=2))[1]
	#If there are no viable options, but there are misc. movement options pick one of those
	} else if(sum(directionElig)>0){
		optionChosen<-sample(rep(which(directionElig==1),times=2))[1]		
	#If there are no options whatsoever, return the value for ending the game
	} else {
		optionChosen<-5
	}
	direction<-switch(optionChosen,"L","U","D","R","X")
	return(direction)
}	

#Function for allowing the snake to move through transforming its data matrix
snakeMove <- function(direction,snake,lengthen){

	#For the direction chosen, create new position for the head, add to the matrix
	if(direction=="L"){snake<-rbind(c(snake[1,1]-1,snake[1,2]),snake)}
	if(direction=="R"){snake<-rbind(c(snake[1,1]+1,snake[1,2]),snake)}
	if(direction=="U"){snake<-rbind(c(snake[1,1],snake[1,2]-1),snake)}
	if(direction=="D"){snake<-rbind(c(snake[1,1],snake[1,2]+1),snake)}

	#Remove the tail of the snake if it does not need to grow
	if(lengthen==FALSE){snake<-snake[-length(snake[,1]),]}

	#If it's only the head, transform back into matrix
	if(class(snake)[1]=="numeric"){snake<-matrix(snake,ncol=2)}	
	
	return(snake)
}

#Check to see if the food has been eaten by the snake after the last move
eatenCheck <- function(snake, food){
	if(snake[1,1]==food[1,1]&&snake[1,2]==food[1,2]){return(TRUE)}
	return(FALSE)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Main Function for running the Game #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Function for running the actual game
snakeNom <- function(speed=3){

	#Define 2 variables, one a counter for score, one that gets toggled after snake eats an apple
	scoreCounter<-0
	snakeGrow<-F		

	#Create a gamespace
	createSpace(openingText=TRUE)
	readline(prompt="Welcome to SnakeNom v0.1! Press [enter] to start.")

	#Set starting position for snake and apple
	snake<-startPositionSnake()
	food<-generateFood(snake)
	drawSpace(snake,food,scoreCounter)

	#Main game loop
	while(TRUE){

		#Foodposition returns the relative position in which food is present,
		#namely either below, below and left, left, left and above, above, above and right, right, and right and below.
		#Subsequently, directionFree looks at in which direction movement is unhindered,
		#also returned as a 4-value vector (1 signifies movement option, 0 signifies not possible)
		foodElig<-foodPosition2(snake, food)
		directionElig<-directionFree2(snake)
		directionChosen<-snakeAi(foodElig,directionElig)

		#Check at whether snake is out of movement options. If so, if function below is triggered and game ends
		if(directionChosen=="X"){
			gameDone<-paste("Snake is out of options! Game is finished, snake ate ",scoreCounter," apples!")
			return(gameDone)
		}

		#Snake can move, function that changes snake position gets activated. Also grows the snake if an apple was just eaten
		snake<-snakeMove(directionChosen, snake, lengthen=snakeGrow)
		snakeGrow=F
		if(eatenCheck(snake,food)==T){
			food<-generateFood(snake)
			beep(sound=10)
			snakeGrow<-T
			scoreCounter<-scoreCounter+1
		}

		#Draws the board, then sleeps
		drawSpace(snake,food, scoreCounter)
		Sys.sleep(speed)
	}
}