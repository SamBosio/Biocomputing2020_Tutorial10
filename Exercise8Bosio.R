#Exercise 1

#read file
scores<-read.table("UWvMSU_1-22-13.txt")
#define empty matrices then split team scores into individual matrices
Wmatrix<-matrix(1:46, nrow = 23, ncol = 2)
Mmatrix<-matrix(1:54, nrow = 27, ncol = 2)
Wmatrix[1:23,1]<-as.numeric(scores[scores$V2=="UW",1])
Wmatrix[1:23,2]<-as.numeric(scores[scores$V2=="UW",3])
Mmatrix[1:27,1]<-as.numeric(scores[scores$V2=="MSU",1])
Mmatrix[1:27,2]<-as.numeric(scores[scores$V2=="MSU",3])
Wsmatrix<-matrix(0, nrow = 23, ncol = 2)
Msmatrix<-matrix(0, nrow = 27, ncol = 2)
#for each score
for(i in 1:nrow(Wmatrix)){
  #new line is the sum of previous lines
    Wsmatrix[i,2]<-0+sum(Wmatrix[1:i,2])
}
#define time column
Wsmatrix[,1]<-Wmatrix[,1]
for(i in 1:nrow(Mmatrix)){
  Msmatrix[i,2]<-0+sum(Mmatrix[1:i,2])
}
#define time column
Msmatrix[,1]<-Mmatrix[,1]
#define plot names
time=Wsmatrix[,1]
time2=Msmatrix[,1]
UW=Wsmatrix[,2]
MSU=Msmatrix[,2]
#plot sum lines
plot(time, UW, type='l', col="red")

lines(time2, MSU, col="green")

#Exercise 2

#Allow R to process inputs
readnumber<-function(){
  n<-readline(prompt="Enter a number between 1 and 100 ")
  #return input to computer
  return(n)
}
#initial prompt
print("I'm thinking of a number between 1 and 100 ")
print("Guess:")
#define loop variables
userin<-0
guesses<-0
#generate number
number<-sample(1:100, 1)
#while the user input is not the number
while(userin != number){
  #while they are 10 guesses or under
  while(guesses < 11){
  #input number
  userin<-readnumber()
  #if it is equal, end game and clear environment for next time
  if(userin == number){
    print("You Win!")
    rm(list = ls())
    #if it is higher, say so and prompt again, add 1 to guess count
  }else if(userin < number){
    print("Higher")
    guesses=guesses+1
    #if it is lower, say so and prompt again, add 1 to guess count
  }else if(userin > number){
   print("Lower") 
    guesses=guesses+1
  }
  #end game and clear environment for new game if guesses are over 10
  }else{
    cat("Sorry, the number was ", number)
    rm(list = ls())
  }
  }


