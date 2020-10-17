#Given transaction table
T <- matrix(data = c(90, 50, 40, 200, 120, 125, 40, 500, 60, 150, 200, 550, 11, 19, 30, 0), nrow = 4, ncol = 4,byrow = TRUE)
#Technical coefficients: Matrix A
Iron = c(T[,1]/T[1,4])
Coal = c(T[,2]/T[2,4]) 
Wheat = c(T[,3]/T[3,4])
df=data.frame(Iron,Coal,Wheat) 
df[4,] 
row.names(df) <- list("Iron","Coal","Wheat","Labor Units") 



