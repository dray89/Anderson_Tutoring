library(RConics)

#Given transaction table
T <- matrix(data = c(90, 50, 40, 200, 120, 125, 40, 500, 60, 150, 200, 550, 11, 19, 30, 0), nrow = 4, ncol = 4,byrow = TRUE)
#Technical coefficients: Matrix A

Iron = c(T[,1]/T[1,4])
Coal = c(T[,2]/T[2,4]) 
Wheat = c(T[,3]/T[3,4])

df=data.frame(Iron,Coal,Wheat) 
df[4,] 
row.names(df) <- list("Iron","Coal","Wheat","Labor Units") 
I <- diag(c(1,1,1))

A = df[-c(4),-c(4)]
A = df[c(1:3),c(1:3)]
I_A = I - A

IA_det = det(I_A_matrix)
I_A_matrix = data.matrix(I_A)
adjoint_T = adjoint(I_A_matrix)
leontief = adjoint_T/IA_det

vectorI_df = df[c(4),]
vectorI_matrix = data.matrix(vectorI_df)
vectorI = as.vector(vectorI_matrix)
embodied_labor = vectorI*leontief

A_rate = A*1.1
I_Arate = I - A_rate

IArate_matrix = data.matrix(I_Arate)
adjoint_IArate = adjoint(IArate_matrix)

IA_det = det(IArate_matrix)

general_profit_rate = adjoint_IArate/IA_det
gpr_matrix = data.matrix(general_profit_rate)

interim_calculations = vectorI_matrix%*%gpr_matrix
input_sums = rowSums(T[,-c(4)])
new_matrix = cbind(T, input_sums)  
surplus = new_matrix[,4] - new_matrix[,5]
surplus[4] = 0
new_matrix = cbind(new_matrix, surplus) 
denominator = interim_calculations%*%surplus[1:3]
wage = 1/denominator
vector_p = wage%*%interim_calculations
p_r = vector_p%*%surplus[1:3]

unit_price_of_wheat = vector_p[3]
gross_wheat_output = T[3,4]
gross_revenue = unit_price_of_wheat*gross_wheat_output
wheat_inputs = T[,3]
input_costs = wheat_inputs[1:3]*vector_p
cost_of_production = sum(input_costs)
profit = gross_revenue - cost_of_production

#at 10% general rate of profit, we assume this is the return to capital 
return_to_capital = .1*profit
return_to_labor = wheat_inputs[4]*wage
solow_residual = profit - return_to_capital - return_to_labor
