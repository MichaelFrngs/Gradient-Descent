#Initial Arbitrary guess
x=0

#Function
OriginalFunction = function(x){x**3 - 2 * x +2} 
#Derivative of function
d.OriginalFunction = function(x){3*x**2-2} 


i=0 #iterations
while(i<30){x= x-OriginalFunction(x)/d.OriginalFunction(x)
i=i+1}

#At this point, y = 0. This will let you find the minimum of cost(error) equations.
print(x)

#This method doesn't always work, especially if there are local minima.




#3d function - Gradient Decent

#Initial Arbitrary guess
x=1000
y=2000
DescentStrength = 0.0001 #Arbitrary number. Too large will overshoot and never find the solution. Too little and you need too much computer power.

#Function
OriginalFunction = function(x,y){x**2+y**2}
dfdx = function(x,y){2*x*y**2} 
dfdy = function(x){x**2+2*y} 

#Derivative of function
Jacobian = function(x,y){
  c(dfdx(x,y),
    dfdy(y)
    )
} 
MaxIterations = 100000 #Adjust as needed.
i=0 #iterations
while(i<MaxIterations){
  x= x - DescentStrength*Jacobian(x,y)[1]
  y= y - DescentStrength*Jacobian(x,y)[2]
  i=i+1
}
print(x)
print(y)
