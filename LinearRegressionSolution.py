# -*- coding: utf-8 -*-

import numpy as np
import pylab




def gradient_descent(t0, t1, alpha, x, y, ep, max_iter):

    converged = False
    iter = 0
    length = len(x)

    # initial error calculation (squared cost function)

    firstError = 0.0
    totalErrors = ((t0 + t1*x) - y)**2
    firstError = (np.sum(totalErrors))*(1.0/(length+2))
        

    # Iterate Loop
    while not converged:
        
        # for each training sample, compute the gradient (for lamda0 and lamda1)

        grad0 =    (1.0/length)*  (np.sum((t0 + t1*x) - y))
        grad1 = (1.0/length)*  (np.sum(((t0 + t1*x) - y)*x))

              
        # update the lamda_temp
        temp0 = t0 - alpha * grad0
        temp1 = t1 - alpha * grad1
    
        # update lamda
        t0 = temp0
        t1 = temp1

        # calculate the new error after updating lambda0 and lambda1
        secondError = 0.0
        allErrors = ((t0 + t1*x) - y)**2
        secondError = (np.sum(allErrors))*(1.0/(length+2))

        print "Current Error Value ", secondError
        
        if abs(firstError-secondError) <= ep:
            print 'Converged, iterations: ', iter, '!!!'
            converged = True
    
        firstError = secondError   # update error 
        iter += 1  # update iter
    
        if iter == max_iter:
            print 'Max interactions exceeded!'
            converged = True

    return t0,t1
    
    
    
    
def main():
 
    # read data from file 
    data = np.genfromtxt('DataSet1.csv', dtype=float, delimiter = ',')

    # extract NumPy arrays for x and y training data
    x = data[:,0]
    y = data[:,1]
    

    # set initial values for lambda (these values are randomly chosen)
    lambda0 = 3.8
    lambda1 = 0.7
    
    pylab.plot(x,y,'o')
    pylab.show()
    
 
    alpha = 0.01 # learning rate
    ep = 0.0001 # convergence criteria
    max_iter=500000

    # call gredient decent, and get intercept(=lambda0) and slope(lambda1)
    # You objective is to write the function gradient_decent
    lambda0, lambda1 = gradient_descent(lambda0, lambda1, alpha, x, y, ep, max_iter)
    print 'lambda0 and  lambda1 = ', lambda0, lambda1 


    # plot the y values proposes by our algorithm
    for i in range(len(x)):
        y_predict = lambda0 + lambda1*x 

    pylab.plot(x,y,'o')
    pylab.plot(x,y_predict,'k-')
    pylab.show()
    print "Done!"#
    
    
main()