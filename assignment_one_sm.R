#Rachael Lee | Assignment 1 | Stochastic Modeling 
rm(list = ls())

#Decision Tree
decisiontree <- function(outcomes, probability){
  decisions <- outcomes*probability
  return(rowSums(decisions))}

#Here I wrote a simple function that takes an outcome matrix and the probabilities and returns the estimated values.
#I will be using this for the calculations below.

#Question 2

  #Option 1: futures contract for 10 tons (+$110)
  #Option 2: futures contract for 5 tons (+$65) and market price for 5 tons
  #Option 3: market price for 10 tons

    potential <- c(0.078, 0.083, 0.087, 0.091, 0.096)
    sugarhigh_probability <- c(0.05, 0.25, 0.35, 0.20, 0.15)
  #Q2.1
    sugarhigh_avgprice <- sum(potential *sugarhigh_probability)
    #multiplying each price by the likelihood and adding them together creates a likely output price of 0.0877
  #Q2.2
    sugar_prices <- c(0.0851*2000, sugarhigh_avgprice*2000)
    #Here I converted the prices from by pound to by ton
    option_matrix <- matrix(c(10, 0, 5, 5, 0, 10), 3, byrow = TRUE)
    #Next I put the options into the option matrix 
    decisiontree(option_matrix, sugar_prices) + c(110, 65, 0)
    # 1812 1793 1754
  #Q2.3
    #The best option is Option 3, buy all at market price for an estimated total of $1,754
    #This just means that the price of the future contract does not offset the increase in price over the time period
  #Q2.4
    print(decisiontree(option_matrix, sugar_prices))
    #[1] 1702 1728 1754
    #These are the prices for just the sugar, so the contracts need to be equal to the price differential to be "at least as good"
    1754 - 1702 == 52
    1754 - 1728 == 26

#Question 3
  #Option 1: Admit All
  #Option 2: Admit None
  #Q3.1
    #Lets define our variables:
      ill_probability <- c(0.05, 0.95)
      ill_healthy <- matrix(c(-100000, 10000), 1)
    #Multiply the variables by their likelihood and add to find the expected value
      decisiontree(ill_healthy, ill_probability)
      #$4500
  #Q3.2
  #Q3.3
    #TN = 100%, FN = 10%, TP = 90%, FP = 0%
    #All negative patients test negative, but 10% of positive patients falsely test negative 
      #Probability of negative test P(-) = TN + FN
      sum(ill_probability*c(0.1, 1))
      #0.955 -- this means that 95.5% of immigrants will test negative
  #Q3.4
    #Bayes Theorum inverts the probabilities
    #Probability of a Not Diseased person testing positive and probability of a not-diseased person testing negative
    #False positive, True negative = FP, TN = 0, 1
  #Q3.5
    #Next we want to use the decision tree to calculate the estimated value of admitting all immigrants.
    ill_admit_probability <- c(0.95*0, 0.95*1, 0.05*0.9, 0.05*0.1)
    #The probabilities are those of ill/healthy patients times the accuracy of the tests to make a paradigm of (ND+, ND-, D+, D-)
    test_cost = 500
    ill_admit_outcomes <- matrix(c(10000-test_cost, 10000-test_cost, -test_cost, -100000-test_cost), 1)
    #The outcome matrix adds the projected value of an immigrant minus the $500 cost of the test.
    decisiontree(ill_admit_probability, ill_admit_outcomes)
    #$8500, which is not one of the answers, so we choose "Answer Not Shown"
  #Q3.6
    #We're looking for a value of x that would cause the estimated value to equal zero.
    x = 450
    (0.95*(1000-x) + 0.045*(-x) + 0.005*(-100000 - x))
    #I struggled getting any of my solve functions to work, so instead I input each of the potential test costs to find when they ==0
  