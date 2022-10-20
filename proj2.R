# 1.Nan Zhao s2314337  2.Bokun Yu s2359196  3.Lulu Xu s2418317
# https://github.com/nanzhao1/project2.git
# contribution:
# Nan Zhao: code funtion Strategy, function Pall, function Pone, function dloop, make remarks on results
# Bokun Yu: collobarate to code functions Pone and dloop, write comments for functions and debug functions.
# Lulu Xu: code the examples to estimate probabilities and visualise it, modify the coding structures

# define a function Strategy
# the function Strategy conduct a simulation with strategy 1 or 2 or 3 to test whether the prisoner k succeed to find the card k

# input: 
# n means half of the total number of prisoners
# k means the selected prisoner's number
# strategy space is {1,2,3}
# strategy 1 represents that the prisoner k starts at the box with the number k on it, and when the card number inside this box is not k, then open the box with the number same as the card number inside the previous box, repeat this process until find the card with number k 
# strategy 2 represents that the prisoner k starts at a randomly selected box, then repeat the process as strategy 1.
# strategy 3 represents that the prisoner k open n boxes randomly to find the card with number k on it.
# Card means a 2*n by 1 vector, its index is the box number, its element is the card number
# i.e. Card[i] = j, i is the index of box, j is the card number in that box

# output: 
# if the prisoner k is successful to find the card k, the function Strategy will return 1. Otherwise, return 0

Strategy = function(n, k, strategy, Card){
  Count = 0 
  # When the simulation is successful, the count will be assigned to 1, otherwise, the count is still 0
  Box = array(1:(2*n))
  if (strategy != 3){ 
  # if strategy is not 3, then need to choose a box k to start
    Box_index = k 
    # if the strategy is 1, the prisoner starts at the box with their number on it
    if (strategy == 2) { 
    # if the strategy is 2
      ran_num = sample(Box,1) 
      # variable ran_sum uses sample function to randomly choose a number in 1:2*n 
      Box_index = ran_num  
      # the prisoner starts from a randomly selected box with the number ran_num
    }
    trials = 1 
    # variable trials is used to count the number of trials 
    while (trials <= n && Card[Box_index] != k) {
      # the prisoners can only try n times at most, if the trial times less or equal to n and didn't find the card number k, keep running
      # otherwise, break the loop, and get the number of trials.
      Box_index = Card[Box_index]
      trials = trials + 1
    }
    if (Card[Box_index] == k && trials <= n) {
    # if the prisoner k succeeds in finding the card k within n times
      Count = 1   
      # if the prisoner finds its number, say k, the Count is assigned to 1
    }
  }else{
    Box_index = sample(Box, n) 
    # open n boxes randomly, checking each card for their number
    if (tabulate(match(Card[Box_index],k)) == 1){
      # match function is used to see if the card number inside the box same as the selected prisoner number k or not. If match successfully return 1, otherwise return 0.
      # tabulate function is used to sum up the matching results(0 or 1) to see if there exists a box with card number k. 
      Count = 1 
      # if the n random selected boxes have the card with number k, assign the Count 1
    }
  }
  return(Count) 
  # return the value of Count(0 or 1) to measure whether it is successful
}





# define a function Pone
# the function Pone estimates the probability of a single prisoner succeeding in finding the corresponding number

# input：
# n means half of the total number of prisoners
# k means the selected prisoner's number
# strategy space is {1,2,3}，and the representation of the strategy is as the same as that in the function Strategy
# nreps is the number of replicate simulations to run to estimate the the probability, 10000 is reasonable

# output:
# return Probability_Estimate, which means the probability of a single prisoner succeeding in finding their number


Pone = function(n, k, strategy, nreps){
  Box <- array(1:(2*n))
  # create the 1 by 2*n boxes
  Success = 0
  # Success is the number of times when the prisoner manages to find its number
  for (i in 1:nreps){
    Card <- sample(Box, (2*n))
    # shuffle the n cards and distribute them into the n boxes
    # Card[i] = j, i is the index of box, j is the card number in that box
    Success = Success + Strategy(n, k, strategy, Card)
    # When the prisoner k succeed to find the card k within n times, then Strategy is assigned to be 1, then Success times puls 1. Otherwise Strategy is still 0, and Success unchange.
  }  
  Probability_Estimate = Success / nreps
  return(Probability_Estimate)
}





# define a function Pall
# the function Pall estimates the probability of all prisoners succeeding in finding the corresponding number

# input:
# n means half of the total number of prisoners
# strategy space is {1,2,3}，and the representation of the strategy is as the same as that in the function Strategy
# nreps is the number of replicate simulations to run to estimate the the probability, 10000 is reasonable

# output:
# return Probability_Estimate, which means the probability of all prisoners finding their number


Pall  = function(n, strategy, nreps) {
  Success = 0
  # Success is the number of times when the prisoner manages to find its number
  trials = 1
  # variable trials is used to count the number of trials 
  Box <- array(1:(2*n))
  while (trials <= nreps){
    Card <- sample(Box, (2*n))
    # shuffle the n cards and distribute them into the n boxes
    # Card[i] = j, i is the index of box, j is the card number in that box
    prisoner_number = 1
    # prisoner number indicate that the i-th prisoner to conduct the simulation, continuing the while loop until all prisoners succeed (prisoner_number <= (2*n)).
    # call the Strategy function to see if the i-th prisoner succeed to find corresponding card number, if succeed (Strategy==1), continue the loop; if fail, break the loop.
    while (prisoner_number <= (2*n)  && Strategy(n, prisoner_number, strategy, Card) == 1) {
      prisoner_number = prisoner_number + 1
    }
    if (prisoner_number == (2*n) + 1) {
    # when all 2*n prisoners succeed to find their card number, then prisoner_number will be 2*n+1
      Success =  Success + 1
    }
    trials = trials + 1
  }
  Probability_Estimate = Success / nreps
  return(Probability_Estimate)
}





# use the examples when n=5 and when n=50
# use the function Strategy, function Pone, function Pall to get the result of examples
# then estimate the probability of a single prisoner and all prosoners finding their number
# get the success probabilities under each strategy

# input:
# n=5, strategy = 1 or 2 or 3, nreps = 10000, take a random k-value such as 2
# n=50, strategy = 1 or 2 or 3, nreps = 10000, take a random k-value such as 2

# output:
# the individual and joint success probabilities under each strategy for n=5 and for n=50


# example: strategy 1, n=5
individual_1_5 <- paste("When strategy=1, n=5, the probability of a single prisoner succeeding in finding their number is:",Pone(5,2,1,10000),sep=" ")
print(individual_1_5)
joint_1_5 <- paste("When strategy=1, n=5, the probability of all prisoners finding their number is :",Pall(5,1,10000),sep=" ")
print(joint_1_5)

# example: strategy 2, n=5
individual_2_5 <- paste("When strategy=2, n=5, the probability of a single prisoner succeeding in finding their number is:",Pone(5,2,2,10000),sep=" ")
print(individual_2_5)
joint_2_5 <- paste("When strategy=2, n=5, the probability of all prisoners finding their number is :",Pall(5,2,10000),sep=" ")
print(joint_2_5)

# example: strategy 3, n=5
individual_3_5 <- paste("When strategy=3, n=5, the probability of a single prisoner succeeding in finding their number is:",Pone(5,2,3,10000),sep=" ")
print(individual_3_5)
joint_3_5 <- paste("When strategy=3, n=5, the probability of all prisoners finding their number is :",Pall(5,3,10000),sep=" ")
print(joint_3_5)

# example: strategy 1, n=50
individual_1_50 <- paste("When strategy=1, n=50, the probability of a single prisoner succeeding in finding their number is:",Pone(50,2,1,10000),sep=" ")
print(individual_1_50)
joint_1_50 <- paste("When strategy=1, n=50, the probability of all prisoners finding their number is :",Pall(50,1,10000),sep=" ")
print(joint_1_50)

# example: strategy 2, n=50
individual_2_50 <- paste("When strategy=2, n=50, the probability of a single prisoner succeeding in finding their number is:",Pone(50,2,2,10000),sep=" ")
print(individual_2_50)
joint_2_50 <- paste("When strategy=2, n=50, the probability of all prisoners finding their number is :",Pall(50,2,10000),sep=" ")
print(joint_2_50)

# example: strategy 3, n=50
individual_3_50 <- paste("When strategy=3, n=50, the probability of a single prisoner succeeding in finding their number is:",Pone(50,2,3,10000),sep=" ")
print(individual_3_50)
joint_3_50 <- paste("When strategy=3, n=50, the probability of all prisoners finding their number is :",Pall(50,3,10000),sep=" ")
print(joint_3_50)


# remarks on results:
# As n grows larger, the individual success probabilities of strategy 1 and 3 approach 0.5, while that of strategy 2 approaches 0.38.
# The event that the prisoner k find the number k is identically and independently with success probability p.
# The strategy 2, counterintuitively, the probability of the event that the individual find the number in 2*n boxes by n trials is not converging to 0.5.
# Under the circumstances that the prisoner K open the box J containing the card J, the prisoner could never find the card K in strategy 2, which results in a lower probability.
# The event that all the prisoners, simultaneously, find their number in one experiment follows the distribution of binomial (100, p). The consequent success probability if p^100, which is close to 0.
# When n is large enough, the joint success probabilities of strategy 2 and strategy 3 are approaching 0. 
# When n is large enough, counterintuitively, the joint success probabilities of strategy 1 is approximating to 0.31 instead of 0.
# The strategy 1 turns out an effective way to succeed, as the joint success probability boosts.
# In strategy 1, if one prisoner fails, then there will a lot of other prisoners fail. While one prisoner succeeds, the other prisoners consequently succeed as well.
# That is, the prisoners tend to succeed or fail in the meantime. The consequent joint success probability is high.
# It can be viewed as a component of circles. The elements in the same circle will succeed or fail simultaneously. Whenever the maximum circle length is no longer than trial time, the prisoners can all succeed.



# define a function dloop
# dloop is the function to estimate the probability of each loop length from 1 to 2n occuring at least once in a randomly shuffing of cards to boxes by simulation.

# input:
# n means half of the total number of prisoners
# nreps is the number of replicate simulations to run to estimate the the probability, 1000 is reasonable

# output:
# The return of dloop is a 1 by 2n array, whose entry i represents the probability of i-length loop occuring at least once.

dloop = function(n, nreps){
  # n is half of the total number of prisoners
  # nreps is the number of simulations. 
  occuring_times = array(0, 2*n)
  # occuring_times is a 1 by 2n array used to record how many times that different length loops occur at least once in nreps trials. 
  Box = array(1:(2*n))
  for (i in 1:nreps){
    Card = sample(1,(2*n))
    frequency = array(0,(2*n))
    # frequency is a 1 by 2n array used to record the occurence of different length loops, ie if length i loop occurs at least once, then i-th entry of frequency is assigned to be 1. Otherwise, it is still 0.
    for (k in 1:(2*n)) {
      Box_index = k
      trials = 1
      # trials records how many times that prisoner k can find its card k. 
      # In other words, the value of trails is the length of the loop where prisoner k is in. 
      while (Card[Box_index] != k) {
        Box_index = Card[Box_index]
        # Box_index is changed to be the card number inside the previous box
        trials = trials + 1
      }
      frequency[trials] = 1
      # the trials-th entry of frequency is assigned to be 1 when the loop of length trials occur at least once 
    }
    occuring_times = occuring_times + frequency
    # sum how many times of different length loops occur at least once in nreps trials.
    prob = occuring_times / nreps
    # prob is a 1 by 2*n used to estimate the probability of 1-2*n length loop occur at least once 
  }
  return(prob)
}



# assess the probability that there is no loop longer than 50 in a random reshuffling of cards to boxes
# visualise the probabilities sensibly

# input:
# n=50, nreps = 10000

# output:
# the probability that there is no loop longer than 50 in a random reshuffling of cards to boxes
# picture shows the corresponding probabilities

prob = dloop(50,10000)
# show the probability of each loop length from 1 to 100 occurring at least once in a random shuffling of cards to boxes
print(prob)
# aim to assess the probability that there is no loop longer than 50 in a random reshuffling of cards to boxes
# probability equation: the above probability equals 1 minus the one when the loop length is longer than 50 assessed by function dloop
result <- paste("The probability that there is no loop longer than 50 in a random reshuffling cards to boxes:",1-sum(prob[51:100]),sep=" ")
print(result)
# visualise the probabilities
plot(prob)











