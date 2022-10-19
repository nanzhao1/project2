# 1.Nan Zhao s2314337  2.Bokun Yu s2359196  3.Lulu Xu s2418317
# https://github.com/nanzhao1/project2.git
# contribution:


# conduct a simulation with strategy 1 or 2 or 3 to test whether the prisoner k succeed to find the card k
# if the prisoner k is successful to find the card k, the function Strategy will return 1. Otherwise, return 0
Strategy = function(n, k, strategy, Card){
  # 2*n is the total number of prisoners
  # k is the selected prisoner's number
  # strategy space is {1,2,3}
  # strategy 1 represents that the prisoner k starts at the box with the number k on it, and when the card number inside this box is not k, then open the box with the number same as the card number inside the previous box, repeat this process until find the card with number k 
  # strategy 2 represents that the prisoner k starts at a randomly selected box, then repeat the process as strategy 1.
  # strategy 3 represents that the prisoner k open n boxes randomly to find the card with number k on it.
  # Card is a 2*n by 1 vector, its index is the box number, its element is the card number
  # i.e. Card[i] = j, i is the index of box, j is the card number in that box
  Count = 0 
  # When the simulation is successful, the count is assigned to 1, otherwise, the count is still 0
  Box = array(1:(2*n))
  if (strategy != 3){# if strategy is not 3,that is 1 or 2, go this way
    # if the strategy is 1
    Box_index = k # the prisoner starts at the box with their number on it
    # if the strategy is 2
    if (strategy == 2) {
      ran_num = sample(Box,1) 
      Box_index = ran_num 
      # the prisoner starts from a randomly selected box with the number ran_num.
      # use sample to random a number in 1:2*n 
    }
    trials = 1 # count the number of trials 
    while (trials <= n && Card[Box_index] != k) {
      # the prisoners can only try n times at most, if the trial times less or equal to n and didn't find the card number k, keep running
      # otherwise, break the loop, and get the number of trials.
      Box_index = Card[Box_index]
      trials = trials + 1
    }
    if (Card[Box_index] == k) {
      Count = 1
      # only if the prisoner find its number, say k, the Count is assigned to 1
    }
  }else{
    Box_index = sample(Box, n) 
    # open n boxes at random, checking each card for their number
    if (tabulate(match(Card[Box_index],k)) == 1){
      # match function is used to see if the card number inside the box same as the selected prisoner number k or not. If match successfully return 1, otherwise return 0.
      # tabulate function is used to sum up the matching results(0 or 1) to see if there exists a box with card number k. 
      # if the n random selected boxes have the card with number k, assign the Count 1
      Count = 1
    }
  }
  # return the value of Count(0 or 1) to measure whether it is successful
  return(Count)
}

# estimate the probability of a single prisoner succeeding in finding the corresponding number.
Pone = function(n, k, strategy, nreps){
  # n is the number of prisoners, k the prisoner's number,
  # strategy 1 2 3 
  # nreps is the number of replicate simulations to run to estimate the the probability, 1000 is reasonable
  Box <- array(1:(2*n))
  # create the 1 by 2*n boxes
  Success = 0
  # Success is the number of times when the prisoner manages to find its number
  for (i in 1:nreps){
    Card <- sample(Box, (2*n))
    # define 1 by 2n boxes which have a card of its number inside it, i.e. the ith box has a card with number i
    # shuffle the n cards and distribute them into the n boxes
    # Card[i] = j, i is the index of box, j is the card number in that box
    Success = Success + Strategy(n, k, strategy, Card)
    
  }  
  Probability_Estimate = Success / nreps
  return(Probability_Estimate)
  
}

Pall  = function(n, strategy, nreps) {
  Success = 0
  trials = 1
  Box <- array(1:(2*n))
  while (trials <= nreps){
    
    # define 1 by 2n boxes which have a card of its number inside it, i.e. the ith box has a card with number i
    # shuffle the n cards and distribute them into the n boxes
    # Card[i] = j, i is the index of box, j is the card number in that box
    Card <- sample(Box, (2*n))
    prisoner_number = 1
    # prisoner number indicate that the i-th prisoner to conduct the simulation.
    # call the Strategy function to see if the i-th prisoner succeed to find corresponding card number, if succeed, continue the loop; if fail, break the loop.
    while (prisoner_number <= (2*n)  &&
           Strategy(n, prisoner_number, strategy, Card) == 1) {
      prisoner_number = prisoner_number + 1
    }
    if (prisoner_number == (2*n) + 1) {
      Success =  Success + 1
    }
    trials = trials + 1
  }
  Probability_Estimate = Success / nreps
  return(Probability_Estimate)
}

dloop = function(n, nreps){
  prob = array(0,2*n)
  Box = array(1:(2*n))
  
  k = sample(Box, 1)
  
  for (i in 1:nreps){
    trials = 1
    Card = sample(Box, 2*n)
    Box_index = k
    while (trials <= (2*n) && Card[Box_index] != k) {
      Box_index = Card[Box_index]
      trials = trials + 1
    }
    prob[trials] = prob[trials] + 1
  }
  
  return(prob)
  
}
