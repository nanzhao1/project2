# conduct a single experiment with a specified strategy to test whether the prisoner will succeed 
# if successful, it will return 1. Otherwise, return 0
Strategy = function(n, k, strategy, Card){
  # 2*n is the number of prisoners
  # k is the prisoner's number
  # strategy space is {1,2,3}
  #strategy 1 represents that the prisoner starts at the box with the number k on it
  # Card is 2*n by 1 vector, its index is the box number, its element is thr card number
  # i.e. Card[i] = j, i is the index of box, j is the card number in that box
  Count = 0 
  # whenever the experiment is successful, the count is assigned to 1
  # otherwise, the count is still 0
  Box = array(1:(2*n))
  if (strategy != 3){
    Box_index = k # the prisoner starts at the box with their number on it
    if (strategy == 2) {
      ran_num = sample(Box,1) 
      Box_index = ran_num 
      # the prisoner starts from a randomly selected box
      # use sample to random a number in 1:100 
    }
    trials = 1 # count the number of trials 
    while (trials <= n && Card[Box_index] != k) {
      # the prisoners can only try n times at most
      # if the trial times less than n and didn't find the number, keep running
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
      # if the n random selected boxes have its number, assign the Count 1
      Count = 1
    }
  }
  # return the Count to measure whether it is successful
  return(Count)
}

# estimate the probability of a single prisoner succeeding in finding their number
Pone = function(n, k, strategy, nreps){
  # n is the number of prisoners, k the prisoners's number,
  # strategy 1 2 3
  # nreps the number of replicate simulations to run to estimate the the probability, 1000 is reasonable
  Box <- array(1:(2*n))
  # create the 2*n boxes
  Success = 0
  # Success is the number of times when the prisoner manages to find its number
  for (i in 1:nreps){
    Card <- sample(Box, (2*n))
    # define n boxes which have a card of its number inside it, i.e. the ith box has a card with number i
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
    
    # define n boxes which have a card of its number inside it, i.e. the ith box has a card with number i
    
    # shuffle the n cards and distribute them into the n boxes
    # Card[i] = j, i is the index of box, j is the card number in that box
    Card <- sample(Box, (2*n))
    prisoner_number = 1
    
    while (prisoner_number <= (2*n)  &&
           Strategy((2*n), prisoner_number, strategy, Card) == 1) {
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

