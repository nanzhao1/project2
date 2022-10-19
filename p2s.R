Strategy = function(n, k, strategy, Card){
  #n is 
  Count = 0
  Box = array(1:(2*n))
  if (strategy != 3){
    Box_index = k # the prisoner starts at the box with their number on it
    if (strategy == 2) {
      ran_num = sample(Box,1) 
      Box_index = ran_num # the prisoner starts from a randomly selected box
    }
    trials = 1 # count the number of trials
    while (trials <= n && Card[Box_index] != k) {
      Box_index = Card[Box_index]
      trials = trials + 1
    }
    if (Card[Box_index] == k) {
      Count = 1
    }
  }else{
    Box_index = sample(Box, n) 
    # open n boxes at random, checking each card for their number
    if (tabulate(match(Card[Box_index],k)) == 1){
      Count = 1
    }
  }
  
  return(Count)
}

Pone = function(n, k, strategy, nreps){
  # n is the number of prisoners, k the prisoners's number,
  # strategy 1 2 3
  # nreps the number of replicate simulations to run to estimate the the probability, 1000 is reasonable
  Box <- array(1:(2*n))
  Success = 0
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

