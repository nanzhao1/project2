pone=function(n,k,strategy,nreps){
  
  # n is the total number of prisoners
  # k is the first prisoner's number
  # strategy represent strtegies 1,2,3
  # nreps is the number of experiments
  success=0
  for (i in 1:nreps){
    box=c(1:n)
    card_number=sample(box,n)
    if (strategy == 1){
      times=1
      index=k
      while(times <= n/2 && card_number[index] != k)
      {index = card_number[index]
      times = times+1}
      if (card_number[index]==k)
      {success=success+1}
    }
    else if (strategy==2){
      k=sample(box,1)
      times=1
      index=k
      while(times <= n/2 && card_number[index] != k)
      {index = card_number[index]
      times = times+1}
      if (card_number[index]==k)
      {success=success+1}
    }
    else if (strategy==3){
      for (j in 1:n/2){
        k=sample(box,1)
        if (card_number[k] == k)
        {success=success+1
        break}
      }
    }
    else {print('wrong')}
  }
  success_rate=success/nreps
  print(success_rate)}

Pall  = function(n, strategy, nreps) {
  Success = 0
  trials = 1
  while (trials <= nreps){
    Box <- array(1:n) 
    # define n boxes which have a card of its number inside it, i.e. the ith box has a card with number i
    Card <- sample(Box, n) 
    # shuffle the n cards and distribute them into the n boxes
    # Card[i] = j, i is the index of box, j is the card number in that box
    k = 1
    prisoner_number = k
    while (Pone(n, prisoner_number, strategy, 1) !=0 && prisoner_number <= n) {
      prisoner_number = prisoner_number + 1
    }
    if (prisoner_number == n) {
      Success =  Success + 1
    }
    trials = trials + 1
    
  }
  Probability_Estimate = Success / nreps
  
  return(Probability_Estimate)
}


