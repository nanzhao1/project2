# dloop is the function to estimate the probability of each loop length from 1 to 2n occuring at least once in a randomly shuffing of cards to boxes by simulation.
# The return of dloop is a 1 by 2n array, whose entry i represents the probability of i-length loop occuring at least once.
dloop = function(n, nreps){
  # 2n is the number of prisoners.
  # nreps is the number of simulations. 
  # frequency is a nreps by 2n array used to record the different length loops' occuring probabilities
  frequency = matrix(0,nrow=nreps,ncol=2*n) 
  for (i in 1:nreps){
    Card = sample(1:(2*n))
    for (k in 1:(2*n)) {
      Box_index = k
      # trials records how many times that prisoner k can find its card k. 
      # In other words, the value of trails is the length of the loop where prisoner k is in. 
      trials = 1
      while (Card[Box_index] != k) {
        Box_index = Card[Box_index]
        trials = trials + 1
      }
      frequency[i,trials]=1
    }
    
  }
  return(colSums(frequency)/nreps)
}
