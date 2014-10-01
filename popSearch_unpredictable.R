#!/usr/bin/env Rscript

# Random search

# 5 iterations over 3 conditions cost cost 5 Mbs

rm(list = ls())

# Import packages and functions
library(foreach)
library(iterators)
library(doSNOW)


# function to remember

 exper <- function(x){return((x-utility)*dnorm(x,0,1))}

# Adding two normal distributions we get again a normal. A part of the variance in the popularity of the objects is innatic to the individual and another part is inherent to the objects. In this way, we can manipulate the variability in the population. Here I desing the desing the entire parameter space that will be tested in the simulation.

theRatioA <-  seq(0,1,by = 0.1)
theRatioA <- sqrt(theRatioA)
theRatioB <-  seq(1,0,by = -0.1)
theRatioB <- sqrt(theRatioB)
ratioSpace <- matrix(rbind(theRatioA,theRatioB),nrow = 2, ncol = length(theRatioA))
# costSpace <- c(1/2^4) # limited space
# costSpace <- c(1/2^3,1/2^4,1/2^5) # extended space
costSpace <- costs <- c(1/2^3,1/2^4,1/2^5,1/2^6,1/2^7) # huge space
costSpace <- t(replicate(length(ratioSpace[1,]),costSpace))
costSpace <- as.vector(costSpace)
# ratioSpace <- rbind(t(ratioSpace),t(ratioSpace),t(ratioSpace)) # expended space
ratioSpace <- rbind(t(ratioSpace),t(ratioSpace),t(ratioSpace),t(ratioSpace),t(ratioSpace)) #  huge space

# parameterSpace <- cbind(t(ratioSpace),costSpace) # limited space.
parameterSpace <- cbind(ratioSpace, costSpace) # extended space.
noParSets <- dim(parameterSpace)[1]


# we can compute the thresholds at the beginning for each cost 
# it should speed up computations
findOptimalThresholds <- function(utility, cost) {
  exper <- function(x){return((x-utility)*dnorm(x,0,1))}
  result <- (integrate(exper,utility,Inf)[[1]] - cost)^2 
  return(result)
} 
optThresholds <- matrix(rep(NA, 2*length(costs)), nrow=2)
optThresholds[1,] <- costs

for(cs in 1:length(costs)) { 
  optThresholds[2,cs] <- optim(par=0.1, fn=findOptimalThresholds, cost=costs[cs])$par[[1]]}


mean <- 0   
objectiveDataSpace <- matrix(rep(0,11000),nrow = 100, ncol = 110)
for (i in 1:110){
    if (i %% 11 != 0){objectiveData <- rnorm(n = 100,mean,theRatioA[i%%11])}
    else {objectiveData <- rnorm(n = 100,mean,1)}
    objectiveDataSpace[,i] <- objectiveData
}



runOneCondition <- function(cond, parameterSpace, optThresholds,objectiveDataSpace){

  #############################################################################
  #Parameters of the environment. noAgents stands for the number of agents in the simulation#. At the moment I use 1000 but it might make sense to make it even larger for the final# publication. Bigloop the number of repetitions. Search is the max length of search. Me#an stands for the mean utility in the environment.
  #############################################################################

  noAgents <-  1000
  bigLoop <- 100
  prediction <- 0
  noAlts <- 100
  mean <- 0
  reg2 <-  0
  noParSets <- dim(parameterSpace)[1]


  ratioA <- parameterSpace[cond,1]   # this is the objective part.
  ratioB <- parameterSpace[cond,2]   # this is the subjective, idiosyncratic part.
  cost <- parameterSpace[cond,3]
  threshold <- optThresholds[2, optThresholds[1,] == cost ]

  # Three level of memories are used. The first saves results at the level of the agent. THe second saves results at the level of the repetition of the simulation and the final saves results at the level of the parametric space.

  # memDRounds <- c(rep(0,noAgents))
  # maxDynamic <-  c(rep(0,noAgents))
  # memChoice <- c(rep(0,noAgents))
  # memUncer <- c(rep(0,noAgents))
  # orderMemory <- list()
  # errorMemory <- list()

  # # Second level.
  # listMemChoice <- vector('list', bigLoop)
  # listMemDRounds <- vector('list', bigLoop)
  # listMemUncer <- vector('list', bigLoop)
  # listMaxDynamic <- vector('list', bigLoop)
  # bigOrderMemory <- list()
  # bigErrorMemory <- list()




  # a giant matrix for all the data, in tidy format
  # vars: parSetID, iterBigLoop, agent, memDRounds, maxDynamic, memChoice, memUncer
  # bigCondID, ratioA, ratioB, cost - left out, easier to add it after the simulation
  results <- matrix(NA,nrow=noAgents*bigLoop, ncol=9)
  # resultsDetailed <- matrix(NA,nrow=noAlts*bigLoop, ncol=9)
  resultsDetailed <- data.frame()

  counter <- 1

  for (iter in 1:bigLoop){

    # ptm <- proc.time()

    # cat("We are in condition ", cond, "iteration ", iter, "\n")

    # this has to be taken above
   

    if (cond %% 11 != 0){check <- cond %% 11} else {check <- 11}
    if (iter %% 10 != 0){check2 <- iter %/% 10} else {check2 <- (iter %/% 10) - 1}
    objectiveData <- objectiveDataSpace[,check2*11 + check]
    uncer2 <- ratioA
    theData <- objectiveData
    theData <- cbind(seq(1,noAlts,1),rep(0,noAlts),rep(0,noAlts),rep(0,noAlts),rep(0,noAlts),theData,objectiveData,rep(0,noAlts),rep(0,noAlts))
    theData <- as.data.frame(theData)
    colnames(theData) <- c("identity","popularity","views","average","biased","quality","objective","luck","predictions")

    # This part loops in the number of agents in the simulation. First the individual part of the variance is defined and then the simulaiton begins.

    error <- rnorm(n = noAlts*noAgents,mean,ratioB)
    errorMemory <- matrix(error, nrow = noAlts, ncol = noAgents)


    for (agent in 1:noAgents){

      memDynamic <- c(rep(0,noAlts))
      lr <- 0
      pr <- 1

      # The first agent searches at random as there are no available cues. The agents coming after him search on the basis of popularity. When two or more items have the same popularity they search at random.

      # adding subjective part of the preference - agent specific
      theData$quality <- theData$objective + errorMemory[,agent]

      # randomizing the order of alternatives, "random search"
      theData[,"luck"] <- sample(noAlts)

      # The first agent selects always at random.
      if (agent == 1){
        theData <- theData[order(theData$luck,decreasing = TRUE),]
      }else{

        # unmark all that for random search. This will also accelerate the algorithm.
        # reg <- lm(theData$objective ~ theData$popularity + 1)
        reg2 <- lm(theData$objective ~ log2(theData$popularity + 1))
        # reg2 <- lm(theData$objective ~ log2(theData$popularity + 1) + theData$biased)
        # reg2 <- lm(theData$objective ~ log2(theData$views + 1) + theData$average)
        # bc <- boxcox(reg2)
        # reg <- lm(theData$objective ~ thedata$popularity + thedata$biased)
        #  uncer <- summary(reg)$sigma
        uncer2 <- summary(reg2)$sigma
        #alpha <- summary(reg2)$coefficients[1]
        # beta <-  summary(reg2)$coefficients[2]
        predictions <- predict.lm(reg2, newdata = theData)
        theData$predictions <- predictions

        # At that point we define the heuristics that will be used by the agents. Average stands for ordering the search accoring to the mean scores of the items, popularity according to their market share and fially luck correspond to the random ordering of the traditional theory,

        theData <- theData[order(theData$popularity,theData$luck,decreasing = TRUE ),]
        # theData <- theData[order(theData$predictions,theData$luck,decreasing = TRUE                  ),]
        # theData <- theData[order(theData$luck,decreasing = TRUE),]
      }


      highestUtility <-  max(theData[1:noAlts,"quality"])
      # In this loop the agents search through the items until the cost of searhing one more item is higher than the benefits from learning it's utility. I use a while loop in order to implement that. The maximum length of search is 100.

      for (i in 1:noAlts){
        # print(i)
        utility <- max(theData[1:i,"quality"])
        
        if (pr == 1){
          # theValue <- integrate(exper,utility,Inf)[[1]]
          memDynamic[i] <- utility - (cost*i)
          #if (theValue - cost < 0 || i == noAlts) {
          if (utility > threshold || i == noAlts) {  
            pr <- 2
            theMax <- max.col(t(theData[1:i,"quality"]))
            remember <- theData[theMax,"identity"]
          }
          lr <- lr + 1
        }
        if(pr==2){break}
      }

      differ <- highestUtility - utility
      # adding "sampling" or "viewings" of the alternative
      theData[1:lr,"views"] <- theData[1:lr,"views"] + 1
      theData[1:lr,"average"] <- (theData[1:lr,"average"]*(theData[1:lr,"views"] - 1) + theData[1:lr,"quality" ])/theData[1:lr,"views"]

      # adding the choice to the popularity cue of that alternative
      theData[theMax,"popularity"] <- theData[theMax,"popularity"] + 1
      theData[theMax,"biased"] <- ( theData[theMax,"biased"]*(theData[theMax,"popularity"] - 1) + theData[theMax,"quality"] )/theData[theMax,"popularity"]

      # copy the results to the memories of the first level.
      # memChoice[agent] <- remember  # ID of a chosen alternative
      # memUncer [agent] <- uncer2  # saving objective part of the variance
      # memDRounds[agent] <- lr  # length of search for each agent
      # maxDynamic[agent] <- memDynamic[lr]  # overall utility when costs are deducted
      # if (maxDynamic[agent] == 0){maxDynamic[agent] <- memDynamic[lr - 1]}
      # if (agent %% 100 == 0){orderMemory[[agent]] <- theData}

      # vars: parSetID, iterBigLoop, agent, memDRounds, maxDynamic, memChoice, memUncer
      if (memDynamic[lr] == 0){
        maxDynamic <- memDynamic[lr - 1]
      }else{
        maxDynamic <- memDynamic[lr]
      }
      results[counter,] <- c(cond, iter, agent, lr, maxDynamic, remember, uncer2, differ, highestUtility)
      counter <- counter + 1


    }  # end of one iteration


    # copy the results to the memories of the second level.
    # listMemChoice[[iter]] <- memChoice
    # listMemDRounds[[iter]] <- memDRounds
    # listMaxDynamic[[iter]] <- maxDynamic
    # listMemUncer[[iter]] <- memUncer
    # bigOrderMemory[[iter]] <- orderMemory
    # bigErrorMemory[[iter]] <- errorMemory

    # print(proc.time() - ptm)

    # saving detailed results from one iteration (basically matrix "theData")
    # vars: "identity","popularity","views","average","biased","quality","objective","luck","predictions"
    resultsDetailed <- rbind(resultsDetailed, theData)

  }

  # results <- list(listMemChoice=listMemChoice, listMemDRounds=listMemDRounds,
  #               listMaxDynamic=listMaxDynamic, listMemUncer=listMemUncer,
  #               bigOrderMemory=bigOrderMemory)

  return( list(results, resultsDetailed))

}


# ----
# Running the simulation on multiple cores or a cluster
# ----

# registering cores/cluster
cl <- makeCluster(7, type="SOCK") # using 2 cores on a single machine
clusterSetupRNG(cluster, seed = 29012001) 
registerDoSNOW(cl)

# specifying a function to combine the results
comb <- function(x, ...) {
  lapply(seq_along(x),
    function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])) )
}

# running the simulation, and measuring running time 
ptm <- proc.time()
resultsFinal <- foreach (cond = 1:noParSets, .combine=comb, .init=list(list(), list())) %dopar% {
  #cat("Condition ", cond, "finished!", "\n")
  runOneCondition(cond, parameterSpace, optThresholds, objectiveDataSpace)
}
proc.time() - ptm

# we have to stop/deregister the cluster
stopCluster(cl)


# ----
# Transforming and saving final data
# ----

# transforming results to a data frame and adding column names
results <- as.data.frame(do.call("rbind", resultsFinal[[1]]))
colnames(results) <- c("parSetID", "iterBigLoop", "agent", "memDRounds", "maxDynamic", "memChoice", "memUncer","forgoneUtility","highestUtility")

# same for the detailed results
resultsDetailed <- as.data.frame(do.call("rbind", resultsFinal[[2]]))
colnames(resultsDetailed) <- c("identity","popularity","views","average","biased","quality","objective","luck","predictions")


# setwd("~/Desktop")
# #setwd("/home/mpib/analytis/Results")
# save(parameterSpace, results, resultsDetailed, file = "../dataRaw/popSearch.RData")
save(objectiveDataSpace, parameterSpace, results, resultsDetailed, file = "popSearch_unpredictable.RData")





