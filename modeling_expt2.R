
# ------------------------------------------------------------------------- #
# --------- Adding types, but not tokens, affects property induction ------ #
# ----------------- Modeling Experiment 2 conditions ---------------------- #
# -------------------------------------------------------------------------- #

library(cowplot)
library(tidyverse)
library(BayesFactor)

# rm(list = ls()) # use Ctrl + Shift + F10 to clear environment
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("./BayesGen-BX.R")
set.seed(1)

# ------------ PARAMETERS ----------------------------- #
phi <- 1                 # phi = 1: uniform prior over hypotheses, phi > 1: greater prior belief in larger hypotheses
nits <- 1000             # number of iterations

# --------------------------- generate predictions ------------------------------ #
parits <- 3000  # how many times do I want to test out different parameter values?

typesCols <- c("2tokens", "4tokens")
typesHigh <- matrix(data = c(rep(0, parits*2)), nrow = parits)
colnames(typesHigh) <- typesCols

typesLow <- typesMed <- typesHigh

tokensCols <- c("1type", "2types", "3types")
tokensHigh <- matrix(data = c(rep(0, parits*3)), nrow = parits)
colnames(tokensHigh) <- tokensCols

tokensLow <- tokensMed <- tokensHigh

pars <- c("thetaType", "thetaToken", "target1", "target2", "target3", "typesd", "tokensd")
parValues <- matrix(data = c(rep(0, parits*length(pars))), nrow = parits)
colnames(parValues) <- pars

test <- c(.51, .707, .9)  # this is overridden in l. 47
nt <- length(test)                                    # number of test items

# --------------------------- generate predictions ------------------------------ #
# use a loop to test different parameter values
for (a in 1:parits) {
  
  # sample different parameter values
  # theta values to represent sampling assumptions/informational value
  thetaType <- runif(1, min = .1, max = .5)          # value used to discuss modeling results = .3
  thetaToken <- runif(1, min = .1, max = thetaType)  # value in paper = .15
  thetaRep <- thetaRep2 <- thetaRep3 <- thetaToken
  # thetaRep3 <- thetaToken
  # thetaRep <- thetaRep3 - .1
  # thetaRep2 <- thetaRep3 - .05
  
  # stimulus values for types
  targetsM <- runif(1, .2, .6) # select the centre of a uniform distribution, from which to sample types
  targetsW <- runif(1, .005, .2)  # select the width of a uniform distribution, from which to sample types
  
  targets <- runif(3, targetsM - targetsW, targetsM + targetsW) # sample from that uniform distribution
  targets <- sort(targets)
  target1 <- targets[2]  # value in paper = .45
  target2 <- targets[3]                   # .46
  target3 <- targets[1]                   # .44

  # when randomly generating stimulus values for types/tokens, how much standard deviation to use?
  typesd <- runif(1, .01, .1)             # .06
  tokensd <- typesd/6                     # .01, > experiment 1 (.009) because images are now reflected/rotated

  bg2 <- matrix(data = c(rep(0, nits*nt)),   # create enough 0s for each iteration x each test item - to be filled with predicted generalisation probability
                nrow = nt)                   # 1 row for each test item (high, medium, low sim)
  bg4 <- bg44 <- bg444 <- bg222 <- bg22 <- bg2   # make all others in the same way - need one matrix per condition

      for( i in 1:nits ) {     # average out predictions over nits iterations
        
        train1 <- rnorm(1, mean = target1, sd = typesd) # generates number randomly from a uniform distribution, then /.2 makes it smallish, then +.4 to approach .5
        train2 <- rnorm(1, mean = target2, sd = typesd) # 2nd training exemplar
        train3 <- rnorm(1, mean = target3, sd = typesd) # 3rd training exemplar
      
        # generalisation from adding tokens 
        train1a <- rnorm(1, mean = train1, sd = tokensd)  # the repetition is constrained to be within a range around the first training item
        train1b <- rnorm(1, mean = train1, sd = tokensd)  # the above is true for all tokens
        train1c <- rnorm(1, mean = train1, sd = tokensd)
      
        train2a <- rnorm(1, mean = train2, sd = tokensd)  # the repetition is constrained to be within a range around the second training item
        train2b <- rnorm(1, mean = train2, sd = tokensd)  # the above is true for all tokens
        train2c <- rnorm(1, mean = train2, sd = tokensd)
      
        train3a <- rnorm(1, mean = train3, sd = tokensd)  # the repetition is constrained to be within a range around the third training item
        train3b <- rnorm(1, mean = train3, sd = tokensd)  # the above is true for all tokens
        train3c <- rnorm(1, mean = train3, sd = tokensd)
      
        # determine test categories
        testH <- (max(c(train1, train2, train3)) + .05)
        test <- c(testH, testH+((.95-testH)/2), .95)
        
        bg2[,i] <- (BayesGen(train1, test, thetaType, phi) +
                    BayesGen(train1a, test, thetaToken, phi))/2
        
        bg22[,i] <- (BayesGen(c(train1, train2), test, thetaType, phi) +
                       BayesGen(c(train1a, train2a), test, thetaRep2, phi))/2
        
        bg222[,i] <- (BayesGen(c(train1, train2, train3), test, thetaType, phi) +
                      BayesGen(c(train1a, train2a, train3a), test, thetaRep3, phi))/2
      
        bg4[,i] <- (BayesGen(train1, test, thetaType, phi))*1/4 +
                    (BayesGen(c(train1a, train1b, train1c), test, thetaRep, phi))*3/4
      
        bg44[,i] <- (BayesGen(c(train1, train2), test, thetaType, phi))*2/8 +
                     (BayesGen(c(train1a, train1b, train1c, train2a, train2b, train2c), test, thetaRep2, phi))*6/8
      
        bg444[,i] <- (BayesGen(c(train1, train2, train3), test, thetaType, phi))*3/12 +
                      (BayesGen(c(train1a, train1b, train1c, train2a, train2b, train2c, train3a, train3b, train3c), test, thetaRep3, phi))*9/12
      }

  bg2 <- rowMeans(bg2)          # calculate the mean of the N(nits) generalisation probabilities for each similarity category
  bg22 <- rowMeans(bg22)        # for 11 condition
  bg222 <- rowMeans(bg222)      # for 111 condition etc.,
  bg4 <- rowMeans(bg4)
  bg44 <- rowMeans(bg44)
  bg444 <- rowMeans(bg444)
  
  typesHigh[a,1] <- bg2[1] < bg222[1] # adding types with 2 tokens increases gen at high categories
  typesHigh[a,2] <- bg4[1] < bg444[1]
  
  typesMed[a,1] <- bg2[2] > bg222[2] # adding types with 2 tokens decreases gen at med categories
  typesMed[a,2] <- bg4[2] > bg444[2] # adding types with 4 tokens decreases gen at med categories
  
  typesLow[a,1] <- bg2[3] > bg222[3] # adding types with 2 tokens decreases gen at med categories
  typesLow[a,2] <- bg4[3] > bg444[3] # adding types with 4 tokens decreases gen at med categories
  
  gap <- .1
  tokensHigh[a,1] <- between(bg4[1], left = bg2[1] - gap/2, right = bg2[1] + gap/2)
  tokensHigh[a,2] <- between(bg44[1], left = bg22[1] - gap/2, right = bg22[1] + gap/2)
  tokensHigh[a,3] <- between(bg444[1], left = bg222[1] - gap/2, right = bg222[1] + gap/2)
  
  tokensMed[a,1] <- between(bg4[2], left = bg2[2] - gap/2, right = bg2[2] + gap/2)
  tokensMed[a,2] <- between(bg44[2], left = bg22[2] - gap/2, right = bg22[2] + gap/2)
  tokensMed[a,3] <- between(bg444[2], left = bg222[2] - gap/2, right = bg222[2] + gap/2)
  
  tokensLow[a,1] <- between(bg4[3], left = bg2[3] - gap/2, right = bg2[3] + gap/2)
  tokensLow[a,2] <- between(bg44[3], left = bg22[3] - gap/2, right = bg22[3] + gap/2)
  tokensLow[a,3] <- between(bg444[3], left = bg222[3] - gap/2, right = bg222[3] + gap/2)
  
  parValues[a,] <- c(thetaType, thetaToken, target1, target2, target3, typesd, tokensd)
  
}

sum(typesHigh[,1]) # adding types with 2 tokens increases gen at high categories
sum(typesHigh[,2]) # adding types with 4 tokens increases gen at high categories

sum(typesMed[,1])
sum(typesMed[,2])

sum(typesLow[,1])
sum(typesLow[,2])
# cbind(typesMed, parValues)
typesHigh
typesMed
typesLow

head(tokensHigh)
sum(tokensHigh[,1])  # adding tokens with 1 type does not change gen at high categories - it DECREASES GEN
sum(tokensHigh[,2])  # adding tokens with 2 types does not change gen at high categories
sum(tokensHigh[,3])  # adding tokens with 3 types does not change gen at high categories

sum(tokensMed[,1])  # adding tokens with 1 type does not change gen at med categories - it DECREASES GEN
sum(tokensMed[,2])  # adding tokens with 2 types does not change gen at med categories
sum(tokensMed[,3])  # adding tokens with 3 types does not change gen at med categories

sum(tokensLow[,1])  # adding tokens with 1 type does not change gen at low categories - it DECREASES GEN
sum(tokensLow[,2])  # adding tokens with 2 types does not change gen at low categories
sum(tokensLow[,3])  # adding tokens with 3 types does not change gen at low categories

tokensHigh
tokensMed
tokensLow



# vals <- as.data.frame(cbind(test, bg2, bg22, bg222, bg4, bg44, bg444))           # combine the similarity category values with generalisation probs for each condtiion
# vals <- gather(vals, key = "genCond", value = "genProb", bg2:bg444)   # gather so each row is a different generalisation probability
# 
# trains <- c(train1, train2, train3)        # collate the training stimulus values
# trains <- as.data.frame(trains)
# 
# trains1 <- c(train1, train2, train3, train1a, train2a, train3a)
# trains1 <- as.data.frame(trains1)
# trains3 <- c(train1, train2, train3, train1a, train2a, train3a, train1b, train2b, train3b, train1c, train2c, train3c)
# trains3 <- as.data.frame(trains3)
# 
# gg2 <- ggplot(vals[1:9,], aes(x = test, y = genProb)) +     # plot predicted generalisation ratings
#   geom_point(aes(x = test, y = genProb, group = genCond, colour = genCond), size = 2) +
#   geom_line(aes(colour = genCond, linetype = genCond), size = 1.4 ) +
#   theme_bw(base_size = 12) +
#   labs(title = "2 Token presentations of each type", x = "Stimulus value", y = "Generalisation Probability") +
#   scale_x_continuous(limits = c(min(trains), 1), breaks = seq(.4, 1, by = .1)) +
#   scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
#   scale_color_manual(name = "Number of \ntypes", labels = c("1", "2", "3"), values = c("red", "coral1", "seagreen")) +
#   scale_linetype_manual(name = "Number of \ntypes", labels = c("1", "2", "3"), values = c("solid", "longdash", "dotdash")) +
#   geom_point(data = trains1, aes(x = trains1, y = 1)) +    # show training stimulus values
#   geom_vline(xintercept = test, linetype = "dashed", colour = "blue")  +
#   theme(legend.position = c(.75, .75))
# 
# #bg444 exceeds 10, so doesn't show up
# 
# gg4 <- ggplot(vals[10:18,], aes(x = test, y = genProb)) +     # plot predicted generalisation ratings
#   geom_point(aes(x = test, y = genProb, group = genCond, colour = genCond), size = 2) +
#   geom_line(aes(colour = genCond, linetype = genCond), size = 1.4 ) +
#   theme_bw(base_size = 12) +
#   labs(title = "4 Token presentations of each type", x = "Stimulus value", y = "Generalisation Probability") +
#   scale_x_continuous(limits = c(min(trains), 1), breaks = seq(.4, 1, by = .1)) +
#   scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
#   scale_color_manual(name = "Number of \ntypes", labels = c("1", "2", "3"), values = c("red", "coral1", "seagreen")) +
#   scale_linetype_manual(name = "Number of \ntypes", labels = c("1", "2", "3"), values = c("solid", "longdash", "dotdash")) +
#   geom_point(data = trains3, aes(x = trains3, y = 1)) +    # show training stimulus values
#   geom_vline(xintercept = test, linetype = "dashed", colour = "blue")  +
#   theme(legend.position = c(.75, .75))
# 
# plot_grid(gg2, gg4, labels = "AUTO", ncol = 2)  # plot types and tokens graphs next to each other

