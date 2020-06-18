
# ------------------------------------------------------------------------- #
# --------- Adding types, but not tokens, affects property induction ------ #
# ----------------- Modeling Experiment 2 conditions ---------------------- #
# -------------------------------------------------------------------------- #

require(cowplot)
require(tidyverse)
require(BayesFactor)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("./BayesGen-BX.R")

# -------------------------- fixed and free parameters ------------------------------------- #
# fixed (for now)
phi <- 1            # phi = 1 -> uniform prior over all hypotheses, phi > 1 -> greater prior belief in larger hypotheses
thetaType <- .3         # theta = 1 -> strong sampling
thetaRep3 <- .15
thetaRep <- thetaRep3 - .1
thetaRep2 <- thetaRep3 - .05

nits <- 1000          # number of iterations

test <- c(.51, .707, .9)  # this is overridden in l. 47
nt <- length(test)                                    # number of test items

bg2 <- matrix(data = c(rep(0, nits*nt)),   # create enough 0s for each iteration x each test item - to be filled with predicted generalisation probability
              nrow = nt)                   # 1 row for each test item (high, medium, low sim)
bg4 <- bg44 <- bg444 <- bg222 <- bg22 <- bg2   # make all others in the same way - need one matrix per condition

target1 <- .45    
target2 <- .46
target3 <- .44
typesd <- .06      # when randomly generating stimulus values for types, how much standard deviation to use? 

probIgnore <- 0   # probability that an individual ignores repetitions - ignore/not sort of represents sampling with/without replacement
tokensd <- .01    # when randomly generating stimulus values for tokens, how much standard deviation to use? tokensd < typesd
# expect the sd to be greater than experiment 1 (.009) because images are now reflected/rotated

ignores <- rbinom(nits, size = 1, prob = probIgnore)           # if 1 -> on that iteration, ignore repetition
ignores <- matrix(rep(ignores, nt), nrow = nt, byrow = TRUE)   # just copy the ignores across 3 rows to conform to following ifelse condition

# --------------------------- generate predictions ------------------------------ #
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

  # generalisation from adding types
  # testH <- (max(c(train1, train2, train3)) + .01)
  # test <- c(testH, testH+((.9-testH)/2), .9)
  
  bg2[,i] <- (BayesGen(train1, test, thetaType, phi) +
              BayesGen(train1a, test, thetaRep, phi))/2
  
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

