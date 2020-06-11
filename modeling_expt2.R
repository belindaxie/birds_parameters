require(cowplot)
require(tidyverse)
require(BayesFactor)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("./BayesGen-BX.R")

# this deals with the conditions from Experiment 2
# 2 -> 22 -> 222 and 4 -> 44 -> 444

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
  
  # is the second token ignored or treated as a discrete exemplar?
  # bg2[,i] <- if_else(ignores[,1] == 1,     # if the set probIgnore dictates ignoring a repetition in this iteration,
  #                BayesGen(train1, test, thetaType, phi),              # then ignore repetition and behave as if only 1 training exemplar
  #                BayesGen(c(train1, train1a), test, thetaType, phi))  # otherwise, treat as 2 (only slightly) discrete exemplars
  
  bg2[,i] <- (BayesGen(train1, test, thetaType, phi) +
              BayesGen(train1a, test, thetaRep, phi))/2
  
  # bg22[,i] <- if_else(ignores[,i] == 1,
  #                BayesGen(c(train1, train2), test, thetaRep, phi),
  #                BayesGen(c(train1, train1a, train2, train2a), test, theta, phi))
  
  bg22[,i] <- (BayesGen(c(train1, train2), test, thetaType, phi) +
                 BayesGen(c(train1a, train2a), test, thetaRep2, phi))/2
  
  # bg222[,i] <- if_else(ignores[,i] == 1,
  #                BayesGen(c(train1, train2, train3), test, thetaRep, phi),
  #                BayesGen(c(train1, train1a, train2, train2a, train3, train3a), test, theta, phi))
  
  bg222[,i] <- (BayesGen(c(train1, train2, train3), test, thetaType, phi) +
                BayesGen(c(train1a, train2a, train3a), test, thetaRep3, phi))/2
  
  # bg4[,i] <- if_else(ignores[,i] == 1,     # if the set probIgnore dictates ignoring a repetition in this iteration,
  #                BayesGen(train1, test, theta, phi),              # then ignore repetition and behave as if only 1 training exemplar
  #                BayesGen(c(train1, train1a, train1b, train1c), test, theta, phi))  # otherwise, treat as 2 (only slightly) discrete exemplars
  
  bg4[,i] <- (BayesGen(train1, test, thetaType, phi))*1/4 +
              (BayesGen(c(train1a, train1b, train1c), test, thetaRep, phi))*3/4
  # bg4[,i] <- (BayesGen(train1, test, thetaType, phi) +
  #               BayesGen(c(train1a, train1b, train1c), test, thetaRep, phi))/2

  # bg44[,i] <- if_else(ignores[,i] == 1,
  #                BayesGen(c(train1, train2), test, thetaRep, phi),
  #                BayesGen(c(train1, train1a, train1b, train1c, train2, train2a, train2b, train2c), test, theta, phi))
  
  bg44[,i] <- (BayesGen(c(train1, train2), test, thetaType, phi))*2/8 +
               (BayesGen(c(train1a, train1b, train1c, train2a, train2b, train2c), test, thetaRep2, phi))*6/8
   # bg44[,i] <- (BayesGen(c(train1, train2), test, thetaType, phi) +
   #             BayesGen(c(train1a, train1b, train1c, train2a, train2b, train2c), test, thetaRep, phi))/2
   # 
  # bg444[,i] <- if_else(ignores[,i] == 1,
  #                BayesGen(c(train1, train2, train3), test, thetaRep, phi),
  #                BayesGen(c(train1, train1a, train1b, train1c, train2, train2a, train2b, train2c, train3, train3a, train3b, train3c), test, theta, phi))
  
  bg444[,i] <- (BayesGen(c(train1, train2, train3), test, thetaType, phi))*3/12 +
                (BayesGen(c(train1a, train1b, train1c, train2a, train2b, train2c, train3a, train3b, train3c), test, thetaRep3, phi))*9/12
  # bg444[,i] <- (BayesGen(c(train1, train2, train3), test, thetaType, phi) +
  #               BayesGen(c(train1a, train1b, train1c, train2a, train2b, train2c, train3a, train3b, train3c), test, thetaRep, phi))/2
}

# ------------------- categorise predictions into 10 bins/response choices -------------------- #
catNames <- c("high", "med", "low")                                # names of similarity categories             
# respLabs <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10')   # names of response options (1-10 scale)
# 
# bg2respProbs <- function(bgprobs) {  # function to convert bayes generalisation probabilities (0-1) to response (1-10 categories) probabilities 
#   bgxx <- bgprobs                    # which condition's generalisation probabilities are we dealing with?
#   rownames(bgxx) <- catNames         # specify rownames that represent similarity categories
#   # bgxx <- round(bgxx*10) #+ 1         # *10 to make <1 probabilities->integers, *1/11 shifts from 11-pt scale (0-10)->10-pt scale (0-9), +1 to shift to 1-10, round forces belonging to 1/10 categories
#   max(bgxx)                          # max should be 10
#   min(bgxx)                          # min should be 1
#   
#   bgxx <- as.data.frame(t(bgxx))   # transpose so that similarity categories occupy columns instead of rows, then convert to dataframe
#   
#   highN <- transform(bgxx, group = cut(high, breaks = seq(1, 11, by = 1),     # create a new group column that groups high responses into 10 bins
#                                        labels = respLabs,                     # label each bin
#                                        include.lowest = TRUE, right = FALSE)) # each bin includes responses that go up to the higher interval boundary
#   medN <- transform(bgxx, group = cut(med, breaks = seq(1, 11, by = 1), labels = respLabs, include.lowest = TRUE, right = FALSE)) # repeat for med responses
#   lowN <- transform(bgxx, group = cut(low, breaks = seq(1, 11, by = 1), labels = respLabs, include.lowest = TRUE, right = FALSE)) # repeat for low responses
#   
#   respProbs <- rbind(table(highN$group), table(medN$group), table(lowN$group))  # combine these grouped responses together
#   rownames(respProbs) <- catNames                 # label them according to similarity category
#   respProbs <- respProbs/nits                     # divide the counts/number of observations to get the probabilities
#   respProbs                                       # print these response probabilities                            
# }

# ----------------------- plot predictions ------------------------------ #
# ------------------ effect of adding types  ---------------------------- #
# bg2 <- round(bg2*10) #+ 1     # the logic of this arithmetic is explained in line 76
# bg22 <- round(bg22*10) #+ 1
# bg222 <- round(bg222*10) #+ 1
# bg4 <- round(bg4*10) #+ 1
# bg44 <- round(bg44*10) #+ 1
# bg444 <- round(bg444*10) #+ 1

bg2 <- rowMeans(bg2)          # calculate the mean of the N(nits) generalisation probabilities for each similarity category
bg22 <- rowMeans(bg22)        # for 11 condition
bg222 <- rowMeans(bg222)      # for 111 condition etc.,
bg4 <- rowMeans(bg4)
bg44 <- rowMeans(bg44)
bg444 <- rowMeans(bg444)

vals <- as.data.frame(cbind(test, bg2, bg22, bg222, bg4, bg44, bg444))           # combine the similarity category values with generalisation probs for each condtiion
vals <- gather(vals, key = "genCond", value = "genProb", bg2:bg444)   # gather so each row is a different generalisation probability

trains <- c(train1, train2, train3)        # collate the training stimulus values
trains <- as.data.frame(trains)

trains1 <- c(train1, train2, train3, train1a, train2a, train3a)
trains1 <- as.data.frame(trains1)
trains3 <- c(train1, train2, train3, train1a, train2a, train3a, train1b, train2b, train3b, train1c, train2c, train3c)
trains3 <- as.data.frame(trains3)

gg2 <- ggplot(vals[1:9,], aes(x = test, y = genProb)) +     # plot predicted generalisation ratings
  geom_point(aes(x = test, y = genProb, group = genCond, colour = genCond), size = 2) +
  geom_line(aes(colour = genCond, linetype = genCond), size = 1.4 ) +
  theme_bw(base_size = 12) +
  labs(title = "2 Token presentations of each type", x = "Stimulus value", y = "Generalisation Probability") +
  scale_x_continuous(limits = c(min(trains), 1), breaks = seq(.4, 1, by = .1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
  scale_color_manual(name = "Number of \ntypes", labels = c("1", "2", "3"), values = c("red", "coral1", "seagreen")) +
  scale_linetype_manual(name = "Number of \ntypes", labels = c("1", "2", "3"), values = c("solid", "longdash", "dotdash")) +
  geom_point(data = trains1, aes(x = trains1, y = 1)) +    # show training stimulus values
  geom_vline(xintercept = test, linetype = "dashed", colour = "blue")  +
  theme(legend.position = c(.75, .75))
gg2

#bg444 exceeds 10, so doesn't show up

gg4 <- ggplot(vals[10:18,], aes(x = test, y = genProb)) +     # plot predicted generalisation ratings
  geom_point(aes(x = test, y = genProb, group = genCond, colour = genCond), size = 2) +
  geom_line(aes(colour = genCond, linetype = genCond), size = 1.4 ) +
  theme_bw(base_size = 12) +
  labs(title = "4 Token presentations of each type", x = "Stimulus value", y = "Generalisation Probability") +
  scale_x_continuous(limits = c(min(trains), 1), breaks = seq(.4, 1, by = .1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
  scale_color_manual(name = "Number of \ntypes", labels = c("1", "2", "3"), values = c("red", "coral1", "seagreen")) +
  scale_linetype_manual(name = "Number of \ntypes", labels = c("1", "2", "3"), values = c("solid", "longdash", "dotdash")) +
  geom_point(data = trains3, aes(x = trains3, y = 1)) +    # show training stimulus values
  geom_vline(xintercept = test, linetype = "dashed", colour = "blue")  +
  theme(legend.position = c(.75, .75))
gg4

plot_grid(gg2, gg4, labels = "AUTO", ncol = 2)  # plot types and tokens graphs next to each other

# --------------- calculate sum of squares errors ---------------- #
partMeans <- read.csv("./csv/means_exp2.csv")

totMeans <- cbind(partMeans, vals)
totMeans$genProb <- totMeans$genProb*10

totMeans$sse <- (totMeans$meanResp - totMeans$genProb)^2
totMeans$genProb <- ifelse(totMeans$genProb < 1, 1, totMeans$genProb)  # because minimum rating = 1
# sum(totMeans$sse)   # for model comparison, comparing theta model vs. pIgnore model

ggscatter(totMeans, x = "genProb", y = "meanResp",
          add = "reg.line", conf.int = TRUE, cor.coef = FALSE, cor.method = "pearson",
          xlab = "Model Predictions", ylab = "Mean Participant Ratings", main = "B. Experiment 2") +
  coord_cartesian(xlim = c(1,9)) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(limits = c(1,9), breaks = 1:9)

corr <- correlationBF(x = totMeans$genProb, y = totMeans$meanResp)
corr
# a <- (totMeans$meanResp - totMeans$genProb)^2
# sum(a)
