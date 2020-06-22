
# ------------------------------------------------------------------------- #
# --------- Adding types, but not tokens, affects property induction ------ #
# ----------------- Modeling Experiment 3 conditions ---------------------- #
# -------------------------------------------------------------------------- #

require(cowplot)
require(tidyverse)
require(BayesFactor)
require(ggpubr)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("./BayesGen-BX.R")
set.seed(1)

# ------------ PARAMETERS ----------------------------- #
phi <- 1                 # phi = 1: uniform prior over hypotheses, phi > 1: greater prior belief in larger hypotheses
nits <- 1000             # number of iterations

# --------------------------- generate predictions ------------------------------ #
parits <- 3000  # how many times do I want to test out different parameter values?

test <- c(.49, .7, .9)  # this is overridden in l. 50
nt <- length(test)      # number of test items

namescol <- c("typelabel", "tokenlabel")
decreaseHigh <- matrix(data = c(rep(0, parits*2)), nrow = parits)
colnames(decreaseHigh) <- namescol

decreaseLow <- decreaseMed <- decreaseHigh

pars <- c("thetaType", "thetaToken", "target1", "target2", "target3", "target4", "target5", "target6", "typesd", "tokensd")
parValues <- matrix(data = c(rep(0, parits*length(pars))), nrow = parits)
colnames(parValues) <- pars

# --------------------------- generate predictions ------------------------------ #
# use a loop to test different parameter values
for (a in 1:parits) {
  
  # sample different parameter values
  # theta values to represent sampling assumptions/informational value
  thetaType <- runif(1, min = .1, max = .5)          # value used to discuss modeling results = .3
  thetaToken <- runif(1, min = .1, max = thetaType)  # value in paper = .15 
  # stimulus values for types
  targetsM <- runif(1, .2, .6) # select the centre of a uniform distribution, from which to sample types
  targetsW <- runif(1, .005, .2)  # select the width of a uniform distribution, from which to sample types
  
  targets <- runif(6, targetsM - targetsW, targetsM + targetsW) # sample from that uniform distribution
  targets <- sort(targets)
  target1 <- targets[4]  # value in paper = .452
  target2 <- targets[3]                   # .45
  target3 <- targets[1]                   # .446
  target4 <- targets[2]                   # .448
  target5 <- targets[5]                   # .454
  target6 <- targets[6]                   # .456
  
  # when randomly generating stimulus values for types/tokens, how much standard deviation to use?
  typesd <- runif(1, .01, .05)            # .03
  tokensd <- typesd/3                     # .01, > experiment 1 (.009) because images are now reflected/rotated
  
  bg1token <- matrix(data = c(rep(0, nits*nt)),   # create enough 0s for each iteration x each test item - to be filled with predicted generalisation probability
                   nrow = nt)                   # 1 row for each test item (high, medium, low sim)
  bg2token <- bg3token <- bg4token <- bg5token <- bg1token # make all others in the same way - need one matrix per condition
  bg1type <- bg2type <- bg3type <- bg4type <- bg5type <- bg1token
  
    for( i in 1:nits ) {     # average out predictions over nits iterations
      
      train1 <- rnorm(1, mean = target1, sd = typesd) # generates number randomly from a uniform distribution, then /.2 makes it smallish, then +.4 to approach .5
    
      # generalisation from adding OLD tokens 
      train1a <- rnorm(1, mean = train1, sd = tokensd)  # the repetition is constrained to be within a range around the first training item
      train1b <- rnorm(1, mean = train1, sd = tokensd)  # the above is true for all tokens
      train1c <- rnorm(1, mean = train1, sd = tokensd)
      train1d <- rnorm(1, mean = train1, sd = tokensd)
      train1e <- rnorm(1, mean = train1, sd = tokensd)
    
      # testH <- (max(c(train1, train1a, train1b, train1c, train1d, train1e)) + .01)
      # test <- c(testH, testH+((.9-testH)/2), .9)
      test <- c(.51, .707, .9)
      
      bg1token[,i] <- (BayesGen(train1, test, thetaType, phi))*1/2 +
                        (BayesGen(c(train1a), test, thetaToken, phi))*1/2
      bg2token[,i] <- (BayesGen(train1, test, thetaType, phi))*1/3 +
                        (BayesGen(c(train1a, train1b), test, thetaToken, phi))*2/3
      bg3token[,i] <- (BayesGen(train1, test, thetaType, phi))*1/4 +
                        (BayesGen(c(train1a, train1b, train1c), test, thetaToken, phi))*3/4
      bg4token[,i] <- (BayesGen(train1, test, thetaType, phi))*1/5 +
                        (BayesGen(c(train1a, train1b, train1c, train1d), test, thetaToken, phi))*4/5
      bg5token[,i] <- (BayesGen(train1, test, thetaType, phi))*1/6 +
                        (BayesGen(c(train1a, train1b, train1c, train1d, train1e), test, thetaToken, phi))*5/6
      
      # generalisation from adding NEW tokens 
      train1v <- rnorm(1, mean = target2, sd = typesd)  # the repetition is constrained to be within a range around the first training item
      train1w <- rnorm(1, mean = target3, sd = typesd)  # the above is true for all tokens
      train1x <- rnorm(1, mean = target4, sd = typesd)
      train1y <- rnorm(1, mean = target5, sd = typesd)
      train1z <- rnorm(1, mean = target6, sd = typesd)
    
      testHnew <- (max(c(train1, train1v, train1w, train1x, train1y, train1z)) + .01)
      testnew <- c(testHnew, testHnew+((.9-testHnew)/2), .9)
      
      # adding type-label isntances
      bg1type[,i] <- BayesGen(c(train1, train1v), test, thetaType, phi) 
      bg2type[,i] <- BayesGen(c(train1, train1v, train1w), test, thetaType, phi)  
      bg3type[,i] <- BayesGen(c(train1, train1v, train1w, train1x), test, thetaType, phi) 
      bg4type[,i] <- BayesGen(c(train1, train1v, train1w, train1x, train1y), test, thetaType, phi)
      bg5type[,i] <- BayesGen(c(train1, train1v, train1w, train1x, train1y, train1z), test, thetaType, phi) 
    }

  bg1token <- rowMeans(bg1token)          # calculate the mean of the N(nits) generalisation probabilities for each similarity category
  bg2token <- rowMeans(bg2token)
  bg3token <- rowMeans(bg3token)
  bg4token <- rowMeans(bg4token)
  bg5token <- rowMeans(bg5token)

  bg1type <- rowMeans(bg1type)          # calculate the mean of the N(nits) generalisation probabilities for each similarity category
  bg2type <- rowMeans(bg2type)
  bg3type <- rowMeans(bg3type)
  bg4type <- rowMeans(bg4type)
  bg5type <- rowMeans(bg5type)

  decreaseHigh[a,1] <- bg1type[1] > bg5type[1]
  decreaseHigh[a,2] <- bg1token[1] > bg5token[1]

  decreaseMed[a,1] <- bg1type[2] > bg5type[2]
  decreaseMed[a,2] <- bg1token[2] > bg5token[2]

  decreaseLow[a,1] <- bg1type[3] > bg5type[3]
  decreaseLow[a,2] <- bg1token[3] > bg5token[3]
  
  parValues[a,] <- c(thetaType, thetaToken, target1, target2, target3, target4, target5, target6, typesd, tokensd)
  
}
# parValues

decreaseHigh  # false when training items are quite high (>.5)?
decreaseMed
decreaseLow

sum(decreaseHigh[,1]) # adding type-label instances increases gen at high-sim category
sum(decreaseHigh[,2]) # adding token-label instances increases gen at high-sim category

sum(decreaseMed[,1]) # adding type-label instances increases gen at med-sim category
sum(decreaseMed[,2]) # adding token-label instances increases gen at med-sim category

sum(decreaseLow[,1]) # adding type-label instances increases gen at low-sim category
sum(decreaseLow[,2]) # adding token-label instances increases gen at low-sim category

# # old repetitions
# vals <- as.data.frame(cbind(test, bg2old, bg3old, bg4old, bg5old, bg6old)) # combine the similarity category values with generalisation probs for each condtiion
# vals <- gather(vals, key = "genCond", value = "genProb", bg2old:bg6old)   # gather so each row is a different generalisation probability
# # vals$genProb <- ifelse(vals$genProb > 10, 10, vals$genProb)
# 
# trainsOld <- c(train1, train1a, train1b, train1c, train1d, train1e)
# trainsOld <- as.data.frame(trainsOld)
# 
# ggOld <- ggplot(vals, aes(x = test, y = genProb)) +     # plot predicted generalisation ratings
#   geom_point(aes(x = test, y = genProb, group = genCond, colour = genCond), size = 2) +
#   geom_line(aes(colour = genCond, linetype = genCond), size = 1.4) +
#   theme_bw(base_size = 12) +
#   labs(title = "Adding token-label instances", x = "Stimulus value", y = "Generalisation Probability") +
#   scale_x_continuous(limits = c(min(trainsOld), 1), breaks = seq(.4, 1, by = .1)) +
#   scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
#   scale_color_manual(name = "Number of \ninstances", labels = c("1", "2", "3", "4", "5"), values = c("red", "coral1", "seagreen", "blue", "orchid")) +
#   scale_linetype_manual(name = "Number of \ninstances", labels = c("1", "2", "3", "4", "5"), values = c("solid", "longdash", "dotdash", "dotted", "twodash")) +
#   geom_point(data = trainsOld, aes(x = trainsOld, y = 1)) +    # show training stimulus values
#   geom_vline(xintercept = test, linetype = "dashed", colour = "blue")  +
#   theme(legend.position = c(.85, .7))
# ggOld
# 
# # trainsOld <- c(train1, train1a, train1b, train1c, train1d, train1e)
# # trainsOld <- as.data.frame(trainsOld)
# 
# 
# # new repetitions
# valsNew <- as.data.frame(cbind(test, bg2new, bg3new, bg4new, bg5new, bg6new))
# valsNew <- gather(valsNew, key = "genCond", value = "genProb", bg2new:bg6new)   # gather so each row is a different generalisation probability
# valsNew$genProb <- ifelse(valsNew$genProb > 10, 10, valsNew$genProb)
# 
# trainsNew <- c(train1, train1v, train1w, train1x, train1y, train1z)
# trainsNew <- as.data.frame(trainsNew)
# 
# ggNew <- ggplot(valsNew, aes(x = test, y = genProb)) +     # plot predicted generalisation ratings
#   geom_point(aes(x = test, y = genProb, group = genCond, colour = genCond), size = 2) +
#   geom_line(aes(colour = genCond, linetype = genCond), size = 1.4) +
#   theme_bw(base_size = 12) +
#   labs(title = "Adding type-label instances", x = "Stimulus value", y = "Generalisation Probability") +
#   scale_x_continuous(limits = c(min(trainsNew), 1), breaks = seq(.4, 1, by = .1)) +
#   scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
#   scale_color_manual(name = "Number of \ninstances", labels = c("1", "2", "3", "4", "5"), values = c("red", "coral1", "seagreen", "blue", "orchid")) +
#   scale_linetype_manual(name = "Number of \ninstances", labels = c("1", "2", "3", "4", "5"), values = c("solid", "longdash", "dotdash", "dotted", "twodash")) +
#   geom_point(data = trainsNew, aes(x = trainsNew, y = 1)) +    # show training stimulus values
#   geom_vline(xintercept = test, linetype = "dashed", colour = "blue")  +
#   theme(legend.position = c(.85, .7))
# ggNew
# 
# plot_grid(ggNew, ggOld, labels = "AUTO", ncol = 2)  # plot types and tokens graphs next to each other
# 
# # --------------- calculate sum of squares errors ---------------- #
# partMeans <- read.csv("./csv/means_exp3.csv")
# colnames(valsNew)[1] <- "test"
# predMeans <- rbind(vals, valsNew)
# 
# totMeans <- cbind(partMeans, predMeans)
# totMeans$genProb <- totMeans$genProb*10
# 
# totMeans$sse <- (totMeans$meanResp - totMeans$genProb)^2
# totMeans$genProb <- ifelse(totMeans$genProb < 1, 1, totMeans$genProb)  # because minimum rating = 1
# 
# # sum(totMeans$sse)   # for model comparison, comparing theta model vs. pIgnore model
# 
# ggscatter(totMeans, x = "genProb", y = "meanResp",
#           add = "reg.line", conf.int = TRUE, cor.coef = FALSE, cor.method = "pearson",
#           xlab = "Model Predictions", ylab = "Mean Participant Ratings", main = "C. Experiment 3") +
#   coord_cartesian(xlim = c(1,9)) +
#   scale_x_continuous(breaks = 1:9) +
#   scale_y_continuous(limits = c(1,9), breaks = 1:9)
# 
# corr <- correlationBF(x = totMeans$genProb, y = totMeans$meanResp)
# corr