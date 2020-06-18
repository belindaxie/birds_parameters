
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

# ------------ PARAMETERS ----------------------------- #
phi <- 1                 # phi = 1: uniform prior over hypotheses, phi > 1: greater prior belief in larger hypotheses
nits <- 1000             # number of iterations

# --------------------------- generate predictions ------------------------------ #
parits <- 10  # how many times do I want to test out different parameter values?

# -------------------------- fixed and free parameters ------------------------------------- #
# fixed (for now)
thetaDecrease <- .1
thetaOld <- .15# - thetaDecrease         # theta = 1 -> strong sampling04
thetaNew <- .3# - thetaDecrease

nits <- 1000          # number of iterations

test <- c(.49, .7, .9)  # this is overridden in l. 50
nt <- length(test)                                    # number of test items

bg2old <- matrix(data = c(rep(0, nits*nt)),   # create enough 0s for each iteration x each test item - to be filled with predicted generalisation probability
              nrow = nt)                   # 1 row for each test item (high, medium, low sim)
bg3old <- bg4old <- bg5old <- bg6old <- bg2old # make all others in the same way - need one matrix per condition
bg2new <- bg3new <- bg4new <- bg5new <- bg6new <- bg2old

target1 <- .452    
target2 <- .45   
target3 <- .446   
target4 <- .448  
target5 <- .454
target6 <- .456    
typesd <- .06      # when randomly generating stimulus values for types, how much standard deviation to use? 

oldsd <- .01         # same as expt1b

newsd <- .03           # should approximate sd of type
# --------------------------- generate predictions ------------------------------ #
for( i in 1:nits ) {     # average out predictions over nits iterations
  
  train1 <- rnorm(1, mean = target1, sd = typesd) # generates number randomly from a uniform distribution, then /.2 makes it smallish, then +.4 to approach .5

  # generalisation from adding OLD tokens 
  train1a <- rnorm(1, mean = train1, sd = oldsd)  # the repetition is constrained to be within a range around the first training item
  train1b <- rnorm(1, mean = train1, sd = oldsd)  # the above is true for all tokens
  train1c <- rnorm(1, mean = train1, sd = oldsd)
  train1d <- rnorm(1, mean = train1, sd = oldsd)
  train1e <- rnorm(1, mean = train1, sd = oldsd)

  # testH <- (max(c(train1, train1a, train1b, train1c, train1d, train1e)) + .01)
  # test <- c(testH, testH+((.9-testH)/2), .9)
  test <- c(.51, .707, .9)
  
  bg2old[,i] <- (BayesGen(train1, test, thetaNew, phi))*1/2 +
    (BayesGen(c(train1a), test, thetaOld, phi))*1/2
  bg3old[,i] <- (BayesGen(train1, test, thetaNew, phi))*1/3 +
    (BayesGen(c(train1a, train1b), test, thetaOld, phi))*2/3
  bg4old[,i] <- (BayesGen(train1, test, thetaNew, phi))*1/4 +
    (BayesGen(c(train1a, train1b, train1c), test, thetaOld, phi))*3/4
  bg5old[,i] <- (BayesGen(train1, test, thetaNew, phi))*1/5 +
    (BayesGen(c(train1a, train1b, train1c, train1d), test, thetaOld, phi))*4/5
  bg6old[,i] <- (BayesGen(train1, test, thetaNew, phi))*1/6 +
    (BayesGen(c(train1a, train1b, train1c, train1d, train1e), test, thetaOld, phi))*5/6
  
  # generalisation from adding NEW tokens 
  train1v <- rnorm(1, mean = target2, sd = newsd)  # the repetition is constrained to be within a range around the first training item
  train1w <- rnorm(1, mean = target3, sd = newsd)  # the above is true for all tokens
  train1x <- rnorm(1, mean = target4, sd = newsd)
  train1y <- rnorm(1, mean = target5, sd = newsd)
  train1z <- rnorm(1, mean = target6, sd = newsd)

  # testHnew <- (max(c(train1, train1v, train1w, train1x, train1y, train1z)) + .01)
  # testnew <- c(testHnew, testHnew+((.9-testHnew)/2), .9)
  
  # is the second token ignored or treated as a discrete exemplar?
  bg2new[,i] <- BayesGen(c(train1, train1v), test, thetaNew, phi) 
  bg3new[,i] <- BayesGen(c(train1, train1v, train1w), test, thetaNew, phi)  
  bg4new[,i] <- BayesGen(c(train1, train1v, train1w, train1x), test, thetaNew, phi) 
  bg5new[,i] <- BayesGen(c(train1, train1v, train1w, train1x, train1y), test, thetaNew, phi)
  bg6new[,i] <- BayesGen(c(train1, train1v, train1w, train1x, train1y, train1z), test, thetaNew, phi) 
}
bg2old <- rowMeans(bg2old)          # calculate the mean of the N(nits) generalisation probabilities for each similarity category
bg3old <- rowMeans(bg3old)
bg4old <- rowMeans(bg4old)
bg5old <- rowMeans(bg5old)
bg6old <- rowMeans(bg6old)

bg2new <- rowMeans(bg2new)          # calculate the mean of the N(nits) generalisation probabilities for each similarity category
bg3new <- rowMeans(bg3new)
bg4new <- rowMeans(bg4new)
bg5new <- rowMeans(bg5new)
bg6new <- rowMeans(bg6new)

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