#############################################################################
##                                                                         ## 
##          Project:   Parallel Analysis example                           ##
##                                                                         ##
##-------------------------------------------------------------------------##
##          Programmer:    Ou Zhang                                        ##
##          Request Date:  4-13-2020                                       ##
##          Initial Code:  4-13-2020                                       ##
##          Goals:   To provide code for parallel analysis                 ##
##          Input:  (internal data from package)                           ##
##          Output: output from r console                                  ##
##          Note: 2 packages can operate parallel analysis (paran, psych)  ##
##-------------------------------------------------------------------------##
##          Modification History:                                          ##
##          When:                                                          ##
##          Who:                                                           ##
##          Change:                                                        ##
##-------------------------------------------------------------------------##
## Step 1: Set work directory
rm(list=ls())

## Step 2: load required packages 
packages <- c("paran","psych","GPArotation","tidyverse",
              "ggplot2","psychTools","MASS")
packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }  
})

## Step 3: Set up key libraries and source code
proj.path = file.path("c:/temp ");
data.path = file.path(proj.path,"data/");
out.path = file.path(proj.path,"out/");

setwd(proj.path)

## ------- Start Example --------##

# -----  Example 1: package - paran --- #
data("USArrests")

## perform a standard parallel analysis on the US Arrest data 
# using 50% percentile.
paran(USArrests, iterations=5000)

## a conservative analysis with different result (95% and 99%)
paran(USArrests, iterations=5000, centile=95)


## ---- Example 2--------  ##
p <- 20 # number of variables 

# The limiting case of lambda in PCAs of the 
# correlation matrix of uncorrelated data
for (n in c(100, 1000, 1000000)) {
  u <- matrix(rnorm(n*p),n,p)
  lambda_u <- u %>% 
    cor() %>% eigen(, only.values = TRUE)
    cat("For n = ", n, ", Lambda_U (PCA) = \n", sep="")
    print(lambda_u$values)
  cat("\n")
}

#  The limiting case of lambda in PCAs of the variance-covariancematrix 
#  of uncorrelated data with differing variances
for (n in c(100, 1000, 1000000) ) {
  std <- runif(n=p, min=1, max=50) %>% sqrt()
  
  u <- matrix(rnorm(n*p, sd=std), 
              n, p, byrow=TRUE)
  
  lambda_u <- u %>% 
    cor() %>% eigen(, only.values = TRUE)
  cat("For n = ", n, ", Lambda_U (PCA) = \n", sep="")
  print(lambda_u$values)
  cat("\n")
}


# The limiting case of lambda in CFAs of uncorrelated data
for (n in c(100, 1000, 1000000)) {
  u <- matrix(rnorm(n*p), n, p)
  tmp_u <- u %>% 
    cor() %>% 
    ginv() %>% 
    diag() %>%
    diag() %>% 
    ginv() %>% 
    eigen(, only.values = TRUE)
    
  lambda_u <- u %>% 
    cor() - tmp_u$values
  cat("For n = ", n, ", Lambda_U (CFA) = \n", sep="")
  print(lambda_u)
  cat("\n")
}

## -------------- Example 2 from Rachael Smyth &Andrew Johnson ----------------#
data("bfi")

# Describe data
describe(bfi[1:25])
sum(complete.cases(bfi[1:25]))

# Bartlett's Test of Sphericity
# The most liberal test is Bartlett's test of sphericity - 
# this evaluates whether or not the variables intercorrelate
# at all, by evaluating the observed correlation matrix against an "identity matrix"
# If this test is not statistically significant, you should not
# employ a factor analysis.
cortest.bartlett(bfi[1:25])

# Bartlett's test was statistically significant, suggesting that 
# the observed correlation matrix among the items is
# not an identity matrix.

# --- Determining the Number of Factors to Extract --- #

# Test 1: Scree Plot
bfi[,1:25] %>% scree()

# Test 2: Parallel Analysis
bfi[,1:25] %>% fa.parallel()

## -------------- Example 3 from Sakaluk & Short (2016) ----------------#
# Sakaluk, J. K., & Short, S. D. (2016). A Methodological Review of Exploratory 
# Factor Analysis in Sexuality Research: Used Practices, Best Practices, 
# and Data Analysis Resources. Journal of Sex Research.

# STEP 1: Read-in data
dat = msq
keys = make.keys(msq[1:75],list(
  EA = c('active', 'energetic', 'vigorous', 'wakeful', 'wide.awake', 'full.of.pep',
         'lively', '-sleepy', '-tired', '-drowsy'),
  TA =c('intense', 'jittery', 'fearful', 'tense', 'clutched.up', '-quiet', '-still',
        '-placid', '-calm', '-at.rest') ,
  PA =c('active', 'excited', 'strong', 'inspired', 'determined', 'attentive',
        'interested', 'enthusiastic', 'proud', 'alert'),
  NAf =c('jittery', 'nervous', 'scared', 'afraid', 'guilty', 'ashamed', 'distressed',
         'upset', 'hostile', 'irritable' ),
  HAct = c('active', 'aroused', 'surprised', 'intense', 'astonished'),
  aPA = c('elated', 'excited', 'enthusiastic', 'lively'),
  uNA = c('calm', 'serene', 'relaxed', 'at.rest', 'content', 'at.ease'),
  pa = c('happy', 'warmhearted', 'pleased', 'cheerful', 'delighted' ),
  LAct = c('quiet', 'inactive', 'idle', 'still', 'tranquil'),
  uPA =c( 'dull', 'bored', 'sluggish', 'tired', 'drowsy'),
  naf = c( 'sad', 'blue', 'unhappy', 'gloomy', 'grouchy'),
  aNA = c('jittery', 'anxious', 'nervous', 'fearful', 'distressed'),
  Fear = c('afraid' , 'scared' , 'nervous' , 'jittery' ) ,
  Hostility = c('angry' , 'hostile', 'irritable', 'scornful' ),
  Guilt = c('guilty' , 'ashamed' ),
  Sadness = c( 'sad' , 'blue' , 'lonely', 'alone' ),
  Joviality =c('happy','delighted', 'cheerful', 'excited', 'enthusiastic', 'lively', 'energetic'), Self.Assurance=c( 'proud','strong' , 'confident' , '-fearful' ),
  Attentiveness = c('alert' , 'determined' , 'attentive' )
  #acquiscence = c('sleepy' , 'wakeful' , 'relaxed','tense')
))

# obtain efa data
msq.scores = scoreItems(keys,msq[1:75])
efa.data = msq.scores$scores

set.seed(123)
parallel = fa.parallel(efa.data,      # specify our data frame;
                       fm = 'ml',     # indicate that we want to estimate eigenvalues using maximum likelihood 
                       fa = 'fa',     # indicate that we only want the CF eigenvalues 
                       n.iter = 50,   # indicate that 50 times simulation for our parallel analysis;
                       SMC = TRUE,    # indicate that we want to use squared multiple correlations (SMCs)
                       quant = .95)   # specify that we would like the 95th quantile



# ---  Making a Pretty Scree Plot with Parallel Analysis Using ggplot2 --- # 
# STEP 1: Create data frame from observed eigenvalue data
obs <- data.frame(parallel$fa.values)
obs$type <- c('Observed Data')
obs$num <- c(row.names(obs))
obs$num <- as.numeric(obs$num)
colnames(obs) <- c('eigenvalue', 'type', 'num')

# STEP 2: Calculate quantiles for eigenvalues, 
# but only store those from simulated CF model in percentile1
percentile <- apply(parallel$values, 2, function(x) quantile(x, .95))
min <- as.numeric(nrow(obs))
min <- (4 * min) - (min - 1)
max <- as.numeric(nrow(obs))
max <- 4 * max
percentile1 <- percentile[min:max]

# Create data frame called &amp;amp;amp;amp;amp;quot;sim&amp;amp;amp;amp;amp;quot; with simulated eigenvalue data
sim <- data.frame(percentile1)
sim$type <- c("Simulated Data (95th %ile)")
sim$num <- c(row.names(obs))
sim$num <- as.numeric(sim$num)
colnames(sim) <- c("eigenvalue", "type", "num")

# Merge the two data frames (obs and sim) together into data frame called eigendat
eigendat <- rbind(obs, sim)

# Apply ggplot2
apatheme <- theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    text = element_text(family = "Arial"),
    legend.title = element_blank(),
    legend.position = c(.7, .8),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black")
  )

# Use data from eigendat. Map number of factors to x-axis, eigenvalue to y-axis, and give different data point shapes depending on whether eigenvalue is observed or simulated
p <- ggplot(eigendat, aes(x = num, y = eigenvalue, shape = type)) +
  # Add lines connecting data points
  geom_line() +
  # Add the data points.
  geom_point(size = 4) +
  # Label the y-axis 'Eigenvalue'
  scale_y_continuous(name = "Eigenvalue") +
  # Label the x-axis 'Factor Number', and ensure that it ranges from 1-max # of factors, increasing by one with each 'tick' mark.
  scale_x_continuous(name = "Factor Number", breaks = min(eigendat$num):max(eigendat$num)) +
  # Manually specify the different shapes to use for actual and simulated data, in this case, white and black circles.
  scale_shape_manual(values = c(16, 1)) +
  # Add vertical line indicating parallel analysis suggested max # of factors to retain
  geom_vline(xintercept = parallel$nfact, linetype = "dashed") +
  # Apply our apa-formatting theme
  apatheme
# Call the plot. Looks pretty!
p

# Save a high-res version of your plot, 
# and you've got yourself a pretty scree plot with parallel analysis:
ggsave('parallel.png', width=6, height=6, unit='in', dpi=300)



##  ----  EOF  ---- ##