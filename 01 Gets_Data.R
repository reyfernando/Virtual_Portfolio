


############
##Analyzing Stocks
############

##Getting the financial data

##In this lab, you will learn more about the analysis of stock returns by using the R packages PerformanceAnalytics, zoo and tseries (click them for more information).
##In this first exercise, you will use the get.hist.quote function from the tseries package. 
##The function allows you to download historical financial data over the web. 
##The code on the right downloads the adjusted closing prices from the start of 1998 up to the end of 2009 
##from Yahoo! for the stocks with tickers VBLTX and FMAGX. 

##The code further specifies that the data should be of the zoo class which is indexed by time. 
##Since we deal with monthly data, it is a good idea to change the class of the time series index 
##with the as.yearmon() function.


# Load relevant packages
install.packages("PerformanceAnalytics")
install.packages("zoo")
install.packages("tseries")
library(PerformanceAnalytics);library(zoo);library(tseries);

# Get the monthly adjusted closing price data on VBLTX, FMAGX and SBUX from Yahoo! 
#using the tseries function get.hist.quote(). Set the sample to Jan 1998 through Dec 2009.

# Get the adjusted closing prices from Yahoo!
VBLTX_prices <- get.hist.quote(instrument="vbltx", start="1998-01-01",end="2009-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)
VBLTX_prices <- get.hist.quote(instrument="vbltx", start="1998-01-01",end="2009-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)

FMAGX_prices <- get.hist.quote(instrument="fmagx", start="1998-01-01",end="2009-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)

SBUX_prices <- get.hist.quote(instrument="sbux", start="1998-01-01",end="2009-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)

UBS_prices <- get.hist.quote(instrument="goog", start="1998-01-01",end="2009-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)


?get.hist.quote

# Change the class of the time index to yearmon which is appropriate for monthly data
# index() and as.yearmon() are functions in the zoo package  

index(VBLTX_prices) <- as.yearmon(index(VBLTX_prices))
index(FMAGX_prices) <- as.yearmon(index(FMAGX_prices))
index(SBUX_prices) <- as.yearmon(index(SBUX_prices))
# Inspect your data
start(SBUX_prices)
end(SBUX_prices)

plot(FMAGX_prices)

plot(VBLTX_prices)

plot(SBUX_prices)


all_prices <- merge(VBLTX_prices, FMAGX_prices, SBUX_prices)



log(all_prices[2:n,1]) - log(all_prices[1:(n-1),1])



all_prices[2:n]

#################
# Compounded Interest
################


# Compute continuously compounded 1-month returns
sbux_ccret <- log(sbux_prices_df[2:n, 1]) - log(sbux_prices_df[1:(n-1), 1])

# Assign names to the continuously compounded 1-month returns
names(sbux_ccret) <-sbux_df[2:n,"Date"]

# Show sbux_ccret

head(sbux_ccret)

diff(log(all_prices))

########################
##Calculating the returns

##In the previous exercise, we obtained the price series for multiple stocks. It is convenient to store these time series in a single variable before we start the analysis. You will use the merge() function to do that, since it merges series by column while taking into account the time index.
##Remember that the continuously compounded returns are defined as the difference between the log prices. Once all price series are merged by column, you can easily calculate the continuously compounded returns. Use the log() function to calculate the log prices and apply the diff() function to the object that contains the log prices to get the continuously compounded returns.
###############################


# The variables VBLTX_prices, FMAGX_prices and SBUX_prices are preloaded in your workspace

# Create merged price data

all_prices <- merge(VBLTX_prices, FMAGX_prices, SBUX_prices)

# Rename columns
colnames(all_prices) <- c("VBLTX", "FMAGX", "SBUX")

# Calculate cc returns as difference in log prices
all_returns = diff(log(all_prices))

# Look at the return data
start(all_returns)
end(all_returns)
colnames(all_returns) 
head(all_returns)


###Plotting financial data with PerformanceAnalytics

# 'all_returns' is preloaded in your workspace.

# Plot returns after using the PerformanceAnalytics function chart.TimeSeries().
# This function creates a slightly nicer looking plot than plot.zoo()
chart.TimeSeries(all_returns, legend.loc="bottom", main=" ") 

# The previous charts are a bit hard to read. The PerformanceAnalytics function
# chart.Bar makes it easier to compare the returns of different assets on the 
# same plot
chart.Bar(all_returns, legend.loc="bottom", main=" ")


# Cumulative return plot - must use simple returns (!) and not cc returns for this
# Use PerformanceAnalytics function chart.CumReturns()
simple_returns <- diff(all_prices)/lag(all_prices, k=-1);



chart.CumReturns(simple_returns, wealth.index = TRUE, geometric = TRUE, legend.loc = "topleft", colorset = (1:12), begin = c("first", "axis", main="Future Value of $1 invested"))







####################################
##Create graphical summary for a return series
###As a first step, the code on the right extracts the "core data" from the all_returns variable and stores it in the matrix return_matrix. "Extracting the core data" in this case means stripping off the index/time attributes and returning only the observations. Secondly, the graphical window is subdivided into two rows and two columns with the par(mfrow=c(2,2,)) command. Finally, the code generates a histogram, boxplot, density and qqplot of the return data. This allows you to visually assess the symmetry of the return distribution, whether or not there are fat tails, outliers, etc.
################################

# Create matrix with returns
return_matrix <- coredata(all_returns);
return_matrix[,"VBLTX"]


# Generate four panel plots
par(mfrow=c(2,2))

hist(return_matrix[,"VBLTX"],main="VBLTX",
     xlab="VBLTX", probability=T, col="slateblue1")
boxplot(return_matrix[,"VBLTX"],outchar=T, main="Boxplot", col="slateblue1")
plot(density(return_matrix[,"VBLTX"]),type="l", main="Smoothed density",
     xlab="monthly return", ylab="density estimate", col="slateblue1")
qqnorm(return_matrix[,"VBLTX"], col="slateblue1")
qqline(return_matrix[,"VBLTX"])
par(mfrow=c(1,1))



SBUX

par(mfrow=c(2,2))

hist(return_matrix[,"SBUX"],main="SBUX",
     xlab="SBUX", probability=T, col="slateblue1")
boxplot(return_matrix[,"SBUX"],outchar=T, main="Boxplot", col="slateblue1")
plot(density(return_matrix[,"SBUX"]),type="l", main="Smoothed density",
     xlab="monthly return", ylab="density estimate", col="slateblue1")
qqnorm(return_matrix[,"SBUX"], col="slateblue1")
qqline(return_matrix[,"SBUX"])





?coredata
#############################
##Have a look at the plot. The distribution of returns on VBLTX looks symmetric but show fatter tails than normal. The histogram and boxplot indicate a nice symmetric distribution centered near zero with a SD around 0.025 and with a few positive and negative outliers. The QQ-plot is nice and linear in the middle but deviates in the tails.
##############################

###

##Return distribution comparison
##While the plot in the previous exercise allowed you to investigate the returns of a single stock, it is often of interest to compare several return series with each other. The code on the right does that by generating three boxplots that use the standard boxplot() function from base R. While that is okay, the PerformanceAnalytics package offers you the more convenient chart.Boxplot function.


# Create matrix with returns
return_matrix <- coredata(all_returns);

# Show boxplot of three series on one plot
boxplot(return_matrix[,"VBLTX"], return_matrix[,"FMAGX"], return_matrix[,"SBUX"],
        names=colnames(return_matrix), col="slateblue1")

# Do the same thing using the PerformanceAnalytics function chart.Boxplot



chart.Boxplot(all_returns, names = TRUE, as.Tufte = FALSE, sort.by = c(NULL, "mean",
                                                                       "median", "variance"), colorset = "black", symbol.color = "red",
              mean.symbol = 1, median.symbol = "|", outlier.symbol = 1,
              show.data = NULL, add.mean = TRUE, sort.ascending = FALSE,
              xlab = "Return", main = "Return Distribution Comparison",
              element.color = "slateblue1")


×
###Correct! The side-by-side boxplot is a very good way to quickly compare distributions. Here we see that the distribution of the bond fund, VBLTX, is the most concentrated about its mean (smallest standard deviation) and SBUX has the widest distribution.
#####
#####
#####Compute univariate descriptive statistics
#####
#####

# Note: all_returns is preloaded in your workspace

# Create matrix with returns
return_matrix <- coredata(all_returns);

summary(return_matrix)
return_matrix
# Compute descriptive statistics by column using the base R function apply()
args(apply)
apply(return_matrix, 2, mean)
apply(return_matrix, 2, var)
apply(return_matrix, 2, sd)

# A nice PerformanceAnalytics function that computes all of the relevant descriptive statistics is table.Stats

apply(return_matrix, 2, skewness)
apply(return_matrix, 2, kurtosis)
apply(return_matrix, 2, sd)

table.Stats(all_returns)



####Great! Have a look at the output above of table.Stats. These results are typical: individual stocks have the highest standard deviation, portfolios have smaller standard deviations than individual stocks, and bonds have the smallest standard deviations. All distributions are negatively skewed, and FMAGX and SBUX have larger negative returns than positive returns. Also, the excess kurtosis measures (R reports excess kurtosis, not regular kurtosis) for all are positive (around 2.5), indicating fatter tails than the normal.



# Note: return_matrix is preloaded in your workspace

# Annualized continuously compounded mean 
12*apply(return_matrix, 2, mean);
# Annualized simple mean
exp(12*apply(return_matrix, 2, mean)) - 1;

# Annualized standard deviation values

sqrt(12)*apply(return_matrix, 2, sd);





##Great work! Notice that the large annual SD for SBUX at 41% and the small annual SD for the bond fund at 9%.


######
###Bivariate graphical analysis
#####
#A graphical analysis can often help you to get a grasp on the co-movement between financial assets. The pairs() function generates all possible pair-wise scatter plots between series.
##To get a more formal grasp on the co-movement between assets, the covariance and correlation matrix of the returns can be computed by applying the var and cor on the return_matrix.


# Note: return_matrix is preloaded in your workspace

# Display all possible pair-wise scatter plots

pairs(return_matrix, col="slateblue1", pch=16)


# Compute 3 x 3 covariance and correlation matrices
var(return_matrix)
cor(return_matrix)





