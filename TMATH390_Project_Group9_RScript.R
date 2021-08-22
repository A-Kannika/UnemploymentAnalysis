################################################################################
# TMATH 390 Project Group 9                                                    #
# Analysis of Unemployment Rate: Data by County                                #
# in Washington State and Oregon State from March 2021                         #
# Armstrong Kannika, and Jessie De Jesus                                       #
# June 4, 2021                                                                 #
# Prof. M Kennedy                                                              #
################################################################################

# Download the data file and read the data into R
## change the working directory to the directory that you save the data in.
setwd("/Users/akannika/Documents/UWT/Spring 2021/390/TMATH390_Project")

## use read.csv, assuming your data is a csv file named TMATH390Data.csv
data.df = read.csv("TMATH390_Project_data_Group9.csv")

## QQ Plot code
## modify the graphing 2 windows before plot the data
par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
qqnorm(data.df$unemploymentRate[data.df$UrbanOrRural=="Urban"], 
       ylim = c(4, 11),
       xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", 
       main = "Urban Area",
       cex.main = 1, font.main = 1,
       col = "#6495ED", pch = 19)
qqline(data.df$unemploymentRate[data.df$UrbanOrRural=="Urban"])
qqnorm(data.df$unemploymentRate[data.df$UrbanOrRural=="Rural"], 
       ylim = c(4, 11),
       xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", 
       main = "Rural Area",
       cex.main = 1, font.main = 1,
       col = "#6495ED", pch = 19)
qqline(data.df$unemploymentRate[data.df$UrbanOrRural=="Rural"])
mtext("Unemploymented Rate by County\n in Washington State and Oregon State in March 2021", 
      outer = TRUE, cex = 1, font = 2 )

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>> QQ Plot summary <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<#
# The QQ plot show that the distribution of the unemployment rate in the      #
# urban area and the rural area in WA and OR in March 2021 follow the normal  #
# distribution. But the slope of the QQ plot in the rural area is slightly    #
# different from the urban area, but not much, So the standard deviation of   #
# both areas are not too difference.                                          #
###############################################################################

## Box plot
## modify the graphing one window before plot the data
par(mfrow = c(1, 1), mar = c(3, 3, 5, 0.5), mgp = c(2, 0.5, 0), las = 1)
boxplot(data.df$unemploymentRate[data.df$UrbanOrRural=="Urban"],
        data.df$unemploymentRate[data.df$UrbanOrRural=="Rural"], names = c("Urban Area","Rural Area"),
        data = data.df, main = "Unemploymented Rate by County\n in Washington State and Oregon State in March 2021",  
        xlab = "County Area Location", ylab = "Unemploymented Rate (%)", 
        col=c("#138D75","#7B241C"))$stats 

## Boxplot stats
#      [,1] [,2]
# [1,] 4.50  4.6  # Lower whisker
# [2,] 5.90  6.3  # Q1
# [3,] 6.75  7.4  # Q2 (median)
# [4,] 7.10  8.1  # Q3
# [5,] 8.80 10.5  # Upper whisker
## attr[,1] = Urban, and attr[,2] = Rural
## range of Urban area is  8.8-4.5 = 4.3, IQR = Q3-Q1 = 7.1-5.9 = 1.2
## range of Rural area is 10.5-4.6 = 5.9, IQR = Q3-Q1 = 8.1-6.3 = 1.8

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Boxplot summary <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<#
# - The center of boxplot plot show that the the distribution of unemployment #
# rate in the rural area is higher than the urban area. This is supported by  #
# the numeric measure. The median of unemployment rate's distribution in the  #
# urban area (6.75) is lower than the rural area (7.4). Actually, it should   #
# be noted that even the third quartile of unemployment rate's distribution   #
# in the urban area (7.1) is lower than the median of unemployment rate's     #
# distribution in the rural area. We therefore conclude that general,         #
# unemployment rate in the urban area is lower than the rural area.           #
# - The spread of the boxplot, judging by the range of the data, show that    #
# there is more variability in the unemployment rate's distribution in the    #
# rural area (range = 5.9) than there is in the unemployment rate's           #
# distribution in the urban area (range = 4.3). On the other hand, if we look #
# at the IQR, which measures the variability only among the middle 50% of the #
# distribution, we see more spread in the unemployment rate in the rural area #
# (IQR = 1.8) than the unemployment rate in the urban area (IQR = 1.2).       #
###############################################################################

## Histogram
par(mfrow=c(1,2),oma = c(0, 0, 2, 0))

# Histogram for Urban Area
# Add a Normal Curve
x1 = data.df$unemploymentRate[data.df$UrbanOrRural=="Urban"]
h<-hist(x1, main = "Urban Area",  xlab = "Unemploymented Rate (%)", xlim = c(4, 11),
        ylab = "Frequency (Counties)", ylim = c(0, 14),col = c("#138D75"), font.main = 1)
xfit<-seq(min(x1),max(x1),length=40) 
yfit<-dnorm(xfit,mean=mean(x1),sd=sd(x1)) 
yfit <- yfit*diff(h$mids[1:2])*length(x1) 
lines(xfit, yfit, col="blue", lwd=2)

# Histogram for Rural Area
# Add a Normal Curve
x2 <- data.df$unemploymentRate[data.df$UrbanOrRural=="Rural"]
h<-hist(x2, main = "Rural Area",  xlab = "Unemploymented Rate (%)", xlim = c(4, 11),
        ylab = "Frequency (Counties)", ylim = c(0, 14),col = c("#7B241C"), font.main = 1) 
xfit<-seq(min(x2),max(x2),length=40) 
yfit<-dnorm(xfit,mean=mean(x2),sd=sd(x2)) 
yfit <- yfit*diff(h$mids[1:2])*length(x2) 
lines(xfit, yfit, col="blue", lwd=2)

mtext("Histogram of Unemploymented Rate by County\n in Washington State and Oregon State in March 2021", 
      outer = TRUE, cex = 1, font = 2 )

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Histogram summary <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<#
# The histogram show that the center of the unemployment rate's distribution   #
# in the urban area is less than the rural area, and the spread is notably     #
# different. The unemployment rate in the urban area fall between 4.5 - 9      #
# while the unemployment rate in the rural area that range is 4 - 11. But the  #
# histogram and the plot show that the distribution of unemployment rate in    #
# both area follow the normal distribution.                                    #
###############################################################################


# summaries of the data; x1 = Urban, x2 = rural
summary(x1)
summary(x2)

# summary(x1)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.500   5.900   6.750   6.612   7.100   8.800 
#
# summary(x2)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.60    6.30    7.40    7.33    8.10   10.50 

# Standard deviation
sd(x1)  # 1.001108
sd(x2)  # 1.413389

# sample size
length(x1) # 52
length(x2) # 23

# The degrees of freedom for the case of two independent samples with unequal p
# opulation variances is (Welch’s formula):
welch.fn<-function(s1,s2,n1,n2)
{
  return(floor((s1^2/n1+s2^2/n2)^2/((s1^2/n1)^2/(n1-1)+(s2^2/n2)^2/(n2-1))))
}
# Example, define the values
s1=1.0 # sample standard deviation from sample 1
s2=1.41# sample standard deviation from sample 2
n1=52 # sample size from sample 1
n2=23 # sample size from sample 2
# Call the function to return the df
welch.fn(s1,s2,n1,n2)

# df = 32

# 2-sample t-test
t.test(x1, x2, mu = 0, conf.level = 0.95)

#>>>>>>>>>>>>>>>>>>>>>>>> Welch Two Sample t-test <<<<<<<<<<<<<<<<<<<<<<<<<<<<<#
# data:  x1 = urban and x2 = rural                                             #
# t = -2.2067, df = 32.164, p-value = 0.03459                                  #
# alternative hypothesis: true difference in means is not equal to 0           #
# 95 percent confidence interval:                                              #
#  -1.38234320 -0.05544944                                                     #
# sample estimates:                                                            #
#  mean of x mean of y                                                         #
#   6.611538  7.330435                                                         #
#------------------------------------------------------------------------------#
# Report the values:                                                           #
# • The observed value of the test statistic: t = -2.2067                      #
# • The Welch’s degrees of freedom: df = 32                                    #
# • The p-value: p-value = 0.03459                                             #
# • The confidence interval for the difference between the means.              #
# 95 percent confidence interval: between -1.38234320 and -0.05544944          #
################################################################################

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Conclusion <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<#
# We are 95% confident that the true difference in mean of the unemployment    #
# rate in the urban area counties and the rural area counties, in Washington   #
# State and Oregon State, in March 2021 is somewhere between -1.3832 and       #
# -0.554. Since the interval does not include 0, so we have evidence that the  #
# true mean values do differ.                                                  #
# Since the p-value is 0.0346, less than the significance level, which is 0.05,#
# we have evidence to reject the null hypothesis. We can therefore conclude    #
# that the true difference in mean of the unemployment rate in the urban area  #
# counties and the rural area counties, in Washington State and Oregon State,  #
# in March 2021 are different.                                                 #
# The result of this research shows that the unemployment rate, in March 2021, #
# in the urban area counties is lower than in the rural area counties in       #
# Washington state and Oregon State. However, the data used in this research   #
# is only one month data. There are many factors that we can use to analyze    #
# the economic performance between the rural area and the urban area in        #
# Washington State and Oregon State.                                           #
################################################################################