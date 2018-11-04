---
layout: post
title:  "Dynamic Pricing: Chocolate"
image: "https://tykiww.github.io/assets/dynamic%20pricing/3.png"
date:   2018-05-05
excerpt: "Dynamic Pricing Model in R"
project: true
---


Here's a hypothetical situation we can whip up: As an international wholesale retail owner of a particular chocolate brand, your goal is to map the optimum price for your world-famous candy bar using historical data. You have collected data on the past years' price changes along with quantity information in response to these price adjustments. What will you do?

![](https://collectiveinnovation.com/wp-content/uploads/2016/02/Dynamic-Pricing.png)

Dynamic pricing is an extremely complex subject. So much so, it hurts to wrap my head around. There are so many different approaches when it comes to optimization. We want it just right!

Simply stated, dynamic pricing is a strategy businesses employ that adjusts prices based on the demand of the market.

Various approaches are used, and they are all seemingly different. Today, we will be looking at revenue and cost information gleaned from the demand of customers. This process is different from what I have traditionally studied. We will use a more bayes-esque approach by using a known 'linear' demand curve. Instead of using timeseries analysis, we can model customer behavior using this known distribution. How do we code this up? Fortunately, R allows for rather easy vizualization to bring some theory to life. 

Here are the packages we will use today. I actually don't think any of them are too necessary to do this analysis. Yet, if you are following along with my code, please load them!

```r
library(dplyr)
library(tidyverse)
```

Data is gathered from an international wholesale retail store and is available in my [repository](https://github.com/tykiww/projectpage). It looks at the quantity of a chocolate bar/thing sold at the specific price range across all the stores. Unfortunately, these chocolates are different size bars, so we are not controlling for the same exact product, but if you take a glance at the product itself, they are technically 'the same'.

I'll begin by running the data from my repository, then cleaning it up.

```r
url <- "https://raw.githubusercontent.com/tykiww/projectpage/master/datasets/chocolate/Online%20Retail.csv"
hm <- read_csv(url)
# cleaning up the chocolate data. 
# Parsing outliers and limiting range of data.
hm <- subset(hm,hm$UnitPrice>1 & hm$UnitPrice < 10)
dat <- aggregate(hm$Quantity,list(hm$UnitPrice),sum)
dat <- subset(dat,dat$x<2000)
names(dat) <- c("price","quantity")

raw <- as.data.frame(dat)
```

We will operate under the assumption that our demand is linear (even thought it isn't really). Regardless, there is supposed evidence that it is ['good enough'](https://www0.gsb.columbia.edu/faculty/azeevi/PAPERS/pwr_simple_models_ob_az.pdf), but what do I know? (Not much, but I do know that the basis for all measurement of curvature began in the greek days of Eudoxus and Archimedes and was refined by Newton by the creation of calculus). Therefore, we will be working with our y = ßx + a + error model from a regression! This will be our posterior model that we will refer to towards the end to make our estimations on the best possible price.

```r
# Begin by finding our estimate values.
lm(quantity ~ price,raw) %>%
  summary
```

    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   502.32      88.72   5.662 1.31e-06 ***
    ## price         -65.18      19.20  -3.394  0.00154 ** 
    
We notice here that our price ß is -65.18 and 502.32 our alpha. Our demand curve will look like this.

```r
ggplot(raw, aes(price, quantity)) +
  geom_point(shape=1) +
  geom_smooth(method='lm') +
  ggtitle('Chocolate Demand')
```

[](https://tykiww.github.io/assets/dynamic%20pricing/1.png)

We can now create a demand curve function using the information we have that will help us with forecasting our historical demand. It's pretty easy from just modeling our regression equation. Since we are creating a random normal distribution of errors, we also need to make sure to set our seed.

```r
set.seed(15)
sd(raw$price) # 2.286543
demand <- function(x, b = -65.18, a = 502.32, sd = 2.286543) {
  err <- rnorm(length(x), sd = sd)
  y <- x*b + a + err
  y
}
```

Now, unfortunately, we do not have historical price information. I actually realized this just now! Yet, do not fear. *We can just make it up.* I know this isn't the best idea, but if we did happen to have the historical price information, it wouldn't be difficult to model it off of what I have here. We'll just create a years' worth of daily prices that fluctuate under a normal distribution. It is possible that we may run into some negative floats (maybe two? Nah, probably just one), but our standard deviation is rather small so I am not counting on it. Also, since we have already modeled our demand function after our data, our predictions will just be off by the factor of having different historical prices.

Now we are here, but we have to make sure to get our equations correct. These are the ones we will be using:

    ## Historical Revenue = Historical demand * Historical price
    ## Historical Cost = unit price * historical demand.
    ## Historical Profit = ( Historical Price - Historical Cost ) * demand

Note here that we are using our demand to calculate revenue. This may not be up to accounting standards, but it actually won't be too off. The principle of demand seems to apply in this situation. Our r-squared value is at 20%, but we will have to make do. As long as we know that our linear model is significant, we should not be having too many problems. 

Another note is that we are going to set our manufacturer's unit price of the candy bar to be 50 cents and fluctuate it with the same demand as the chocolate bar. Furthermore, we are not accounting for any variable costs that may arise. Needless to say, our margin will be rather big with or without them with in the end. So many things to keep in mind.

```r
# Obtaining historical price (i)
summary(raw$price) # mean = 4.029

price.hist <- rnorm(365, mean = 4.029)
price.hist <- price.hist  # Keeping the minimum the same

# Getting our historical demand
demand.hist <- demand(price.hist) 
demand.hist <- demand.hist + 50 # adjusting demand

# Grabbing historical Revenue
revenue.hist <- price.hist*demand.hist

# Making up cost per bar (i)
unit.cost <- 0.5

# Here are the historical costs
cost.hist <- unit.cost*demand.hist 

# Now is our historical profits
profit.hist <- (price.hist - unit.cost)*demand.hist 

# creating dataframe to get ready to plot.
views <- data.frame('Period' = seq(1,365),'Daily.Prices' = price.hist,
                  'Daily.Demand' = demand.hist, 'Daily.Revenue' = revenue.hist,
                  'Daily.Cost' = cost.hist, 'Daily.Profit' = profit.hist)

```

Now, we can graph out our cost, profit, and revenues that we have randomly created. Let's map them out according to the last year.

```r
ggplot(views, aes(Period, Daily.Revenue, colour = 'Historical Revenue')) +
  geom_line() +
  geom_line(aes(Period, Daily.Profit, colour = 'Historical Profit')) +
  geom_line(aes(Period, Daily.Cost, colour = 'Historical Cost')) +
  labs(title = '365 Day Performance', colour = '')
```

![](https://tykiww.github.io/assets/dynamic%20pricing/2.png)

We can definitely see the randomness! I don't think any company actually works like this. Some of the revenue on these days are so low. Pretty surreal situation, but anything is possible. It would have been nice to have real data as that would model the business setting. Regardless, we have a balance here.

Now, we will do a regression of the above data.

```r
lm.fit <- lm(demand.hist ~ price.hist)
summary(lm.fit)
```

    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 790.61982    0.34005    2325   <2e-16 ***
    ## price.hist  -65.20197    0.06056   -1077   <2e-16 *** 

To find the optimum price at any level, we use this linear model that we created to optimize our profits! This will be part of our estimates for optimization. We'll first initialize and tuck them away for use later.

```r
# initializing info
intercept <- lm.fit$coefficients[1] 
p.est <- lm.fit$coefficients[2]  
p.rev <- -intercept/(2*p.est) # estimated revenue price
p.prof <- (p.est*unit.cost - intercept)/(2*p.est) # estimated profit price
```

We'll next set up functions that map out our 'true revenue' and 'true profits' that come from our original chocolate data.

```r
# True curves
true.rev <- function(p) p*(-65.18*p + 502.32) # Revenue from original linear model data.
true.prof <- function(p) (p - unit.cost)*(-65.18*p + 502.32) # price from original data.
```

Also, let's create functions that reflect our new estimates from our fake random data that we created earlier.

```r
# Estimated curves
est.rev <- function(p) p*(lm.fit$coefficients[2]*p + lm.fit$coefficients[1])
est.prof <- function(p) (p - unit.cost)*(lm.fit$coefficients[2]*p + lm.fit$coefficients[1])
```

Now that we have our set-up, let's get all of our functions mapped out! I decided here to use ggplot. You could also use the base R plots and specify `add = TRUE` if you like. ggplot graphics are the best!

```r
ggplot(data = data.frame(Price = 0)) +
  stat_function(fun = true.rev, mapping = aes(x = Price, color = 'True Revenue')) +
  stat_function(fun = true.prof, mapping = aes(x = Price, color = 'True Profit')) +
  stat_function(fun = est.rev, mapping = aes(x = Price, color = 'Estimated Revenue')) +
  stat_function(fun = est.prof, mapping = aes(x = Price, color = 'Estimated Profit')) +
  scale_x_continuous(limits = c(0, 11)) +
  scale_y_continuous(limits = c(-50, 1500)) +
  labs(title = 'True curves w/o Error') +
  ylab('Results') +
  scale_color_manual(name = "", values = c("True Revenue" = 2, "True Profit" = 3, "Estimated Revenue" = 4, "Estimated Profit" = 6)) 
```

![](https://tykiww.github.io/assets/dynamic%20pricing/3.png)

We observe: Our estimated profits for chocolates are extremely close to our historical true profits! According to the timeseries model, this next year's profits will probably be slightly higher than our past 'true profits'. In this case, we would set our prices in between 4-5 dollars for the candy bar (what an expensive piece of candy!). It seems, due to the elasticity of demand, that it is in our favor to raise the price higher than what our current profits show as our new estimates will supposedly yield higher revenue and higher profits!

(Actually, if you didn't catch on earlier, you may have noticed that I added 50 to our historical demand earlier. This was because our estimated profits and true profit curves were going to overlap each other due to overfitting. I wanted to be able to visualize the curves in the right way. Technically, our data is corrupt because of the random sampling and inflated demand.)

If we had real historical price changes over the span of a year, we may have had completely different results.. Yet it still works! Given a situation where you have to make dynamic price changes throughout the year, you could predict this next year's price! I would love to apply something of this nature in a product/retail side of business. It seems rather exciting to me.

I hope you are able to enjoy one of the methods of dynamic pricing. This estimation technique allows us to glance at historical trends in our time series, uses a bayes-esque approach to predict the future prices, and reflect new estimates in line with our old data. Please try this out when you actually have some good info! It's rather easy. All you need is a price-quantity set with historical price and cost changes. I will hopefully be looking into more application examples!!
