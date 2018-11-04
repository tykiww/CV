---
layout: post
title:  "Monte Carlo Methods: Bootstrapping"
image: "http://needtoknow.nas.edu/energy/media/media_browser_uploads/images/item_1_.jpg"
date:   2017-08-05
excerpt: "Bootstrapping in R"
project: true
---


We oftentimes undermine the power that non-parametric methods have in predicting probabilities of outcomes. Sometimes, we just don't have enough data to find out a confidence estimate and interval of an outcome. Other times, we lack information to perform accurate hypothesis tests of statistical significance. These situations usually lead to needing models to figure out likelihood estimates of certain outcomes with the limited information we have!

Fortunately, we have the monte carlo method to give us a good gist. Monte carlo sampling is a technique that allows for estimates to follow a pattern of the law of large numbers, creating a central limit distribution. In other words, MC takes a random sample of certain events (estimates) over and over and over and over again to obtain numerical events from uncertainty. Monte Carlo sampling is very useful when determining confidence estimates from non-parametric distributions. As long as we are familiar with a 'true' proportion or a tendency of the estimate, we can create confidence estimates.

![](https://thumbs.gfycat.com/InbornTerrificAvocet-size_restricted.gif)

One type of monte carlo sampling is the bootstrap technique (this is something that I went deeper when I covered [random forest models](https://tykiww.github.io/2017-04-05-rf-model/) in the past). Bootstrapping is one of the most useful procedures in determining confidence interval parameter estimates and is a sampling technique with replacement. This means, that every time an object is sampled, it is replaced back with the same probability of independent occurrence. This is especially useful when we have known/estimated probabilities of certain outcomes and we are testing with limited amounts of data. Bootstrapping has been known to be valuable in dealing with optimization when there may be heteroscedasticity  (errors are not normally distributed) and distributional assumptions may not be met.

In short, it's a ridiculously useful technique to create more data when we have just partial information. Well then, let's take a look at some coding!

===================================

At first, I was going to take a look at some success/failure outcomes (ie. binomial distribution), but I thought it might be nicer to look at more complex situations.



Here's a hypothetical situation where the bootstrap & monte carlo technique may be useful:

  - Suppose your company has 4 sales areas that have a history of closing a certain probability of deals. Let's call these divisions A, B, C, and D. 

  - With phone rates being 2 cents a call, the manager was worried that their area selections are becoming obsolete and that they are going to lose revenue.

  - Because past data has shown that at least 4 percent of people who answer calls buy your product (On average, one sale makes about $199), the manager has decided that they wanted to re-evaluate how costly each phone call would be and see how many calls it will take to get at least one of every single area to respond. 

  - After repeated data collection, we have observed that each division has respective probabilities of c("A" = 0.10,"B" = 0.25,"C" = 0.15,"D" = 0.40, "E" = .775) people answering phone calls. E stands for the average compliment of these probabilities, so it would  be the probability of no one answering.


**What is our estimated profit/per call?**

**Are these collective areas profitable before all fixed and variable costs (taxes, overhead, and employee compensation)?**

**Is this a valid measure of area performance?**

Alright, let's get going already. First, I'll call out the beloved `dplyr` package to help me with this function. I honestly don't need it, but it makes this code a little more fun to work with. Well, since we are looking at how many tries it takes until we succeed on getting at least one call from each area. This is a great example of testing out four different geometric distributions! For this problem, instead of starting off each individual area and performing an rnbinom, I decided to stick them all into one sample and bootstrap it. I decided to do this because I wanted to see what the estimated profit per call was if we were calling each area at once. Here, bootstrapping actually allows us to resample, independently, without replacement, thus bringing the probability of getting at least one of each call closer to the true values (0.10, 0.25, 0.25, 0.40). 

I also toyed with changing the boolean *TRUE* to a character and character matching twice -once with `grepl` to produce the T, second by using `matches` to match the TRUE with itself. These were marked with a counter until the while loop marked every area as TRUE.

This procedure is run n times indicating the total number of monte carlo samples to go through to perform the count for how many calls it takes for us to get an answer from all four areas.

```r
library(dplyr)

numcall4 <- function(n=100) {
tha.calls <-c()
for (j in 1:n) {
  count <- 0
  A <- FALSE
  B <- FALSE
  C <- FALSE
  D <- FALSE
  E <- FALSE
  while (!(A & B & C & D)) {
    count <- count + 1
    num.box <- sample(c("A", "B", "C", "D", "E"), size = 1, replace = TRUE, prob = c(0.10, 0.25, 0.15, 0.40, 0.775))
      # just for fun, I did a character match with the boolean TRUE
    if (0 < sum(matches(match = "TRUE",vars = as.character(grepl("A",num.box))))) {
      A <- TRUE
    } else if (0 < sum(matches(match = "TRUE",vars = as.character(grepl("B",num.box))))) {
      B <- TRUE
    } else if (0 < sum(matches(match = "TRUE",vars = as.character(grepl("C",num.box))))) {
      C <- TRUE
    } else if (0 < sum(matches(match = "TRUE",vars = as.character(grepl("D",num.box))))) {
      D <- TRUE
    } else
      E <- FALSE # doesn't do anything.
  }
  tha.calls[j] <- sum(count)
}

tha.calls # about 20
}
yup <- mean(numcall4())
hist(numcall4(), main = "Histogram of Bootstrapped Calls")
c("mean # of calls" = yup)
```

![](https://tykiww.github.io/img/boot/bootfix1.png)

Output

    ## mean count of calls 
    ##           25.61 

The histogram shows a negative binomial pattern with a low probability and an estimate of about 25. If you're trying to replicate, don't worry if your histogram looks slightly diffferent from mine. Now that we have a function that gives us a MC estimate of the number of calls it takes, next we can assess the profitability of each outcome and check the confidence interval of that estimate.

```r
# Vectorize values
sale.prob <- .04 # probability of completing a sale/total answered phone calls.
ppc <- .02 # price per call
avrev <- 199 # average revenue
nReps <- 100 # number of repetitions

```

I previously tried this with 1000 bootstrap samples with 1000 monte carlo repetitions, but I realized that this was going to take way too long and I had to end the code early. More points are always nice for bootstrap sampling as our n increases, our p(power to reject the null | Ho) increases.
I put in a timer to see how long it would take with just 100x100. It should roughly be 100 times shorter than what I was trying before!

```r
ptm <- proc.time()  # Start the clock!

check <- c()
av_calls <- c()
for (i in 1:nReps) {
av_calls[i]<- mean(numcall4())
check[i] <- sale.prob*av_calls[i]*avrev - av_calls[i]*ppc
}

proc.time() - ptm   # Stop the clock

length(check) # Just to see if I have 100 elements in the vector
# t/s = to succeed
c("mean calls t/s"=mean(av_calls), "mean short-profit t/s"=mean(check))
```

      ##                user  system elapsed 
      ##            30.212   0.213  30.924 
      ## mean calls t/s mean short-profit t/s 
      ##        22.5900              179.3646 
      
Oh good, it only took about 30 seconds. That's not bad. No wonder 1000x1000 took me forever: That would have taken me about (100)30/60 which would be roughly 50 min. I have a rather nice mac that runs fairly fast. I wonder how my CPU really compares with other machines.

Each `check` value corresponds to a monte carlo element of the short-term profit that we would earn taking into account the average revenue minus the price per call. It seems like, on average, we make about 179.37 dollars after successfully getting at least one call from  each of the four areas. Now, dividing that by the average number of calls gives us about 7.94 dollars on average per-call.

```r
ave_calls <- mean(av_calls)
short_profit <- mean(check)
c("profit/call" = short_profit/ave_calls )
# this means we make $ 7.94 on average per call that goes through to all 4 areas.
```

    ## profit/call 
    ## 7.94 
    
So, in essence, if we were trying to make sure to randomly hit all 4 areas, we would expect to make 7.94 per call that goes through and 179.37 on average if they made at least (about) 23, calls randomly until they hit all 4 areas. 

Let's take a look at how confident we are about this estimate.

```r
hist(check,main = "Histogram of short-term profit")
mc.err <- quantile(check,c(.025,.975))
abline(v=mc.err,col='red')

c(mc.err[1],"estimate"=short_profit,mc.err[2])
```

    ##      2.5% estimate    97.5% 
    ##  158.0437 179.3646 200.494


![](https://tykiww.github.io/img/boot/bootfix2.png)

After taking a look at the histogram of the data, we can be quite certain that 95% of the estimates that we have created can be expressed by (158.0437, 200.494). In other words, we are 95% confident that the true mean short-profit is captured in this interval. I guess our confidence interval is rather wide with a margin of error of about 21 dollars. For better estimates, we should probably perform even more bootstrap samples to narrow up the confidence interval.

*See how useful this is??* I guess another step I could add is comparing the data to a monte carlo assesed without area "A" as it had the lowest probability outcome. If I was a manager, I would take a comparison and drop one of the areas and focus on a section that yields more revenue. Another option is raising or lowering the price on the product we're selling depending on the price elasticity of demand. It may seem intuitive that if we drop A that we would make even more money, but we never know with such low probabilities... I would do this by specifying the prob of "A"=0 in the vector above and comparing estimates by hypothesis testing. (Ho: No difference when removing area A, Ha: There is a difference). 

Depending on the time, focus, and energy of the employees we could make an important cost-cutting decision to increase our profits!

I guess there were a few things that I could have done better from this analysis. This is NOT by all means a procedure that accurately predicts the exact profits that will be gained by the company. I guess this model is more of a prescriptive measure than a predictive measure. Yet, if we have data enough to compare different set groups, we will be able to make important and informed decisions for what to do. Another important thing to consider, is that this model is probably **_only useful when the probabilities of certain areas are very similar and hard to decipher just by intuition_**. If area D had a probability of .80 for calls received, I would most definitely drop A and possibly C and/or D.

I hope this reflects, at least, some of my abilities of effective data use in an applied setting.

Thank you for patiently reading (:


