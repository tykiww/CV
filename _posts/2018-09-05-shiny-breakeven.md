---
layout: post
title:  "Simple Breakeven Analysis Using Shiny"
image: "https://raw.githubusercontent.com/tykiww/tykiww.github.io/master/img/br-shiny/breakevens.png"
date:   2018-09-05
excerpt: "Breakeven Analysis in Shiny using R"
project: true
---


At what point of revenues, monthly or annually, do you need to sell in order to cover your costs of business? Are these margins satisfying to you? Is this even feasible? 

![](https://s3.amazonaws.com/lowres.cartoonstock.com/food-drink-sandwich-nutritional_diet-brown_bread-sandwich_bar-sandwich_selection-tcln3_low.jpg)

Let's imagine you want to start a small business opening up a sandwich shop in Seattle, WA. You are allured by the homely atmosphere and long to create that "second home" for all those travelling to and fro. To accomplish this plan, you decide that you need to define 3 things to see if this is even a feasible task (this is way oversimplified, but let's just leave it be for the sake of this post).

  1. Price
  2. Figure your operating expenses and variable costs
  3. Break Even point
  4. Other economical/market barriers
  
After all this, I will show you what we have done using a shiny dashboard! So, if you get board reading, make sure to skip towards the end!

1. To determine your price, you could do 2 things: Cost-based pricing OR Price-based costing. 

  - Cost Based Pricing looks first at your costs, then picks a desired contribution margin that you want to stick to, then determines your costs.
  - Price Based costing looks at the customer demand, then adjusts accordingly.
  
Here, we will initially start with the Price-based option by looking at our competitors. If anything changes, we will move the average cost of our meal accordingly. To do this, 2e'll check out online reviews for likely prices. On [yelp](https://goo.gl/JkTzrL) we notice a total of 403 registered shops with:

```r
barplot(c("$"=237,"$$"=165,"$$$"=1), col = "steel blue", xlab = "Price Range", ylab = "# Stores", main = "Yelp Sandwich Shop Market Estimation")
```

![](1.png)

    $ : 237 : $8 (under 10)
    $$ : 165 : $18 (11-30)
    $$$: 1 : $42 (30-60)

Let's say that you are interested in entering the "double dollar" market. It seems like a reasonable price. Might be a bit risky as it is most likely not inundated with too many people, and less competition. However, we do not know all the details. For now, we will set our price at $18 and continue to do our math.

2. We will move on to applying our total costs accross the organization. 

  - Building is roughly $1.50M _after interest_ paying off 5% per annum (20 years to pay off)
    - rest of the overhead comes up to be about $100,000 per year ($2,000,000 in total fixed)
    - No down payment! You made the dumb decision to put your house up for collateral..
  - Variable costs (food and compensation) $10 / sandwich. (Let's say your chefs and hands are salaried.)

3. Now our breakeven point is a measure that describes "how many units do I need to sell until my sales reaches that total amount of costs I owe?" This can be calculated in 2 ways.


$$ \textrm{Breakeven Point} =  \frac {\textrm{Total Fixed Cost}} {\textrm{Unit Price} - \textrm{Unit Cost}}$$
  $$ \textrm{Breakeven Point} =  \frac {\textrm{Total Fixed Cost}}{\frac {\textrm{Contribution Margin per Unit}} {\textrm{Sales Price}} }$$

Of course, there's actually more ways to do this but we will choose the first since we already have the information. 

```r
2000000/(18-10)
```

    ## [1] 250000

Hmm this is quite a heft sum! **250,000** units sold and we are breaking even. That seems feasible, but a challenge. That means yearly we need to sell to about **12500** units per year. Break that down into months, **1040** meals per month. Might seem a bit tough for a botique sandwich shop.

One thing to realize is that they are profiting. $225,000 in revenue. Yet, to pay it off in the long run is now dependent on how the marketing, opearation, and maintenance costs will go. Doable? Might be tough for the first few years, but not impossible to pull it off. I would get a cheaper location or lower the fixed costs.

4. After surveying the location, customers, marketing strategy and other potential barriers (really none other than the competition) you decide you are good to go! You decided that your target audience wasn't here just for the food, but for a second home away from their work and other distractions. You wanted them to stay for longer, creating a homely environment (probably need to be less ambitious on the unit goals). You decided then to put in a $300,000 down payment and found a different location for a bit cheaper. Good luck!

<hr>

Here is a representation of what we just calculated into a simple dashboard using the shiny app! I have been recently messing around with the code and it has been a bit of a steep learning curve, but it was finally figured out! You are welcome to either run the code, visit this [page](https://inform-analytics.shinyapps.io/breakeven/), or just view it in the window below.

```r
# Breakeven Run Script
library(shiny)
library(plotly)
library(tidyverse)

source(file = "https://raw.githubusercontent.com/tykiww/projectpage/master/Shiny/BreakEven%20Server.R")
source(file = "https://raw.githubusercontent.com/tykiww/projectpage/master/Shiny/BreakEven%20UI.R")

shinyApp(ui = ui, server = server)
```
<iframe src="https://inform-analytics.shinyapps.io/breakeven/" style="width:750px; height: 750px;">
<embed src="https://inform-analytics.shinyapps.io/breakeven/" style="width:750px; height: 750px;">

If you look carefully, our calculations for this example exceed the dimension of the plot. However, you are welcome to play around with it using different numbers! It's a bit of a fun project that was created on the side. Later, we'll be working on how to actually create these dashboards, but that won't be for a little while. 

Visualization is important. Being able to see your calculations will become an integral part of the transforming world. Putting up graphs is one way of doing this, creating interactive plots is another. Why don't we just merge the two and create an interactive dashboard for everyone to use?

Hope you enjoyed.
