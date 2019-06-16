---
layout: post
title:  "Let's scrape Amazon with RSelenium"
image: "https://tykiww.github.io/assets/selenium/3.png"
# date:   2018-04-05
excerpt: "Scraping Amazon with RSelenium in R"
project: true
---

You know those "I am not a robot" confirmation boxes? They always get in the way, especially when I'm trying to grab large amounts of information from google searches. Previously, I primarily scraped web content through rvest. Fast and pretty reliable, but I would often come to a wall. Then came Selenium. When I first heard what it was, I had no idea how it worked. Huh? How does a web app testing software do scraping? It took forever to figure out how to use this application (I had to learn it in python first, then translate it in R).

In layman terms, web app testing is used for application developers to see the efficacy of the application by automatically testing the usefulness of the app. Yet, the process for pointing and clicking is done by identifying css or xml tags (you see where I am headed? If not, it's okay. You should start with my post learning how to use [rvest](https://tykiww.github.io/2017-07-05-luhn-with-rvest/)). From that point, grabbing the information is pretty easy. Yet, the best part about selenium is this: __It acts as if someone was just pointing and clicking from your computer rather than a bot flipping through the internet__. Hence, less likely to get shut down!

Of course, R may not be the easiest nor the best way to use the Selenium server (there seems to be better methods with other IDEs. ie. Firefox Selenium IDE) but I'll show you how I figured it out.

We'll get started right away.

Unfortunately, since I last loaded the package, it has become defunct. I found out later it was because the dependencies `binman` and `wdman` were deleted from the CRAN server due to "checks" problems. Don't ask me too many details, because I am no any expert. Good thing we can access the archived RSelenium and old dependencies.

```r
library(devtools)
### install_version("binman", version = "0.1.0", repos = "https://cran.uni-muenster.de/")
### install_version("wdman", version = "0.2.2", repos = "https://cran.uni-muenster.de/")
### install_version("RSelenium", version = "1.7.1", repos = "https://cran.uni-muenster.de/")
library("RSelenium")
```

If you are super picky about the "proper" way to use things, there is yet another administrative task we have to go through in order to use RSelenium. You also need to get our hands on Docker, since rsDriver is not currently working. To solve this, make sure to go  and download Docker from this [site](https://store.docker.com/) after creating an account. It's free. Don't worry. Yet, I am not really in the mood to run through two separate ways to run Selenium, so I will show you how I use this. If you're interested in using Docker, check out this [cool resource](https://callumgwtaylor.github.io/blog/2018/02/01/using-rselenium-and-docker-to-webscrape-in-r-using-the-who-snake-database/) with animal species analysis using Rselenium. Slightly different from my intentions.

We initially will begin by using the `rsDriver()` function with the correct port and browser information. Usually, as a developer tool, Firefox is the main, but since I bought this new laptop I have not once touched it. I suggest if you are following along, use Chrome browser to access Selenium. 

```r
browse <- rsDriver(port=4444L,browser="chrome")$client
```

Now that we have our browser, let's start playing! Here's a demonstration with our beloved google. This is what a split view of what will pop up as soon as you type in the url. 

```r
url <- "https://google.com/ncr"
browse$navigate(url)
```

![](https://raw.githubusercontent.com/tykiww/imgbucket/master/assets/selenium/1.png)

One important aspect to note is unique ability of Selenium to act as if we are actually pointing and clicking on the information, so sites will not suspect us to be 'bots'. Even when we are automating any searches, we won't be knocked out for 'suspicious activity'. 

Of course there will be times when we may get blocked (I do all the time), but all we need to do is scan each page with an `if` `else` and if it so happens that a bot comes up, we can just make sure to click the appropriate box. For extreme cases, there may be a "find an image that has a car" type of dialog. Those might be more of a challenge, but I can imagine that this can be taken care of with tensorflow or some other visual machine learning tool. Who knows? For now, all I know is my limited ability to scrape the internet!

Anyways, back to our browser. Every step requires that we identify the exact element that we are searching for, then perform an action (Just like pointing and clicking). Unfortuantely, we can't just point and click (Aww..). Yet, luckily for us we have really great tools at our dispense. Our CSS selector gadjet from chrome (or Firefox) and the inspect element item. The CSS selector tool is probably my favorite. It is a chrome addon that I use all the time. Yould probably get by with only using the CSS selector because it has an Xpath searching functionality. Yet if we have both tools, we will have a much easier time getting what we need. I will not be showing you how to use the CSS selector here, but the internet has plenty of resources on how you can take care of that on your own!

Here, we see that our browse value has an option to `findElement()`. We will specify that we found the search box with the css selector, then paste in the CSS code. You will see the cursor move blink on the text box just after you run this line.

```r
wxbox <- browse$findElement(using = 'css selector',"#lst-ib")
```

Now to start typing, we will use the new object that we have created and use the option to `SendKeysToElement(list())`. Inside goes our search term. 

```r
name <- "Walmart"
wxbox$sendKeysToElement(list(name))
```

![](https://raw.githubusercontent.com/tykiww/imgbucket/master/assets/selenium/2.png)

You'll notice immediately that the screen will start to type on it's own and work just like you were online (You can also see what is trending with 'Walmart' ahaha).

To search, now, is just as easy. We just find the element that specifies the search button and we `$clickElement()`. Interestingly, you'll notice if you run the first line below, there may be a problem. It will tell you that the element you are looking for is not visible (thus impeding you from clicking the button). No worries! There are work arounds to these things. Just `findElement()` to a new location or just find the inside search button. Both of which is included below.

```r
# This is the original search button
browse$findElement(using = 'css selector', value = "center input")$clickElement()
# This is the tertiary search button
browse$findElement(using = 'css selector', value = "span input")$clickElement()
```

I've hit the jackpot with odd search items. The Florida man jailed story says, "Florida man jailed for stealing $1.16 worth of doughnuts from Walmart, deputies say". No comment.

![](https://raw.githubusercontent.com/tykiww/imgbucket/master/assets/selenium/3.png)

You can also pull out specific images or text from the page. This will usually require an xpath for ease. I have already gone and grabbed it, but if you are curious how to pull out the xpath, take a look at one of my older [posts](https://tykiww.github.io/2017-02-05-SLR-Plotly/).

Say we wanted to pull out the industry marker and explanation from this page. We will begin by finding the xpath for both of those items.

```r
xp1 <- '//*[@id="rhs_block"]/div/div[1]/div/div[1]/div[2]/div[1]/div/div[1]/div/div/div[2]/div[2]/span'
xp2 <- '//*[@id="rhs_block"]/div/div[1]/div/div[1]/div[2]/div[3]/div/div[1]/div/div/div/span[1]'
```

We'll then  move on to obtaining the specific element. Where we specified "css selector", we can fill two objects as industry and explanation and pull them out using `$getElementText()`.

```r
industry <- browse$findElement(using = 'xpath', value = xp1)
explanation <- browse$findElement(using = 'xpath', value = xp2)
item1 <- industry$getElementText()[[1]][1]
item2 <- explanation$getElementText()[[1]][1]
```

![](https://raw.githubusercontent.com/tykiww/imgbucket/master/assets/selenium/4.png)

Pretty simple huh? I'll just finish by column binding the strings, then we have ourselves a mini-dataset.

```r
c(name,item1,item2)
```

    ## [1] "Walmart" 
    ## [2] "Retail company"
    ## [3] "Walmart Inc. is an American multinational retail corporation that operates a chain of 
    ## hypermarkets, discount department stores, and grocery stores."

When I showed this to my brother, he said something along the lines of, "Can't you just do this with your mouse?" Which is true. He had me there. It seems like such a roundabout way to perform one simple task.

However, we cannot forget, this is for testing and automating web apps. Just last week, I was creating a summary report for a manager and he needed me to sort through information for over 700 Japanese companies and identify key industries that identified with our product markers. I wasn't ready at that moment to go through every page, so I decided to create a link of 400 companies, sorted them by industry (cutting them down to about 1/3) then automating summary articles for me to glaze over. What would have taken me about an hour or so, with diminishing returns, to weed through only took about 20-30 minutes. Cutting my time by half.

Learning productivity comes at a cost for time, but ends up as a valuable investment for your future.

This was just a simple introduction to Selenium. There are many more things you can do, but it's more fun to explore on your own!

At the bottom you'll see how selenium could be used to scrape a huge pile of customer review data on a specific cushion toilet seat from Amazon. I searched for the highest ratings to study why people like this particular brand, enough to find the most common reasons for loving their purchase. IF I happened to be creating the new top-selling 'super cushy toilet seat' I would love to know what kinds of preferences are out in the market. 

```r
# Begin Driver
browse <- rsDriver(port=4444L,browser="chrome")$client

# amazon cushy toilet seat reviews.
url <- "https://www.amazon.com/Mayfair-Toilet-Molded-Easy-Clean-Change/product-reviews/B00004T158/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews"
# opened up the comments page.
browse$navigate(url)
# Found 5 star reviews
xpath5 <- '//*[@id="histogramTable"]/tbody/tr[1]/td[1]/a'
fivestar <- browse$findElement(using = 'xpath', value = xpath5)
fivestar$clickElement()
# get base url
nurl <- fivestar$getCurrentUrl()[[1]][1]
ogurl <- nurl %>%
  substr(1,nchar(nurl)-1)

# set up for loop that goes through 50 pages and collects all 5 star ratings.
ll <- c()
rat <- list()
for ( i in 1:50) {
  ll[i] <- ogurl %>%
    paste(as.character(i),sep = "")
  browse$navigate(ll[i])
  ch <- browse$findElement(using = 'css selector', value ="#cm_cr-review_list" )
  rat[[i]]<- ch$getElementText()[[1]][1] %>%
    strsplit("\nReport abuse\n")
}

```

Hope you enjoyed this post. Thank you for reading!


