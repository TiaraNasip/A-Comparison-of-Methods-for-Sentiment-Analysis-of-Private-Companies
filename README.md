# A-Comparison-of-Methods-for-Sentiment-Analysis-of-Private-Companies
Codes used for Master Thesis

#########################    1 DATA COLLECTION    #############################

## Collection of Tweets Data

# install.packages("rtweet")
library(rtweet)

# 26TH FEB 2021 - FRIDAY
# Keyword: Tesla stock/stocks
data=search_tweets( "Tesla stock",
                    n = 2000,
                    retryonratelimit = TRUE,
                    include_rts = FALSE,
                    since = "2021-02-26",
                    until = "2021-02-27",
                    quoted_text=FALSE, 
                    retweet_text=FALSE,
                    lang="en")

data2=search_tweets( "Tesla stocks",
                     n = 2000,
                     retryonratelimit = TRUE,
                     include_rts = FALSE,
                     since = "2021-02-26",
                     until = "2021-02-27",
                     quoted_text=FALSE, 
                     retweet_text=FALSE,
                     lang="en")

stock=rbind(data,data2)

# Keyword: Tesla share/shares
data3=search_tweets( "Tesla share",
                     n = 2000,
                     retryonratelimit = TRUE,
                     include_rts = FALSE,
                     since = "2021-02-26",
                     until = "2021-02-27",
                     quoted_text=FALSE, 
                     retweet_text=FALSE,
                     lang="en")

data4=search_tweets( "Tesla shares",
                     n = 1000,
                     retryonratelimit = TRUE,
                     include_rts = FALSE,
                     since = "2021-02-26",
                     until = "2021-02-27",
                     quoted_text=FALSE, 
                     retweet_text=FALSE,
                     lang="en")

share=rbind(data3,data4)

# Keyword: Tesla invest/invests/investing/invested
data5=search_tweets( "Tesla invest",
                     n = 2000,
                     retryonratelimit = TRUE,
                     include_rts = FALSE,
                     since = "2021-02-26",
                     until = "2021-02-27",
                     quoted_text=FALSE, 
                     retweet_text=FALSE,
                     lang="en")

data6=search_tweets( "Tesla invests",
                     n = 500,
                     retryonratelimit = TRUE,
                     include_rts = FALSE,
                     since = "2021-02-26",
                     until = "2021-02-27",
                     quoted_text=FALSE, 
                     retweet_text=FALSE,
                     lang="en")

data7=search_tweets( "Tesla investing",
                     n = 500,
                     retryonratelimit = TRUE,
                     include_rts = FALSE,
                     since = "2021-02-26",
                     until = "2021-02-27",
                     quoted_text=FALSE, 
                     retweet_text=FALSE,
                     lang="en")

data8=search_tweets( "Tesla invested",
                     n = 500,
                     retryonratelimit = TRUE,
                     include_rts = FALSE,
                     since = "2021-02-26",
                     until = "2021-02-27",
                     quoted_text=FALSE, 
                     retweet_text=FALSE,
                     lang="en")

invest=rbind(data5,data6,data7,data8)

# Keyword: Tesla trade/trades/trading/traded
data9=search_tweets( "Tesla trade",
                     n = 500,
                     retryonratelimit = TRUE,
                     include_rts = FALSE,
                     since = "2021-02-26",
                     until = "2021-02-27",
                     quoted_text=FALSE, 
                     retweet_text=FALSE,
                     lang="en")

data10=search_tweets( "Tesla trades",
                      n = 500,
                      retryonratelimit = TRUE,
                      include_rts = FALSE,
                      since = "2021-02-26",
                      until = "2021-02-27",
                      quoted_text=FALSE, 
                      retweet_text=FALSE,
                      lang="en")

data11=search_tweets( "Tesla trading",
                      n = 500,
                      retryonratelimit = TRUE,
                      include_rts = FALSE,
                      since = "2021-02-26",
                      until = "2021-02-27",
                      quoted_text=FALSE, 
                      retweet_text=FALSE,
                      lang="en")

data12=search_tweets( "Tesla traded",
                      n = 500,
                      retryonratelimit = TRUE,
                      include_rts = FALSE,
                      since = "2021-02-26",
                      until = "2021-02-27",
                      quoted_text=FALSE, 
                      retweet_text=FALSE,
                      lang="en")

trade=rbind(data9,data10,data11,data12)

# Keyword: Tesla buy/buys/buying/bought
data13=search_tweets( "Tesla buy",
                      n = 2000,
                      retryonratelimit = TRUE,
                      include_rts = FALSE,
                      since = "2021-02-26",
                      until = "2021-02-27",
                      quoted_text=FALSE, 
                      retweet_text=FALSE,
                      lang="en")

data14=search_tweets( "Tesla buys",
                      n = 500,
                      retryonratelimit = TRUE,
                      include_rts = FALSE,
                      since = "2021-02-26",
                      until = "2021-02-27",
                      quoted_text=FALSE, 
                      retweet_text=FALSE,
                      lang="en")

data15=search_tweets( "Tesla buying",
                      n = 500,
                      retryonratelimit = TRUE,
                      include_rts = FALSE,
                      since = "2021-02-26",
                      until = "2021-02-27",
                      quoted_text=FALSE, 
                      retweet_text=FALSE,
                      lang="en")

data16=search_tweets( "Tesla bought",
                      n = 500,
                      retryonratelimit = TRUE,
                      include_rts = FALSE,
                      since = "2021-02-26",
                      until = "2021-02-27",
                      quoted_text=FALSE, 
                      retweet_text=FALSE,
                      lang="en")

buy=rbind(data13,data14,data15,data16)

# Keyword: Tesla sell/sells/selling/sold
data17=search_tweets( "Tesla sell",
                      n = 1000,
                      retryonratelimit = TRUE,
                      include_rts = FALSE,
                      since = "2021-02-26",
                      until = "2021-02-27",
                      quoted_text=FALSE, 
                      retweet_text=FALSE,
                      lang="en")

data18=search_tweets( "Tesla sells",
                      n = 500,
                      retryonratelimit = TRUE,
                      include_rts = FALSE,
                      since = "2021-02-26",
                      until = "2021-02-27",
                      quoted_text=FALSE, 
                      retweet_text=FALSE,
                      lang="en")

data19=search_tweets( "Tesla selling",
                      n = 500,
                      retryonratelimit = TRUE,
                      include_rts = FALSE,
                      since = "2021-02-26",
                      until = "2021-02-27",
                      quoted_text=FALSE, 
                      retweet_text=FALSE,
                      lang="en")

data20=search_tweets( "Tesla sold",
                      n = 500,
                      retryonratelimit = TRUE,
                      include_rts = FALSE,
                      since = "2021-02-26",
                      until = "2021-02-27",
                      quoted_text=FALSE, 
                      retweet_text=FALSE,
                      lang="en")

sell=rbind(data17,data18,data19,data20)

data_day1=rbind(stock,share,invest,trade, buy, sell)

# drop duplicated tweets and tweet replies
data_day1=data_day1[!duplicated(data_day1$text),]

# save data
save(data_day1,file="data_day1.RData")

# These sets of codes are executed everyday until the 16th of April 2021

## Collection of stock price data (Yahoo Finance)

# install.packages("tidyquant")
# install.packages("BatchGetSymbols")
library(tidyquant)
library(BatchGetSymbols)
library(quantmod)

# 26/02/2021 - 11/04/2021

tsla <- tq_get("TSLA", get = "stock.prices", from="2021-02-26", to="2021-04-11")

sp500 <- tq_get("^GSPC", get = "stock.prices", from="2021-02-26", to="2021-04-11")
dji <- tq_get("^DJI", get = "stock.prices", from="2021-02-26", to="2021-04-11")
nasdaq <- tq_get("^IXIC", get = "stock.prices", from="2021-02-26", to="2021-04-11")


# building a new matrix combining the closing prices of all 4 components 
tsla=rename(tsla,TSLA=close)
sp500=rename(sp500,SP500=close)
dji=rename(dji,DJI=close)
nasdaq=rename(nasdaq, NASDAQ=close)

stockprices=cbind(tsla[,2],tsla[,6],sp500[,6],dji[,6],nasdaq[,6])

save(stockprices,file="stockprices.RData")

# 05/03/2021 - 17/04/2021

tsla <- tq_get("TSLA", get = "stock.prices", from="2021-03-05", to="2021-04-17")

sp500 <- tq_get("^GSPC", get = "stock.prices", from="2021-03-05", to="2021-04-17")
dji <- tq_get("^DJI", get = "stock.prices", from="2021-03-05", to="2021-04-17")
nasdaq <- tq_get("^IXIC", get = "stock.prices", from="2021-03-05", to="2021-04-17")


# building a new matrix combining the closing prices of all 4 components 
tsla=rename(tsla,TSLA=close)
sp500=rename(sp500,SP500=close)
dji=rename(dji,DJI=close)
nasdaq=rename(nasdaq, NASDAQ=close)

stockprices1=cbind(tsla[,2],tsla[,6],sp500[,6],dji[,6],nasdaq[,6])

save(stockprices1,file="stockprices1.RData")


######################      2 MERGE & CLEAN DATA     ##########################

# 1 Merge data & discard useless variables (26\02\2021 - 11\04\2021)
dataset=rbind(data_day1,data_day2,data_day3,data_day4,data_day5,data_day6,
              data_day7,data_day8,data_day9,data_day10,data_day11,data_day12,
              data_day13,data_day14,data_day15,data_day16,data_day17,data_day18,
              data_day19,data_day20,data_day21,data_day22,data_day23,data_day24,
              data_day25,data_day26,data_day27,data_day28,data_day29,data_day30,
              data_day31,data_day32,data_day33,data_day34,data_day35,data_day36,
              data_day37,data_day38,data_day39,data_day40, data_day41,data_day42,
              data_day43,data_day44,data_day45)

dataset=dataset[,c(3:5,7,10:18,32,33,35,36,38,39,41:63,65:72,74:76,78:85)]

dataset=dataset[,c(1:9,12:27,43:47,51:61)]

save(dataset,file="dataset.RData")

# Due to one date missing in the dataset, we later on had to shift the experiment
# timeframe to 04/03/2021 - 16/04/2021

# 1b Merge data & discard useless variables (12\04\2021 - 16\04\2021)

dataset12=rbind(data_day46, data_day47, data_day48,
                data_day49, data_day50)

dataset11=dataset12[,c(3:5,7,10:18,32,33,35,36,38,39,41:63,65:72,74:76,78:85)]

dataset11=dataset11[,c(1:9,12:27,43:47,51:61)]

save(dataset11, file="dataset11.RData")

# 2 discard semi-duplicated tweets

# METHOD 1 - MANUALLY SCAN THE DATA 
# PRO: CAN RECORD THE NUMBER OF OCCURENCES
# CON: NOT CLEAN, NOT ALL DUPLICATES ARE REMOVED, TIME CONSUMING

for (i in 733:111947){
  if (substr(dataset[733,3],1,50)==substr(dataset[i+1,3],1,50)) {
    dataset[i+1,3]="duplicate"} else
    {dataset
    }
}

dataset=dataset[!duplicated(dataset$text),]
dataset=dataset[-734,]

# tweet 12661: "why some chinese are buying electric car brands like Nio"

for (i in 12661:111865){
  if (substr(dataset[12661,3],1,50)==substr(dataset[i+1,3],1,50)) {
    dataset[i+1,3]="duplicate"} else
    {dataset
    }
}

dataset=dataset[!duplicated(dataset$text),]

# tweet 15199: "Billionaire investor Ron Baron's firm sold 1.8 million Tesla shares"

for (i in 15199:111816){
  if (substr(dataset[15199,3],1,50)==substr(dataset[i+1,3],1,50)) {
    dataset[i+1,3]="duplicate"} else
    {dataset
    }
}

dataset=dataset[!duplicated(dataset$text),]

# tweet 15226: "Bitcoin Affect? Billionaire Fund Manager Sells 1.8M Tesla Shares for Clients"

for (i in 15226:111791){
  if (substr(dataset[15226,3],1,50)==substr(dataset[i+1,3],1,50)) {
    dataset[i+1,3]="duplicate"} else
    {dataset
    }
}

dataset=dataset[!duplicated(dataset$text),]

# tweet 19296: "Analysts tells Tesla to dump Bitcoin for buybacks as shares plunge..."

for (i in 19296:111722){
  if (substr(dataset[19296,3],1,50)==substr(dataset[i+1,3],1,50)) {
    dataset[i+1,3]="duplicate"} else
    {dataset
    }
}

dataset=dataset[!duplicated(dataset$text),]

# tweet 28183: "Critics Claim Tesla Should Sell Bitcoin Position, Electric Vehicle Firm's Shares Down 30% Since Buying - Bitcoin"

for (i in 28183:111652){
  if (substr(dataset[28183,3],1,50)==substr(dataset[i+1,3],1,50)) {
    dataset[i+1,3]="duplicate"} else
    {dataset
    }
}

dataset=dataset[!duplicated(dataset$text),]

# METHOD 2: AUTOMATICALLY SUBSTR ALL DATA N REMOVE DUPLICATES
# PRO: CLEAN & EFFICIENT
# CON: NOT ABLE TO RECORD NUMBER OF OCCURRENCES

dataset2=matrix(0,nrow(dataset),1)

for (i in 1:nrow(dataset)){
  dataset2[i,1]=substr(dataset[i,3],1,50)
}

wh=which(duplicated(dataset2))

dataset=dataset[-c(wh),]

# We end up with METHOD 2 and did not proceed with recording the number of 
# occurrences

# 2b discard semi-duplicated tweets

dataset112=matrix(0,nrow(dataset11),1)

for (i in 1:nrow(dataset11)){
  dataset112[i,1]=substr(dataset11[i,3],1,50)
}

wh=which(duplicated(dataset112))

dataset11=dataset11[-c(wh),]

# 3 Removing tweets by Elon Musk & adding a dummy variable

wh2=which(dataset$screen_name=="elonmusk")

dataset[,1]=as.Date(dataset$created_at)

elon_tweet=dataset[wh2,1]

dataset$elon_dummy=matrix(0,nrow(dataset),1)

for (i in 1:nrow(dataset)){
  if (dataset[i,1]=="2021-03-25") {
    dataset[i,42]=1} else
    {dataset[i,42]=0
    }
}

dataset=dataset[-wh2,]

##################    3 UNSUPERVISED LEARNING METHOD    ########################

# 1 PRE-PROCESSING

# install.packages("stringr")
# install.packages("qdapRegex")
library(tm)
library(stringr)
library(ngram)
library(qdapRegex)

dat=dataset1$text
dat=concatenate(dat)

str=preprocess(dat, case="lower", remove.punct=FALSE, remove.numbers = FALSE, 
               fix.spacing = FALSE)

# 1b PRE-PROCESSING

dat=dataset11$text
dat=concatenate(dat)

str2=preprocess(dat, case="lower", remove.punct=FALSE, remove.numbers = FALSE, 
               fix.spacing = FALSE)

# 2 UNSUPERVISED SENTIMENT ANALYSIS

# install.packages("textdata")
library(tidytext)
ge=get_sentiments("afinn")

# Find the tweeted words that match with the dictionary
words.tweeted=strsplit(str, " ")[[1]]
words.afinn=as.matrix(ge)[,1]
m=match(words.tweeted,words.afinn) 

# how many words that can be scored have ever been used? 
sum(!is.na(m))

# build the score vector
score=ge[as.matrix(m),2]
score[is.na(score)]=0

# combine the the tweeted words vector and the score vector
newdata=cbind(as.matrix(words.tweeted),score)

save(newdata,file="newdata.RData")

# descriptives
unsupervised_sentiment=dataset111$unsupervised_score
t=table(unsupervised_sentiment)
round(prop.table(table(unsupervised_sentiment)),2)
quantile(unsupervised_sentiment,c(0.1,0.25,0.5,0.75,0.9))
hist(unsupervised_sentiment)
boxplot(score_numeric)


# match the scored words back into the tweets dataset
dat2=dataset1$text
dat2=strsplit(dat2," ")

unsupervised_score=matrix(0,nrow(dataset1),1)

for (i in 1:nrow(unsupervised_score)) {
  unsupervised_score[i,1]=sum(newdata[1:length(dat2[[i]]),2])
  newdata=newdata[-c(1:length(dat2[[i]])),]
}

save(unsupervised_score,file="unsupervised_score.RData")

for (i in 1:nrow(unsupervised_score)) {
  if (unsupervised_score[i,1]>5) {
    unsupervised_score[i,1]=5} else {
      unsupervised_score[i,1]=unsupervised_score[i,1]
    }
}

for (i in 1:nrow(unsupervised_score)) {
  if ((unsupervised_score[i,1])<(-5)) {
    unsupervised_score[i,1]=(-5)} else {
      unsupervised_score[i,1]=unsupervised_score[i,1]
    }
}

dataset=cbind(dataset,unsupervised_score)

save(unsupervised_score,file="unsupervised_score.RData")

# 2b UNSUPERVISED SENTIMENT ANALYSIS

library(tidytext)
ge=get_sentiments("afinn")

# Find the tweeted words that match with the dictionary
words.tweeted=strsplit(str, " ")[[1]]
words.afinn=as.matrix(ge)[,1]
m=match(words.tweeted,words.afinn) 

# how many words that can be scored have ever been used? 
sum(!is.na(m))

# build the score vector
score=ge[as.matrix(m),2]
score[is.na(score)]=0

# combine the the tweeted words vector and the score vector
newdata1=cbind(as.matrix(words.tweeted),score)

save(newdata1,file="newdata1.RData")

# match the scored words back into the tweets dataset
dat2=dataset11$text
dat2=strsplit(dat2," ")

unsupervised_score1=matrix(0,nrow(dataset11),1)

for (i in 1:nrow(unsupervised_score1)) {
  unsupervised_score1[i,1]=sum(newdata1[1:length(dat2[[i]]),2])
  newdata1=newdata1[-c(1:length(dat2[[i]])),]
}

save(unsupervised_score1,file="unsupervised_score1.RData")

for (i in 1:nrow(unsupervised_score1)) {
  if (unsupervised_score1[i,1]>5) {
    unsupervised_score1[i,1]=5} else {
      unsupervised_score1[i,1]=unsupervised_score1[i,1]
    }
}

for (i in 1:nrow(unsupervised_score1)) {
  if ((unsupervised_score1[i,1])<(-5)) {
    unsupervised_score1[i,1]=(-5)} else {
      unsupervised_score1[i,1]=unsupervised_score1[i,1]
    }
}

dataset11$elon_dummy=matrix(0,nrow(dataset11),1)

unsupervised_score=unsupervised_score1
dataset11=cbind(dataset11,unsupervised_score)


####################     4 SUPERVISED LEARNING METHOD      #####################

# 1 Uniform subsampling of 5000 tweets
dataset$spvSentiment=NA
wread=sample(nrow(dataset),5000) 

# 2 Add sentiment score to those tweets
dataset[wread[1],]$text
dataset[wread[1],]$unsupervised_score
dataset[wread[1],]$spvSentiment=1

# 3 Save the classifications
save(dataset, file="dataset.Rdata")
sum(!is.na(dataset$spvSentiment))
save(wread, file="wread.Rdata")


# 1b Uniform subsampling of 209 tweets
dataset11$spvSentiment=NA
wread11=sample(nrow(dataset11),209) 

# 2b Add sentiment score to those tweets
dataset11[wread11[1],]$text
dataset11[wread11[1],]$unsupervised_score
dataset11[wread11[1],]$spvSentiment=1

# 3b Save the classifications
save(dataset11, file="dataset.Rdata")
sum(!is.na(dataset11$spvSentiment))
save(wread11, file="wread11.Rdata")

# Combining the two datasets from 04/03/2021 - 16/04/2021
dataset=rbind(dataset,dataset11)
dataset=dataset[10635:102995,]

# 4 Create a dummy for country (1 for US+Canada & 0 otherwise)

# We take last 2 words in the location column (to capture 2-word states eg New York)
# install.packages("stringr")
library(stringr)
country=dataset$location
country2=word(country,start=-2,end=-1)

# We take last 1 word in the location column (to capture 1-word states (Florida) & abbreviations (TX)) 
country1=word(country,-1)

# We create a new matrix for US state names and abbr
usa=cbind(state.abb,state.name)

# We match the 2-word & 1-word states to the state.name and run a loop to turn them all into abbr (need
# to run the loop twice, 1 for 2-word & 1 for 1-word)
m=match(country2,usa[,2])
for (i in 1:nrow(dataset)) {
  if (is.na(m[i])==TRUE) {
    country1[i]=country1[i]} else {
      country1[i]=usa[m[i],1]
    }
}

m=match(country1,usa[,2])
for (i in 1:nrow(dataset)) {
  if (is.na(m[i])==TRUE) {
    country1[i]=country1[i]} else {
      country1[i]=usa[m[i],1]
    }
}

# For all US states we insert "US" in the dataset$country_code
m2=match(country1,usa[,1])
for (i in 1:nrow(dataset)) {
  if (is.na(m2[i])==TRUE) {
    dataset$country_code[i]=dataset$country_code[i]} else {
      dataset$country_code[i]="US"
    }
}

# For all locations with one of the names below we insert "US" in the dataset$country_code
USA_names=c("US","USA","America","States")
for (i in 1:nrow(dataset)) {
  if (any(country1[i]==USA_names)) {
    dataset$country_code[i]="US"} else {
      dataset$country_code[i]=dataset$country_code[i]
    }
}

# For all locations written "Canada" we insert "CA" in the dataset$country_code
for (i in 1:nrow(dataset)) {
  if (country1[i]=="Canada") {
    dataset$country_code[i]="CA"} else {
      dataset$country_code[i]=dataset$country_code[i]
    }
}

# We add a new column in dataset called country_dummy
dataset$country_dummy=NA
US_CA=c("US","CA")

# Country_dummy is 1 when the country code is US/Can, 0 for others & NA if unspecified
for (i in 1:nrow(dataset)) {
  if (dataset$location[i]=="") {
    dataset$country_code[i]="NS"
  }
}

for (i in 1:nrow(dataset)) {
  if (is.na(dataset$country_code[i])) {
    dataset$country_dummy[i]=0}
  else if (any(dataset$country_code[i]==US_CA)) {
    dataset$country_dummy[i]=1} else {
      dataset$country_dummy[i]=NA
    }
}


#########   ALL CODES BELOW ARE ONLY FOR A SUBSET OF THE WHOLE DATA     ########
#########              THAT HAVE BEEN MANUALLY CLASSIFIED              #########

 
##############      SECTION 1: STANDARDIZING TEXTS        #####################

# Create a new dataset of the manually classified tweets, select only predictors &
# labels
dataset2=dataset[!is.na(dataset1$spvSentiment),c(3,8,9,35,36,41,42,44,45)]

dataset2=dataset1[1:100,]

# Pre-processing of texts
dat=dataset2$text
for (i in 1:nrow(dataset2)) {
  dat[i]=concatenate(dat[i])
}

# remove URL links, mentions and hashtags
# install.packages("ngram")
# install.packages("qdapRegex")
library(stringr)
library(ngram)
library(qdapRegex)

rm_twitter_n_url <- rm_(pattern=pastex("@rm_twitter_url", "@rm_url"))
for (i in 1:nrow(dataset2)) {
  dat[i]=rm_twitter_n_url(dat[i])
  dat[i]=rm_tag(dat[i])
  dat[i]=rm_hash(dat[i])
}

# remove upper cases, punctuations and numbers
for (i in 1:nrow(dataset2)) {
  dat[i]=preprocess(dat[i], case="lower", remove.punct = TRUE, remove.numbers = TRUE,
                    fix.spacing=TRUE)
}
dataset2$text=dat


###############           SECTION 2: CORRECT TYPOS        ######################

# 1) tsla/teslas=tesla

for (i in 1:nrow(dataset2)) {
  z=unlist(strsplit(dataset2$text[i]," "))
  for (j in 1:length(z)) {
    if (z[j]=="tsla") {
      z[j]="tesla"} else {
        z[j]=z[j]
      }
  } 
  z=paste(z,collapse=" ")
  dataset2$text[i]=z
}


for (i in 1:nrow(dataset2)) {
  z=unlist(strsplit(dataset2$text[i]," "))
  for (j in 1:length(z)) {
    if (z[j]=="teslas") {
      z[j]="tesla"} else {
        z[j]=z[j]
      }
  } 
  z=paste(z,collapse=" ")
  dataset2$text[i]=z
}

# 2) elon musks = elon musk

for (i in 1:nrow(dataset2)) {
  z=unlist(strsplit(dataset2$text[i]," "))
  for (j in 1:length(z)) {
    if (z[j]=="musks") {
      z[j]="musk"} else {
        z[j]=z[j]
      }
  } 
  z=paste(z,collapse=" ")
  dataset2$text[i]=z
}

save(dataset2, file="dataset2.RData")

#############             SECTION 3: BUILDING TF-IDF        ###################

# Splitting the label & predictors
x=dataset2[,-c(8)]
y=dataset2[,c(8)]

# Split training & test set
wTrain=sample(nrow(x),1500)
train=x[c(wTrain),]
test=x[-c(wTrain),]

# 1st Textual Representation - TF-IDF
# install.packages("text2vec")
# install.packages("data.table")
# install.packages("magrittr")
# install.packages("tm")
library(text2vec)
library(data.table)
library(magrittr)
library(tm)

# Tokenize the words in training set
prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(train$text, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  progressbar = FALSE)

sw2=c(stopwords("en"), "can","get", "will", "s", "sampp", "since")

# Create the vocabulary and remove stopwords, very common and very unusual words
vocab = create_vocabulary(it_train, stopwords=sw2)
pruned_vocab = prune_vocabulary(vocab, 
                                doc_proportion_max=0.9,
                                doc_proportion_min=0.001)

# Create the word vectorizer
vectorizer = vocab_vectorizer(pruned_vocab)

# Tokenize the words in test set
it_test = tok_fun(prep_fun(test$text))
it_test = itoken(it_test, progressbar = FALSE)

# Create a dtm matrix for training set
dtm_train = create_dtm(it_train, vectorizer)
dim(dtm_train)

# define tf-idf model
tf_idf = TfIdf$new()

# fit model to train data and transform train data with fitted model
dtm_train_tfidf = fit_transform(dtm_train, tf_idf)

# Create a dtm matrix for test set
dtm_test_tfidf = create_dtm(it_test, vectorizer)

# apply pre-trained tf-idf transformation to test data
dtm_test_tfidf = transform(dtm_test_tfidf, tf_idf)

save(y,file="y.RData")


######         SECTION 4: ADDING OTHER PREDICTORS TO THE TF-IDF DTM      ######

# Factor the categorical variable (verified)
x$verified=factor(x$verified)

# Replace the missing values in our country_dummy with "Not Specified" and factor
# this variable
for (i in 1:nrow(x)) {
  if (is.na(x[i,8])) {
    x[i,8]="Not Specified"} else {
      x[i,8]=x[i,8]
    }
}
x$country_dummy=factor(x$country_dummy)

# Convert factor variables to dummy indicator variables for every level
conv_verified=model.matrix(~verified-1, x)
conv_elon_dummy=model.matrix(~country_dummy-1, x)
x2=data.frame(x[,-c(1,6,8)],conv_verified,conv_elon_dummy)

# Combining our TF-IDF DTM with all the other predictors
TfIdf_DTM_Train=as.data.frame(as.matrix(dtm_train_tfidf))
TfIdf_DTM_Train=cbind(TfIdf_DTM_Train,x2[wTrain,])

TfIdf_DTM_Test=as.data.frame(as.matrix(dtm_test_tfidf))
TfIdf_DTM_Test=cbind(TfIdf_DTM_Test,x2[-wTrain,])

save(TfIdf_DTM_Train, file="TfIdf_DTM_Train.RData")
save(TfIdf_DTM_Test, file="TfIdf_DTM_Test.RData")

###########      SECTION 5: BUILDING A WORD2VEC MATRIX        ##################

# 2nd Textual Representation - Word2Vec
library(word2vec)
library(tm)

# Preprocess texts
w2v=word2vec( dataset2$text,
              type = "cbow",
              dim=1000,
              split = c(" ","\t"),
              stopwords = stopwords("en"),
)

w2v_mat=as.matrix(w2v)

D2V=doc2vec(w2v,dataset2$text)
D2V[1596,]=0


########      SECTION 6: ADDING OTHER PREDICTORS TO THE D2V MATRIX    #########

D2V_Train=cbind(D2V[wTrain,],x2[wTrain,])

D2V_Test=cbind(D2V[-wTrain,],x2[-wTrain,])

save(D2V_Train, file="D2V_Train.RData")
save(D2V_Test, file="D2V_Test.RData")


###################     5 MACHINE LEARNING ALGORITHMS     ######################

#########       SECTION 1: RUN PREDICTIVE METHODS ON TF-IDF          ##########

# install.packages("tictoc")
library(tictoc)

load("TfIdf_DTM_Train.RData")
load("TfIdf_DTM_Test.RData")
load("y.RData")

yfactor=factor(y)

# M1 ordinalNet

# install.packages("ordinalNet")
library(ordinalNet)

tic("model fitting")
ord_net1=ordinalNet(
  as.matrix(TfIdf_DTM_Train),
  yfactor[wTrain])
toc()

# model fitting: 38086.178 sec elapsed

# Predict the model on the test set
pred1 = predict(ord_net1, as.matrix(TfIdf_DTM_Test))

# Since the result of preds are in probabilities, the predicted scores would be 
# the ones with highest probabilities
pred2=matrix(NA,500,1)
for (i in 1:500) {
  pred2[i]=which.max(pred1[i,]) 
}

# scaling it back to -5 to 5
pred2=pred2-6

# confusion table
table(y[-wTrain],pred2)

# sum of misclassification errors
ME_ordnet_TfIdf=500-sum(diag(table(y[-wTrain],pred2)))   # 404 


# M2 glmnetcr

# install.packages("glmnetcr")
library(glmnetcr)
# install.packages("imager")
library(imager)

tic("model fitting")
glmnet_cr = glmnetcr(x=TfIdf_DTM_Train, y=yfactor[wTrain], maxit=1000)
toc()

# model fitting: 20.072 sec elapsed

# Predict the model on the test set
pred3 = predict.glmnetcr(glmnet_cr, TfIdf_DTM_Test)

# Select the optimal path
s=select.glmnetcr(glmnet_cr, which="AIC")

# confusion table
table(y[-wTrain],pred3$class[,s])

# sum of misclassification errors
ME_glmnetcr_TfIdf=500-(9+1+2+49+21+14+4+1)                 # 399


# M3 Random Forest

library(randomForest)

tic("model fitting")
rf=randomForest(as.matrix(TfIdf_DTM_Train), y[wTrain], ntree=1000)
toc()

# model fitting: 2010.382 sec elapsed

# Predict the model on the test set
pred4=predict(rf,TfIdf_DTM_Test)

# Confusion table
table(y[-wTrain],round(pred4))

# Sum of misclassification errors
ME_rf_TfIdf=500-(1+1+108+37+1)                        # 352


# M4 glmnet with family gaussian

library(glmnet)

tic("model fitting")
# choosing the optimal lambda
cv=cv.glmnet(x=as.matrix(TfIdf_DTM_Train), y=y[wTrain], family="gaussian", alpha=0)

glm=glmnet(x=as.matrix(TfIdf_DTM_Train), y=y[wTrain], family="gaussian", alpha=0, lambda=cv$lambda.min)
toc()

# model fitting: 19.66 sec elapsed

# predict model
pred5=predict(glm,as.matrix(TfIdf_DTM_Test))

# confusion table
table(y[-wTrain], round(pred5))

# sum of misclassification errors
ME_glmgaussian_TfIdf=500-(114+37)                      # 349



##########      SECTION 2: RUN PREDICTIVE METHODS ON W2V MATRIX      ##########

load("D2V_Train.RData")
load("D2V_Test.RData")

# M1 ordinalNet

tic("model fitting")
ord_net2=ordinalNet(
  as.matrix(D2V_Train),
  yfactor[wTrain]
)
toc()

# model fitting: 3598.508 sec elapsed

# Predict the model on the test set
pred6 = predict(ord_net2, as.matrix(D2V_Test))

# Since the result of preds are in probabilities, the predicted scores would be 
# the ones with highest probabilities
pred7=matrix(NA,500,1)
for (i in 1:500) {
  pred7[i]=which.max(pred6[i,]) 
}

# scaling it back to -5 to 5
pred7=pred7-6

# confusion table
table(y[-wTrain], pred7)

# misclassification error
ME_ordnet_D2V=500-(165+3)                                   # 332


# M2 glmnetcr

tic("model fitting")
glmnet_cr2 = glmnetcr(x =D2V_Train, y =yfactor[wTrain], maxit=1000, pmax=2000)
toc()

# model fitting: 8.18 sec elapsed

# Predict the model on the test set
pred8 = predict(glmnet_cr2, D2V_Test)

s2=select.glmnetcr(glmnet_cr2, which="AIC")

# confusion table
table(y[-wTrain],pred8$class[,s2])

# misclassification error
ME_glmnetcr_D2V=500-(165+2)                                  #  333


# M3 Random Forest

tic("model fitting")
rf2=randomForest(as.matrix(D2V_Train), y[wTrain], ntree=1000)
toc()

# model fitting: 476.594 sec elapsed

# Predict the model on the test set
pred9=predict(rf2,D2V_Test)

# Confusion table
table(y[-wTrain],round(pred9))

# misclassification error
ME_rf_D2V=500-(1+2+110+31+3)                                  # 353


# M4 glmnet with family gaussian

# choosing the optimal lambda
tic("model fitting")
cv2=cv.glmnet(x=as.matrix(D2V_Train), y=y[wTrain], family="gaussian", alpha=0)

glm2=glmnet(x=as.matrix(D2V_Train), y=y[wTrain], family="gaussian", alpha=0, lambda=cv2$lambda.min)
toc()

# model fitting: 16.936 sec elapsed

# predict model
pred10=predict(glm2,as.matrix(D2V_Test))

# confusion table
table(y[-wTrain], round(pred10))

# sum of misclassification errors
ME_glmgaussian_D2V=500-(97+35+2)                           #  366



###########      SECTION 3: SIMPLIFY Y INTO A 3-LEVEL FACTOR     ##############

for (i in 1:nrow(dataset2)) {
  if (dataset2[i,8]>(1)) {
    dataset2[i,8]=1} 
}


for (i in 1:nrow(dataset2)) {
  if (dataset2[i,8]<(-1)) {
    (dataset2[i,8]=(-1))} 
}

y2=dataset2[,c(8)]


########      SECTION 4: RUN PREDICTIVE METHODS ON TF-IDF USING y2     ########  

yfactor2=factor(y2)

# M1 ordinalNet

tic("model fitting")
ord_net3=ordinalNet(
  as.matrix(TfIdf_DTM_Train),
  yfactor2[wTrain])
toc()

# model fitting: 8578.071 sec elapsed

# Predict the model on the test set
pred11 = predict(ord_net3, as.matrix(TfIdf_DTM_Test))

# Since the result of preds are in probabilities, the predicted scores would be 
# the ones with highest probabilities
pred12=matrix(NA,500,1)
for (i in 1:500) {
  pred12[i]=which.max(pred11[i,]) 
}

# scaling it back to -1 to 1
pred12=pred12-2

# confusion table
table(y2[-wTrain],pred12)

# sum of misclassification errors
ME_ordnet_TfIdf2=500-sum(diag(table(y2[-wTrain],pred12)))          # 272


# M2 glmnetcr

tic("model fitting")
glmnet_cr3 = glmnetcr(x=TfIdf_DTM_Train, y=yfactor2[wTrain], maxit=1000)
toc()

# model fitting: 4.013 sec elapsed

# Predict the model on the test set
pred13 = predict.glmnetcr(glmnet_cr3, TfIdf_DTM_Test)

# Select the optimal path
s3=select.glmnetcr(glmnet_cr3, which="AIC")

# confusion table
table(y2[-wTrain],pred13$class[,s3])

# sum of misclassification errors
ME_glmnetcr_TfIdf2=500-sum(diag(table(y2[-wTrain],pred13$class[,s3])))                                     # 261


# M3 Random Forest

tic("model fitting")
rf3=randomForest(as.matrix(TfIdf_DTM_Train), yfactor2[wTrain], ntree=1000)
toc()

# model fitting: 1157.287 sec elapsed

# Predict the model on the test set
pred14=predict(rf3,TfIdf_DTM_Test)

# Confusion table
table(y2[-wTrain],pred14) 

# Sum of misclassification errors
ME_rf_TfIdf2=500-sum(diag(table(y2[-wTrain],pred14)))             # 242


# M4 glmnet with family gaussian

# choosing the optimal lambda

tic("model fitting")
cv3=cv.glmnet(x=as.matrix(TfIdf_DTM_Train), y=y2[wTrain], family="gaussian")

glm3=glmnet(x=as.matrix(TfIdf_DTM_Train), y=y2[wTrain], family="gaussian", lambda=cv3$lambda.min)
toc()

# model fitting: 31.045 sec elapsed

# predict model
pred15=predict(glm3,as.matrix(TfIdf_DTM_Test))

# confusion table
table(y2[-wTrain], round(pred15)) 

# sum of misclassification errors
ME_glmgaussian_TfIdf2=500-sum(diag(table(y2[-wTrain],round(pred15))))                                 # 326



########      SECTION 5: RUN PREDICTIVE METHODS ON W2V USING y2     ########  

# M1 ordinalNet

tic("model fitting")
ord_net4=ordinalNet(
  as.matrix(D2V_Train),
  yfactor2[wTrain])
toc()

# model fitting: 2199.016 sec elapsed

# Predict the model on the test set
pred16 = predict(ord_net4, as.matrix(D2V_Test))

# Since the result of preds are in probabilities, the predicted scores would be 
# the ones with highest probabilities
pred17=matrix(NA,500,1)
for (i in 1:500) {
  pred17[i]=which.max(pred16[i,]) 
}

# scaling it back to -1 to 1
pred17=pred17-2

# confusion table
table(y2[-wTrain],pred17)

# sum of misclassification errors
ME_ordnet_D2V2=500-sum(diag(table(y2[-wTrain],pred17)))            # 272


# M2 glmnetcr

tic("model fitting")
glmnet_cr4 = glmnetcr(x=D2V_Train, y=yfactor2[wTrain], maxit=1000, pmax=2000)
toc()

# model fitting: 2.107 sec elapsed

# Predict the model on the test set
pred18 = predict.glmnetcr(glmnet_cr4, D2V_Test)

# Select the optimal path
s4=select.glmnetcr(glmnet_cr4, which="AIC")

# confusion table
table(y2[-wTrain], pred18$class[,s4])

# sum of misclassification errors
ME_glmnetcr_D2V2=500-sum(diag(table(y2[-wTrain],pred18$class[,s4])))                                       # 276


# M3 Random Forest

tic("model fitting")
rf4=randomForest(as.matrix(D2V_Train), yfactor2[wTrain], ntree=1000)
toc()

# model fitting: 130.804 sec elapsed

# Predict the model on the test set
pred19=predict(rf4,D2V_Test)

# Confusion table
table(y2[-wTrain],pred19)

# Sum of misclassification errors
ME_rf_D2V2=500-sum(diag(table(y2[-wTrain],pred19)))                # 260


# M4 glmnet with family gaussian

# choosing the optimal lambda

tic("model fitting")
cv4=cv.glmnet(x=as.matrix(D2V_Train), y=y2[wTrain], family="gaussian")

glm4=glmnet(x=as.matrix(D2V_Train), y=y2[wTrain], family="gaussian", lambda=cv4$lambda.min)
toc()

# model fitting: 374.5 sec elapsed

# predict model
pred20=predict(glm4,as.matrix(D2V_Test))

# confusion table
table(y2[-wTrain], round(pred20))

# sum of misclassification errors
ME_glmgaussian_D2V2=500-(148+31)                                  #  321



#################       CHECK MER ALSO FOR USPV SCORES        ##################

## FOR 11-LEVEL SCORES
a=which(!is.na(dataset1$spvSentiment))
c=dataset1$unsupervised_score[a]
d=dataset1$spvSentiment[a]
table(d[-wTrain],c[-wTrain])
mer_uspv1=500-sum(diag(table(d[-wTrain],c[-wTrain]))) # 347

## FOR 3-LEVEL SCORES
b=dataset1$uspv[a]
table(y[-wTrain],b[-wTrain])
mer_uspv2=500-sum(diag(table(y[-wTrain],b[-wTrain]))) # 257



######### 6 RERUN THE CODES FROM PART 4 SECTION 1 FOR THE WHOLE DATASET ########

##############      SECTION 1: STANDARDIZING TEXTS        #####################


# Load data
load("dataset.RData")

# Create a new dataset of the manually classified tweets, select only predictors &
# labels
dataset2=dataset[,c(3,8,9,35,36,41,42,44,45)]

# install.packages("ngram")
# install.packages("qdapRegex")
library(stringr)
library(ngram)
library(qdapRegex)

# Pre-processing of texts
dat=dataset2$text
for (i in 1:nrow(dataset2)) {
  dat[i]=concatenate(dat[i])
}

# remove URL links, mentions and hashtags

rm_twitter_n_url <- rm_(pattern=pastex("@rm_twitter_url", "@rm_url"))
for (i in 1:nrow(dataset2)) {
  dat[i]=rm_twitter_n_url(dat[i])
  dat[i]=rm_tag(dat[i])
  dat[i]=rm_hash(dat[i])
}

# remove upper cases, punctuations and numbers
for (i in 1:nrow(dataset2)) {
  dat[i]=preprocess(dat[i], case="lower", remove.punct = TRUE, remove.numbers = TRUE,
                    fix.spacing=TRUE)
}
dataset2$text=dat


###############           SECTION 2: CORRECT TYPOS        ######################

# 1) tsla/teslas=tesla

for (i in 1:nrow(dataset2)) {
  z=unlist(strsplit(dataset2$text[i]," "))
  for (j in 1:length(z)) {
    if (z[j]=="tsla") {
      z[j]="tesla"} else {
        z[j]=z[j]
      }
  } 
  z=paste(z,collapse=" ")
  dataset2$text[i]=z
}


for (i in 1:nrow(dataset2)) {
  z=unlist(strsplit(dataset2$text[i]," "))
  for (j in 1:length(z)) {
    if (z[j]=="teslas") {
      z[j]="tesla"} else {
        z[j]=z[j]
      }
  } 
  z=paste(z,collapse=" ")
  dataset2$text[i]=z
}

# 2) elon musks = elon musk

for (i in 1:nrow(dataset2)) {
  z=unlist(strsplit(dataset2$text[i]," "))
  for (j in 1:length(z)) {
    if (z[j]=="musks") {
      z[j]="musk"} else {
        z[j]=z[j]
      }
  } 
  z=paste(z,collapse=" ")
  dataset2$text[i]=z
}


###########      SECTION 5: BUILDING A WORD2VEC MATRIX        ##################

# 2nd Textual Representation - Word2Vec
library(word2vec)
library(tm)

# Preprocess texts
w2v=word2vec( dataset2$text,
              type = "cbow",
              dim=1000,
              split = c(" ","\t"),
              stopwords = stopwords("en"),
)

w2v_mat=as.matrix(w2v)

D2V=doc2vec(w2v,dataset2$text)

for (i in 1:nrow(D2V)) {
  if (is.na(D2V[i,])==TRUE) {
    D2V[i,]=0} else {
      D2V[i,]=D2V[i,]
    }
}


########      SECTION 6: ADDING OTHER PREDICTORS TO THE D2V MATRIX    #########

# Splitting the label & predictors
x=dataset2[,-c(8)]
y=dataset2[,c(8)]

# Split training & test set
wTrain=which(!is.na(y))
train=x[c(wTrain),]
test=x[-c(wTrain),]

# Factor the categorical variable (verified)
x$verified=factor(x$verified)

# Replace the missing values in our country_dummy with "Not Specified" and factor
# this variable
for (i in 1:nrow(x)) {
  if (is.na(x[i,8])) {
    x[i,8]="Not Specified"} else {
      x[i,8]=x[i,8]
    }
}
x$country_dummy=factor(x$country_dummy)

# Convert factor variables to dummy indicator variables for every level
conv_verified=model.matrix(~verified-1, x)
conv_elon_dummy=model.matrix(~country_dummy-1, x)
x2=data.frame(x[,-c(1,6,8)],conv_verified,conv_elon_dummy)

D2V_Train=cbind(D2V[wTrain,],x2[wTrain,])

D2V_Test=cbind(D2V[-wTrain,],x2[-wTrain,])


###########      SECTION 3: SIMPLIFY Y INTO A 3-LEVEL FACTOR     ##############

y2=y[wTrain]

for (i in 1:length(y2)) {
  if (y2[i]>(1)) {
    y2[i]=1} 
}


for (i in 1:length(y2)) {
  if (y2[i]<(-1)) {
    (y2[i]=(-1))} 
}

# M3 Random Forest

library(randomForest)

yfactor2=factor(y2)

rf=randomForest(as.matrix(D2V_Train), yfactor2, ntree=1000)

# Predict the model on the test set
pred=predict(rf,D2V_Test)

# Confusion table
table(pred)


#############             SECTION 3: BUILDING TF-IDF        ###################

# 1st Textual Representation - TF-IDF
# install.packages("text2vec")
# install.packages("data.table")
# install.packages("magrittr")
# install.packages("tm")
library(text2vec)
library(data.table)
library(magrittr)
library(tm)

# Tokenize the words in training set
prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(train$text, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  progressbar = FALSE)

sw2=c(stopwords("en"), "can","get", "will", "s", "sampp", "since")

# Create the vocabulary and remove stopwords, very common and very unusual words
vocab = create_vocabulary(it_train, stopwords=sw2)
pruned_vocab = prune_vocabulary(vocab, 
                                doc_proportion_max=0.9,
                                doc_proportion_min=0.001)

# Create the word vectorizer
vectorizer = vocab_vectorizer(pruned_vocab)

# Tokenize the words in test set
it_test = tok_fun(prep_fun(test$text))
it_test = itoken(it_test, progressbar = FALSE)

# Create a dtm matrix for training set
dtm_train = create_dtm(it_train, vectorizer)
dim(dtm_train)

# define tf-idf model
tf_idf = TfIdf$new()

# fit model to train data and transform train data with fitted model
dtm_train_tfidf = fit_transform(dtm_train, tf_idf)

# Create a dtm matrix for test set
dtm_test_tfidf = create_dtm(it_test, vectorizer)

# apply pre-trained tf-idf transformation to test data
dtm_test_tfidf = transform(dtm_test_tfidf, tf_idf)


######         SECTION 4: ADDING OTHER PREDICTORS TO THE TF-IDF DTM      ######


# Combining our TF-IDF DTM with all the other predictors
TfIdf_DTM_Train=as.data.frame(as.matrix(dtm_train_tfidf))
TfIdf_DTM_Train=cbind(TfIdf_DTM_Train,x2[wTrain,])

TfIdf_DTM_Test=as.data.frame(as.matrix(dtm_test_tfidf))
TfIdf_DTM_Test=cbind(TfIdf_DTM_Test,x2[-wTrain,])


###   SECTION 5: USING RANDOM FOREST TO PREDICT THE WHOLE SCORES OF DATASET  ###


# M3 Random Forest

rf2=randomForest(as.matrix(TfIdf_DTM_Train), yfactor2, ntree=1000)

# Predict the model on the test set
pred14=predict(rf2,TfIdf_DTM_Test)

# Confusion table
table(pred,pred14) 

# Create a full column matrix of spv sentiment using W2V and TF-IDF

# W2V
spv_w2v=matrix(NA,106584,1)

spv_w2v[wTrain]=y2
a=as.numeric(pred)
a=a-2
spv_w2v[-wTrain]=a

# TF-IDF
spv_tfidf=matrix(NA,106584,1)

spv_tfidf[wTrain]=y2
b=as.numeric(pred14)
b=b-2
spv_tfidf[-wTrain]=b

save(spv_w2v, file="spv_w2v.RData")
save(spv_tfidf, file="spv_tfidf.RData")


#####################      7 CORRELATION ANALYSIS       ########################

# LOAD & MERGE DATA

load("dataset.RData")
load("spv_w2v.RData")
load("spv_tfidf.RData")

dataset$spv_tfidf=spv_tfidf
dataset$spv_w2v=spv_w2v


# SIMPLIFY UNSUPERVISED SCORES INTO A 3-LEVEL FACTOR    

dataset$uspv=NA

for (i in 1:nrow(dataset)) {
  if (dataset$unsupervised_score[i]>=(1)) {
    dataset$uspv[i]=1} 
}


for (i in 1:nrow(dataset)) {
  if (dataset$unsupervised_score[i]<=(-1)) {
    (dataset$uspv[i]=(-1))} 
}


for (i in 1:nrow(dataset)) {
  if (dataset$unsupervised_score[i]==(0)) {
    (dataset$uspv[i]=(0))} 
}


##############    COMPARISON OF DIFFERENT WEIGHING METHODS    ##################
  

load("dataset.RData")
load("stockprices1.RData")
load("stockprices.RData")

dataset111=dataset

## 1 FOR NORMALIZED SUM (WE END UP CHOOSING THIS METHOD)

#   CREATING COLUMNS OF SENTIMENTS AND STOCK VARIATION
stockprices1$TSLA_var=0
stockprices1$spv_tfidf=0
stockprices1$spv_w2v=0
stockprices1$uspv=0

#   RATE OF CHANGE OF STOCK PRICES
stockprices1$TSLA_var[1]=(stockprices1$TSLA[1]-stockprices$TSLA[5])/stockprices$TSLA[5]

for (i in 2:nrow(stockprices1)) {
  stockprices1$TSLA_var[i]=(stockprices1$TSLA[i]-stockprices1$TSLA[i-1])/stockprices1$TSLA[i-1]
}

stockprices1$TSLA_var=round(stockprices1$TSLA_var,8)
stockprices1$TSLA_var=stockprices1$TSLA_var*10

#   Create a matrix of number of tweets per day
a=matrix(0,44,7000)

a[1,1:length(which(dataset111$created_at=="2021-03-04"))]=which(dataset111$created_at=="2021-03-04")
a[2,1:length(which(dataset111$created_at=="2021-03-05"))]=which(dataset111$created_at=="2021-03-05")
a[3,1:length(which(dataset111$created_at=="2021-03-06"))]=which(dataset111$created_at=="2021-03-06")
a[4,1:length(which(dataset111$created_at=="2021-03-07"))]=which(dataset111$created_at=="2021-03-07")
a[5,1:length(which(dataset111$created_at=="2021-03-08"))]=which(dataset111$created_at=="2021-03-08")
a[6,1:length(which(dataset111$created_at=="2021-03-09"))]=which(dataset111$created_at=="2021-03-09")
a[7,1:length(which(dataset111$created_at=="2021-03-10"))]=which(dataset111$created_at=="2021-03-10")
a[8,1:length(which(dataset111$created_at=="2021-03-11"))]=which(dataset111$created_at=="2021-03-11")
a[9,1:length(which(dataset111$created_at=="2021-03-12"))]=which(dataset111$created_at=="2021-03-12")
a[10,1:length(which(dataset111$created_at=="2021-03-13"))]=which(dataset111$created_at=="2021-03-13")
a[11,1:length(which(dataset111$created_at=="2021-03-14"))]=which(dataset111$created_at=="2021-03-14")
a[12,1:length(which(dataset111$created_at=="2021-03-15"))]=which(dataset111$created_at=="2021-03-15")
a[13,1:length(which(dataset111$created_at=="2021-03-16"))]=which(dataset111$created_at=="2021-03-16")
a[14,1:length(which(dataset111$created_at=="2021-03-17"))]=which(dataset111$created_at=="2021-03-17")
a[15,1:length(which(dataset111$created_at=="2021-03-18"))]=which(dataset111$created_at=="2021-03-18")
a[16,1:length(which(dataset111$created_at=="2021-03-19"))]=which(dataset111$created_at=="2021-03-19")
a[17,1:length(which(dataset111$created_at=="2021-03-20"))]=which(dataset111$created_at=="2021-03-20")
a[18,1:length(which(dataset111$created_at=="2021-03-21"))]=which(dataset111$created_at=="2021-03-21")
a[19,1:length(which(dataset111$created_at=="2021-03-22"))]=which(dataset111$created_at=="2021-03-22")
a[20,1:length(which(dataset111$created_at=="2021-03-23"))]=which(dataset111$created_at=="2021-03-23")
a[21,1:length(which(dataset111$created_at=="2021-03-24"))]=which(dataset111$created_at=="2021-03-24")
a[22,1:length(which(dataset111$created_at=="2021-03-25"))]=which(dataset111$created_at=="2021-03-25")
a[23,1:length(which(dataset111$created_at=="2021-03-26"))]=which(dataset111$created_at=="2021-03-26")
a[24,1:length(which(dataset111$created_at=="2021-03-27"))]=which(dataset111$created_at=="2021-03-27")
a[25,1:length(which(dataset111$created_at=="2021-03-28"))]=which(dataset111$created_at=="2021-03-28")
a[26,1:length(which(dataset111$created_at=="2021-03-29"))]=which(dataset111$created_at=="2021-03-29")
a[27,1:length(which(dataset111$created_at=="2021-03-30"))]=which(dataset111$created_at=="2021-03-30")
a[28,1:length(which(dataset111$created_at=="2021-03-31"))]=which(dataset111$created_at=="2021-03-31")
a[29,1:length(which(dataset111$created_at=="2021-04-01"))]=which(dataset111$created_at=="2021-04-01")
a[30,1:length(which(dataset111$created_at=="2021-04-02"))]=which(dataset111$created_at=="2021-04-02")
a[31,1:length(which(dataset111$created_at=="2021-04-03"))]=which(dataset111$created_at=="2021-04-03")
a[32,1:length(which(dataset111$created_at=="2021-04-04"))]=which(dataset111$created_at=="2021-04-04")
a[33,1:length(which(dataset111$created_at=="2021-04-05"))]=which(dataset111$created_at=="2021-04-05")
a[34,1:length(which(dataset111$created_at=="2021-04-06"))]=which(dataset111$created_at=="2021-04-06")
a[35,1:length(which(dataset111$created_at=="2021-04-07"))]=which(dataset111$created_at=="2021-04-07")
a[36,1:length(which(dataset111$created_at=="2021-04-08"))]=which(dataset111$created_at=="2021-04-08")
a[37,1:length(which(dataset111$created_at=="2021-04-09"))]=which(dataset111$created_at=="2021-04-09")
a[38,1:length(which(dataset111$created_at=="2021-04-10"))]=which(dataset111$created_at=="2021-04-10")
a[39,1:length(which(dataset111$created_at=="2021-04-11"))]=which(dataset111$created_at=="2021-04-11")
a[40,1:length(which(dataset111$created_at=="2021-04-12"))]=which(dataset111$created_at=="2021-04-12")
a[41,1:length(which(dataset111$created_at=="2021-04-13"))]=which(dataset111$created_at=="2021-04-13")
a[42,1:length(which(dataset111$created_at=="2021-04-14"))]=which(dataset111$created_at=="2021-04-14")
a[43,1:length(which(dataset111$created_at=="2021-04-15"))]=which(dataset111$created_at=="2021-04-15")
a[44,1:length(which(dataset111$created_at=="2021-04-16"))]=which(dataset111$created_at=="2021-04-16")

##   COMPUTE THE NORMALIZED SUM OF SCORES OF EACH TRADING DAY (SUM OF SCORES OF DAY BEFORE/NO. OF TOTAL TWEETS OF DAY BEFORE)
##   FOR MONDAYS WE'RE CONSIDERING TWEETS OF FRIDAY-SUNDAY

# For spv_tfidf
stockprices1[1,7]=(sum(dataset111$spv_tfidf[a[1,]]))/(length(dataset111$spv_tfidf[a[1,]]))

stockprices1[2,7]=(sum(dataset111$spv_tfidf[a[2,]])+sum(dataset111$spv_tfidf[a[3,]])+sum(dataset111$spv_tfidf[a[4,]]))/(length(dataset111$spv_tfidf[a[2:4,]]))
for (i in 3:6) {
  stockprices1[i,7]=(sum(dataset111$spv_tfidf[a[i+2,]]))/(length(dataset111$spv_tfidf[a[i+2,]]))
}

stockprices1[7,7]=(sum(dataset111$spv_tfidf[a[9,]])+sum(dataset111$spv_tfidf[a[10,]])+sum(dataset111$spv_tfidf[a[11,]]))/(length(dataset111$spv_tfidf[a[9:11,]]))
for (i in 8:11) {
  stockprices1[i,7]=(sum(dataset111$spv_tfidf[a[i+4,]]))/(length(dataset111$spv_tfidf[a[i+4,]]))
}

stockprices1[12,7]=(sum(dataset111$spv_tfidf[a[16,]])+sum(dataset111$spv_tfidf[a[17,]])+sum(dataset111$spv_tfidf[a[18,]]))/(length(dataset111$spv_tfidf[a[16:18,]]))
for (i in 13:16) {
  stockprices1[i,7]=(sum(dataset111$spv_tfidf[a[i+6,]]))/(length(dataset111$spv_tfidf[a[i+6,]]))
}

stockprices1[17,7]=(sum(dataset111$spv_tfidf[a[23,]])+sum(dataset111$spv_tfidf[a[24,]])+sum(dataset111$spv_tfidf[a[25,]]))/(length(dataset111$spv_tfidf[a[23:25,]]))
for (i in 18:20) {
  stockprices1[i,7]=(sum(dataset111$spv_tfidf[a[i+8,]]))/(length(dataset111$spv_tfidf[a[i+8,]]))
}

stockprices1[21,7]=(sum(dataset111$spv_tfidf[a[29,]])+sum(dataset111$spv_tfidf[a[30,]])+sum(dataset111$spv_tfidf[a[31,]])+sum(dataset111$spv_tfidf[a[32,]]))/(length(dataset111$spv_tfidf[a[29:32,]]))
for (i in 22:25) {
  stockprices1[i,7]=(sum(dataset111$spv_tfidf[a[i+11,]]))/(length(dataset111$spv_tfidf[a[i+11,]]))
}

stockprices1[26,7]=(sum(dataset111$spv_tfidf[a[37,]])+sum(dataset111$spv_tfidf[a[38,]])+sum(dataset111$spv_tfidf[a[39,]]))/(length(dataset111$spv_tfidf[a[37:39,]]))
for (i in 27:30) {
  stockprices1[i,7]=(sum(dataset111$spv_tfidf[a[i+13,]]))/(length(dataset111$spv_tfidf[a[i+13,]]))
}


# For spv_w2v
stockprices1[1,8]=(sum(dataset111$spv_w2v[a[1,]]))/(length(dataset111$spv_w2v[a[1,]]))

stockprices1[2,8]=(sum(dataset111$spv_w2v[a[2,]])+sum(dataset111$spv_w2v[a[3,]])+sum(dataset111$spv_w2v[a[4,]]))/(length(dataset111$spv_w2v[a[2:4,]]))
for (i in 3:6) {
  stockprices1[i,8]=(sum(dataset111$spv_w2v[a[i+2,]]))/(length(dataset111$spv_w2v[a[i+2,]]))
}

stockprices1[7,8]=(sum(dataset111$spv_w2v[a[9,]])+sum(dataset111$spv_w2v[a[10,]])+sum(dataset111$spv_w2v[a[11,]]))/(length(dataset111$spv_w2v[a[9:11,]]))
for (i in 8:11) {
  stockprices1[i,8]=(sum(dataset111$spv_w2v[a[i+4,]]))/(length(dataset111$spv_w2v[a[i+4,]]))
}

stockprices1[12,8]=(sum(dataset111$spv_w2v[a[16,]])+sum(dataset111$spv_w2v[a[17,]])+sum(dataset111$spv_w2v[a[18,]]))/(length(dataset111$spv_w2v[a[16:18,]]))
for (i in 13:16) {
  stockprices1[i,8]=(sum(dataset111$spv_w2v[a[i+6,]]))/(length(dataset111$spv_w2v[a[i+6,]]))
}

stockprices1[17,8]=(sum(dataset111$spv_w2v[a[23,]])+sum(dataset111$spv_w2v[a[24,]])+sum(dataset111$spv_w2v[a[25,]]))/(length(dataset111$spv_w2v[a[23:25,]]))
for (i in 18:20) {
  stockprices1[i,8]=(sum(dataset111$spv_w2v[a[i+8,]]))/(length(dataset111$spv_w2v[a[i+8,]]))
}

stockprices1[21,8]=(sum(dataset111$spv_w2v[a[29,]])+sum(dataset111$spv_w2v[a[30,]])+sum(dataset111$spv_w2v[a[31,]])+sum(dataset111$spv_w2v[a[32,]]))/(length(dataset111$spv_w2v[a[29:32,]]))
for (i in 22:25) {
  stockprices1[i,8]=(sum(dataset111$spv_w2v[a[i+11,]]))/(length(dataset111$spv_w2v[a[i+11,]]))
}

stockprices1[26,8]=(sum(dataset111$spv_w2v[a[37,]])+sum(dataset111$spv_w2v[a[38,]])+sum(dataset111$spv_w2v[a[39,]]))/(length(dataset111$spv_w2v[a[37:39,]]))
for (i in 27:30) {
  stockprices1[i,8]=(sum(dataset111$spv_w2v[a[i+13,]]))/(length(dataset111$spv_w2v[a[i+13,]]))
}


# For uspv
stockprices1[1,9]=(sum(dataset111$uspv[a[1,]]))/(length(dataset111$uspv[a[1,]]))

stockprices1[2,9]=(sum(dataset111$uspv[a[2,]])+sum(dataset111$uspv[a[3,]])+sum(dataset111$uspv[a[4,]]))/(length(dataset111$uspv[a[2:4,]]))
for (i in 3:6) {
  stockprices1[i,9]=(sum(dataset111$uspv[a[i+2,]]))/(length(dataset111$uspv[a[i+2,]]))
}

stockprices1[7,9]=(sum(dataset111$uspv[a[9,]])+sum(dataset111$uspv[a[10,]])+sum(dataset111$uspv[a[11,]]))/(length(dataset111$uspv[a[9:11,]]))
for (i in 8:11) {
  stockprices1[i,9]=(sum(dataset111$uspv[a[i+4,]]))/(length(dataset111$uspv[a[i+4,]]))
}

stockprices1[12,9]=(sum(dataset111$uspv[a[16,]])+sum(dataset111$uspv[a[17,]])+sum(dataset111$uspv[a[18,]]))/(length(dataset111$uspv[a[16:18,]]))
for (i in 13:16) {
  stockprices1[i,9]=(sum(dataset111$uspv[a[i+6,]]))/(length(dataset111$uspv[a[i+6,]]))
}

stockprices1[17,9]=(sum(dataset111$uspv[a[23,]])+sum(dataset111$uspv[a[24,]])+sum(dataset111$uspv[a[25,]]))/(length(dataset111$uspv[a[23:25,]]))
for (i in 18:20) {
  stockprices1[i,9]=(sum(dataset111$uspv[a[i+8,]]))/(length(dataset111$uspv[a[i+8,]]))
}

stockprices1[21,9]=(sum(dataset111$uspv[a[29,]])+sum(dataset111$uspv[a[30,]])+sum(dataset111$uspv[a[31,]])+sum(dataset111$uspv[a[32,]]))/(length(dataset111$uspv[a[29:32,]]))
for (i in 22:25) {
  stockprices1[i,9]=(sum(dataset111$uspv[a[i+11,]]))/(length(dataset111$uspv[a[i+11,]]))
}

stockprices1[26,9]=(sum(dataset111$uspv[a[37,]])+sum(dataset111$uspv[a[38,]])+sum(dataset111$uspv[a[39,]]))/(length(dataset111$uspv[a[37:39,]]))
for (i in 27:30) {
  stockprices1[i,9]=(sum(dataset111$uspv[a[i+13,]]))/(length(dataset111$uspv[a[i+13,]]))
}

# SPEARMAN CORRELATION TEST
y=stockprices1$TSLA_var
x1=stockprices1$spv_tfidf
x2=stockprices1$spv_w2v
x3=stockprices1$uspv

y_var=rank(y)
tfidf=rank(x1)
w2v=rank(x2)
uspv=rank(x3)

A=cbind(y_var, tfidf,w2v,uspv)

ACF1=acf(A,lag.max=5)


##   2 FOR NORMALIZED WEIGHTED INDEX

#   CREATING COLUMNS OF WEIGHTED SENTIMENTS
stockprices1$weighted_spv_tfidf=0
stockprices1$weighted_spv_w2v=0
stockprices1$weighted_uspv=0

#    CREATE A COLUMN OF WEIGHTS (RETWEET COUNT)
dataset111$rt_fv=0

for (i in 1:nrow(dataset111)) {
  dataset111$rt_fv[i]=dataset111$retweet_count[i]
}

#     MULTIPLY NON ZERO WEIGHTS WITH THE SCORES
for (i in 1:nrow(dataset111)) {
  if (dataset111$rt_fv[i]!=0) {
    dataset111$weighted_spv_tfidf[i]=dataset111$spv_tfidf[i]*(dataset111$rt_fv[i]+1)} else {
      dataset111$weighted_spv_tfidf[i]=dataset111$spv_tfidf[i]
    }
}

for (i in 1:nrow(dataset111)) {
  if (dataset111$rt_fv[i]!=0) {
    dataset111$weighted_spv_w2v[i]=dataset111$spv_w2v[i]*(dataset111$rt_fv[i]+1)} else {
      dataset111$weighted_spv_w2v[i]=dataset111$spv_w2v[i]
    }
}

for (i in 1:nrow(dataset111)) {
  if (dataset111$rt_fv[i]!=0) {
    dataset111$weighted_uspv[i]=dataset111$uspv[i]*(dataset111$rt_fv[i]+1)} else {
      dataset111$weighted_uspv[i]=dataset111$uspv[i]
    }
}

##   COMPUTE THE NORMALIZED SUM OF SCORES OF EACH TRADING DAY (SUM OF SCORES OF DAY BEFORE/NO. OF TOTAL TWEETS OF DAY BEFORE)
##   FOR MONDAYS WE'RE CONSIDERING TWEETS OF FRIDAY-SUNDAY

# For spv_tfidf
stockprices1[1,10]=(sum(dataset111$weighted_spv_tfidf[a[1,]]))/(length(dataset111$weighted_spv_tfidf[a[1,]])+sum(dataset111$rt_fv[a[1,]]))

stockprices1[2,10]=(sum(dataset111$weighted_spv_tfidf[a[2,]])+sum(dataset111$weighted_spv_tfidf[a[3,]])+sum(dataset111$weighted_spv_tfidf[a[4,]]))/(length(dataset111$weighted_spv_tfidf[a[2:4,]])+sum(dataset111$rt_fv[a[2:4,]]))
for (i in 3:6) {
  stockprices1[i,10]=(sum(dataset111$weighted_spv_tfidf[a[i+2,]]))/(length(dataset111$weighted_spv_tfidf[a[i+2,]])+sum(dataset111$rt_fv[a[i+2,]]))
}

stockprices1[7,10]=(sum(dataset111$weighted_spv_tfidf[a[9,]])+sum(dataset111$weighted_spv_tfidf[a[10,]])+sum(dataset111$spv_tfidf[a[11,]]))/(length(dataset111$weighted_spv_tfidf[a[9:11,]])+sum(dataset111$rt_fv[a[9:11,]]))
for (i in 8:11) {
  stockprices1[i,10]=(sum(dataset111$weighted_spv_tfidf[a[i+4,]]))/(length(dataset111$weighted_spv_tfidf[a[i+4,]])+sum(dataset111$rt_fv[a[i+4,]]))
}

stockprices1[12,10]=(sum(dataset111$weighted_spv_tfidf[a[16,]])+sum(dataset111$weighted_spv_tfidf[a[17,]])+sum(dataset111$weighted_spv_tfidf[a[18,]]))/(length(dataset111$weighted_spv_tfidf[a[16:18,]])+sum(dataset111$rt_fv[a[16:18,]]))
for (i in 13:16) {
  stockprices1[i,10]=(sum(dataset111$weighted_spv_tfidf[a[i+6,]]))/(length(dataset111$weighted_spv_tfidf[a[i+6,]])+sum(dataset111$rt_fv[a[i+6,]]))
}

stockprices1[17,10]=(sum(dataset111$weighted_spv_tfidf[a[23,]])+sum(dataset111$weighted_spv_tfidf[a[24,]])+sum(dataset111$weighted_spv_tfidf[a[25,]]))/(length(dataset111$weighted_spv_tfidf[a[23:25,]])+sum(dataset111$rt_fv[a[23:25,]]))
for (i in 18:20) {
  stockprices1[i,10]=(sum(dataset111$weighted_spv_tfidf[a[i+8,]]))/(length(dataset111$weighted_spv_tfidf[a[i+8,]])+sum(dataset111$rt_fv[a[i+8,]]))
}

stockprices1[21,10]=(sum(dataset111$weighted_spv_tfidf[a[29,]])+sum(dataset111$weighted_spv_tfidf[a[30,]])+sum(dataset111$weighted_spv_tfidf[a[31,]])+sum(dataset111$weighted_spv_tfidf[a[32,]]))/(length(dataset111$weighted_spv_tfidf[a[29:32,]])+sum(dataset111$rt_fv[a[29:32,]]))
for (i in 22:25) {
  stockprices1[i,10]=(sum(dataset111$weighted_spv_tfidf[a[i+11,]]))/(length(dataset111$weighted_spv_tfidf[a[i+11,]])+sum(dataset111$rt_fv[a[i+11,]]))
}

stockprices1[26,10]=(sum(dataset111$weighted_spv_tfidf[a[37,]])+sum(dataset111$weighted_spv_tfidf[a[38,]])+sum(dataset111$weighted_spv_tfidf[a[39,]]))/(length(dataset111$weighted_spv_tfidf[a[37:39,]])+sum(dataset111$rt_fv[a[37:39,]]))
for (i in 27:30) {
  stockprices1[i,10]=(sum(dataset111$weighted_spv_tfidf[a[i+13,]]))/(length(dataset111$weighted_spv_tfidf[a[i+13,]])+sum(dataset111$rt_fv[a[i+13,]]))
}

# For spv_w2v
stockprices1[1,11]=(sum(dataset111$weighted_spv_w2v[a[1,]]))/(length(dataset111$weighted_spv_w2v[a[1,]])+sum(dataset111$rt_fv[a[1,]]))

stockprices1[2,11]=(sum(dataset111$weighted_spv_w2v[a[2,]])+sum(dataset111$weighted_spv_w2v[a[3,]])+sum(dataset111$weighted_spv_w2v[a[4,]]))/(length(dataset111$weighted_spv_w2v[a[2:4,]])+sum(dataset111$rt_fv[a[2:4,]]))
for (i in 3:6) {
  stockprices1[i,11]=(sum(dataset111$weighted_spv_w2v[a[i+2,]]))/(length(dataset111$weighted_spv_w2v[a[i+2,]])+sum(dataset111$rt_fv[a[i+2,]]))
}

stockprices1[7,11]=(sum(dataset111$weighted_spv_w2v[a[9,]])+sum(dataset111$weighted_spv_w2v[a[10,]])+sum(dataset111$weighted_spv_w2v[a[11,]]))/(length(dataset111$weighted_spv_w2v[a[9:11,]])+sum(dataset111$rt_fv[a[9:11,]]))
for (i in 8:11) {
  stockprices1[i,11]=(sum(dataset111$weighted_spv_w2v[a[i+4,]]))/(length(dataset111$weighted_spv_w2v[a[i+4,]])+sum(dataset111$rt_fv[a[i+4,]]))
}

stockprices1[12,11]=(sum(dataset111$weighted_spv_w2v[a[16,]])+sum(dataset111$weighted_spv_w2v[a[17,]])+sum(dataset111$weighted_spv_w2v[a[18,]]))/(length(dataset111$weighted_spv_w2v[a[16:18,]])+sum(dataset111$rt_fv[a[16:18,]]))
for (i in 13:16) {
  stockprices1[i,11]=(sum(dataset111$weighted_spv_w2v[a[i+6,]]))/(length(dataset111$weighted_spv_w2v[a[i+6,]])+sum(dataset111$rt_fv[a[i+6,]]))
}

stockprices1[17,11]=(sum(dataset111$weighted_spv_w2v[a[23,]])+sum(dataset111$weighted_spv_w2v[a[24,]])+sum(dataset111$weighted_spv_w2v[a[25,]]))/(length(dataset111$weighted_spv_w2v[a[23:25,]])+sum(dataset111$rt_fv[a[23:25,]]))
for (i in 18:20) {
  stockprices1[i,11]=(sum(dataset111$weighted_spv_w2v[a[i+8,]]))/(length(dataset111$weighted_spv_w2v[a[i+8,]])+sum(dataset111$rt_fv[a[i+8,]]))
}

stockprices1[21,11]=(sum(dataset111$weighted_spv_w2v[a[29,]])+sum(dataset111$weighted_spv_w2v[a[30,]])+sum(dataset111$weighted_spv_w2v[a[31,]])+sum(dataset111$weighted_spv_w2v[a[32,]]))/(length(dataset111$weighted_spv_w2v[a[29:32,]])+sum(dataset111$rt_fv[a[29:32,]]))
for (i in 22:25) {
  stockprices1[i,11]=(sum(dataset111$weighted_spv_w2v[a[i+11,]]))/(length(dataset111$weighted_spv_w2v[a[i+11,]])+sum(dataset111$rt_fv[a[i+11,]]))
}

stockprices1[26,11]=(sum(dataset111$weighted_spv_w2v[a[37,]])+sum(dataset111$weighted_spv_w2v[a[38,]])+sum(dataset111$weighted_spv_w2v[a[39,]]))/(length(dataset111$weighted_spv_w2v[a[37:39,]])+sum(dataset111$rt_fv[a[37:39,]]))
for (i in 27:30) {
  stockprices1[i,11]=(sum(dataset111$weighted_spv_w2v[a[i+13,]]))/(length(dataset111$weighted_spv_w2v[a[i+13,]])+sum(dataset111$rt_fv[a[i+13,]]))
}


# For uspv
stockprices1[1,12]=(sum(dataset111$weighted_uspv[a[1,]]))/(length(dataset111$weighted_uspv[a[1,]])+sum(dataset111$rt_fv[a[1,]]))

stockprices1[2,12]=(sum(dataset111$weighted_uspv[a[2,]])+sum(dataset111$weighted_uspv[a[3,]])+sum(dataset111$weighted_uspv[a[4,]]))/(length(dataset111$weighted_uspv[a[2:4,]])+sum(dataset111$rt_fv[a[2:4,]]))
for (i in 3:6) {
  stockprices1[i,12]=(sum(dataset111$weighted_uspv[a[i+2,]]))/(length(dataset111$weighted_uspv[a[i+2,]])+sum(dataset111$rt_fv[a[i+2,]]))
}

stockprices1[7,12]=(sum(dataset111$weighted_uspv[a[9,]])+sum(dataset111$weighted_uspv[a[10,]])+sum(dataset111$weighted_uspv[a[11,]]))/(length(dataset111$weighted_uspv[a[9:11,]])+sum(dataset111$rt_fv[a[9:11,]]))
for (i in 8:11) {
  stockprices1[i,12]=(sum(dataset111$weighted_uspv[a[i+4,]]))/(length(dataset111$weighted_uspv[a[i+4,]])+sum(dataset111$rt_fv[a[i+4,]]))
}

stockprices1[12,12]=(sum(dataset111$weighted_uspv[a[16,]])+sum(dataset111$weighted_uspv[a[17,]])+sum(dataset111$weighted_uspv[a[18,]]))/(length(dataset111$weighted_uspv[a[16:18,]])+sum(dataset111$rt_fv[a[16:18,]]))
for (i in 13:16) {
  stockprices1[i,12]=(sum(dataset111$weighted_uspv[a[i+6,]]))/(length(dataset111$weighted_uspv[a[i+6,]])+sum(dataset111$rt_fv[a[i+6,]]))
}

stockprices1[17,12]=(sum(dataset111$weighted_uspv[a[23,]])+sum(dataset111$weighted_uspv[a[24,]])+sum(dataset111$weighted_uspv[a[25,]]))/(length(dataset111$weighted_uspv[a[23:25,]])+sum(dataset111$rt_fv[a[23:25,]]))
for (i in 18:20) {
  stockprices1[i,12]=(sum(dataset111$weighted_uspv[a[i+8,]]))/(length(dataset111$weighted_uspv[a[i+8,]])+sum(dataset111$rt_fv[a[i+8,]]))
}

stockprices1[21,12]=(sum(dataset111$weighted_uspv[a[29,]])+sum(dataset111$weighted_uspv[a[30,]])+sum(dataset111$weighted_uspv[a[31,]])+sum(dataset111$weighted_uspv[a[32,]]))/(length(dataset111$weighted_uspv[a[29:32,]])+sum(dataset111$rt_fv[a[29:32,]]))
for (i in 22:25) {
  stockprices1[i,12]=(sum(dataset111$weighted_uspv[a[i+11,]]))/(length(dataset111$weighted_uspv[a[i+11,]])+sum(dataset111$rt_fv[a[i+11,]]))
}

stockprices1[26,12]=(sum(dataset111$weighted_uspv[a[37,]])+sum(dataset111$weighted_uspv[a[38,]])+sum(dataset111$weighted_uspv[a[39,]]))/(length(dataset111$weighted_uspv[a[37:39,]])+sum(dataset111$rt_fv[a[37:39,]]))
for (i in 27:30) {
  stockprices1[i,12]=(sum(dataset111$weighted_uspv[a[i+13,]]))/(length(dataset111$weighted_uspv[a[i+13,]])+sum(dataset111$rt_fv[a[i+13,]]))
}

# SPEARMAN CORRELATION TEST
y=stockprices1$TSLA_var
x111=stockprices1$weighted_spv_tfidf
x222=stockprices1$weighted_spv_w2v
x333=stockprices1$weighted_uspv

y_vr=rank(y)
tfidf2=rank(x111)
w2v2=rank(x222)
uspv2=rank(x333)

A2=cbind(y_vr, tfidf2,w2v2,uspv2)

ACF2=acf(A2,lag.max=5)



################################################################################


### FOR NON-NORMALIZED WEIGHTED INDEX AND SUM

stockprices1$weighted_spv_tfidf=0
stockprices1$weighted_spv_w2v=0
stockprices1$weighted_uspv=0

dataset111$weighted_spv_tfidf=dataset111$spv_tfidf
dataset111$weighted_spv_w2v=dataset111$spv_w2v
dataset111$weighted_uspv=dataset111$uspv

dataset111$rt_fv=0

for (i in 1:nrow(dataset111)) {
  dataset111$rt_fv[i]=dataset111$retweet_count[i]+0.5*dataset111$favorite_count[i]
}

for (i in 1:nrow(dataset111)) {
  if (dataset111$rt_fv[i]!=0) {
    dataset111$weighted_spv_tfidf[i]=dataset111$spv_tfidf[i]*dataset111$rt_fv[i]} else {
      dataset111$weighted_spv_tfidf[i]=dataset111$spv_tfidf[i]
    }
}

for (i in 1:nrow(dataset111)) {
  if (dataset111$rt_fv[i]!=0) {
    dataset111$weighted_spv_w2v[i]=dataset111$spv_w2v[i]*dataset111$rt_fv[i]} else {
      dataset111$weighted_spv_w2v[i]=dataset111$spv_w2v[i]
    }
}

for (i in 1:nrow(dataset111)) {
  if (dataset111$rt_fv[i]!=0) {
    dataset111$weighted_uspv[i]=dataset111$uspv[i]*dataset111$rt_fv[i]} else {
      dataset111$weighted_uspv[i]=dataset111$uspv[i]
    }
}

# For spv_tfidf
stockprices1[1,10]=(sum(dataset111$weighted_spv_tfidf[a[1,]]))

stockprices1[2,10]=(sum(dataset111$weighted_spv_tfidf[a[2,]])+sum(dataset111$weighted_spv_tfidf[a[3,]])+sum(dataset111$weighted_spv_tfidf[a[4,]]))
for (i in 3:6) {
  stockprices1[i,10]=(sum(dataset111$weighted_spv_tfidf[a[i+2,]]))
}

stockprices1[7,10]=(sum(dataset111$weighted_spv_tfidf[a[9,]])+sum(dataset111$weighted_spv_tfidf[a[10,]])+sum(dataset111$spv_tfidf[a[11,]]))
for (i in 8:11) {
  stockprices1[i,10]=(sum(dataset111$weighted_spv_tfidf[a[i+4,]]))
}

stockprices1[12,10]=(sum(dataset111$weighted_spv_tfidf[a[16,]])+sum(dataset111$weighted_spv_tfidf[a[17,]])+sum(dataset111$weighted_spv_tfidf[a[18,]]))
for (i in 13:16) {
  stockprices1[i,10]=(sum(dataset111$weighted_spv_tfidf[a[i+6,]]))
}

stockprices1[17,10]=(sum(dataset111$weighted_spv_tfidf[a[23,]])+sum(dataset111$weighted_spv_tfidf[a[24,]])+sum(dataset111$weighted_spv_tfidf[a[25,]]))
for (i in 18:20) {
  stockprices1[i,10]=(sum(dataset111$weighted_spv_tfidf[a[i+8,]]))
}

stockprices1[21,10]=(sum(dataset111$weighted_spv_tfidf[a[29,]])+sum(dataset111$weighted_spv_tfidf[a[30,]])+sum(dataset111$weighted_spv_tfidf[a[31,]])+sum(dataset111$weighted_spv_tfidf[a[32,]]))
for (i in 22:25) {
  stockprices1[i,10]=(sum(dataset111$weighted_spv_tfidf[a[i+11,]]))
}

stockprices1[26,10]=(sum(dataset111$weighted_spv_tfidf[a[37,]])+sum(dataset111$weighted_spv_tfidf[a[38,]])+sum(dataset111$weighted_spv_tfidf[a[39,]]))
for (i in 27:30) {
  stockprices1[i,10]=(sum(dataset111$weighted_spv_tfidf[a[i+13,]]))
}

# For spv_w2v
stockprices1[1,11]=(sum(dataset111$weighted_spv_w2v[a[1,]]))

stockprices1[2,11]=(sum(dataset111$weighted_spv_w2v[a[2,]])+sum(dataset111$weighted_spv_w2v[a[3,]])+sum(dataset111$weighted_spv_w2v[a[4,]]))
for (i in 3:6) {
  stockprices1[i,11]=(sum(dataset111$weighted_spv_w2v[a[i+2,]]))
}

stockprices1[7,11]=(sum(dataset111$weighted_spv_w2v[a[9,]])+sum(dataset111$weighted_spv_w2v[a[10,]])+sum(dataset111$weighted_spv_w2v[a[11,]]))
for (i in 8:11) {
  stockprices1[i,11]=(sum(dataset111$weighted_spv_w2v[a[i+4,]]))
}

stockprices1[12,11]=(sum(dataset111$weighted_spv_w2v[a[16,]])+sum(dataset111$weighted_spv_w2v[a[17,]])+sum(dataset111$weighted_spv_w2v[a[18,]]))
for (i in 13:16) {
  stockprices1[i,11]=(sum(dataset111$weighted_spv_w2v[a[i+6,]]))
}

stockprices1[17,11]=(sum(dataset111$weighted_spv_w2v[a[23,]])+sum(dataset111$weighted_spv_w2v[a[24,]])+sum(dataset111$weighted_spv_w2v[a[25,]]))
for (i in 18:20) {
  stockprices1[i,11]=(sum(dataset111$weighted_spv_w2v[a[i+8,]]))
}

stockprices1[21,11]=(sum(dataset111$weighted_spv_w2v[a[29,]])+sum(dataset111$weighted_spv_w2v[a[30,]])+sum(dataset111$weighted_spv_w2v[a[31,]])+sum(dataset111$weighted_spv_w2v[a[32,]]))
for (i in 22:25) {
  stockprices1[i,11]=(sum(dataset111$weighted_spv_w2v[a[i+11,]]))
}

stockprices1[26,11]=(sum(dataset111$weighted_spv_w2v[a[37,]])+sum(dataset111$weighted_spv_w2v[a[38,]])+sum(dataset111$weighted_spv_w2v[a[39,]]))
for (i in 27:30) {
  stockprices1[i,11]=(sum(dataset111$weighted_spv_w2v[a[i+13,]]))
}


# For uspv
stockprices1[1,12]=(sum(dataset111$weighted_uspv[a[1,]]))

stockprices1[2,12]=(sum(dataset111$weighted_uspv[a[2,]])+sum(dataset111$weighted_uspv[a[3,]])+sum(dataset111$weighted_uspv[a[4,]]))
for (i in 3:6) {
  stockprices1[i,12]=(sum(dataset111$weighted_uspv[a[i+2,]]))
}

stockprices1[7,12]=(sum(dataset111$weighted_uspv[a[9,]])+sum(dataset111$weighted_uspv[a[10,]])+sum(dataset111$weighted_uspv[a[11,]]))
for (i in 8:11) {
  stockprices1[i,12]=(sum(dataset111$weighted_uspv[a[i+4,]]))
}

stockprices1[12,12]=(sum(dataset111$weighted_uspv[a[16,]])+sum(dataset111$weighted_uspv[a[17,]])+sum(dataset111$weighted_uspv[a[18,]]))
for (i in 13:16) {
  stockprices1[i,12]=(sum(dataset111$weighted_uspv[a[i+6,]]))
}

stockprices1[17,12]=(sum(dataset111$weighted_uspv[a[23,]])+sum(dataset111$weighted_uspv[a[24,]])+sum(dataset111$weighted_uspv[a[25,]]))
for (i in 18:20) {
  stockprices1[i,12]=(sum(dataset111$weighted_uspv[a[i+8,]]))
}

stockprices1[21,12]=(sum(dataset111$weighted_uspv[a[29,]])+sum(dataset111$weighted_uspv[a[30,]])+sum(dataset111$weighted_uspv[a[31,]])+sum(dataset111$weighted_uspv[a[32,]]))
for (i in 22:25) {
  stockprices1[i,12]=(sum(dataset111$weighted_uspv[a[i+11,]]))
}

stockprices1[26,12]=(sum(dataset111$weighted_uspv[a[37,]])+sum(dataset111$weighted_uspv[a[38,]])+sum(dataset111$weighted_uspv[a[39,]]))
for (i in 27:30) {
  stockprices1[i,12]=(sum(dataset111$weighted_uspv[a[i+13,]]))
}

# SPEARMAN CORRELATION TEST
y=stockprices1$TSLA_var
x111=stockprices1$weighted_spv_tfidf
x222=stockprices1$weighted_spv_w2v
x333=stockprices1$weighted_uspv


# SPEARMAN CORRELATION TEST
y3=rank(y)
TFIDF4=rank(x111)
W2V4=rank(x222)
AFIN4=rank(x333)

A3=cbind(y3,TFIDF4,W2V4,AFIN4)

ACF3=acf(A3,lag.max=5)


## FOR SUM

# For spv_tfidf
stockprices1[1,7]=(sum(dataset111$spv_tfidf[a[1,]]))

stockprices1[2,7]=(sum(dataset111$spv_tfidf[a[2,]])+sum(dataset111$spv_tfidf[a[3,]])+sum(dataset111$spv_tfidf[a[4,]]))

for (i in 3:6) {
  stockprices1[i,7]=(sum(dataset111$spv_tfidf[a[i+2,]]))
}

stockprices1[7,7]=(sum(dataset111$spv_tfidf[a[9,]])+sum(dataset111$spv_tfidf[a[10,]])+sum(dataset111$spv_tfidf[a[11,]]))

for (i in 8:11) {
  stockprices1[i,7]=(sum(dataset111$spv_tfidf[a[i+4,]]))
}

stockprices1[12,7]=(sum(dataset111$spv_tfidf[a[16,]])+sum(dataset111$spv_tfidf[a[17,]])+sum(dataset111$spv_tfidf[a[18,]]))

for (i in 13:16) {
  stockprices1[i,7]=(sum(dataset111$spv_tfidf[a[i+6,]]))
}

stockprices1[17,7]=(sum(dataset111$spv_tfidf[a[23,]])+sum(dataset111$spv_tfidf[a[24,]])+sum(dataset111$spv_tfidf[a[25,]]))

for (i in 18:20) {
  stockprices1[i,7]=(sum(dataset111$spv_tfidf[a[i+8,]]))
}

stockprices1[21,7]=(sum(dataset111$spv_tfidf[a[29,]])+sum(dataset111$spv_tfidf[a[30,]])+sum(dataset111$spv_tfidf[a[31,]])+sum(dataset111$spv_tfidf[a[32,]]))

for (i in 22:25) {
  stockprices1[i,7]=(sum(dataset111$spv_tfidf[a[i+11,]]))
}

stockprices1[26,7]=(sum(dataset111$spv_tfidf[a[37,]])+sum(dataset111$spv_tfidf[a[38,]])+sum(dataset111$spv_tfidf[a[39,]]))

for (i in 27:30) {
  stockprices1[i,7]=(sum(dataset111$spv_tfidf[a[i+13,]]))
}

# For spv_w2v
stockprices1[1,8]=(sum(dataset111$spv_w2v[a[1,]]))

stockprices1[2,8]=(sum(dataset111$spv_w2v[a[2,]])+sum(dataset111$spv_w2v[a[3,]])+sum(dataset111$spv_w2v[a[4,]]))

for (i in 3:6) {
  stockprices1[i,8]=(sum(dataset111$spv_w2v[a[i+2,]]))
}

stockprices1[7,8]=(sum(dataset111$spv_w2v[a[9,]])+sum(dataset111$spv_w2v[a[10,]])+sum(dataset111$spv_w2v[a[11,]]))

for (i in 8:11) {
  stockprices1[i,8]=(sum(dataset111$spv_w2v[a[i+4,]]))
}

stockprices1[12,8]=(sum(dataset111$spv_w2v[a[16,]])+sum(dataset111$spv_w2v[a[17,]])+sum(dataset111$spv_w2v[a[18,]]))

for (i in 13:16) {
  stockprices1[i,8]=(sum(dataset111$spv_w2v[a[i+6,]]))
}

stockprices1[17,8]=(sum(dataset111$spv_w2v[a[23,]])+sum(dataset111$spv_w2v[a[24,]])+sum(dataset111$spv_w2v[a[25,]]))

for (i in 18:20) {
  stockprices1[i,8]=(sum(dataset111$spv_w2v[a[i+8,]]))
}

stockprices1[21,8]=(sum(dataset111$spv_w2v[a[29,]])+sum(dataset111$spv_w2v[a[30,]])+sum(dataset111$spv_w2v[a[31,]])+sum(dataset111$spv_w2v[a[32,]]))

for (i in 22:25) {
  stockprices1[i,8]=(sum(dataset111$spv_w2v[a[i+11,]]))
}

stockprices1[26,8]=(sum(dataset111$spv_w2v[a[37,]])+sum(dataset111$spv_w2v[a[38,]])+sum(dataset111$spv_w2v[a[39,]]))

for (i in 27:30) {
  stockprices1[i,8]=(sum(dataset111$spv_w2v[a[i+13,]]))
}


# For uspv
stockprices1[1,9]=(sum(dataset111$uspv[a[1,]]))

stockprices1[2,9]=(sum(dataset111$uspv[a[2,]])+sum(dataset111$uspv[a[3,]])+sum(dataset111$uspv[a[4,]]))

for (i in 3:6) {
  stockprices1[i,9]=(sum(dataset111$uspv[a[i+2,]]))
}

stockprices1[7,9]=(sum(dataset111$uspv[a[9,]])+sum(dataset111$uspv[a[10,]])+sum(dataset111$uspv[a[11,]]))

for (i in 8:11) {
  stockprices1[i,9]=(sum(dataset111$uspv[a[i+4,]]))
}

stockprices1[12,9]=(sum(dataset111$uspv[a[16,]])+sum(dataset111$uspv[a[17,]])+sum(dataset111$uspv[a[18,]]))

for (i in 13:16) {
  stockprices1[i,9]=(sum(dataset111$uspv[a[i+6,]]))
}

stockprices1[17,9]=(sum(dataset111$uspv[a[23,]])+sum(dataset111$uspv[a[24,]])+sum(dataset111$uspv[a[25,]]))

for (i in 18:20) {
  stockprices1[i,9]=(sum(dataset111$uspv[a[i+8,]]))
}

stockprices1[21,9]=(sum(dataset111$uspv[a[29,]])+sum(dataset111$uspv[a[30,]])+sum(dataset111$uspv[a[31,]])+sum(dataset111$uspv[a[32,]]))

for (i in 22:25) {
  stockprices1[i,9]=(sum(dataset111$uspv[a[i+11,]]))
}

stockprices1[26,9]=(sum(dataset111$uspv[a[37,]])+sum(dataset111$uspv[a[38,]])+sum(dataset111$uspv[a[39,]]))

for (i in 27:30) {
  stockprices1[i,9]=(sum(dataset111$uspv[a[i+13,]]))
}

# SPEARMAN CORRELATION TEST
y=stockprices1$TSLA_var
x1=stockprices1$spv_tfidf
x2=stockprices1$spv_w2v
x3=stockprices1$uspv

y3=rank(y)
x11=rank(x1)
x22=rank(x2)
x33=rank(x3)

A4=cbind(y, x11,x22,x33)

ACF4=acf(A4,lag.max=5)


#########################          8  WORDCLOUDS       #########################

library(wordcloud)
library(tm)
library(SnowballC)
library(topicmodels)

# 1 WORDCLOUD FOR ALL TWEETS

# text N rows, one column, just text 
text_corpus <- VCorpus(VectorSource(dataset$text))

# cleaning 
text_corpus_clean <- tm_map(text_corpus, content_transformer(tolower))

sw=c(stopwords(),"tesla", "tsla", "stock","stocks","share","shares","invest", "invests",
     "investing", "invested", "trade", "trades", "trading", "traded", "buy", "buys",
     "buying", "bought", "sell", "sells", "sold", "selling", "can","get", "will","", "'ll")

text_corpus_clean <- tm_map(text_corpus_clean, removeWords, sw)

text_corpus_clean <- tm_map(text_corpus_clean, stemDocument)

text_corpus_clean <- tm_map(text_corpus_clean, removeNumbers)

text_corpus_clean <- tm_map(text_corpus_clean, removePunctuation)

text_corpus_clean <- tm_map(text_corpus_clean, stripWhitespace)

text_corpus_clean <- tm_map(text_corpus_clean, PlainTextDocument)

# explorative plot 
wordcloud(text_corpus_clean, max.words=200, random.order=FALSE, rot.per=0.15, colors=brewer.pal(8, "Dark2"), scale=c(3.0,0.25))


###############################################################################


# 2 WORDCLOUD GROUPED BY SENTIMENTS

# wordcloud for TF-IDF negative scores

# text N rows, one column, just text 
w=which(dataset1$spv_tfidf==(-1))
text_corpus <- VCorpus(VectorSource(dataset1$text[w]))

# cleaning 

text_corpus_clean <- tm_map(text_corpus, content_transformer(tolower))

sw=c(stopwords(),"tesla", "tsla", "stock","stocks","share","shares","invest", "invests",
     "investing", "invested", "trade", "trades", "trading", "traded", "buy", "buys",
     "buying", "bought", "sell", "sells", "sold", "selling", "can","get", "will","", "'ll",
     "just", "elonmusk", "car", "bitcoin")

text_corpus_clean <- tm_map(text_corpus_clean, removeWords, sw)

text_corpus_clean <- tm_map(text_corpus_clean, stemDocument)

text_corpus_clean <- tm_map(text_corpus_clean, removeNumbers)


text_corpus_clean <- tm_map(text_corpus_clean, removePunctuation)

text_corpus_clean <- tm_map(text_corpus_clean, stripWhitespace)

text_corpus_clean <- tm_map(text_corpus_clean, PlainTextDocument)


# explorative plot 

wordcloud(text_corpus_clean, max.words=200, random.order=FALSE, rot.per=0.15, colors=brewer.pal(8, "Dark2"), scale=c(2.0,0.25))


##############################################################################


# wordcloud for TF-IDF neutral scores

# text N rows, one column, just text 
w2=which(dataset1$spv_tfidf==0)
text_corpus <- VCorpus(VectorSource(dataset1$text[w2]))

# cleaning 


text_corpus_clean <- tm_map(text_corpus, content_transformer(tolower))

sw=c(stopwords(),"tesla", "tsla", "stock","stocks","share","shares","invest", "invests",
     "investing", "invested", "trade", "trades", "trading", "traded", "buy", "buys",
     "buying", "bought", "sell", "sells", "sold", "selling", "can","get", "will","", "'ll", 
     "just", "elonmusk", "car", "bitcoin")

text_corpus_clean <- tm_map(text_corpus_clean, removeWords, sw)

text_corpus_clean <- tm_map(text_corpus_clean, stemDocument)

text_corpus_clean <- tm_map(text_corpus_clean, removeNumbers)


text_corpus_clean <- tm_map(text_corpus_clean, removePunctuation)

text_corpus_clean <- tm_map(text_corpus_clean, stripWhitespace)

text_corpus_clean <- tm_map(text_corpus_clean, PlainTextDocument)


# explorative plot 

wordcloud(text_corpus_clean, max.words=200, random.order=FALSE, rot.per=0.15, colors=brewer.pal(8, "Dark2"), scale=c(2.5,0.25))


##############################################################################


# wordcloud for TF-IDF positive scores

# text N rows, one column, just text 
w3=which(dataset1$spv_tfidf==1)
text_corpus <- VCorpus(VectorSource(dataset1$text[w3]))

# cleaning 


text_corpus_clean <- tm_map(text_corpus, content_transformer(tolower))

sw=c(stopwords(),"tesla", "tsla", "stock","stocks","share","shares","invest", "invests",
     "investing", "invested", "trade", "trades", "trading", "traded", "buy", "buys",
     "buying", "bought", "sell", "sells", "sold", "selling", "can","get", "will","", "'ll",
     "just", "elonmusk", "car", "bitcoin")

text_corpus_clean <- tm_map(text_corpus_clean, removeWords, sw)

text_corpus_clean <- tm_map(text_corpus_clean, stemDocument)

text_corpus_clean <- tm_map(text_corpus_clean, removeNumbers)


text_corpus_clean <- tm_map(text_corpus_clean, removePunctuation)

text_corpus_clean <- tm_map(text_corpus_clean, stripWhitespace)

text_corpus_clean <- tm_map(text_corpus_clean, PlainTextDocument)

# explorative plot 

wordcloud(text_corpus_clean, max.words=200, random.order=FALSE, rot.per=0.15, colors=brewer.pal(8, "Dark2"), scale=c(2.0,0.25))

## WE APPLIED THE SAME CODES ALSO FOR UNSUPERVISED AND WORD2VEC SENTIMENTS


######################       9 PLOTS & DESCRIPTIVES       ######################

## Plot Tesla Stock Price for past 10 years

library(ggplot2)
TSLA$Date=as.Date(TSLA$Date)
ggplot(data=TSLA, aes(Date,Close)) + geom_line(colour="red") + ggtitle("Tesla Stock Price from 2010 to 2021")


## Plot Tesla Stock Price from 5/3/2021 - 17/4/2021

# download stock price data (Yahoo Finance)

# install.packages("tidyquant")
# install.packages("BatchGetSymbols")
library(tidyquant)
library(BatchGetSymbols)
library(quantmod)

tsla <- tq_get("TSLA", get = "stock.prices", from="2021-03-05", to="2021-04-17")

sp500 <- tq_get("^GSPC", get = "stock.prices", from="2021-03-05", to="2021-04-17")
nasdaq <- tq_get("^IXIC", get = "stock.prices", from="2021-03-05", to="2021-04-17")


# building a new matrix combining the closing prices of all 4 components 
tsla=rename(tsla,Date=date)
tsla=rename(tsla,TSLA_Close=Close)
sp500=rename(sp500,SP500_Close=SP500)
nasdaq=rename(nasdaq, NSDQ_Close=NASDAQ)

stockprices=cbind(tsla[,2],tsla[,6], sp500[,6], nasdaq[,6])

# plot time series of TSLA, SP500 and NASDAQ stock price
ggplot(data=stockprices, aes(Date,TSLA_Close)) + geom_line(colour="orange") + ggtitle("Tesla Closing Stock Price from 05/03/2021 to 17/04/2021")

ggplot(data=stockprices, aes(Date,SP500_Close)) + geom_line(colour="blue") + ggtitle("Tesla Stock Price from 2010 to 2021")
ggplot(data=stockprices, aes(Date,NSDQ_Close)) + geom_line(colour="purple") + ggtitle("Nasdaq Composite Closing Stock Price from 05/03/2021 to 17/04/2021")


# combine all time series in one plot
# install.packages("reshape2")                 # Install reshape2 package
library("reshape2")  

data_long <- melt(stockprices, id.vars = "Date")

ggplot(data_long,                            # Draw ggplot2 time series plot
       aes(x = Date,
           y = value,
           col = variable)) +
  geom_line()

# plot of stock price variation
ggplot(data=stockprices1, aes(date,TSLA_var)) + geom_line(colour="maroon") + ggtitle("Tesla Stock Price Variation")


# Descriptives

# for uspv, tf-idf & w2v (-1,0,1)
# table of scores
USPV=table(dataset1$uspv)
TF_IDF=table(dataset1$spv_tfidf)
W2V=table(dataset1$spv_w2v)

b=rbind(USPV,TF_IDF,W2V)

# marginal dispersion
USPV=round(prop.table(table(dataset1$uspv)),2)
TF_IDF=round(prop.table(table(dataset1$spv_tfidf)),2)
W2V=round(prop.table(table(dataset1$spv_w2v)),2)

a=rbind(USPV,TF_IDF,W2V)

# histograms
hist(dataset1$uspv, col="brown")
hist(dataset1$spv_tfidf, col="green")
hist(dataset1$spv_w2v, col="blue")

# for original uspv (-5 to 5)
hist(unsupervised_score, col="brown")
boxplot(unsupervised_score, xlab="Unsupervised Method", ylab="Score", col="yellow")
