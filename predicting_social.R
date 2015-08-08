library(ggplot2)
library(gmodels)
library(AER)
library(glmmML) #http://www.inside-r.org/packages/cran/glmmML/docs/glmmML
library(lme4)
library(stargazer)

### LOAD ADJUSTED ARTICLES INTO THE DATASET
#articles <- read.csv("~/Dropbox/uknews_gender/ukdf_adjusted.csv")
articles <- read.table("uk_articles_data.csv")

articles$date <- as.Date(articles$date)
articles$title <- NULL
articles$url <- NULL



########################################
####  UNIVARIATE SUMMARY STATISTICS ####
########################################

#### PUBLISHER ####
summary(articles$publisher)

#### SECTION ####
summary(articles$section)

counts <- table(articles$publisher, articles$section)
par(las=2)
barplot(counts, main="Article Distribution by Section and Publisher",
        xlab="Number of Articles", col=c("darkblue","red", "yellow"),
        legend = rownames(counts), beside=TRUE,  horiz=TRUE)

# DROP OBSERVATIONS WITH SECTIONS THAT DON'T SPAN ALL THREE PUBS
# artsculture
# entertainment
# specialaudience
articles <- subset(articles, section=="sport" | section=="sciencetech" | section=="opinion" | section=="news" | section=="moneyfinance" | section =="lifestyle")
# Now 246728 observations

CrossTable(articles$section, articles$publisher)


# SHOW RESULT OF DROPPING OBSERVATIONS
counts <- table(articles$publisher, articles$section)
par(las=2)
barplot(counts, main="Article Distribution by Section and Publisher",
        xlab="Number of Gears", col=c("darkblue","red", "yellow"),
        legend = rownames(counts), beside=TRUE,  horiz=TRUE)

#### GENDER ####
# B = BOTH
# F = WOMEN
# M = MEN
# X = UNKNOWN
# E = ERROR
# CGENDER:
# 0 = MALE
# 1 = FEMALE
# 2 = BOTH
# 3 = UNKNOWN OR ERROR
summary(articles$gender)
CrossTable(articles$publisher, articles$gender)
CrossTable(articles$section, articles$gender)

# REMOVE ALL OBSERVATIONS THAT ARE UNKNOWN OR ERRORS
articles <- subset(articles, cgender<=2)
# NOW WE ARE DOWN TO 162217 OBSERVATIONS

summary(articles$gender)
CrossTable(articles$publisher, articles$gender)
CrossTable(articles$section, articles$gender)

#### DATE ####
hist(articles$date, "weeks", format = "%d %b")
hist(articles$date, "months", format = "%d %b")

summary(articles$date)

#### TITLE_TOKENS ####
hist(articles$log_title_tokens, breaks=200)
summary(articles$log_title_tokens)

#### DAYS_SINCE_FIRST ####
hist(articles$log_title_tokens, breaks=200)
summary(articles$days_since_first)


#### TOTAL_ARTICLES ####
summary(articles$total_articles)
hist(articles$total_articles, breaks=200)

#### SOCIAL ####
## SET ALL NEGATIVE VALUES TO 0 ##
articles$social = (abs(articles$social)+articles$social)/2

## NOW HISTOGRAMS ##
hist(articles$social, breaks=200)
hist(articles$facebook, breaks=200)
hist(articles$twitter, breaks=200)
hist(articles$googleplus, breaks=200)

hist(articles$log_social, breaks=200)
hist(articles$log_facebook, breaks=200)
hist(articles$log_twitter, breaks=200)

#######################################
####  BIVARIATE SUMMARY STATISTICS ####
#######################################

# SOCIAL ON WORDCOUNT
p <- ggplot(articles, aes(x=log_wordcount, y=log_social, colour=gender)) + geom_point(shape=1) 
p + facet_grid(publisher ~ section)

# SOCIAL ON DEMEANED_WORDOUNCT
p <- ggplot(articles, aes(x=log_wordcount_demeaned, y=log_social, colour=gender)) + geom_point(shape=1) 
p + facet_grid(publisher ~ section)

# SOCIAL ON WEEKDAY
p <- ggplot(articles, aes(x=weekday, y=log_social, colour=gender)) + geom_point(shape=1) 
p + facet_grid(publisher ~ section)

# SOCIAL ON MONTH
p <- ggplot(articles, aes(x=month, y=log_social, colour=gender)) + geom_point(shape=1) 
p + facet_grid(publisher ~ section)

# SOCIAL ON TOTAL_ARTICLES
p <- ggplot(articles, aes(x=log_total_articles, y=log_social, colour=gender)) + geom_point(shape=1) 
p + facet_grid(publisher ~ section)

# SOCIAL ON TITLE_TOKENS
p <- ggplot(articles, aes(x=log_title_tokens, y=log_social, colour=gender)) + geom_point(shape=1) 
p + facet_grid(publisher ~ section)


#################################
####  TESTING POISSON MODELS ####
#################################

articles$gF <- factor(articles$gender, levels = c("M","F","B"))
articles$sF <- factor(articles$section, levels = c("news",
                                                        "lifestyle",
                                                        "moneyfinance",
                                                        "opinion",
                                                        "sciencetech",
                                                        "sport"))
articles$wdF <- factor(articles$weekday)

### DO ANALYSIS FOR THE GUARDIAN
guardian <- subset(articles, publisher=="guardian")
dailymail <- subset(articles, publisher=="dailymail")
telegraph <- subset(articles, publisher=="telegraph")


################################
summary(t1 <- glm(social~log_title_tokens + gF,data=guardian,family=poisson))
## calculate and store predicted values
guardian$t1hat <- predict(m1, type="response")

## order by program and then by math
guardian <- guardian[with(guardian, order(gF, log_title_tokens)), ]

## create the plot
ggplot(guardian, aes(x = log_title_tokens, y = t1hat, colour = gF)) +
  geom_point(aes(y = social), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "Words in Title", y = "Social Media Impressions")
################################
paper = dailymail

summary(t1 <- glm(social~log_title_tokens + I(log_title_tokens^2) + gF + sF + gF:sF,data=paper,family=poisson))
## calculate and store predicted values
paper$t1hat <- predict(t1, type="response")

## order by program and then by math
paper <- paper[with(paper, order(log_title_tokens, gF)), ]

## create the plot
ggplot(paper, aes(x = log_title_tokens, y = t1hat, colour = gF)) +
#  geom_point(aes(y = social), alpha=.5, position=position_jitter(h=.2)) +
  facet_grid(sF ~ ., scales="free") + 
  geom_line(size = 1) +
  labs(x = "log(Words in Title)", y = "Social Media Impressions") + 
  ggtitle("Predicted Daily Mail Social Media Impressions Per Article") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))



##################################################
####          MULTILEVEL POISSON MODELS       ####
####  ACCOUNTING FOR VARIATION BETWEEN PEOPLE ####
##################################################

summary(f1 <- glmer(social~ title_tokens + I(title_tokens^2) + total_articles + wdF + gF+ sF + gF:sF + (1|bylines), data=guardian, family=poisson))

summary(f2 <- glmer(social~ title_tokens + I(title_tokens^2) + total_articles + wdF + gF+ sF + gF:sF + (1|bylines), data=dailymail, family=poisson))

summary(f3 <- glmer(social~ title_tokens + I(title_tokens^2) + total_articles + wdF + gF+ sF + gF:sF + (1|bylines), data=telegraph, family=poisson))

stargazer(f1,f2,f3, type="text")

summary(f2 <- glmer(social~ title_tokens + I(title_tokens^2) + wdF + gF + sF + (1|bylines), data=telegraph, family=poisson))

summary(f3 <- glmer(social~ title_tokens + I(title_tokens^2) + wdF + gF + sF + (1|bylines), data=guardian, family=poisson))

summary(f4 <- glmer(social~ title_tokens + I(title_tokens^2) + wdF + gF + sF + (1|bylines), data=dailymail, family=poisson))

summary(f5 <- glmer(social~ title_tokens + I(title_tokens^2) + wdF + gF + sF + (1|bylines), data=dailymail, family=poisson))


#(social ~ gF + sF + gF:sF + wordcount, family=poisson, data=guardian))
#dispersiontest(m1)
#(est <- cbind(Estimate = coef(m1), confint(m1)))
#exp(est)

#summary(m2 <- glmmadmb(social~ wordcount + (1|bylines), 
#                        data=articles, 
#                        zeroInflation=FALSE, 
#                        family="poisson"))

#ggplot(people, aes(x=log_pop, y=article_count)) + geom_point(shape=1) 


summary(fit <- glm(social~log_title_tokens + gF + sF + gF:sF,data=guardian,family=poisson))
(est <- cbind(Estimate = coef(fit), confint(fit)))
exp(est)       
dispersiontest(fit)


#### WRITE TO FILE####
#write.table(articles, "uk_articles_data.csv")
