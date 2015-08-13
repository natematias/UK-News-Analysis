library(ggplot2)
library(gmodels)
library(AER)
library(glmmML) #http://www.inside-r.org/packages/cran/glmmML/docs/glmmML
library(lme4)
library(stargazer)
library(sjPlot)

### LOAD ADJUSTED ARTICLES INTO THE DATASET
#articles <- read.csv("~/Dropbox/uknews_gender/ukdf_adjusted.csv")
articles <- read.table("uk_articles_data_notext_notitles.csv")

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
summary(articles$log_total_articles)
hist(articles$log_total_articles, breaks=200)

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

# SOCIAL ON TITLE TOKENS
p <- ggplot(articles, aes(x=title_tokens, y=log_social, colour=gender)) + geom_point(shape=1) 
p + facet_grid(publisher ~ section)

# SOCIAL ON log transformed TITLE TOKENS
p <- ggplot(articles, aes(x=log_title_tokens, y=log_social, colour=gender)) + geom_point(shape=1) 
p + facet_grid(publisher ~ section)

# SOCIAL ON WEEKDAY
p <- ggplot(articles, aes(x=weekday, y=log_social, colour=gender)) + geom_point(shape=1) 
p + facet_grid(publisher ~ section)

# SOCIAL ON MONTH
p <- ggplot(articles, aes(x=month, y=log_social, colour=gender)) + geom_point(shape=1) 
p + facet_grid(publisher ~ section)

# SOCIAL ON TOTAL_ARTICLES
p <- ggplot(articles, aes(x=total_articles, y=log_social, colour=gender)) + geom_point(shape=1) 
p + facet_grid(publisher ~ section)

# SOCIAL ON log transformed TOTAL_ARTICLES
p <- ggplot(articles, aes(x=log_total_articles, y=log_social, colour=gender)) + geom_point(shape=1) 
p + facet_grid(publisher ~ section)


#################################
####  TESTING POISSON MODELS ####
#################################

#articles$gF <- factor(articles$gender, levels = c("M","F","B"))
articles$gF <- factor(articles$gender, levels = c("M","F"))
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
paper = dailymail

summary(t1 <- glm(social~log_title_tokens + I(log_title_tokens^2) + gF + sF + gF:sF,data=paper,family=poisson))
dispersiontest(t1)
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


################################
paper = guardian

summary(t1 <- glm(social~log_title_tokens + I(log_title_tokens^2) + gF + sF + gF:sF,data=paper,family=poisson))
dispersiontest(t1)
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
  ggtitle("Predicted Guardian Social Media Impressions Per Article") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))
################################
paper = telegraph

summary(t1 <- glm(social~log_title_tokens + I(log_title_tokens^2) + gF + sF + gF:sF,data=paper,family=poisson))
dispersiontest(t1)
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
  ggtitle("Predicted Telegraph Social Media Impressions Per Article") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))


##################################################
####          MULTILEVEL POISSON MODELS       ####
####  ACCOUNTING FOR VARIATION BETWEEN PEOPLE ####
##################################################

summary(g1 <- glmer(social~ log_title_tokens +  I(log_title_tokens^2) + (1|bylines), data=guardian, family=poisson))
summary(g2 <- glmer(social~ log_title_tokens +  I(log_title_tokens^2) + total_articles + (1|bylines), data=guardian, family=poisson))
summary(g3 <- glmer(social~ log_title_tokens +  I(log_title_tokens^2) + log_total_articles + (1|bylines), data=guardian, family=poisson))
summary(g4 <- glmer(social~ log_title_tokens +  I(log_title_tokens^2) + log_total_articles + wdF + (1|bylines), data=guardian, family=poisson))
summary(g5 <- glmer(social~ log_title_tokens +  I(log_title_tokens^2) + log_total_articles + wdF + sF + (1|bylines), data=guardian, family=poisson))
summary(g6 <- glmer(social~ log_title_tokens +  I(log_title_tokens^2) + log_total_articles + wdF + sF + gF + gF:sF + (1|bylines), data=guardian, family=poisson))
# get the deviances
anova(g1, g2, g3,g4,g5,g6, test = "F")
# DEVIANCE TABLE 
#g1 21888485
#g2 21888475 <-- is total articles not very important?
#g3 21888435 <-- maybe clustering? total_articles does seem important theoretically, so keep it in
#g4 21824029
#g5 21622303
#g6 21607227 <-- this is the best fit
stargazer(g1,g2,g3,g4,g5,g6, type="text")

summary(d1 <- glmer(social~ log_title_tokens +  I(log_title_tokens^2) + (1|bylines), data=dailymail, family=poisson))
summary(d2 <- glmer(social~ log_title_tokens +  I(log_title_tokens^2) + total_articles + (1|bylines), data=dailymail, family=poisson))
summary(d3 <- glmer(social~ log_title_tokens +  I(log_title_tokens^2) + log_total_articles + (1|bylines), data=dailymail, family=poisson))
summary(d4 <- glmer(social~ log_title_tokens +  I(log_title_tokens^2) + log_total_articles + wdF + (1|bylines), data=dailymail, family=poisson))
summary(d5 <- glmer(social~ log_title_tokens +  I(log_title_tokens^2) + log_total_articles + wdF + sF + (1|bylines), data=dailymail, family=poisson))
summary(d6 <- glmer(social~ log_title_tokens +  I(log_title_tokens^2) + log_total_articles + wdF + sF + gF + gF:sF + (1|bylines), data=dailymail, family=poisson))
# get the deviances 
anova(d1, d2, d3, d4, d5, d6, test = "F")
stargazer(g1,g2,g3,g4,g5,g6, type="text")

summary(t1 <- glmer(social~ log_title_tokens + (1|bylines), data=telegraph, family=poisson))
summary(t2 <- glmer(social~ log_title_tokens +  I(log_title_tokens^2) + total_articles + (1|bylines), data=telegraph, family=poisson))
summary(t3 <- glmer(social~ log_title_tokens +  I(log_title_tokens^2) + log_total_articles + (1|bylines), data=telegraph, family=poisson))
summary(t4 <- glmer(social~ log_title_tokens +  I(log_title_tokens^2) + log_total_articles + wdF + (1|bylines), data=telegraph, family=poisson))
summary(t5 <- glmer(social~ log_title_tokens +  I(log_title_tokens^2) + log_total_articles + wdF + sF + (1|bylines), data=telegraph, family=poisson))
summary(t6 <- glmer(social~ log_title_tokens +  I(log_title_tokens^2) + log_total_articles + wdF + sF + gF + gF:sF + (1|bylines), data=telegraph, family=poisson))
anova(t1, t2, t3, t4, t5, t6, test = "F")
stargazer(t1,t2,t3,t4,t5,t6, type="text")

## SHOW ALL MODELS
stargazer(g6,d6,t6, type="text")


#### WRITE TO FILE####
#write.table(articles, "uk_articles_data_notext_notitles.csv")
