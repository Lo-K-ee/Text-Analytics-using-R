library(tidytext)
library(tidyverse)
library(ggpubr) #to combine the ggplots
library(stopwords) #to remove the stopwords
library(ggrepel)
# load the 'ted_talks' data
#=========================
setwd("C:/Files/UoE/Modules/Spring/MA331-7-SP Programming and Text Analytics with R/Report")
load(file="ted_talks.rda")
MyData <- ted_talks %>%
  filter(speaker %in% c("Arthur Benjamin", "Scott McCloud"))

glimpse(MyData)

#Extracting the words & removing the stopwords in MyData

MyData <- MyData %>% 
  unnest_tokens(word, text)

MyData <- MyData %>%
  anti_join(get_stopwords())

#######################WORD COUNT GRAPH#####################
  
word_count <- MyData %>%
  count(speaker, sort=FALSE)

ggplot(word_count, aes(n, speaker)) +
  geom_bar(stat = "identity", position = "dodge", fill = 'cornflowerblue') +
  ggtitle("The Speakers' word counts")  +
  geom_text(aes(label=n),position=position_dodge(width=2), hjust=1.2, color = 'white')

######################Different Speeches###################

speech_count <- MyData %>%
  count(talk_id, speaker, headline)

speech_count %>% ggplot(aes(x=speaker, y=n, fill = headline)) + 
  geom_col(width = 0.7 ,position = "dodge") + 
  ggtitle("Word count per speaker's speech") +
  geom_text(aes(label=n), position=position_dodge(width=0.7), vjust=2, color = 'white')

#######################LAB 7#######################
#Counting top words of Arthur_Benjamin in both the talks

Arthur_Benjamin <- MyData %>%
  filter(speaker == "Arthur Benjamin") %>%
  count(speaker, word, sort=TRUE)

#Counting the top words of Scott_McCloud

Scott_McCloud <- MyData %>%
  filter(speaker == "Scott McCloud") %>%
  count(speaker, word, sort=TRUE)

#Visualisation of top words

A_B_topwords <- Arthur_Benjamin %>%
  slice_max(n, n=30) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n,word)) + 
  geom_col(fill = 'cornflowerblue') + ggtitle("Arthur Benjamin's Top Words")

S_M_topwords <- Scott_McCloud %>%
  slice_max(n, n=40) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n,word)) + geom_col(fill = 'cornflowerblue') +
  ggtitle("Scott McCloud's Top Words")

#Combining Top words graphs
ggarrange(A_B_topwords, S_M_topwords)


#Comparing Speaker words
bind_rows(Arthur_Benjamin, Scott_McCloud) %>%
  group_by(word) %>%
  filter(sum(n) > 10) %>%
  ungroup() %>%
  pivot_wider(names_from="speaker",
              values_from = "n",
              values_fill = 0) %>%
  ggplot(aes(`Scott McCloud`, `Arthur Benjamin`)) + 
  geom_abline(color="red2", size = 1.2, alpha = 0.8, lty = 2) +
  geom_text_repel(aes(label=word), max.overlaps = 15, label.size = 0.25, label.r = 0.15) + 
  ggtitle("Comparison of Top words by the Ted Speakers")


#######################LAB 8#######################
library(tidytext)
library(textdata)

#Compute_OR function from dsEssex package
compute_OR <- function(numerator, denominator, correction = TRUE){
  if(correction){
    ODDS_N = (numerator + 0.5) / (sum(numerator) - numerator + 0.5)
    ODDS_D = (denominator + 0.5) / (sum(denominator) - denominator + 0.5)
  } else {
    ODDS_N = numerator / (sum(numerator) - numerator)
    ODDS_D = denominator / (sum(denominator) - denominator)
  }
  return(OR = ODDS_N/ODDS_D)
}

#nrc Sentiment Analysis

speakers_senti <- rbind(Arthur_Benjamin, Scott_McCloud)

speakers_senti <- speakers_senti %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  count(speaker, sentiment)

senti_table2 <- head(speakers_senti, 10)
colnames(senti_table2)[which(colnames(senti_table2)=="n")] <- "Arthur_Benjamin"
senti_table_Arthur <- subset(senti_table2, select = c("sentiment", "Arthur_Benjamin"))

senti_table1 <- tail(speakers_senti, 10)
colnames(senti_table1)[which(colnames(senti_table1)=="n")] <- "Scott_McCloud"
senti_table_Scott <- subset(senti_table1, select = ("Scott_McCloud"))

senti_table <- cbind(senti_table_Arthur, senti_table_Scott)

senti_table <- senti_table %>% 
  mutate(OR = compute_OR(Arthur_Benjamin, Scott_McCloud, correction = FALSE), 
         log_OR = log(OR), sentiment = reorder(sentiment, log_OR))

#nrc Sentiment analysis plot

senti_table

senti_table %>%
  ggplot(aes(sentiment, log_OR, fill = log_OR < 0)) +
  geom_col(show.legend = TRUE) +
  ylab("Log odds ratio") + ggtitle("The association between sentiments") +
  coord_flip() + 
  scale_fill_manual(name = "", values = c("green", "red"))

########################LAB 9##############################

#bing Sentiment Analysis

bing <- get_sentiments("bing")

#Arthur Benjamin's words
Arthur_Benjamin_bing <- MyData %>%
  filter(speaker == "Arthur Benjamin") %>%
  anti_join(get_stopwords()) %>%
  count(speaker, word, sort=TRUE) %>%
  inner_join(get_sentiments("bing"), by = "word")

Arthur_Benjamin_bing$n[Arthur_Benjamin_bing$sentiment == 'negative'] <- -1 * 
  Arthur_Benjamin_bing$n[Arthur_Benjamin_bing$sentiment == 'negative']

ggplot(Arthur_Benjamin_bing, aes(word, n)) + 
  geom_col(fill = 'cornflowerblue') +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1), ) +
  ylab("number of words") +
  ggtitle("Sentiment analysis of Arthur Benjamin's words")

#Scott McCloud's words
Scott_McCloud_bing <- MyData %>%
  filter(speaker == "Scott McCloud") %>%
  anti_join(get_stopwords()) %>%
  count(speaker, word, sort=TRUE) %>%
  inner_join(get_sentiments("bing"), by = "word")

Scott_McCloud_bing$n[Scott_McCloud_bing$sentiment == 'negative'] <- -1 * 
  Scott_McCloud_bing$n[Scott_McCloud_bing$sentiment == 'negative']

ggplot(Scott_McCloud_bing, aes(word, n)) + 
  geom_col(fill = 'cornflowerblue') +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
  ylab("number of words") +
  ggtitle("Sentiment analysis of Scott McCloud's words")

#Percentage of sentiment in Arthur's words
Arthur_sentiment_percent <- MyData %>%
  filter(speaker == "Arthur Benjamin") %>%
  anti_join(get_stopwords()) %>%
  count(speaker, word, sort=TRUE) %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(sentiment) %>%
  mutate(total = sum(n), percent = n/total) %>%
  arrange(desc(percent))

#Percentage of sentiment in Scott's words
Scott_sentiment_percent <- MyData %>%
  filter(speaker == "Scott McCloud") %>%
  anti_join(get_stopwords()) %>%
  count(speaker, word, sort=TRUE) %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(sentiment) %>%
  mutate(total = sum(n), percent = n/total) %>%
  arrange(desc(percent))
  
#Pie visualization of sentiment percentages

par(mfrow=c(1,2))

pie(Arthur_sentiment_percent$percent, 
    Arthur_sentiment_percent$percent*100, 
    xlab="Arthur Benjamin",
    col=c("springgreen2", "coral1"))
mtext("Sentiment Percent",side=3,line=-4,outer=TRUE, cex = 2, font = 2)
pie(Scott_sentiment_percent$percent, 
    Scott_sentiment_percent$percent*100, 
    xlab="Scott McCloud",
    col=c("springgreen2", "coral1"))

legend("topright", c("Positive","Negative"), cex = 0.8,
       fill = c("springgreen2", "coral1"))  

