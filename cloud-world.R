library(tm)
library(ggplot2)
library(lsa)
library(twitteR)
library(class)
library(wordcloud)
library(RColorBrewer)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)


consumer_key <- "YRAvjFDwiu5h8YsluLT1SxTDi"
consumer_secret <- "OWiImz1h6JcpBetu2tf1ffw9fzsgDnFDdbmsQvc16BOts7gFg5"
access_token <- "734595186513305600-gqZvqC5DnA6zI1pf8pva3pzijBPxnCM"
access_secret <- "ua9JcXDT2RnM7ifVspnd1e8e36CM7Q2AORp0H0qA5bQfV"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


mach_tweets = searchTwitter("HARVEY OR TROPICAL HARVEY OR #HARVEY", n=2000, lang="en", since="2017-08-26")
mach_text = sapply(mach_tweets, function(x) x$getText())
# create a corpus
mach_corpus = Corpus(VectorSource(mach_text))
dataTwitte = twListToDF(mach_tweets)
mach_corpus <- tm_map(mach_corpus,function(row) iconv(row, "latin1", "ASCII", sub=""))
mach_corpus <- tm_map(mach_corpus, function(x) removeWords(x, stopwords("english")))
mach_corpus <- tm_map(mach_corpus, stemDocument, language = "english")
mach_corpus <- tm_map(mach_corpus, removePunctuation)
mach_corpus <- tm_map(mach_corpus, tolower)


# create document term matrix applying some transformations
tdm = TermDocumentMatrix(mach_corpus,control = list(removePunctuation = TRUE,removeNumbers = TRUE, tolower = TRUE))


#Create document-term matrix
dtm <- DocumentTermMatrix(mach_corpus)


# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)





#Number of topics
k <- 10

#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE




#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))



#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("C:\\Users\\mmoghadasi\\Desktop\\ttu\\LDA\\LDAGibbs",k,"DocsToTopics.csv"))


#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,6))
write.csv(ldaOut.terms,file=paste("C:\\Users\\mmoghadasi\\Desktop\\ttu\\LDA\\LDAGibbs",k,"TopicsToTerms.csv"))


ap_lda <- LDA(dtm, k = 10, control = list(seed = 1234))
ap_topics <- tidy(ap_lda, matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("C:\\Users\\mmoghadasi\\Desktop\\ttu\\LDA\\LDAGibbs",k,"TopicProbabilities.csv"))



# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# save the image in png format
png("MachineLearningCloud.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()
