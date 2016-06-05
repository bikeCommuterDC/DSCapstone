library(dplyr)
library(stringr)
library(qdapRegex)
options(java.parameters = "-Xmx8gb")
library(rJava)
library(RColorBrewer)
library(reshape2)
library(quanteda)

#import DTMs
setwd("C:/Users/rbechtel/Documents/Coursera/10_Capstone")

p1 = 0.4

#import full data files
twitter.all <- readLines("./raw_data/en_US.twitter.txt", encoding="UTF-8", skipNul = TRUE)
news.all<- readLines("./raw_data/en_US.news.txt", encoding="UTF-8", skipNul = TRUE)
blogs.all <- readLines("./raw_data/en_US.blogs.txt", encoding="UTF-8", skipNul = TRUE)

all.docs <- c(twitter.all, news.all, blogs.all)

set.seed(1234)

numSampleLines = round(length(all.docs) * p1, 0)
sampledLines = sort(sample(1:length(all.docs), numSampleLines, replace = FALSE))

#create sample dataset
sample.data <- all.docs[sampledLines]

#clean sample data
sample.data <- iconv(sample.data, to = "ASCII", from = "UTF-8", sub="") # remove non-alpha numeric characters
sample.data <- gsub("[[:punct:]]","",sample.data) #remove puncuation
sample.data <-gsub("\\S+d\\S+","",sample.data)#removes words with numbers in the middle
sample.data <-gsub("\\d\\S+","",sample.data) #remove words wtih numbers at the beginning
sample.data <-gsub("\\d\\S+","",sample.data)#removes words with numbers at the end
sample.data <- gsub("\\d","",sample.data) #remove isolated numbers
sample.data <- tolower(sample.data) # convert all characters to lowercase
sample.data <- gsub("\\s+"," ",sample.data) #remove extra whitespace
sample.data <- str_trim(sample.data, side = c("both")) # remove white space at beginning or end of string


#saveRDS(sample.data,"./samples/sample_data.RDS")
#sample.data <- readRDS("./samples/sample_data.RDS")

remove(twitter.all, blogs.all, news.all, all.docs)


library(doParallel)
registerDoParallel(cores = 4)

dfm1 <- dfm(sample.data, ngrams=1)
saveRDS(dfm1,"./dfm/dfm1.rds")
remove(dfm1)
dfm2 <- dfm(sample.data, ngrams=2)
saveRDS(dfm2,"./dfm/dfm2.rds")
remove(dfm2)
dfm3 <- dfm(sample.data, ngrams=3)
saveRDS(dfm3,"./dfm/dfm3.rds")
remove(dfm3)
dfm4 <- dfm(sample.data, ngrams=4)
saveRDS(dfm4,"./dfm/dfm4.rds")
remove(dfm4)

dfm1 <- readRDS("./dfm/dfm1.rds")
dfm2 <- readRDS("./dfm/dfm2.rds")
dfm3 <- readRDS("./dfm/dfm3.rds")
dfm4 <- readRDS("./dfm/dfm4.rds")


calcFreq <- function(dfm){
        freq <- colSums(dfm)
        wf <- data.frame(word=names(freq), freq=freq, stringsAsFactors = FALSE) %>% arrange(desc(freq))
        wf$word <- gsub("_"," ",wf$word)
        wf
}

unigram <- calcFreq(dfm1) %>% filter(freq > 1)
bigram <- calcFreq(dfm2) %>% filter(freq > 1)
trigram <- calcFreq(dfm3) %>% filter(freq > 1)
fourgram <- calcFreq(dfm4) %>% filter(freq > 1)

bigram$pre <- word(bigram$word,1,1)
bigram$post <- word(bigram$word,2,2)

trigram$pre <- word(trigram$word,1,2)
trigram$post <- word(trigram$word,3,3)

fourgram$pre <- word(fourgram$word,1,3)
fourgram$post <- word(fourgram$word,4,4)

saveRDS(unigram,"./rds/unigram.rds")
saveRDS(bigram,"./rds/bigram.rds")
saveRDS(trigram,"./rds/trigram.rds")
saveRDS(fourgram,"./rds/fourgram.rds")

unigram <- readRDS("./rds/unigram.rds")
bigram <- readRDS("./rds/bigram.rds")
trigram <- readRDS("./rds/trigram.rds")
fourgram <- readRDS("./rds/fourgram.rds")

d = .2 #discount factor for smoothing

smoothAbsoluteDiscount <- function(ngram){
        
        ngram.unk <- ngram %>%
                group_by(pre) %>%
                summarize(num = n(), freq = num * d) %>%
                mutate(post = "UNK", word = paste(pre, " ", post)) %>%
                select(word, freq, pre, post)
        
        ngram.s <- rbind(ngram, ngram.unk) %>%
                mutate(freq1 = ifelse(post == "UNK", freq, freq - d)) %>%
                arrange(pre, desc(freq))
        
}

bigram.s <- smoothAbsoluteDiscount(bigram)
trigram.s <- smoothAbsoluteDiscount(trigram)
fourgram.s <- smoothAbsoluteDiscount(fourgram)

saveRDS(unigram,"./lookups/unigram.rds")
saveRDS(bigram.s,"./lookups/bigram.rds")
saveRDS(trigram.s,"./lookups/trigram.rds")
saveRDS(fourgram.s,"./lookups/fourgram.rds")

#unigram.s <- readRDS("./lookups/unigram.rds")
bigram.s <- readRDS("./lookups/bigram.rds")
trigram.s <- readRDS("./lookups/trigram.rds")
fourgram.s <- readRDS("./lookups/fourgram.rds")

#remove ngrams with lower probability than "UNK"
reduceNgramTables <- function(ngram.s){
        
        ngram.unk <- ngram.s %>%
                filter(post == "UNK")
        
        ngram.small <- left_join(ngram.s, ngram.unk, by = "pre") %>%
                filter(freq1.x >= freq.y & freq1.x > 1) %>%
                arrange(pre, desc(freq1.x)) %>%
                select(pre, post.x) %>%
                rename(post = post.x)
        
        ngram.small
        
}

bigram.small <- reduceNgramTables(bigram.s)
trigram.small <- reduceNgramTables(trigram.s)
fourgram.small <- reduceNgramTables(fourgram.s)

saveRDS(bigram.small,"./lookups/bigram.small.rds")
saveRDS(trigram.small,"./lookups/trigram.small.rds")
saveRDS(fourgram.small,"./lookups/fourgram.small.rds")


#find the most common endings for bigrams
top.unigrams <- bigram.s %>%
                        group_by(post) %>%
                        summarize(freq = sum(freq)) %>%
                        arrange(desc(freq)) %>%
                        filter(post != "UNK") %>%
                        select(freq, post)
                        

saveRDS(top.unigrams, "./lookups/topUnigrams.rds")

top.unigrams <- readRDS("./lookups/topUnigrams.rds")
bigram.small <- readRDS("./lookups/bigram.small.rds")
trigram.small <- readRDS("./lookups/trigram.small.rds")
fourgram.small <- readRDS("./lookups/fourgram.small.rds")

format(object.size(bigram.small), unit = "MB")
format(object.size(trigram.small), unit = "MB")
format(object.size(fourgram.small), unit = "MB")

predictNextWord <- function(phrase){
        
        #clean the input phrase using the same methods as testing data
        
        print(paste0("The uncleaned phrase is: ",phrase))
        
        phrase <- gsub("[[:punct:]]","",phrase) #remove puncuation
        phrase <-gsub("\\S+d\\S+","",phrase)#removes words with numbers in the middle
        phrase <-gsub("\\d\\S+","",phrase) #remove words wtih numbers at the beginning
        phrase <-gsub("\\d\\S+","",phrase)#removes words with numbers at the end
        phrase <- gsub("\\d","",phrase) #remove isolated numbers
        phrase <- tolower(phrase) # convert all characters to lowercase
        phrase <- gsub("\\s+"," ",phrase) #remove extra whitespace
        phrase <- str_trim(phrase, side = c("both")) # remove white space at beginning or end of string
        
        print(paste0("The cleaned phrase is: ",phrase))
        
        word.count <- sapply(gregexpr("\\S+", phrase), length)
        
        print(paste0("The intital word count is ", word.count))
        
        #reduce the phrase to the last 3 words if it has more than three words
        if(word.count > 3){
                phrase <- word(phrase, word.count - 2, word.count)
                word.count = 3
        }
        
        print(paste0("The revised word count is ", word.count))
        print(paste0("The searched phrase is: ", phrase))
        
        #calculate the number of words in the revised phrase (could be less than 3 if the inital phrase was less than 3 words)
        word.count <- sapply(gregexpr("\\S+", phrase), length)
        
        loop.count <- 1
        
        while(word.count > -1){
                print(paste0("###",loop.count,"###"))
                print(paste0("The word count is: ",word.count))
                print(paste0("The current phrase is: ",phrase))
                
                
                #find matching prefix
                if(word.count == 3){
                        
                        matches <- fourgram.small %>% 
                                filter(pre == phrase)
                        
                }else if(word.count == 2){
                        
                        matches <- trigram.small %>% 
                                filter(pre == phrase)
                        
                }else if(word.count == 1){
                        matches <- bigram.small %>% 
                                filter(pre == phrase)
                }else if(word.count == 0){
                        predictions <- top.unigrams[1:5,]
                }else{
                        print("ERROR #1")
                }
                
                if(nrow(matches) > 0 & matches[1,2] != "UNK"){
                        predictions <- matches[1:5,]
                        break
                }else if(nrow(matches) == 0 | matches[1,2] == "UNK"){
                        print("TRIMMING THE PHRASE")
                        phrase <- word(phrase,-(word.count-1), word.count)
                        word.count <- word.count - 1
                }else {
                        print("Another scenario exists")
                }
                
                loop.count <- 1
                
        }
        
        #predictions[!(predictions[,2] %in% c("UNK", NA)),2]
        
        matches
}
        
a<-predictNextWord("adam sandler")

q1 <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
q2 <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
q3 <- "Hey sunshine, can you follow me and make me the"
q4 <- "Very early observations on the Bills game: Offense still struggling but the"
q5 <- "Go on a romantic date at the"
q6 <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
q7 <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
q8 <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
q9 <- "Be grateful for the good times and keep the faith during the"
q10 <- "If this isn't the cutest thing you've ever seen, then you must be"


q1 <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"


