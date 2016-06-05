top.unigrams <- readRDS("topUnigrams.rds")
bigram.small <- readRDS("bigram.small.rds")
trigram.small <- readRDS("trigram.small.rds")
fourgram.small <- readRDS("fourgram.small.rds")

predictNextWord <- function(phrase){
        
        #clean the input phrase using the same methods as testing data
        
        #print(paste0("The uncleaned phrase is: ",phrase))
        
        phrase <- gsub("[[:punct:]]","",phrase) #remove puncuation
        phrase <-gsub("\\S+d\\S+","",phrase)#removes words with numbers in the middle
        phrase <-gsub("\\d\\S+","",phrase) #remove words wtih numbers at the beginning
        phrase <-gsub("\\d\\S+","",phrase)#removes words with numbers at the end
        phrase <- gsub("\\d","",phrase) #remove isolated numbers
        phrase <- tolower(phrase) # convert all characters to lowercase
        phrase <- gsub("\\s+"," ",phrase) #remove extra whitespace
        phrase <- str_trim(phrase, side = c("both")) # remove white space at beginning or end of string
        
        #print(paste0("The cleaned phrase is: ",phrase))
        
        word.count <- sapply(gregexpr("\\S+", phrase), length)
        
        #print(paste0("The intital word count is ", word.count))
        
        #reduce the phrase to the last 3 words if it has more than three words
        if(word.count > 3){
                phrase <- word(phrase, word.count - 2, word.count)
                word.count = 3
        }
        
        #print(paste0("The revised word count is ", word.count))
        #print(paste0("The searched phrase is: ", phrase))
        
        #calculate the number of words in the revised phrase (could be less than 3 if the inital phrase was less than 3 words)
        word.count <- sapply(gregexpr("\\S+", phrase), length)
        
        loop.count <- 1
        
        while(word.count > -1){
                #print(paste0("###",loop.count,"###"))
                #print(paste0("The word count is: ",word.count))
                #print(paste0("The current phrase is: ",phrase))
                
                
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
                        phrase <- word(phrase,-(word.count-1), word.count)
                        word.count <- word.count - 1
                }else {
                        print("Another scenario exists")
                }
                
                loop.count <- 1
                
        }
        
        paste(predictions[!(predictions[,2] %in% c("UNK", NA)),2], sep="", collapse = " | ")
        
}
