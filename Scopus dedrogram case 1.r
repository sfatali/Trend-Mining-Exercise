rm(list = ls())

if ("ggplot2" %in% installed.packages() == FALSE)
{
  install.packages("ggplot2", dependencies = TRUE,repos='http://cran.rstudio.com/')
}
if ("tm" %in% installed.packages() == FALSE)
{
  install.packages("tm", dependencies = TRUE,repos='http://cran.rstudio.com/')
  
}

if ("magrittr" %in% installed.packages() == FALSE)
{
  install.packages("magrittr", dependencies = TRUE,repos='http://cran.rstudio.com/')
}

if ("ape" %in% installed.packages() == FALSE)
{
  install.packages("ape", dependencies = TRUE,repos='http://cran.rstudio.com/')
}

library(ggplot2)
library(tm)
library(magrittr)

library(ape)

setwd('C:\\My space\\EMSE - Oulu\\Semester 1\\Emerging Trends In Software Engineering\\excercise\\TrendMining-master\\data')

load("my_scopus_ci_data.RData")
scopus_articles = my_articles
#load("my_STO_ci_data.RData")
#sto_questions = my_articles
#load("my_twitter_ci_data.RData") 
#twitter_posts = my_articles

stopword_list = c(stopwords("english"), "use", "data", "microservice", "microservices", "service", "services",
                  "application", "system", "systems", "architecture", "applications", "paper", "can", "based", 
                  "approach", "provide", "based", "using", "software", "twitter", "com", "architecture", "via", 
                  "new", "pic", "need", "want", "like", "one", "will", "development", "model", "models", "also", 
                  "architectures", "used", "present", "environment", "way", "app", "get", "project", "request", 
                  "different", "code", "also", "create", "great", "however", "run", "running", "multiple", "know", 
                  "now", "trying", "example", "micro", "two", "just", "file", "call", "make", "access", "ive", 
                  "another", "best", "question", "message", "don't", "dont", "build", "work", "thanks", "following",
                  # new!!
                  "environments", "various", "devices", "users", "user", "within", "needed", "propose", 
                  "presents", "specific", "large", "developed", "solutions", "towards", "many",
                  "existing", "well", "small", "provides", "set", "level", "real", "oriented", "process", "proposed",
                  "information", "driven", "results", "approaches", 
                  "show", "processes", "number", "key", "high", "end", "current", "technology") #, "solution", "process")

scopus_text <- paste (scopus_articles$Title, scopus_articles$Abstract_clean)

source("grouping_script.r")
scopus_text = grouping(scopus_text)

#sto_text <- paste (sto_questions$Title, sto_questions$Abstract_clean)
#twitter_text <- paste (twitter_posts$Title, twitter_posts$Abstract_clean)

#write.csv(scopus_text, file="scopus_text.csv")

removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)

scopus_text = scopus_text %>% removeNumbers %>% removeSpecialChars %>% tolower %>% removePunctuation() %<% stripWhitespace()
#sto_text = sto_text %>% removeNumbers %>% removeSpecialChars %>% tolower
#twitter_text = twitter_text %>% removeNumbers %>% removeSpecialChars %>% tolower

scopus_text =  removeWords(scopus_text, stopword_list)
#sto_text =  removeWords(sto_text,stopword_list)
#twitter_text =  removeWords(twitter_text,stopword_list)

corpus <- Corpus(VectorSource(scopus_text))

dtm = DocumentTermMatrix(corpus, control=list(stemming=FALSE, wordLengths=c(2, Inf), bounds=list(global=c(5,Inf))))

df <- dtm %>% as.matrix %>% colSums %>% subset (. >= 56) %>% data.frame(term=names(.), freq=.)

ggplot(df, aes(x = reorder(term, freq), y = freq)) + geom_bar(stat = "identity") +xlab("Terms") + ylab("Count") + coord_flip()

#dtm <- dtm[1:200,]
#dtm <- removeSparseTerms(dtm, .9)
matrix = as.matrix(dtm)
rownames(matrix) = my_articles$Title

dist.mat <- dist(matrix)
h <- hclust(dist.mat, method = "ward.D")

pdf("Scopus dendogram 1.pdf", width=50, height=50)
#par(cex=0.7, mar=c(5, 8, 4, 1))
plot(h, labels = my_articles$Title, sub = "", cex=0.7, mar=c(5, 8, 4, 1), hang = -1)
plot(as.phylo(h), cex = .7, label.offset = 1)
plot(as.phylo(h), type = "fan", cex = .7, label.offset = 1)
dev.off()

#ggdendrogram(h, labels = my_articles$Title, rotate = TRUE)
