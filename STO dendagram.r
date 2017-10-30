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

#load("my_scopus_ci_data.RData")
#scopus_articles = my_articles
load("my_STO_ci_data.RData")
sto_questions = my_articles
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
                  # new!! scopus part 1
                  "environments", "various", "devices", "users", "user", "within", "needed", "propose", 
                  "presents", "specific", "large", "developed", "solutions", "towards", "many",
                  "existing", "well", "small", "provides", "set", "level", "real", "oriented", "process", "proposed",
                  "information", "driven", "results", "approaches", 
                  "show", "processes", "number", "key", "high", "end", "current", "technology",
                  # new!! scopus part2
                  "platform", "design", "resources", "management", "time", "framework", "network", "research",
                  "components", "support", "implementation", "analysis", "monitoring", "solution", "communication", "context", 
                  "computing", "complexity", "evolution", "open", "migration", "lightweight",
                  "engineering", "challenges", "requirements", "digitalization", "order", "due", "building", "providers", "first",
                  "needs", "business",
                  # STO part 1
                  "im", "working", "see", "without", "case", "possible", "able", "etc", "every", "something", "currently",
                  "created", "works", "found", "looking", "right", "seems", "find", "id", "add", "main", "good", "say",
                  "think", "start", "files", "method", "idea", "since", "really", "understand", "handle", "cant", "getting",
                  "even", "change", "still", "store", "consul", "doesnt", "tried", "name", "update", "creating", "several",
                  "local", "inside", "class", "fine", "sure", "lets", "eg", "apps", "projects", "anyone", "try", "state", "project",
                  "may", "external", "common", "page", "info", "keep", "custom", "going", "layer", "available", "across", "part",
                  "uses", "done", "point", "image", "share", "core", "already", "write", "send", "version",
                  "please", "help", "issue", "problem", "read", "better", "simple", "module", "go", "lot", "error", "single", "separate",
                  "post"
                  # STO part 2
                  )

sto_text <- paste (sto_questions$Title, sto_questions$Abstract_clean)

source("grouping_script.r")
sto_text = grouping(sto_text)

#sto_text <- paste (sto_questions$Title, sto_questions$Abstract_clean)
#twitter_text <- paste (twitter_posts$Title, twitter_posts$Abstract_clean)

removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)

sto_text = sto_text %>% removeNumbers %>% removeSpecialChars %>% tolower %>% removePunctuation() %>% stripWhitespace()
#twitter_text = twitter_text %>% removeNumbers %>% removeSpecialChars %>% tolower

sto_text =  removeWords(sto_text, stopword_list)
#twitter_text =  removeWords(twitter_text,stopword_list)


corpus <- Corpus(VectorSource(sto_text))

dtm = DocumentTermMatrix(corpus, control=list(stemming=FALSE, wordLengths=c(2, Inf), bounds=list(global=c(5,Inf))))
dtm <- removeSparseTerms(dtm, .975)
df <- dtm %>% as.matrix %>% colSums %>% subset (. >= 200) %>% data.frame(term=names(.), freq=.)

ggplot(df, aes(x = reorder(term, freq), y = freq)) + geom_bar(stat = "identity") +xlab("Terms") + ylab("Count") + coord_flip()

#View(as.matrix(dtm))
dim(as.matrix(dtm))
length(my_articles$Title)

#dtm <- dtm[1600:2000,]
dist.mat <- dist(as.matrix(dtm))
h <- hclust(dist.mat, method = "ward.D")

pdf("STO dendogram 6.pdf", width=50, height=50)
plot(h, labels = my_articles$Title, sub = "", cex=0.7, mar=c(5, 8, 4, 1), hang = -1)
#plot(as.phylo(h), cex = .7, label.offset = 1)
#plot(as.phylo(h), type = "fan", cex = .7, label.offset = 1)
dev.off()


