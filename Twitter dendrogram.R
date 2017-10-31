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

load("my_twitter_ci_data.RData") 
twitter_posts = my_articles

stopword_list = c(stopwords("english"), "use", "data", "microservice", "microservices", "service", "services",
                  "application", "system", "systems", "architecture", "applications", "paper", "can", "based", 
                  "approach", "provide", "based", "using", "software", "twitter", "com", "architecture", "via", 
                  "new", "pic", "need", "want", "like", "one", "will", "development", "model", "models", "also", 
                  "architectures", "used", "present", "environment", "way", "app", "get", "project", "request", 
                  "different", "code", "also", "create", "great", "however", "run", "running", "multiple", "know", 
                  "now", "trying", "example", "micro", "two", "just", "file", "call", "make", "access", "ive", 
                  "another", "best", "question", "message", "don't", "dont", "build", "work", "thanks", "following",
                  # Hierarhical clustering: scopus part 1
                  "environments", "various", "devices", "users", "user", "within", "needed", "propose", 
                  "presents", "specific", "large", "developed", "solutions", "towards", "many",
                  "existing", "well", "small", "provides", "set", "level", "real", "oriented", "process", "proposed",
                  "information", "driven", "results", "approaches", 
                  "show", "processes", "number", "key", "high", "end", "current", "technology",
                  # Hierarhical clustering: scopus part2
                  "platform", "design", "resources", "management", "time", "framework", "network", "research",
                  "components", "support", "implementation", "analysis", "monitoring", "solution", "communication", "context", 
                  "computing", "complexity", "evolution", "open", "migration", "lightweight",
                  "engineering", "challenges", "requirements", "digitalization", "order", "due", "building", "providers", "first",
                  "needs", "business",
                  # Hierarhical clustering: STO
                  "im", "working", "see", "without", "case", "possible", "able", "etc", "every", "something", "currently",
                  "created", "works", "found", "looking", "right", "seems", "find", "id", "add", "main", "good", "say",
                  "think", "start", "files", "method", "idea", "since", "really", "understand", "handle", "cant", "getting",
                  "even", "change", "still", "store", "consul", "doesnt", "tried", "name", "update", "creating", "several",
                  "local", "inside", "class", "fine", "sure", "lets", "eg", "apps", "projects", "anyone", "try", "state", "project",
                  "may", "external", "common", "page", "info", "keep", "custom", "going", "layer", "available", "across", "part",
                  "uses", "done", "point", "image", "share", "core", "already", "write", "send", "version",
                  "please", "help", "issue", "problem", "read", "better", "simple", "module", "go", "lot", "error", "single", "separate",
                  "post",
                  # Hierarhical clustering: Twitter part 1
                  "talk", "learn", "tech", "developer", "programming", "vs", "world", "today", "us", "re", "next", "developers", "big",
                  "check", "don", "dev", "de", "day", "news", "full", "much", "join", "nice", "live", "technologypic", "york",
                  "register", "week", "ready", "th", "ll", "latesting", "take", "look", "und", "london", "native", "talking",
                  "developing", "presentation", "top", "introduction", "watch", "team", "modern", "tomorrow", "ve", "started",
                  "speaking", "teams", "panel", "en", "must", "exhibit", "keynote", "der", "forward", "opportunities", "con", "series",
                  "journey",  "week", "teams", "register", "architect", "last",
                  # Hierarhical clustering: Twitter part 2
                  "meetup", "book", "guide", "summit", "article", "blog", "slides", "workshop", "journal", "video", "job", "future",
                  "interesting", "talks", "cool", "awesome", "love", "things", "lessons", "manage", "managing", "podcast", "die", "principles",
                  "free")

twitter_text <- paste (twitter_posts$Title, twitter_posts$Abstract_clean)
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)


twitter_text = twitter_text %>% removeNumbers %>% removeSpecialChars %>% tolower %>% removePunctuation() %>% stripWhitespace()

source("grouping_script.r")
twitter_text = grouping(twitter_text)

twitter_text =  removeWords(twitter_text, stopword_list)

corpus <- Corpus(VectorSource(twitter_text))

dtm = DocumentTermMatrix(corpus, control=list(stemming=FALSE, wordLengths=c(2, Inf), bounds=list(global=c(5,Inf))))

dtm <- removeSparseTerms(dtm, .9997)
dim(as.matrix(dtm))
df <- dtm %>% as.matrix %>% colSums %>% subset (. >= 750) %>% data.frame(term=names(.), freq=.)

dim(df)

ggplot(df, aes(x = reorder(term, freq), y = freq)) + geom_bar(stat = "identity") +xlab("Terms") + ylab("Count") + coord_flip()

#View(as.matrix(dtm))

#dtm <- dtm[1:200,]
dist.mat <- dist(as.matrix(dtm))
h <- hclust(dist.mat, method = "ward.D")

pdf("Twitter dendogram 1.pdf", width=50, height=50)
plot(h, labels = my_articles$Title, sub = "", cex=0.7, mar=c(5, 8, 4, 1), hang = -1)
#plot(as.phylo(h), cex = .7, label.offset = 1)
#plot(as.phylo(h), type = "fan", cex = .7, label.offset = 1)
dev.off()


