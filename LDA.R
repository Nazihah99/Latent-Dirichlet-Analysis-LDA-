library(tm)
library(tidytext)
library(topicmodels)
library(tidyr)
library(ggplot2)
library(dplyr)
library(gridExtra)
docs = Corpus(DirSource("C:/Users/Lenovo/Downloads/TextFile3/TextFile",encoding="UTF-8"))

# Data acquisition
docs = tm_map(docs, content_transformer(tolower)) # Transform to lower case
toSpace = content_transformer(function(x, pattern) {return(gsub(pattern, " ", x))})
docs = tm_map(docs, gsub, pattern = '“', replacement = '')
docs = tm_map(docs, gsub, pattern = '”', replacement = '')
docs = tm_map(docs, gsub, pattern = '’', replacement = '')
docs = tm_map(docs, gsub, pattern = '‘', replacement = '')
docs = tm_map(docs, gsub, pattern = '—', replacement = '')
docs = tm_map(docs, removePunctuation) # remove punctuation
docs = tm_map(docs, removeWords, c("local","new", "executives", "per", "science", "government", "development", "digital", "order",
                                   "uob", "malaysia",  "2020", "2021", "2023", "2022","20212030", "2030", "industry", "billion",
                                   "national", "different", "increase", "course", "subject", "subjects", "must", "added", 
                                   "used", "datuk",  "plan", "malaysias", "technology", "technologies", "services", "work",
                                   "potential", "content", "future", "economy","starting", "year", "sabah", "kuala", "lumpur",
                                   "introduced", "country","ministry", "region", "area", "hub", "areas", "significant", 
                                   "minister", "improve", "interest", "bhd", "various", "fresh","many", "companies", "roadmap",
                                   "businesses", "international", "users", "programme", "state", "towards", "russia", "across", 
                                   "implementation", "customers", "prices", "ensure", "sdn", "precision", "include", 
                                   "malaysian", "world", "system", "benefits", "data", "solutions", "provide","production",
                                   "energy", "sector", "develop", "players", "reduce", "million", "number", "cent", "level",
                                   "sharing", "impact","act", "time", "strategic", "support", "economic", "public", "global",
                                   "chairman", "market", "products", "aims", "company", "chargers", "charging", "google",
                                   "create", "part", "ecosystem", "startup", "programmes", "startups", "promote", "power",
                                   "told", "countries", "start", "quality", "field", "product", "growth", 
                                   "business", "management", "countrys", "according", "based", "among", "nation", "advanced",
                                   "tawau", "kinabalu", "johor", "investment", "policy", "due", "goal", "phase", "creativity"))
docs = tm_map(docs, removeWords, stopwords("english")) # remove stopwords
docs = tm_map(docs, stripWhitespace) # remove whitespace
myStopwords = c("can", "say", "said", "one", "way", "use", "also", "howev", "tell", "will", "much", "need",  "take", 
                "tend", "even","like","particular","rather","said","get","well","make","ask","come","end", "first","two",
                "help","often","may","might","see","someth","thing","point","post","look","right","now","think","'ve ", 
                "including","'re ")
docs = tm_map(docs, removeWords, myStopwords) # remove custom stopwords

# Create document term matrix
tdm = DocumentTermMatrix(docs)

# Perform topic modeling
ap_lda = LDA(tdm, k = 4, control = list(seed = 123))

# i. Extract the per-topic-per-word-probabilities
ap_topics = tidy(ap_lda, matrix = "beta")
ap_topics

# Find the top 10 terms for each topic
ap_top_terms = ap_topics %>% group_by (topic) %>% top_n (8, beta) %>% ungroup() %>% arrange(topic, -beta)
print(ap_top_terms, n=100)
# Visualize the top terms for each topic
ap_top_terms %>%   mutate(term = reorder(term, beta)) %>%   ggplot(aes(term, beta, fill = factor(topic))) +   geom_col(show.legend = FALSE) +   facet_wrap(~ topic, scales = "free") +   coord_flip()

# ii. Extract the relevant beta spread.
beta_spread = ap_topics %>% mutate (topic=paste0("topic",topic)) %>% spread(topic,beta) %>%
  filter (topic1>0.005 | topic2 > 0.005) %>% mutate(log_ratio = log2(topic2/topic1))
plot1 = beta_spread %>%   mutate(term = reorder(term, log_ratio)) %>%  ggplot(aes(term, log_ratio)) +
  geom_col(show.legend = FALSE, fill = "seagreen") + coord_flip() + labs(title = "topic1>0.005 | topic2 > 0.005") 

beta_spread = ap_topics %>% mutate (topic=paste0("topic",topic)) %>% spread(topic,beta) %>%
  filter (topic1>0.005 | topic3 > 0.005) %>% mutate(log_ratio = log2(topic3/topic1))
plot2 = beta_spread %>%   mutate(term = reorder(term, log_ratio)) %>%  ggplot(aes(term, log_ratio)) +
  geom_col(show.legend = FALSE, fill = "pink") + coord_flip() + labs(title = "topic1>0.005 | topic3 > 0.005") 

beta_spread = ap_topics %>% mutate (topic=paste0("topic",topic)) %>% spread(topic,beta) %>%
  filter (topic1>0.005 | topic4 > 0.005) %>% mutate(log_ratio = log2(topic4/topic1))
plot3 = beta_spread %>%   mutate(term = reorder(term, log_ratio)) %>%  ggplot(aes(term, log_ratio)) +
  geom_col(show.legend = FALSE, fill = "purple") + coord_flip() + labs(title = "topic1>0.005 | topic4 > 0.005") 

beta_spread = ap_topics %>% mutate (topic=paste0("topic",topic)) %>% spread(topic,beta) %>%
  filter (topic2>0.005 | topic3 > 0.005) %>% mutate(log_ratio = log2(topic3/topic2))
plot4 = beta_spread %>%   mutate(term = reorder(term, log_ratio)) %>%  ggplot(aes(term, log_ratio)) +
  geom_col(show.legend = FALSE, fill = "darkblue") + coord_flip() + labs(title = "topic2>0.005 | topic3 > 0.005") 

beta_spread = ap_topics %>% mutate (topic=paste0("topic",topic)) %>% spread(topic,beta) %>%
  filter (topic2>0.005 | topic4 > 0.005) %>% mutate(log_ratio = log2(topic4/topic2))
plot5 = beta_spread %>%   mutate(term = reorder(term, log_ratio)) %>%  ggplot(aes(term, log_ratio)) +
  geom_col(show.legend = FALSE, fill = "red") + coord_flip() + labs(title = "topic2>0.005 | topic4 > 0.005") 

beta_spread = ap_topics %>% mutate (topic=paste0("topic",topic)) %>% spread(topic,beta) %>%
  filter (topic3>0.005 | topic4 > 0.005) %>% mutate(log_ratio = log2(topic4/topic3))
plot6 = beta_spread %>%   mutate(term = reorder(term, log_ratio)) %>%  ggplot(aes(term, log_ratio)) +
  geom_col(show.legend = FALSE, fill = "cyan") + coord_flip() + labs(title = "topic3>0.005 | topic4 > 0.005") 

figure = grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 2)

# iii. Extract the per-document-per-topic-probabilities
ap_documents=tidy(ap_lda,matrix="gamma") 
print(ap_documents, n=144)

# iv. Relevant Analysis -- Term with highest frequency in text document
p=tidy(tdm) %>% group_by(document) %>% arrange(desc(count)) %>% slice(1)
print(p,n=40)
