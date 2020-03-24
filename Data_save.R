library(scholar)
library(timevis)
library(tm)
library(wordcloud)
library(rsconnect)
library(readr)
GS_id<-"aZ4E5I4AAAAJ&hl"
profile<-get_profile(GS_id)
cite.yr<-get_citation_history(GS_id)
pubs<-get_publications(GS_id)
predH<-predict_h_index(GS_id)
saveRDS(profile, "Profile.RDS")
saveRDS(cite.yr, "citeyr.RDS")
saveRDS(pubs, "pubs.RDS")
saveRDS(predH, "predH.RDS")
WoSpapers <- read_delim("WoSpapers.csv", ";", escape_double = FALSE, trim_ws = TRUE)
# Define the folder where the text files are
a <-Corpus(VectorSource(WoSpapers$AB))
a
# Preprocessing text
a <- tm_map(a, removeNumbers) # Not necessary if numbers are important for you
a <- tm_map(a, removePunctuation)
a <- tm_map(a , stripWhitespace)
a <- tm_map(a, tolower)
# Stopwords are words such as "we" "the" "and" "so", etc. You can add your own words to the list
a <- tm_map(a, removeWords, c(stopwords("english"), "can", "also", "may"))
# a <- tm_map(a, stemDocument, language = "english") # You can also do steamming if you want

# Computing the term document matrix
tdm <- TermDocumentMatrix(a)

# Transforming data for wordcloud
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
saveRDS(d, "d.RDS")

timeline_dat <- data.frame(
  id      = 1:4,
  content = c("University of Pretoria","World Pheasant Association", "Newcastle University",
              "Norwegian Institute for Nature Research"),
  start   = c("2009-01-01","2011-04-01", "2012-12-01",
              "2019-01-04"),
  end     = c("2012-01-01","2012-11-01", "2018-12-31",NA)
)

saveRDS(timeline_dat,"timeline_dat.RDS")
