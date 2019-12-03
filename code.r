###############################################################
####                                                       ####​
####   These are the variables and hyperparameters you     ####​
####   will need to set to run the program. Follow the     ####​
####   instructions given in the comments.                 ####​
####                                                       ####​
###############################################################​


# set the working directory  - keep the quotation marks​
setwd("C:/Users/metl023/Desktop/topicmodel")​

# load data - insert the file name here in the form '[name].csv' - keep the quotation marks​
data <- read.csv("Reviews.csv", header=TRUE)​

dim(data)

data <- subset(data, year > 2010)​

dim(data)

# list the column where the text data lives in the form data$NAME (where [NAME] is the column name) - no quotation marks
myText = data$text

# set the columns names for the metadata - keep the quotation marks
myContinuousValue = 'rating'

# set the value - this is the hyperparameter that can be changed - much like with k-means clustering
k = 10

# insert any common words you would like the model to ignore here wrap them in quotation marks. If there are none leave blank​
customStopWords = c("bonnets","broadband","a", "about", "above", "above", "across", "after", "afterwards", "again", "against", "all", "almost", "alone", "along", "already", "also","although","always","am","among", "amongst", "amoungst", "amount",  "an", "and", "another", "any","anyhow","anyone","anything","anyway", "anywhere", "are", "around", "as",  "at", "back","be","became", "because","become","becomes", "becoming", "been", "before", "beforehand", "behind", "being", "below", "beside", "besides", "between", "beyond", "bill", "both", "bottom","but", "by", "call", "can", "cannot", "cant", "co", "con", "could", "couldnt", "cry", "de", "describe", "detail", "do", "done", "down", "due", "during", "each", "eg", "eight", "either", "eleven","else", "elsewhere", "empty", "enough", "etc", "even", "ever", "every", "everyone", "everything", "everywhere", "except", "few", "fifteen", "fify", "fill", "find", "fire", "first", "five", "for", "former", "formerly", "forty", "found", "four", "from", "front", "full", "further", "get", "give", "go", "had", "has", "hasnt", "have", "he", "hence", "her", "here", "hereafter", "hereby", "herein", "hereupon", "hers", "herself", "him", "himself", "his", "how", "however", "hundred", "ie", "if", "in", "inc", "indeed", "interest", "into", "is", "it", "its", "itself", "keep", "last", "latter", "latterly", "least", "less", "ltd", "made", "many", "may", "me", "meanwhile", "might", "mill", "mine", "more", "moreover", "most", "mostly", "move", "much", "must", "my", "myself", "name", "namely", "neither", "never", "nevertheless", "next", "nine", "no", "nobody", "none", "noone", "nor", "nothing", "now", "nowhere", "of", "off", "often", "on", "once", "one", "only", "onto", "or", "other", "others", "otherwise", "our", "ours", "ourselves", "out", "over", "own","part", "per", "perhaps", "please", "put", "rather", "re", "same", "see", "seem", "seemed", "seeming", "seems", "serious", "several", "she", "should", "show", "side", "since", "sincere", "six", "sixty", "so", "some", "somehow", "someone", "something", "sometime", "sometimes", "somewhere", "still", "such", "system", "take", "ten", "than", "that", "the", "their", "them", "themselves", "then", "thence", "there", "thereafter", "thereby", "therefore", "therein", "thereupon", "these", "they", "thick", "thin", "third", "this", "those", "though", "three", "through", "throughout", "thru", "thus", "to", "together", "too", "top", "toward", "towards", "twelve", "twenty", "two", "un", "under", "until", "up", "upon", "us", "very", "via", "was", "we", "well", "were", "what", "whatever", "when", "whence", "whenever", "where", "whereafter", "whereas", "whereby", "wherein", "whereupon", "wherever", "whether", "which", "while", "whither", "who", "whoever", "whole", "whom", "whose", "why", "will", "with", "within", "without", "would", "yet", "you", "your", "yours", "yourself", "yourselves", "the","called","told","said","just","come","dont","asked","got","didnt","going","people","know","customer")​

###############################################################
####  End of variables section   ####
###############################################################​
​
​
​
###############################################################​
####                                                       ####​
####   This part imports the relevant libraries.           ####​
####                                                       ####​
###############################################################​
​
​
# import libraries. You will need to select a mirror (list of countries) from the GUI list​
packages <- c("stm", "Rtsne", "geometry", "RColorBrewer", "tm", "wordcloud")​
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]​
if(length(new.packages)) install.packages(new.packages)​
​
library(stm)​
library(Rtsne)​
library(geometry)​
library(RColorBrewer)​
library(tm)​
library(wordcloud)​
​
###############################################################​
####  End of libraries section   ####​
###############################################################​
​
​
​
###############################################################​
####                                                       ####​
####   This part cleans the data. See the slides from      ####​
####   Wednesday and Friday for more information.          ####​
####                                                       ####​
###############################################################​
​
# stemming/stopwords/etc.​
processed <- textProcessor(documents=myText, metadata = data, customstopwords = customStopWords, stem = FALSE, striphtml = TRUE)​
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)​
​
#output will have object meta, Abstract, and vocab​
docs <- out$documents​
vocab <- out$vocab​
meta <-out$meta​
​
###############################################################​
####  End of data cleaning section         ####​
###############################################################​
​
​
​
###############################################################​
####                                                       ####​
####   This part builds the actual model. See the slides   ####​
####   from Friday for more information.                   ####​
####                                                       ####​
###############################################################​
​
# build the model​
result <- stm(docs, vocab, K = k, prevalence=~ s(eval(as.name(myContinuousValue))), init.type = "LDA", data=meta)​
​
#display model results​
plot(result)​
labelTopics(result, topics = NULL, n = 7, frexweight = 0.5)​
​
###############################################################​
####  End of model build section           ####​
###############################################################​
​
​
​
###############################################################​
####                                                       ####​
####   This part builds the visualisation (tagclouds).     ####​
####   See the slides from Thursday for more information.  ####​
####                                                       ####​
###############################################################​
​
# make wordclouds​
pdf("topic_cloud.pdf")​
for (i in 1:k){​
  cloud(result, topic = i, max.words = 50, random.color = TRUE, colors = brewer.pal(6,"Dark2"))​
  title(paste("Topic ", i))​
}​
dev.off() ​
​
regress <- estimateEffect(c(1:k) ~ s(rating), result, metadata = meta, prior = '1e-5')​
 ​
# print a regression of topic to sentiment​
for (i in c(1:k)) {  ​
   x = i​
   title = paste("trend_topic_",x,".jpg", sep="")​
   jpeg(title)​
   plot(regress, "rating", method = "continuous", topics = x, model = result, ci.level = 0, printlegend = FALSE, xlab = "Rating")​
   dev.off()​
}​
 ​
#end
