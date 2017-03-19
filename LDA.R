##Building Word Clouds for Tech choice data ####
require(tm)
require(SnowballC)
library(ggplot2)
library(lsa)
library(devtools)
library(wordcloud)
install.packages("curl")
install_github("kasperwelbers/semnet")
install_github("amcat/amcat-r")
install.packages("printr",repos = "http://cran.us.r-project.org/")
library(semnet)


setwd("C:\\Users\\xiyan_wang\\Text")

removeCommonTerms <- function (x, pct) 
{
  stopifnot(inherits(x, c("DocumentTermMatrix", "TermDocumentMatrix")), 
            is.numeric(pct), pct > 0, pct < 1)
  m <- if (inherits(x, "DocumentTermMatrix")) 
    t(x)
  else x
  t <- table(m$i) < m$ncol * (pct)
  termIndex <- as.numeric(names(t[t]))
  if (inherits(x, "DocumentTermMatrix")) 
    x[, termIndex]
  else x[termIndex, ]
}

trust <- read.csv("C:/Users/xiyan_wang/Text/Wireline Data for Concept Mining.csv", dec=",", comment.char="#")

ISP_promoter = subset(trust,Segment == "ISP" & NPS_Recode == "Promoter")
ISP_detractor = subset(trust,Segment == "ISP" & NPS_Recode == "Detractor")
TV_promoter = subset(trust,Segment == "TV" & NPS_Recode == "Promoter")
TV_detractor = subset(trust,Segment == "TV" & NPS_Recode == "Detractor")
Phone_promoter = subset(trust,Segment == "Phone" & NPS_Recode == "Promoter")
Phone_detractor = subset(trust,Segment == "Phone" & NPS_Recode == "Detractor")


trustCorpus <- Corpus(VectorSource(Phone_detractor$NPS2))

toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
trustCorpus <- tm_map(trustCorpus, toSpace, "-")
trustCorpus <- tm_map(trustCorpus, toSpace, "'")
trustCorpus <- tm_map(trustCorpus, toSpace, "'")
trustCorpus <- tm_map(trustCorpus, toSpace, "^")
trustCorpus <- tm_map(trustCorpus, removeNumbers)
trustCorpus <- tm_map(trustCorpus, removePunctuation)
trustCorpus <- tm_map(trustCorpus, removeWords, c(stopwords('english'),"anything","can","one","want","may","will","think","use","way","anyth","alway",
                                                  "too","don","don't","know","seem","there's","there","thing","make","happen","take","much","none","also",
                                                  "already","the","there","someth","someo","feel","cause","become","lot","just","now","sure","people",
                                                  "need","like","mani","get","work","everthing","everth","becaus"))
trustCorpus <- tm_map(trustCorpus, stemDocument)
trustCorpus <- tm_map(trustCorpus, PlainTextDocument)

##check
writeLines(as.character(trustCorpus[[3]]))


###for text network analysis
#first create a TDM (term document matrix)
dtm= DocumentTermMatrix(trustCorpus,control=list(bounds = list(global = c(50,Inf))))
removeCommonTerms(dtm ,.8)
#convert rownames to filenames
#rownames(dtm) <- filenames
#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
freq[ord]
write.csv(freq[ord],"word_freq_nps.csv")


###delete docs without words after processing
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]           #remove all docs without words



#######################
###Layout for igraph###
#######################
##create a termdocMatrix for adjacency matrix
### after Terms that appear in <10 documents are discarded

tdm= TermDocumentMatrix(trustCorpus,control=list(bounds = list(global = c(50,Inf))))
tdm[5:10,1:20]
tdm = as.matrix(tdm)
mode(tdm) = "numeric"
# transform into a term-term adjacency matrix
termMatrix <- tdm %*% t(tdm)
# inspect terms numbered 5 to 10
termMatrix[5:10,5:10]


##build a graph
library(igraph)
# build a graph from the above matrix
g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")

# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
#simplify edges
g_backbone = getBackboneNetwork(g, alpha=0.00001, max.vertices=100)
vcount(g_backbone)
ecount(g_backbone)

##################
###Build a graph##
##################
# set seed to make the layout reproducible
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g_backbone)



# community detection 
g.community<-walktrap.community(g_backbone)
# This algorithm uses random walks to find the most densely connected subgraphs.
members<-membership(g.community)



plot.igraph(g_backbone, layout=layout1)

##plot attribute
#vertex size -> word frequency
#collapse matrix by summing over columns
freq <- rowSums(as.matrix(tdm))
freq = data.frame(freq)
V(g_backbone)$label.cex <- (V(g_backbone)$degree+0.1) / (max(V(g_backbone)$degree)+1)



par(mar=c(.1,.1,.1,.1))    # sets the edges of the plotting area
plot.igraph(g_backbone,
            layout=layout1,
            vertex.size = 0,
            vertex.label.cex=V(g_backbone)$label.cex*1.3,
            vertex.frame.color = NA,
            vertex.color = NA,
            vertex.label.color = ifelse(members==1,"red","blue"),
            edge.arrow.size=.5

)



library('visNetwork') 
g_backbone_vis <- toVisNetworkData(g_backbone)
nodes = g_backbone_vis$nodes

tech  =  visNetwork(nodes, edges = g_backbone_vis$edges)
tech %>% visSave(file = "wireline_tvdetractor.html")




##############################################
########   Load the topic modeling   #########
##############################################

#load topic models library
library(topicmodels)
#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 10
#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm.new,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,10))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))

