##Building Word Clouds for Tech choice data ####
require(tm)
require(SnowballC)
require(wordcloud)
library(ggplot2)
library(lsa)
library(scatterplot3d)
library(devtools)
install.packages("curl")
install_github("kasperwelbers/semnet")
install.packages("printr",repos = "http://cran.us.r-project.org/")
library(semnet)
library(printr)

trust <- read.csv("C:/Users/xiyan_wang/Text/trust.csv", dec=",", comment.char="#")

trustCorpus <- Corpus(VectorSource(trust$Trust))

trustCorpus <- tm_map(trustCorpus, PlainTextDocument)
trustCorpus <- tm_map(trustCorpus, removePunctuation)
trustCorpus <- tm_map(trustCorpus, removeWords, stopwords('english'))
trustCorpus <- tm_map(trustCorpus, stemDocument)

wordcloud(trustCorpus, max.words = 100, random.order = FALSE)

# MDS with raw term-document matrix compute distance matrix
td.mat <- as.matrix(TermDocumentMatrix(trustCorpus))
td.mat
dist.mat <- dist(t(as.matrix(td.mat)))
dist.mat  # check distance matrix

td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat)  # weighting
lsaSpace <- lsa(td.mat.lsa)  # create LSA space
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace)))  # compute distance matrix
dist.mat.lsa  # check distance mantrix

# MDS
fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 2)
points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y, 
                                                                  color = df$view)) + geom_text(data = points, aes(x = x, y = y - 0.2, label = row.names(df)))


#------------------------------------------------------------------------------

# plot
fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 2)
colors <- rep(c("blue", "green", "red"), each = )
scatterplot3d(fit$points[, 1], fit$points[, 2], fit$points[, 3], color = par("col"), 
              pch = 16, main = "Semantic Space Scaled to 3D", xlab = "x", ylab = "y", 
              zlab = "z", type = "h")

qplot(fit$points[, 1], fit$points[, 2], geom = "point",main = "Semantic Space Scaled to 2D")





