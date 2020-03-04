library(tm)
library(NLP)
library(qdap) # Quantitative discourse analysis of transcripts.
library(qdapDictionaries)
library(dplyr) # Data wrangling, pipe operator %>%().
library(RColorBrewer) # Generate palette of colours for plots.
library(ggplot2) # Plot word frequencies.
library(scales) # Include commas in numbers.
library(wordcloud)
library(dplyr)
library(stringr)


#rm(list = ls())
#ls()
#getwd()
#setwd("/Users/andrecarvalho/Desktop/TM")


rm(list = ls())
ls()
getwd()
setwd("D:/TM5")


cps <- Corpus(DirSource('D:/TM5',
                        encoding = "UTF-8"),
              readerControl = list(language = "pt"))


cps <- Corpus(DirSource("/Users/andrecarvalho/Desktop/TM2",
                        encoding = "UTF-8"),
              readerControl = list(language = "pt"))


cps <- tm_map(cps, stripWhitespace)
cps <- tm_map(cps, content_transformer(tolower))
cps <- tm_map(cps, removeWords, stopwords("portuguese"))
#cps <- tm_map(cps, stemDocument)
cps <- tm_map(cps, removeNumbers)
cps <- tm_map(cps, removePunctuation)
cps <- tm_map(cps, removeWords, c("falo", "galeno", "senhor","proqu","a","A","agora","ainda","pra","tÃ¡","dÃ¡","lÃ¡","tÃ´","nÃ©",
                                  "algum","alguma","algumas","alguns","antes","senhora","quatro","aqui","aÃ","conta", "faz", "hoje",   
                                  "ao","Ao","aos","aqui","as","se","As","assim","Assim","bem","zero", "vinte", "dia", "coisa", "coisas",     
                                  "das","dava","de","dela","dele","dentro","dizer", "dizia", "do","muita", "venha", "entrar", "opinião",  
                                  "dos", "dous","e","E","ela","ele","eles","isso","isto","la","em","Em","ter", "ano","liga","longo","lógico",
                                  "fora","fosse","gente","lhe","lo","logo","mais","mas","Mas","me","como","muitas","quais","tantas","tatiana",  
                                  "meus","mim","na","pelo", "Pois", "por", "este","diz","então",  "expndexpndtwkerning", "vagas","vezes","lado",
                                  "nas","nem","no","No","nos","nossa","o","com", "é", "cielo" , "expandedcolortblcssrgbccccssrgbccc","têm","nisso",
                                  "O", "os", "Os", "ou","para", "pela", "D", "aí", "né", "vou", "cento","robfs","servieo","toda","todo","todos","tudo",
                                  "que","Que","queria","se","Se","sem","da", "r", "tá", "colortblredgreenblueredgreenblueredgreenblue","deftab","meo","neo",   
                                  "senhor","senhora","seu","seus","si","sua","tal", "helveticaneue","ffs", "helveticaneue" , "cocoatextscalingcocoaplatformfonttblffnilfcharset",
                                  "paperwpaperhmarglmargrviewwviewhviewkind","pardpardeftabslpartightenfactor","rtfansiansicpgcocoartf","enteo","cartfes","inteligeancia",
                                  "obstrued","substitued",
                                  "ve","sf","cb","cf",
                                  "inteligeancia","robf","entanto","cor",
                                  "tanto", "tarde","doi","veio", "vi", "vai"))

################################################
###############################################
##    DTM


dtm <- DocumentTermMatrix(cps)
dim(dtm)


findFreqTerms(dtm, lowfreq = 10)

freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)

length(freq)
head(freq, 10)
tail(freq, 10)

ord <- order(freq)

dtmr <- DocumentTermMatrix(cps, contro=list(wordLengths=c(4, 20), bounds = list(global = c(5,45))))
dim(dtmr)

dtms <- removeSparseTerms(dtm, 0.7)
dim(dtms)
inspect(dtms)


write.csv(freq, file = "frequencies.csv")

findAssocs(dtm,terms = "artificial",0.41)
findAssocs(dtm, c("artificial", "humano", "trabalho"), c(0.7, 0.7, 0.7))

plot(dtm, terms = names(findAssocs(dtm,term="artificial",0.8)[["artificial"]]), corThreshold = 0.80)

plot(dtm, terms = names(findAssocs(dtm,term="artificial",0.7)[["humano"]]), corThreshold = 0.80, attrs=list(node=list(label="foo", fillcolor="lightgreen", fontsize="16", shape="ellipse"), edge=list(color="cyan"), graph=list(rankdir="LR")))

# remove sparse terms
dtm2 <- removeSparseTerms(dtm, sparse = 0.70)
m2 <- as.matrix(dtm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")


plot(fit)
rect.hclust(fit, k = 6) # modelo com 6 clusters

### wordcloud

set.seed(142)
wordcloud(names(freq), freq, max.words = 100)
wordcloud(names(freq), freq, min.freq = 100)
wordcloud(names(freq), freq, min.freq = 100, colors = brewer.pal(6, "Dark2"))
wordcloud(names(freq), freq, min.freq = 100, scale=c(5, .1), colors = brewer.pal(6, "Dark2"))

words <- dtm %>%
  as.matrix %>%
  colnames %>%
  (function(x) x[nchar(x) < 20])

length(words)

head(words, 5)

summary(nchar(words))

table(nchar(words))

dist_tab(nchar(words))

data.frame(nletters=nchar(words))                     %>%
  ggplot(aes(x=nletters))                             +
  geom_histogram(binwidth = 1)                        +
  geom_vline(xintercept = mean(nchar(words)),
             colour="green", size=1, alpha= .5)       +
  labs(x="Numero de Letras", y="Numero de palavras")

words %>%
  str_split("") %>%
  sapply(function(x) x[-1]) %>%
  unlist %>%
  dist_tab %>%
  mutate(Letter=factor(toupper(interval),
                       levels=toupper(interval[order(freq)]))) %>%
  ggplot(aes(Letter, weight=percent)) +
  geom_bar() +
  coord_flip() +
  labs(y="Proportion") +
  scale_y_continuous(breaks=seq(0, 12, 2),
                     label=function(x) paste0(x, "%"),expand=c(0,0), limits=c(0,12))
                     
  
  words %>%
    lapply(function(x) sapply(letters, gregexpr, x, fixed=TRUE)) %>%
    unlist %>%
    (function(x) x[x!=-1]) %>%
    (function(x) setNames(x, gsub("nnd", "", names(x)))) %>%
    (function(x) apply(table(data.frame(letter=toupper(names(x)),
                                        position=unname(x))),
                       1, function(y) y/length(x))) %>%
    qheat(high="green", low="yellow", by.column=NULL,
          values=TRUE, digits=3, plot=FALSE) +
    labs(y="Letter", x="Position") +
    theme(axis.text.x=element_text(angle=0)) +
    guides(fill=guide_legend(title="Proportion"))  
  
  
  
  
  
################################################
################################################

tdm <- TermDocumentMatrix(cps,
                          control = list(wordLengths = c(1, Inf)))



(freq.terms <- findFreqTerms(tdm, lowfreq = 15))

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 10)
df <- data.frame(term = names(term.freq), freq = term.freq)

library(ggplot2)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()

findAssocs(tdm, "não", 0.1)


m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]

library(wordcloud)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F, colors = pal)

tdm2 <- removeSparseTerms(tdm, sparse = 0.90)
m2 <- as.matrix(tdm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D2")

plot(fit)
rect.hclust(fit, k = 5)

