library(tesseract)#for OCR can read image,pdf and 
library(tidyverse)
library(tm)
library(SnowballC)
library(magick) #for image preprocessing
library(wordcloud)
library(rpart)
library(rpart.plot)
library(e1071)
library(nnet)
library(stringr)
library(tokenizers)

#install.packages("tokenizers")
#library(tokenizers)

#library(readImage)

#install.packages("readImage")
tesseract_info()
tesseract_download("nld")
tesseract_download("hin")

#eng <- tesseract("eng")
#let <- tesseract::ocr("letter 1", engine = eng)
#cat(let)

let <- ocr("let1.png", engine = tesseract("eng"))
let
#a = cat(let)#we can see that data is read properly
#a
#let = as.String(let)
#let = as.data.frame(let)
let = strsplit(let,"\n")
#aa = tokenize_paragraphs(let, paragraph_break = '\n\n', simplify = FALSE)
#aa
bb=c()
for (i in seq(1,length(let[[1]]))){
  k = tokenize_lines(let[[1]][i], simplify = FALSE)
  bb=c(bb,k)
}
bb
#bb = tokenize_lines(aa, simplify = FALSE)
#bb

let2 =ocr("let2.png", engine = tesseract("eng"))
let2
#let2 = as.String(let2)

hin = ocr("hin1.png", engine = tesseract("hin")) #it sometimes work some times doesn't

input <- image_read("hin2.png")
text <- input %>%
  image_resize("2000x") %>%
  image_convert(type = 'Grayscale') %>%
 
  image_write(format = 'png', density = '300x300') %>%
  tesseract::ocr(engine = "hin") 

cat(text)

#let3 = as.data.frame(matrix(0, ncol = 1, nrow = 0))
#colnames(let3) <- c("Data")


let3 = paste(let, let2)
let3 = as.String(let3)
let3

image_read("book.png") %>%
  image_ocr() %>%
  cat()

# Convert sentences into a Corpus
corp <- Corpus(VectorSource(bb))

# Compute Term Frequency
tdm1 <- TermDocumentMatrix(corp)
inspect(tdm1)

## Tokenization (may or may not need it)
corp2 <- tm_map(corp, stripWhitespace) 
corp3 <- tm_map(corp2, removePunctuation)

corp4 <- tm_map(corp3, removeWords, stopwords("en"))

## Stemming
corp5 <- tm_map(corp4, stemDocument) 

tdm2 <- TermDocumentMatrix(corp5)
inspect(tdm2)

## Term Frequency - Inverse Document Frequency
tfidf <- weightTfIdf(tdm2)
inspect(tfidf)

dtm <- DocumentTermMatrix(corp5)
inspect(dtm)

findFreqTerms(dtm, 4)

dtm_tfidf <- DocumentTermMatrix(corp5, control = list(weighting = weightTfIdf))
dtm_tfidf = removeSparseTerms(dtm_tfidf, 0.95)
dtm_tfidf

freq = data.frame(sort(colSums(as.matrix(dtm_tfidf)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2"))

### i have trouble tokenizing the text
#1. punctuations are not breaking sentance
#2. that's y tfidf is not reliable
#3. since tokenization is not working can't process further for classification etc.
#4. need appointment for friday regarding this.
