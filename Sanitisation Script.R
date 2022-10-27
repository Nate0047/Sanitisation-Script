# LIBRARY AND PACKAGES ---------------------------------------------------------
.libPaths("C:/R library")

if(!require("tidyverse")) {install.packages("tidyverse")}
library(tidyverse)
library(magrittr)
if(!require("quanteda")) {install.packages("quanteda")}
library(quanteda)
if(!require("openNLP")) {install.packages("openNLP")}
library(openNLP)
if(!require("NLP")) {install.packages("NLP")}
library(NLP)

# IMPORT DATA ------------------------------------------------------------------
ID <- c(1, 2, 3, 4, 5)
Text <- c("Jeremy hit me over the head with a pan", "I want to go down Grovefield Lane", "this is very personal information Fred", "Bobby wanted John to eat cheese", "Ash said that yesterday")

text.df <- data.frame(ID, Text)
rm(ID, Text)

testcorp <- corpus(text.df$Text, docnames = text.df$ID)

testcorp # aim to extract named entities from this corpus

# CREATE ANNOTATION FUNCTION ---------------------------------------------------
require(openNLP)
require(openNLPdata)

# openNLP annotator objects
sent_token_annotator <- openNLP::Maxent_Sent_Token_Annotator()
word_token_annotator <- openNLP::Maxent_Word_Token_Annotator()
pos_tag_annotator <- openNLP::Maxent_POS_Tag_Annotator()
annotator_pipeline <- NLP::Annotator_Pipeline(
  sent_token_annotator,
  word_token_annotator,
  pos_tag_annotator
)

# function for annotation
annotateDocuments <- function(doc, pos_filter = NULL) {
  doc <- as.String(doc)
  doc_with_annotations <- NLP::annotate(doc, annotator_pipeline)
  tags <- sapply(subset(doc_with_annotations, type=="word")$features, `[[`, "POS")
  tokens <- doc[subset(doc_with_annotations, type=="word")]
  if (!is.null(pos_filter)) {
    res <- tokens[tags %in% pos_filter]
  } else {
    res <- paste0(tokens, "_", tags)
  }
  res <- paste(res, collapse = " ")
  return(res)
}

# APPLY ANNOTATOR TO CORPUS OBJECT ---------------------------------------------

annotated_corpus <- lapply(as.character(testcorp)[1:5], annotateDocuments)

annotated_corpus # works well if names and locations are capitalised. 

# FILTER OUT PERSONAL PRONOUNS -------------------------------------------------

named_entities <- data.frame(sapply(as.character(testcorp)[1:5], annotateDocuments, pos_filter = c("NNP", "NNPS", "")))

named_entities # df of named entities within testcorp - could add to a stop word list?

ls_named_entities <- list(named_entities$sapply.as.character.testcorp..1.5...annotateDocuments..pos_filter...c..NNP...)

# split combine entities - i.e., "Grovefield Road" is split into two entries 
# (in future will be better to leave and do ngram removal, but this is too much work for now).
ls_named_entities <- list(unique(rapply(ls_named_entities, strsplit, split=" ")))

corp_toks <- tokens(testcorp) # breaks corpus down into its tokens

# remove ls_named_entities from corpus tokens.
corp_toks %<>%
  tokens_select(., pattern = ls_named_entities[[1]], selection = "remove", padding = TRUE)

# turn token object into character object for reading back into corpus
corp_toks_tochar <- sapply(corp_toks, paste, collapse = " ")

# create new corpus using sanitised char data, but using original doc vars&names
sanitised_corpus <- corpus(corp_toks_tochar, docnames = text.df$ID)

# OUTPUT SANITISED CORPUS BACK TO EXCEL FORMAT ---------------------------------
sanitised.df <- data.frame(sanitised_corpus)
# need to create new ID column? 
View(sanitised.df)

write_excel_csv(sanitised.df, "sanitised dataframe.csv")
