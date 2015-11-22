library(tm)
library(SnowballC)
library(RWeka)
library(rJava)
library(RWekajars)
##top 20 user review text and stars
top20rev<-top20rev[c("text","stars")]
top20rev$stars<-ifelse(top20rev$stars==1,"one",
                       ifelse(top20rev$stars==2,"two",
                              ifelse(top20rev$stars==3,"three",
                                     ifelse(top20rev$stars==4,"four",
                                            "five"))))
top20revFN<-function(x) {
  top20rev[which(top20rev$stars==x),]
}
top20rev_one<-top20revFN("one")
top20rev_two<-top20revFN("two")
top20rev_three<-top20revFN("three")
top20rev_four<-top20revFN("four")
top20rev_five<-top20revFN("five")

textFN<-function(x){
  paste(x$text,x$stars,sep=" ")
}

text1_top<-textFN(top20rev_one)
text2_top<-textFN(top20rev_two)
text3_top<-textFN(top20rev_three)
text4_top<-textFN(top20rev_four)
text5_top<-textFN(top20rev_five)

TMFN<-function(x,y) {
  z<-Corpus(VectorSource(x))
  z<-tm_map(z, tolower)
  z<-tm_map(z, removePunctuation)
  z<-tm_map(z, removeNumbers)
  myStopwords<-c(stopwords('english'), "yelp")
  z<-tm_map(z, removeWords, myStopwords)
  z<-tm_map(z, stemDocument)
  z<-tm_map(z, PlainTextDocument)
  dtm<-TermDocumentMatrix(z,control= list(wordLengths = c(1, Inf)))
  findAssocs(dtm, y, 0.15);
}

onestarterms_top<-TMFN(text1_top,"one")
twostarterms_top<-TMFN(text2_top,"two")
threestarterms_top<-TMFN(text3_top,"three")
fourstarterms_top<-TMFN(text4_top,"four")
fivestarterms_top<-TMFN(text5_top,"five")


##after top 20 user review text and stars
allrevAfter<-allrevAfter[c("text","stars")]
allrevAfter$stars<-ifelse(allrevAfter$stars==1,"one",
                       ifelse(allrevAfter$stars==2,"two",
                              ifelse(allrevAfter$stars==3,"three",
                                     ifelse(allrevAfter$stars==4,"four",
                                            "five"))))
allrevAfterFN<-function(x) {
  allrevAfter[which(allrevAfter$stars==x),]
}
allrevAfter_one<-allrevAfterFN("one")
allrevAfter_two<-allrevAfterFN("two")
allrevAfter_three<-allrevAfterFN("three")
allrevAfter_four<-allrevAfterFN("four")
allrevAfter_five<-allrevAfterFN("five")

textFN<-function(x){
  paste(x$text,x$stars,sep=" ")
}

text1_next<-allrevAfterFN(allrevAfter_one)
text2_next<-allrevAfterFN(allrevAfter_two)
text3_next<-allrevAfterFN(allrevAfter_three)
text4_next<-allrevAfterFN(allrevAfter_four)
text5_next<-allrevAfterFN(allrevAfter_five)

onestarterms_next<-TMFN(text1_next,"one")
twostarterms_next<-TMFN(text2_next,"two")
threestarterms_next<-TMFN(text3_next,"three")
fourstarterms_next<-TMFN(text4_next,"four")
fivestarterms_next<-TMFN(text5_next,"five")

