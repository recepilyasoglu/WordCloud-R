# gerekli kutuphane tanimlamalari
library(XML)
library(tm)
library(corpus)
library(wordcloud)
library(wordcloud2)
library(SnowballC)
library(RColorBrewer)
library(ggplot2)

res <- XML::readHTMLTable(paste0('http://cran.r-project.org/web/packages/available_packages_by_name.html'), which = 1)

# Desteklenen metin dosyasi bicimlerini gormek icin getReaders islevini kullanabiliriz
getReaders()
getSources()

v <- Corpus(VectorSource(res$V2))
v

# ilk bes belgeyi görmek için corpus'ta aşagidaki komutu calistirabiliriz
inspect(head(v, 5))

#arastirmada alakassz tüm verileri filtrelemek için kullanlabilir 
# donusturme yontemlerinin listesini gormek icin getTransformations islevini kullanabiliriz
getTransformations()

#Temizleme sürecindeki ilk adim, engellenen kelimeleri kaldirmaktır. 
#tm paketi zaten asagidakilerin listesini tutar 
#farklı dillerde stopwords. Listeyi ingilizce görüntülemek için asagidaki işlevi kullaniriz
stopwords("english")

# stopwords kelimelerini derlemimizden cikarmak icin 
# tm_map fonksiyonunu asagidaki gibi kullanabiliriz
removeWords('To be or not to be.', stopwords("english"))

# stopwords sözcüklerini derlemimizden çıkarmak için tm_map işlevini aşağıdaki gibi kullanabiliriz
v <- tm_map(v, removeWords, stopwords("english"))

# derlemimizin ilk beş girişini tekrar inceleyelim
inspect(head(v, 5))

# noktalama isaretlerini ve kucuk harflerin cıikarildigini gorebiliriz
v <- tm_map(v, content_transformer(tolower))
v <- tm_map(v, removePunctuation)
v <- tm_map(v, stripWhitespace)
inspect(head(v, 5))

# Veri Gorsellestirme
wordcloud::wordcloud(v)

# Modeli gelistirmek

# hala gereksiz kelimeler ve sayilar bulunuyor bunlari kaldirmak icin
v <- tm_map(v, removeNumbers)

tdm <- TermDocumentMatrix(v)

# kelimelerin gecme sikliklarina goz atalim
inspect(tdm[1:10, 1:25])

# herbir sozcuk icin toplum olusum sayisini cikarabiliriz
# acikklamalarda gecen tum terimleri en az 100 kez gosterelim
findFreqTerms(tdm, lowfreq = 100)

# yukaridaki listeden bizimle ilgisi olmayan kelimeler icin 
# kendiş stopWords fonksiyonumuzu yazabiliriz
myStopwords <- c('the','via','package','based','using')

# bu kelimeleri derlemimizden cikaralim
v <- tm_map(v, removeWords, myStopwords)

# isimlerin çoğul hallerini kaldıralım. Bunun için stemming algoritmalarını kullanabiliriz
wordStem(c('dogs','walk','print','printed','printer','printing'))

# kelimelri farkli bir nesneye kopyalama
d <- v

# burada Snowballc paketinden stemDocument fonksiyonunu cagirdik
v <- tm_map(v, stemDocument, language = "english")

# kendi fonksiyonumuzu yaziyoruz
# buradaki amac, her belgeyi bir boslukla kelimelere bolmek
stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary = dictionary)
  x <- paste(x, sep = "", collapse = " ")
  PlainTextDocument(stripWhitespace(x))
}
v <- lapply(v, stemCompletion2, dictionary = d)  

# VCorpus'a donusturmek icin aşagidaki komutu calistiriyoruz
v <- Corpus(VectorSource(v))  
tdm <- TermDocumentMatrix(v)
findFreqTerms(tdm, lowfreq = 100)  

dtm <- TermDocumentMatrix(v)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 10)

if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

# Onceki parcacigin ciktisi
set.seed(1234)
wordcloud::wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                     max.words = 200, random.order = FALSE, rot.per = 0.35,
                     colors = brewer.pal(8, "Dark2"))


# en sik kullanilan ilk 10 kelimenin grafigi
barplot(d[1:10, ]$freq, las = 2, names.arg = d[1:10,]$word,
        col = "lightblue", main = "En Sık Kullanılan Kelimeler", 
        ylab = "Kelime Frekansları")

