#instalacja: install.packages('randomForest')
#dodanie biblioteki z lasem losowym:
library(randomForest)
library(plotly)
packageVersion('plotly')

#Położenie danych
directory = "/home/adam/Dokumenty/17_MOW_randomforest"

#Połączenie tabel studentów uczących się portugalskiego i matematyki:
d1=read.table(paste(directory,"/student-mat.csv", sep=""),sep=",",header=TRUE)
d2=read.table(paste(directory,"/student-por.csv", sep=""),sep=",",header=TRUE)
d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet","Dalc","Walc"))

#Zgodnie z wynikami grupowania, zamiast kolumn Dalc i Walc tworzona jest kolumna alcoholic:
#Dodanie kolumny alc, i ustawienie 1 i 2 w zależności od parametrów Dalc i Walc
d3$Alc[d3$Dalc==1 & d3$Walc == 1] = "abstainer" #Niepijacy
d3$Alc[!(d3$Dalc==1 & d3$Walc == 1)] = "alcoholic" #Pijacy
#Zrobienie z kolumny typu wyliczeniowego:
d3$Alc = as.factor(d3$Alc)
#Utworzenie d.alcoholics, która stanowi tabelę ostatecznie stosowaną do nauczania 
#przez usunięcie kolumn Dalc i Walc
d.alc = d3[ , -which(names(d3) %in% c("Dalc","Walc"))]

#Wpływ liczby drzew na wyniki modelu ------------------------------------------------------------------

#Testowanych będzie kilka modeli lasu losowego
#Maksymalna i minimalna liczba drzew w modelach:
min.trees = 2
max.trees = 1000
#Liczba modeli:
rf.count = 30
rf.ids = 1:rf.count
#podstawa logarytmu i skali wykladniczej (bo fajnie by bylo, zeby podzialka byla wykladnicza)
lbase = 2
#Wektor przechowujący ilość drzew dla każdego z modeli:
trees.counts = lbase^seq(log(min.trees,lbase), log(max.trees,lbase), length=rf.count)
#Zaokraglenia, aby liczby drzew były całkowite:
trees.counts = as.integer(trees.counts)

print("Liczby drzew w testowanych modelach:")
str(trees.counts, vec.len=rf.count)

#Wektor, który będzie przechowywał błędy oob lasów:
rf.ooberrs = vector(length=rf.count)

for(id in rf.ids)
{
  ctree = trees.counts[[id]]
  forest = randomForest(Alc ~ .,data = d.alc, ntree = ctree)
  #blad oob drzew liczac do itego drzewa w lesie znajduje sie na itej pozycji err.rate
  rf.ooberrs[id] = forest$err.rate[[ctree]]
}
#Utworzenie tabelki zawierającej w jednej kolumnie liczbę drzew a w drugiej błędt
trees.vs.ooberrs = data.frame(trees.counts,rf.ooberrs)

#Wykres zależności Błąd OOB(Liczba drzew w lesie)
p.rcount.oob = plot_ly(trees.vs.ooberrs, x = ~trees.counts, y = ~rf.ooberrs, type = 'scatter', mode = 'lines') %>%
layout(title = "Zależnośc błędu OOB od liczby drzew w lesie", 
       xaxis = list(type = "log", title="Liczba drzew"),
       yaxis=list(title="Estymacja bledu OOB"))
p.rcount.oob

#Sprawdzenie nadmiernego dopasowania:

#Podział na train i test:
s = sample(x = nrow(d.alc), size = floor(.7*nrow(d.alc)), replace = F)
train = d.alc[s, ]
test  = d.alc[-s, ]

#Wektory, które będą przechowywać błędy na danych testowych i treningowych:
rf.testerrs = vector(length=rf.count)
rf.trainerrs = vector(length=rf.count)
print(nrow(train))
print(nrow(test))
for(id in rf.ids)
{
  ctree = trees.counts[[id]]
  forest.test = randomForest(Alc ~ .,data = train, ntree = ctree,
                        xtest=subset(test,select=-Alc),ytest=test$Alc)
  #blad na tescie
  rf.testerrs[id]=forest.test$test$err.rate[[ctree]]
  
  forest.train = randomForest(Alc ~ .,data = train, ntree = ctree,
                        xtest=subset(train,select=-Alc),ytest=train$Alc)
  #blad na treningu
  rf.trainerrs[id] = forest.train$test$err.rate[[ctree]]
}
#rysowanie wykresu Bledu na tescie i treningu od liczby drzew w lesie
trees.test.train = data.frame(trees.counts,rf.testerrs,rf.trainerrs)
p.train.test = plot_ly(data = trees.test.train, x = ~trees.counts, type = 'scatter', mode = 'lines',
                       y = ~rf.testerrs, name = 'Błąd na zbiorze testowym', line = list(color = 'rgb(22, 96, 167)')) %>%
add_trace(p.train.test, y = ~rf.trainerrs, name = 'Błąd na zbiorze treningowym', line = list(color = 'rgb(205, 12, 24)')) %>%
layout(p.train.test, title = "Zależnośc błędów na zbiorach od liczby drzew w lesie", 
       xaxis = list(type = "log", title="Liczba drzew"),
       yaxis=list(title="Blad"))
p.train.test

#Wpływ zmiany parametru liczby atrybutów wybieranych przy podziale węzła na algorytm-------------------
#Testowanych będzie kilka modeli lasu losowego
print(paste("Łączna liczba parametrów: ", (ncol(d.alc)-1)))
#Maksymalna i minimalna liczba drzew w modelach:
min.params = 1
max.params = (ncol(d.alc)-1)
#Liczba modeli:
rf.ids = min.params:max.params
rf.ooberrs = vector(length=length(rf.ids))
for(id in rf.ids)
{
  ctree = 500
  forest = randomForest(Alc ~ .,data = d.alc, ntree = ctree, mtry=id)
  #blad oob drzew liczac do itego drzewa w lesie znajduje sie na itej pozycji err.rate
  rf.ooberrs[id] = forest$err.rate[[ctree]]
}
mtry.obb = data.frame(rf.ids,rf.ooberrs)
#Wykres zależności Błąd OOB(Liczba losowo wybieranych parametrow przy podziale wezla)
p.mtry.obb = plot_ly(mtry.obb, x = ~rf.ids, y = ~rf.ooberrs, type = 'scatter', mode = 'lines') %>%
  layout(title = "Zależnośc błędu OOB od liczby wybieranych atrbutów przy podziale", 
         xaxis = list(title="Liczba losowych atrybutów używanych do podziału węzła"),
         yaxis=list(title="Estymacja błędu OOB"))
p.mtry.obb


#Wpływ długości drzew na wyniki------------------------------------------------------------------------
#Dużo krótkich drzew
#Mało długich drzew
#Dużo długich drzew
#nodesize

#generacja trees.counts - wektora liczb drzew
min.trees=2
max.trees=500
#Liczba modeli:
trees.count = 16
trees.ids = 1:trees.count
#podstawa logarytmu i skali wykladniczej (bo fajnie by bylo, zeby podzialka byla wykladnicza)
lbase = 2
#Wektor przechowujący ilość drzew dla każdego z modeli:
trees.counts = lbase^seq(log(min.trees,lbase), log(max.trees,lbase), length=trees.count)
#Zaokraglenia, aby liczby drzew były całkowite:
trees.counts = as.integer(trees.counts)

#generacla nodesize.counts - wektora wielkosci w lisciach
min.nodesize = 1
max.nodesize = 200
nodesizes.count = 10
nodesizes = seq(min.nodesize,max.nodesize,length=nodesizes.count)
nodesizes = as.integer(nodesizes)

nodesizes.treescounts = matrix(nrow = nodesizes.count, ncol = trees.count)

for(ns.id in 1:length(nodesizes))
{
  max.bigtree.length = 0
  for(ts.id in 1:length(trees.counts))
  {
    forest = randomForest(Alc ~ .,data = d.alc, ntree = trees.counts[[ts.id]], nodesize = nodesizes[[ns.id]])
    nodesizes.treescounts[ns.id,ts.id] = forest$err.rate[[trees.counts[[ts.id]]]]
    #ndbigtree to liczba wezlow drzewa dla kazdego drzewa. max(...) daje dlugosc najdluzszego drzewa w lesie
    bigtree.length = min(forest$forest$ndbigtree)
    max.bigtree.length = max(c(bigtree.length,max.bigtree.length))
  }
  print(paste("dla nodesize: ",nodesizes[ns.id],"Rozmiar najwiekszego drzewa: ", max.bigtree.length))
}
colnames(nodesizes.treescounts) = trees.counts
rownames(nodesizes.treescounts) = nodesizes

#ZAMIENIC trees.counts na trees
p.nodesizes.treescounts = plot_ly(z = nodesizes.treescounts,
                                  type="heatmap", colorscale = "Greys", y=nodesizes, x=trees.counts) %>%
  layout(title = "Wiele krótkich drzew a niewiele długich drzew", 
         xaxis = list(title="Liczba drzew"),
         yaxis=list(title="Rozmiar minimalny liści"))
p.nodesizes.treescounts


#Najistotniejsze atrybuty------------------------------------------------------------------------------
forest = randomForest(Alc ~ .,data = d.alc, importance=TRUE)
importance = forest$importance

importance.sorted = importance[order(-importance[,"MeanDecreaseGini"]),]
importance.sorted

#importanceSD = forest$importanceSD