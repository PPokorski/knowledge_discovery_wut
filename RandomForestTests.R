#Autor: Adam Deptuła

#instalacja: install.packages('randomForest')
#dodanie biblioteki z lasem losowym:
library(randomForest)
library(plotly)

#packageVersion('plotly') #sprawdzenie wersji plotly

#Położenie danych
directory = "/home/adam/Dokumenty/17_MOW_rf/knowledge_discovery_wut"

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
#Te badania będą dwuczęściowe:
# 1.  Wygenerowanie lasów o różnej liczbie drzew i dla każdego lasu zbadanie błędu OOB
# 2.  Wygenerowanie lasów o różnej liczbie drzew i wyszkolenie lasów na zbiorze treningowym
#     a następnie testowanie na testowym.

# 1.
#Testowanych będzie kilka modeli lasu losowego
#Maksymalna i minimalna liczba drzew w modelach:
min.trees = 2
max.trees = 1000
#Liczba modeli:
count = 30
#podstawa logarytmu wykorzystywanego do wygenerowania wektora zawierającego liczby drzew
#i wykorzystywana do skalowania osi wykresu
lbase = 2
#Wektor przechowujący ilość drzew dla każdego z modeli:
trees = lbase^seq(log(min.trees,lbase), log(max.trees,lbase), length=count)
#Zastosowanie logarytmów w wykładniku jest po to, aby drzewa należały do przedziału min.trees - max.trees

#Zaokraglenia, aby liczby drzew były całkowite:
trees = as.integer(trees)

print("Liczby drzew w testowanych modelach:")
str(trees, vec.len=count)

#Wektor, który będzie przechowywał błędy oob lasów:
ooberrs = vector(length=count)

#Wypełnienie wektora błędów oob
for(id in seq_along(trees))
{
  #liczba drzew w lesie:
  ctree = trees[[id]]
  #Zbudowanie lasu o zadanej liczbie drzew:
  #Ponieważ na wyniki spory wpływ ma losowość, pojedyncza wartość błędu dla danej liczby drzew
  #będzie określana jako wartość średnia z 5 prób dla danej liczby drzew
  tests = numeric(10)
  for(idt in seq_along(tests))
  {
    forest = randomForest(Alc ~ .,data = d.alc, ntree = ctree)
    tests[idt] = forest$err.rate[[ctree]]
  }
  #blad oob drzew liczac do itego drzewa w lesie znajduje sie na itej pozycji err.rate
  ooberrs[id] = mean(tests)
}
#Utworzenie tabeli zawierającej w jednej kolumnie liczbę drzew a w drugiej błąd oob odpowiadający
#lasowi o tej liczbie drzew
trees.vs.ooberrs = data.frame(trees,ooberrs)

#write.csv(t(round(trees.vs.ooberrs,2)), file = paste(directory,"/Klasyfikacja_Wyniki/a_1.csv", sep=""))

#Wykres zależności Błąd OOB(Liczba drzew w lesie)
p.trees.oob = plot_ly(trees.vs.ooberrs, x = ~trees, y = ~ooberrs, type = 'scatter', mode = 'lines') %>%
layout(title = "Zależnośc błędu OOB od liczby drzew w lesie", 
       xaxis = list(type = "log", title="Liczba drzew"),
       yaxis=list(title="Estymacja bledu OOB"))
p.trees.oob

# 2.

#Podział na train i test:
s = sample(x = nrow(d.alc), size = floor(.7*nrow(d.alc)), replace = F)
train = d.alc[s, ]
test  = d.alc[-s, ]

#Wektory, które będą przechowywać błędy na danych testowych i OOB:
rf.testerrs = numeric(count)
rf.trainerrs = numeric(count)

for(id in 1:count)
{
  ctree = trees[[id]]
  
  tests = matrix(nrow = 2, ncol = 5)
  for(idt in seq_along(1:ncol(tests)))
  {
    #Wytrenowanie lasu na treningowym i testowanie na testowym
    forest = randomForest(Alc ~ .,data = train, ntree = ctree,
                               xtest=subset(test,select=-Alc),ytest=test$Alc)
    
    tests[1,idt] = forest$test$err.rate[[ctree]]
    tests[2,idt] = forest$err.rate[[ctree]]
  }
  #blad na tescie
  rf.testerrs[id]= mean(tests[1,])
  #blad OOB
  rf.trainerrs[id] = mean(tests[2,])
}
#rysowanie wykresu Bledu na tescie i OOB od liczby drzew w lesie
trees.test.train = data.frame(trees,rf.testerrs,rf.trainerrs)
p.train.test = plot_ly(data = trees.test.train, x = ~trees, type = 'scatter', mode = 'lines',
                       y = ~rf.testerrs, name = 'Błąd na zbiorze testowym', line = list(color = 'rgb(22, 96, 167)'))%>%
add_trace(y = ~rf.trainerrs, name = 'Błąd OOB', line = list(color = 'rgb(205, 12, 24)')) %>%
layout(xaxis = list(type = "log", title="Liczba drzew"),
       yaxis=list(title="Błąd"))
p.train.test

#Wpływ zmiany parametru liczby atrybutów wybieranych przy podziale węzła na algorytm-------------------
#Testowanych będzie kilka modeli lasu losowego
print(paste("Łączna liczba parametrów: ", (ncol(d.alc)-1)))
#Maksymalna i minimalna liczba drzew w modelach:
min.params = 1
max.params = (ncol(d.alc)-1)
#Liczba modeli:
rf.ids = min.params:max.params
ooberrs = vector(length=length(rf.ids))
for(id in rf.ids)
{
  ctree = 500
  forest = randomForest(Alc ~ .,data = d.alc, ntree = ctree, mtry=id)
  #blad oob drzew liczac do itego drzewa w lesie znajduje sie na itej pozycji err.rate
  ooberrs[id] = forest$err.rate[[ctree]]
}
mtry.obb = data.frame(rf.ids,ooberrs)
#Wykres zależności Błąd OOB(Liczba losowo wybieranych parametrow przy podziale wezla)
p.mtry.obb = plot_ly(mtry.obb, x = ~rf.ids, y = ~ooberrs, type = 'scatter', mode = 'lines') %>%
  layout(title = "Zależnośc błędu OOB od liczby wybieranych atrbutów przy podziale", 
         xaxis = list(title="Liczba losowych atrybutów używanych do podziału węzła"),
         yaxis=list(title="Estymacja błędu OOB"))
p.mtry.obb


#Wpływ długości drzew na wyniki------------------------------------------------------------------------
#Dużo krótkich drzew
#Mało długich drzew
#Dużo długich drzew
#nodesize

#generacja trees - wektora liczb drzew
min.trees=2
max.trees=500
#Liczba modeli:
trees.count = 16
trees.ids = 1:trees.count
#podstawa logarytmu i skali wykladniczej (bo fajnie by bylo, zeby podzialka byla wykladnicza)
lbase = 2
#Wektor przechowujący ilość drzew dla każdego z modeli:
trees = lbase^seq(log(min.trees,lbase), log(max.trees,lbase), length=trees.count)
#Zaokraglenia, aby liczby drzew były całkowite:
trees = as.integer(trees)

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
  for(ts.id in 1:length(trees))
  {
    forest = randomForest(Alc ~ .,data = d.alc, ntree = trees[[ts.id]], nodesize = nodesizes[[ns.id]])
    nodesizes.treescounts[ns.id,ts.id] = forest$err.rate[[trees[[ts.id]]]]
    #ndbigtree to liczba wezlow drzewa dla kazdego drzewa. max(...) daje dlugosc najdluzszego drzewa w lesie
    bigtree.length = min(forest$forest$ndbigtree)
    max.bigtree.length = max(c(bigtree.length,max.bigtree.length))
  }
  print(paste("dla nodesize: ",nodesizes[ns.id],"Rozmiar najwiekszego drzewa: ", max.bigtree.length))
}
colnames(nodesizes.treescounts) = trees
rownames(nodesizes.treescounts) = nodesizes

#ZAMIENIC trees na trees
p.nodesizes.treescounts = plot_ly(z = nodesizes.treescounts,
                                  type="heatmap", colorscale = "Greys", y=nodesizes, x=trees) %>%
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