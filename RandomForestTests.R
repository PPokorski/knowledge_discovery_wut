#Autor: Adam Deptuła

#instalacja: install.packages('randomForest')
#dodanie biblioteki z lasem losowym:
library(randomForest)
library(cluster)
library(plotly)

#packageVersion('plotly') #sprawdzenie wersji plotly

#Polozenie danych
directory = "/home/adam/Dokumenty/17_MOW_rf/knowledge_discovery_wut"

#Polaczenie tabel studentow uczacych sie portugalskiego i matematyki:
d1=read.table(paste(directory,"/student-mat.csv", sep=""),sep=",",header=TRUE)
d2=read.table(paste(directory,"/student-por.csv", sep=""),sep=",",header=TRUE)
d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet","Dalc","Walc"))

grouping_table = d3[,c("Dalc", "Walc")]
fit = kmeans(grouping_table, 2)
d3$Alc = factor(fit$cluster)
#Utworzenie d.alcoholics, ktora stanowi tabele ostatecznie stosowana do nauczania 
#przez usuniecie kolumn Dalc i Walc
d.alc = d3[ , -which(names(d3) %in% c("Dalc","Walc"))]

#Wplyw liczby drzew na wyniki modelu ------------------------------------------------------------------
#Te badania beda dwuczesciowe:
# 1.  Wygenerowanie lasow o roznej liczbie drzew i dla kazdego lasu zbadanie bledu OOB
# 2.  Wygenerowanie lasow o roznej liczbie drzew i wyszkolenie lasow na zbiorze treningowym
#     a następnie testowanie na testowym.

# 1.
#Testowanych będzie kilka modeli lasu losowego
#Maksymalna i minimalna liczba drzew w modelach:
min.trees = 2
max.trees = 1000
#Liczba modeli:
count = 30
#podstawa logarytmu wykorzystywanego do wygenerowania wektora zawierajacego liczby drzew
#i wykorzystywana do skalowania osi wykresu
lbase = 2
#Wektor przechowujacy ilosc drzew dla kazdego z modeli:
trees = lbase^seq(log(min.trees,lbase), log(max.trees,lbase), length=count)
#Zastosowanie logarytmow w wykladniku jest po to, aby drzewa nalezaly do przedzialu min.trees - max.trees

#Zaokraglenia, aby liczby drzew byly calkowite:
trees = as.integer(trees)

print("Liczby drzew w testowanych modelach:")
str(trees, vec.len=count)

#Wektor, ktory bedzie przechowywal bledy oob lasow:
ooberrs = vector(length=count)

#Wypelnienie wektora bledow oob
for(id in seq_along(trees))
{
  #liczba drzew w lesie:
  ctree = trees[[id]]
  #Zbudowanie lasu o zadanej liczbie drzew:
  #Poniewaz na wyniki spory wplyw ma losowosc, pojedyncza wartosc bledu dla danej liczby drzew
  #będzie okreslana jako wartosc srednia z 5 prob dla danej liczby drzew
  tests = numeric(10)
  for(idt in seq_along(tests))
  {
    forest = randomForest(Alc ~ .,data = d.alc, ntree = ctree)
    tests[idt] = forest$err.rate[[ctree]]
  }
  #blad oob drzew liczac do itego drzewa w lesie znajduje sie na itej pozycji err.rate
  ooberrs[id] = mean(tests)
}
#Utworzenie tabeli zawierającej w jednej kolumnie liczbe drzew a w drugiej blad oob odpowiadajacy
#lasowi o tej liczbie drzew
trees.vs.ooberrs = data.frame(trees,ooberrs)

#write.csv(t(round(trees.vs.ooberrs,2)), file = paste(directory,"/Klasyfikacja_Wyniki/a_1.csv", sep=""))

#Wykres zaleznosci Blad OOB(Liczba drzew w lesie)
p.trees.oob = plot_ly(trees.vs.ooberrs, x = ~trees, y = ~ooberrs, type = 'scatter', mode = 'lines') %>%
layout(title = "Zaleznosc bledu OOB od liczby drzew w lesie", 
       xaxis = list(type = "log", title="Liczba drzew"),
       yaxis=list(title="Estymacja bledu OOB"))
p.trees.oob

# 2.

#Podzial na train i test:
s = sample(x = nrow(d.alc), size = floor(.7*nrow(d.alc)), replace = F)
train = d.alc[s, ]
test  = d.alc[-s, ]

#Wektory, ktore będą przechowywac bledy na danych testowych i OOB:
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
                       y = ~rf.testerrs, name = 'Blad na zbiorze testowym', line = list(color = 'rgb(22, 96, 167)'))%>%
add_trace(y = ~rf.trainerrs, name = 'Blad OOB', line = list(color = 'rgb(205, 12, 24)')) %>%
layout(xaxis = list(type = "log", title="Liczba drzew"),
       yaxis=list(title="Blad"))
p.train.test

#Wplyw zmiany parametru liczby atrybutow wybieranych przy podziale wezla na algorytm-------------------
#Testowanych bedzie kilka modeli lasu losowego
print(paste("Laczna liczba parametrow: ", (ncol(d.alc)-1)))
#Maksymalna i minimalna liczba drzew w modelach:
min.params = 1
max.params = (ncol(d.alc)-1)
#Liczba modeli:
rf.ids = min.params:max.params
ooberrs = vector(length=length(rf.ids))
for(id in rf.ids)
{
  ctree = 500
  tests = numeric(20)
  for(idt in seq_along(tests))
  {
    forest = randomForest(Alc ~ .,data = d.alc, ntree = ctree, mtry=id)
    #blad oob drzew liczac do itego drzewa w lesie znajduje sie na itej pozycji err.rate
    tests[idt] = forest$err.rate[[ctree]] 
  }
  ooberrs[id] = mean(tests) 
}
mtry.obb = data.frame(rf.ids,ooberrs)
#write.csv(t(round(mtry.obb,3)), file = paste(directory,"/Klasyfikacja_Wyniki/b_1.csv", sep=""))
#Wykres zaleznosci Blad OOB(Liczba losowo wybieranych parametrow przy podziale wezla)
p.mtry.obb = plot_ly(mtry.obb, x = ~rf.ids, y = ~ooberrs, type = 'scatter', mode = 'lines') %>%
  layout(title = "Błąd OOB od liczby wybieranych atrbutów", 
         xaxis = list(title="Liczba losowych atrybutów używanych do podziału węzła"),
         yaxis=list(title="Estymacja błędu OOB"))
p.mtry.obb


#Wplyw dlugosci drzew na wyniki------------------------------------------------------------------------
#Duzo krotkich drzew
#Malo dlugich drzew
#Duzo dlugich drzew
#nodesize
#Bede analizowal zaleznosć BladOOB (liczba drzew, dlugosc drzew)

#generacja trees - wektora liczb drzew
min.trees=2
max.trees=500
#Liczba modeli:
trees.count = 16
trees.ids = 1:trees.count
#podstawa logarytmu i skali wykladniczej
lbase = 2
#Wektor przechowujacy ilosc drzew dla kazdej dlugosci:
trees = lbase^seq(log(min.trees,lbase), log(max.trees,lbase), length=trees.count)
#Zaokraglenia, aby liczby drzew byly calkowite:
trees = as.integer(trees)

#generacja nodesize.counts - wektora przechowujacego rozne wielkosci lisci
min.nodesize = 1
max.nodesize = 200
nodesizes.count = 10
nodesizes = seq(min.nodesize,max.nodesize,length=nodesizes.count)
nodesizes = as.integer(nodesizes)

nodesizes.treescounts = matrix(nrow = nodesizes.count, ncol = trees.count)

for(ns.id in seq_along(nodesizes))
{
  max.bigtree.length = 0
  for(ts.id in seq_along(trees))
  {
    tests.err.rate = numeric(20)
    tests.ndbigtree = numeric(20)
    for(idt in seq_along(tests.err.rate))
    {
      forest = randomForest(Alc ~ .,data = d.alc, ntree = trees[[ts.id]], nodesize = nodesizes[[ns.id]])
      tests.err.rate[idt] = forest$err.rate[[trees[[ts.id]]]]
      #srednia liczba wezlow drzewa
      tests.ndbigtree[idt] =  mean(forest$forest$ndbigtree)
    }
    
    nodesizes.treescounts[ns.id,ts.id] = mean(tests.err.rate) 
    #ndbigtree to liczba wezlow drzewa dla kazdego drzewa. 
    bigtree.length = mean(tests.ndbigtree)
    #Najwieksza srednia dlugosc drzewa - do wypisywania na konsole, tylko, zeby na szybko zarysowac liczbe drzew
    max.bigtree.length = max(c(bigtree.length,max.bigtree.length))
  }
  print(paste("dla nodesize: ",nodesizes[ns.id],"Najwieksza srednia dl. drzew: ", max.bigtree.length))
}
colnames(nodesizes.treescounts) = trees
rownames(nodesizes.treescounts) = nodesizes
#write.csv(t(round(nodesizes.treescounts,2)), file = paste(directory,"/Klasyfikacja_Wyniki/c_1.csv", sep=""))

p.nodesizes.treescounts = plot_ly(z = nodesizes.treescounts,
                                  type="heatmap", colorscale = "Greys", y=nodesizes, x=trees) %>%
  layout(title = "Wiele krotkich drzew a niewiele dlugich drzew", 
         xaxis = list(title="Liczba drzew"),
         yaxis=list(title="Rozmiar minimalny lisci"))
p.nodesizes.treescounts


#Najistotniejsze atrybuty------------------------------------------------------------------------------
forest = randomForest(Alc ~ .,data = d.alc, importance=TRUE)
importance = forest$importance

importance.sorted = importance[order(-importance[,"MeanDecreaseGini"]),]
importance.sorted
#write.csv(t(round(importance.sorted,2)), file = paste(directory,"/Klasyfikacja_Wyniki/d_1.csv", sep=""))
#importanceSD = forest$importanceSD
