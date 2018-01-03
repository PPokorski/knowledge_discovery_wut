library(cluster)
library(plotly)

d1=read.table("D:\\User\\Pobrane\\student-alcohol-consumption\\mow_ws\\student-mat.csv",sep=",",header=TRUE)
d2=read.table("D:\\User\\Pobrane\\student-alcohol-consumption\\mow_ws\\student-por.csv",sep=",",header=TRUE)

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet","Dalc","Walc"))

clara_sizes = c(1, 10, 100, 300, nrow(d3))
metric_types = c("euclidean", "manhattan")
# kmeans + pam with each metric + clara with each size and each metric
number_of_grouping = 1 + length(metric_types) * (1 + length(clara_sizes))
number_of_classes = 2

alco = matrix(nrow=5, ncol=5)

for(i in 1:dim(alco)[1])
{
    for(j in 1:dim(alco)[2])
    {
        alco[i,j] = sum(d3$Dalc == i & d3$Walc == j, na.rm=TRUE)
    }
}
p = plot_ly(z = alco, type="heatmap")
p

grouping_table = d3[,c("Dalc", "Walc")]

for(i in 1:number_of_grouping)
{
    if(i == 1)
    {
        print("Using kmeans!")
        fit = kmeans(grouping_table, number_of_classes)
    }
    else if(i == 2 || i == 3)
    {
        print("Using pam!")
        cat("Metric_types:", metric_types[i - 1], "\n")
        fit = pam(grouping_table, number_of_classes, metric = metric_types[i - 1])
    }
    else
    {
        print("Using clara!")
        cat("Metric_types:", metric_types[(i %% length(metric_types)) + 1], "\n")
        cat("Number of samples:", clara_sizes[(i - 1) %/% length(metric_types) - 1], "\n")
        fit = clara(grouping_table, number_of_classes,
                    metric = metric_types[(i %% length(metric_types)) + 1],
                    samples = clara_sizes[i %/% length(metric_types) - 1], pamLike=TRUE)
    }
    
    d3$cluster = fit$cluster

    r = ggplotly(ggplot(d3, aes(x = Dalc, 
                        y = Walc,
                        color = factor(cluster))) +
                        geom_point(alpha = 0.1))

    print(r)
}