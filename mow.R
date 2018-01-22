# Author: Piotr Pokorski

library(cluster)
library(plotly)

# Function used to calculate the qualiy metrics:
# 1. Inside cluster sum of squared distanes WC
# 2. Between cluster sum of squared distanes BC
# 3. Mixed metric WC / BC
calculate_quality <- function(data1, data2, centers)
{
    wc_quality = sum ((data1$Dalc - centers[1, 1])^2 + (data1$Walc - centers[1, 2])^2) + 
                 sum ((data2$Dalc - centers[2, 1])^2 + (data2$Walc - centers[2, 2])^2)

    bc_quality = (centers[1, 1] - centers[2, 1])^2 + (centers[1, 2] - centers[2, 2])^2

    mixed_quality = wc_quality / bc_quality

    print(paste("WC quality:", wc_quality, sep=" "))
    print(paste("BC quality:", bc_quality, sep=" "))
    print(paste("Mixed quality: ", mixed_quality, sep=" "))
}

# Load the data from the files. Please change the path if yours is different.
d1=read.table("D:\\User\\Pobrane\\student-alcohol-consumption\\mow_ws\\student-mat.csv",sep=",",header=TRUE)
d2=read.table("D:\\User\\Pobrane\\student-alcohol-consumption\\mow_ws\\student-por.csv",sep=",",header=TRUE)

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet","Dalc","Walc"))

# The numbers of samples used for CLARA grouping. All of them will be tested
clara_sizes = c(1, 10, 100, 300, nrow(d3))
# What metrics should be tested for CLARA and PAM grouping
metric_types = c("euclidean", "manhattan")
# kmeans + PAM with each metric + CLARA with each samples number and each metric
number_of_grouping = 1 + length(metric_types) * (1 + length(clara_sizes))
number_of_classes = 2

# Will be used to generate heatmap of number of students according to their alcohol consumption
alco = matrix(nrow=5, ncol=5)

for(i in 1:dim(alco)[1])
{
    for(j in 1:dim(alco)[2])
    {
        # Count how much students where assigned Dalc = i and Walc = j
        alco[i,j] = sum(d3$Dalc == i & d3$Walc == j, na.rm=TRUE)
    }
}

# Generate heatmap
x_label = list(title = "Day-to-day alcohol consumption")
y_label = list(title = "Weekend alcohol consumption")
p = plot_ly(z = alco, type="heatmap") %>%
layout(title="Liczba studentow spozywajacych dane ilosci alkoholu",
       xaxis=x_label,
       yaxis=y_label)
p

# Table used for grouping, contains only relevant attributes
grouping_table = d3[,c("Dalc", "Walc")]

for(i in 1:number_of_grouping)
{
    # K-means method
    if(i == 1)
    {
        print("Using kmeans!")
        fit = kmeans(grouping_table, number_of_classes)
        centers=as.data.frame(fit$centers)
        plot_title = "Grouping using K-means method"
    }
    # PAM method
    else if(i == 2 || i == 3)
    {
        print("Using pam!")
        cat("Metric_types:", metric_types[i - 1], "\n")
        fit = pam(grouping_table, number_of_classes, metric = metric_types[i - 1])
        centers=as.data.frame(fit$medoids)

        plot_title = paste("Grouping using PAM method. Metric:", metric_types[i - 1], "metric", sep= " ")
    }
    # CLARA method
    else
    {
        print("Using clara!")
        cat("Metric_types:", metric_types[(i %% length(metric_types)) + 1], "\n")
        cat("Number of samples:", clara_sizes[(i - 1) %/% length(metric_types) - 1], "\n")
        fit = clara(grouping_table, number_of_classes,
                    metric = metric_types[(i %% length(metric_types)) + 1],
                    samples = clara_sizes[i %/% length(metric_types) - 1], pamLike=TRUE)
        centers=as.data.frame(fit$medoids)

        plot_title = paste("Grouping using CLARA method. Metric:", metric_types[(i %% length(metric_types)) + 1],
                           "Samples number:", clara_sizes[(i - 1) %/% length(metric_types) - 1], sep= " ")
    }

    # Add cluster number to original data
    d3$cluster = factor(fit$cluster)

    # Calculate quality metrics for both clusters
    calculate_quality(d3[d3$cluster == 1,], d3[d3$cluster == 2,], centers)

    # For each grouping method generate plot showing the grouping result
    r = ggplotly(ggplot(d3, aes(x = Dalc, y = Walc, color = cluster)) +
                        geom_point() +
                        geom_point(data=centers, aes(Dalc, Walc, color=factor(1:2)), pch=8, size=10) +
                        labs(title=plot_title, x = "Day-to-day alcohol consumption", y = "Weekend alcohol consumption"))

    # To view the plots please uncomment this line.
    # Warning! Each plot will be openened in a seperate tab in your browser.
#    print(r)
}