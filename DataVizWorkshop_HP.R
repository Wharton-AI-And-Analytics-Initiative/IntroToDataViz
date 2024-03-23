#ignore
update.packages()
yes

getwd()
setwd("/Users/jsamper/Downloads/Harry_Potter_Movies")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
library(dplyr)
library(tidyverse)
library(ggplot2)
hpcharacters <- read.csv("Characters.csv")

#creating a generic bar chart
bar1 <- ggplot(data=hpcharacters, aes(x=House)) +
  geom_bar(stat = "count")
bar1
#this chart contains missing values and schools we are not interested in

#our first task is to figure out how many characters in the dataset were in each house during their time at Hogwarts
count(hpcharacters, House)

bar2 <- hpcharacters %>%
  drop_na(House) %>%
  filter(House %in% c("Gryffindor", "Hufflepuff", "Ravenclaw", "Slytherin")) %>%
  ggplot(aes(x=House)) +
  geom_bar(stat = "count")
bar2
#better! We now only see the information we are interested in. But, it's not very visually appealing.

#let's start by assigning colors to each level of the categorical 'house' variable
bar3 <- hpcharacters %>%
  drop_na(House) %>%
  filter(House %in% c("Gryffindor", "Hufflepuff", "Ravenclaw", "Slytherin")) %>%
  ggplot(aes(x=House, fill=House)) +
  geom_bar(stat = "count") +
  scale_fill_manual (values=c("red",
                              "yellow",
                              "blue",
                              "green"))
bar3
#we have color! Awesome! If you're a perfectionist like me, these colors won't do because they don't reflect the proper shade of each color. You can select very specific colors using hexadecimal values

#more representative colors for houses
bar4 <- hpcharacters %>%
  drop_na(House) %>%
  filter(House %in% c("Gryffindor", "Hufflepuff", "Ravenclaw", "Slytherin")) %>%
  ggplot(aes(x=House, fill=House)) +
  geom_bar(stat = "count",
           show.legend = F) +
  scale_fill_manual (values=c("#740001",
                              "#ecb939",
                              "#0e1a40",
                              "#1a472a"))
bar4
#look how pretty. Now we need to adjust labels and background

#removing background and altering labels
bar5 <- hpcharacters %>%
  drop_na(House) %>%
  filter(House %in% c("Gryffindor", "Hufflepuff", "Ravenclaw", "Slytherin")) %>%
  ggplot(aes(x=House, fill=House)) +
  geom_bar(stat = "count",
           show.legend = F) +
  labs(title = "Count of Characters Sorted into Each Hogwarts House",
       x = " ",
       y = " ") +
  scale_fill_manual (values=c("#740001",
                              "#ecb939",
                              "#222f5b",
                              "#2a623d")) +
  theme(panel.background = element_blank(),
        plot.title=element_text(hjust=0.5))
bar5

#editing the text on the y and x axes to be black and be larger
bar6 <- hpcharacters %>%
  drop_na(House) %>%
  filter(House %in% c("Gryffindor", "Hufflepuff", "Ravenclaw", "Slytherin")) %>%
  ggplot(aes(x=House, fill=House)) +
  geom_bar(stat = "count",
           show.legend = F) +
  labs(title = "Count of Characters Sorted into Each Hogwarts House",
       x = " ",
       y = " ") +
  scale_fill_manual (values=c("#740001",
                              "#ecb939",
                              "#222f5b",
                              "#2a623d")) +
  theme(panel.background = element_blank(),
        plot.title=element_text(hjust=0.5),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12))
bar6

#reordering the houses according to count value
hpcharacters2 <- hpcharacters
class(hpcharacters2$House)
hpcharacters2$House <- as.factor(hpcharacters2$House)
hpcharacters2$House <- factor(hpcharacters2$House, levels = c("Gryffindor", "Slytherin", "Ravenclaw", "Hufflepuff"))
str(hpcharacters2$House)

bar7 <- hpcharacters2 %>%
  drop_na(House) %>%
  filter(House %in% c("Gryffindor", "Slytherin", "Ravenclaw", "Hufflepuff")) %>%
  ggplot(aes(x=House, fill=House)) +
  geom_bar(stat = "count",
           show.legend = F) +
  labs(title = "Count of Characters Sorted into Each Hogwarts House",
       x = " ",
       y = " ") +
  scale_fill_manual (values=c("#740001",
                              "#2a623d",
                              "#222f5b",
                              "#ecb939")) +
  theme(panel.background = element_blank(),
        plot.title=element_text(hjust=0.5),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12))
bar7

#woohoo! We made a visually appealing and concise figure fit for a presentation!

#######################################################
#now onto clustering
#we will be using data from a Pokemon dataset
#in the 
#Diaglogue dataset has place ID, character ID, and chapter ID. Need to create new dataframe joining dialogue dataset with other datasets


#installing additional packages for clustering
#for executing k-means clustering
install.packages("stats")
#for visualizations
install.packages("ggfortify")
install.packages("highcharter")
install.packages("igraph")
#for correlation matrix
install.packages("corrplot")
install.packages("superml")
install.packages("highcharter")
install.packages("plotly")
install.packages("viridisLite")
install.packages("cowplot")
install.packages("treemap")
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("maps")
install.packages("countrycode")
install.packages("purrr")
install.packages("htmltools")
install.packages("factoextra")
install.packages("NbClust")
yes
no

library(stats)
library(ggfortify)
library(tidyverse)
library(corrplot)
library(highcharter)
library(superml)
library(igraph)
library(highcharter)
library(plotly)
library(viridisLite)
library(cowplot)
library(treemap)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(maps)
library(countrycode)
library(purrr)
library(htmltools)
library(factoextra)


pokemon <- read.csv("pokemon.csv")
view(pokemon)

#correlation matrix
corrdf <- pokemon %>%
  select(Total, HP, Attack, Defense, Special.Attack, Special.Defense, Speed)

corrdf <- data.frame(corrdf)
corrp <- cor(method = "pearson", corrdf[sapply(corrdf, function(x) !is.factor(x))])
hchart(corrp) %>%
  hc_title(
    text = "Pearson Correlation Matrix of Pokemon Skills"
  )

label <- LabelEncoder$new()
pokemon$Type <- label$fit_transform(pokemon$Type)

#new dataframe for PCA
selected_df <- pokemon %>%
  drop_na() %>%
  select(Speed, Defense)

#this works!!!
hchart(princomp(selected_df, cor = TRUE)) |>
  hc_title(text = "Principal Components Based on the Correlation Between Speed and Defense Scores")

pca <- prcomp(selected_df, scale = TRUE)
pca

comp <- data.frame(pca$x)

#k-means
plot_ <- function(i){
  cluster <- kmeans(comp, centers = i, nstart = 25)
  plot_list <- fviz_cluster(cluster, geom = "point", data = comp) + ggtitle(paste("k = ", i)) + theme_minimal()
  return(plot_list)
}
  
library(gridExtra)
grid.arrange(plot_(2),plot_(3),plot_(4),plot_(5),plot_(6),plot_(7), nrow = 2, top ="K Clusters by each Principal Component") + theme_minimal()

#silhouette method
fviz_nbclust(comp, kmeans, method = "silhouette") + 
  theme_minimal()

library(NbClust)

#d index
nb <- NbClust(comp, diss=NULL, distance = "euclidean", 
              min.nc=2, max.nc=5, method = "kmeans", 
              index = "all", alphaBeale = 0.1)