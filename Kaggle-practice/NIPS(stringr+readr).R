# Am I missing a dataset? Fork this and add it below!
# Also, identifying whether a dataset is referenced by a paper could be improved dramatically

library(dplyr)
library(ggplot2)
library(ggrepel)
library(readr)
library(stringr)
library(tidyr)

papers <- read_csv("../input/papers.csv")

papers$paper_text_no_refs <- str_c(str_sub(papers$paper_text, 1, floor(str_length(papers$paper_text)/2)), 
                                   sub("references.*", "references", 
                                       str_sub(papers$paper_text, floor(str_length(papers$paper_text)/2)+1,-1),
                                       ignore.case=TRUE))
#str_sub(東西，開始，結束(-1 = 最後一個))

dataset_names <- c("MNIST", "CIFAR", "SVHN", "PASCAL", "KITTI", "TFD", "SensIT", "Connect4", "Protein", "STL10",
                   "adult", "credit", "kr-vs-kp", "promoters", "votes", "UCI", "digg", "HepTH",
                   "citeseer", "MovieLens", "RocketFuel", "tweet", "twitter", "bAbI", "TreeBank", "Text8",
                   "SARCOS", "NORB", "TIMIT", "ImageNet", "Street View", "VGG", "Caltech-101",
                   "FM-IQA", "AP News", "newsgroups", "diabetes", "HES", "prostate", "MS COCO",
                   "Toronto Face", "glaucoma", "Alzheimer’s", "news20", "scleroderma",
                   "puzzle", "MADELON", "ENRON", "WIPO", "reuters")

data_references <- papers[,"year"]
for (dataset_name in dataset_names) {
  data_references[dataset_name] <- 0
  matches <- grep(str_c(" ", dataset_name, "[^a-z]"), papers$paper_text_no_refs, ignore.case = TRUE)
  data_references[matches, dataset_name] <- 1
}

data_references_by_year <- data_references %>%
  gather("dataset", "isreferenced", 2:ncol(data_references)) %>%
  group_by(year, dataset) %>%
  summarise(references=sum(isreferenced))

popular_datasets <- unique(data_references_by_year[data_references_by_year$references>=10,]$dataset)
popular_data_references_by_year <- data_references_by_year[data_references_by_year$dataset %in% popular_datasets,]

p <- ggplot(popular_data_references_by_year,
            aes(x=year, y=references, group=dataset, color=dataset)) +
  geom_line() +
  geom_text_repel(data=popular_data_references_by_year[popular_data_references_by_year$year==2016,],
                  aes(x=year, y=references, label=dataset)) +
  ggtitle("Popular Dataset References Over Time") +
  xlab("Year") +
  ylab("Number of Papers Referencing the Dataset") +
  theme_light(base_size=14) +
  theme(legend.title=element_blank())
ggsave("popular_data_references_by_year.png", p, height=6, width=10, units="in")