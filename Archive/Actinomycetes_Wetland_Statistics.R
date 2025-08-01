######ARCHIVE PROGRAM NOT ASSOCIATED WITH MANUSCRIPT##################



# Author: Andrew Pilat
# History:
# Created - 4/7/25
# Discontinued - 7/31/25
# Affiliation: Kenyon College, Department of Biology




library(dunn.test)

stats_actino <- read.csv("/Users/andrewpilat/Documents/Honors/Kraken Spreadsheets/Stats_actino.csv")
stats_actino <- stats_actino[1:35,]
stats_actino$Actinomycetes <- as.numeric(stats_actino$Actinomycetes)

kruskal.test(stats_actino$Actinomycetes, stats_actino$Sample) ##p-value = 0.002187

dunn.test(stats_actino$Actinomycetes, stats_actino$Sample, method = "bh")

help("kruskal.test")
help(dunn.test)
