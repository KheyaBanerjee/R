# Load the data
nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv", sep=",")

#order the data
nba <- nba[order(nba$PTS),]

#make row names out of names column
row.names(nba) <- nba$Name

#subset the data to remove first column (which doesn't contain numbers)
nba <- nba[,2:20]

#create matrix (heatmap function doesn't work with data frame)
nba_matrix <- data.matrix(nba)

nba_heatmap <- heatmap(nba_matrix, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))

nba_heatmap <- heatmap(nba_matrix, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10))