#Dictionalyの呼び出し
dic <- read.csv("R/dictionary.csv", header = FALSE)
colnames(dic) <- c("word", "count", "g1", "g2", "g3", "g4", "g5", "g6",
                   "g7", "g8", "g9", "g10", "g11", "g12", "g13")
dic$word <- as.character(dic$word)
