#Function for probability of success for each time

library(readxl)
 Graph_Data <- read_excel("/Users/Cristina/Downloads/Call%20Data.xlsx", sheet = 2, col_names = TRUE)
  Graph_Data[] <- lapply(Graph_Data, as.character)
  Graph_Data$Time <- factor(Graph_Data$Time, levels = c("8", "9", "10", "11", "12", "1", "2", "3", "4"))
Graph_Probs <- function(x) {
			sum((Graph_Data$Time) == x & (Graph_Data$Success) == "Yes")/sum((Graph_Data$Time) == x)
}
times <- c(8:12, 1:4)
Prob_Vec <- sapply(times, Graph_Probs)
Prob_Vec

#Bar Chart w/ percentages

library(dplyr)
library(ggplot2) 
ggplot(Graph_Data %>% count(Time, Success) %>% 
mutate(pct=n/sum(n)), 
aes(Time, n, fill=Success)) 
+ geom_bar(stat="identity") 
+ geom_text(aes(label=paste0(sprintf("%1.1f", pct*100), "%")), position=position_stack(vjust=0.5))
