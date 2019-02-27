createCEFRgraph <- function(superCategory) {
  
  localTitle <- toString(superCategory)
  plugTitle <- paste(localTitle, "by CEFR level and SubCategory", sep=" ")
  
  functionData <- read_excel("a1-c2.xlsx")
  
  functionData %>%
    filter(SuperCategory == superCategory) %>%
    ggplot(aes(x=SubCategory, fill=Level)) +
    geom_bar(position = position_stack(reverse = TRUE)) +
    coord_flip() +
    theme(legend.position = "top") +
    ggtitle(plugTitle)
}