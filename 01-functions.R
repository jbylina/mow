require(ggplot2)
require(caret)

plot_conf_matrix <- function(fit){
  tab <- confusionMatrix(fit)
  tab <- tab$table/colSums(tab$table)
  
  confusion <- as.data.frame(tab)
  
  plot <- ggplot(confusion) +
    geom_tile(aes(x=Reference, y=Prediction, fill=Freq)) +
    geom_text(aes(x=Reference, y=Prediction, label = round(Freq, 2))) +
    scale_x_discrete(name="Actual Class") +
    scale_y_discrete(name="Predicted Class")+
    scale_fill_gradient(breaks=seq(from=0, to=1, by=.2)) +
    labs(fill="Normalized\nFrequency") +
    theme(legend.text=element_text(size=15), legend.position="bottom", legend.key.width=unit(1.5,"cm"))
  print(plot)
}