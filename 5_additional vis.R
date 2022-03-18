  library(ggplot2)
  library(dplyr)
  library(forcats)
  library(cowplot)
  library(hrbrthemes)
  
  PATH <- "E://Files/HaHaHariss/22Winter/Policy Lab/Data"
  
  df_stats <- read.csv(file.path(PATH, "modelstats.csv"), encoding="UTF-8")
  colnames(df_stats) <- c('Group','Shift','Mean','Min','Median','Max','Variance')
  df_stats$Group <- gsub("\u200b", "", as.character(df_stats$Group))
  df_stats$Shift <- factor(gsub("\u200b", "", as.character(df_stats$Shift)), levels=c("Morning", "Afternoon", "Night"))
  df_stats$Mean <- as.numeric(gsub("\u200b", "", as.character(df_stats$Mean)))
  df_stats$Min <- as.numeric(gsub("\u200b", "", as.character(df_stats$Min)))
  df_stats$Max <- as.numeric(gsub("\u200b", "", as.character(df_stats$Max)))
  df_stats$Variance <- as.numeric(gsub("\u200b", "", as.character(df_stats$Variance)))

p1 <- ggplot(df_stats[c(1:6),],aes(x = Shift, group = Group, fill = Group))+
  geom_bar(aes(y = Max),stat="identity", col = 'black',width=0.2, position="dodge") +
  geom_line( aes(y = Variance / 2), col = 'darkred',size=1.5) +
  theme_ipsum() +
  scale_y_continuous(name = "Max", limits=c(0,120), 
    sec.axis = sec_axis(~.*2, name="Vairance")) +
  theme(axis.title.y = element_text(color = 'black', size=13),
    axis.title.y.right = element_text(color = 'darkred', size=13)) + 
  ggtitle('Status Quo')
p1

p2 <- ggplot(df_stats[c(4:6),],aes(x = Shift, group = Group, fill = Group))+
  geom_bar(aes(y = Max),stat="identity", col = 'black',width=0.2) +
  geom_line( aes(y = Variance / 2), col = 'darkred',size=1.5) +
  theme_ipsum() +
  scale_y_continuous(name = "Max", limits=c(0,120), 
                     sec.axis = sec_axis(~.*2, name="Vairance")) +
  theme(axis.title.y = element_text(color = 'black', size=13),
        axis.title.y.right = element_text(color = 'darkred', size=13)) + 
  ggtitle('Simple Re-distribution')
p2

p3 <- ggplot(df_stats[c(7:9),],aes(x = Shift, group = Group))+
  geom_bar(aes(y = Max),stat="identity", col = 'black',width=0.2) +
  geom_line( aes(y = Variance / 2), col = 'darkred',size=1.5) +
  theme_ipsum() +
  scale_y_continuous(name = "Max", limits=c(0,120), 
                     sec.axis = sec_axis(~.*2, name="Vairance")) +
  theme(axis.title.y = element_text(color = 'black', size=13),
        axis.title.y.right = element_text(color = 'darkred', size=13)) + 
  ggtitle('Clustered Re-distribution')
p3

plot_grid(p1,p2,ncol = 2)
plot_grid(p2,p3,ncol = 2)
