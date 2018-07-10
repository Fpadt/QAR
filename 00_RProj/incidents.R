library(ggplot2)

setkey(dt, START)
dt[, CNT:= 1:.N]
dt[, S:=as.integer(START)]
dt[, E:=as.integer(END)]

setorder(dt, END)
setkey(dt, END)

ggplot(dt, aes(x=reorder(CNT,X = START), ymin=START, ymax=END, 
       lower = E, upper = S, middle = (S+E)/2 )) +
  geom_boxplot(stat='identity', colour = "#3366FF", fill = "#3366FF") +
  coord_flip()


