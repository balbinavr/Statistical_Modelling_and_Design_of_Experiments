
rcs = read.csv(file="csvRLq.csv")
rcs = t(rcs)
rcs = data.frame(rcs)

ggplot(rcs, aes(c(0.4, 0.7, 0.8, 0.925), y=rcs$X1)) + 
  geom_errorbar(aes(ymin=rcs$X1-rcs$X2, ymax=rcs$X1+rcs$X2), width=.1, col="red") +
  geom_point() +
  xlab("rho") +
  ylab("Lq") +
  ggtitle("R - Lq chart")



rcs = read.csv(file="csvJLq.csv")
rcs = t(rcs)
rcs = data.frame(rcs)

ggplot(rcs, aes(c(0.4, 0.7, 0.8, 0.925), y=rcs$X1)) + 
  geom_errorbar(aes(ymin=rcs$X1-rcs$X2, ymax=rcs$X1+rcs$X2), width=.1, col="red") +
  geom_point() +
  xlab("rho") +
  ylab("Lq") +
  ggtitle("Java - Lq chart")



rcs = read.csv(file="csvRWq.csv")
rcs = t(rcs)
rcs = data.frame(rcs)

ggplot(rcs, aes(c(0.4, 0.7, 0.8, 0.925), y=rcs$X1)) + 
  geom_errorbar(aes(ymin=rcs$X1-rcs$X2, ymax=rcs$X1+rcs$X2), width=.1, col="red") +
  geom_point() +
  xlab("rho") +
  ylab("Wq") +
  ggtitle("R - Wq chart")

rcs = read.csv(file="csvJWq.csv")
rcs = t(rcs)
rcs = data.frame(rcs)

ggplot(rcs, aes(c(0.4, 0.7, 0.8, 0.925), y=rcs$X1)) + 
  geom_errorbar(aes(ymin=rcs$X1-rcs$X2, ymax=rcs$X1+rcs$X2), width=.1, col="red") +
  geom_point() +
  xlab("rho") +
  ylab("Wq") +
  ggtitle("Java - Wq chart")
