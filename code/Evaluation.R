## Purpose: Evaluation
require(patchwork)
require(ggplot2)
require(ggpubr)

#### load model results
ridge<-data.frame(pred=as.integer(ridge.pred > 0.5), true=tst$y) %>% count(true,pred) %>% mutate(freq=n/sum(n))

# plot(ridge.fit)

ridge<-data.framedata.frame(pred=) 

plotTable <- ridge %>%
  mutate(goodbad = ifelse(ridge$pred == ridge$true, "good", "bad")) %>%
  group_by(true) %>%
  mutate(prop = round(n/sum(n),digits =2))

ridge.confusion<-ggplot(data = plotTable, mapping = aes(x = true, y = pred, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = prop), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "steelblue", bad = "red")) +
  labs(title = "Ridge", x = "True dementia", y="Predictive dementia")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggexport(ridge.confusion, filename = "./tables and figures/ridge confusion.png", res = 600, width = 3000, height = 3000)
  # xlim(rev(levels(ridge$true)))


lasso<-data.frame(pred=as.integer(lasso.pred > 0.5), true=tst$y) %>% count(true,pred) %>% mutate(freq=n/sum(n))
# plot(lasso.fit)

plotTable <- lasso %>%
  mutate(goodbad = ifelse(lasso$pred == lasso$true, "good", "bad")) %>%
  group_by(true) %>%
  mutate(prop = round(n/sum(n),digits =2))

lasso.confusion<-ggplot(data = plotTable, mapping = aes(x = true, y = pred, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = prop), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "steelblue", bad = "red")) +
  labs(title = "Lasso", x = "True dementia", y="Predictive dementia")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggexport(lasso.confusion, filename = "./tables and figures/lasso confusion.png", res = 600, width = 3000, height = 3000)


ggpubr::ggarrange(ridge.confusion, lasso.confusion, 
                  labels = c("A: Ridge", "B: Lasso"),
                  legend = "bottom", common.legend=TRUE,
                  hjust = -0.8) %>%
  ggexport(filename = "./tables and figures/confusion matrix.png", res = 600, width = 3000, height = 3000)


