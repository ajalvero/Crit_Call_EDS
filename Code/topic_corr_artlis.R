library(tidyverse)
topic_2 <- read.csv("topic_2_frl_neg_corr.csv")
topic_7 <- read.csv("topic_7_frl_pos_corr.csv")

gamma_2 <- topic_2$topic_2
gamma_7 <- topic_7$topic_7
frl_2 <- topic_2$FRL
frl_7 <- topic_7$FRL

plot(frl_2,gamma_2, main ='Topic 2',
     xlab ='Reported Free and Reduced Lunch', ylab ='Gamma')

abline(lm(frl_2~gamma_2), col= 'blue')
top_2_fit <- lm(gamma_2~frl_2)
summary(top_2_fit)

top_7_fit <- lm(frl_7~gamma_7)
summary(top_7_fit)

plot(top_7_fit)

plot(frl_2,gamma_2)
plot(frl_7, gamma_7)
abline(lm(gamma_7~frl_7), col='blue')
t.test(frl_7,gamma_7)
t.test(frl_2,gamma_2)

id_7 <- topic_7$ï..document
id_2 <- topic_2$ï..document

ggplot(topic_7, aes(x= frl_7, y= gamma_7)) +geom_text(aes(label=id_7))
ggplot(topic_2, aes(x= frl_2, y= gamma_2)) +geom_text(aes(label=id_2))

cook <-cooks.distance(top_7_fit)
plot(top_7_fit, which =c(4))
id_7
