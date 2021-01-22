data <- read.csv("/home/aida/Projects/Stereotype/Data/study1_ddr.csv")
#data$Frequency <- data$Frequency * 819
data$communion <- scale(data$communion, center=T)
data$agency <- scale(data$agency, center=T)
psych::describe(data)

mod <- glm(Frequency ~ agency + communion, data=data, family = poisson)
summary(mod)

dots <- read.csv("/home/aida/Projects/Stereotype/Data/study1_norm.csv")
lines <- read.csv("/home/aida/Projects/Stereotype/Data/study1_changes.csv")


dots$predicted <- predict(mod, type="response")
d <- dots[with(dots, order(agency)),]

g <- ggplot(data=d,aes(x=agency, y=Frequency)) + 
  geom_point(data=d,aes(y=Frequency), position=position_jitter(h=.2)) + 
  geom_label_repel(aes(label=sgt, x=agency, y=Frequency),data=d) + 
  theme_bw() + geom_smooth(method = "lm", alpha = 0.2, colour="purple") + 
  labs(x= "Competence", y = "False Positive Labels") + 
  theme(axis.title.x = element_text(size=15), 
        axis.title.y = element_text(size=15))

d <- dots[with(dots, order(communion)),]

g <- ggplot(data=d,aes(x=communion, y=Frequency)) + 
  geom_point(data=d,aes(y=Frequency), position=position_jitter(h=.2)) + 
  geom_label_repel(aes(label=sgt, x=communion, y=Frequency),data=d) + 
  geom_smooth(method = "lm", alpha = 0.2, colour="purple") +
  theme_bw()+ 
  labs(x= "Warmth", y = "False Positive Labels") + 
  theme(axis.title.x = element_text(size=15), 
        axis.title.y = element_text(size=15))