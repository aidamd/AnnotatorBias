ghc <- read.csv("/home/aida/Projects/Stereotype/posts_fleiss.csv")

hate_mod <- glm(hate ~ has_sgt, family=binomial(), data=ghc)

ghc$disagree <- 1 - ghc$agreement
disagree_mod <- glm(disagree ~ has_sgt, family=binomial(), data=ghc)
summary(disagree_mod)

disagree_hate <- glm(agreement ~ has_sgt, family=binomial(), data=ghc[ghc$hate == 1,])
summary(disagree_hate)

disagree_non_hate <- glm(agreement ~ has_sgt, family=binomial(), data=ghc[ghc$hate == 0,])
summary(disagree_non_hate)


posts <- read.csv("/home/aida/Projects/Stereotype/Data/posts_agree_stereo.csv")
stereo_hate <- glm(hate ~ competence + warmth , family=binomial(), data=posts)
stereo_agree<- glm(agreement ~ competence + warmth , family=binomial(), data=posts)
warmth_agree<- glm(agreement ~ warmth , family=binomial(), data=posts)

posts$disagree <- 1 - posts$agreement

gg <- list()
gg[[1]] <- ggplot(posts, aes(x=warmth, y=disagree, group=hate)) + 
  geom_smooth(method=lm, level=0.95) + 
  theme_bw() + 
  labs(title ="", y = "Disagreement", x = "SGT's Warmth") + 
  theme(text = element_text(size=20, family ="times")) +
  coord_cartesian(ylim = c(0, 1))

gg[[2]] <- ggplot(posts, aes(x=competence, y=disagree, group=hate)) + 
  geom_smooth(method=lm, level=0.95) + 
  theme_bw() + 
  labs(title ="", y = "", x = "SGT's Competence") + 
  theme(text = element_text(size=20, family ="times")) +
  coord_cartesian(ylim = c(0, 1))

do.call("grid.arrange", c(gg, nrow = 1))

