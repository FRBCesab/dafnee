papers <- read.delim(here::here("outputs", "final_papers_list.txt"), header = TRUE)

papers <- papers[!is.na(papers$"isdafnee"), ]
papers <- papers[!is.na(papers$"SCOPUS_source"), ]
impacts <- papers[!is.na(papers$"IPP"), ]
impacts$isdafnee[impacts$isdafnee%in%"ok"] <- "YES"
impacts$isdafnee[impacts$isdafnee%in%"no"] <- "NO"


impacts$isdafnee <- as.factor(impacts$isdafnee)
impacts$isdafnee <- factor(impacts$isdafnee , levels=c("YES","NO"))

ipp_test <- wilcox.test(IPP ~ isdafnee, data = impacts)
snip_test <- wilcox.test(SNIP ~ isdafnee, data = impacts)

library(ggplot2)

a <- ggplot(impacts, aes(x = isdafnee, y = IPP,colors=isdafnee))+
  geom_boxplot(
    fill=c("#a8bd91","#f0a067"),
    width = .5, 
    outlier.shape = NA
  ) +  ylab("Impact factors") +
  xlab("Is Dafnee")+
  theme_bw() + ylim(0,60)+
  ggdist::stat_halfeye(
    adjust = .5,
    width = .6,
    .width = 0,
    justification = -.3,
    alpha = .1,
    fill='blue',
    point_colour = NA)+
  geom_point(
    size = 1,
    alpha = .2,
    col="gray",
    position = position_jitter(
      seed = 1, width = .1
    ))+ ggtitle(paste0("IPP, wilcox.test p=",round(ipp_test$p.value,3)," n=",nrow(impacts)))

b <-  ggplot(impacts, aes(x = isdafnee, y = SNIP,colors=isdafnee))+
  geom_boxplot(
    fill=c("#a8bd91","#f0a067"),
    width = .5, 
    outlier.shape = NA
  ) +  ylab("Impact factors") +
  xlab("Is Dafnee")+
  theme_bw() + ylim(0,12)+
  ggdist::stat_halfeye(
    adjust = .5,
    width = .6,
    .width = 0,
    justification = -.3,
    alpha = .1,
    fill='blue',
    point_colour = NA)+
  geom_point(
    size = 1,
    alpha = .2,
    col="gray",
    position = position_jitter(
      seed = 1, width = .1
    ))+ ggtitle(paste0("SNIP, wilcox.test p=",round(snip_test$p.value,3)," n=",nrow(impacts)))



papers <- read.delim(here::here("outputs", "final_papers_list.txt"), header = TRUE)
papers <- papers[!is.na(papers$"isdafnee"), ]
papers <- papers[!is.na(papers$"SCOPUS_source"), ]
impacts <- papers[!is.na(papers$"scopus_citation"), ]
impacts$isdafnee[impacts$isdafnee%in%"ok"] <- "YES"
impacts$isdafnee[impacts$isdafnee%in%"no"] <- "NO"

impacts$isdafnee <- as.factor(impacts$isdafnee)
impacts$isdafnee <- factor(impacts$isdafnee , levels=c("YES","NO"))

cit_test <- wilcox.test(scopus_citation ~ isdafnee, data = impacts)

library(ggplot2)

c <- ggplot(impacts, aes(x = isdafnee, y = scopus_citation,colors=isdafnee))+
  geom_boxplot(
    fill=c("#a8bd91","#f0a067"),
    width = .5, 
    outlier.shape = NA
  ) +  ylab("Citations") +
  xlab("Is Dafnee")+
  theme_bw() + ylim(0,1000)+
  ggdist::stat_halfeye(
    adjust = .5,
    width = .6,
    .width = 0,
    justification = -.3,
    alpha = .1,
    fill='blue',
    point_colour = NA)+
  geom_point(
    size = 1,
    alpha = .2,
    col="gray",
    position = position_jitter(
      seed = 1, width = .1
    ))+ ggtitle(paste0("Citations, wilcox.test p=",round(cit_test$p.value,9)," n=",nrow(impacts)))


pdf(here::here("figures", "fig-boxplot.pdf"), width = 10, height = 10)
gridExtra::grid.arrange(a,b,c,ncol=2)
dev.off()

