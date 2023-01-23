
#FIG BARS----

papers <- read.delim(here::here("outputs", "final_papers_list.txt"), header = TRUE)

papers <- papers[!is.na(papers$"isdafnee"), ]
papers <- papers[!is.na(papers$"SCOPUS_source"), ]

no_daf <- tapply(papers$"isdafnee", papers$"collection", 
                 function(x) length(which(x == "no")))
is_daf <- tapply(papers$"isdafnee", papers$"collection", 
                 function(x) length(which(x == "ok")))

no_daf_perc <- round(100 * no_daf / (no_daf + is_daf), 1)
is_daf_perc <- round(100 * is_daf / (no_daf + is_daf), 1)
is_daf_perc["ALL"]=mean(is_daf_perc)
no_daf_perc["ALL"]=mean(no_daf_perc)

centers <- c("NCEAS", "SDIV", "CESAB","ALL")

is_daf_perc <- is_daf_perc[c(4,2, 3, 1)]
no_daf_perc <- no_daf_perc[c(4,2, 3, 1)]

pdf(here::here("figures", "fig-bars.pdf"), width = 8, height = 8)


par(xaxs = "i", yaxs = "i", mar = c(2.5, 2.5, 1, 1))

plot(0, type = "n", axes = FALSE, ann = FALSE, bty = "n", xlim = c(0.55, 4.45), 
     ylim = c(0, 100))

grid()

for (i in 1:length(centers)) {
  
  rect(i - .45, 0, i + .45, is_daf_perc[i], border = "white", col = "#5ec27a")
  rect(i - .45, is_daf_perc[i], i + .45, 100, border = "white", col = "#c2925e")
  text(i, -5, names(no_daf_perc)[i], font = 2, xpd = TRUE, col = "#666666")
}
par(mgp = c(3, 0.20, 0))
axis(2, at = seq(0, 100, by= 10), labels = paste0(seq(0, 100, by= 10), "%"), 
     las = 2, col.axis = "#666666", lwd = 0)

dev.off()

#----

#FIG TRENDS-----

papers$"year" <- as.numeric(papers$"year")
papers <- papers[!is.na(papers$"year"), ]



daf_perc <- data.frame(tapply(papers$"isdafnee", list(papers$"year", papers$"collection"), 
                   function(x) 100 * length(which(x == "ok")) / length(x)))  
daf_perc <- daf_perc[c(-1,-nrow(daf_perc)),]
daf_perc$"year" <- as.numeric(rownames(daf_perc))
daf_perc$all <- apply(daf_perc[,1:3],1,mean,na.rm=T)

daf_perc[which(daf_perc$"year" == 2012), 1] <- NA
daf_perc[which(daf_perc$"year" == 2013), 3] <- NA

k <- 2
years <- seq(min(daf_perc$year), max(daf_perc$year), by = k)

dat <- dat_se <- data.frame()
for (year in years) {
  dat <- rbind(dat, 
               data.frame(t(apply(daf_perc[which(daf_perc$year %in% seq(year, year + (k - 1))), ], 
                                  2, mean, na.rm = TRUE))))
  dat_se <- rbind(dat_se, 
               data.frame(t(apply(daf_perc[which(daf_perc$year %in% seq(year, year + (k - 1))), ], 
                                  2, sd, na.rm = TRUE))))
}

dat$year <- years
dat_se$year <- years

pdf(here::here("figures", "fig-trend.pdf"), width = 12, height = 12)

cols <- c("#685383", "#ab6655", "#F8D548")

par(mgp = c(3, 1, 0), mar = c(1.5, 4.5, 1, 1))
plot(0, type = "n", xlim = range(papers$"year"), ylim = c(0, 100),
     ann = FALSE, axes = FALSE, bty = "n")
grid()

for (i in 1:3) {
  
  tmp <- dat[ , c(i, 4)]
  tmp_se <- dat_se[ , c(i, 4)]
  tmp <- tmp[!is.na(tmp[ , 1]), ]
  tmp_se <- tmp_se[!is.na(tmp_se[ , 1]), ]
  
  polygon(x = c(tmp$"year", rev(tmp$"year"), tmp$"year"[1]),
          y = c(tmp[ , 1] - tmp_se[ , 1], rev(tmp[ , 1] + tmp_se[ , 1]), tmp[1, 1] - tmp_se[1, 1]),
          col = paste0(cols[i], "22"), border = paste0(cols[i],"44"))
}

for (i in 1:3) {
  
  tmp <- dat[ , c(i, 4)]
  tmp_se <- dat_se[ , c(i, 4)]
  tmp <- tmp[!is.na(tmp[ , 1]), ]
  tmp_se <- tmp_se[!is.na(tmp_se[ , 1]), ]
  
  lines(tmp$"year", tmp[ , 1], lwd = 2, col = cols[i])
}

dat[ , c(i, 4)]

lines(dat$"year", dat$all, lwd = 2, col = "black",size=2)


par(mgp = c(3, 0.25, 0))
axis(1, dat$year, lwd = 0, col.axis = "#666666")
axis(2, at = axTicks(2), labels = paste0(axTicks(2), "%"), 
     lwd = 0, col.axis = "#666666", las = 2)
mtext("% of non profit papers", side = 2, line = 3, 
      col = "#666666", font = 2)
text(1997, 2, "CESAB", font = 2, cex = 1.85, col = "#685383", pos = 4)
text(1997, 8, "SDIV", font = 2, cex = 1.85, col = "#F8D548", pos = 4)
text(1997, 14, "NCEAS", font = 2, cex = 1.85, col = "#ab6655", pos = 4)
box()


dev.off()

#----

#BOXPLOT ALL----

papers <- read.delim(here::here("outputs", "final_papers_list.txt"), header = TRUE)
papers <- papers[!is.na(papers$"isdafnee"), ]
papers <- papers[!is.na(papers$"SCOPUS_source"), ]
impacts <- papers[!is.na(papers$"SNIP"), ]
impacts <- papers[!is.na(papers$"scopus_citation"), ]
impacts$isdafnee[impacts$isdafnee%in%"ok"] <- "YES"
impacts$isdafnee[impacts$isdafnee%in%"no"] <- "NO"
impacts$isdafnee <- as.factor(impacts$isdafnee)
impacts$isdafnee <- factor(impacts$isdafnee , levels=c("YES","NO"))
impacts$isdafnee[impacts$isdafnee%in%"ok"] <- "YES"
impacts$isdafnee[impacts$isdafnee%in%"no"] <- "NO"
impacts$citationyear <- (impacts$scopus_citation/(2023-as.numeric(as.character(impacts$year)))+1)

ipp_test <- wilcox.test(IPP ~ isdafnee, data = impacts)
round(ipp_test$p.value,3)
snip_test <- wilcox.test(SNIP ~ isdafnee, data = impacts)
round(snip_test$p.value,3)
cit_test <- wilcox.test(scopus_citation ~ isdafnee, data = impacts)
round(cit_test$p.value,3)


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

d <- ggplot(impacts, aes(x =SNIP ,  y = citationyear, colour = isdafnee))+geom_point()+
      scale_colour_manual(values = c("#a8bd91","#f0a067"))+
      stat_smooth(method = "lm", formula = y ~ x, se = TRUE)+
      coord_trans(y="log10")+
      theme_bw() +
      theme(panel.grid.minor = element_blank())+
      ylab('Citations/years')+
      theme(legend.position = c(0.9, 0.9))
      

pdf(here::here("figures", "fig-boxplot.pdf"), width = 16, height = 10)
gridExtra::grid.arrange(a,b,c,d,ncol=2)
dev.off()

#----

#BOXPLOT BETWEEN CENTERS----



#BOXPLOT ALL----

papers <- read.delim(here::here("outputs", "final_papers_list.txt"), header = TRUE)
papers <- papers[!is.na(papers$"isdafnee"), ]
papers <- papers[!is.na(papers$"SCOPUS_source"), ]
impacts <- papers[!is.na(papers$"SNIP"), ]
impacts <- papers[!is.na(papers$"scopus_citation"), ]
impacts$isdafnee[impacts$isdafnee%in%"ok"] <- "YES"
impacts$isdafnee[impacts$isdafnee%in%"no"] <- "NO"
impacts$isdafnee <- as.factor(impacts$isdafnee)
impacts$isdafnee <- factor(impacts$isdafnee , levels=c("YES","NO"))
impacts$isdafnee[impacts$isdafnee%in%"ok"] <- "YES"
impacts$isdafnee[impacts$isdafnee%in%"no"] <- "NO"
impacts$citationyear <- (impacts$scopus_citation/(2023-as.numeric(as.character(impacts$year)))+1)



ipp_test <- lm(IPP ~ collection, data = impacts)
summary(ipp_test)
snip_test <- lm(SNIP ~ collection, data = impacts)
sniptest <- summary(snip_test)

ggplot(impacts, aes(x = collection, y = SNIP,colors=collection))+
  geom_boxplot(
    fill=c("#685383", "#ab6655", "#F8D548"),
    width = .5, 
    outlier.shape = NA
  ) +  ylab("Impact factors") +
  xlab("Synthesis centers")+
  theme_bw() + ylim(0,15)+
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
    ))

ggplot(impacts, aes(x = collection, y = citationyear,colors=collection))+
  geom_boxplot(
    fill=c("#685383", "#ab6655", "#F8D548"),
    width = .5, 
    outlier.shape = NA
  ) +  ylab("Citations") +
  xlab("Synthesis centers")+
  theme_bw() + ylim(0,100)+
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
    ))
   




