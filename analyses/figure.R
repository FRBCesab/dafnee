papers <- read.delim(here::here("outputs", "final_papers_list.txt"), header = TRUE)

papers <- papers[!is.na(papers$"isdafnee"), ]
papers <- papers[!is.na(papers$"SCOPUS_source"), ]

no_daf <- tapply(papers$"isdafnee", papers$"collection", 
                 function(x) length(which(x == "no")))
is_daf <- tapply(papers$"isdafnee", papers$"collection", 
                 function(x) length(which(x == "ok")))

no_daf_perc <- round(100 * no_daf / (no_daf + is_daf), 1)
is_daf_perc <- round(100 * is_daf / (no_daf + is_daf), 1)

centers <- c("NCEAS", "SDIV", "CESAB")

is_daf_perc <- is_daf_perc[c(2, 3, 1)]
no_daf_perc <- no_daf_perc[c(2, 3, 1)]

pdf(here::here("figures", "fig-bars.pdf"), width = 4, height = 8)


par(xaxs = "i", yaxs = "i", mar = c(2.5, 2.5, 1, 1))

plot(0, type = "n", axes = FALSE, ann = FALSE, bty = "n", xlim = c(0.55, 3.45), 
     ylim = c(0, 100))

grid()

for (i in 1:length(centers)) {
  
  rect(i - .45, 0, i + .45, no_daf_perc[i], border = "white", col = "#A8BD91")
  rect(i - .45, no_daf_perc[i], i + .45, 100, border = "white", col = "#f0a067")
  text(i, -5, names(no_daf_perc)[i], font = 2, xpd = TRUE, col = "#666666")
}
par(mgp = c(3, 0.20, 0))
axis(2, at = seq(0, 100, by= 10), labels = paste0(seq(0, 100, by= 10), "%"), 
     las = 2, col.axis = "#666666", lwd = 0)

dev.off()


papers$"year" <- as.numeric(papers$"year")
papers <- papers[!is.na(papers$"year"), ]



daf_perc <- tapply(papers$"isdafnee", list(papers$"year", papers$"collection"), 
                   function(x) 100 * length(which(x == "ok")) / length(x))   

daf_perc <- as.data.frame(daf_perc)
daf_perc$"year" <- as.numeric(rownames(daf_perc))
daf_perc <- daf_perc[-nrow(daf_perc), ]
daf_perc <- daf_perc[-1, ]
daf_perc[which(daf_perc$"year" == 2012), 1] <- NA
daf_perc[which(daf_perc$"year" == 2013), 3] <- NA

k <- 3
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

pdf(here::here("figures", "fig-trend.pdf"), width = 14, height = 8)

cols <- c("#685383", "#ab6655", "#F8D548")

par(mgp = c(3, 1, 0), mar = c(1.5, 4.5, 1, 1))
plot(0, type = "n", xlim = range(papers$"year"), ylim = c(30, 90),
     ann = FALSE, axes = FALSE, bty = "n")
grid()

for (i in 1:3) {
  
  tmp <- dat[ , c(i, 4)]
  tmp_se <- dat_se[ , c(i, 4)]
  tmp <- tmp[!is.na(tmp[ , 1]), ]
  tmp_se <- tmp_se[!is.na(tmp_se[ , 1]), ]
  
  polygon(x = c(tmp$"year", rev(tmp$"year"), tmp$"year"[1]),
          y = c(tmp[ , 1] - tmp_se[ , 1], rev(tmp[ , 1] + tmp_se[ , 1]), tmp[1, 1] - tmp_se[1, 1]),
          col = paste0(cols[i], "66"), border = cols[i])
}

for (i in 1:3) {
  
  tmp <- dat[ , c(i, 4)]
  tmp_se <- dat_se[ , c(i, 4)]
  tmp <- tmp[!is.na(tmp[ , 1]), ]
  tmp_se <- tmp_se[!is.na(tmp_se[ , 1]), ]
  
  lines(tmp$"year", tmp[ , 1], lwd = 2, col = cols[i])
}

par(mgp = c(3, 0.25, 0))
axis(1, dat$year, lwd = 0, col.axis = "#666666")
axis(2, at = axTicks(2), labels = paste0(axTicks(2), "%"), 
     lwd = 0, col.axis = "#666666", las = 2)
mtext("% of Dafnee papers", side = 2, line = 3, 
      col = "#666666", font = 2)
text(1997, 32, "CESAB", font = 2, cex = .85, col = "#685383", pos = 4)
text(1997, 34, "SDIV", font = 2, cex = .85, col = "#F8D548", pos = 4)
text(1997, 36, "NCEAS", font = 2, cex = .85, col = "#ab6655", pos = 4)
box()


dev.off()


