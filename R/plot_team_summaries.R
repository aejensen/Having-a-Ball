rm(list=ls())
load("../results/teams_grouped_by_ETI.RData")

teamData$groups <- teamData$team
levels(teamData$groups) <- as.vector(floor(part_4$optim$bestmem))

teams_tab <- as.matrix(table(teamData$team, teamData$groups))
teamAverages <- sapply(rownames(teams_tab), function(name) {
	mean(teamData[teamData$team == name, "ETI"])
})
teamSD <- sapply(rownames(teams_tab), function(name) {
	sd(teamData[teamData$team == name, "ETI"])
})
teamLower <- sapply(rownames(teams_tab), function(name) {
	quantile(teamData[teamData$team == name, "ETI"], 0.025)
})
teamMedian <- sapply(rownames(teams_tab), function(name) {
	quantile(teamData[teamData$team == name, "ETI"], 0.5)
})
teamUpper <- sapply(rownames(teams_tab), function(name) {
	quantile(teamData[teamData$team == name, "ETI"], 0.975)
})
class = apply(teams_tab, 1, function(q) as.numeric(colnames(teams_tab))[which.max(q)])

tab <- data.frame(Average = teamAverages, 
                  SD = teamSD, 
                  "lower" = teamLower, 
                  "median" = teamMedian,
                  "upper" = teamUpper,
                  class = class)
colnames(tab) <- c("Average", "SD", "2.5%", "50%", "97.5%", "class_num")
tab <- tab[order(tab$Average, decreasing=TRUE), ]
tab$Group <- tab$class_num
tab$Group[tab$class_num == "2"] <- "A"
tab$Group[tab$class_num == "0"] <- "B"
tab$Group[tab$class_num == "3"] <- "C"
tab$Group[tab$class_num == "1"] <- "D"

library("ggplot2")
library("ggrepel")
library("lemon")


pdf("../figures/fig5.pdf", width = 8, height = 6)
ggplot(data=tab,
       aes(x=Average, y=SD, label=rownames(tab))) + 
    stat_smooth(method="lm", size=1, se=FALSE, col="black") +
    geom_point(aes(col=factor(class_num), shape=factor(class_num)), size=2.5) + 
    geom_text_repel(size=3) + 
    xlab(expression("Seasonal average" ~ widehat(ETI)[m])) +
    ylab(expression("Seasonal SD of" ~ widehat(ETI)[m])) + 
    scale_x_continuous(breaks =seq(9.25, 11.75, length.out=6)) + 
    scale_y_continuous(breaks=seq(3, 5.4, length.out=7)) +    
    scale_shape_manual(values=c(15, 16, 17, 18)) +
    scale_color_manual(values=c("#CC0000", "darkgreen", "#0000CC", "#000000")) + 
    theme_classic() + 
    theme(legend.position="none",
          axis.ticks.length = unit(.25, "cm"), 
          axis.line = element_line(),
          axis.text.x = element_text(colour="black"),
          axis.text.y = element_text(colour="black")) + 
    lemon::coord_flex_cart(bottom=capped_horizontal(), 
                     left=capped_vertical(capped="both"))
dev.off()
