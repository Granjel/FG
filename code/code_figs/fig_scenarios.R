library(bipartite)
library(igraph)
library(rgl)
library(networkD3)
library(reshape2)
library(webshot)
library(ggpubr)
#library(shape)
library(gridBase)
library(grid)




X <- t(matrix(c(0, -8, 3, 0, -3,
                0, -2, -6, 0, 0,
                0, 4, -5, 0, 0,
                0, 0, 0, 0, -5,
                0, 0, 0, 0, 0), nrow = 5, ncol = 5))
colnames(X) <- c("Cb", "Pl", "To", "Gv", "dummy")
web <- graph_from_adjacency_matrix(X, mode = "directed", weighted = TRUE, diag = TRUE, add.colnames = NULL)
mtf <- layout.circle(web)
V(web)$color <- c("coral1", rep("darkcyan", 3), "white")
E(web)$lty <- 1; E(web)$lty[which(E(web)$weight < 0)] <- 3
mtf[, 1] <- c(0, -1, 1, 0, 0)
mtf[, 2] <- c(0.3, 0, 0, -0.25, 0)




spnames <- 2.85


wt <- 18
png(filename = "figures/interaction_layers.png", width = wt, height = wt * 1.5, units = "in", res = 500)

layout(matrix(c(1, 1, 2, 2,
                1, 1, 2, 2,
                3, 3, 4, 4,
                3, 3, 4, 4,
                5, 5, 0, 0,
                5, 5, 0, 0), nrow = 6, ncol = 4, byrow = TRUE))

plot(web, layout = mtf,
     edge.color = c("gray50", "gray50", "gray75", "black", "gray20", "gray20", "black", "gray75"),
     edge.curved = c(0, 0, 0, 0.25, 0.25, 0.25, 0.25, 0),
     edge.width = abs(E(web)$weight) * 1.05,
     edge.arrow.size = 2.1,
     edge.loop.angle = c(0, 0, 0, 2, 0, 0, 1, 0),
     edge.label = c(expression(alpha[inter]), expression(gamma), expression(gamma), expression(beta[gamma]),
                    expression(beta[alpha]), expression(alpha[intra]), expression(alpha[intra])),
     edge.label.x = c(-0.35, -0.5, 0.475, -0.1, 0.1, 1.2, -1.2),
     edge.label.y = c(-0.1, 0.65, 0.65, 0.425, -0.625, -0.5, -0.5),
     edge.label.font = 2,
     edge.label.cex = 4,
     edge.label.family = "Helvetica",
     edge.label.color = "black",
     vertex.color = c("coral1", rep("darkcyan", 3), rgb(0, 0, 0, 0)),
     vertex.label.family = "Helvetica",
     vertex.label.color = "white",
     vertex.label.font = 2,
     vertex.label.cex = spnames,
     vertex.frame.color = c(rep("white", 4), rgb(0, 0, 0, 0)),
     vertex.size = c(rep(15, 4), 35) * 1.5)
title(main = "Scenario 1", cex.main = 4)
mtext(side = 3, line = -1, at = -1.25, adj = 1, padj = 1, cex = 3.5, expression(bold("a")))
mtext(side = 3, line = -1, at = 0.75, adj = 1, padj = 1, cex = 2, expression(italic("Plant pairwise interactions")))


plot(web, layout = mtf,
     edge.color = c("gray50", "gray50", "gray75", "black", "gray20", "gray20", "black", "gray75"),
     edge.curved = c(0, 0, 0, 0.25, 0.25, 0.25, 0.25, 0),
     edge.width = abs(E(web)$weight) * 1.05,
     edge.arrow.size = 2.1,
     edge.loop.angle = c(0, 0, 0, 2, 0, 0, 1, 0),
     edge.label = c(expression(alpha[inter]), expression(gamma), expression(gamma), expression(beta[gamma]),
                    expression(beta[alpha]), expression(alpha[intra]), expression(alpha[intra])),
     edge.label.x = c(-0.35, -0.5, 0.475, -0.1, 0.1, 1.2, -1.2),
     edge.label.y = c(-0.1, 0.65, 0.65, 0.425, -0.625, -0.5, -0.5),
     edge.label.font = 2,
     edge.label.cex = 4,
     edge.label.family = "Helvetica",
     edge.label.color = "black",
     vertex.color = c("coral1", rep("darkcyan", 3), rgb(0, 0, 0, 0)),
     vertex.label.family = "Helvetica",
     vertex.label.color = "white",
     vertex.label.font = 2,
     vertex.label.cex = spnames,
     vertex.frame.color = c(rep("white", 4), rgb(0, 0, 0, 0)),
     vertex.size = c(rep(15, 4), 35) * 1.5)
title(main = "Scenario 2", cex.main = 4)
mtext(side = 3, line = -1, at = -1.25, adj = 1, padj = 1, cex = 3.5, expression(bold("b")))
mtext(side = 3, line = -1, at = 0.75, adj = 1, padj = 1, cex = 2, expression(italic("Plant pairwise and HOIs")))


plot(web, layout = mtf,
     edge.color = c("gray50", "gray50", "gray75", "black", "gray20", "gray20", "black", "gray75"),
     edge.curved = c(0, 0, 0, 0.25, 0.25, 0.25, 0.25, 0),
     edge.width = abs(E(web)$weight) * 1.05,
     edge.arrow.size = 2.1,
     edge.loop.angle = c(0, 0, 0, 2, 0, 0, 1, 0),
     edge.label = c(expression(alpha[inter]), expression(gamma), expression(gamma), expression(beta[gamma]),
                    expression(beta[alpha]), expression(alpha[intra]), expression(alpha[intra])),
     edge.label.x = c(-0.35, -0.5, 0.475, -0.1, 0.1, 1.2, -1.2),
     edge.label.y = c(-0.1, 0.65, 0.65, 0.425, -0.625, -0.5, -0.5),
     edge.label.font = 2,
     edge.label.cex = 4,
     edge.label.family = "Helvetica",
     edge.label.color = "black",
     vertex.color = c("coral1", rep("darkcyan", 3), rgb(0, 0, 0, 0)),
     vertex.label.family = "Helvetica",
     vertex.label.color = "white",
     vertex.label.font = 2,
     vertex.label.cex = spnames,
     vertex.frame.color = c(rep("white", 4), rgb(0, 0, 0, 0)),
     vertex.size = c(rep(15, 4), 35) * 1.5)
title(main = "Scenario 3", cex.main = 4)
mtext(side = 3, line = -1, at = -1.25, adj = 1, padj = 1, cex = 3.5, expression(bold("c")))
mtext(side = 3, line = -1, at = 0.75, adj = 1, padj = 1, cex = 2, expression(italic("Plant and grasshopper pairwise")))


plot(web, layout = mtf,
     edge.color = c("gray50", "gray50", "gray75", "black", "gray20", "gray20", "black", "gray75"),
     edge.curved = c(0, 0, 0, 0.25, 0.25, 0.25, 0.25, 0),
     edge.width = abs(E(web)$weight) * 1.05,
     edge.arrow.size = 2.1,
     edge.loop.angle = c(0, 0, 0, 2, 0, 0, 1, 0),
     edge.label = c(expression(alpha[inter]), expression(gamma), expression(gamma), expression(beta[gamma]),
                    expression(beta[alpha]), expression(alpha[intra]), expression(alpha[intra])),
     edge.label.x = c(-0.35, -0.5, 0.475, -0.1, 0.1, 1.2, -1.2),
     edge.label.y = c(-0.1, 0.65, 0.65, 0.425, -0.625, -0.5, -0.5),
     edge.label.font = 2,
     edge.label.cex = 4,
     edge.label.family = "Helvetica",
     edge.label.color = "black",
     vertex.color = c("coral1", rep("darkcyan", 3), rgb(0, 0, 0, 0)),
     vertex.label.family = "Helvetica",
     vertex.label.color = "white",
     vertex.label.font = 2,
     vertex.label.cex = spnames,
     vertex.frame.color = c(rep("white", 4), rgb(0, 0, 0, 0)),
     vertex.size = c(rep(15, 4), 35) * 1.5)
title(main = "Scenario 4", cex.main = 4)
mtext(side = 3, line = -1, at = -1.25, adj = 1, padj = 1, cex = 3.5, expression(bold("d")))
mtext(side = 3, line = -1, at = 0.75, adj = 1, padj = 1, cex = 2, expression(italic("Plant HOIs, pairwise grasshoppers")))


plot(web, layout = mtf,
     edge.color = c("gray50", "gray50", "gray75", "black", "gray20", "gray20", "black", "gray75"),
     edge.curved = c(0, 0, 0, 0.25, 0.25, 0.25, 0.25, 0),
     edge.width = abs(E(web)$weight) * 1.05,
     edge.arrow.size = 2.1,
     edge.loop.angle = c(0, 0, 0, 2, 0, 0, 1, 0),
     edge.label = c(expression(alpha[inter]), expression(gamma), expression(gamma), expression(beta[gamma]),
                    expression(beta[alpha]), expression(alpha[intra]), expression(alpha[intra])),
     edge.label.x = c(-0.35, -0.5, 0.475, -0.1, 0.1, 1.2, -1.2),
     edge.label.y = c(-0.1, 0.65, 0.65, 0.425, -0.625, -0.5, -0.5),
     edge.label.font = 2,
     edge.label.cex = 4,
     edge.label.family = "Helvetica",
     edge.label.color = "black",
     vertex.color = c("coral1", rep("darkcyan", 3), rgb(0, 0, 0, 0)),
     vertex.label.family = "Helvetica",
     vertex.label.color = "white",
     vertex.label.font = 2,
     vertex.label.cex = spnames,
     vertex.frame.color = c(rep("white", 4), rgb(0, 0, 0, 0)),
     vertex.size = c(rep(15, 4), 35) * 1.5)
title(main = "Scenario 5", cex.main = 4)
mtext(side = 3, line = -1, at = -1.25, adj = 1, padj = 1, cex = 3.5, expression(bold("e")))
mtext(side = 3, line = -1, at = 0.75, adj = 1, padj = 1, cex = 2, expression(italic("All pairwise and HOIs")))


dev.off()
