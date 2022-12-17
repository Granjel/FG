load("results/matrices.RData")
A <- cbind(alpha, matrix(0, nrow(alpha), 6))
colnames(A) <- c(colnames(alpha), colnames(gamma))
B <- cbind(t(gamma), matrix(0, 6, 6))
colnames(B) <- c(colnames(t(gamma)), colnames(gamma))
AB <- as.matrix(rbind(A, B))

#colnames(AB) <- c(1:35, c("Cb", "Cd", "Ci", "Ee", "Pg", "Pp"))

colnames(AB) <- c("Ae", "Be", "Ca", "Dg", "Dc", "Gv", "Pl", "Tp", "Lv", "To",
                  "Ra", "Po", "Pa", "Sp", "Cj", "Fa", "Rx", "Em", "Am", "Ma",
                  "Fr", "Lc", "Cr", "Or", "Pt", "Pe", "Gd", "Tf", "Vp", "Ao",
                  "Cb", "Cd", "Ci", "Ee", "Pg", "Pp")

library(bipartite)
library(igraph)
library(rgl)
library(networkD3)
library(reshape2)
library(webshot)
library(ggpubr)
#library(shape)

wadj <- graph_from_adjacency_matrix(AB, mode = "directed", weighted = TRUE, diag = FALSE, add.colnames = NULL)
colrs <- c(rep("darkcyan", nrow(alpha)), rep("coral1", 6))
V(wadj)$color <- colrs
E(wadj)$lty <- 1; E(wadj)$lty[which(E(wadj)$weight < 0)] <- 3 #negative = dotted

#trick to display plants and grasshoppers differently
#colnames(alpha) <- 1:35
wadj_alpha <- graph_from_adjacency_matrix(as.matrix(alpha), mode = "directed", weighted = TRUE, diag = FALSE, add.colnames = NULL)
colrs <- rep("darkcyan", nrow(alpha))
V(wadj_alpha)$color <- colrs

lyo_alpha <- layout.sphere(wadj_alpha)
#lyo_alpha[13, 1] <- lyo_alpha[13, 1] * 0
#lyo_alpha[14, 1] <- lyo_alpha[14, 1] * 1.2
#lyo_alpha[31, 1] <- lyo_alpha[31, 1] * 0.75
#lyo_alpha[31, 2] <- lyo_alpha[31, 2] * 1.05
#lyo_alpha[4, 2] <- lyo_alpha[4, 2] * 0.85
#lyo_alpha[5, 1] <- lyo_alpha[5, 1] * 0.91
#lyo_alpha[32, 1] <- lyo_alpha[32, 1] * 1.2
#lyo_alpha[25, 2] <- lyo_alpha[25, 2] * 1.45
#lyo_alpha[22, 1] <- lyo_alpha[22, 1] * 1.05
#lyo_alpha[22, 2] <- lyo_alpha[22, 2] * 0.9
#lyo_alpha[12, 2] <- lyo_alpha[12, 2] * 1.05
#lyo_alpha[35, 1] <- lyo_alpha[35, 1] -0.05
#lyo_alpha[35, 2] <- lyo_alpha[35, 2] -0.2
#lyo_alpha[1, 1] <- lyo_alpha[1, 1] + 0.125
#lyo_alpha[11, 2] <- lyo_alpha[11, 2] - 0.125
#lyo_alpha[21, 1] <- lyo_alpha[21, 1] * 0.95
#lyo_alpha[21, 2] <- lyo_alpha[21, 2] + 0.1
#lyo_alpha[34, 1] <- lyo_alpha[34, 1] - 0.1
#lyo_alpha[34, 2] <- lyo_alpha[34, 2] - 0.01
#lyo_alpha[3, 1] <- lyo_alpha[3, 1] + 0.075
#lyo_alpha[3, 2] <- lyo_alpha[3, 2] - 0.075
#lyo_alpha[2, 1] <- lyo_alpha[2, 1] - 0.075
#lyo_alpha[2, 2] <- lyo_alpha[2, 2] + 0.05
#lyo_alpha[20, 2] <- lyo_alpha[20, 2] + 0.1
#lyo_alpha[28, 2] <- lyo_alpha[28, 2] + 0.1
#lyo_alpha[16, 1] <- lyo_alpha[16, 1] + 0.0225
#lyo_alpha[16, 2] <- lyo_alpha[16, 2] - 0.09
#lyo_alpha[9, 2] <- lyo_alpha[9, 2] - 0.05

lyo <- layout.sphere(wadj)
lyo[1:30,] <- lyo_alpha
lyo[1:30, 2] <- lyo[1:30, 2]
lyo[31,] <- c(-1.0, 1.2, 1)
lyo[32,] <- c(-0.65, 1.6, 1)
lyo[33,] <- c(-0.25, 1.8, 1)
lyo[34,] <- c(0.25, 1.8, 1)
lyo[35,] <- c(0.65, 1.6, 1)
lyo[36,] <- c(1.0, 1.2, 1)
E(wadj)$lty <- 1; E(wadj)$lty[which(E(wadj)$weight < 0)] <- 2 #negative = dotted

cedge <- rep(0.2, length(E(wadj)$weight)) #custom curved edges
#cedge[798] <- -0.1
#cedge[804:805] <- -0.1

#png(filename = "figures/network.png", width = 10, height = 10, units = "in", res = 1200)
#par(mar = c(rep(0, 4)))
#plot(wadj, layout = lyo,
#     edge.curved = cedge,
#     edge.width = (abs(E(wadj)$weight) + 0.15) * 3,
#     edge.arrow.size = 0.5,
#     vertex.label.family = "Helvetica",
#     vertex.label.color = "white",
#     vertex.label.font = 2,
#     vertex.label.cex = 1.15,
#     vertex.frame.color = "white")
#dev.off()


#### motifs ################################################################################################
#source("code/tools/igraphplot2.R")
#environment(plot.igraph2) <- asNamespace('igraph')
#environment(igraph.Arrows2) <- asNamespace('igraph')

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

#png(filename = "figures/motifs.png", width = 10, height = 10, units = "in", res = 1200)
#par(mar = c(0, 2, 0, 2))
#plot(web, layout = mtf,
#     edge.color = c("gray50", "gray50", "gray75", "black", "gray20", "gray20", "black", "gray75"),
#     edge.curved = c(0, 0, 0, 0.25, 0.25, 0.25, 0.25, 0),
#     edge.width = abs(E(web)$weight),
#     edge.arrow.size = 2,
#     edge.loop.angle = c(0, 0, 0, 2, 0, 0, 1, 0),
#     edge.label = c(expression(alpha[inter]), expression(gamma), expression(gamma), expression(beta),
#                    expression(beta), expression(alpha[intra]), expression(alpha[intra])),
#     edge.label.x = c(-0.35, -0.5, 0.475, -0.1, 0.1, 1.2, -1.2),
#     edge.label.y = c(-0.1, 0.65, 0.65, 0.425, -0.625, -0.5, -0.5),
#     edge.label.font = 2,
#     edge.label.cex = 4,
#     edge.label.family = "Helvetica",
#     edge.label.color = "black",
#     vertex.color = c("coral1", rep("darkcyan", 3), rgb(0, 0, 0, 0)),
#     vertex.label.family = "Helvetica",
#     vertex.label.color = "white",
#     vertex.label.font = 2,
#     vertex.label.cex = 1.5,
#     vertex.frame.color = c(rep("white", 4), rgb(0, 0, 0, 0)),
#     vertex.size = c(rep(15, 4), 35))
#dev.off()



##open_png (REMOVE # BELOW TO SAVE)
##png(filename = "figures/network_motif.png", width = 20, height = 10, units = "in", res = 1200)
#
#line = -1
#cex = 2.5
#side = 3
#adj = 0.01
#
##par(mfrow = c(2, 2), mar = c(rep(2, 4)))
#
##full network
#plot(wadj, layout = lyo,
#     edge.curved = cedge,
#     edge.width = (abs(E(wadj)$weight) + 0.125) * 3,
#     edge.arrow.size = 0.5,
#     vertex.label.family = "Helvetica",
#     vertex.label.color = "white",
#     vertex.label.font = 2,
#     vertex.label.cex = 1.15,
#     vertex.frame.color = "white")
#
#mtext("A", side = side, line = line, cex = cex, adj = adj)
#
##motifs
#plot(web, layout = mtf,
#     edge.color = c("gray50", "gray50", "gray75", "black", "gray20", "gray20", "black", "gray75"),
#     edge.curved = c(0, 0, 0, 0.25, 0.25, 0.25, 0.25, 0),
#     edge.width = abs(E(web)$weight),
#     edge.arrow.size = 2,
#     edge.loop.angle = c(0, 0, 0, 2, 0, 0, 1, 0),
#     edge.label = c(expression(alpha), expression(gamma), expression(gamma), expression(beta),
#                    expression(beta), expression(lambda), expression(lambda)),
#     edge.label.x = c(-0.35, -0.5, 0.475, -0.1, 0.1, 1.25, -1.215),
#     edge.label.y = c(-0.1, 0.65, 0.65, 0.425, -0.625, -0.475, -0.475),
#     edge.label.font = 2,
#     edge.label.cex = 1.75,
#     edge.label.family = "Helvetica",
#     edge.label.color = "black",
#     vertex.color = c("coral1", rep("darkcyan", 3), rgb(0, 0, 0, 0)),
#     vertex.label.family = "Helvetica",
#     vertex.label.color = "white",
#     vertex.label.font = 2,
#     vertex.label.cex = 1.15,
#     vertex.frame.color = c(rep("white", 4), rgb(0, 0, 0, 0)),
#     vertex.size = c(rep(15, 4), 35))
#
#mtext("B", side = side, line = line, cex = cex, adj = adj)

#dev.off()





#### Distributions
#par(mfrow = c(1, 1), mar = c(rep(0, 4)))

beta_gp <- Reduce('+', beta_gp)
beta_pp <- Reduce('+', beta_pp)
beta <- beta_gp + beta_pp
rm(beta_gp, beta_pp)

library(ggplot2) #load ggplot2
library(cowplot) #load cowplot
library(ggbreak) #axis break

#data frames with only one column, to use in ggplot2
alpha[which(alpha == 0)] <- NA
a <- data.frame("alpha" = c(as.vector(as.matrix(alpha)),
                            rep(NA, prod(dim(alpha)) - prod(dim(alpha)))))
gamma[which(gamma == 0)] <- NA
g <- data.frame("gamma" = c(as.vector(as.matrix(gamma)),
                            rep(NA, prod(dim(alpha)) - prod(dim(gamma)))))
beta[which(beta == 0)] <- NA
b <- data.frame("beta" = c(as.vector(as.matrix(beta)),
                           rep(NA, prod(dim(alpha)) - prod(dim(beta)))))
igr[which(igr == 0)] <- NA
l <- data.frame("lambda" = c(as.vector(as.matrix(igr)),
                             rep(NA, prod(dim(alpha)) - length(igr))))

cc <- c(alpha[which(!is.na(alpha))],
        gamma[which(!is.na(gamma))],
        beta[which(!is.na(beta))])
cn <- c(rep("alpha", length(alpha[which(!is.na(alpha))])),
        rep("gamma", length(gamma[which(!is.na(gamma))])),
        rep("beta", length(beta[which(!is.na(beta))])))

cdist <- data.frame("type" = as.factor(cn), coef = cc)
cdist$type <- factor(cdist$type, levels = c("alpha", "gamma", "beta"))

#histograms:

con_alpha <- round(length(which(!is.na(alpha))) / length(as.vector(alpha)), 2)
con_gamma <- round(length(which(!is.na(gamma))) / length(as.vector(gamma)), 2)
con_beta <- round(length(which(!is.na(beta))) / length(as.vector(beta)), 2)

con_text <- data.frame(
  connectance = c(paste("Cn =", con_alpha, sep = " "),
                  paste("Cn =", con_gamma, sep = " "),
                  paste("Cn =", con_beta, sep = " ")),
  type = factor(levels(cdist$type), levels = levels(cdist$type))
)

sz <- 20
lsz1 <- 1.5
lsz2 <- 1.25
cz <- 0.45

p3 <- 
  ggplot(cdist, aes(x = coef)) +
  geom_density(aes(fill = type, color = type), outline.type = "full", alpha = 0.5) +
  geom_histogram(aes(y = after_stat(density), fill = type, color = type), binwidth = 0.005, alpha = 0.75) +
  facet_grid(rows = vars(type), scales = "free") +
  geom_text(data = con_text, mapping = aes(x = -Inf, y = -Inf, label = connectance),
            hjust = -0.375, vjust = -3, size = sz * cz) +
  scale_fill_manual(name = "Network layers:", values = c("gray20", "gray50", "gray75"),
                    labels = c(expression(alpha), expression(gamma), expression(beta))) +
  scale_color_manual(name = "Network layers:", values = c("gray20", "gray50", "gray75"),
                     labels = c(expression(alpha), expression(gamma), expression(beta))) +
  labs(y = "Density", x = "Interaction coefficients") +
  geom_vline(xintercept = 0, lty = "dotted") +
  coord_cartesian(xlim = c(-0.5, 0.5)) +
  theme_classic() +
  theme(legend.position = c(0.825, 0.9),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = sz * lsz1, face = "bold", family = "Helvetica"),
        axis.title.x = element_text(size = sz * lsz1, family = "Helvetica"),
        axis.title.y = element_text(size = sz * lsz1, family = "Helvetica"),
        axis.text.x = element_text(size = sz, family = "Helvetica"),
        axis.text.y = element_text(size = sz, family = "Helvetica"),
        strip.background = element_blank(),
        strip.text.y = element_blank())



####################################################################################
spp <- read.table("results/str_coex/FG-structural_coexistence.txt", header = TRUE, sep = "\t")

p4 <- ggplot(spp, aes(x = SND, y = SFD)) +
  geom_point(aes(fill = as.factor(feasibility)), color = "black", size = 3, shape = 21) +
  scale_fill_manual(name = "Feasible triplet?",
                    values = c("black", "white"),
                    labels = c("No", "Yes")) +
  scale_y_continuous(name = "Structural fitness\ndifferences (SFD)", expand = c(0.005, 0.005)) +
  scale_x_continuous(name = "Structural niche differences (SND)", expand = c(0.005, 0.005)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "top",
        legend.title = element_text(size = sz * lsz2, family = "Helvetica"),
        legend.text = element_text(size = sz * lsz2, family = "Helvetica"),
        axis.title.x = element_text(size = sz * lsz1, family = "Helvetica"),
        axis.title.y = element_text(size = sz * lsz1, family = "Helvetica"),
        axis.text.x = element_text(size = sz),
        axis.text.y = element_text(size = sz))

pvoid <- ggplot() + theme_void()

pX <- ggarrange(p3, pvoid, p4, nrow = 1, widths = c(5, 1, 5))



#### FULL FIGURE
library(gridBase)
library(grid)

png(filename = "figures/network_distr_coex.png", width = 20, height = 20, units = "in", res = 1000)

layout(matrix(c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2,
                1, 1, 1, 1, 1, 2, 2, 2, 2, 2,
                1, 1, 1, 1, 1, 2, 2, 2, 2, 2,
                1, 1, 1, 1, 1, 2, 2, 2, 2, 2,
                3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
                3, 3, 3, 3, 3, 3, 3, 3, 3, 3), nrow = 6, ncol = 10, byrow = TRUE))
#layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE))

spnames <- 2.85

plot(wadj, layout = lyo,
     edge.curved = cedge,
     edge.width = (abs(E(wadj)$weight) + 0.3) * 3,
     edge.arrow.size = 0.6,
     vertex.label.family = "Helvetica",
     vertex.label.color = "white",
     vertex.label.font = 2,
     vertex.label.cex = spnames,
     vertex.frame.color = "white")

plot(web, layout = mtf,
     edge.color = c("gray50", "gray50", "gray75", "black", "gray20", "gray20", "black", "gray75"),
     edge.curved = c(0, 0, 0, 0.25, 0.25, 0.25, 0.25, 0),
     edge.width = abs(E(web)$weight),
     edge.arrow.size = 2,
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
     vertex.size = c(rep(15, 4), 35))

plot.new()

vps <- baseViewports()
pushViewport(vps$figure)
vp1 <-plotViewport(c(2, 3, 2, 3))
print(pX, vp = vp1)

line = 85
cex = 3
side = 3
adj = 0.01

mtext("a", side = side, line = line, cex = cex, adj = adj)

line = 2

mtext("c", side = side, line = line, cex = cex, adj = adj)

adj = 0.535

mtext("d", side = side, line = line, cex = cex, adj = adj)

line = 85

mtext("b", side = side, line = line, cex = cex, adj = adj)

dev.off(); beep(2)
