####################
### DENSITY PLOT ###
####################

t1 <- getProbeInfo(rgset, type = "I")[, c("Name", "nCpG")]
t2 <- getProbeInfo(rgset, type = "II")[, c("Name", "nCpG")]
probeTypes <- rbind(t1, t2)
probeTypes$Type <- rep(
  x = c("I", "II"),
  times = c(nrow(t1), nrow(t2)))


tIred <- getProbeInfo(rgset, type = "I-Red")[, "Name"]
tIgreen <- getProbeInfo(rgset, type = "I-Green")[, "Name"]
tII <- getProbeInfo(rgset, type = "II")[, "Name"]


bval <- as.data.frame(getBeta(preprocessRaw(rgset)))
bvalIred <- subset(bval, rownames(bval) %in% tIred)
bvalIgreen <- subset(bval, rownames(bval) %in% tIgreen)
bvalII <- subset(bval, rownames(bval) %in% tII)

class(bvalIred)
rownames(bvalIred)


length(tIred) == dim(bvalIred)[1]
length(tIgreen) == dim(bvalIgreen)[1]
length(tII) == dim(bvalII)[1]

library(ggplot2)
library(plotly)

b <- bval[sample(seq_len(nrow(bval)), 200000), ]

plotbval <- as.data.frame(tidyr::pivot_longer(b, cols = seq_len(ncol(bval)),
                                              names_to = "sample",
                                              values_to = "bval"))


ggplotly(ggplot(plotbval, aes(x = bval, color = sample)) +
           stat_density(position = "identity", geom = "line") +
           theme_bw() +
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())
)

b <- bvalIred[sample(seq_len(nrow(bvalIred)), 200000), ]

plotbval <- as.data.frame(tidyr::pivot_longer(b, cols = seq_len(ncol(bval)),
                                              names_to = "sample",
                                              values_to = "bval"))


ggplotly(ggplot(plotbval, aes(x = bval, color = sample)) +
           stat_density(position = "identity", geom = "line") +
           theme_bw() +
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())
)
b <- bvalIgreen[sample(seq_len(nrow(bvalIgreen)), 200000), ]

plotbval <- as.data.frame(tidyr::pivot_longer(b, cols = seq_len(ncol(bval)),
                                              names_to = "sample",
                                              values_to = "bval"))


ggplotly(ggplot(plotbval, aes(x = bval, color = sample)) +
           stat_density(position = "identity", geom = "line") +
           theme_bw() +
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())
)
b <- bvalII[sample(seq_len(nrow(bvalII)), 200000), ]

plotbval <- as.data.frame(tidyr::pivot_longer(b, cols = seq_len(ncol(bval)),
                                              names_to = "sample",
                                              values_to = "bval"))


ggplotly(ggplot(plotbval, aes(x = bval, color = sample)) +
           stat_density(position = "identity", geom = "line") +
           theme_bw() +
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())
)













channel <- reactive(getProbeInfo(rval_rgset(), type = input$probeType)[, "Name"])

beta_raw <- reactive(subset(rval_rgset_getBeta(), rownames(rval_rgset_getBeta()) %in% channel()))
rval_plot_densityplotraw <- reactive(create_densityplot(beta_raw(), nrow(beta_raw())))

#beta_normalized <- reactive(rval_gset_getBeta()[rownames(rval_gset_getBeta()) %in% channel(),])
#rval_plot_densityplot <- reactive(create_densityplot(beta_normalized(), nrow(beta_normalized())))






