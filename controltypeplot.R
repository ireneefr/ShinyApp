control <- "BISULFITE CONVERSION II"


if ("BISULFITE CONVERSION I" %in% c("BISULFITE CONVERSION I", "BISULFITE CONVERSION II", "HYBRIDIZATION", "SPECIFICITY I",
                                    "SPECIFICITY II", "TARGET REMOVAL")){
  threshold <- 1
} else if ("BISULFITE CONVERSION I" %in% c("EXTENSION", "STAINING", "NON-POLYMORPHIC")){
  threshold <- 5 # you can increase the threshold
  
} else {threshold <- 0}


red <- getRed(rgset)
green <- getGreen(rgset)
ctrlAddress <- getControlAddress(rgset, controlType = control)

red_control <- log2(r[ctrlAddress, ,drop = FALSE])
array <- "R02C02"
subset <- reshape2::melt(red_control)
subset2 <- subset[grepl(array, subset$Var2), ]

ggplot(data=as.data.frame(subset), aes(x=Var2, y=value)) +
  geom_point(color="darkgreen", size=1.5) + scale_y_continuous(limits = c(-1, 20)) +
  theme(axis.text.x = element_text(hjust = 1, angle=45)) +
  geom_hline(yintercept =threshold, linetype="dashed") + ylab("Log2 Intensity") +
  scale_x_discrete(labels=groupNames) + xlab("Samples") #+ ggtitle(paste("Green Channel", title))
