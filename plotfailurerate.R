library(minfi)
rawminfi<-read.metharray(paste0(file.path("/illumina/raw/Arrays","SMET0331","203939360073","203939360073_R0"),1:8,"C01"),force=T)
qcReport.full(rawminfi)
genomestudio <- preprocessNoob(rawminfi,dyeMethod="single")
betas <- getBeta(genomestudio)
pval <- as.matrix(detectionP(rawminfi))
betas <- as.matrix(betas)
betas[pval>=0.01] <- NA
fail<-as.data.frame(cbind(sort((apply(betas,2,function(x) sum(is.na(x)))/nrow(betas))*100)))
colnames(fail)[1]<-"probe_failure_rate"
library(ggplot2)
ggplot(fail,aes(x=rownames(fail),y=probe_failure_rate)) +geom_bar(stat="identity",fill="steelblue")+theme_bw()+scale_y_continuous(expand = c(0,0),limits = c(0,max(fail$probe_failure_rate)+0.5))+geom_hline(yintercept=5, linetype="dashed",color = "red", size=0.5)+ geom_hline(yintercept=10, linetype="dashed",color = "red", size=0.5)+xlab("Sample Name")+ylab("% Probe Failure Rate")+coord_flip()

