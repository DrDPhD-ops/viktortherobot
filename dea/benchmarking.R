library(Benchmarking)
library(mosaic)

olympics <- read.csv("olympics.csv")
olympics <- olympics[1:26,]
olympics[,2:ncol(olympics)] <- olympics[,2:ncol(olympics)] + 0.000000000000000001
rownames(olympics) <- as.character(olympics$Country)
olympics <- olympics[,2:ncol(olympics)]
olympics <- as.data.frame(olympics)


# Density plot
plot(density(test,na.rm=TRUE, data=dataName, legend=T, xlab="xLabel", ylab="yLabel", main="main label here")

#olympics$Reward2GDP <- olympics$Reward/(olympics$GDP*1000000000)
#olympics$Athlets2Pop <- olympics$Athletes/(olympics$Population)

dea.plot.frontier(olympics$Athletes, olympics$TotMedals, txt=rownames(olympics), RTS='crs')
dea.plot.frontier(olympics$Athletes, olympics$TotMedals, txt=rownames(olympics), RTS='irs')
dea.plot.frontier(olympics$Athletes, olympics$TotMedals, txt=rownames(olympics), RTS='drs')
dea.plot.frontier(olympics$Athletes, olympics$TotMedals, txt=rownames(olympics), RTS='vrs')

dea.plot.frontier(olympics$Reward, olympics$TotMedals, txt=rownames(olympics), RTS='crs')
dea.plot.frontier(olympics$Reward, olympics$TotMedals, txt=rownames(olympics), RTS='irs')
dea.plot.frontier(olympics$Reward, olympics$TotMedals, txt=rownames(olympics), RTS='drs')
dea.plot.frontier(olympics$Reward, olympics$TotMedals, txt=rownames(olympics), RTS='vrs')
dea.plot.frontier(olympics$Reward, olympics$TotMedals, txt=rownames(olympics), RTS='fdh')






dea1.crs <- dea(olympics$Athletes, olympics$TotMedals, RTS='crs')
dea1.vrs <- dea(olympics$Athletes, olympics$TotMedals, RTS='vrs')


scaleEffect <- cbind(dea1.crs$eff, dea1.vrs$eff - dea1.crs$eff)
rownames(scaleEffect) <- rownames(olympics)
colnames(scaleEffect) <- c('CRS', 'SCALE')

#barplot(t(scaleEffect[,1:2]))
barchart(scaleEffect, horizontal=T, scales = list(y = list(cex=1.5)))


sav <- (1- dea(olympics$Athletes, olympics$TotMedals)$eff)*olympics$Athletes



dea1 <- dea(inputs, outputs, RTS='vrs')
results1 <- cbind(as.character(olympics$Country), dea1$eff) 

p2 <- olympics$Country[x[,2]]
p1 <- olympics$Country[x[,1]]
p <- cbind(as.character(p1), as.character(p2))
cbind(as.character(olympics$Country), p, dea1$eff)


dea1 <- dea(inputs, outputs, RTS='crs')
results1 <- cbind(as.character(olympics$Country), dea1$eff) 

p2 <- olympics$Country[x[,2]]
p1 <- olympics$Country[x[,1]]
p <- cbind(as.character(p1), as.character(p2))
cbind(as.character(olympics$Country), p, dea1$eff)


inputs <- subset(olympics, select=c(Population, Athletes, GDP))
outputs <- subset(olympics, select=c(TotMedals, GoldCount))




dea2 <- dea(inputs, outputs, RTS='crs')
results1 <- cbind(as.character(olympics$Country), dea1$eff) 
results1

dea2 <- dea(inputs, outputs, RTS='vrs')
results1 <- cbind(as.character(olympics$Country), dea1$eff) 
results1



p <- cbind(as.character(p1), as.character(p2))
cbind(p, dea1$eff)




inputs <- subset(olympics, select=c(Athletes, Population, GDP))
outputs <- subset(olympics, select=c(TotMedals, GoldCount))

inputs <- subset(olympics, select=c(Athletes, Population, GDP))
outputs <- subset(olympics, select=c(Athletes, Population, GDP))



dea1 <- dea(inputs, outputs, RTS='vrs')
results2 <- cbind(as.character(olympics$Country), dea1$eff) 
cbind(results1, results2)


dea.plot.frontier(olympics$Athletes, olympics$TotMedals, txt=olympics$Country, RTS='vrs')
dea.plot.frontier(olympics$Athletes, olympics$TotMedals, txt=olympics$Country, RTS='fdh')

dea(olympics$Reward, olympics$TotMedals, RTS='crs')



olympics$Country[olympics$TotMedals/olympics$Reward == max(olympics$TotMedals/olympics$Reward)]

dea.crs <- dea(inputs, outputs, RTS='crs')
dea.vrs <- dea(inputs, outputs, RTS='vrs', SLACK=TRUE)
dea.fdh <- dea(inputs, outputs, RTS='fdh')

plot(dea.fdh$eff, olympics$Reward)
dealogit <- glm(dea.fdh$eff ~ rank(olympics$Reward))


peers.crs <- peers(dea.crs)
peers.crs <- matrix(rownames(olympics)[peers.crs[,1:ncol(peers.crs)]], ncol=ncol(peers.crs))
rownames(peers.crs) <- rownames(olympics)
peers.crs <- as.data.frame(peers.crs)

peers.vrs <- peers(dea.vrs)
peers.vrs <- matrix(rownames(olympics)[peers.vrs[,1:ncol(peers.vrs)]], ncol=ncol(peers.vrs))
rownames(peers.vrs) <- rownames(olympics)
peers.vrs <- as.data.frame(peers.vrs)

peers.fdh <- peers(dea.fdh)
peers.fdh <- matrix(rownames(olympics)[peers.fdh[,1:ncol(peers.fdh)]], ncol=ncol(peers.fdh))
rownames(peers.fdh) <- rownames(olympics)
peers.fdh <- as.data.frame(peers.fdh)

cbind(peers.crs, peers.vrs, peers.fdh)

hfunds <- read.csv("C:/rplaygound/benchmarking/HFcrosssectFeb2014.csv", sep=";", dec=",")
hfunds <- as.data.frame(hfunds)

unique(hfunds$Strategy)

colnames(hfunds)

inputs <- subset(hfunds, select=c(NAVusd))
outputs <- subset(hfunds, select=c(NAVgr1yr))



inputs <- abs(min(inputs)) + inputs + 0.0000000000000001
outputs <- abs(min(outputs)) + outputs + 0.0000000000000001



dea.plot.frontier(inputs[,1], outputs[,1], 
                  txt=hfunds$Name,
                  RTS='drs')





for (i in 5:ncol(hfunds)) {
  hfunds[,i] <- hfunds[,i] + abs(min(hfunds[,i])) + 0.0000000000000001
}




inputs <- subset(hfunds, select=c(NAVusd))
outputs <- subset(hfunds, select=c(NAVgr1yr))




dea.plot.frontier(inputs[,1], outputs[,1], 
                  txt=hfunds$Name,
                  RTS='drs')



inputs <- subset(hfunds, select=c(NAVusd, MStdDev_1Y))
outputs <- subset(hfunds, select=c(NAVgr1yr, NAVgr3yr))



dea1 <- dea(inputs, outputs, RTS='crs')
results1 <- cbind(as.character(hfunds$Name), as.character(hfunds$Strategy), dea1$eff) 

results1[results1[,3]==1,1:2] 


dea1 <- dea(inputs, outputs, RTS='vrs')
results1 <- cbind(as.character(hfunds$Name), as.character(hfunds$Strategy), dea1$eff) 

results1[results1[,3]==1,1:2] 





