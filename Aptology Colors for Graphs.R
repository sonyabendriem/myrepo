###### Attribute Histograms ###### 

# to do: change color scheme for Aptology, restrict from -3 to 3

pdf("6.3 Attribute Theta Estimate Distributions (for TM).pdf", onefile=TRUE)
par(mfrow=c(3,2))
for (i in seq_along(atts_theta)){
  the.att <- atts_theta[i]
  the.att.label <- unique(traits[traits$attribute==gsub("_theta", "", the.att), "attributeLabel"])
  g = df2[[the.att]]
  m<-mean(g, na.rm=TRUE)
  std<-sd(g, na.rm=TRUE)
  hist(g, density=200, breaks=20, xlim = c(-3,3), prob=TRUE, col = "#476E87", border="black", main=the.att.label)
  curve(dnorm(x, mean=m, sd=std), 
        col="#27AFAF", lwd=2, add=TRUE, yaxt="n")
}
dev.off()

#green: 30CC69 (second C could be 0)
#royal blue: 2F5CEA (5 could be a S)
#light greenish grey: BBC9C7
#navy: 324158
#bright blue: 34DDE8
#teal: 27AFAF
#grey blue: B7D4DC
#dark grey: 606D6D
#blueish grey: 96B4BD
# aptology dark blue: 476E87