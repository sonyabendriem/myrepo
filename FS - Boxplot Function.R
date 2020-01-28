



VizAutoBox = function(df,grp) {
  pdf("./AttributeBoxplot.pdf", onefile=TRUE)
  par(mfrow=c(2,2))
  n1 = readline(prompt="Starting Column Number: ") #takes the first column number from dataframe
  n1 = as.integer(n1)
  n2 = readline(prompt="End Column Number: ") #takes the last column number from dataframe
  n2 = as.integer(n2)
  lt = readline(prompt = "Legend Title:  ") #name of the legend so we know what type of data is being explored
  lt = as.character(lt)
  pal = ifelse(as.integer(length(unique(grp))) <= 3,"PRGn","Spectral")
  looplst = names(df[n1:n2])
  for (i in seq_along(looplst)) {
    print(
      ggplot(df, aes_string(y = looplst[i], x = "grp",fill = "grp")) +
        geom_boxplot() +
        scale_fill_brewer(name = lt,palette = pal) +
        xlab("")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
    )
  }
  dev.off()
}