library(reshape2)

beta=coef(mod)
tmp <- as.data.frame(as.matrix(beta))
tmp$coef <- row.names(tmp)
tmp <- reshape2::melt(tmp, id = "coef")
tmp$variable <- as.numeric(gsub("s", "", tmp$variable))
tmp$lambda <- mod$lambda[tmp$variable+1] # extract the lambda values
tmp$norm <- apply(abs(beta[-1,]), 2, sum)[tmp$variable+1] # compute L1 norm

tmp_alt <- tmp[tmp$value !=0,]

lasso.plot <- ggplot(tmp[tmp$coef != "(Intercept)",], aes(log(lambda), value, group = coef)) + 
  geom_line(aes(col = coef), show.legend = F) + 
  scale_x_reverse() +
  xlab("Lambda (log scale)") 

save(file = "04_presentation/data/lasso.plot.rda", lasso.plot)

plotly::ggplotly(lasso.plot)


