data("diamonds")
pl <- list()
data("economics")
p <- ggplot(data.frame(x = c(1,2,3), y = c(1, 4, 9)), aes(x=x, y=y)) + geom_line()
pl$plot1 <- p

pl$list <- list()
pl$list$eco <- ggplot(economics, aes(x=pce, y=unemploy)) + geom_point()
pl$list$dia <- ggplot(diamonds, aes(x=cut, y=depth)) + geom_boxplot()

pp <- ggplot(diamonds, aes(x = price, y = table)) + geom_point()
pp <- labeled_plot(pp, width=10, height=10, filename='lol.pdf')
pl$list$list2 <- list(p=pp)
save_plotlist(pl, '/Users/ally/test', verbose = Inf)
save_plotlist(pl, '/Users/ally/test', overwrite = 'all', verbose = 1)
