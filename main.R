library(ggplot2)

gg <- ggplot(iris, aes(Sepal.Length, iris$Sepal.Width, color = Species))

gg + geom_point(size=1) + theme(legend.position = "left") + geom_smooth(method = lm)




## TS_BK
ts.sim <- function(p, q, tb = 20, xe = 100) {
    x <- seq(1, xe, by = 0.1)
    y <- p*(1-q^(x - tb))/(1-q)
    y[1:tb] <- 0
    hx = log10(0.5)/log10(q)
    
    df <- data.frame(x = x, y = y)
    qplot(x, y, data = df, geom = "line") +
        geom_vline(aes(xintercept  = hx), colour = "red") +
        geom_text(aes(x = hx, y = 5, label = round(hx, digits = 2))) + 
        geom_text(aes(x = xe, y = max(y), label = round(max(y), digits = 2)))
}

ts.sim(1, 0.9)

