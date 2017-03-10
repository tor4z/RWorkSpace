library(ggplot2)

gg <- ggplot(iris, aes(Sepal.Length, iris$Sepal.Width, color = Species))

gg + geom_point(size=1) + theme(legend.position = "left") + geom_smooth(method = lm)

library(AMORE)
# P is the input vector
P <- matrix(sample(seq(-1,1,length=1000), 1000, replace=FALSE), ncol=1)
# The network will try to approximate the target P^2
target <- P^2
# We create a feedforward network, with two hidden layers.
# The first hidden layer has three neurons and the second has two neurons.
# The hidden layers have got Tansig activation functions and the output layer is Purelin.
net <- newff(n.neurons=c(1,3,2,1), learning.rate.global=1e-2, momentum.global=0.5,
             error.criterium="LMS", Stao=NA, hidden.layer="tansig",
             output.layer="purelin", method="ADAPTgdwm")
result <- train(net, P, target, error.criterium="LMS", report=TRUE, show.step=100, n.shows=5 )
y <- sim(result$net, P)
plot(P,y, col="blue", pch="+")

points(P,target, col="red", pch="x")


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

