alt <- c(14, 29, 6, 25, 18, 4, 18, 12, 22, 6, 30, 11, 30, 5, 20,
         13, 9, 32, 24, 13, 19, 4, 28, 14, 29)
resp <- c(0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0,
          1, 0, 0, 1, 1, 1)


ave <- glm(resp ~ alt,family=binomial)

summary(ave)

plot(alt,resp,xlab="Altura das árvores (vezes 10)", 
     ylab="Presença de ave(0 e 1) e prob encontro de ave 
(curva)")
curve((exp(?3.0597 + 0.1615*x))  / (1+(exp(?3.0597 + 
                                              0.1615*x))),add=TRUE)


plot(alt,resp,xlab="Altura das árvores (vezes 10)",
     ylab="Presença de ave(0 e 1) e prob encontro de ave
(curva)")
curve((exp(-3.0597 + 0.1615*x)) / (1+(exp(-3.0597 +
                                            0.1615*x))),add=TRUE)