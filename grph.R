par(mfrow = c(1,1))
a1 = c(0.1,0.2,0.4,0.5)



a2 = c(0.9309,
       0.9676,
       
       0.9848,
       1
       
       
)
a3 = c(0.2538,
       0.359,
      
       0.4748,
       0.5831
       
)
a4 = c(0.4693,
       0.57409,
      
       0.7964,
       0.9399
       
)
plot(a1, a2, main = "Power against size for size=1000 for log(s)", ylim = c(0,1), col = "red", lty = 1, lwd = 3, type = "b")
lines(a1, a3, ylim = c(0,1), col = "blue", lty = 1, lwd = 3, type = "b")
lines(a1, a4, ylim = c(0,1), col = "green", lty = 1, lwd = 3, type = "b")
legend(x="topleft",legend = c("chaterjee & De","D_n","W_n"), fill = c("red","blue","green"))




a3 = c(0.8447,
       0.9073,
       0.9662,
       0.9971
       
)
a4 = c(0.9591,
       0.9974,
       0.9991,
       1








a1 = c(50,
       100,
       250,
       500,
       1000
)


a2 = c(0.6309,
       0.6676,
       0.7099,
       0.8048,
       0.8447
       
       
)
a3 = c(0.0538,
       0.059,
       0.0632,
       0.0748,
       0.0831
       
)
a4 = c(0.0693,
       0.07409,
       0.08967,
       0.0964,
       0.1399
       
)
plot(a1, a2, main = "Power against size for delta=0.1 for log(s)", ylim = c(0,1), col = "red", lty = 1, lwd = 3, type = "b")
lines(a1, a3, ylim = c(0,1), col = "blue", lty = 1, lwd = 3, type = "b")
lines(a1, a4, ylim = c(0,1), col = "green", lty = 1, lwd = 3, type = "b")
legend(x="topleft",legend = c("Q_n","D_n","W_n"), fill = c("red","blue","green"))





a1 = c(50,
       100,
       250,
       500,
       1000
)


a2 = c(0.7655,
       0.8072,
       0.8686,
       0.9341,
       0.9761
       
       
       
)
a3 = c(0.0501,
       0.0578,
       0.0958,
       0.1446,
       0.2295
       
       
)
a4 = c(0.0606,
       0.075,
       0.1515,
       0.27434,
       0.49608
       
)
plot(a1, a2, main = "Power against size for delta=0.1 for log(s)", ylim = c(0,1), col = "red", lty = 1, lwd = 3, type = "b")
lines(a1, a3, ylim = c(0,1), col = "blue", lty = 1, lwd = 3, type = "b")
lines(a1, a4, ylim = c(0,1), col = "green", lty = 1, lwd = 3, type = "b")
legend(x="topleft",legend = c("Q_n","D_n","W_n"), fill = c("red","blue","green"))
