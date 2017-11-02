mat <- rbind(c(0,1,1,1,1),
             rep(0, 5),
             rep(0, 5),
             rep(0, 5),
             rep(0, 5))

colnames(mat) <- c("PlayTennis", "Outlook","Temperature","Humidity","Wind")

rownames(mat) <- colnames(mat)

g <- graph.adjacency(mat, mode = "directed")

l <- rbind(c(5, 7),
           c(0, 5),
           c(3.3, 5),
           c(6.7,5),
           c(10, 5))

plot(g, layout = l)


d <- data.frame(Wind = c("weak", "strong", "weak", "weak", "weak", "strong", 
                         "strong", "weak", "weak", "weak", "strong", "strong",
                         "weak", "strong"),
                PlayTennis = c("no", "no", "yes", "yes", "yes", "no", "yes", 
                               "no", "yes", "yes", "yes", "yes", "yes", "no"))

knitr::kable(prop.table(table(d),2), digits = 3)
