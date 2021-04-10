files <- list.files("C:\\Users\\DELL\\Desktop\\FE515\\BH\\may6th", pattern="*.csv")
for (i in files) {
  name <- paste("data.", substr(i, start = 10, stop = 11), substr(i, start = 13, stop = 14), sep = '')
  assign(name, read.csv(paste("C:\\Users\\DELL\\Desktop\\FE515\\BH\\may6th\\", i, sep = '')))
}
data.list <- ls()
data.list <- data.list[which(grepl("data.0", data.list))]


# Define size of buckets and sd of price change and do some other prepare
library(quantmod)
open <- getSymbols("SPY", from = "2010-04-01", to = "2010-05-07", auto.assign = F)
open <- as.numeric(open$SPY.Open)
n <- 50 # number of buckets
bucket.column <- NULL
price.change <- NULL
whole.data.frame <- NULL
j <- 1
for (i in data.list[-length(data.list)]) {
  df <- get(i)
  bucket.column <- c(bucket.column, sum(df$Volume))
  price.change <- c(price.change, diff(c(open[j], df$Last)))
  whole.data.frame <- rbind(whole.data.frame, df)
  j <- j + 1
}
bucket <- mean(bucket.column)/n
estimated.sd <- sd(price.change)
whole.data.frame <- rbind(whole.data.frame, data.0506)
whole.data.frame$index.tao <- NA

splict <- which(whole.data.frame$X == 1)
start.line <- max(splict)

whole.data.frame$Date.Time <- as.POSIXct(as.character(whole.data.frame$Date.Time), format = "%Y-%m-%dT%H:%M:%OS", tz = "")
whole.data.frame$time.bar <- 1: (nrow(whole.data.frame))


# Attempt on volume bucket
index.tao <- 1
whole.data.frame$index.tao[1] <- 1
i <- 1
while (i < nrow(whole.data.frame)) {
  bucket.fitted <- sum(whole.data.frame$Volume[which(whole.data.frame$index.tao == index.tao)])
  temp.fit <- bucket.fitted + whole.data.frame$Volume[i + 1]
  if (bucket.fitted < bucket && temp.fit > bucket ){
    whole.data.frame$index.tao[i + 1] <- index.tao
    index.tao <- index.tao + 1
    whole.data.frame$index.tao[i + 2] <- index.tao
    i <- i + 2
  }else if(temp.fit < bucket){
    whole.data.frame$index.tao[i + 1] <- index.tao
    i <- i + 1
  }else if(bucket.fitted > bucket){
    index.tao <- index.tao + 1
    whole.data.frame$index.tao[i + 1] <- index.tao
    i <- i + 1
  } 
}


# Find |Vs-Vb|
z.seq <- NULL
for (j in 1:nrow(whole.data.frame)) {
  if (j %in% splict){
    z.related <- pnorm((whole.data.frame$Last[j] - open[ceiling(j/390)])/estimated.sd)
  }else{
    z.related <- pnorm((whole.data.frame$Last[j] - whole.data.frame$Last[j-1])/estimated.sd)
  }
  z.seq <- c(z.seq, z.related)
}
vs.minus.vb <- whole.data.frame$Volume * (1 - 2 * z.seq)


# Find VPIN and CDF(VPIN)
training.VPIN <- NULL
VPIN.seq <- NULL
start.tao <- whole.data.frame$index.tao[start.line]

for (j in 59 : (start.tao - 1)) {
  tao.seq <- max(1, (j - 50)) : (j-1)
  numerator <- 0
  denominator <- 0
  for(i in tao.seq){
    timebar.seq <- which(whole.data.frame$index.tao == i)
    numerator <- numerator + abs(sum(vs.minus.vb[timebar.seq]))
    denominator <- denominator + sum(whole.data.frame$Volume[timebar.seq])
  }
  training.VPIN <- c(training.VPIN, as.numeric(numerator)/denominator)
}

for (j in start.tao : max(whole.data.frame$index.tao)) {
  tao.seq <- (j - 50) : (j-1)
  numerator <- 0
  denominator <- 0
  for(i in tao.seq){
    timebar.seq <- which(whole.data.frame$index.tao == i)
    numerator <- numerator + abs(sum(vs.minus.vb[timebar.seq]))
    denominator <- denominator + sum(whole.data.frame$Volume[timebar.seq])
  }
  VPIN.seq <- c(VPIN.seq, as.numeric(numerator)/denominator)
}

CDF.f <- ecdf(training.VPIN)
VPIN.CDF <- CDF.f(VPIN.seq)


# Plot
library(doBy)
related.time <- summaryBy(time.bar ~ index.tao, data = whole.data.frame[start.line : nrow(whole.data.frame),], FUN = min)[-1]
related.time.s <- whole.data.frame$Date.Time[related.time[, 1]-1]
timeline <- as.POSIXct(as.character(data.0506$Date.Time), format = "%Y-%m-%dT%H:%M:%OS", tz = "")
plot(timeline, data.0506$Last, type = "l", lwd = 2, main = "VPIN on May 6th, 2010", ylab = "SPY movement")
time.point <- as.POSIXct(c("2010-05-06 14:40:00 EDT", "2010-05-06 15:08:00 EDT", "2010-05-06 14:39:00 EDT", "2010-05-06 15:14:00 EDT",
                           "2010-05-06 15:18:00 EDT", "2010-05-06 15:31:00 EDT", "2010-05-06 15:35:00 EDT", "2010-05-06 15:38:00 EDT"))
polygon(x = c(time.point[3:4], time.point[4:3]), y = c(106.5, 106.5, 117.2, 117.2), col = "green", border = NA)
polygon(x = c(time.point[5:6], time.point[6:5]), y = c(106.5, 106.5, 117.2, 117.2), col = "green", border = NA)
polygon(x = c(time.point[7:8], time.point[8:7]), y = c(106.5, 106.5, 117.2, 117.2), col = "green", border = NA)
polygon(x = c(time.point[1:2], time.point[2:1]), y = c(106.5, 106.5, 117.2, 117.2), col = "grey", border = NA)
lines(timeline, data.0506$Last, type = "l", lwd = 2)
par(new = T)
plot(related.time.s, VPIN.CDF, type = "b", lwd = 1.5, pch = 15, cex = .7, col = "red", xlim = c(min(timeline), max(timeline)), 
     ylim  = c(0.65, 1),xlab = "", ylab = "", axes = F)
axis(4, at = seq(0.7, 1, 0.05), col = "black", las = 0)
legend("bottomleft", col = c("black", "red"), c("SPY", "CDF(VPIN)"), bty = "n", lty = c(1, 1), pch = c(NA, 15), ncol = 2)
