
c_light <- c("#DCBCBC")
c_light_highlight <- c("#C79999")
c_mid <- c("#B97C7C")
c_mid_highlight <- c("#A25050")
c_dark <- c("#8F2727")
c_dark_highlight <- c("#7C0000")

fit <- readRDS(file = 'modeling/output/3slopes/fit_chillh10_differentcutpoints.rds')

B <- 50
idx <- rep(1:(B), each=2)
x <- sapply(1:length(idx), function(b) if(b %% 2 == 0) idx[b] + 0.5 else idx[b] - 0.5)/B - 0.01

params <- extract(fit)

obs_counts <- hist(responsedf$obs, breaks=((0:(B)))/B , plot=FALSE)$counts
pad_obs_counts <- sapply(idx, function(n) obs_counts[n])

pred_counts <- sapply(1:4000, function(n) 
  hist(c(params$y_degen_gen[n,], params$y_prop_gen[n,]),  breaks=((0:(B)))/B , plot=FALSE)$counts)

probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
cred <- sapply(1:c(B), function(b) quantile(pred_counts[b,], probs=probs))
pad_cred <- do.call(cbind, lapply(idx, function(n) cred[1:9,n]))

plot(1, type="n", main="Posterior Predictive Distribution",
     xlim=c(0, B)/B , xlab="y",
     ylim=c(0, max(c(obs_counts, cred[9,]))), ylab="")

polygon(c(x, rev(x)), c(pad_cred[1,], rev(pad_cred[9,])),
        col = c_light, border = NA)
polygon(c(x, rev(x)), c(pad_cred[2,], rev(pad_cred[8,])),
        col = c_light_highlight, border = NA)
polygon(c(x, rev(x)), c(pad_cred[3,], rev(pad_cred[7,])),
        col = c_mid, border = NA)
polygon(c(x, rev(x)), c(pad_cred[4,], rev(pad_cred[6,])),
        col = c_mid_highlight, border = NA)
# lines(x, pad_cred[5,], col=c_dark, lwd=2)

lines(x, pad_obs_counts, col="white", lty=1, lw=2)
lines(x, pad_obs_counts, col="black", lty=1, lw=1.5)

