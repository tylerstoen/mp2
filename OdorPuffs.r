library(mgcv)

# cockroach data
download.file(url = 'https://ruizt.quarto.pub/stat545/data/cockroach.RData',
              destfile = 'data/cockroach.RData')
load(here::here('cockroach.RData'))

# the data are firing events ('spike times') for cockroach antennal lobe neurons
# when stimulated by a citronella odor puff; each trial is one 15s stimulation
head(df_n1)

## Panel raster plot from ev_nX (list of spike-time vectors), with stimulus window highlighted
plot_rasters_from_ev <- function(ev, n_trials = 10, trials = NULL,
                                 stim_on = 5.99, stim_off = 6.49,
                                 ncol = 5, T_end = 15, seed = 1) {
  stopifnot(is.list(ev))
  all_trials <- names(ev)
  
  if (is.null(trials)) {
    set.seed(seed)
    trials <- sample(all_trials, size = min(n_trials, length(all_trials)))
  } else {
    trials <- intersect(trials, all_trials)
  }
  
  n_trials <- length(trials)
  nrow <- ceiling(n_trials / ncol)
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)
  par(mfrow = c(nrow, ncol), mar = c(2.2, 2.2, 2.0, 0.5), mgp = c(1.5, 0.5, 0))
  
  for (tr in trials) {
    times <- ev[[tr]]
    times <- times[is.finite(times) & times >= 0 & times <= T_end]
    
    plot(NA, xlim = c(0, T_end), ylim = c(0, 1),
         xlab = "", ylab = "", yaxt = "n", main = tr)
    
    rect(stim_on, 0, stim_off, 1, border = NA, col = adjustcolor("red", 0.5))
    
    if (length(times)) segments(times, 0.2, times, 0.8, lwd = 1)
    
    box()
    axis(1, at = 5.99, labels = 5.99, cex.axis = 0.75)
  }
}

# visualize data: raster plot for neuron 2, 6 stimulation trials
plot_rasters_from_ev(ev_n2, n_trials = 6, ncol = 2)
plot_rasters_from_ev(ev_n3, n_trials = 6, ncol = 2)

# fit an nhpp model to neuron 2
df_n2$trial <- factor(df_n2$trial)
fit <- gam(y ~ stim + s(t, bs = "cr", k = 15) + s(trial, bs = 're'),
           family = poisson(), 
           offset = log_dt,
           gamma = 1.4,
           data = df_n2, 
           method = "REML")

# additive effect of stimulation on log-intensity
plot(fit, select = 1)

# same, but multiplicative effect on intensity
p <- plot(fit, select = 1)[[1]]
x <- p$x
y <- exp(p$fit)
lo <- exp(p$fit - 2 * p$se)
hi <- exp(p$fit + 2 * p$se)
plot(x, y, type = "n",
     xlab = "t (s)", ylab = "multiplicative effect (exp of smooth)",
     main = "effect of stimulation")

# uncertainty ribbon
polygon(c(x, rev(x)), c(lo, rev(hi)),
        border = NA, col = adjustcolor("gray", 0.25))

# highlight stimulus window
usr <- par("usr")
rect(5.99, usr[3], 6.49, usr[4], border = NA, col = adjustcolor("red", 0.20))

# mean curve
lines(x, y, lwd = 1)

# no effect line
abline(h = 1, lty = 2)
