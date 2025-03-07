
#### imports: base, stats, ggplot2, scico
#### suggests:
########( do i need to actually declare the base and stats imports? they're base R packages, so probably not)
### I'll do proper documentation when I add these functions to ztils
### this file is meant to aid in the development of these functions
### expect frequent changes

## mod : the model used to predict
## dat : the data used to render the "real" points on the graph and for aggregating groups to determine prediction limits (should be the same as the data used in the model)
## pvar : the predictor variable (x variable / variable the model will predict against)
## rvar : the response variable (y variable / variable the model is predicting)
## grp : the group; should be a factor; one response curve will be made for each group
## len : the length of the variable over which to predict (higher = more resolution, essentially)
## interval : the type of interval to predict ("confidence" or "prediction")
## correction : the type of correction to apply to the prediction ("normal", "exponential", or "logit")
## pallt : color palette to use (uses scico palettes, because i like them the most. if you want it to use other palettes [RColorBrewer, etc] do it yourself <3 )

predict_groupR <- function(mod, dat, pvar, rvar, grp, len, interval = "confidence", correction = "normal", pallt) {
  ### deparse variables
  d_pvar <- deparse(substitute(pvar))
  d_rvar <- deparse(substitute(rvar))
  d_grp  <- deparse(substitute(grp))
  ### get explicit names (weird, but necessary for renaming the dx columns \>_>/ )
  pvar_name <- colnames(dat[d_pvar])
  rvar_name <- colnames(dat[d_rvar])
  grp_name  <- colnames(dat[d_grp])
  ## get group data ready
  grps  <- sort(unique(dat[[d_grp]]))
  ngrps <- length(grps)
  ## get predictor range for each group
  agg <- aggregate(dat[[d_pvar]] ~ dat[[d_grp]], data = dat, range)
  dx_pvar <- data.frame(pvar = numeric(0))
  for (i in 1:ngrps) {
    tpvar <- data.frame(pvar = seq(agg[[2]][i,1], agg[[2]][i,2], length = len))
    dx_pvar <- rbind(dx_pvar, tpvar)
  }
  dx <- data.frame(grp = rep(agg[[1]], each = len),
                   pvar = dx_pvar)
  colnames(dx) <- c(grp_name, pvar_name)
  ## make prediction
  if (interval == "confidence") {
    ### we don't need to explicitly declare that it's a confidence interval, the predict function defaults to it
    pred <- predict(mod, newdata = dx, se.fit = TRUE, type = "response")
    ### check for correction type
    if (correction == "exponential") {
      dx$mn <- exp(qnorm(0.5,   pred$fit, pred$se.fit))
      dx$lo <- exp(qnorm(0.025, pred$fit, pred$se.fit))
      dx$up <- exp(qnorm(0.975, pred$fit, pred$se.fit))
    } else if (correction == "logit") {
      dx$mn <- plogis(qnorm(0.5,   pred$fit, pred$se.fit))
      dx$lo <- plogis(qnorm(0.025, pred$fit, pred$se.fit))
      dx$up <- plogis(qnorm(0.975, pred$fit, pred$se.fit))
    } else {
      dx$mn <- qnorm(0.5,   pred$fit, pred$se.fit)
      dx$lo <- qnorm(0.025, pred$fit, pred$se.fit)
      dx$up <- qnorm(0.975, pred$fit, pred$se.fit)
    }
  } else { ### end confidence interval
    pred <- predict(mod, newdata = dx, se.fit = TRUE,
                    type = "response", interval = "prediction")
    ### check for correction type
    if (correction == "exponential") {
      dx$mn <- exp(pred$fit[,"fit"])
      dx$lo <- exp(pred$fit[,"lwr"])
      dx$up <- exp(pred$fit[,"upr"])
    } else if (correction == "logit") {
      dx$mn <- plogis(pred$fit[,"fit"])
      dx$lo <- plogis(pred$fit[,"lwr"])
      dx$up <- plogis(pred$fit[,"upr"])
    } else {
      dx$mn <- pred$fit[,"fit"]
      dx$lo <- pred$fit[,"lwr"]
      dx$up <- pred$fit[,"upr"]
    }
  } ### end prediction interval
  ## initialize plot with real data
  p <- ggplot2::ggplot() + 
    ggplot2::geom_point(data = dat, ggplot2::aes(x=.data[[d_pvar]], y=.data[[d_rvar]], color=.data[[d_grp]]))
  ## loop through treatments
  for (g in 1:ngrps) {
    flag <- which(dx[[d_grp]] == grps[g])
    tdx <- dx[flag,]
    p <- p + 
      ggplot2::geom_line(data=tdx, ggplot2::aes(x=.data[[d_pvar]], y=lo, color = .data[[d_grp]]),
                linewidth=1, show.legend=FALSE)+
      ggplot2::geom_line(data=tdx, ggplot2::aes(x=.data[[d_pvar]], y=mn, color = .data[[d_grp]]),
                linewidth=2, show.legend=FALSE)+
      ggplot2::geom_line(data=tdx, ggplot2::aes(x=.data[[d_pvar]], y=up, color = .data[[d_grp]]),
                linewidth=1, show.legend=FALSE)+
      ggplot2::geom_ribbon(data=tdx, ggplot2::aes(x=.data[[d_pvar]], ymin=lo, ymax=up,
                                fill=.data[[d_grp]]), alpha = 0.5)
  }
  p <- p +
    scico::scale_color_scico_d(begin=0.9, end=0.1, palette=pallt)+
    scico::scale_fill_scico_d(begin=0.9, end=0.1, palette=pallt)+
    ggplot2::labs(
      title = paste("Real data vs predicted 95%", interval, "interval"),
      subtitle = paste("Model:", deparse(mod$call))
    )+
    ggplot2::theme_minimal()+
    ggplot2::theme(
      text = ggplot2::element_text(size=16),
      legend.position="right",
      axis.title = ggplot2::element_text(size=16, face= "bold"),
      title = ggplot2::element_text(size=20, face="bold", lineheight = .5),
      plot.subtitle = ggplot2::element_text(size=14, face = "italic")
    )
  p
}

#### end predict_groupR function

## testing
mars <- mtcars
cmod <- lm(mpg~cyl + disp, data=mars)
predict_groupR(cmod, mars, disp, mpg, cyl, 100, pallt = a_palette)

predict_groupR(d24_mass_mod, d24_f, daysfromgermination, mass, treatment, 100, interval = "confidence", correction = "exponential", pallt = a_palette)

##### the following is (pretty much) straight from the textbook and uses 
##### semi-best practices. use this as a reference

len <- 50
agg <- aggregate(daysfromgermination~treatment, data=d24_f, range)
px1 <- seq(agg$daysfromgermination[1,1], agg$daysfromgermination[1,2], length = len)
dx <- data.frame(treatment = rep(agg$treatment, each=len),
                 daysfromgermination = c(px1, px1, px1, px1))
pred <- predict(d24_mass_mod, newdata=dx, se.fit=TRUE, type="response")
dx$mn <- exp(qnorm(0.5, pred$fit, pred$se.fit))
dx$lo <- exp(qnorm(0.025, pred$fit, pred$se.fit))
dx$up <- exp(qnorm(0.975, pred$fit, pred$se.fit))

treatments24 <- sort(unique(d24_f$treatment))
ntreatments24 <- length(treatments24)

p <- ggplot() + 
  geom_point(data = d24_f, aes(x=daysfromgermination, y=mass, color=treatment))
for (i in 1:ntreatments24) {
  flag <- which(dx$treatment == treatments24[i])
  tdx <- dx[flag,]
  p <- p + 
    geom_line(data=tdx, aes(x=daysfromgermination, y=lo, color = treatment),
              linewidth=1, show.legend=FALSE)+
    geom_line(data=tdx, aes(x=daysfromgermination, y=mn, color = treatment),
              linewidth=2, show.legend=FALSE)+
    geom_line(data=tdx, aes(x=daysfromgermination, y=up, color = treatment),
              linewidth=1, show.legend=FALSE)+
    geom_ribbon(data=tdx, aes(x=daysfromgermination, ymin=lo, ymax=up,
                              fill=treatment), alpha = 0.5)
}
p <- p +
  scale_color_scico_d(begin=0.9, end=0.1, palette=a_palette)+
  scale_fill_scico_d(begin=0.9, end=0.1, palette=a_palette)+
  guides(color=guide_legend(title="Treatment"))+
  labs(title = str_wrap("Real vs Predicted Tomato Fruit Mass", 40))+
  xlab("Days From Germination")+
  ylab("Mass (g)")+
  theme_minimal()+
  theme(
    text = element_text(size=16, family="mont"),
    legend.position="right",
    axis.title = element_text(size=16, family = "mont", face= "bold"),
    title = element_text(size=20, family="open", face="bold", lineheight = .5)
  )
p

##### the following abomination is how i was initially making prediction plots
##### it's bad for several reasons, most of all the fact that it creates new
##### models for each treatment, rather than using a single model
##### just to reiterate. DO NOT DO THIS!!! THIS IS HERE FOR REFERENCE ONLY!

# initialize plot
p <- ggplot()+
  geom_point(data=Li_data, aes(x=Qamb, y=logitPS2, color=Treatment, shape=Treatment),
             alpha = 0.8)
# initialize sequence
Qamb <- seq(min(Li_data$Qamb), max(Li_data$Qamb), length=200)
# loop over treatments
for (x in treatment_order) {
  ## treatment specific LM
  lm_x <- (lm(logitPS2 ~ Qamb, 
              data=Li_data %>% filter(Treatment == x)))
  ## calculate predictions
  prx <- as.data.frame(x=Qamb)
  pred <- predict(lm_x, newdata=prx, se.fit=TRUE, type="terms")
  prx$lo <- exp(qnorm(0.025, pred$fit, pred$se.fit))
  prx$mn <- exp(qnorm(0.5, pred$fit, pred$se.fit))
  prx$up <- exp(qnorm(0.975, pred$fit, pred$se.fit))
  prx$Treatment <- x
  prx$Treatment <- factor(prx$Treatment, levels = treatment_order)
  ## add predicted points
  p <- p + 
    geom_line(data=prx, aes(x=Qamb, y=lo, color = Treatment), linewidth=1, show.legend=FALSE)+
    geom_line(data=prx, aes(x=Qamb, y=mn, color = Treatment), linewidth=2, show.legend=FALSE)+
    geom_line(data=prx, aes(x=Qamb, y=up, color = Treatment), linewidth=1, show.legend=FALSE)+
    geom_ribbon(data=prx, aes(x=Qamb, ymin=lo, ymax=up, fill=Treatment), alpha = 0.5)
}
p <- p +
  scale_color_scico_d(begin=0.9, end=0, palette=a_palette)+
  scale_fill_scico_d(begin=0.9, end=0, palette=a_palette)+
  labs(title = str_wrap("Real vs Predicted for Li-600 logitPS2 ~ Treatment + Qamb", 40))+
  ylab("logit PhiPS2")+
  xlab("Ambient Light (lumens)")+
  theme_minimal()+
  theme(
    text = element_text(size=16, family="mont"),
    legend.position="right",
    axis.title = element_text(size=16, family = "mont", face= "bold"),
    title = element_text(size=20, family="open", face="bold", lineheight = .5)
  )

print(p)