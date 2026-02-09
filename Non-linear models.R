###############################################################
# PLS 206 Modeling Non-Linear Relationships in R
# (Polynomials • Splines • LOESS • GAMs with mgcv)
###############################################################

## 0) Setup ----------------------------------------------------

# Core packages
library(ggplot2)
library(mgcv)  #3 package that makes the gam function
library(splines) ## allows us to make splines..
library(patchwork)
##install.packages("gratia")
library(gratia)
library(gamair)  ##
library(splines)




theme_set(theme_classic(base_size = 12))
## define color pallete
# Helper for consistent color palette
pal <- c("species1" = "purple", "species2" = "orange2", "species3" = "dodgerblue")

# Helper: build a useful prediction data frame with 95% CI ribbons
# Works for lm() and gam() (Gaussian); for loess() we handle separately below.
predict_df <- function(model, newdata, yname = NULL, se = TRUE, type = "response") {
  p <- predict(model, newdata = newdata, se.fit = se, type = type)
  out <- newdata
  if (is.list(p)) {
    out$.fit <- as.numeric(p$fit)
    out$.se  <- as.numeric(p$se.fit)
  } else {
    out$.fit <- as.numeric(p)
    out$.se  <- NA_real_
  }
  if (!is.null(yname) && yname %in% names(newdata)) {
    out[[yname]] <- newdata[[yname]]
  }
  out$.lwr <- out$.fit - 2 * out$.se
  out$.upr <- out$.fit + 2 * out$.se
  out
}

# Helper: partial-effect predictions for one smooth (mgcv)
# Returns centered smooth estimate and ±2 SE for a single term
partial_smooth_df <- function(model, term, grid, hold = list()) {
  nd <- grid
  # Fill in held-constant values (e.g., other predictors at mean/reference)
  for (nm in names(hold)) nd[[nm]] <- hold[[nm]]
  pt <- predict(model, newdata = nd, type = "terms", se.fit = TRUE)
  # mgcv names smooth terms like "s(x)" or "s(x):group"
  nd$est <- as.numeric(pt$fit[, term, drop = TRUE])
  nd$se  <- as.numeric(pt$se.fit[, term, drop = TRUE])
  nd$lwr <- nd$est - 2 * nd$se
  nd$upr <- nd$est + 2 * nd$se
  nd
}

# A small convenience for clean legend order
# scale_species <- list(
#   scale_color_manual(values = pal, name = "Species"),
#   scale_fill_manual(values = pal,  name = "Species")
# )

## 1) Data simulation (reproducible, no math needed to understand) --------
# We'll create two datasets:
#  (A) species1: single species to compare 1D nonlinear fits
#  (B) drought: three species with different nonlinear curves vs soil moisture

set.seed(42)

# Soil grid for sim
soil_grid <- 1:100

# Noise pattern that increases then stabilizes (purely for realism)
x_knot <- seq(0, 100, by = 10)
sd_seq <- c(0.1, 0.1, 0.25, 0.5, 1, 2, 1, 1, 1, 1, 2)
sd_model <- gam(sd_seq ~ s(x_knot))
sd_pred  <- as.numeric(predict(sd_model, data.frame(x_knot = soil_grid)))
sd_pred  <- pmax(sd_pred, 0.05)

# Generator that smooths a coarse “shape” then adds heteroscedastic noise
geno_sim <- function(coarse_shape, scale = 1) {
  y_raw <- coarse_shape * scale
  m     <- gam(y_raw ~ s(x_knot))
  mu    <- as.numeric(predict(m, data.frame(x_knot = soil_grid)))
  out   <- mu + rnorm(length(soil_grid), sd = sd_pred)
  pmax(out, 0)  # enforce non-negative biomass
}

species_shapes <- list(
  species1 = c(0,0,0,1,4,5,8,10,10,9,7),
  species2 = c(0,0,2,8,9,9,11,12,9,8,6),
  species3 = c(0,0,0,0,0,0,0,10,11,13,15)
)

specieslist <- list(
  species1 = geno_sim(species_shapes$species1, 1.5),
  species2 = geno_sim(species_shapes$species2, 0.7),
  species3 = geno_sim(species_shapes$species3, 1.0)
)

drought <- data.frame(
  species = factor(rep(names(specieslist), each = length(soil_grid))),
  soil    = rep(soil_grid, times = length(specieslist)),
  biomass = unname(unlist(specieslist))
)

species1 <- subset(drought, species == "species1")

# Quick look
str(drought)

## 2) Single-species: baseline scatter + linear model ----------------------

p1 <- ggplot(species1, aes(soil, biomass)) +
  geom_point(size = 1, alpha = 0.8) +
  labs(title = "Single species: biomass vs soil", x = "Soil moisture (index)", y = "Biomass")

p1

# Linear baseline
m_lin <- lm(biomass ~ soil, data = species1)

# Predict across a smooth grid for plotting
grid1 <- data.frame(soil = seq(min(species1$soil), max(species1$soil), length.out = 200))
pred_lin <- predict_df(m_lin, grid1)

p1 +
  geom_line(data = pred_lin, aes(y = .fit), linewidth = 1) +
  geom_ribbon(data = pred_lin, aes(y = .fit, ymin = .lwr, ymax = .upr), alpha = 0.25, fill = "grey70")

## 3) Polynomials (raw and orthogonal) ------------------------------------

# Quadratic (orthogonal poly)---> predicted values
m_poly2 <- lm(biomass ~ poly(soil, 2), data = species1)
pred_poly2 <- predict_df(m_poly2, grid1)

# Cubic (orthogonal poly)
m_poly3 <- lm(biomass ~ poly(soil, 3), data = species1)
pred_poly3 <- predict_df(m_poly3, grid1)

# Raw polynomial (equivalent shape to poly(..., raw = TRUE))
m_raw3 <- lm(biomass ~ soil + I(soil^2) + I(soil^3), data = species1)
pred_raw3 <- predict_df(m_raw3, grid1)

# Overlay: linear vs quadratic vs cubic (orthogonal) vs raw cubic
p1 +
  geom_line(data = pred_lin,   aes(y = .fit, color = "Linear"), linewidth = 1) +
  geom_line(data = pred_poly2, aes(y = .fit, color = "Poly deg 2"), linewidth = 1) +
  geom_line(data = pred_poly3, aes(y = .fit, color = "Poly deg 3 (orthogonal)"), linewidth = 1) +
  geom_line(data = pred_raw3,  aes(y = .fit, color = "Poly deg 3 (raw)"), linewidth = 1, linetype = "11") +
  scale_color_manual(values = c("Linear" = "black", "Poly deg 2" = "dodgerblue",
                                "Poly deg 3 (orthogonal)" = "orange2",
                                "Poly deg 3 (raw)" = "firebrick")) +
  labs(color = NULL, title = "Polynomial fits")

## 4) Splines (natural & B-splines) ---------------------------------------

# Natural cubic spline with df = 6
m_ns <- lm(biomass ~ splines::ns(soil, df = 6), data = species1)
pred_ns <- predict_df(m_ns, grid1)

## spline--> what is a cubic spline--> changing degrees of freedom can do what --> knots fit the line that is inherently non-linear
## adding more abilty to wiggle--> it helps it increase the fit of the non-linear curve
## gam is trying to figure out how many degrees of freedom is there in a non-linear curve
## degrees of freedom is limited by the sample size

# B-spline with df = 6
m_bs <- lm(biomass ~ splines::bs(soil, df = 6), data = species1)
pred_bs <- predict_df(m_bs, grid1)

p1 +
  geom_line(data = pred_ns, aes(y = .fit, color = "Natural spline (df=6)"), linewidth = 1) +
  geom_line(data = pred_bs, aes(y = .fit, color = "B-spline (df=6)"), linewidth = 1) +
  scale_color_manual(values = c("Natural spline (df=6)" = "darkgreen",
                                "B-spline (df=6)" = "magenta4")) +
  labs(color = NULL, title = "Splines provide local flexibility with smooth joins")

## 5) LOESS (local regression) --------------------------------------------


## locally weighted fits 
m_loess <- loess(biomass ~ soil, data = species1, span = 0.4)  # adjust span to see smoother vs wigglier
pred_loess <- data.frame(
  soil = grid1$soil,
  .fit = as.numeric(predict(m_loess, newdata = grid1))
)

p1 +
  geom_line(data = pred_loess, aes(y = .fit), color = "darkgreen", linewidth = 1) +
  labs(title = "LOESS: locally weighted fits (span controls smoothness)")
  #+ geom_smooth() #uncomment to compare

## 6) GAM (single smooth) --------------------------------------------------


## this is better than using a cross validation
# Thin-plate regression spline (default), penalized via REML
m_gam1 <- gam(biomass ~ s(soil, k = 10), data = species1, method = "REML")

pred_gam1 <- predict_df(m_gam1, grid1)

p1 +
  geom_line(data = pred_gam1, aes(y = .fit), color = "red", linewidth = 1) +
  geom_ribbon(data = pred_gam1, aes(y = .fit,ymin = .lwr, ymax = .upr), fill = "red", alpha = 0.2) +
  labs(title = "GAM: penalized smooth (λ via REML)")
## esitmated value-->
## smooth--> effective dof--> can conclude that it is a non linear thing

##edf is saying what? indactes that there is nonlineartity
## going to plot residuals--> and fitted vs respnse--> main dea is--> is ther eweirdness between fitted and resid/ are we cap relationship that is explaining variance

### are we /did we ins thereextra wiggliness that we didn't accoun t dor 
## we have to small of a k value--> 

### lets do three species--> 

## apply next year with him--> talk about fullbright and write a proposal for research...
## something with water, with wetland ecosystem structure. something that provides life.


# Quick “what to report”
summary(m_gam1)  # edf, F, p for smooths; adjusted R2; deviance explained
#(note intercept is not really intercept...its the mean...)
mean(species1$biomass)
gam.check(m_gam1) # residual/k-index checks (adequate k if p-value not small)

## 7) Multi-species: linear vs polynomial vs GAM with by-smooths -----------

p_multi <- ggplot(drought, aes(soil, biomass, color = species)) +
  geom_point(size = 0.9, alpha = 0.8) +
  scale_color_manual(values = pal, name = "Species") +
  labs(title = "Three species across soil gradient")

p_multi

# Linear with interaction
m_lin_multi <- lm(biomass ~ soil * species, data = drought)
pred_lin_multi <- predict_df(m_lin_multi, drought)
p_multi +
  geom_line(data = pred_lin_multi, aes(y=.fit), linewidth = 1)

# Quadratic interaction (raw polynomial terms per species)
m_quad_multi <- lm(biomass ~ species + soil:species + I(soil^2):species, data = drought)
pred_quad_multi <- predict_df(m_quad_multi, drought)
p_multi +
  geom_line(data = pred_quad_multi, linewidth = 1, aes(y=.fit))

# GAM: separate smooth per species + main effect of species (REQUIRED)
m_gam_by <- gam(biomass ~ species + s(soil, by = species, k = 10),
                data = drought, method = "REML")

summary(m_gam_by)     # one smooth per species; check edf/F/p for each
gam.check(m_gam_by)   # k-index ~ 1 is fine

pred_gam_by <- predict_df(m_gam_by, drought)
p_multi +
  geom_line(data = pred_gam_by, linewidth = 1, aes(y=.fit)) +
  labs(title = "GAM with factor–smooth interactions: one curve per species")

# (What not to do): omit main effect of species (only for demonstration)
m_gam_bad <- gam(biomass ~ s(soil, by = species, k = 10), data = drought, method = "REML")
pred_gam_bad <- predict_df(m_gam_bad, drought)
p_multi +
  geom_line(data = pred_gam_bad, aes(y=.fit), linewidth = 1) +
  labs(title = "⚠️ Don’t omit main effect when using by=species")


## 9) Diagnostics figure (optional but good practice) ----------------------
## you need to check if a data set is inhearently non-linear, if you know they are non-linear thats what you'll be using...
dres <- data.frame(
  fit  = fitted(m_gam_by),
  res  = residuals(m_gam_by, type = "pearson")
)

p_resid <- ggplot(dres, aes(fit, res)) +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "steelblue", linewidth = 0.7) +
  labs(x = "Fitted values", y = "Pearson residuals", title = "Residuals vs fitted")

p_qq <- ggplot(dres, aes(sample = res)) +
  stat_qq(alpha = 0.5) +
  stat_qq_line(color = "red") +
  labs(x = "Theoretical quantiles", y = "Sample quantiles", title = "Q–Q plot (Gaussian family)")

p_resid + p_qq

## 10) 2D smooths and non-Gaussian: quick real-data demos ------------------

# (A) gamair::bird (presence/absence): Binomial + spatial smooth s(e, n)
if (!"gamair" %in% rownames(installed.packages())) install.packages("gamair", quiet = TRUE)
library(gamair)
data(bird)
bird <- transform(bird,
                  crestlark = factor(crestlark),
                  linnet    = factor(linnet),
                  e = x/1000, n = y/1000)


### we are plotting predicted value in two dim grid

## how do we know if our data is hearently non-linear?  when its bimodal?
## dummy data set 
# Binomial spatial GAM (probability surface)
m_bird <- gam(linnet ~ s(e, n, k = 400), data = bird, family = binomial, method = "REML")
summary(m_bird)
gam.check(m_bird)

# Predict on a grid
e_seq <- seq(min(bird$e), max(bird$e), length.out = 150)
n_seq <- seq(min(bird$n), max(bird$n), length.out = 150)
grid_bird <- expand.grid(e = e_seq, n = n_seq)

# For binomial, use type="response" for straight probability
grid_bird$prob <- predict(m_bird, newdata = grid_bird, type = "response")


ggplot(grid_bird, aes(e, n, fill = prob)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  labs(title = "Linnet occurrence probability (GAM, binomial)", x = "Easting (km)", y = "Northing (km)")


# (B) gamair::aral (chlorophyll): Gaussian + spatial smooth s(lat, lon)
data(aral)
m_aral <- gam(chl ~ s(lat, lon, k = 200), data = aral, method = "REML")
summary(m_aral)
gam.check(m_aral)

# Predict on a grid
lat_seq <- seq(min(aral$lat), max(aral$lat), length.out = 150)
lon_seq <- seq(min(aral$lon), max(aral$lon), length.out = 150)
grid_aral <- expand.grid(lat = lat_seq, lon = lon_seq)
pred_aral <- predict_df(m_aral, grid_aral, se = TRUE)

ggplot(pred_aral, aes(lon, lat, fill = .fit)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "green") +
  labs(title = "Aral Sea chlorophyll (GAM, Gaussian)", x = "Longitude", y = "Latitude")

## 11) Spatial heterogeneity can confound effects: demo --------------------

# Simulate a field with spatial gradient + genotype effect
set.seed(11)
ko <- data.frame(row = rep(1:30, each = 30),
                 col = rep(1:30, times = 30),
                 accession = sample(c("wildtype","knockout"), 900, replace = TRUE))
ko$dist <- sqrt((ko$row - 10)^2 + (ko$col - 10)^2) / 2
ko$is_knockout <- as.numeric(ko$accession == "knockout")
ko$yield <- 10 + 0.25 * ko$is_knockout + ko$dist / 2 + rnorm(900)

# Naive model (ignores field pattern)
m_ko_naive <- gam(yield ~ accession, data = ko)
summary(m_ko_naive)

# Spatial GAM controls field heterogeneity
m_ko_sp <- gam(yield ~ accession + s(row, col, k = 40), data = ko, method = "REML")
summary(m_ko_sp)

# Visualize predicted field pattern
pred_ko <- predict_df(gam(yield ~ s(row, col, k = 40), data = ko, method = "REML"),
                      ko)
ggplot(pred_ko, aes(col, row, fill = .fit)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "green4") +
  labs(title = "Estimated field heterogeneity (GAM s(row, col))")

## 12) Model comparison overlay (single species, many methods) -------------

# Put several fits on one plot for direct visual comparison
p_compare <- p1 +
  geom_line(data = pred_lin,   aes(y = .fit, color = "Linear"), linewidth = 1) +
  geom_line(data = pred_poly3, aes(y = .fit, color = "Poly deg 3"), linewidth = 1) +
  geom_line(data = pred_ns,    aes(y = .fit, color = "Natural spline (df=6)"), linewidth = 1) +
  geom_line(data = pred_loess, aes(y = .fit, color = "LOESS span=0.4"), linewidth = 1) +
  geom_line(data = pred_gam1,  aes(y = .fit, color = "GAM s(soil), k=10"), linewidth = 1) +
  scale_color_manual(values = c("Linear" = "black",
                                "Poly deg 3" = "darkorange3",
                                "Natural spline (df=6)" = "navy",
                                "LOESS span=0.4" = "darkgreen",
                                "GAM s(soil), k=10" = "red")) +
  labs(color = NULL, title = "One dataset, different non-linear methods (overlay)")

p_compare

## 13) GAM with selection (auto-shrink unhelpful smooths) ------------------

set.seed(7)
n <- 400
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
y  <- 2*x1 + 3*sin(x2) + rnorm(n, sd = 0.6)
df_sel <- data.frame(y, x1, x2, x3)

m_sel <- gam(y ~ s(x1) + s(x2)+ s(x3), data = df_sel, method = "REML", select = TRUE)
summary(m_sel)
# If a smooth’s edf ~ 1, it’s essentially linear; if edf ~ 0, it’s been shrunken away.

# Visualize smooths quickly
draw(m_sel)  # from gratia (nice defaults)

