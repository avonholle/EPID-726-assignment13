# sim.growth.data.R

#  pks = c("ggplot2", "gridExtra", "reshape")
#  install.packages(pks)

require(splines)
require(nlme)
require(ggplot2)
library(gridExtra)
require(reshape)

# simulate infant growth data for figure in R
# some code adapted from simcurve.an.20131120.R
# ...............................................


# SITAR model 
# y_it = alpha_i + h( [t-beta_i] / exp(-gamma_i) )
# h is the cubic natural spline

# let t = 0, 1, 2, 5, 6, 7, 9, and 12 (8 fixed time points)

# Simple model adapted from Beath 2008, doi: 10.1002/sim.2718
# ######################################################

# make function for model similar to what was in the program to obtain parameters.
fitnlme2 <- function(age, 
                     s1,s2,s3,s4,
                     salpha0, ealpha, sbeta0, sbeta1) {
  nsmatrix <- as.matrix( ns( (age-sbeta0)/exp(-sbeta1), knots=myknots, Boundary.knots=mybounds) )
  as.vector(  salpha0 + ealpha +
                t( 	matrix(rep(1,4),ncol=4) %*%
                      t( cbind(  s1*nsmatrix[,1], 
                                 s2*nsmatrix[,2], 
                                 s3*nsmatrix[,3], 
                                 s4*nsmatrix[,4]))
  )
  )
}

# Take function from Beath and use it to generate data
s1=0.5
s2=0.5
s3=0.8
s4=0.4

s5=0.4
splinecoefs <- as.matrix(cbind(s1,s2,s3,s4)); splinecoefs
salpha0=0.5
sbeta0=0.2
sbeta1=-0.6
addalpha=0

age.sim = c(0, 1, 2, 5, 6, 7, 9, 12) # age at 0, 1, ..., 12 months
myknots=c(1,2,9) # 3 knots at 2, 6 and 9 months
mybounds=c(0,12) # bounds at 0 to 12 months for spline

test = (age.sim-sbeta0)/exp(sbeta1)
dim(test)
length(test)
nsmatrix <- as.matrix( ns( (age.sim-sbeta0)/exp(sbeta1), knots=myknots, Boundary.knots=mybounds) )
dim(nsmatrix)

bmi.sim2 =  as.vector(  salpha0 + addalpha + 
              t(   matrix(rep(1,4),ncol=4) %*%
                     t( cbind(  s1*nsmatrix[,1], 
                                s2*nsmatrix[,2], 
                                s3*nsmatrix[,3], 
                                s4*nsmatrix[,4]))
))

bmi.sim2 # these are the 8 data points for log(bmi) with these parameters

# #######################################
# Now generate data to simulate curve.
# #######################################
n <- 100
x1 <- seq(0,12,length.out=n) # 12 months time span
col1 <- "blue"

# function with sbeta0 or sbeta1, meant to indicate random effects
y.base <- function (x, alpha, e.alpha, beta0, beta1)
{fitnlme2(x, s1, s2, s3, s4,
          e.alpha, alpha, beta0, beta1)
}

fitnlme2(x1, s1, s2, s3, s4, 
         0,1,2,3)

# GET outcome data via function
fx.1 <- splinefun(x1, y.base(x1,2,0,1,1), method=c("natural")) # test out 

# Set up different shifts in alpha, beta0, and beta1 (3rd, 4th and 5th parameters in the y.base function)
data.1 <- as.data.frame( cbind( x1,
                                y.base(x1,2,0,0,0),
                                y.base(x1,2,0.1,0,0),
                                y.base(x1,2,0,1,0),
                                y.base(x1,2,0,0,0.5),
                                y.base(x1,2,0,0,0),
                                y.base(x1,2,-0.1,0,0), 
                                y.base(x1,2,0,-1,0), 
                                y.base(x1,2,0,0,-0.5))) # put together different types of curves with different alpha, b0 and b1
colnames(data.1) <- c("x1", "bapos", "a1pos", "b0pos", "b1pos", "baneg", "a1neg", "b0neg", "b1neg")
data.1[1:10,]

data.2 <- melt(data.1, id="x1")
head(data.2)
data.2$sign <- substr(data.2$variable,3,3)
levels(factor(data.2$sign))
data.2$v2 <- substr(data.2$variable,1,2)
levels(factor(data.2$v2))
data.2$sign <- factor(data.2$sign,labels=c("negative sign", "positive sign"))
table(data.2$sign)
data.2$sign.2 = with(data.2, {ifelse(v2=="ba", "o", sign)})
table(data.2$sign.2)
levels(factor(data.2$sign.2))
data.2$sign.2 <- factor(data.2$sign.2, labels=c("Negative sign",
                                                "Positive sign",
                                                "Overall mean"))
table(data.2$sign.2)

# Rearrange the data frame so I can plot each random effect in separate panes
# .........................................................
levels(data.2$sign.2)
data.2.extra.a1 = data.2[data.2$sign.2=="Overall mean" & data.2$sign=="positive sign",]
nrow(data.2.extra.a1)
data.2.extra.a1$v2="a1"
data.2.extra.b0 = data.2[data.2$sign.2=="Overall mean" & data.2$sign=="positive sign",]
data.2.extra.b0$v2="b0"
data.2.extra.b1 = data.2[data.2$sign.2=="Overall mean" & data.2$sign=="positive sign",]
data.2.extra.b1$v2="b1"
nrow(data.2.extra.b1)


data.2.add = rbind(data.2[(data.2$sign.2 %in% c("Negative sign", "Positive sign")),],
                   data.2.extra.a1,
                   data.2.extra.b0,
                   data.2.extra.b1)
levels(factor(data.2.add$v2))
with(data.2.add, {table(v2,sign.2)})

# Plot the curve
# ######################################################
levels(factor(data.2$v2))
plot.1 <- ggplot(data=data.2, aes(x=x1, y=value, group=v2, color=v2)) +
  geom_line() +
  facet_wrap(~sign) +
  xlab("Time") +
  ylab("log(weight)") +
  theme_bw() +
  scale_colour_manual("Random effects\nindicating change in:", 
                      values = c("ba" = "black", 
                                 "a1" = "blue", 
                                 "b0" = "red",
                                 "b1" = "green"),
                      labels = c("a1" = expression(paste(" Size: ", alpha[0])),
                                 "b0" = expression(paste(" Tempo: ", beta[0])),
                                 "b1" = expression(paste(" Velocity: ", beta[1])),
                                 "ba" = "Overall mean curve")
  )

plot.1


# Try the size/tempo/velocity in panes instead of all in one plot
# with pos and neg w/in each pane
# ..............................

levels(factor(data.2.add$v2))
data.2.add$v2 = factor(data.2.add$v2, labels=c(expression(paste("Size: ", alpha[i])),
                                               expression(paste("Tempo: ", beta[i])),
                                               expression(paste("Velocity: ", gamma[i]))))
levels(data.2.add$v2)
levels(data.2$sign.2)

plot.2 <- ggplot(data=data.2.add, 
                 aes(x=x1, y=value, group=sign.2, color=sign.2)) +
  geom_line(lwd=2) +
  facet_grid(~v2, labeller = label_parsed) +
  xlab("Time (months)") +
  ylab("log(weight (kg))") +
  theme_bw(base_size=25) +
  theme(legend.position="bottom") +
  scale_colour_manual("Random effects indicating change in:", 
                      breaks = c( "Positive sign",
                                  "Overall mean",
                                  "Negative sign"),
                      values = c("Negative sign" = "blue", 
                                 "Positive sign" = "red",
                                 "Overall mean" = "black")) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12)) +
  scale_y_continuous(breaks = c(1.7, 2.0, 2.2, 2.4, 2.6))

plot.2


#setwd("C:/Users/Ann/Dropbox/unc.grad.school.2015.spring/epid.726/figure/sim.growth")
setwd("C:/Users/vonholle/Dropbox/unc.grad.school.2015.spring/epid.726/figure/sim.growth")
# output plot
png(file ="sim.growth.png", width=3600, height=1800, 
     units="px", res=300, pointsize=12)  
  plot.2
dev.off()


# TEST area.........................
# ..................................
# test = as.matrix(ns((age.sim-sbeta0)/exp(sbeta1), knots=myknots, Boundary.knots=mybounds))
# class(test)
# test$matrix
# 
# dim(t(test))
# dim(splinecoefs)
# test2 = matrix(1,8,5)
# test2
# 
# splinecoefs %*% t(test2)
# 
# splinecoefs %*% t(test)
# test

# 
# fitnlme <- function(age,s1,s2,s3,s4,s5,salpha0,sbeta0,sbeta1){
#   splinecoefs <- as.matrix(cbind(s1,s2,s3,s4,s5))
#   as.vector(salpha0 + t(matrix(rep(1,5),ncol=5) %*%
#                           t(splinecoefs*as.matrix(ns((age-sbeta0)/exp(sbeta1),
#                                                      knots=myknots,Boundary.knots=mybounds)))))
# }

# 
# class(ns((age.sim-sbeta0)/exp(sbeta1), knots=myknots,Boundary.knots=mybounds))
# 
# bmi.sim = as.vector(salpha0 + t(matrix(rep(1,5),ncol=5) %*%
#                                   t(splinecoefs %*% t(as.matrix(ns((age.sim-sbeta0)/exp(sbeta1),
#                                                                    knots=myknots, Boundary.knots=mybounds))))
# )
# )
# 
# sim.male.weight <-
#   nlme(logWt~fitnlme(Agedays,s1,s2,s3,s4,s5,alpha0,beta0,beta1),
#        data=capsMales,
#        fixed = s1+s2+s3+s4+s5+alpha0 ~ 1,
#        random = alpha0+beta0+beta1 ~ 1 | Id,
#        start = c(inits1,inits2,inits3,inits4,inits5,inits0)
#   )



# if you want to add arrows pointing to changes
# .............................................

# 
# plot.1 <- ggplot(data=data.2, aes(x=x1, y=value, group=v2, color=v2)) +
#   geom_line() +
#   facet_wrap(~sign) +
#   geom_segment(data=line.data,
#                aes(x = line.data[,1], 
#                    y = line.data[,2], 
#                    xend = line.data[,3],
#                    yend = line.data[,4]),
#                arrow=arrow(angle=25, length=unit(0.5,"cm")),
#                size=1) +
#   scale_colour_manual("Random effects", 
#                       values = c("ba" = "black", 
#                                  "a1" = "blue", 
#                                  "b0" = "red",
#                                  "b1" = "green"),
#                       labels = c("alpha",
#                                  "beta0",
#                                  "beta1",
#                                  "base")) +
#   xlab("Time") +
#   ylab("log(weight)") +
#   theme_bw() 
# 
# plot.1

# 
# # set up arrows to indicate the different growth curves
# line.data <- data.frame(x1 = c(0,4,6,10,0,4,2,10), 
#                         y1 = c(2,
#                                y.base(4,2,0,0,0), 
#                                y.base(6,2,0,0,0), 
#                                y.base(10,2,0,0,0),
#                                2,
#                                y.base(4,2,0,0,0), 
#                                y.base(2,2,0,0,0), 
#                                y.base(10,2,0,0,0)), 
#                         x2 = c(0,4,4,13,0,4,4,13), 
#                         y2 = c(2,
#                                y.base(4,2,-0.1,0,0), 
#                                y.base(4,2,0,0,0), 
#                                y.base(13,2,0,0,-1),
#                                2,
#                                y.base(4,2,0,0,0), 
#                                y.base(4,2,2,0,0), 
#                                y.base(13,2,0,0,1)
#                         ),
#                         v2 = c("ba", "a1", "b0", "b1", "ba", "a1", "b0", "b1"), 
#                         sign = c("n",  "n", "n", "n", "p", "p", "p", "p"),
#                         sign.2 = c("b",  "n", "n", "n", "b", "p", "p", "p"),
#                         s1=1, l1=0.5)
# 
# line.data$sign <- factor(line.data$sign,labels=c("Negative sign", "Positive sign"))
