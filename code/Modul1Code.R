library(car)
library(ggplot2)
library(MASS)
library(grid)

# read the data:
bodyfat=read.csv("bodyfat.csv",header=T)
# Outlier test:
bodyfat[c(39,42,182),]
# delete outlier:
bodyfat = bodyfat[-c(39,42,182),]

#Features selection:
model <- lm(BODYFAT~., data = bodyfat)
model.none <- lm(BODYFAT~1,data = bodyfat)
model.AIC <- step(model,direction = "both", k=2,scope=list(lower=model.none,upper=model),trace=T)
model.BIC <- step(model,direction = "both", k=log(249),scope=list(lower=model.none,upper=model),trace=T)

#model fitting:
model.BIC = lm(BODYFAT~ HEIGHT +ABDOMEN+WRIST, data= bodyfat)

#check important features:
abdomen = bodyfat$ABDOMEN
wrist = bodyfat$WEIGHT
height = bodyfat$HEIGHT

#diagnose plots:
residuals = model.BIC$residuals

df = data.frame(y = model.BIC$residuals)
a = ggplot(df, aes(sample = y))+ stat_qq()+ geom_point(stat = "qq",color = "darkred")

b = ggplot(data=NULL, aes(x = abdomen, y = residuals)) + 
  geom_point(color = "darkred") + geom_smooth(color = "blue",method = 'loess') 

c = ggplot(data=NULL, aes(x = wrist, y = residuals)) + 
  geom_point(color = "darkred") + geom_smooth(color = "blue",method = 'loess') 

d = ggplot(data=NULL, aes(x = height, y = residuals)) + 
  geom_point(color = "darkred") + geom_smooth(color = "blue",method = 'loess') 

grid.newpage()  
pushViewport(viewport(layout = grid.layout(2,2)))
vplayout <- function(x,y){
  viewport(layout.pos.row = x, layout.pos.col = y)
}
print(a, vp = vplayout(1,1))   
print(b, vp = vplayout(1,2))   
print(c, vp = vplayout(2,1))  
print(d, vp = vplayout(2,2))