df = read.csv("forests.csv")
library("mgcv")
m = glm(Cover_Type ~ Elevation + Slope,data = df)
