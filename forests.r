df = read.csv("forests.csv")
df$Soil_Type = apply(df, 1, function(x)
{
  for (i in 1:40)
    if (x[i + 15] == 1) {
      return(i)
    }
  return(0)
})
df$Wilderness_Area = apply(df, 1, function(x)
{
  for (i in 1:4)
    if (x[i + 11] == 1) {
      return(i)
    }
  return(0)
})
df[12:55] = NULL
df$Soil_Type = factor(df$Soil_Type)
df$Wilderness_Area = factor(df$Wilderness_Area)
df$Cover_Type = factor(df$Cover_Type)
library("mgcv")
glm0 = glm(
  Cover_Type ~ .,
  data = df,
  family = binomial()
)
summary(glm0)
glm1 = glm(
  Cover_Type ~ Elevation + Aspect + Vertical_Distance_To_Hydrology
  + Hillshade_Noon + Hillshade_3pm +
    Horizontal_Distance_To_Fire_Points,
  data = df,
  family = binomial()
)
summary(glm1)
gam1 = gam(
  Cover_Type ~ s(Elevation) + Aspect + Vertical_Distance_To_Hydrology
  + Hillshade_Noon + Hillshade_3pm,
  data = df,
  family = binomial()
)
summary(gam1)
