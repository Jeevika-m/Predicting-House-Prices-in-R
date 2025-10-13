install.packages("readxl")     # run only once
install.packages("ggplot2")    # for plots
install.packages("corrplot")   # for correlation plots

library(readxl)
library(ggplot2)
library(corrplot)


house_data <- read_excel("Real estate valuation data set.xlsx", sheet = "工作表1")


colnames(house_data) <- c("ID", "YearTransaction", "HouseAge", "MRTDistance",
                          "NumConvenienceStores", "Latitude", "Longitude", "PricePerUnit")


house_data <- house_data[, -1]


head(house_data)
str(house_data)
summary(house_data)


house_model <- glm(PricePerUnit ~ HouseAge + MRTDistance + NumConvenienceStores + Latitude + Longitude,
                   data = house_data, family = gaussian())


summary(house_model)


predicted_prices <- predict(house_model, house_data, type = "response")


ggplot(house_data, aes(x=HouseAge, y=PricePerUnit)) +
  geom_point(color="darkgreen") +
  geom_smooth(method="glm", formula=y~x, color="orange") +
  ggtitle("House Price vs Age") +
  theme_minimal()


ggplot(house_data, aes(x=MRTDistance, y=PricePerUnit)) +
  geom_point(color="purple") +
  geom_smooth(method="glm", formula=y~x, color="red") +
  ggtitle("House Price vs Distance to MRT") +
  theme_light()


ggplot(house_data, aes(x=PricePerUnit)) +
  geom_histogram(fill="steelblue", bins=15, color="black") +
  ggtitle("Distribution of House Prices") +
  xlab("Price per Unit") +
  ylab("Frequency")


numeric_vars <- house_data[, c("HouseAge", "MRTDistance", "NumConvenienceStores", "Latitude", "Longitude", "PricePerUnit")]
corrplot(cor(numeric_vars), method="shade", addCoef.col="black", tl.col="blue", tl.srt=45)


rmse <- sqrt(mean((house_data$PricePerUnit - predicted_prices)^2))

mae <- mean(abs(house_data$PricePerUnit - predicted_prices))

cat("GLM (Multiple Regression) RMSE:", rmse, "\n")
cat("GLM (Multiple Regression) MAE:", mae, "\n")


ggplot(house_data, aes(x=PricePerUnit, y=predicted_prices)) +
  geom_point(color="darkblue") +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed", size=1.2) +
  ggtitle("Actual vs Predicted Prices") +
  xlab("Actual Price") +
  ylab("Predicted Price") +
  theme_minimal()
