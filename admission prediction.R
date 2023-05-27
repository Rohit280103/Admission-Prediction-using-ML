#libraries 
library(readxl)
library(datasets)
library(tidyr)
library(utils)
library(ggplot2)
library(ggcorrplot)
library(GGally)
library(caret)
library(psych)
library(shiny)
library(shinythemes)
library(dplyr)

#importing the csv file
df<- read.csv("C:/Users/SAROJ/Desktop/r jury/Admission_Predict.csv")

#Assessing the data set
head(df)
dim(df)

# Get column names 
column_names <- names(df) 
print(column_names)
#Dropping the non-required columns
df <- subset(df, select = -c(Serial.No.))
head(df)


#check for blank values in column
colSums(is.na(df))
#Dropping the rows will null values
df <- na.omit(df)

#rechecking for null values
colSums(is.na(df))



#Checking for unique values in research column
unique(df$Research)
# Replacing "Yes" with 1 and "No" with 0 in the Research column
df$Research <- ifelse(df$Research == "Yes", 1, ifelse(df$Research == "No", 0, df$Research))
head(df)

#Checking the type if data columns
str(df)

# Convert character column to integer
df$Research <- as.integer(df$Research) 
str(df)

#checking outliers with box plot
boxplot(df$GRE.Score, main="Boxplot of Gre Score", xlab="Gre Score", ylab="Value")
boxplot(df$TOEFL.Score, main="Boxplot of Toefel Score", xlab="Toefel score", ylab="Value")
boxplot(df$University.Rating, main="Boxplot of University", xlab="University", ylab="Value")
boxplot(df$SOP, main="Boxplot of SOP", xlab="SOP", ylab="Value")
boxplot(df$LOR, main="Boxplot of LOR", xlab="LOR", ylab="Value")
boxplot(df$CGPA, main="Boxplot of CGPA", xlab="CGPA", ylab="Value")
boxplot(df$Research, main="Boxplot of reseach", xlab="reseach", ylab="Value")
boxplot(df$Chance.of.Admit, main="Boxplot of admission chance", xlab="chance for admission", ylab="Value")

# clearly we have some outliers in some of the columns: Gre score, toefel score
## Removing outliers from columns
# Calculate the lower and upper bounds for outliers using the IQR method
gre_iqr <- IQR(df$GRE.Score)
toefl_iqr <- IQR(df$TOEFL.Score)

gre_lower <- quantile(df$GRE.Score)[2] - (1.5 * gre_iqr)
gre_upper <- quantile(df$GRE.Score)[4] + (1.5 * gre_iqr)

toefl_lower <- quantile(df$TOEFL.Score)[2] - (1.5 * toefl_iqr)
toefl_upper <- quantile(df$TOEFL.Score)[4] + (1.5 * toefl_iqr)

# Remove outliers from the dataset
df <- df[df$GRE.Score >= gre_lower & df$GRE.Score <= gre_upper, ]
df <- df[df$TOEFL.Score >= toefl_lower & df$TOEFL.Score <= toefl_upper, ]

#checking the updated dataset
boxplot(df$GRE.Score, main="Boxplot of Gre Score", xlab="Gre Score", ylab="Value")
boxplot(df$TOEFL.Score, main="Boxplot of Toefel Score", xlab="Toefel score", ylab="Value")


#Exploratory data analysis
describe(df)
summary(df)

# Convert "Research" column to numeric
df$Research <- as.numeric(df$Research)
# Compute the correlation matrix
cor_matrix <- cor(df)
# Create the correlation heatmap using ggcorrplot
ggcorrplot(cor_matrix, lab = TRUE)

# gre score, toefel score and cgpa are most important attributes
# lets confirm it with a scatter plot
plot(df$GRE.Score, df$Chance.of.Admit)
plot(df$TOEFL.Score, df$Chance.of.Admit)
plot(df$CGPA, df$Chance.of.Admit)

#creating a linear regression model

#storing the independent variables in x
X <- df[, c("TOEFL.Score", "CGPA", "GRE.Score")]

#storing the dependent variabel in y
y <- df[, c("Chance.of.Admit")]

# Split dataset into training and testing sets
set.seed(123)
my_split_indices <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[my_split_indices, ]
y_train <- y[my_split_indices]
X_test <- X[-my_split_indices, ]
y_test <- y[-my_split_indices]

nrow(X_train)
nrow(y_train)
nrow(X_test)
nrow(y_test)



# linear regression model
# Fit linear regression model on the training set
model <- lm(y_train ~ ., data = X_train)
predictions <- predict(model, newdata = X_test)


# Compare predicted values with actual values
comparison <- data.frame(Actual = y_test, Predicted = predictions)
print(comparison)

# checkinng for accuracy
# Calculate mean squared error (MSE)
mse <- mean((predictions - y_test)^2)
print(paste("Mean Squared Error (MSE):", mse))

# Calculate mean absolute error (MAE)
mae <- mean(abs(predictions - y_test))
print(paste("Mean Absolute Error (MAE):", mae))

summary(model)$r.squared

# Calculate percentage MSE
mse_percentage <- (mean((y_test - predictions)^2) / mean(y_test^2)) * 100

# Print the percentage values

cat("Percentage MSE:", mse_percentage, "%\n")


summary(model)$r.squared


saveRDS(model, file = "C:/Users/SAROJ/Desktop/r jury/trained_model.RDS")

# Load the trained model	
model <- readRDS("C:/Users/SAROJ/Desktop/r jury/trained_model.RDS")

# UI logic
ui <- fluidPage(
  titlePanel("Admission Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("gre", "GRE Score:", min = 0, max = 340, value = 300),
      numericInput("toefl", "TOEFL Score:", min = 0, max = 120, value = 100),
      numericInput("cgpa", "CGPA:", min = 0, max = 4, value = 3.5),
      actionButton("predictBtn", "Predict"),
      verbatimTextOutput("prediction")
    ),
    
    mainPanel()
  )
)

server <- function(input, output) {
  output$prediction <- renderText({
    prediction <- predict(model, newdata = data.frame(GRE.Score = input$gre, TOEFL.Score = input$toefl, CGPA = input$cgpa))
    paste("Predicted Chance of Admission:", round(prediction, 2))
  })
}

shinyApp(ui = ui, server = server)


