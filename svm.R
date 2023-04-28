library(ggplot2)
library(e1071)

# Set seed for reproducibility
set.seed(123)

# Generate random angle values between 0 and 2*pi
angles_circle1 <- runif(50, min = 0, max = 2*pi)
angles_circle2 <- runif(50, min = 0, max = 2*pi)

# Convert angles to x and y coordinates on a circle of radius 4 and 1
x_circle1 <- 4*cos(angles_circle1)
y_circle1 <- 4*sin(angles_circle1)
x_circle2 <- cos(angles_circle2)
y_circle2 <- sin(angles_circle2)

# Add noise to both circles
noise_circle1 <- rnorm(50, mean = 0, sd = 0.5)
x_circle1 <- x_circle1 + noise_circle1
y_circle1 <- y_circle1 + noise_circle1

noise_circle2 <- rnorm(50, mean = 0, sd = 0.25)
x_circle2 <- x_circle2 + noise_circle2
y_circle2 <- y_circle2 + noise_circle2

# Create data frames for circle 1 and circle 2
circle1_df <- data.frame(x = x_circle1, y = y_circle1, class = "Circle 1")
circle2_df <- data.frame(x = x_circle2, y = y_circle2, class = "Circle 2")

# Combine the two data frames into a single data frame
df <- rbind(circle1_df, circle2_df)

# Encode the class variable as a factor with numeric levels
df$class <- as.numeric(factor(df$class)) - 1

# Create SVM model with radial basis function kernel
svm_model <- svm(class ~ x + y, data = df, kernel = "radial")

# Create grid of x and y values for visualization
x_grid <- seq(-5, 5, by = 0.05)
y_grid <- seq(-5, 5, by = 0.05)
grid_df <- expand.grid(x = x_grid, y = y_grid)

# Predict class labels for grid points
grid_df$class <- predict(svm_model, newdata = grid_df)

# Create a scatter plot with decision boundary
ggplot(df, aes(x = x, y = y, color = factor(class))) +
  geom_point(alpha = 0.8, size = 3) +
  geom_contour(data = grid_df, aes(x = x, y = y, z = class),
               breaks = c(-1, 0, 1), color = "black", alpha = 0.5) +
  labs(x = "X-axis", y = "Y-axis", title = "Linearly separating two circles using SVM") +
  scale_color_manual(values = c("red", "blue"))
