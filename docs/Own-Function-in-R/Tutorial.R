# How to write functions in R
# Elena Kosourova
# https://www.dataquest.io/blog/write-functions-in-r/

# 1
vector <- c(3, 5, 2, 3, 1, 4)

min(vector)
mean(vector)
print(median(vector))
print(sum(vector))
print(range(vector))
print(str(vector))
print(length(vector))
print(sort(vector, decreasing=TRUE))
print(exists('vector'))  ## note the quotation marks

# 2
circumference <- function(r){
  2*pi*r
}
circumference(2)
circumference(10)

foo <- function(r) {
  2 * 3.141 * r
}

# 3
hello_world <- function(){
  'Hello, World!'
}
print(hello_world())

# 4
circumference <- function(r=1){
  2*pi*r
}
print(circumference())
print(circumference(2))

# 5
sum_two_nums <- function(x, y){
  x + y
}
print(sum_two_nums(1, 2))

# 6
sum_two_nums <- function(x, y) x + y
print(sum_two_nums(1, 2))

sum_two_nums <- function(x, y) x + y
sum_two_nums <- \(x, y) x + y

# 7
mean_median <- function(vector){
  mean <- mean(vector)
  median <- median(vector)
  return(c(mean, median))
}
print(mean_median(c(1, 1, 1, 2, 3)))

# 8
subtract_two_nums <- function(x, y){
  x - y
}
print(subtract_two_nums(3, 1))

# 9
subtract_two_nums <- function(x, y){
  x - y
}
print(subtract_two_nums(x=3, y=1))
print(subtract_two_nums(y=1, x=3))

# 10
calculate_calories_women <- function(weight, height, age){
  (10 * weight) + (6.25 * height) - (5 * age) - 161
}

# 11
print(calculate_calories_women(age=30, 60, 165))

calculate_calories_women <- function(weight, height, age=30){
  (10 * weight) + (6.25 * height) - (5 * age) - 161
}
print(calculate_calories_women(60, 165))

# 12
circumference <- function(r){
  2*pi*r
}
circumference_radius_5 <- circumference(5)
print(circumference_radius_5)

# 13
mean_median <- function(vector){
  mean <- mean(vector)
  median <- median(vector)
  return(c(mean, median))
}

# 14
radius_from_diameter <- function(d){
  d/2
}

circumference <- function(r){
  2*pi*r
}

print(circumference(radius_from_diameter(4)))

# 15
sum_circle_ares <- function(r1, r2, r3){
  circle_area <- function(r){
    pi*r^2
  }
  circle_area(r1) + circle_area(r2) + circle_area(r3)
}

print(sum_circle_ares(1, 2, 3))

a <- 5

foo <- function(x){
  a <- x
  a
}

foo(1)
a

# 16
print(circle_area(10))

# 17
circle_area <- function(r){
  pi*r^2
}

sum_circle_ares <- function(r1, r2, r3){
  circle_area(r1) + circle_area(r2) + circle_area(r3)
}

print(sum_circle_ares(1, 2, 3))
print(circle_area(10))
