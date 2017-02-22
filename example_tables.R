# This file contains example data used by the shiny application to print tables
### add some example tables
add_gross <- data.frame(
    variable = c("Ex Ante Gross", "A", "B", "C", "Ex Post Gross", "Ex Post NTG", "Ex Post Net"), 
    given = c(100, -30, 20, -40, NA,0.6, NA), 
    total = c(100, NA, NA, NA, 50, NA, 30), 
    base = c(NA,70, 70, 50, NA, 30, NA), 
    increase = c(NA, 0, 20, 0, NA, 0, NA), 
    decrease = c(NA, 30, 0, 40, NA, 20, NA))

add_net <- data.frame(
  variable = c("Ex Ante Gross", "Ex Ante NTG", "Ex Ante Net", "A", "B", "C", "RR NTG", "Ex Post Net"), 
  given = c(100, 0.8, NA, -30, 20, -40, 0.6, NA), 
  total = c(100, NA, 80, NA, NA, NA, NA, 30), 
  base = c(NA, 80, NA, 59, 59, 45, 30, NA), 
  increase = c(NA, 0, NA, 0, 14, 0, 0, NA), 
  decrease = c(NA, 20, NA, 21, 0, 28, 15, NA))

add_hybrid <- data.frame(
  variable = c("Ex Ante Gross", "Ex Ante NTG", "A", "B", "C", "RR NTG", "Ex Post Net"), 
  given = c(100, 0.8, -30, 20, -40, 0.6, NA), 
  total = c(100, NA, NA, NA, NA, NA, 30), 
  base = round(c(NA, 86.6666666666667, 62.9166666666667, 62.9166666666667, 47.0833333333333, 30, NA),1), 
  increase = round(c(NA, 0, 0, 15.8333333333333, 0, 0, NA),1), 
  decrease = round(c(NA, 13.3333333333333, 23.75, 0, 31.6666666666667, 17.0833333333333, NA),1))

mult_gross <- data.frame(
  variable = c("Ex Ante Gross", "HOU", "delta Watts", 
"ISR", "Ex Post Gross", "Ex Post NTG", "Ex Post Net"), given = c(100, 
1.2, 0.6, 0.8, NA, 0.85, NA), total = c(100, NA, NA, NA, 57.6, 
NA, 48.96), 
base = round(c(NA, 100, 75.0666666666667, 57.6, NA, 48.96, 
NA),1), 
increase = round(c(NA, 14.5333333333333, 0, 0, NA, 0, NA),1), 
decrease = round(c(NA, 
0, 39.4666666666667, 17.4666666666667, NA, 8.64, NA),1))

mult_net <- data.frame(
  variable = c("Ex Ante Gross", "Ex Ante NTG", "Ex Ante Net", 
"HOU", "delta Watts", "ISR", "RR NTG", "Ex Post Net"), given = c(100, 
0.65, NA, 1.2, 0.6, 0.8, 0.85, NA), total = c(100, NA, 65, NA, 
NA, NA, NA, 48.96), base = round(c(NA, 65, NA, 65, 46.1533333333333, 
33.1466666666667, 33.1466666666667, NA),1), increase = round(c(NA, 0, 
NA, 10.7266666666667, 0, 0, 15.8133333333333, NA),1), decrease = round(c(NA, 
35, NA, 0, 29.5733333333333, 13.0066666666667, 0, NA),1))

mult_hybrid <- data.frame(
  variable = c("Ex Ante Gross", "HOU", "delta Watts", 
"ISR", "Ex Post Gross", "Ex Post NTG", "Ex Post Net"), given = c(100, 
1.2, 0.6, 0.8, NA, 0.85, NA), total = c(100, NA, NA, NA, 57.6, 
NA, 48.96), base = c(NA, 100, 72, 57.6, NA, 48.96, NA), increase = c(NA, 
20, 0, 0, NA, 0, NA), decrease = c(NA, 0, 48, 14.4, NA, 8.64, 
NA)
)

# End of example tables
