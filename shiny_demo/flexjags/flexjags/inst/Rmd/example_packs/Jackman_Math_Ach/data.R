#+ echo=FALSE, fig.width=4, fig.height=4

data1 = read.table("students.txt")
data2 = read.table("schools.txt")

# math scores (dependent variable)
math <- data1$math

## j indexes which school the student is in
j <- match(data1$school,
           unique(data1$school))

## number of schools
J <- max(j) 
