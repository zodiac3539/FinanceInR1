information_table <- data.frame(
    month=c("1m", "3m", "6m"),
    libor_rate=c(0.01, 0.02, 0.03),
    tbill_rate=c(0.015, 0.022, 0.033)
)

add_column <- data.frame(
    rprate=c(0.01, 0.022, 0.031)
)

add_table <- data.frame(
    month=c("12m", "18m"),
    libor_rate=c(0.04, 0.05),
    tbill_rate=c(0.045, 0.055)
)

libor<-c(0.01, 0.02, 0.03)
tbill<-c(0.01, 0.02, 0.05, 0.12)
swap<-c("1m", "2m", "3m", "6m", "12m")
counterpartyrisk<-c(TRUE, FALSE, TRUE)
list_all <- list(libor, tbill, swap, counterpartyrisk)

information_table <- data.frame(
    people_name=c("Tom", "Jane", "Greg", "Kelly"),
    people_gender=c(1, 2, 1, 2)
)
information_table$people_gender <- factor(information_table$people_gender, labels=c("Male", "Female"))

getsquare<-function(x) {
    y <- x^2
    return(y)
}

getsquare(2)
getsquare(4)

information_table <- data.frame(
    libor_rate=c(0.01, 0.02, 0.03),
    tbill_rate=c(0.015, 0.022, 0.033)
)

changevalues <- function(x) {
    y <- x*2
    return (y)
}

information_table<-sapply(information_table, changevalues)

is_positive <- function(x) {
    if(x > 0) {
        print("It's positive")
    } else if(x == 0) {
        print("It's Zero")
    } else {
        print("It's negative")
    }
}
