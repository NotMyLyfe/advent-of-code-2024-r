input <- read.table("input", header = FALSE)

day_1_part_1 <- function(input) {
    input$V1 <- sort(as.numeric(input$V1))
    input$V2 <- sort(as.numeric(input$V2))
    diff <- abs(input$V1 - input$V2)
    sum(diff)
}

day_1_part_1(input)
