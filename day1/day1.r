input <- read.table("input", header = FALSE)

day_1_part_1 <- function(input) {
    input$V1 <- sort(input$V1)
    input$V2 <- sort(input$V2)
    diff <- abs(input$V1 - input$V2)
    sum(diff)
}

day_1_part_1(input)

day_1_part_2 <- function(input) {
    freq_table <- table(input$V2)
    stringed_input <- as.character(input$V1)
    freq <- freq_table[stringed_input]
    freq[which(is.na(freq))] <- 0
    sum(
        input$V1 * freq
    )
}

day_1_part_2(input)
