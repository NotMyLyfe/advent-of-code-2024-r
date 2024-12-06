input <- read.table("input", header = FALSE, fill = TRUE)

day_2_part_1 <- function(input) {
    mat <- as.matrix(input)
    sum(apply(
        X = mat,
        MARGIN = 1,
        FUN = function(x) {
            x <- x[!is.na(x)]
            pair_dif <- x[2:length(x)] - x[1:(length(x) - 1)]

            (length(unique(sign(pair_dif))) == 1 &&
                max(abs(pair_dif)) <= 3 &&
                min(abs(pair_dif)) >= 1)
        }
    ))
}

day_2_part_1(input)
