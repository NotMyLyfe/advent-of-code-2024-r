input <- read.table("input", header = FALSE, fill = TRUE)

safety_check <- function(arr) {
    length(unique(sign(arr))) == 1 &&
        max(abs(arr)) <= 3 &&
        min(abs(arr)) >= 1
}

day_2_part_1 <- function(input) {
    mat <- as.matrix(input)
    sum(apply(
        X = mat,
        MARGIN = 1,
        FUN = function(x) {
            x <- x[!is.na(x)]
            pair_dif <- x[2:length(x)] - x[1:(length(x) - 1)]

            safety_check(pair_dif)
        }
    ))
}

day_2_part_1(input)

day_2_part_2 <- function(input) {
    mat <- as.matrix(input)
    sum(apply(
        X = mat,
        MARGIN = 1,
        FUN = function(x) {
            x <- x[!is.na(x)]
            pair_dif <- x[2:length(x)] - x[1:(length(x) - 1)]

            if (safety_check(pair_dif) ||
                safety_check(pair_dif[-1]) ||
                safety_check(pair_dif[-length(pair_dif)])) {
                return(1)
            }

            for (i in 2:(length(x) - 1)) {
                skipped_dif <- x[i + 1] - x[i - 1]
                dif_list <- c(
                    pair_dif[-c(i, i - 1)],
                    skipped_dif
                )
                if (safety_check(dif_list)) {
                    return(1)
                }
            }
            return(0)
        }
    ))
}

day_2_part_2(input)
