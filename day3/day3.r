input <- readChar("input", file.info("input")$size)

day_3_part_1 <- function(input) {
    matches <- gregexpr(
        r"[mul\(\d{1,3},\d{1,3}\)]",
        input
    )
    matches <- unlist(regmatches(input, matches))
    matches <- gsub(r"[(mul\(|\))]", "", matches)
    matches <- gsub(",", "*", matches)
    eval_str <- paste(matches, collapse = "+")
    eval(parse(text = eval_str))
}

day_3_part_1(input)

day_3_part_2 <- function(input) {
    total <- day_3_part_1(input)
    void_matches <- gregexpr(
        r"[don't\(\)((?!do\(\))(.|\s))*]",
        input,
        perl = TRUE
    )
    void_matches <- unlist(regmatches(input, void_matches))
    total - sum(
        sapply(
            void_matches,
            day_3_part_1
        )
    )
}

day_3_part_2(input)
