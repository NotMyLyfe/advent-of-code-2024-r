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
