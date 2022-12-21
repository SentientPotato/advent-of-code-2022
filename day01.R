##### Part One -----
text_in = paste(readLines("input.txt"), collapse = "\n")
calorie_lists = unlist(strsplit(text_in, "\n\n"))
convert_list = function(list_in) as.numeric(unlist(strsplit(list_in, "\n")))
sums = sapply(calorie_lists, function(x) sum(convert_list(x)))
sums[which.max(sums)]

##### Part Two -----
sum(head(sort(sums, decreasing = TRUE), 3))
