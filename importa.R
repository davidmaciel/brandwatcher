read_bw <- function(file, 
                       cols = c("Url","Twitter Reply to", 
                                "Twitter Retweet of", "Sentiment")){
  assertive::assert_is_character(file, "stop")
  assertive::assert_is_character(cols, "stop")
  data.table::fread(
    file,
    sep = ",",
    skip = 6,
    header = T,
    encoding = "UTF-8",
    select = cols)
}