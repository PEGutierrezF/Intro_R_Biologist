
library(fs)   # https://fs.r-lib.org/.  fs is a cross-platform, uniform interface to file system operations via R. 
library(readr)

dir_create("data")
dir_create("data_output")
dir_create("fig_output")

download.file("https://ndownloader.figshare.com/files/22031487",
              "data/books.csv", mode = "wb")

download.file("https://ndownloader.figshare.com/files/22051506",
              "data_output/books_reformatted.csv", mode = "wb")
books2 <- read_csv("data_output/books_reformatted.csv")  # load the data and assign it to books


