library(rvest)
library(tidyr)
library(dplyr)
library(stringr)
library(stringi)
library(lubridate)
library(ggplot2)
library(ggimage)
library(magick)
library(RColorBrewer)

#Create table manually for the names and urls of different competitions 
urls <- data.frame(name = c("WPC 2019",
                            "WPC 2022 - Qualifying A",
                            "WPC 2022 - Qualifying B",
                            "WPC 2022 - Qualifying C",
                            "WPC 2022 - Final",
                            "WPC 2023 - First Round A",
                            "WPC 2023 - First Round B",
                            "WPC 2023 - First Round C",
                            "WPC 2023 - First Round D",
                            "WPC 2023 - First Round E",
                            "WPC 2023 - First Round F",
                            "WPC 2023 - Semi Final 1",
                            "WPC 2023 - Semi Final 2",
                            "WPC 2023 - Final",
                            "Australia 2022",
                            "Australia 2023",
                            "Canada 2023 - First Round",
                            "Canada 2023 - Final",
                            "Canada 2024 - First Round A",
                            "Canada 2024 - First Round B",
                            "Canada 2024 - Final",
                            "Denmark 2024 - First Round A",
                            "Denmark 2024 - First Round B",
                            "Sweden 2023",
                            "Sweden 2024 - First Round A",
                            "Sweden 2024 - First Round B",
                            "Sweden 2024 - Final",
                            "Spain 2023",
                            "US National 2022",
                            "US National 2024 - First Round A",
                            "US National 2024 - First Round B",
                            "US National 2024 - First Round C",
                            "US National 2024 - Final"                            ),
                   url = c("https://www.worldjigsawpuzzle.org/wjpc/2019/individual/final",
                           "https://www.worldjigsawpuzzle.org/wjpc/2022/individual/A",
                           "https://www.worldjigsawpuzzle.org/wjpc/2022/individual/B",
                           "https://www.worldjigsawpuzzle.org/wjpc/2022/individual/C",
                           "https://www.worldjigsawpuzzle.org/wjpc/2022/individual/final",
                           "https://www.worldjigsawpuzzle.org/wjpc/2023/individual/A",
                           "https://www.worldjigsawpuzzle.org/wjpc/2023/individual/B",
                           "https://www.worldjigsawpuzzle.org/wjpc/2023/individual/C",
                           "https://www.worldjigsawpuzzle.org/wjpc/2023/individual/D",
                           "https://www.worldjigsawpuzzle.org/wjpc/2023/individual/E",
                           "https://www.worldjigsawpuzzle.org/wjpc/2023/individual/F",
                           "https://www.worldjigsawpuzzle.org/wjpc/2023/individual/S1",
                           "https://www.worldjigsawpuzzle.org/wjpc/2023/individual/S2",
                           "https://www.worldjigsawpuzzle.org/wjpc/2023/individual/final",
                           "https://www.worldjigsawpuzzle.org/ajpa/nationals2022/individual",
                           "https://www.worldjigsawpuzzle.org/ajpa/nationals2023/individual",
                           "https://www.worldjigsawpuzzle.org/cjpa/nationals2023/individual",
                           "https://www.worldjigsawpuzzle.org/cjpa/nationals2023/individual/final",
                           "https://www.worldjigsawpuzzle.org/cjpa/nationals2024/individual/A",
                           "https://www.worldjigsawpuzzle.org/cjpa/nationals2024/individual/B",
                           "https://www.worldjigsawpuzzle.org/cjpa/nationals2024/individual/final",
                           "https://www.worldjigsawpuzzle.org/danskpuslespilsforening/2024/individual/A",
                           "https://www.worldjigsawpuzzle.org/danskpuslespilsforening/2024/individual/B",
                           "https://www.worldjigsawpuzzle.org/svenskapusselforbundet/2023/individual",
                           "https://www.worldjigsawpuzzle.org/svenskapusselforbundet/2024/individual/A",
                           "https://www.worldjigsawpuzzle.org/svenskapusselforbundet/2024/individual/B",
                           "https://www.worldjigsawpuzzle.org/svenskapusselforbundet/2024/individual/final",
                           "https://www.worldjigsawpuzzle.org/aepuzz/nationals2023/individual",
                           "https://www.worldjigsawpuzzle.org/usajpa/nationals2022/individual",
                           "https://www.worldjigsawpuzzle.org/usajpa/nationals2024/individual/A",
                           "https://www.worldjigsawpuzzle.org/usajpa/nationals2024/individual/B",
                           "https://www.worldjigsawpuzzle.org/usajpa/nationals2024/individual/C",
                           "https://www.worldjigsawpuzzle.org/usajpa/nationals2024/individual/final"
                           ))
      

#function for testing whether we can download a given url
readUrl <- function(url) {
  out <- tryCatch(
    {
      download.file(url, destfile = "scrapedpage.html", quiet = TRUE)
      return(1)
    },
    error = function(cond) {
      return(0)
    },
    warning = function(cond) {
      return(0)
    }
  )
  return(out)
}

#create an empty data.frame
total_list <- data.frame(name = character(),
                         image_name = character(),
                         cropped_image_name = character(),
                         top_20_average = character())

# go through the urls one by one and get average minutes for top 20 contestants and 
# download the images
for(i in seq_len(nrow(urls))) {

url <- urls$url[i]
readUrl(url)

content <- read_html("scrapedpage.html")

times_by_contestants <- content %>% 
  html_nodes(xpath = '//div[@class="tiempo"]') %>%
  html_text() %>% as.data.frame()

top_20_average <- times_by_contestants %>% head(20) %>% 
                  mutate(time_in_minutes = hour(hms(gsub(" ","", .)))*60 +
                         minute(hms(gsub(" ","", .)))) %>% select(time_in_minutes) %>%
                  summarise(avg_minutes = mean(time_in_minutes))

image_url <- paste0("https://www.worldjigsawpuzzle.org/users/", content %>%
  html_nodes(xpath = '//img[@class="puzzle_unico"]') %>%
  html_attr("src"))

image_name <- paste0(gsub(" ","_",str_to_lower(gsub("[^[:alnum:][:space:]]","",urls$name[i]))),".jpg")

download.file(image_url, destfile = image_name ,mode = "wb")

# Read the image
img <- image_read(image_name)

# Get the image information
info <- image_info(img)

# Check the dimensions
width <- info$width
height <- info$height
print(paste("Width:", width, "Height:", height))

# Crop the image to remove the empty spaces on the sides
new_width <- as.integer(round((width / 10) * 7.75,0))
width_diff <- as.integer((width - new_width) / 2)
units_to_crop_top <- as.integer(round((height / 10) * 1.75,0))
new_height <- height - units_to_crop_top
top_crop <- units_to_crop_top 

geometry <- sprintf("%dx%d+%d+%d", new_width, new_height, width_diff, top_crop)
cropped_img <- image_crop(img, geometry = geometry)

cropped_image_name <- paste0(gsub(" ","_",str_to_lower(gsub("[^[:alnum:][:space:]]","",urls$name[i]))),"_cropped.jpg")

# Save the cropped image
image_write(cropped_img, path = cropped_image_name)

temp_data_frame <- data.frame(name =  urls$name[i], image_name, cropped_image_name, top_20_average)

total_list <<-  rbind(total_list , temp_data_frame)

}

#create ggplot, when reordering by minutes, images might overlap, therefore I left it as it is
total_list_plot <- 
  ggplot(total_list, aes(y = name, x = avg_minutes)) +
  geom_image(aes(image = image_name), size = 0.08) +
  labs(title = "Speed Puzzling - Avg. Minutes Spent by top 20 Contestants Across Competitions") +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(size = 7)) 

#save ggplot
ggsave("speed_puzzling.png", plot = total_list_plot, width = 6, height = 4, dpi = 300)