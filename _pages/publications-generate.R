library(tidyverse)
library(here)
library(glue)
library(lubridate)
library(googlesheets4)
gs4_deauth()
publications <- read_sheet("https://docs.google.com/spreadsheets/d/1BZlN8CC1bG2h-a-gSwrCQC17Yl4dlJqwXw5570K_kuc/edit?usp=sharing",
           sheet = "publications",
           col_types = "c") %>% 
  mutate(anchor = ifelse(is.na(short_title), 
                         janitor::make_clean_names(title),
                         janitor::make_clean_names(short_title)))


# create the table data
publications_table <- publications %>%
  arrange(desc(date), type) %>%
    mutate(title = ifelse(!is.na(url), str_c("<a href='", url, "'>", title, "</a>"))) %>% #turn title into link
  mutate(award = case_when(
    award == "Best Paper"         ~ str_c("<i class='fa fa-trophy'></i><em> ", award, "</em> ·"),
    # award == "Honourable Mention" ~ str_c("<img src='/assets/images/ribbon_xs.png' style='height: 1em;'><em> ", award, "</em> ·"),
    TRUE                          ~       ""
  ),
  pdf = ifelse(is.na(pdf), "", str_c("<span class='publication-extra'><a href='", pdf, "'>", 'pdf', "</a></span>")),
  materials = ifelse(is.na(materials),  "", str_c(" · <span class='publication-extra'><a href='", materials, "'>", 'materials', "</a></span>")),
  ebook = ifelse(is.na(ebook), "", str_c(" · <span class='publication-extra'><a href='", ebook, "'>", 'ebook', "</a></span>")),
  blog = ifelse(is.na(blog),  "", str_c(" · <span class='publication-extra'><a href='", blog, "'>", 'blog', "</a></span>")),
  full_talk = ifelse(is.na(full_talk), "", str_c(" · <span class='publication-extra'><a href='", full_talk, "'>", 'video of full talk', "</a></span>")),
  bibtex = ifelse(is.na(bibtex),  "", str_c(" · <span class='publication-extra'><a href='", bibtex, "'>", 'bibtex', "</a></span>"))) %>% 
  mutate(citation = str_c("<a class='anchor' id='", anchor, "'></a>",
                          "<span class='pub-title'>", title, "</span><br>"),
         citation = str_c(citation,
                          authors_full, "<br>",
                          venue, "<br>",
                          award, pdf, materials, ebook, blog, full_talk, bibtex,
                          sep = " ")
           ) %>% 
  mutate(citation = str_replace(citation, "Van Doren BM", "<b>Van Doren BM</b>")) %>%  # make my name bold
  mutate(teaser_video_embed = case_when(
      str_detect(teaser_video_embed, "youtube") ~ glue("<div class='embed-responsive embed-responsive-16by9'><iframe class='embed-responsive-item' src='{teaser_video_embed}' allowfullscreen></iframe></div>"),
      str_detect(teaser_video_embed, "vimeo") ~ glue("<div class='embed-responsive embed-responsive-16by9'><iframe class='embed-responsive-item' src='{teaser_video_embed}' allow='fullscreen'></iframe></div>"),
      is.na(teaser_video_embed) ~ ""
    )) %>%
  mutate(altmetric_badge = ifelse(is.na(doi),"",
                                  str_c("<div data-badge-popover='bottom' data-badge-type='donut' data-doi='",
                                  doi,"' data-hide-no-mentions='true' class='altmetric-embed'></div>")))



# draw a table with heading for each year
outfile <- c('_pages/publications.md')
header <- 
  '---
layout: single
permalink: /publications/
author_profile: true
title: "Publications"
classes: wide
---
'
write_lines(header,outfile)

years <- publications$date %>% year() %>% unique()
for (cur_year in years){
  publications_table %>% 
    filter(year(date) == cur_year) %>% 
    select(citation,altmetric_badge) %>%
    knitr::kable(caption = cur_year, format = "html",
                 table.attr='class="publication-table"', escape = FALSE) %>%
    write_lines(outfile,append=TRUE)
} 


