# Regex to locate links in text
find_link <- regex("
  \\[   # Grab opening square bracket
  .+?   # Find smallest internal text as possible
  \\]   # Closing square bracket
  \\(   # Opening parenthesis
  .+?   # Link text, again as small as possible
  \\)   # Closing parenthesis
  ",
                   comments = TRUE)

# Function that removes links from text and replaces them with superscripts that are 
# referenced in an end-of-document list. 
sanitize_links <- function(text){
  if (PDF_EXPORT) {
    str_extract_all(text, find_link) %>% 
      pluck(1) %>% 
      walk(function(link_from_text){
        title <- link_from_text %>% str_extract('\\[.+\\]') %>% str_remove_all('\\[|\\]') 
        link <- link_from_text %>% str_extract('\\(.+\\)') %>% str_remove_all('\\(|\\)')
        
        # add link to links array
        links <<- c(links, link)
        
        # Build replacement text
        new_text <- glue('{title}<sup>{length(links)}</sup>')
        
        # Replace text
        text <<- text %>% str_replace(fixed(link_from_text), new_text)
      })
  }
  text
}

# Take entire positions dataframe and removes the links 
# in descending order so links for the same position are
# right next to eachother in number. 
strip_links_from_cols <- function(data, cols_to_strip){
  for (i in seq_len(nrow(data))) {
    for (col in cols_to_strip) {
      data[i, col] <- sanitize_links(data[i, col])
    }
  }
  data
}

# Take a position dataframe and the section id desired
# and prints the section to markdown. 
print_section <- function(position_data, section_id){
  position_data %>% 
    filter(section == section_id) %>% 
    arrange(desc(end)) %>% 
    mutate(id = row_number()) %>% 
    pivot_longer(
      starts_with('description'),
      names_to = 'description_num',
      values_to = 'description',
      values_drop_na = TRUE
    ) %>% 
    group_by(id) %>% 
    mutate(
      descriptions = list(description)
    ) %>% 
    ungroup() %>% 
    filter(description_num == 'description_1') %>% 
    mutate(
      timeline = ifelse(
        is.na(start) | start == end,
        end,
        glue('{end} - {start}')
      ),
      description_bullets = map_chr(descriptions, ~ paste('-', ., collapse = '\n')),
    ) %>% 
    strip_links_from_cols(c('title', 'description_bullets')) %>% 
    mutate_all(~ ifelse(is.na(.), 'N/A', .)) %>% 
    glue_data(
      "### {title}",
      "\n\n",
      "{loc}",
      "\n\n",
      "{institution}",
      "\n\n",
      "{timeline}", 
      "\n\n",
      "{description_bullets}",
      "\n\n\n",
    )
}


# For bib files.
# using bib2df spaces are expected around '=' between keys and values
# read_lines("biblio.bib") %>% str_replace("([a-z])=([\"\\{])", "\\1 = \\2") %>% write_lines("biblio_corrected.bib")
# if needed

print_articles <- function(bibdf, type = "ARTICLE", cv_author = "Ginolhac, A") {
  bibdf %>% 
    filter(CATEGORY == type) %>% 
    arrange(desc(YEAR)) %>% 
    # collapse authors 
    mutate(# find location of cv author in the list
      position = map_int(AUTHOR, ~ which(str_detect(.x, cv_author))),
      # shorten author list when possible
      #AUTHOR2  = case_when(
      #  position > 10 ~ AUTHOR,
      #  position >= 5 ~  AUTHOR[1:5],
      #  TRUE ~ AUTHOR
      #),
      authors = map_chr(AUTHOR, ~ glue_collapse(.x, last = " and ")),
      # highlight cv author in bold
      authors = str_replace(authors, cv_author, glue("**{cv_author}**")),
      # clean up titles from curly braces
      clean_title = str_remove_all(TITLE, "[\\{\\}]")) %>% 
    glue_data(
      "### {clean_title}",
      "\n\n",
      "{authors}",
      " _{JOURNAL}_ ", " **{VOLUME}**, {PAGES}",
      "\n\n",
      "N/A",
      "\n\n",
      "{YEAR}", 
      "\n\n\n",
    )
}
#bibdf <- bib2df::bib2df("citations.bib")
print_articles(bib2df::bib2df("citations.bib"))
