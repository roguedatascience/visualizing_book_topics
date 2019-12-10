library(tidyverse)
library(gutenbergr)
library(ggthemes)
library(text2vec)
library(textmineR)
library(stringr)
library(topicmodels)
library(fuzzyjoin)


##################################################
#
# Part 1: Gettting data and setting it up
#
##################################################


works <-
    gutenberg_metadata %>%
    mutate(title =
               title %>%
               str_replace_all('[[:punct:]]', ' ') %>%
               str_replace_all('[^A-z0-9 ]', '') %>%
               str_replace_all(' +', ' '))

top_1000 <-
    read_csv('top_1000.csv') %>%
    mutate(title =
               title %>%
               str_replace_all('[[:punct:]]', ' ') %>%
               str_replace_all('[^A-z0-9 ]', '') %>%
               str_replace_all(' +', ' '))

top_books <-
    top_1000 %>%
    #inner_join(works, by = 'title') %>%
    stringdist_inner_join(works, by = 'title',
                         max_dist = 0.1,  method = 'jw',
                         distance_col = 'Distance') %>%
    select(-title.x) %>%
    rename(title = title.y) %>%
    group_by(title) %>%
    slice(c(1)) %>%
    ungroup()

books_df <-
    gutenberg_download(top_books$gutenberg_id, meta_fields = "title") %>%
    select(-gutenberg_id) %>%
    group_by(title) %>%
    mutate(chapter_log = grepl('^ ?chapter', text, ignore.case = TRUE)) %>%
    mutate(chapter = cumsum(chapter_log)) %>%
    filter(chapter_log == FALSE) %>%
    filter(chapter > 0) %>%
    select(-chapter_log) %>%
    group_by(title, chapter) %>%
    summarise(text = paste(text, collapse = ' ')) %>%
    ungroup() %>%
    mutate(id = paste(title, chapter, sep = '__')) %>%
    filter(!is.na(title)) %>%
    mutate(text =
               text %>%
               str_replace_all('[[:punct:]]', ' ') %>%
               str_replace_all('[^a-zA-Z ]', ''))

write_rds(books_df, 'top_books.rds')




################################
#
# Topic Modeling Calculations
#
################################

books_df <-
    read_rds('top_books.rds')


### DTM

dtm <-
    CreateDtm(tolower(books_df$text), 
              doc_names = books_df$id,
              ngram_window = c(1, 1))

# explore basic frequencies & curate vocabulary
tf <-
    TermDocFreq(dtm = dtm)

# Eliminate words appearing less than 2 times or in more than half of the documents
vocabulary <-
    tf$term[ tf$term_freq > 4 &
                 tf$doc_freq < nrow(dtm) * 0.4 ]

dtm_topic <-
    dtm[, vocabulary]

dtm_topic <-
    dtm_topic[rowSums(dtm_topic) > 0,]


set.seed(13)
lda_model <-
    FitLdaModel(dtm = dtm_topic,
                k = 10,
                iterations = 200)

write_rds(lda_model, 'lda_model.rds')






################################
#
# Process Topic Modeling
#
################################

lda_model <-
    read_rds('lda_model.rds')

topic_dist <-
    lda_model$theta %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    separate(rowname, c('title', 'chapter'), '__')

prc_results <-
    psych::principal(
        topic_dist %>%
            select(starts_with('t_')),
        rotate = 'varimax',
        nfactors = 2,
        scores = TRUE)

prc_scores <-
    prc_results$scores %>%
    as.data.frame()

topic_dist <-
    topic_dist %>%
    bind_cols(prc_scores)


topic_dist %>%
    filter(title == "Excellent Women") %>%
    ggplot(aes(x = RC1, y = RC2)) +
    geom_path(size = 0.03)

# topic_dist %>%
#     ggplot(aes(x = RC1, y = RC2)) +
#     geom_path(size = 0.03) +
#     facet_wrap(~title, ncol = 3) +
#     #theme_tufte(ticks = FALSE) +
#     theme(axis.title.x = element_blank(),
#           axis.text.x = element_blank(),
#           axis.ticks.x = element_blank(),
#           axis.title.y = element_blank(),
#           axis.text.y = element_blank(),
#           axis.ticks.y = element_blank())

book_list_df <-
    topic_dist %>%
    group_by(title) %>%
    summarise(cnt = n()) %>%
    arrange(cnt)




################################
#
# Testing spline
#
################################

topic_dist_1 <-
    topic_dist %>%
    filter(title == "Excellent Women")

plot.new()

spline_df <-
    xspline(topic_dist_1$RC1,
            topic_dist_1$RC2,
            shape = 1, draw = FALSE) %>%
    as.data.frame()

spline_df %>%
    ggplot(aes(x = x, y = y)) +
    geom_path()



################################
#
# Calculationg all splines
#
################################

get_splines <- function(book_title){
    
    topic_dist_temp <-
        topic_dist %>%
        filter(title == book_title)
    
    plot.new()
    
    spline_df_temp <-
        xspline(topic_dist_1$RC1,
                topic_dist_1$RC2,
                shape = 1, draw = FALSE) %>%
        as.data.frame() %>%
        mutate(title = book_title)
    
    return(spline_df_temp)
    
}

all_book_titles <-
    unique(topic_dist$title)

splines_df <-
    map(all_book_titles, get_splines) %>%
    bind_rows()



################################
#
# Visualization example
#
################################

splines_df %>%
    filter(title == 'The Magnificent Ambersons') %>%
    mutate(ord = 1:n()) %>%
    ggplot(aes(
        x = x,
        y = y,
        col = ord
    ),
    fill = 'white') +
    geom_point(alpha = 0.5,
               size = 40,
               shape = 21) +
    scale_radius() +
    #scale_size(range = c(75, 75)) +
    xlim(c( min(spline_df$x) - sd(spline_df$x),
            max(spline_df$x) + sd(spline_df$x) )) +
    ylim(c( min(spline_df$y) - sd(spline_df$y),
            max(spline_df$y) + sd(spline_df$y) )) +
    theme_void() +
    theme(legend.position = 'none') +
    theme(plot.background = element_rect(fill = 'white')) +
    scale_color_gradient(low = '#A8CF37', high = '#71CDE5')

