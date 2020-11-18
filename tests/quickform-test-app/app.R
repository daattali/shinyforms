library(shinyforms)
quickform(
title = "My Survey",
description = 'Describe your survey here',
questions = list(
  list(id = 'name',
       type = 'shortanswer',
       question = 'What is your name?'),
  list(id = "age",
       type = "numeric",
       question = "Age (yrs)",
       required = TRUE),
  list(id = 'ethnicity',
       type = "multiplechoice",
       question = "Are you of Hispanic, Latino, or of Spanish origin?" ,
       choices = list('No', 'Yes'),
       required = TRUE),
  list(id = 'shortanswer',
       type = 'shortanswer',
       question = 'One word to describe this app',
       required = TRUE),
  list(id = 'user_opinion',
       type = 'paragraph',
       question= 'Please provide any feedback')
),
gmail = FALSE,
folder = 'test-output'
)
