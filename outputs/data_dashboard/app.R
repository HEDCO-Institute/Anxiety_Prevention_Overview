#install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rio, here, readxl, shiny, bib2df, stringi, DT, openxlsx)

#### Set-up ####

# Specify update dates
last_search <- "May 2023"
next_search <- "May 2025"

#Import cleaned app data (for exporting all data)
app_df <- import(here("data", "apo_app_data.xlsx")) %>% 
  mutate(percent_race_ethnicity = str_replace_all(percent_race_ethnicity, "Mixed", "Multiracial")) %>% #per user-testing feedback
  relocate(percent_race_ethnicity, study_percent_ell, study_percent_frpl, study_percent_female, Intervention, outcome_list, link_text, link_author, .after = last_col())


# Functions
# Function to create HTML links for each intervention
create_links <- function(interventions, websites, clearinghouses) {
  # Split the interventions, website links, and clearinghouse links by "; " and " |~| "
  interventions <- str_split(interventions, "; ")[[1]]
  websites <- str_split(websites, " \\|~\\| ")[[1]]
  clearinghouses <- str_split(clearinghouses, " \\|~\\| ")[[1]]
  
  # Remove "NA", empty strings, and trim whitespace
  websites <- websites[!is.na(websites) & websites != ""]
  clearinghouses <- clearinghouses[!is.na(clearinghouses) & clearinghouses != ""]
  
  # Get unique website and clearinghouse links (excluding empty strings)
  unique_websites <- unique(trimws(websites))
  unique_clearinghouses <- unique(trimws(clearinghouses))
  
  # Construct the combined link text for Website and Clearinghouse
  website_link <- if (length(unique_websites) > 0) {
    paste0("<a href='", unique_websites[1], "' target='_blank'>Website</a>")
  } else {
    ""
  }
  
  clearinghouse_link <- if (length(unique_clearinghouses) > 0) {
    paste0("<a href='", unique_clearinghouses[1], "' target='_blank'>Clearinghouse</a>")
  } else {
    ""
  }
  
  # Combine the links with " | " separator if both are available
  combined_links <- paste(
    c(website_link, clearinghouse_link)[nchar(c(website_link, clearinghouse_link)) > 0],
    collapse = " | ")
  
  # Combine the interventions with "; " separator
  combined_interventions <- paste(interventions, collapse = "; ")
  
  # Append the combined links at the end of the full intervention list if there are any valid links
  if (nchar(combined_links) > 0) {
    paste0(combined_interventions, " (", combined_links, ")")
  } else {
    combined_interventions
  }
}


#Tidy data for dashboard
a5 <- app_df %>% 
  mutate(linked_title = ifelse(!is.na(link_text), paste0("<a href='", link_text, "' target='_blank'>", title, "</a>"), title),
         linked_author = ifelse(!is.na(link_author), paste0("<a href='", link_author, "' target='_blank'>", study_cor_author, "</a>"), study_cor_author),
         intervention_links = pmap_chr(list(Intervention, website_links, clearinghouse_links),
                                       create_links)) %>% 
  mutate(percent_race_ethnicity = str_replace_all(percent_race_ethnicity, "Mixed", "Multiracial")) %>% #per user-testing feedback
  rename(intervention_name = Intervention,
         Intervention = intervention_links) %>% 
  select(study_publication_year, linked_title, linked_author, everything()) %>% 
  relocate(percent_race_ethnicity, study_percent_ell, study_percent_frpl, study_percent_female, Intervention, outcome_list, .after = last_col())
  

# Define filter options
country_choices <- sort(unique(a5$study_country))
state_choices <- sort(unique(a5$study_state[!a5$study_state %in% c("Non-US Study", "Not reported")]))
grade_choices <- c("K", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
schtyp_choices <- sort(unique(trimws(unlist(strsplit(a5$study_school_type[!a5$study_school_type %in% c("Not reported")], ";")))))
community_choices <- sort(unique(trimws(unlist(strsplit(a5$study_school_area[!a5$study_school_area %in% c("Not reported")], ";")))))
school_choices <- c("Elementary School", "Primary School", "Middle School", "High School", "Secondary School")
outcome_choices <- sort(unique(trimws(unlist(strsplit(a5$outcome_list[!a5$outcome_list %in% c("Not reported")], ";")))))
intervention_choices <- list(
  "Name Brand Interventions" = c(
    "Aussie Optimism",
    "Cool Kids Program",
    "e-Couch Anxiety and Worry Program",
    "e-GAD",
    "FRIENDS",
    "Lessons for Living: Think well, do well",
    "Norwegian Universal Preventive Program for Social Anxiety",
    "Positive Search Training",
    "Taming Worry Dragons",
    "Think, Feel, Do",
    "Thiswayup",
    "Unified Protocol for Transdiagnostic Treatment of Emotional Disorders"),
  "Generic Interventions" = c(
    "Behavioral Activation",
    "Cognitive Behavior",
    "Emotion Regulation",
    "Other Prevention Practice")
  )


#### UI #### 
ui <- fluidPage(
  # HTML customization ####
  tags$head(
    tags$style(
      HTML('
           .title-panel {
             text-align: left;
             padding-left: 10px; 
             font-size: 28px;
             font-family: "Source Sans", sans-serif;
           }
           
           .dt-center.dt-body-center.column-Title {
             width: 700px !important; 
           }
           
           body {
             font-family: "Source Sans", sans-serif; 
           }
           
           .reset-button {
             padding-left: 5px; 
           }
           
           table {
             border-collapse: collapse;
             width: 100%;
             border: 1px solid #ddd;
           }
           th, td {
             text-align: left;
             padding: 8px;
             border-bottom: 1px solid #ddd;
           }
           
           .table-container {
             display: grid;
             grid-template-columns: repeat(3, 1fr); 
             gap: 5px;
           }
           .table {
             padding: 5px;
           }
      
            $(document).ready(function() {
             $("[data-toggle=tooltip]").tooltip();
           });
         ')
      ),
    
    # Google Analytics tracking info (TODO: UPDATE WITH APO INFO) ####
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Open+Sans"),
    HTML('<!-- Google tag (gtag.js) -->
          <script async src="https://www.googletagmanager.com/gtag/js?id=G-N0K06NP7VK"></script>
          <script>
            window.dataLayer = window.dataLayer || [];
            function gtag(){dataLayer.push(arguments);}
            gtag("js", new Date());
          
            gtag("config", "G-N0K06NP7VK");
          </script>')
       ),
  
  # Application title ####
  tags$h1("Anxiety Prevention Research Database", class = "title-panel"),
  
  # Instructions ####
  div("Step 1 - Select criteria to filter data:", 
      style = "text-align: left; font-size: 16px; margin-top: 10px; padding-left: 10px; color: #007030; font-weight: bold;"),
  div("Tip: Filters will show all studies that include, but are not limited to, your selected filter(s). If an option isn't available in a filter, no studies included data for it.", 
      style = "text-align: left; font-size: 12px; margin-top: 2px; padding-left: 10px; margin-bottom: 5px;"),
  
  # Filters ####
  fluidRow(
    div(
      selectizeInput(
        "country_filter",
        "Country:",
        choices = country_choices,
        multiple = TRUE
      ),
      style = "display:inline-block; width:25%; margin-left: 25px;"
    ),
    div(
      selectizeInput("school_type_filter", "School Type:", choices = schtyp_choices, multiple = TRUE),
      style = "display:inline-block; width:25%;"
    ),
    div(
      selectizeInput("urbanicity_filter", "Community Type (Rurality):", choices = community_choices, multiple = TRUE),
      style = "display:inline-block; width:25%;"
    )
  ),
  
  # Conditional display of the state filter when "United States" is selected
  conditionalPanel(
    condition = "input.country_filter.includes('United States')",
    fluidRow(
      div(
        selectizeInput("state_filter", "State:", choices = state_choices, multiple = TRUE),
        style = "display:inline-block; width:25%; margin-left: 25px;"
      )
    )
  ),
  
  # Common second row for grade level, outcome, and research design filters
  fluidRow(
    div(
      selectizeInput("grade_level_filter", "Grade Level:", choices = grade_choices, multiple = TRUE),
      style = "display:inline-block; width:25%; margin-left: 25px;"
    ),
    div(
      selectizeInput("school_level_filter", "School Level:", choices = school_choices, multiple = TRUE),
      style = "display:inline-block; width:25%;"
    ),
    div(
      selectizeInput("outcome_filter", "Outcome:", choices = outcome_choices, multiple = TRUE),
      style = "display:inline-block; width:25%;"
    ),
  ),
  
  # Reset button
  fluidRow(
    div(
      selectizeInput("intervention_filter", "Intervention:", choices = intervention_choices, multiple = TRUE),
      style = "margin-left: 25px; display:inline-block; width:50%;"
    ),
    div(actionButton("resetFilters", "Reset Filters", class = "reset-button"),
        style = "padding-left: 30px;")
  ),
  
  # Instructions ####
  div("Step 2 - Results that meet your criteria are below:", 
      style = "text-align: left; font-size: 16px; margin-top: 10px; padding-left: 10px; margin-bottom: 5px; color: #007030; font-weight: bold;"),
  
  # TabsetPanel data table, summary statistics, and glossary ####
  tabsetPanel(type = "tabs",
              ##### Data Table #####
              tabPanel("Data Table",
                       mainPanel(
                         h2("Research Articles", style = "display: inline-block; margin-right: 20px;"),
                         div(style = "display: flex; justify-content: space-between; align-items: center; ", 
                             p("All studies meeting your criteria:"),
                             ##### Download Button #####
                             div(
                               downloadButton("downloadData", "Download All Data", style = "display: inline-block; margin-right: 10px; margin-bottom: 5px; margin-left: 10px;"),
                               downloadButton("downloadFilteredData", "Download Filtered Data", style = "display: inline-block; margin-bottom: 5px; margin-left: 10px;")
                             )
                         ),
                         DTOutput("table")
                       )
              ),
              ##### Summary Table#####
              tabPanel("Summary Statistics",
                       h2("Summary Statistics"),
                       p("The tables below present frequencies based on the filters you have selected above. The tables will automatically update as you change filters. Tables may not add to 100% since studies can include multiple options."),
                       uiOutput("summary_stats_table")
              ),
              ##### Glossary #####
              tabPanel("Glossary",
                       
                       h3("Glossary of Terms"),
                       
                       h4("Publication Year:"),
                       p("Year the study was published."),
                       
                       h4("Title:"),
                       p("Title of the article this study is based on. If a link is available, it will link to the full article."),
                       
                       h4("Corresponding Author:"),
                       p("Author listed in article as corresponding author. If a link is available, it will link to their public website."),
                       
                       h4("Location:"),
                       p("Country (and state if U.S. study) where study was conducted."),
                       
                       #h4("Data Years:"),
                       #p("The years from which data originated."),
                       
                       h4("Community Type:"),
                       p("Rural, Suburban, and/or Urban."),
                       
                       h4("School Type"),
                       p("Type of school (e.g., Charter, Public)."),
                       
                       h4("Grade/School Level:"),
                       p("Grade level of students / Education level of schools (e.g., Elementary, Primary)."),
                       
                       h4("Sample Size:"),
                       p("Number of students, classrooms, and schools included in the study."),
                       
                       # h4("Age:"),
                       # p("Average or median age with the standard or deviation or range, depending on what was reported in the study."),
                       
                       h4("Intervention:"),
                       p("Name(s) of the anxiety prevention intervention studied. If available, name-brand interventions have a clickable link to the program's website and/or clearinghouse page. efinitions for generic intervention names are provided below:"),
                       
                       tags$ul(
                         tags$li(HTML("<strong>Emotion Regulation (ER):</strong> Emotion Regulation program focused on teaching skills related to identifying, understanding, and managing emotions")),
                         tags$li(HTML("<strong>Behavioral Activation (BA):</strong> Behavioral Activation program focused on increasing engagement in positive or rewarding activities")),
                         tags$li(HTML("<strong>School-based anxiety prevention program (SBAPP):</strong> A school-based intervention program designed to prevent the onset of anxiety")),
                         tags$li(HTML("<strong>Treatment Curriculum:</strong> A generic intervention program designed to treat anxiety symptoms")),
                         tags$li(HTML("<strong>Positive Search Training (PST):</strong> Training individuals to preferentially focus attention on positive stimuli, while ignoring negative/threat stimuli, via computer-assisted programs")),
                         tags$li(HTML("<strong>Cognitive Behavioural Intervention (CBI):</strong> Clinician-delivered manualized program, based on the Take Action Program, that involved psycho-education, training in relaxation techniques, using coping statements, graded exposure, and social skills development"))
                       ),
                       
                  
                       h4("Outcome:"),
                       p("Variables researchers reported as outcomes."),
                       
                       tags$ul(
                         tags$li(HTML("<strong>Anxiety Diagnosis:</strong> A formal diagnosis of clinical anxiety based on established criteria by a mental health professional or cutoff on a validated screening tool.")),
                         tags$li(HTML("<strong>Anxiety Symptoms:</strong> Signs of anxiety, such as excessive worry or fear.")),
                         tags$li(HTML("<strong>Depression:</strong> Persistent sadness, loss of interest, fatigue, and changes in appetite or sleep that impair daily functioning (symptoms or diagnosis).")),
                         tags$li(HTML("<strong>Educational Achievement:</strong> The level of success a person has achieved in their academic or educational pursuits.")),
                         tags$li(HTML("<strong>Self-Harm:</strong> Intentional injury inflicted on oneself.")),
                         tags$li(HTML("<strong>Stress:</strong> A physical or emotional response to external pressures or challenges.")),
                         tags$li(HTML("<strong>Substance-Use:</strong> The consumption of drugs or alcohol.")),
                         tags$li(HTML("<strong>Subsyndromal Anxiety:</strong> A set of anxiety symptoms that do not meet the full criteria for a clinical anxiety diagnosis.")),
                         tags$li(HTML("<strong>Suicidal Ideation:</strong> Thoughts or considerations about taking one's own life.")),
                         tags$li(HTML("<strong>Well-being:</strong> A general state of health, happiness, and life satisfaction."))
                       ),
                       
                       h4("% Female:"),
                       p("Percentage of female students included in the study."),
                       
                       h4("% Race/Ethnicity:"),
                       p("Percentage of the student race/ethnicity demographics reported in the study. Note that percentages may not add to 100% and are based on what was reported in the study. Some categories are abbreviated:"),
                       
                       tags$ul(
                         tags$li(HTML("<strong>AIAN:</strong> American Indian and Alaskan Native")),
                         tags$li(HTML("<strong>NHPI:</strong> Native Hawaiian and Pacific Islander"))
                       ),
                       
                       h4("% ELL"),
                       p("Percentage of students classified as early language learners."),
                       
                       h4("% FRPL"),
                       p("Percentage of students qualifiying for free or reduced priced lunch."),
              )
  ),
  fluidRow(
    column(12,  # Makes it span the full width
           div(
             style = "text-align: left; padding: 10px; background-color: #f4f4f4; width: 100%; font-size: 14px; margin-top: 15px;",
             p(
               HTML("<i class='fa fa-calendar'></i> Last search of the literature: <b>", last_search, "</b>"),
               HTML("<br><i class='fa fa-refresh'></i> Updated search expected: <b>", next_search, "</b>")
             )
           )
    ))
)

#### SERVER #### 
server <- function(input, output, session) {
  # Create a reactive filtered dataset based on user selections ####
  filtered_dataset <- reactive({
    filtered_data <- a5
    
    # Filter by country
    if (!is.null(input$country_filter) && length(input$country_filter) > 0) {
      filter_expr_country <- do.call(cbind, lapply(input$country_filter, function(country_filter) {
        grepl(country_filter, filtered_data$study_country, ignore.case = TRUE)
      }))
      filtered_data <- filtered_data %>%
        filter(rowSums(filter_expr_country) > 0)
    }
    
    # Filter by state
    if (!is.null(input$state_filter) && length(input$state_filter) > 0) {
      filter_expr_state <- do.call(cbind, lapply(input$state_filter, function(state_filter) {
        grepl(state_filter, filtered_data$study_state, ignore.case = TRUE)
      }))
      filtered_data <- filtered_data %>%
        filter(rowSums(filter_expr_state) > 0)
    }
    
    # Filter by urbanicity
    if (!is.null(input$urbanicity_filter) && length(input$urbanicity_filter) > 0) {
      filter_expr_urbanicity <- do.call(cbind, lapply(input$urbanicity_filter, function(urbanicity_filter) {
        grepl(urbanicity_filter, filtered_data$study_school_area, ignore.case = TRUE)
      }))
      filtered_data <- filtered_data %>%
        filter(rowSums(filter_expr_urbanicity) > 0)
    }
    
    # Filter by school type
    if (!is.null(input$school_type_filter) && length(input$school_type_filter) > 0) {
      filter_expr_school_type <- do.call(cbind, lapply(input$school_type_filter, function(school_type_filter) {
        grepl(school_type_filter, filtered_data$study_school_type, ignore.case = TRUE)
      }))
      filtered_data <- filtered_data %>%
        filter(rowSums(filter_expr_school_type) > 0)
    }
    
    # Filter by grade level
    if (!is.null(input$grade_level_filter) && length(input$grade_level_filter) > 0) {
      filter_expr_grade_level <- do.call(cbind, lapply(input$grade_level_filter, function(grade_level_filter) {
        grepl(grade_level_filter, filtered_data$study_grade_level, ignore.case = TRUE)
      }))
      filtered_data <- filtered_data %>%
        filter(rowSums(filter_expr_grade_level) > 0)
    }
    
    # Filter by school level
    if (!is.null(input$school_level_filter) && length(input$school_level_filter) > 0) {
      filter_expr_school_level <- do.call(cbind, lapply(input$school_level_filter, function(school_level_filter) {
        grepl(school_level_filter, filtered_data$study_school_level, ignore.case = TRUE)
      }))
      filtered_data <- filtered_data %>%
        filter(rowSums(filter_expr_school_level) > 0)
    }
    
    # Filter by outcome
    if (!is.null(input$outcome_filter) && length(input$outcome_filter) > 0) {
      filter_expr_outcome <- do.call(cbind, lapply(input$outcome_filter, function(outcome_filter) {
        grepl(outcome_filter, filtered_data$outcome_list, ignore.case = TRUE)
      }))
      filtered_data <- filtered_data %>%
        filter(rowSums(filter_expr_outcome) > 0)
    }
    
    
    # Filtering by intervention
    # Filtering by intervention
    # Filtering by intervention
    if (!is.null(input$intervention_filter) && length(input$intervention_filter) > 0) {
      # Preprocess intervention list to ensure consistent formatting
      filtered_data <- filtered_data %>%
        mutate(intervention_list_clean = tolower(trimws(Intervention))) # Convert to lowercase and trim whitespace
      
      # Convert intervention filter input to lowercase for consistent matching
      intervention_filter_clean <- tolower(input$intervention_filter)
      
      # Define custom expansions
      custom_matches <- list(
        "cognitive behavior" = c("cognitive behavior", "cognitive behavioural")
      )
      
      # Expand any selected interventions if needed
      expanded_filters <- unlist(lapply(intervention_filter_clean, function(filter_term) {
        if (filter_term %in% names(custom_matches)) {
          custom_matches[[filter_term]] # Include custom matches
        } else {
          filter_term # Keep the original filter term
        }
      }))
      
      # Flatten intervention_choices and include custom expansions
      flat_intervention_choices <- unlist(intervention_choices)
      flat_intervention_choices_lower <- tolower(flat_intervention_choices)
      known_interventions <- unique(c(flat_intervention_choices_lower, unlist(custom_matches)))
      
      # Handle "Other Prevention Practice" case
      if ("other prevention practice" %in% intervention_filter_clean) {
        # Identify rows that do NOT match any known intervention
        if (nrow(filtered_data) > 0) {
          # Check for rows that do NOT match any other intervention
          non_match_filter <- rowSums(sapply(known_interventions, function(choice) {
            grepl(choice, filtered_data$intervention_list_clean, fixed = TRUE)
          })) == 0
        } else {
          non_match_filter <- logical(0) # Return an empty logical vector if no rows exist
        }
        
        # Apply the "Other Prevention Practice" filter
        filtered_data <- filtered_data[non_match_filter, ]
      } else {
        # Match any substring of the expanded filter terms
        if (nrow(filtered_data) > 0) {
          match_filter <- do.call(cbind, lapply(expanded_filters, function(filter_term) {
            grepl(filter_term, filtered_data$intervention_list_clean, fixed = TRUE)
          }))
          
          # Exclude "Aussie Optimism Program: Feeling and Friends" from FRIENDS
          if ("friends" %in% intervention_filter_clean) {
            match_filter <- match_filter & !grepl("aussie optimism program: feeling and friends", filtered_data$intervention_list_clean, fixed = TRUE)
          }
          
          combined_filter <- rowSums(match_filter) > 0
          filtered_data <- filtered_data[combined_filter, ]
        }
      }
      
      # Remove the temporary column
      filtered_data <- filtered_data %>%
        select(-intervention_list_clean)
    }
    
    
    
    
    
    return(filtered_data)
  })
  
  # Render the filtered dataset as a table ####
  output$table <- DT::renderDT({
    filtered_data <- filtered_dataset() %>% 
      dplyr::select(-study_author_year, -study_state, -study_country, -study_grade_level, -study_school_level, -title, -study_cor_author, 
                    -link_text, -link_author, -intervention_name, -website_links, -clearinghouse_links)
    
    # Check if filtered_data has rows
    if (nrow(filtered_data) > 0) {
      # Render the datatable with options
      datatable(
        filtered_data, 
        escape = FALSE,   # No escaping needed if HTML formatting is used
        rownames = FALSE,
        colnames = c("Publication Year", "Title", "Corresponding Author", "Country (State)", 
                     "Community Type", "School Type", "Grade/School Level",
                     "Sample Size", "% Race/Ethnicity", "% ELL", "% FRPL", "% Female", 
                     "Intervention", "Outcome"),
        options = list(
          dom = 'lBfrtipC',
          columnDefs = list(list(width = "1000px", targets = which(colnames(filtered_data) == "Title"))),
          pageLength = 10
        ),
        ,
        caption = htmltools::tags$caption(
          style = 'caption-side: bottom; text-align: left; font-size: 12px; color: #555;',
          "AIAN = American Indian and Alaskan Native; NHPI = Native Hawaiian and Pacific Islander"
        )
      )
    } else {
      # Handle the case when filtered_data is empty
      empty_data <- as.data.frame(matrix(NA, nrow = 0, ncol = ncol(filtered_data)))
      colnames(empty_data) <- colnames(filtered_data)  # Maintain column names consistency
      datatable(
        empty_data, 
        options = list(
          language = list(
            emptyTable = "No data matches your filters."
          )
        )
      )
    }
  })
  
 
  # Reset filters when reset button is selected ####
  observeEvent(input$resetFilters, {
    updateSelectizeInput(session, "school_type_filter", selected = character(0))
    updateSelectizeInput(session, "grade_level_filter", selected = character(0))
    updateSelectizeInput(session, "country_filter", selected = character(0))
    updateSelectizeInput(session, "state_filter", selected = character(0))
    updateSelectizeInput(session, "urbanicity_filter", selected = character(0))
    updateSelectizeInput(session, "outcome_filter", selected = character(0))
    updateSelectizeInput(session, "school_level_filter", selected = character(0))
    updateSelectizeInput(session, "intervention_filter", selected = character(0))
    })

    
  # Render the summary statistics table in the "Summary Statistics" tab ####
  output$summary_stats_table <- renderUI({
    #filtered data to use filters
    filtered_data <- filtered_dataset()


    if (nrow(filtered_data) > 0) {
      
      grade_text_table <- filtered_data %>%
        mutate(study_grade_level = str_remove_all(study_grade_level, "; Not reported|Not reported; ")) %>%
        separate_rows(study_grade_level, sep = "; ") %>%
        mutate(study_grade_level = case_when(
          study_grade_level == "K"  ~ "Kindergarten",
          study_grade_level == "1"  ~ "1st Grade",
          study_grade_level == "2"  ~ "2nd Grade",
          study_grade_level == "3"  ~ "3rd Grade",
          study_grade_level == "4"  ~ "4th Grade",
          study_grade_level == "5"  ~ "5th Grade",
          study_grade_level == "6"  ~ "6th Grade",
          study_grade_level == "7"  ~ "7th Grade",
          study_grade_level == "8"  ~ "8th Grade",
          study_grade_level == "9"  ~ "9th Grade",
          study_grade_level == "10" ~ "10th Grade",
          study_grade_level == "11" ~ "11th Grade",
          study_grade_level == "12" ~ "12th Grade",
          TRUE ~ study_grade_level
        )) %>%
        distinct(row_id = row_number(), study_grade_level) %>% # Ensure unique grade levels per study
        count(study_grade_level) %>%
        mutate(
          percent = paste0(round(n / nrow(filtered_data) * 100, 2), "%") # Percentages based on number of studies
        ) %>%
        arrange(desc(study_grade_level != "Not reported"), desc(n)) %>%
        setNames(c("Grade Level", "Count", "Percent"))
      grade_text_table_render <- renderTable(grade_text_table)
      
      # Generate the Intervention Summary Table
      # Flatten intervention_choices into a single vector
      flat_intervention_choices <- unlist(intervention_choices) # Flatten the list into a vector
      flat_intervention_choices_lower <- tolower(flat_intervention_choices) # Convert to lowercase for matching
      
      # Generate the Intervention Summary Table
      # Generate the Intervention Summary Table
      intervention_summary_table <- filtered_data %>%
        mutate(intervention_list_clean = tolower(trimws(Intervention))) %>% # Preprocess intervention names
        rowwise() %>% # Process each row individually
        mutate(
          # Extract all matching interventions, excluding specific cases
          intervention_grouped = list(unique(c(
            # Match FRIENDS but exclude specific "Aussie Optimism Program"
            if (any(str_detect(intervention_list_clean, "friends")) & 
                !str_detect(intervention_list_clean, "aussie optimism program: feeling and friends")) {
              "FRIENDS"
            } else {
              NULL
            },
            # Match Cognitive Behavior and Cognitive Behavioural
            if (any(str_detect(intervention_list_clean, "cognitive behavior|cognitive behavioural"))) {
              "Cognitive Behavior"
            } else {
              NULL
            },
            # Match using preprocessed flat_intervention_choices
            flat_intervention_choices[sapply(flat_intervention_choices_lower, function(choice) {
              str_detect(intervention_list_clean, choice)
            })],
            # Add "Other Prevention Practice" if no matches
            if (all(!sapply(flat_intervention_choices_lower, function(choice) {
              str_detect(intervention_list_clean, choice)
            })) & 
            !(any(str_detect(intervention_list_clean, "friends") &
                  !str_detect(intervention_list_clean, "aussie optimism program: feeling and friends"))) &
            !any(str_detect(intervention_list_clean, "cognitive behavior|cognitive behavioural"))) {
              "Other Prevention Practice"
            } else {
              NULL
            }
          )))
        ) %>%
        ungroup() %>% # Remove rowwise grouping
        unnest(intervention_grouped) %>% # Expand interventions into separate rows
        filter(!(intervention_grouped == "FRIENDS" & 
                   str_detect(intervention_list_clean, "aussie optimism program: feeling and friends"))) %>% # Exclude Aussie Optimism from FRIENDS
        distinct(row_id = row_number(), intervention_grouped) %>% # Ensure unique interventions per study
        count(intervention_grouped) %>% # Count unique interventions
        mutate(
          percent = paste0(round(n / nrow(filtered_data) * 100, 2), "%") # Percentages based on number of studies
        ) %>%
        arrange(desc(intervention_grouped != "Other Prevention Practice"), desc(n)) %>% # Order the table
        setNames(c("Intervention", "Count", "Percent"))
      
      
      # Render the Intervention Summary Table
      intervention_summary_table_render <- renderTable(intervention_summary_table)


      # function to create summary tables
      process_summary_tables <- function(var_name, data) {
        result <- data %>%
          mutate_at(vars(all_of(var_name)), list(~str_remove_all(.,"; -999|-999; "))) %>%
          mutate(across(all_of(var_name), ~str_replace_all(., ", | and ", "; "))) %>%
          separate_rows(all_of(var_name), sep = "; ") %>%
          mutate(across(all_of(var_name), ~str_trim(.))) %>%
          count(!!sym(var_name)) %>%
          mutate(
            !!var_name := ifelse(!!sym(var_name) == "-999", "Not reported", !!sym(var_name)),
            percent = paste0(round(n / nrow(data) * 100, 2), "%")
          ) %>%
          arrange(desc(!!sym(var_name) != "Not reported"), desc(n))

        return(result)
      }


      # List of variables
      vars_list <- c("study_publication_year", "study_school_area", "study_school_type", "study_school_level",
                     "outcome_list")

      # Use lapply to process data for each variable
      tables_list <- lapply(vars_list, process_summary_tables, data = filtered_data)
      
      # Set custom headers for each table
      tables_list[[1]] <- setNames(tables_list[[1]], c("Publication Year", "Count", "Percent"))
      tables_list[[2]] <- setNames(tables_list[[2]], c("Community Type", "Count", "Percent"))
      tables_list[[3]] <- setNames(tables_list[[3]], c("School Type", "Count", "Percent"))
      tables_list[[4]] <- setNames(tables_list[[4]], c("School Level", "Count", "Percent"))
      tables_list[[5]] <- setNames(tables_list[[5]], c("Outcome", "Count", "Percent"))

      #render tables with concise formatting
      rendered_tables_list <- lapply(tables_list, function(tbl) {
        renderTable(tbl)
      })
      
      #show tables in two columns (with css code)
      div(
        class = "table-container",
        div(class = "table",
            h3("Community Type Table"),
            rendered_tables_list[[2]]
        ),
        div(class = "table",
            h3("School Type Table"),
            rendered_tables_list[[3]]
        ),
        div(class = "table",
            h3("School Level Table"),
            rendered_tables_list[[4]]
        ),
        div(class = "table",
            h3("Grade Level Table"),
            grade_text_table_render
        ),
        
        div(class = "table",
            h3("Outcome Table"),
            rendered_tables_list[[5]]
        ),
        div(class = "table",
            h3("Publication Year Table"),
            rendered_tables_list[[1]]
        ),
        div(class = "table",
            h3("Intervention Table"),
            intervention_summary_table_render
        )
       )

    } else {
      # Display a message when there are no matching results
      div(
        p("No data matches your filters."),
        style = "text-align: center; font-size: 16px; margin-top: 10px; font-weight: bold;"
      )
    }



  })

  # Download buttons ####

  output$downloadData <- downloadHandler(
    filename = "anxiety_prevention_data.xlsx",
    content = function(file) {
      cleaned_df_to_export <- app_df %>% 
        dplyr::select(-study_author_year, -study_state, -study_country, -study_grade_level, -study_school_level)
      
      write.xlsx(cleaned_df_to_export, file)  # all data
    }
  )

  output$downloadFilteredData <- downloadHandler(
    filename = "anxiety_filtered_data.xlsx",
    content = function(file) {
      filtered_data <- filtered_dataset()

      filtered_data_export <- filtered_data %>%
        dplyr::select(-study_author_year, -study_state, -study_country, -study_grade_level, -study_school_level, 
                      -linked_title, linked_author, -website_links, -clearinghouse_links) %>% 
        relocate(link_text, link_author, .after = last_col())

      write.xlsx(filtered_data_export, file)  # Write the filtered data
    }
  )
    
  
}



#### Run ####
shinyApp(ui, server)


#### Deploy ####
#file needs to be .R; files need to be in data_dashboard folder; account needs to be setup
# rsconnect::deployApp(appDir = "outputs/data_dashboard", appName = "anxiety_data_dashboard")

