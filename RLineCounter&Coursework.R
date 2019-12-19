#R Line Counter:  Fri Dec 13 16:36:31 2019 ----

paths <- c("~/Veginvesting","~/Northeastern","~/Graduate Studies Resumes,Transcripts & Applications")
paths <- c(list.dirs("~/R", recursive = F) %>%
             purrr::keep(~ {!str_detect(.x, "rsconnect|win-library|Contributor Repos")}), paths)
# Get all directories under R except for win-library, Scripts and rsconnect

all.rmd <- sapply(paths, list.files, pattern = "[A-Za-z0-9\\_\\.\\s\\-]+\\.[Rr][^d|p]?m?[^x|s|r]?d?$", all.files = T, full.names = T, recursive = T, ignore.case = T, include.dirs = T)
all.rmd <- lapply(all.rmd, function(l){
  l <- l[!grepl("win-library", l)]
  l <- l[!grepl("min.js", l)]
  l <- l[!grepl("pdf", l, ignore.case = T)]
  l <- l[!grepl("png", l, ignore.case = T)]
  l <- l[!grepl("zip", l, ignore.case = T)]
  l <- l[!grepl("rnw", l, ignore.case = T)]
  l <- l[!grepl("rhistory", l, ignore.case = T)]
  l <- l[!grepl("rsconnect", l, ignore.case = T)]
  l <- l[!grepl("rtf", l, ignore.case = T)]
  l <- l[!grepl("rxproj", l, ignore.case = T)]
  l <- l[!grepl("R Code", l, ignore.case = T)]})
all.rmd <- all.rmd[sapply(all.rmd,length) > 0]
all.rmd <- lapply(all.rmd,gsub,pattern="\\/",replacement="\\\\\\")
rmd.attr <- purrr::map_depth(all.rmd, .depth = 2,.f = function(.x){
  # Try to read the RMD
  .file_text <- try({readLines(.x)})
  # If the file doesnt exist return nothing
  if(!HDA::go(".file_text")) return(NULL)
  if (stringr::str_detect(.x, "[Dd]$")){
    .Chunks <- try({.Chunks <- data.frame(begin = stringr::str_which(.file_text, "^```\\{r"), end = stringr::str_which(.file_text, "^```\\s{0,2}$"))})
    if (is.null(.Chunks) | class(.Chunks) == "try-error") {
      .Chunks <- list()
      .Chunks$begin <- stringr::str_which(.file_text, "^```\\{r") %>% print
      .Chunks$end <- stringr::str_which(.file_text, "^```\\s{0,2}$") %>% print
      .Chunks$fn <- .x
      return(.Chunks)
    }
    
    
    #Remove rows from the chunks table indicating the line number of the following repetitive chunk types
    .Chunks <- .Chunks[!.Chunks$begin %in% c(stringr::str_which(.file_text, "```\\{r\\ssetup"), # setup chunk
                                             stringr::str_which(.file_text, "```\\{r\\s\\'As"), # Assignment 
                                             stringr::str_which(.file_text, "```\\{r\\sLib"), #Libraries
                                             stringr::str_which(.file_text, "```\\{r\\s\\'R\\sSel"), # Selenium
                                             stringr::str_which(.file_text, "```\\{r\\s\\'R\\sChunk"), # name Chunks
                                             stringr::str_which(.file_text, "```\\{r\\s\\'R\\sAttach")), ] # Dependencies
    
    .total_lines <- try({.total_lines <- mutate(.Chunks, lines = end - begin) %>% .[["lines"]] %>% sum()})
    if (class(.total_lines) == "try-error"){ print(paste0("File that failed: ", .x))
      print(.Chunks)
    }
    .fns <- purrr::pmap(list(.b = .Chunks$begin, .e = .Chunks$end), .ft = .file_text, function(.b, .e, .ft){
      .ch <- .file_text[.b:.e]
      .fns <- stringr::str_extract_all(.ch, "[A-Za-z\\_\\`][A-Za-z0-9\\.\\_\\:]+(?=\\()") %>%  unlist %>% trimws
    }) %>% unlist %>% unique
  } else {
    .fns <- stringr::str_extract_all(.file_text, "[A-Za-z\\_\\`][A-Za-z0-9\\.\\_\\:]+(?=\\()") %>% unlist %>% trimws %>% unique
    .total_lines <- length(.file_text[purrr::map_lgl(.file_text, ~{nchar(.x) > 0})])
  }
  .pkgs <- stringr::str_extract(.fns, "[[:alnum:]\\_]+(?=\\:\\:)") %>% purrr::keep(~ !is.na(.))
  .fns %<>% purrr::map_chr(function(.x) {
    if(stringr::str_detect(.x, "\\:{2}")) 
      .out <- stringr::str_extract(.x, "(?<=\\:\\:)[A-Za-z0-9\\_\\.]+") 
    else if (stringr::str_detect(.x, "\\:$"))
      .out <- "" 
    else
      .out <- .x
    return(.out)
  }) %>% purrr::keep(~ nchar(.x) > 0)
  .lpkgs <- stringr::str_extract(.file_text, "^(?<=library\\()[:alnum:]+") %>% purrr::keep(~ !is.na(.))
  .spkgs <- stringr::str_extract(.file_text, "(?<=startPkgs\\(c?\\(?)[^\\)]+") %>% purrr::keep(~ !is.na(.)) %>% stringr::str_extract_all("(?<=\\\")[:alnum:]+") %>% unlist
  
  out <- list(Lines = .total_lines, Fns = .fns, Pkgs = c(.pkgs, .lpkgs, .spkgs))
  return(out)
})
# Function to extract functions
# purrr::map(rmd.attr, ~ purrr::keep(.x = .x, ~ length(.) > 2))


total.lines <- purrr::map_depth(rmd.attr, .depth = 2, .f = ~ purrr::pluck(.,"Lines")) %>% unlist %>% sum

all.fns <- purrr::map_depth(rmd.attr, .depth = 2, .f = ~ purrr::pluck(.,"Fns")) %>% unlist
all.pkgs <- purrr::map_depth(rmd.attr, .depth = 2, .f = ~ purrr::pluck(.,"Pkgs")) %>% unlist
.packages <- table(all.pkgs) %>% as.data.frame %>% setNames(c("Package","# of Uses")) %>% dplyr::arrange(desc(`# of Uses`))
.functions <- table(all.fns) %>% as.data.frame %>% setNames(c("Function","# of Uses")) %>% dplyr::arrange(desc(`# of Uses`))

#Load Plus Data:  Fri Dec 13 16:36:17 2019 ----

PlusforTrello <- read_csv(file = "~//Northeastern//MSHDAPlusforTrello.csv")
names(PlusforTrello) %<>% gsub("\\*","", . , perl = T) # Remove the wierd symbols in the names
PlusforTrello$Board %<>% gsub("([A-Z]{2,4})\\s?(\\d{4})","\\1\\2",.) #Format Courses in a Standard Format
PlusforTrello$Board <- purrr::map2_chr(.x = PlusforTrello$Board, .y = PlusforTrello$Date, function(.x, .y){
  if(.y < lubridate::ymd("2017-12-31") & .x == "PPUA5302"){
    .out  <- "PPUA5301"
  } else if (.y < lubridate::ymd("2017-12-31") & .x == "DA5030") {
    .out  <- "DA5020"
  } else if (.y < lubridate::ymd("2018-12-31") & .x == "HINF5512")
    .out  <- "HINF5102"
  else if (str_detect(.x, regex("Project|Incentivized", ignore_case = T)))
    .out <- paste0(str_extract(.x, "([A-Z]{2,4})\\s?(\\d{4})"), " Project")
  else if (nchar(.x) > 8)
    .out <- str_trunc(.x, 11)
  else
    .out <- .x
  return(.out) 
}) %>% as.factor() # Change board names from prior semester back to the actual class call number
#Render Plots:  Thu Dec 12 09:45:16 2019 ----
Coursework <- list(Fall2017 = list(PPUA5301 = list(fullname = "Introduction to Computational Statistics",
                                                   description = "Introduces the fundamental techniques of quantitative data analysis, ranging from foundational skills; such as data description and visualization, probability, and statistics - to the workhorse of data analysis and regression, to more advanced topics; such as machine learning and networks. Emphasizes real-world data and applications using the R statistical computing language. Analyzing and understanding complex data has become an essential component of numerous fields: business and economics, health and medicine, marketing, public policy, computer science, engineering, and many more. Offers students an opportunity to finish the course ready to apply a wide variety of analytic methods to data problems, present their results to nonexperts, and progress to more advanced course work delving into the many topics introduced here.", syllabus = "https://drive.google.com/file/d/1FrAFxRvQcbkcqyPerx9I41EswsBgR_7A/view?usp=sharing",
                                                   projects = c(paste0("http://rpubs.com/yogat3ch/ppua5301hwk", 1:7), "http://rpubs.com/yogat3ch/ppua5301midterm", "http://rpubs.com/yogat3ch/ppua5301hwk8_9", paste0("http://rpubs.com/yogat3ch/ppua5301hwk", 10:12))),
                                   DA5020 = list(fullname = "Collecting, Storing, and Retrieving Data",
                                                 description = "Studies how to build large-scale information repositories of different types of information objects so that they can be selected, retrieved, and transformed for analytics and discovery, including statistical analysis. Analyzes how traditional approaches to data storage can be applied alongside modern approaches that use nonrelational data structures. Through case studies, readings on background theory, and hands-on experimentation, offers students an opportunity to learn how to select, plan, and implement storage, search, and retrieval components of large-scale structured and unstructured information repositories. Emphasizes how to assess and recommend efficient and effective large-scale information storage and retrieval components that provide data scientists with properly structured, accurate, and reliable access to information needed for investigation.", syllabus = "https://drive.google.com/file/d/1dTiQV1NOuHrZJ_Mu7g4UXhl9H1kWIJk0/view?usp=sharing", projects = "http://rpubs.com/yogat3ch/DA5020_Holsenbeck_S_Final"),
                                   dates = lubridate::interval(lubridate::ymd("2017-09-01", tz = "US/Eastern"), lubridate::ymd("2017-12-31"))),
                   Spring2018 = list(PPUA5302 = list(fullname = "Information Design and Visual Analytics",
                                                     description = "Introduces the systematic use of visualization techniques for supporting the discovery of new information as well as the effective presentation of known facts. Based on principles from art, graphic design, perceptual psychology, and rhetoric, offers students an opportunity to learn how to successfully choose appropriate visual languages for representing various kinds of data to support insights relevant to the user's goals. Covers visual data mining techniques and algorithms for supporting the knowledge-discovery process; principles of visual perception and color theory for revealing patterns in data, semiotics, and the epistemology of visual representation; narrative strategies for communicating and presenting information and evidence; and the critical evaluation and critique of data visualizations. Requires proficiency in R.", syllabus = "https://drive.google.com/file/d/1mYnZLr2sQWiqqlhvVS_gZMUhCfFZ_T_j/view?usp=sharing", projects = list("http://rpubs.com/yogat3ch/ppua5302p1", "http://rpubs.com/yogat3ch/ppua5302p2", `PPUA5302 Final: Finance for Freelancers Visualization` = "https://drive.google.com/file/d/1jYbH_wGEPuVeyc07HGB3TQOOM3ogMhb9/view")),
                                     DA5030 = list(fullname = "Introduction to Data Mining/Machine Learning",
                                                     description = "Introduces the fundamental techniques for data mining, combining elements from CS 6140 and CS 6220. Discusses several basic learning algorithms, such as regression and decision trees, along with popular data types, implementation and execution, and analysis of results. Lays the data analytics program foundation of how learning models from data work, both algorithmically and practically. The coding can be done in R, Matlab or Python. Students must demonstrate ability to set up data for learning, training, testing, and evaluating.", syllabus = "https://drive.google.com/file/d/1tAKs7uRSLyZ2fPgiiwSG6M7c6t3VHmrC/view?usp=sharing", projects = "http://rpubs.com/yogat3ch/da5030ppua5302final"),
                                     dates = lubridate::interval(lubridate::ymd("2018-01-01", tz = "US/Eastern"), lubridate::ymd("2018-06-01"))),
                   Fall2018 = list(HINF6400 = list(fullname = "Intro to Health Data Analytics",
                                                   description = "Introduces the field of health data analytics. Topics include understanding stakeholder needs; the variety of types of health data; software tools; as well as case studies from pharma, public health, electronic health records, claims data, and home-monitoring data. Emphasizes the importance of understanding the complexity and potential biases in how health data (direct or indirect) is collected and represented. Presents all data-analytic discussions within a context of health data and stakeholder information needs. Offers students an opportunity to practice presenting the results of analyses.", syllabus = 'https://drive.google.com/file/d/1WyIPwxLc3Rm1jHXR_0sfvrkzZHuKwsV5/view?usp=sharing', projects = c("Incentivized Insurance" = "https://docs.google.com/presentation/d/1WAr7Xvpe9icGm6vJ8ns0VPSsnV9BXHtqLzRuB7n4dVA/edit?usp=sharing")),
                                   HINF5102 = list(fullname = "Data Management in Healthcare",
                                                   description = "Explores issues of data representation in healthcare systems, including patient and provider identification, audit trails, authentication, and reconciliation. Discusses underlying design of repositories for electronic health records (EHRs) and computerized provider order entry (CPOE) systems. Includes an overview of privacy issues, legislation, regulations, and accreditation standards unique to healthcare.", syllabus = "https://drive.google.com/file/d/1kMm4aJyO-u98RWTd-rX15fJYu79d--_Y/view?usp=sharing", projects = c(`Diabetes Management Program` = "https://docs.google.com/document/d/1GNmFbcO8qNla3FCn_Hu4JcKewVfBBOeJwNcA6aGPJ9s/edit")),
                                   CAEP7712 = list(fullname = "Intermediate Statistical Data Analysis Techniques",
                                                   description = "Emphasizes the use of existing theories and models as a basis for the formation of questions and hypotheses and for designing research to address those questions and hypotheses. Covers the logic of design of research and hypothesis testing, regression, general linear model (GLM), statistical model building and testing, hierarchical regression, and analysis of covariance structures. Emphasizes consideration of power and effects. Requires students to do problems on the computer and/or by hand using data sets assigned in class. Requires prior completion of a course in basic statistics and a course in methods of research design or permission of instructor.", syllabus = "https://drive.google.com/file/d/1-E21boFAslUIAWY6lrP5fJGcX8rIa5_H/view?usp=sharing", projects = c(`See Capstone` = "")),
                                   dates = lubridate::interval(lubridate::ymd("2018-09-01", tz = "US/Eastern"), lubridate::ymd("2018-12-31"))),
                   Spring2019 = list(Capstone = list(fullname = "Independent Capstone Study",
                                                     description = "Introduction: The experience of the academic environment can be competitive, stressful, and at times overwhelming for students and faculty alike. Self-compassion, deﬁned by Kristen Neff PhD as including self- kindness, common humanity, and mindfulness, has been associated with general resourcefulness, self-regulation, and well-being in university students, but mindfulness practices evoking self-compassion remain underutilized in academic contexts. Methods: This meta-analysis evaluated ﬁve studies and seeks to establish associations between the characteristics of mindfulness-based interventions in educational contexts, namely contact time, individual time (outside of the group context), type of intervention (Mindfulness-based stress reduction or Self-compassion courses) and their inﬂuence on the between-group (treatment-control) and within-group (pre-post) standardized mean differences (SMD) of scores on the Self-compassion Scale. Results: The SMDs across four studies where treatment & control groups were evaluated, ranged from 0.18-1.17 while SMDs across three studies where pre/post treatment scores were evaluated ranged from 0.39-1.07. In the control/treatment design, Intervention Type (p=0.08), Contact Time (p=0.08) and Duration (p=0.03) were all signiﬁcant predictors of self-compassion. In the Pre/Post treatment design (suited for ad-hoc semester-long class cohort studies), Intervention Type (p=0.001) and Contact Time (p=0) were signiﬁcant predictors while Duration (p=0.161) was not. In both study designs; Intervention type, speciﬁcally interventions modeled after the Self- Compassion Course, was the most inﬂuential factor effecting self- compassion outcomes (C/T: 0 = 0.68 P/P: 0 = 0.43) on self-compassion outcome measures. Conclusion: Two to eight-week mindfulness courses embedded within coursework can have a signiﬁcant effect on self- compassion scores for student cohorts."),
                                     HINF5105 = list(fullname = "The American Healthcare System",
                                                     description = "Covers the organization, financing, and outcomes of the U.S. healthcare system. Studies opportunities and challenges to improve the cost and quality of healthcare and expand adequate coverage to all. Non-health informatics students may be able to take the course with permission of the program director.", syllabus = "https://docs.google.com/document/d/11E93Vql82bTaq8ZWbdqgCqtX7QHcbVnEfkuTKr4CPG0/edit?usp=sharing"),
                                     HINF6500 = list(fullname = "Predictive Analytics and Modeling",
                                                     description = "Seeks to train students to transform data to useful, actionable knowledge through the use of mathematical and computational models. Reviews popular techniques for data mining and health analytics based on regression and machine-learning methodologies. Introduces students to a spectrum of models—ranging from data-driven to principle-based, mechanistic approaches—and examines how to compare models and use them to improve understanding of data. Introduces model-based methods of artificial intelligence as applied to healthcare problems, covering fundamental principles of AI and a variety of applications in healthcare.", syllabus = "https://drive.google.com/file/d/1r_7YWSWgBOBxDn_jPQjbYyDXVOh0frtX/view?usp=sharing", project = "http://rpubs.com/yogat3ch/hinf6500project"),
                                     dates = lubridate::interval(lubridate::ymd("2019-01-01", tz = "US/Eastern"), lubridate::ymd("2019-06-01"))),
                   Fall2019 = list(Capstone = list(fullname = "A Meta-Analysis of the Effect of Mindfulness Training in Education on Self-Compassion",
                                                   description = "Introduction: The experience of the academic environment can be competitive, stressful, and at times overwhelming for students and faculty alike. Self-compassion, deﬁned by Kristen Neff PhD as including self- kindness, common humanity, and mindfulness, has been associated with general resourcefulness, self-regulation, and well-being in university students, but mindfulness practices evoking self-compassion remain underutilized in academic contexts. Methods: This meta-analysis evaluated ﬁve studies and seeks to establish associations between the characteristics of mindfulness-based interventions in educational contexts, namely contact time, individual time (outside of the group context), type of intervention (Mindfulness-based stress reduction or Self-compassion courses) and their inﬂuence on the between-group (treatment-control) and within-group (pre-post) standardized mean differences (SMD) of scores on the Self-compassion Scale. Results: The SMDs across four studies where treatment & control groups were evaluated, ranged from 0.18-1.17 while SMDs across three studies where pre/post treatment scores were evaluated ranged from 0.39-1.07. In the control/treatment design, Intervention Type (p=0.08), Contact Time (p=0.08) and Duration (p=0.03) were all signiﬁcant predictors of self-compassion. In the Pre/Post treatment design (suited for ad-hoc semester-long class cohort studies), Intervention Type (p=0.001) and Contact Time (p=0) were signiﬁcant predictors while Duration (p=0.161) was not. In both study designs; Intervention type, speciﬁcally interventions modeled after the Self- Compassion Course, was the most inﬂuential factor effecting self- compassion outcomes (C/T: 0 = 0.68 P/P: 0 = 0.43) on self-compassion outcome measures. Conclusion: Two to eight-week mindfulness courses embedded within coursework can have a signiﬁcant effect on self- compassion scores for student cohorts.", projects = c(`Presentation` = "https://docs.google.com/presentation/d/1G83k89jex6tOH2txDZK8hdIJNtKcpJWBPffCKwM-2Os/edit?usp=sharing", `Poster` = "https://drive.google.com/file/d/1_Oatf1pF7W1SSwsmdqBnNCLRoBwLpRiG/view?usp=sharing")),
                                   HINF5300 = list(fullname = "Personal Health Interface Design and Development",
                                                   description = "Explores the design of innovative personal health human-computer interface technologies. Examples include assistive technologies that aid persons with disabilities, consumer wellness promotion applications, patient education and counseling systems, interfaces for reviewing personal health records, and elder care and social network systems that monitor health and support independent living. Offers students an opportunity to work in teams to build a prototype personal health interface system to solve a real problem. Topics include needs assessment and participatory research, iterative user interface design methods for health interface development, computational sensing of health states and behavior, software architectures for iteratively testing prototype personal health interface technologies, human-computer interaction issues related to personal health technology, and technology transfer requirements to support future validation studies of technology.", syllabus = "https://docs.google.com/document/d/1zkYDz0pvWurey_ia4DKO5ycpV_DVJKBNjksBCpGKGDc/edit?usp=sharing", projects = c(`Be Efficient - A tool for improving physical activity and parent-child interactions` = "https://bitbucket.org/hinf5300team/be-efficient/wiki/Home")),
                                   dates = lubridate::interval(lubridate::ymd("2019-09-01", tz = "US/Eastern"), lubridate::ymd("2019-12-31")))
)
#'Write Output Summaries and Graphs for all courses':  Fri Dec 13 16:37:18 2019 ----

Coursework <- purrr::imap(Coursework, .dat = PlusforTrello, function(.x, .y, .dat){
  .sem <- .x %>% magrittr::extract(value = - which(names(.) == "dates"))
  # print(paste0(names(.sem), collape = ","))
  # print(class(.x$dates))
  .dat <- .dat %>% dplyr::filter(str_detect(Board, paste0(names(.x), collapse = "|")) & Date %within% .x$dates) # Filter the Courses for that semester
  .thours <- .dat %>% group_by(Week) %>% summarise(TotalHrs = sum(S)) %>% extract2("TotalHrs")
  .avg <- .thours %>% mean(na.rm = T)
  .total <- .thours %>% sum(na.rm = T)
  .gg <- {.dat %>% group_by(Board, Week) %>% summarise(TotalHrsPerWeek=sum(S)) -> .summary} %>% 
    ggplot(data = ., aes(x = Week, y = TotalHrsPerWeek))+
    geom_col(aes(fill = Board)) +
    scale_y_continuous(breaks = function(lims, .b = 5){seq(0, {.b * lims[2] %/% .b}, .b)}, minor_breaks = function(lims, .b = 1){seq(0, {.b * lims[2] %/% .b}, .b)}) +
    geom_hline(yintercept = .avg) + 
    geom_text(aes(x = 0, y = .avg, label = "Average"), vjust = -1, nudge_x = 1.2) +
    coord_flip() +
    theme_minimal() +
    labs(title = glue::glue("{.y} Hours By Week"),
         subtitle = glue::glue("Total hours: {.total}"),
         x = "Week",y = "Hours") +
    theme(plot.title = element_text(hjust = .5), plot.subtitle = element_text(hjust = .5), plot.caption = element_text(hjust=0), axis.text.x = element_text(angle = 45))
  .sem$sem_graph <- .gg
  .summary
  .sem <- purrr::imap(.sem, .dat = .dat, function(.x, .y, .dat){
    .d <- .dat[stringr::str_detect(.dat$Board, .y),]
    .avg <- .d %>% group_by(Week) %>% summarise(`TotalHrs`= sum(S)) %>% extract2("TotalHrs") %>% mean()
    .sum_df <- .d %>% group_by(Board, Week) %>% summarise(`TotalHrs`= sum(S))
    .gg_course <- ggplot(data = .sum_df, aes_string(x = "Week", y = "TotalHrs", fill = "Board")) +
      geom_col() +
      geom_hline(yintercept = .avg) + 
      geom_text(aes(x = 0, y = .avg, label = "Average"), vjust = -1, nudge_x = 1.5) +
      scale_y_continuous(breaks = function(lims, .b = 5){seq(0, {.b * lims[2] %/% .b}, .b)}, minor_breaks = function(lims, .b = 1){seq(0, {.b * lims[2] %/% .b}, .b)}) +
      labs(title = glue::glue("Weekly Hours: {.y}"),
           subtitle = "",
           caption = "",
           x = "Week",y = "Hours") +
      theme(plot.title = element_text(hjust = .5),plot.subtitle = element_text(hjust = .5), axis.text.x = element_text(angle = 45))
    .x$graph <- .gg_course
    .x$table <- .sum_df
    .x$avg <- .avg
    return(.x)
  })
  .sem
})
