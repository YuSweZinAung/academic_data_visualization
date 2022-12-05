server <- function(session, input, output) {
   
  dashboard_dt <- academic_dt[,c("Course","Application_mode","Application_order","Daytime_evening_attendance","Previous_qualification","Previous_qualification_grade" , "Target")]
  dashboard_dt <- academic_dt %>% mutate(Marital_status = case_when(  
    
    Marital_status==1 ~ "single",
    Marital_status==2 ~ "married",  
    Marital_status==3 ~  "widower",
    Marital_status==4 ~  "divorced",
    Marital_status==5 ~  "facto union",
    Marital_status==6 ~  "legally separated"
  ), Nationality = case_when(
    Nationality == 1 ~ "Portuguese",
    Nationality == 2  ~ "German",
    Nationality == 6  ~ "Spanish",
    Nationality == 11  ~ "Italian",
    Nationality == 13  ~ "Dutch",
    Nationality == 14  ~ "English",
    Nationality == 17  ~ "Lithuanian",
    Nationality == 21  ~ "Angolan",
    Nationality == 22  ~ "Cape Verdean",
    Nationality == 24  ~ "Guinean",
    Nationality == 25  ~ "Mozambican",
    Nationality == 26  ~ "Santomean",
    Nationality == 32  ~ "Turkish",
    Nationality == 41  ~ "Brazilian",
    Nationality == 62  ~ "Romanian",
    Nationality == 100  ~ "Moldova (Republic of)",
    Nationality == 101  ~ "Mexican",
    Nationality == 103  ~ "Ukrainian",
    Nationality == 105  ~ "Russian",
    Nationality == 108  ~ "Cuban",
    Nationality == 109  ~ "Colombian" 
    
  )
  , Application_mode =  case_when(
    Application_mode == 1 ~ "1 - 1st phase-general contingent",
    Application_mode == 2 ~ "2 - Ordinance No. 612/93",
    Application_mode == 5 ~ "5 - 1st phase-(Azores Island)",
    Application_mode == 7 ~ "7 - Holders of other higher courses",
    Application_mode == 10 ~ "10 - Ordinance No. 854-B/99",
    Application_mode == 15 ~ "15 - International student (bachelor)",
    Application_mode == 16 ~ "16 - 1st phase-(Madeira Island)",
    Application_mode == 17 ~ "17 - 2nd phase-general contingent",
    Application_mode == 18 ~ "18 - 3rd phase-general contingent",
    Application_mode == 26 ~ "26 - Ordinance No. 533-A/99, item b2",
    Application_mode == 27 ~ "27 - Ordinance No. 533-A/99, item b3",
    Application_mode == 39 ~ "39 - Over 23 years old",
    Application_mode == 42 ~ "42 - Transfer",
    Application_mode == 43 ~ "43 - Change of course",
    Application_mode == 44 ~ "44 - Technological specialization",
    Application_mode == 51 ~ "51 - Change of institution/course",
    Application_mode == 53 ~ "53 - Short cycle diploma holders",
    Application_mode == 57 ~ "57 - Change of institution/course" 
    
    
  ), Course =  case_when(
    Course== 33  ~ "Biofuel Production Technologies",
    Course== 171  ~ "Animation and Multimedia Design",
    Course== 8014  ~ "Social Service (evening attendance)",
    Course== 9003  ~ "Agronomy",
    Course== 9070  ~ "Communication Design",
    Course== 9085  ~ "Veterinary Nursing",
    Course== 9119  ~ "Informatics Engineering",
    Course== 9130  ~ "Equinculture",
    Course== 9147  ~ "Management",
    Course== 9238  ~ "Social Service",
    Course== 9254  ~ "Tourism",
    Course== 9500  ~ "Nursing",
    Course== 9556  ~ "Oral Hygiene",
    Course== 9670  ~ "Advertising and Marketing Management",
    Course== 9773  ~ "Journalism and Communication",
    Course== 9853  ~ "Basic Education",
    Course== 9991  ~ "Management (evening attendance)" 
    
  ), Previous_qualification = case_when(
    
    Mother_qualification == 1 ~ "1 - Secondary Education - 12th",
    Mother_qualification == 2 ~ "2 - Bachelor Degree",
    Mother_qualification == 3 ~ "3 - Degree",
    Mother_qualification == 4 ~ "4 - Master",
    Mother_qualification == 5 ~ "5 - Doctorate",
    Mother_qualification == 6 ~ "6 - Higher Education",
    Mother_qualification == 9 ~ "9 - 12th Year of schooling Not Completed",
    Mother_qualification == 10 ~ "10 - 11th Year of schooling Not Completed",
    Mother_qualification == 11 ~ "11 - 7th Year of schooling",
    Mother_qualification == 12 ~ "12 - 11th year of schooling",
    Mother_qualification == 13 ~ "13 - 2nd year high school",
    Mother_qualification == 14 ~ "14 - 10th year of schooling",
    Mother_qualification == 18 ~ "18 - General commerce course",
    Mother_qualification == 19 ~ "19 - Basic Education 3rd Cycle or Equiv",
    Mother_qualification == 20 ~ "20 - Complementary High School",
    Mother_qualification == 22 ~ "22 - Technical course",
    Mother_qualification == 25 ~ "25 - High School Course Not Completed",
    Mother_qualification == 26 ~ "26 - 7th year of schooling",
    Mother_qualification == 27 ~ "27 - 2nd cycle of high school course",
    Mother_qualification == 29 ~ "29 - 9th Year of Schooling Not Completed",
    Mother_qualification == 30 ~ "30 - 8th year of schooling",
    Mother_qualification == 31 ~ "31 - General Course course",
    Mother_qualification == 33 ~ "33 - Supplementary course",
    Mother_qualification == 34 ~ "34 - Unknown",
    Mother_qualification == 35 ~ "35 - Cannot read or write",
    Mother_qualification == 36 ~ "36 - Can read without 4th year of schooling",
    Mother_qualification == 37 ~ "37 - Basic education 1st cycle",
    Mother_qualification == 38 ~ "38 - Basic Education 2nd Cycle",
    Mother_qualification == 39 ~ "39 - Specialization course",
    Mother_qualification == 40 ~ "40 - Degree (1st cycle)",
    Mother_qualification == 41 ~ "41 - Specialized course",
    Mother_qualification == 42 ~ "42 - Professional course",
    Mother_qualification == 43 ~ "43 - Master (2nd cycle)",
    Mother_qualification == 44 ~ "44 - Doctorate (3rd cycle)"
    
  ), Mother_qualification = case_when(
    Mother_qualification == 1 ~ "1 - Secondary Education - 12th",
    Mother_qualification == 2 ~ "2 - Bachelor Degree",
    Mother_qualification == 3 ~ "3 - Degree",
    Mother_qualification == 4 ~ "4 - Master",
    Mother_qualification == 5 ~ "5 - Doctorate",
    Mother_qualification == 6 ~ "6 - Higher Education",
    Mother_qualification == 9 ~ "9 - 12th Year of schooling Not Completed",
    Mother_qualification == 10 ~ "10 - 11th Year of schooling Not Completed",
    Mother_qualification == 11 ~ "11 - 7th Year of schooling",
    Mother_qualification == 12 ~ "12 - 11th year of schooling",
    Mother_qualification == 13 ~ "13 - 2nd year high school",
    Mother_qualification == 14 ~ "14 - 10th year of schooling",
    Mother_qualification == 18 ~ "18 - General commerce course",
    Mother_qualification == 19 ~ "19 - Basic Education 3rd Cycle or Equiv",
    Mother_qualification == 20 ~ "20 - Complementary High School",
    Mother_qualification == 22 ~ "22 - Technical course",
    Mother_qualification == 25 ~ "25 - High School Course Not Completed",
    Mother_qualification == 26 ~ "26 - 7th year of schooling",
    Mother_qualification == 27 ~ "27 - 2nd cycle of high school course",
    Mother_qualification == 29 ~ "29 - 9th Year of Schooling Not Completed",
    Mother_qualification == 30 ~ "30 - 8th year of schooling",
    Mother_qualification == 31 ~ "31 - General Course course",
    Mother_qualification == 33 ~ "33 - Supplementary course",
    Mother_qualification == 34 ~ "34 - Unknown",
    Mother_qualification == 35 ~ "35 - Cannot read or write",
    Mother_qualification == 36 ~ "36 - Can read without 4th year of schooling",
    Mother_qualification == 37 ~ "37 - Basic education 1st cycle",
    Mother_qualification == 38 ~ "38 - Basic Education 2nd Cycle",
    Mother_qualification == 39 ~ "39 - Specialization course",
    Mother_qualification == 40 ~ "40 - Degree (1st cycle)",
    Mother_qualification == 41 ~ "41 - Specialized course",
    Mother_qualification == 42 ~ "42 - Professional course",
    Mother_qualification == 43 ~ "43 - Master (2nd cycle)",
    Mother_qualification == 44 ~ "44 - Doctorate (3rd cycle)"
    
    
    
  ), Father_qualification  = case_when(
    Father_qualification == 1 ~ "1 - Secondary Education - 12th",
    Father_qualification == 2 ~ "2 - Bachelor Degree",
    Father_qualification == 3 ~ "3 - Degree",
    Father_qualification == 4 ~ "4 - Master",
    Father_qualification == 5 ~ "5 - Doctorate",
    Father_qualification == 6 ~ "6 - Higher Education",
    Father_qualification == 9 ~ "9 - 12th Year of schooling Not Completed",
    Father_qualification == 10 ~ "10 - 11th Year of schooling Not Completed",
    Father_qualification == 11 ~ "11 - 7th Year of schooling",
    Father_qualification == 12 ~ "12 - 11th year of schooling",
    Father_qualification == 13 ~ "13 - 2nd year high school",
    Father_qualification == 14 ~ "14 - 10th year of schooling",
    Father_qualification == 18 ~ "18 - General commerce course",
    Father_qualification == 19 ~ "19 - Basic Education 3rd Cycle or Equiv",
    Father_qualification == 20 ~ "20 - Complementary High School",
    Father_qualification == 22 ~ "22 - Technical course",
    Father_qualification == 25 ~ "25 - High School Course Not Completed",
    Father_qualification == 26 ~ "26 - 7th year of schooling",
    Father_qualification == 27 ~ "27 - 2nd cycle of high school course",
    Father_qualification == 29 ~ "29 - 9th Year of Schooling Not Completed",
    Father_qualification == 30 ~ "30 - 8th year of schooling",
    Father_qualification == 31 ~ "31 - General Course course",
    Father_qualification == 33 ~ "33 - Supplementary course",
    Father_qualification == 34 ~ "34 - Unknown",
    Father_qualification == 35 ~ "35 - Cannot read or write",
    Father_qualification == 36 ~ "36 - Can read without 4th year of schooling",
    Father_qualification == 37 ~ "37 - Basic education 1st cycle",
    Father_qualification == 38 ~ "38 - Basic Education 2nd Cycle",
    Father_qualification == 39 ~ "39 - Specialization course",
    Father_qualification == 40 ~ "40 - Degree (1st cycle)",
    Father_qualification == 41 ~ "41 - Specialized course",
    Father_qualification == 42 ~ "42 - Professional course",
    Father_qualification == 43 ~ "43 - Master (2nd cycle)",
    Father_qualification == 44 ~ "44 - Doctorate (3rd cycle)"
    
  ), 
  Mother_occupation = case_when(
    Mother_occupation == 0 ~ "0 - Student",
    Mother_occupation == 1 ~ "1 - High Level Position",
    Mother_occupation == 2 ~ "2 - Specialists in Intellectual & Science",
    Mother_occupation == 3 ~ "3 - Intermediate Level",
    Mother_occupation == 4 ~ "4 - Administrative staff",
    Mother_occupation == 5 ~ "5 - Personal Services",
    Mother_occupation == 6 ~ "6 - Skilled Workers",
    Mother_occupation == 7 ~ "7 - Skilled Workers",
    Mother_occupation == 8 ~ "8 - Installation and Assembly Workers",
    Mother_occupation == 9 ~ "9 - Unskilled Workers",
    Mother_occupation == 10 ~ "10 - Armed Forces Professions",
    Mother_occupation == 90 ~ "90 - Other Situation",
    Mother_occupation == 99 ~ "99 - (blank)",
    Mother_occupation == 122 ~ "122 - Health professionals",
    Mother_occupation == 123 ~ "123 - teachers",
    Mother_occupation == 125 ~ "125 - Specialists in technologies",
    Mother_occupation == 131 ~ "131 - Intermediate level",
    Mother_occupation == 132 ~ "132 - Intermediate level",
    Mother_occupation == 134 ~ "134 - Intermediate level",
    Mother_occupation == 141 ~ "141 - Office workers",
    Mother_occupation == 143 ~ "143 - Office workers",
    Mother_occupation == 144 ~ "144 - Administrative staff",
    Mother_occupation == 151 ~ "151 - personal service workers",
    Mother_occupation == 152 ~ "152 - sellers",
    Mother_occupation == 153 ~ "153 - Personal care workers and the like",
    Mother_occupation == 171 ~ "171 - Skilled construction workers",
    Mother_occupation == 173 ~ "173 - Skilled workers",
    Mother_occupation == 175 ~ "175 - Workers in F&B",
    Mother_occupation == 191 ~ "191 - cleaning workers",
    Mother_occupation == 192 ~ "192 - Unskilled workers",
    Mother_occupation == 193 ~ "193 - Unskilled workers",
    Mother_occupation == 194 ~ "194 - Meal preparation assistants"
    
    
    
  ), 
  Father_occupation =  case_when(
    Father_occupation == 0 ~ "0 - Student",
    Father_occupation == 1 ~ "1 - High Level Position",
    Father_occupation == 2 ~ "2 - Specialists in Intellectual & Science",
    Father_occupation == 3 ~ "3 - Intermediate Level",
    Father_occupation == 4 ~ "4 - Administrative staff",
    Father_occupation == 5 ~ "5 - Personal Services",
    Father_occupation == 6 ~ "6 - Skilled Workers",
    Father_occupation == 7 ~ "7 - Skilled Workers",
    Father_occupation == 8 ~ "8 - Installation and Assembly Workers",
    Father_occupation == 9 ~ "9 - Unskilled Workers",
    Father_occupation == 10 ~ "10 - Armed Forces Professions",
    Father_occupation == 90 ~ "90 - Other Situation",
    Father_occupation == 99 ~ "99 - (blank)",
    Father_occupation == 122 ~ "122 - Health professionals",
    Father_occupation == 123 ~ "123 - teachers",
    Father_occupation == 125 ~ "125 - Specialists in technologies",
    Father_occupation == 131 ~ "131 - Intermediate level",
    Father_occupation == 132 ~ "132 - Intermediate level",
    Father_occupation == 134 ~ "134 - Intermediate level",
    Father_occupation == 141 ~ "141 - Office workers",
    Father_occupation == 143 ~ "143 - Office workers",
    Father_occupation == 144 ~ "144 - Administrative staff",
    Father_occupation == 151 ~ "151 - personal service workers",
    Father_occupation == 152 ~ "152 - sellers",
    Father_occupation == 153 ~ "153 - Personal care workers and the like",
    Father_occupation == 171 ~ "171 - Skilled construction workers",
    Father_occupation == 173 ~ "173 - Skilled workers",
    Father_occupation == 175 ~ "175 - Workers in F&B",
    Father_occupation == 191 ~ "191 - cleaning workers",
    Father_occupation == 192 ~ "192 - Unskilled workers",
    Father_occupation == 193 ~ "193 - Unskilled workers",
    Father_occupation == 194 ~ "194 - Meal preparation assistants" 
    
  ), Gender = case_when(
    Gender == 1 ~ "male",
    Gender == 0 ~ "female"
    
  ), Daytime_evening_attendance = case_when(
    Daytime_evening_attendance == 1 ~ "daytime",
    Daytime_evening_attendance == 0 ~ "evening"
    
  ), Displaced = case_when(
    Displaced == 1 ~ "Yes",
    Displaced == 0 ~ "No"
    
  ), Educational_special_needs = case_when(
    Educational_special_needs == 1 ~ "Yes",
    Educational_special_needs == 0 ~ "No"
    
  ), Debtor = case_when(
    Debtor == 1 ~ "Yes",
    Debtor == 0 ~ "No"
    
  ), Tuition_fees_up_to_date = case_when(
    Tuition_fees_up_to_date == 1 ~ "Yes",
    Tuition_fees_up_to_date == 0 ~ "No"
    
  ), Scholarship_holder = case_when(
    Scholarship_holder == 1 ~ "Yes",
    Scholarship_holder == 0 ~ "No"
    
  ), International = case_when(
    International == 1 ~ "Yes",
    International == 0 ~ "No"
    
  )  
  )
  # 1.(a) Draw enrollment summary donuts chart ----------------
  Target_Dropout <- "Dropout"
  Target_Graduate <- "Graduate"
  Target_Enrolled <- "Enrolled"
  
  Target_Count <- academic_dt  %>% group_by(Target) %>%  summarise(  Target_n = n() ) 
  
  enrolment_summary <- data.frame(
    category=c(Target_Dropout, Target_Graduate , Target_Enrolled ),
    count=c(Target_Count$Target_n[Target_Count$Target==Target_Dropout] , Target_Count$Target_n[Target_Count$Target==Target_Graduate] , Target_Count$Target_n[Target_Count$Target==Target_Enrolled] )
  )
  
  # Compute percentages
  enrolment_summary$fraction <- enrolment_summary$count / sum(enrolment_summary$count)
  
  # Compute the cumulative percentages (top of each rectangle)
  enrolment_summary$ymax <- cumsum(enrolment_summary$fraction)
  
  # Compute the bottom of each rectangle
  enrolment_summary$ymin <- c(0, head(enrolment_summary$ymax, n=-1))
  
  # Compute label position
  enrolment_summary$labelPosition <- (enrolment_summary$ymax + enrolment_summary$ymin) / 2
  
  # Compute a good label
  enrolment_summary$label <- paste0(enrolment_summary$category, "\n value: ", enrolment_summary$count)
  
  # Make the plot
  output$enrolment_summary_plot <- renderPlot(
    ggplot(enrolment_summary, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
      geom_rect() +
      geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
      scale_fill_brewer(palette=4) +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none")
  )
  
  # 1.(b) Draw course bar chart -----------
  enrolment_summary_reactive = reactive({
    course_summary_dt <- dashboard_dt %>% filter(Target==input$targetEnrolment)
  })
  output$course_summary <- renderPlot({
    ggplot(enrolment_summary_reactive()) + geom_bar(aes(y = fct_rev(fct_infreq(Course))) ,color="black",fill="lightblue") +
      theme_classic() + labs(  y = "Course", x = "Count") 
  })
  # 1.(c) Application order -------------
  application_order_reactive = reactive({
    application_order_dt <- enrolment_summary_reactive() %>% group_by(Application_order) %>% summarise(count=n())
  })
  output$application_order_summary <- renderPlot({
    ggplot(application_order_reactive(), aes(x = Application_order, y = count)) +
      geom_line(color = "black", size = 1) +
      geom_point(color = "#0099f9", size = 5,pch = 24,
                 bg = "lightblue" ) +
      theme_classic() + labs( y = "Count", x = "Application order")
  })
  # 1.(d) day time attendance ---------------
  output$daytime_attendance_summary <- renderPlot({
    v_target = input$targetEnrolment
    if(is.null(v_target)|| v_target == "")
      v_target = "Dropout"
    dt <- dashboard_dt %>% filter(Target==v_target)
    #Attendance <- factor(dt$Daytime_evening_attendance)
    #AttendanceData <- data.frame(Attendance)
    
    #cols <- hcl.colors(length(levels(Attendance)), "BrBG")
    #PieChart(Attendance, data = AttendanceData, hole = 0, fill = cols, labels_cex = 1.6)

    slices <- (dt %>% group_by(Daytime_evening_attendance) %>% summarise(count=n()))$count
    lbls <- (dt %>% group_by(Daytime_evening_attendance) %>% summarise(count=n()))$Daytime_evening_attendance
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels
    lbls <- paste(lbls,"%",sep="") # ad % to labels
    pie(slices,labels = lbls, col=hcl.colors(length(lbls), "Greens"),  radious = 2 )
     
    
  })
  # 1.(e) Previous qualication via grade -----------------
  output$previous_qualification <- renderPlot({
    crop2<-ggplot(enrolment_summary_reactive(), aes(x=Previous_qualification, y= Previous_qualification_grade ,fill=Previous_qualification )) +
      geom_boxplot()+  theme_classic()  + 
      theme(axis.text.x = element_text(angle = 45 , vjust=1 , hjust=1)) + labs(y = "Previous qualification (grade)", x = "Previous qualification") 
    
    crop2   + coord_flip() + 
      scale_fill_viridis(discrete = TRUE, alpha=0.6) +
      #geom_jitter(color="black", size=0.2 )   +
      theme(
        legend.position="none",
        plot.title = element_text(size=11)
      )
  })
  # 1.(f) Application Mode ------------------
  output$application_mode <- renderPlot({
    ggplot(enrolment_summary_reactive()) + geom_bar(aes(y = fct_rev(fct_infreq(Application_mode))) ,color="black",fill="lightblue") +
      theme_classic() + labs(  y = "Application mode", x = "Count")
  })
  
  # 2. Observations ----------------
  output$observation <- renderPlot({
    colX <- input$obX
    colY <- input$obY
    if( column_attributes$Type[column_attributes$Attribute==colX] == "Continuous"){
      
      ggplot(dashboard_dt, aes_string(x = colX , y = colY  )) + geom_point()+  theme_classic()  + 
        theme(axis.text.x = element_text(angle = 45 , vjust=1 , hjust=1)) + 
        labs(y = str_replace_all(colY, "_", " "), x = str_replace_all(colX, "_", " "))  +  facet_grid(. ~ Target)
    } else
    {
      ggplot(dashboard_dt, aes_string(x=colX, y= colY  )) +
        geom_boxplot()+  theme_classic()  + 
        theme(axis.text.x = element_text(angle = 45 , vjust=1 , hjust=1)) + 
        labs(y = str_replace_all(colY, "_", " "), x =   str_replace_all(colX, "_", " ")) +  facet_grid(. ~ Target)
    } 
  })
  
  
  
  # 3. Analysis
  
  
  # 4. Data
  # For Data tab -> csv data
  output$rawtable <- renderPrint({
    academic_dt_sub = academic_dt %>% select(c(Marital_status,
                                         Application_mode,
                                         Application_order,
                                         Course,
                                         Daytime_evening_attendance,
                                         Previous_qualification,
                                         Previous_qualification_grade,
                                         Nationality,
                                         Mother_qualification,
                                         Father_qualification,
                                         Mother_occupation,
                                         Father_occupation,
                                         Admission_grade,
                                         Displaced,
                                         Educational_special_needs,
                                         Debtor,
                                         Tuition_fees_up_to_date,
                                         Gender,
                                         Scholarship_holder,
                                         Age_at_enrollment,
                                         International,
                                         Curricular_units_1st_sem_credited,
                                         Curricular_units_1st_sem_enrolled,
                                         Curricular_units_1st_sem_evaluations,
                                         Curricular_units_1st_sem_approved,
                                         Curricular_units_1st_sem_grade,
                                         Curricular_units_1st_sem_without_evaluations,
                                         Curricular_units_2nd_sem_credited,
                                         Curricular_units_2nd_sem_enrolled,
                                         Curricular_units_2nd_sem_evaluations,
                                         Curricular_units_2nd_sem_approved,
                                         Curricular_units_2nd_sem_grade,
                                         Curricular_units_2nd_sem_without_evaluations,
                                         Unemployment_rate,
                                         Inflation_rate,
                                         GDP,
                                         Target))
    names(academic_dt_sub) = c("Marital_status",
                            "Application_mode",
                            "Application_order",
                            "Course",
                            "Daytime_evening_attendance",
                            "Previous_qualification",
                            "Previous_qualification_grade",
                            "Nationality",
                            "Mother_qualification",
                            "Father_qualification",
                            "Mother_occupation",
                            "Father_occupation",
                            "Admission_grade",
                            "Displaced",
                            "Educational_special_needs",
                            "Debtor",
                            "Tuition_fees_up_to_date",
                            "Gender",
                            "Scholarship_holder",
                            "Age_at_enrollment",
                            "International",
                            "Curricular_units_1st_sem_credited",
                            "Curricular_units_1st_sem_enrolled",
                            "Curricular_units_1st_sem_evaluations",
                            "Curricular_units_1st_sem_approved",
                            "Curricular_units_1st_sem_grade",
                            "Curricular_units_1st_sem_without_evaluations",
                            "Curricular_units_2nd_sem_credited",
                            "Curricular_units_2nd_sem_enrolled",
                            "Curricular_units_2nd_sem_evaluations",
                            "Curricular_units_2nd_sem_approved",
                            "Curricular_units_2nd_sem_grade",
                            "Curricular_units_2nd_sem_without_evaluations",
                            "Unemployment_rate",
                            "Inflation_rate",
                            "GDP",
                            "Target")
    orig <- options(width = 1000)
    print(tail(academic_dt_sub, input$maxrows), row.names = FALSE)
    options(orig)
  })
  
  # For Data tab -> download csv handler
  # output to download data
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("ACADEMIC_DATA_", current_date, ".csv", sep="")
    },
    content = function(file) {
      academic_dt_sub = academic_dt %>% select(c(Marital_status,
                                           Application_mode,
                                           Application_order,
                                           Course,
                                           Daytime_evening_attendance,
                                           Previous_qualification,
                                           Previous_qualification_grade,
                                           Nationality,
                                           Mother_qualification,
                                           Father_qualification,
                                           Mother_occupation,
                                           Father_occupation,
                                           Admission_grade,
                                           Displaced,
                                           Educational_special_needs,
                                           Debtor,
                                           Tuition_fees_up_to_date,
                                           Gender,
                                           Scholarship_holder,
                                           Age_at_enrollment,
                                           International,
                                           Curricular_units_1st_sem_credited,
                                           Curricular_units_1st_sem_enrolled,
                                           Curricular_units_1st_sem_evaluations,
                                           Curricular_units_1st_sem_approved,
                                           Curricular_units_1st_sem_grade,
                                           Curricular_units_1st_sem_without_evaluations,
                                           Curricular_units_2nd_sem_credited,
                                           Curricular_units_2nd_sem_enrolled,
                                           Curricular_units_2nd_sem_evaluations,
                                           Curricular_units_2nd_sem_approved,
                                           Curricular_units_2nd_sem_grade,
                                           Curricular_units_2nd_sem_without_evaluations,
                                           Unemployment_rate,
                                           Inflation_rate,
                                           GDP,
                                           Target))
      names(academic_dt_sub) = c("Marital_status",
                              "Application_mode",
                              "Application_order",
                              "Course",
                              "Daytime_evening_attendance",
                              "Previous_qualification",
                              "Previous_qualification_grade",
                              "Nationality",
                              "Mother_qualification",
                              "Father_qualification",
                              "Mother_occupation",
                              "Father_occupation",
                              "Admission_grade",
                              "Displaced",
                              "Educational_special_needs",
                              "Debtor",
                              "Tuition_fees_up_to_date",
                              "Gender",
                              "Scholarship_holder",
                              "Age_at_enrollment",
                              "International",
                              "Curricular_units_1st_sem_credited",
                              "Curricular_units_1st_sem_enrolled",
                              "Curricular_units_1st_sem_evaluations",
                              "Curricular_units_1st_sem_approved",
                              "Curricular_units_1st_sem_grade",
                              "Curricular_units_1st_sem_without_evaluations",
                              "Curricular_units_2nd_sem_credited",
                              "Curricular_units_2nd_sem_enrolled",
                              "Curricular_units_2nd_sem_evaluations",
                              "Curricular_units_2nd_sem_approved",
                              "Curricular_units_2nd_sem_grade",
                              "Curricular_units_2nd_sem_without_evaluations",
                              "Unemployment_rate",
                              "Inflation_rate",
                              "GDP",
                              "Target")
      write.csv(academic_dt_sub, file)
    }
  )
}




