# Behaviour calculation       =====
#' Set parameters for AE/BV calculation
nback=25
which.apen=2

# Outputs         ======
# The produced figure/tables 
outputs <- list(
  figs = list(),
  tbls = list()
)

# Set ggplot colours        =====
if( getOption("project_custom_ggplot") ){
  options(
    ggplot2.continous.colour = gen_col("rbgo"),
    ggplot2.continous.fill   = gen_col("rbgo"), 
    ggplot2.discrete.colour  = gen_col("rbgo"), 
    ggplot2.discrete.fill    = gen_col("rbgo")
  )
  theme_set( theme_bw() )
}



# Experiment information          =======
experiment_information <- list(
  no = list(
    # probe info
    probe_information = list(
      questions = list(
        probe1 = "I hvilken grad fokuserte du på oppgaven rett før dette spørsmålet?",
        probe2 = "I den grad du ikke fokuserte på oppgaven, tenkte du på ingenting eller tenkte du på noe spesifikt?",
        probe3 = "Var du bevisst på hvor du fokusserte oppmerksomheten din (enten oppgaverelatert eller annet) eller var den spontan?"
      ),
      descriptions = list(
        probe1 = "1 = helt klart IKKE fokussert; 4 = helt klart fokussert",
        probe2 = "1 = helt klart ingenting; 4 = helt klart noe spesifikt",
        probe3 = "1 = helt klart spontan; 4 = helt klart bevisst")
    ),
    
    # Participant feedback
    participant_feedback = list(
      questions = list(
        gloves = "vennligst evaluer i hvilken grad du ble distrahert av å ha på deg engangshansker i løpet av oppgaven:",
        ae_motivation = "Vennligst vurder hvor motivert du var til å produsere tilfeldige sekvenser da eksperimentet startet:",
        mw_confidence = "Når du svarte på spørsmålet om hvor fokusert du var på oppgaven, hvor sikker var du på svaret ditt?",
        task_strategy = "Bruke du noen spesiell strategi for å lage en tilfeldig sekvens?",
        comment_task = "Har du noen bemerkninger angående oppgaven i seg selv",
        off_task_thought = "Når du rapporterte å ikke være fokusert på oppgaven (OFF-TASK), hva tenkte du på da?",
        comment_other = "Har du noen andre kommentarer?"),
      description = list(
        glove = "1 - ikke i det hele tatt distrahert, 7 - ekstremt distrahert",
        ae_motivation = "1 - ikke motivert, 7 - veldig motivert",
        mw_confidence = "1 - ikke sikker i det hele tatt, 7 - ekstremt sikker",
        task_strategy = "Text answer",
        comment_task = "Text answer",
        off_task_thought = "Text answer",
        comment_other = "Text answer")
    )
  ),
  
  en = list(
    probe_information = list(
      questions = list(
        probe1 = "To what extent were you focused on the task right before this question?",
        probe2 = "To the extent that you were not focused on the task, were you thinking about nothing or were you thinking about something specific?",
        probe3 = "Were you aware of where you were directing your attention (either task-related or otherwise) or was it spontaneous?"
      ),
      descriptions = list(
        probe1 = "1 = definitely NOT focused; 4 = definitely focused",
        probe2 = "1 = definitely nothing; 4 = definitely something specific",
        probe3 = "1 = definitely spontaneous; 4 = definitely aware"
      )
    ),
    
    participant_feedback = list(
      questions = list(
        gloves = "Please evaluate to what extent you were distracted by wearing disposable gloves during the task:",
        ae_motivation = "Please assess how motivated you were to produce random sequences when the experiment started:",
        mw_confidence = "When you answered the question about how focused you were on the task, how confident were you in your answer?",
        task_strategy = "Did you use any particular strategy to create a random sequence?",
        comment_task = "Do you have any remarks regarding the task itself?",
        off_task_thought = "When you reported not being focused on the task (OFF-TASK), what were you thinking about?",
        comment_other = "Do you have any other comments?"
      ),
      description = list(
        glove = "1 - not at all distracted, 7 - extremely distracted",
        ae_motivation = "1 - not motivated, 7 - very motivated",
        mw_confidence = "1 - not at all confident, 7 - extremely confident",
        task_strategy = "Text answer",
        comment_task = "Text answer",
        off_task_thought = "Text answer",
        comment_other = "Text answer"
      )
    )
  )
)