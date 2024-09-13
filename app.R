# Load required packages
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(purrr)
library(rlang)

# Define Distribution Metadata
distributions <- list(
  
  # Continuous Distributions
  continuous = list(
    "Normal" = list(
      params = list(
        mean = list(default = 0, min = -Inf, max = Inf),
        sd = list(default = 1, min = 0.01, max = Inf)
      ),
      support = c(-Inf, Inf),
      description = "The normal distribution is a continuous probability distribution characterized by a symmetric, bell-shaped curve. It is defined by two parameters: the mean (μ) and the standard deviation (σ).",
      examples = c(
        "Measurement Errors: Modeling random measurement errors in laboratory experiments, such as variations in repeated measurements of a chemical concentration using the same instrument.",
        "Biological Variations: Representing natural variations in biological characteristics, like human heights or blood pressure readings within a population.",
        "Quality Control: Analyzing product dimensions in manufacturing processes to ensure they meet specified tolerances, assuming the dimensions are normally distributed around a target value."
      )
    ),
    
    "Beta" = list(
      params = list(
        shape1 = list(default = 2, min = 0.01, max = Inf),
        shape2 = list(default = 2, min = 0.01, max = Inf)
      ),
      support = c(0, 1),
      description = "The beta distribution is a continuous probability distribution defined on the interval [0,1], characterized by two shape parameters (α and β). It is flexible and can model various shapes including uniform, U-shaped, or skewed distributions.",
      examples = c(
        "Proportions in Mixtures: Modeling the proportion of a component in a chemical mixture when the proportion is between 0 and 1.",
        "Success Rates: Representing the probability of success in a series of experiments, such as the efficacy rate of a new drug in clinical trials.",
        "Reliability Analysis: Estimating the probability that a system will perform within acceptable limits, especially when probabilities are constrained between 0 and 1."
      )
    ),
    
    "Gamma" = list(
      params = list(
        shape = list(default = 2, min = 0.01, max = Inf),
        rate = list(default = 1, min = 0.01, max = Inf)
      ),
      support = c(0, Inf),
      description = "The gamma distribution is a two-parameter family of continuous probability distributions, often used to model waiting times or lifetimes of events. It is defined by a shape parameter (k) and a rate parameter (θ).",
      examples = c(
        "Waiting Times: Modeling the time until an event occurs, such as the time between emissions of particles in radioactive decay.",
        "Insurance Claims: Representing the distribution of claim amounts in actuarial science, where the amounts are always positive and can vary widely.",
        "Rainfall Modeling: Analyzing the amount of rainfall accumulated in a reservoir over a certain period, which is continuous and positively skewed."
      )
    ),
    
    "Uniform" = list(
      params = list(
        min = list(default = 0, min = -Inf, max = Inf),
        max = list(default = 1, min = -Inf, max = Inf)
      ),
      support = c(-Inf, Inf),
      description = "The uniform distribution is a continuous probability distribution where all outcomes are equally likely within a defined interval [a, b].",
      examples = c(
        "Random Sampling: Simulating a scenario where any outcome within a range is equally likely, such as selecting a random point within a square area.",
        "Scheduling: Modeling arrival times within a fixed interval when any time is equally probable, like customers arriving at a store between opening and closing times.",
        "Random Number Generation: Generating random numbers for simulations where each number within a range has an equal chance of being selected."
      )
    ),
    
    "Exponential" = list(
      params = list(
        rate = list(default = 1, min = 0.01, max = Inf)
      ),
      support = c(0, Inf),
      description = "The exponential distribution is a continuous probability distribution often associated with the time between independent events occurring at a constant average rate. It is characterized by a single rate parameter (λ).",
      examples = c(
        "Time Between Events: Modeling the time between independent events that occur at a constant average rate, such as the time between incoming calls at a call center.",
        "Reliability Engineering: Representing the lifespan of electronic components that have a constant failure rate over time.",
        "Queue Theory: Analyzing service times in queuing systems where the probability of service completion is memoryless."
      )
    ),
    
    "Student's t" = list(
      params = list(
        df = list(default = 5, min = 1, max = Inf)
      ),
      support = c(-Inf, Inf),
      description = "The Student's t-distribution is a continuous probability distribution that arises when estimating the mean of a normally distributed population in situations where the sample size is small. It is characterized by degrees of freedom parameter.",
      examples = c(
        "Small Sample Inference: Estimating the mean of a normally distributed population when the sample size is small and population standard deviation is unknown.",
        "Confidence Intervals: Constructing confidence intervals for the mean in small-sample experiments, such as pilot studies.",
        "Hypothesis Testing: Performing t-tests to compare means between two small independent samples in experimental research."
      )
    ),
    
    "Cauchy" = list(
      params = list(
        location = list(default = 0, min = -Inf, max = Inf),
        scale = list(default = 1, min = 0.01, max = Inf)
      ),
      support = c(-Inf, Inf),
      description = "The Cauchy distribution is a continuous probability distribution with heavy tails and undefined mean and variance. It is characterized by a location parameter and a scale parameter.",
      examples = c(
        "Resonance Behavior: Modeling resonance phenomena in physics, such as the distribution of energy in a resonant system.",
        "Signal Processing: Representing the distribution of phase noise in oscillators where extreme values are more probable than in a normal distribution.",
        "Financial Returns: Analyzing asset returns that may have heavy tails and outliers, which the normal distribution may not capture effectively."
      )
    ),
    
    "Weibull" = list(
      params = list(
        shape = list(default = 2, min = 0.01, max = Inf),
        scale = list(default = 1, min = 0.01, max = Inf)
      ),
      support = c(0, Inf),
      description = "The Weibull distribution is a continuous probability distribution used extensively in reliability engineering and failure analysis. It is characterized by a shape parameter and a scale parameter.",
      examples = c(
        "Material Strength: Modeling the breaking strength of materials under stress testing in engineering.",
        "Failure Times: Representing the distribution of failure times in reliability studies of mechanical components.",
        "Wind Speed Analysis: Analyzing wind speed data for wind energy assessments, as wind speeds often follow a Weibull distribution."
      )
    ),
    
    "Laplace" = list(
      params = list(
        location = list(default = 0, min = -Inf, max = Inf),
        scale = list(default = 1, min = 0.01, max = Inf)
      ),
      support = c(-Inf, Inf),
      description = "The Laplace distribution, also known as the double exponential distribution, is a continuous probability distribution with a sharp peak at the mean and heavy tails. It is characterized by a location parameter and a scale parameter.",
      examples = c(
        "Econometrics: Modeling financial returns that exhibit sharp peaks and heavy tails compared to the normal distribution.",
        "Image Processing: Representing differences in pixel intensities in image edge detection algorithms.",
        "Error Modeling: Analyzing error terms in time series data that may have abrupt changes or jumps."
      )
    ),
    
    "Logistic" = list(
      params = list(
        location = list(default = 0, min = -Inf, max = Inf),
        scale = list(default = 1, min = 0.01, max = Inf)
      ),
      support = c(-Inf, Inf),
      description = "The logistic distribution is similar to the normal distribution but has heavier tails. It is often used in logistic regression and neural networks. It is characterized by a location parameter and a scale parameter.",
      examples = c(
        "Growth Models: Modeling population growth in biology where growth accelerates rapidly and then slows down as it approaches a maximum limit.",
        "Logistic Regression: Serving as the underlying distribution in logistic regression models for binary classification problems.",
        "Psychometrics: Analyzing response times in cognitive tests where the probability of a correct response follows a logistic function over time."
      )
    )
  ),
  
  # Discrete Distributions
  discrete = list(
    "Poisson" = list(
      params = list(
        lambda = list(default = 1, min = 0, max = Inf)
      ),
      support = c(0, Inf),
      description = "The Poisson distribution is a discrete probability distribution expressing the probability of a given number of events occurring in a fixed interval of time or space, given a known average rate of occurrence.",
      examples = c(
        "Event Counts: Counting the number of occurrences of an event in a fixed interval, such as the number of mutations in a strand of DNA per unit length.",
        "Astronomy: Modeling the number of stars observed in a fixed area of the sky during telescope observations.",
        "Traffic Flow: Analyzing the number of cars passing through a checkpoint in a fixed time period."
      )
    ),
    
    "Binomial" = list(
      params = list(
        size = list(default = 10, min = 1, max = Inf),
        prob = list(default = 0.5, min = 0, max = 1)
      ),
      support = c(0, Inf),
      description = "The binomial distribution is a discrete probability distribution of the number of successes in a sequence of n independent experiments, each asking a yes-no question, and each with its own boolean-valued outcome.",
      examples = c(
        "Quality Testing: Determining the number of defective items in a batch of products when each item has a fixed probability of being defective.",
        "Clinical Trials: Modeling the number of patients who experience a side effect in a drug trial.",
        "Survey Analysis: Calculating the number of favorable responses in a survey where respondents answer 'yes' or 'no'."
      )
    ),
    
    "Negative Binomial" = list(
      params = list(
        size = list(default = 5, min = 1, max = Inf),
        prob = list(default = 0.5, min = 0, max = 1)
      ),
      support = c(0, Inf),
      description = "The negative binomial distribution is a discrete probability distribution that models the number of failures before a specified number of successes occurs. It generalizes the geometric distribution.",
      examples = c(
        "Overdispersed Counts: Modeling count data with variance greater than the mean, such as the number of disease occurrences in epidemiology studies.",
        "Ecology Studies: Analyzing the number of species observed in ecological sampling when the occurrence is clustered.",
        "Quality Control: Representing the number of defective items found before a certain number of acceptable items are produced."
      )
    )
  )
)

# Helper Functions

# Function to generate parameter inputs
generate_param_inputs <- function(distribution, distributions_list) {
  category <- ifelse(distribution %in% names(distributions_list$continuous), "continuous", "discrete")
  params <- distributions_list[[category]][[distribution]]$params
  
  # Create a list of numericInput UI elements
  inputs <- map(names(params), function(param_name) {
    param_info <- params[[param_name]]
    numericInput(
      inputId = param_name,
      label = param_name,
      value = param_info$default,
      min = param_info$min,
      max = param_info$max
    )
  })
  
  # Return the UI elements as a tagList using the !!! operator for splicing
  tagList(!!!inputs)
}

# Function to calculate density/probability
calculate_density <- function(distribution, params, x_values) {
  switch(distribution,
         "Normal" = dnorm(x_values, mean = params$mean, sd = params$sd),
         "Beta" = dbeta(x_values, shape1 = params$shape1, shape2 = params$shape2),
         "Gamma" = dgamma(x_values, shape = params$shape, rate = params$rate),
         "Uniform" = dunif(x_values, min = params$min, max = params$max),
         "Exponential" = dexp(x_values, rate = params$rate),
         "Poisson" = dpois(x_values, lambda = params$lambda),
         "Binomial" = dbinom(x_values, size = params$size, prob = params$prob),
         "Negative Binomial" = dnbinom(x_values, size = params$size, prob = params$prob),
         "Student's t" = dt(x_values, df = params$df),
         "Cauchy" = dcauchy(x_values, location = params$location, scale = params$scale),
         "Weibull" = dweibull(x_values, shape = params$shape, scale = params$scale),
         "Laplace" = {
           # Laplace distribution (double exponential)
           mu <- params$location
           b <- params$scale
           (1 / (2 * b)) * exp(-abs(x_values - mu) / b)
         },
         "Logistic" = dlogis(x_values, location = params$location, scale = params$scale),
         NA
  )
}

# Define Shiny UI
ui <- page_sidebar(
  theme = bs_theme(preset = "minty"),
  title = "Distribution Explorer",
  sidebar = sidebar(
    selectInput("distribution", "Select Distribution:",
                choices = c(
                  "Normal", "Beta", "Gamma", "Uniform", "Exponential", 
                  "Poisson", "Binomial", "Student's t", "Cauchy", 
                  "Weibull", "Laplace", "Logistic", "Negative Binomial"
                )),
    uiOutput("parameterInputs")
  ),
  card(
    plotOutput("distPlot")
  ),
  layout_column_wrap(
    width = 1/2,
    card(
      card_header("Distribution Description"),
      htmlOutput("distDescription")
    ),
    card(
      card_header("Examples in Scientific Research"),
      htmlOutput("distExamples")
    )
  )
)

# Define Shiny Server
server <- function(input, output, session) {
  
  # Reactive expression for selected distribution
  selected_dist <- reactive({
    req(input$distribution)
    input$distribution
  })
  
  # Dynamically generate parameter inputs
  output$parameterInputs <- renderUI({
    generate_param_inputs(selected_dist(), distributions)
  })
  
  # Render the distribution plot using ggplot2
  output$distPlot <- renderPlot({
    req(selected_dist())
    distribution <- selected_dist()
    
    # Determine category
    category <- ifelse(distribution %in% names(distributions$continuous), "continuous", "discrete")
    
    # Extract parameters
    params_def <- distributions[[category]][[distribution]]$params
    param_values <- map(names(params_def), ~ input[[.x]]) %>% 
      set_names(names(params_def))
    
    # Define support and adjust x range
    support <- distributions[[category]][[distribution]]$support
    xmin <- ifelse(is.infinite(support[1]), -10, support[1])
    xmax <- ifelse(is.infinite(support[2]), 10, support[2])
    
    # Adjust x range for specific distributions
    if (distribution == "Gamma") {
      xmax <- max(10, qgamma(0.999, shape = param_values$shape, rate = param_values$rate))
    } else if (distribution == "Exponential") {
      xmax <- max(10, qexp(0.999, rate = param_values$rate))
    } else if (distribution == "Poisson") {
      xmax <- max(10, qpois(0.999, lambda = param_values$lambda))
    } else if (distribution == "Weibull") {
      xmax <- max(10, qweibull(0.999, shape = param_values$shape, scale = param_values$scale))
    } else if (distribution == "Negative Binomial") {
      xmax <- max(10, qnbinom(0.999, size = param_values$size, prob = param_values$prob))
    }
    
    # Generate x values within support
    if (category == "continuous") {
      if (distribution == "Beta") {
        x <- seq(0, 1, length.out = 1000)
      } else if (distribution %in% c("Gamma", "Exponential", "Weibull", "Logistic", "Laplace")) {
        x <- seq(max(0, xmin), xmax, length.out = 1000)
      } else {
        x <- seq(xmin, xmax, length.out = 1000)
      }
    } else if (category == "discrete") {
      x <- 0:ceiling(xmax)
    }
    
    # Calculate density or probability
    y <- calculate_density(distribution, param_values, x)
    
    # Create data frame and clean data
    data <- data.frame(x = x, y = y) %>% 
      filter(is.finite(y))
    
    # Plot using ggplot2
    if (category == "discrete") {
      ggplot(data, aes(x = x, y = y)) +
        geom_col(fill = "blue") +
        labs(title = paste(distribution, "Distribution"), x = "x", y = "Probability") +
        theme_minimal()
    } else {
      ggplot(data, aes(x = x, y = y)) +
        geom_line(color = "blue") +
        labs(title = paste(distribution, "Distribution"), x = "x", y = "Density") +
        theme_minimal()
    }
    
  })
  
  # Output the distribution description
  output$distDescription <- renderUI({
    req(selected_dist())
    distribution <- selected_dist()
    category <- ifelse(distribution %in% names(distributions$continuous), "continuous", "discrete")
    HTML(paste("<p>", distributions[[category]][[distribution]]$description, "</p>"))
  })
  
  # Output the distribution examples
  output$distExamples <- renderUI({
    req(selected_dist())
    distribution <- selected_dist()
    category <- ifelse(distribution %in% names(distributions$continuous), "continuous", "discrete")
    examples <- distributions[[category]][[distribution]]$examples
    # Format as unordered list
    html_examples <- paste0("<ul>", paste0("<li>", examples, "</li>"), "</ul>")
    HTML(html_examples)
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)