# Load required packages
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(purrr)

# Define UI
ui <- page_sidebar(
  theme = bs_theme(preset = "minty"), # Use Minty theme from bslib
  title = "Distribution Explorer",
  sidebar = sidebar(
    selectInput("distribution", "Select Distribution:",
                choices = c("Normal", "Beta", "Gamma", "Uniform", "Exponential", "Poisson", "Binomial",
                            "Student's t", "Cauchy", "Weibull", "Laplace", "Logistic", "Negative Binomial")),
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
      card_header("Example in Chemistry R&D at Dow"),
      htmlOutput("distExample")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Define parameters, descriptions, and examples for each distribution
  # Distribution Parameters with Descriptions and Examples
  distributionParams <- list(
    "Normal" = list(
      params = list(
        "mean" = list("default" = 0, "min" = -Inf, "max" = Inf),
        "sd" = list("default" = 1, "min" = 0.01, "max" = Inf)
      ),
      support = c(-Inf, Inf),
      description = "The normal distribution is a continuous probability distribution characterized by a symmetric, bell-shaped curve. It is defined by two parameters: the mean (μ) and the standard deviation (σ).",
      example = "<ul>
                 <li><strong>Measurement Errors:</strong> Modeling random measurement errors in laboratory experiments, such as variations in repeated measurements of a chemical concentration using the same instrument.</li>
                 <li><strong>Biological Variations:</strong> Representing natural variations in biological characteristics, like human heights or blood pressure readings within a population.</li>
                 <li><strong>Quality Control:</strong> Analyzing product dimensions in manufacturing processes to ensure they meet specified tolerances, assuming the dimensions are normally distributed around a target value.</li>
               </ul>"
    ),
    
    "Beta" = list(
      params = list(
        "shape1" = list("default" = 2, "min" = 0.01, "max" = Inf),
        "shape2" = list("default" = 2, "min" = 0.01, "max" = Inf)
      ),
      support = c(0, 1),
      description = "The beta distribution is a continuous probability distribution defined on the interval [0,1], characterized by two shape parameters (α and β). It is flexible and can model various shapes including uniform, U-shaped, or skewed distributions.",
      example = "<ul>
                 <li><strong>Proportions in Mixtures:</strong> Modeling the proportion of a component in a chemical mixture when the proportion is between 0 and 1.</li>
                 <li><strong>Success Rates:</strong> Representing the probability of success in a series of experiments, such as the efficacy rate of a new drug in clinical trials.</li>
                 <li><strong>Reliability Analysis:</strong> Estimating the probability that a system will perform within acceptable limits, especially when probabilities are constrained between 0 and 1.</li>
               </ul>"
    ),
    
    "Gamma" = list(
      params = list(
        "shape" = list("default" = 2, "min" = 0.01, "max" = Inf),
        "rate" = list("default" = 1, "min" = 0.01, "max" = Inf)
      ),
      support = c(0, Inf),
      description = "The gamma distribution is a two-parameter family of continuous probability distributions, often used to model waiting times or lifetimes of events. It is defined by a shape parameter (k) and a rate parameter (θ).",
      example = "<ul>
                 <li><strong>Waiting Times:</strong> Modeling the time until an event occurs, such as the time between emissions of particles in radioactive decay.</li>
                 <li><strong>Insurance Claims:</strong> Representing the distribution of claim amounts in actuarial science, where the amounts are always positive and can vary widely.</li>
                 <li><strong>Rainfall Modeling:</strong> Analyzing the amount of rainfall accumulated in a reservoir over a certain period, which is continuous and positively skewed.</li>
               </ul>"
    ),
    
    "Uniform" = list(
      params = list(
        "min" = list("default" = 0, "min" = -Inf, "max" = Inf),
        "max" = list("default" = 1, "min" = -Inf, "max" = Inf)
      ),
      support = c(-Inf, Inf),
      description = "The uniform distribution is a continuous probability distribution where all outcomes are equally likely within a defined interval [a, b].",
      example = "<ul>
                 <li><strong>Random Sampling:</strong> Simulating a scenario where any outcome within a range is equally likely, such as selecting a random point within a square area.</li>
                 <li><strong>Scheduling:</strong> Modeling arrival times within a fixed interval when any time is equally probable, like customers arriving at a store between opening and closing times.</li>
                 <li><strong>Random Number Generation:</strong> Generating random numbers for simulations where each number within a range has an equal chance of being selected.</li>
               </ul>"
    ),
    
    "Exponential" = list(
      params = list(
        "rate" = list("default" = 1, "min" = 0.01, "max" = Inf)
      ),
      support = c(0, Inf),
      description = "The exponential distribution is a continuous probability distribution often associated with the time between independent events occurring at a constant average rate. It is characterized by a single rate parameter (λ).",
      example = "<ul>
                 <li><strong>Time Between Events:</strong> Modeling the time between independent events that occur at a constant average rate, such as the time between incoming calls at a call center.</li>
                 <li><strong>Reliability Engineering:</strong> Representing the lifespan of electronic components that have a constant failure rate over time.</li>
                 <li><strong>Queue Theory:</strong> Analyzing service times in queuing systems where the probability of service completion is memoryless.</li>
               </ul>"
    ),
    
    "Poisson" = list(
      params = list(
        "lambda" = list("default" = 1, "min" = 0, "max" = Inf)
      ),
      support = c(0, Inf),
      description = "The Poisson distribution is a discrete probability distribution expressing the probability of a given number of events occurring in a fixed interval of time or space, given a known average rate of occurrence.",
      example = "<ul>
                 <li><strong>Event Counts:</strong> Counting the number of occurrences of an event in a fixed interval, such as the number of mutations in a strand of DNA per unit length.</li>
                 <li><strong>Astronomy:</strong> Modeling the number of stars observed in a fixed area of the sky during telescope observations.</li>
                 <li><strong>Traffic Flow:</strong> Analyzing the number of cars passing through a checkpoint in a fixed time period.</li>
               </ul>"
    ),
    
    "Binomial" = list(
      params = list(
        "size" = list("default" = 10, "min" = 1, "max" = Inf),
        "prob" = list("default" = 0.5, "min" = 0, "max" = 1)
      ),
      support = c(0, Inf),
      description = "The binomial distribution is a discrete probability distribution of the number of successes in a sequence of n independent experiments, each asking a yes-no question, and each with its own boolean-valued outcome.",
      example = "<ul>
                 <li><strong>Quality Testing:</strong> Determining the number of defective items in a batch of products when each item has a fixed probability of being defective.</li>
                 <li><strong>Clinical Trials:</strong> Modeling the number of patients who experience a side effect in a drug trial.</li>
                 <li><strong>Survey Analysis:</strong> Calculating the number of favorable responses in a survey where respondents answer 'yes' or 'no'.</li>
               </ul>"
    ),
    
    "Student's t" = list(
      params = list(
        "df" = list("default" = 5, "min" = 1, "max" = Inf)
      ),
      support = c(-Inf, Inf),
      description = "The Student's t-distribution is a continuous probability distribution that arises when estimating the mean of a normally distributed population in situations where the sample size is small. It is characterized by degrees of freedom parameter.",
      example = "<ul>
                 <li><strong>Small Sample Inference:</strong> Estimating the mean of a normally distributed population when the sample size is small and population standard deviation is unknown.</li>
                 <li><strong>Confidence Intervals:</strong> Constructing confidence intervals for the mean in small-sample experiments, such as pilot studies.</li>
                 <li><strong>Hypothesis Testing:</strong> Performing t-tests to compare means between two small independent samples in experimental research.</li>
               </ul>"
    ),
    
    "Cauchy" = list(
      params = list(
        "location" = list("default" = 0, "min" = -Inf, "max" = Inf),
        "scale" = list("default" = 1, "min" = 0.01, "max" = Inf)
      ),
      support = c(-Inf, Inf),
      description = "The Cauchy distribution is a continuous probability distribution with heavy tails and undefined mean and variance. It is characterized by a location parameter and a scale parameter.",
      example = "<ul>
                 <li><strong>Resonance Behavior:</strong> Modeling resonance phenomena in physics, such as the distribution of energy in a resonant system.</li>
                 <li><strong>Signal Processing:</strong> Representing the distribution of phase noise in oscillators where extreme values are more probable than in a normal distribution.</li>
                 <li><strong>Financial Returns:</strong> Analyzing asset returns that may have heavy tails and outliers, which the normal distribution may not capture effectively.</li>
               </ul>"
    ),
    
    "Weibull" = list(
      params = list(
        "shape" = list("default" = 2, "min" = 0.01, "max" = Inf),
        "scale" = list("default" = 1, "min" = 0.01, "max" = Inf)
      ),
      support = c(0, Inf),
      description = "The Weibull distribution is a continuous probability distribution used extensively in reliability engineering and failure analysis. It is characterized by a shape parameter and a scale parameter.",
      example = "<ul>
                 <li><strong>Material Strength:</strong> Modeling the breaking strength of materials under stress testing in engineering.</li>
                 <li><strong>Failure Times:</strong> Representing the distribution of failure times in reliability studies of mechanical components.</li>
                 <li><strong>Wind Speed Analysis:</strong> Analyzing wind speed data for wind energy assessments, as wind speeds often follow a Weibull distribution.</li>
               </ul>"
    ),
    
    "Laplace" = list(
      params = list(
        "location" = list("default" = 0, "min" = -Inf, "max" = Inf),
        "scale" = list("default" = 1, "min" = 0.01, "max" = Inf)
      ),
      support = c(-Inf, Inf),
      description = "The Laplace distribution, also known as the double exponential distribution, is a continuous probability distribution with a sharp peak at the mean and heavy tails. It is characterized by a location parameter and a scale parameter.",
      example = "<ul>
                 <li><strong>Econometrics:</strong> Modeling financial returns that exhibit sharp peaks and heavy tails compared to the normal distribution.</li>
                 <li><strong>Image Processing:</strong> Representing differences in pixel intensities in image edge detection algorithms.</li>
                 <li><strong>Error Modeling:</strong> Analyzing error terms in time series data that may have abrupt changes or jumps.</li>
               </ul>"
    ),
    
    "Logistic" = list(
      params = list(
        "location" = list("default" = 0, "min" = -Inf, "max" = Inf),
        "scale" = list("default" = 1, "min" = 0.01, "max" = Inf)
      ),
      support = c(-Inf, Inf),
      description = "The logistic distribution is similar to the normal distribution but has heavier tails. It is often used in logistic regression and neural networks. It is characterized by a location parameter and a scale parameter.",
      example = "<ul>
                 <li><strong>Growth Models:</strong> Modeling population growth in biology where growth accelerates rapidly and then slows down as it approaches a maximum limit.</li>
                 <li><strong>Logistic Regression:</strong> Serving as the underlying distribution in logistic regression models for binary classification problems.</li>
                 <li><strong>Psychometrics:</strong> Analyzing response times in cognitive tests where the probability of a correct response follows a logistic function over time.</li>
               </ul>"
    ),
    
    "Negative Binomial" = list(
      params = list(
        "size" = list("default" = 5, "min" = 1, "max" = Inf),
        "prob" = list("default" = 0.5, "min" = 0, "max" = 1)
      ),
      support = c(0, Inf),
      description = "The negative binomial distribution is a discrete probability distribution that models the number of failures before a specified number of successes occurs. It generalizes the geometric distribution.",
      example = "<ul>
                 <li><strong>Overdispersed Counts:</strong> Modeling count data with variance greater than the mean, such as the number of disease occurrences in epidemiology studies.</li>
                 <li><strong>Ecology Studies:</strong> Analyzing the number of species observed in ecological sampling when the occurrence is clustered.</li>
                 <li><strong>Quality Control:</strong> Representing the number of defective items found before a certain number of acceptable items are produced.</li>
               </ul>"
    )
  )
  
  # Generate parameter inputs dynamically
  output$parameterInputs <- renderUI({
    dist_name <- input$distribution
    params <- distributionParams[[dist_name]]$params
    inputs <- map(names(params), function(param_name) {
      param_info <- params[[param_name]]
      numericInput(param_name, label = param_name,
                   value = param_info$default,
                   min = param_info$min,
                   max = param_info$max)
    })
    do.call(tagList, inputs)
  })
  
  # Render the distribution plot using ggplot2
  output$distPlot <- renderPlot({
    dist_name <- input$distribution
    params <- distributionParams[[dist_name]]$params
    dist_params <- map_dbl(names(params), ~ input[[.x]])
    names(dist_params) <- names(params) # Ensure names are preserved
    
    # Define the support and generate x values
    support <- distributionParams[[dist_name]]$support
    xmin <- ifelse(is.infinite(support[1]), -10, support[1])
    xmax <- ifelse(is.infinite(support[2]), 10, support[2])
    
    # Adjust x range for specific distributions
    if (dist_name == "Gamma") {
      xmax <- max(10, qgamma(0.999, shape = dist_params[["shape"]], rate = dist_params[["rate"]]))
    } else if (dist_name == "Exponential") {
      xmax <- max(10, qexp(0.999, rate = dist_params[["rate"]]))
    } else if (dist_name == "Poisson") {
      xmax <- max(10, qpois(0.999, lambda = dist_params[["lambda"]]))
    } else if (dist_name == "Weibull") {
      xmax <- max(10, qweibull(0.999, shape = dist_params[["shape"]], scale = dist_params[["scale"]]))
    } else if (dist_name == "Negative Binomial") {
      xmax <- max(10, qnbinom(0.999, size = dist_params[["size"]], prob = dist_params[["prob"]]))
    }
    
    # Generate x values within the support
    if (dist_name == "Beta") {
      x <- seq(0, 1, length.out = 1000)
    } else if (dist_name %in% c("Gamma", "Exponential", "Weibull")) {
      x <- seq(max(0, xmin), xmax, length.out = 1000)
    } else if (dist_name %in% c("Poisson", "Binomial", "Negative Binomial")) {
      x <- 0:xmax
    } else {
      x <- seq(xmin, xmax, length.out = 1000)
    }
    
    # Calculate the density or probability values
    y <- switch(dist_name,
                "Normal" = dnorm(x, mean = dist_params[["mean"]], sd = dist_params[["sd"]]),
                "Beta" = dbeta(x, shape1 = dist_params[["shape1"]], shape2 = dist_params[["shape2"]]),
                "Gamma" = dgamma(x, shape = dist_params[["shape"]], rate = dist_params[["rate"]]),
                "Uniform" = dunif(x, min = dist_params[["min"]], max = dist_params[["max"]]),
                "Exponential" = dexp(x, rate = dist_params[["rate"]]),
                "Poisson" = dpois(x, lambda = dist_params[["lambda"]]),
                "Binomial" = dbinom(x, size = dist_params[["size"]], prob = dist_params[["prob"]]),
                "Student's t" = dt(x, df = dist_params[["df"]]),
                "Cauchy" = dcauchy(x, location = dist_params[["location"]], scale = dist_params[["scale"]]),
                "Weibull" = dweibull(x, shape = dist_params[["shape"]], scale = dist_params[["scale"]]),
                "Laplace" = {
                  # Laplace distribution (double exponential)
                  mu <- dist_params[["location"]]
                  b <- dist_params[["scale"]]
                  (1 / (2 * b)) * exp(-abs(x - mu) / b)
                },
                "Logistic" = dlogis(x, location = dist_params[["location"]], scale = dist_params[["scale"]]),
                "Negative Binomial" = dnbinom(x, size = dist_params[["size"]], prob = dist_params[["prob"]])
    )
    
    # Remove NA or infinite values
    data <- data.frame(x = x, y = y)
    data <- data %>% filter(is.finite(y))
    
    # Plot using ggplot2
    discrete_distributions <- c("Poisson", "Binomial", "Negative Binomial")
    if (dist_name %in% discrete_distributions) {
      ggplot(data, aes(x = x, y = y)) +
        geom_col(fill = "blue") +
        labs(title = paste(dist_name, "Distribution"), x = "x", y = "Probability") +
        theme_minimal(base_size = 16)
    } else {
      ggplot(data, aes(x = x, y = y)) +
        geom_line(color = "blue") +
        labs(title = paste(dist_name, "Distribution"), x = "x", y = "Density") +
        theme_minimal(base_size = 16)
    }
  })
  
  # Output the distribution description
  output$distDescription <- renderUI({
    dist_name <- input$distribution
    HTML(paste("<p>", distributionParams[[dist_name]]$description, "</p>"))
  })
  
  # Output the distribution example
  output$distExample <- renderUI({
    dist_name <- input$distribution
    HTML(paste("<p>", distributionParams[[dist_name]]$example, "</p>"))
  })
}


# Run the Shiny app
shinyApp(ui = ui, server = server)