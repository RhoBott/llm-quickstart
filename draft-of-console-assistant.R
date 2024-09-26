# Load libraries/setup
library(dotenv)  # Will read OPENAI_API_KEY from .env file
library(elmer)
library(tidyverse)
library(rstudioapi)

# Initialize chat with elmer
chat <- elmer::new_chat_openai(
  system_prompt = "You are a data exploration assistant. Guide the user through exploring a data set by asking clear questions, providing complete explanation, and remaining curious."
)

# Helper functions
load_data <- function() {
  cat("Please input the path to your CSV file or type 'NONE' to use a provided data file: ")
  file_path <- readline()
  if (file_path == "NONE") {
    data <- ggplot2::diamonds
  } else {
    tryCatch({
      data <- read_csv(file_path)
    }, error = function(e) {
      cat("Error in loading data: ", e$message, "\n")
      return(NULL)
    })
  }
  return(data)
}

show_initial_data <- function(data) {
  cat("Here are the first few observations of your data set:\n")
  print(head(data))
}

# Summarize categorical and continuous variables
summarize_variable <- function(data, var_name) {
  var_data <- data[[var_name]]
  if (is.factor(var_data) || is.character(var_data)) {
    # For categorical variables
    summary_code <- paste(
      "data |>",
      "group_by(", var_name, ") |>",
      "count() |>",
      "arrange(desc(n))\n",
      "# Here, we group the data by the variable ", var_name, ", count the number of observations in each category, and arrange them in descending order.\n",
      "sum(is.na(data$", var_name, "))\n",
      "# Here, we count the number of missing values in the column ", var_name,
      sep = "\n"
    )
    summary_stats <- data |>
      group_by(.data[[var_name]]) |>
      count() |>
      arrange(desc(n))
    missing_values <- sum(is.na(data[[var_name]]))
  } else {
    # For continuous variables
    summary_code <- paste(
      "data |>",
      "summarize(",
      "  Mean = mean(", var_name, ", na.rm = TRUE),",
      "  Min = min(", var_name, ", na.rm = TRUE),",
      "  Max = max(", var_name, ", na.rm = TRUE)",
      ")\n",
      "# Here, we calculate the mean, minimum, and maximum values of ", var_name, "\n",
      "sum(is.na(data$", var_name, "))\n",
      "# At the same time, we count the number of missing values in the column ", var_name,
      sep = "\n"
    )
    summary_stats <- data |>
      summarize(
        Mean = mean(var_data, na.rm = TRUE),
        Min = min(var_data, na.rm = TRUE),
        Max = max(var_data, na.rm = TRUE)
      )
    missing_values <- sum(is.na(var_data))
  }

  cat("Generated summarization code:\n")
  cat(summary_code, "\n")
  # Use rstudioapi to show code if in RStudio
  if (rstudioapi::isAvailable()) {
    rstudioapi::documentNew(summary_code, type = "r")
    cat("Please look at the new code file in the RStudio IDE to see the code behind this table.\n")
  }

  return(list(summary_stats = summary_stats, missing_values = missing_values))
}

# Plot categorical and continuous variables
plot_variable <- function(data, var_name) {
  var_data <- data[[var_name]]
  if (is.factor(var_data) || is.character(var_data)) {
    plot_code <- paste(
      "ggplot(data, aes(x = ", var_name, ")) +",
      "  geom_bar()\n",
      "# This code uses a bar graph (`geom_bar()`) to visualize a variable (`", var_name, "`)",
      sep = "\n"
    )
    p <- ggplot(data, aes(x = .data[[var_name]])) +
      geom_bar()
    explanation <- "A bar chart is used to show the frequency of different categories in a categorical variable."
  } else {
    plot_code <- paste(
      "ggplot(data, aes(x = ", var_name, ")) +",
      "  geom_histogram(bins = 30)\n",
      "# This code uses a histogram (`geom_histogram()`) to show the distribution of a continuous variable (`", var_name, "`)",
      sep = "\n"
    )
    p <- ggplot(data, aes(x = .data[[var_name]])) +
      geom_histogram(bins = 30)
    explanation <- "A histogram is used to show the distribution of a continuous variable."
  }

  cat("Generated plotting code:\n")
  cat(plot_code, "\n")
  cat("Plot Explanation:\n", explanation, "\n")
  # Use rstudioapi to show code if in RStudio
  if (rstudioapi::isAvailable()) {
    rstudioapi::documentNew(plot_code, type = "r")
    cat("Please look at the new code file in the RStudio IDE to see the code behind this table.\n")
  }
  print(p)
}

# Helper function to summarize two variables
summarize_two_variables <- function(data, var_name1, var_name2) {
  if ((is.factor(data[[var_name1]]) || is.character(data[[var_name1]])) &&
      (is.factor(data[[var_name2]]) || is.character(data[[var_name2]]))) {
    # Both variables are categorical
    summary_code <- paste(
      "data |>",
      "count(", var_name1, ", ", var_name2, ") |>",
      "arrange(desc(n))\n",
      "# Here, we count the number of observations for each combination of the two categorical variables ", var_name1, " and ", var_name2, ", and arrange them in descending order.",
      sep = "\n"
    )
    summary_stats <- data |>
      count(.data[[var_name1]], .data[[var_name2]]) |>
      arrange(desc(n))
  } else {
    # Otherwise, we'll summarize one categorical and one continuous variable
    summary_code <- paste(
      "data |>",
      "group_by(", var_name1, ") |>",
      "summarize(",
      "  mean_", var_name2, " = mean(", var_name2, ", na.rm = TRUE),",
      "  ", var_name2, "_missing = sum(is.na(", var_name2, "))",
      ")\n",
      "# Here, we group the data by the variable ", var_name1, " and for each group, we calculate the mean of ", var_name2, " and count the number of missing values in ", var_name2,
      sep = "\n"
    )
    summary_stats <- data |>
      group_by(.data[[var_name1]]) |>
      summarize(
        mean = mean(.data[[var_name2]], na.rm = TRUE),
        missing = sum(is.na(.data[[var_name2]]))
      )
  }

  cat("Generated summarization code:\n")
  cat(summary_code, "\n")
  # Use rstudioapi to show code if in RStudio
  if (rstudioapi::isAvailable()) {
    rstudioapi::documentNew(summary_code, type = "r")
    cat("Please look at the new code file in the RStudio IDE to see the code behind this table.\n")
  }
  return(summary_stats)
}

plot_two_variables <- function(data, var_name1, var_name2) {
  if ((is.factor(data[[var_name1]]) || is.character(data[[var_name1]])) &&
      (is.factor(data[[var_name2]]) || is.character(data[[var_name2]]))) {
    plot_code <- paste(
      "ggplot(data, aes(x = ", var_name1, ", fill = ", var_name2, ")) +",
      "  geom_bar(position = 'dodge')\n",
      "# This code uses a bar graph (`geom_bar()`) to visualize the relationship between two categorical variables (`", var_name1, "` and `", var_name2, "`)",
      sep = "\n"
    )
    p <- ggplot(data, aes(x = .data[[var_name1]], fill = .data[[var_name2]])) +
      geom_bar(position = "dodge")
    explanation <- "This bar chart shows the frequency of different categories of one variable, divided by the categories of the other variable."
  } else if (is.factor(data[[var_name1]]) || is.character(data[[var_name1]])) {
    plot_code <- paste(
      "ggplot(data, aes(x = ", var_name1, ", y = ", var_name2, ")) +",
      "  geom_boxplot()\n",
      "# This code uses a boxplot (`geom_boxplot()`) to visualize the distribution of a continuous variable across different categories of a categorical variable (`", var_name1, "` and `", var_name2, "`)",
      sep = "\n"
    )
    p <- ggplot(data, aes(x = .data[[var_name1]], y = .data[[var_name2]])) +
      geom_boxplot()
    explanation <- "This boxplot shows the distribution of a continuous variable for different categories of a categorical variable."
  } else {
    plot_code <- paste(
      "ggplot(data, aes(x = ", var_name1, ", y = ", var_name2, ")) +",
      "  geom_point()\n",
      "# This code uses a scatter plot (`geom_point()`) to visualize the relationship between two continuous variables (`", var_name1, "` and `", var_name2, "`)",
      sep = "\n"
    )
    p <- ggplot(data, aes(x = .data[[var_name1]], y = .data[[var_name2]])) +
      geom_point()
    explanation <- "This scatter plot shows the relationship between two continuous variables."
  }

  cat("Generated plotting code:\n")
  cat(plot_code, "\n")
  cat("Plot Explanation:\n", explanation, "\n")
  # Use rstudioapi to show code if in RStudio
  if (rstudioapi::isAvailable()) {
    rstudioapi::documentNew(plot_code, type = "r")
    cat("Please look at the new code file in the RStudio IDE to see the code behind this table.\n")
  }
  print(p)
}

# Main interactive loop
run_data_explorer <- function() {
  cat("Welcome to the Conversational Data Explorer!\n")
  data <- NULL

  while (is.null(data)) {
    data <- load_data()
    if (is.null(data)) {
      cat("Failed to load the data set. Please try again.\n")
    }
  }

  show_initial_data(data)

  first_question <- TRUE

  while (TRUE) {
    if (!first_question) {
      show_initial_data(data)
    }

    if (first_question) {
      cat("What (single) variable do you want to investigate first? (We will look at multiple variables next)\n")
      first_question <- FALSE
    } else {
      cat("What single variable do you want to investigate next?\n")
    }

    var_name <- readline(prompt = ">>> ")

    if (tolower(var_name) == "exit") {
      cat("Very good! Goodbye!\n")
      break
    }

    if (!var_name %in% names(data)) {
      cat("Variable not found in the data set. Please try again.\n")
      next
    }

    response <- chat$chat(paste("What type of variable is", var_name, "? Is it categorical or continuous?"))

    # Handling user choices for variable exploration
    if (grepl("categorical", response, ignore.case = TRUE)) {
      cat("Okay! `", var_name, "` is a categorical variable. Let's take a look.\n")
      summary_result <- summarize_variable(data, var_name)
      summary_stats <- summary_result$summary_stats
      missing_values <- summary_result$missing_values
      print(summary_stats)
      cat("Number of missing values: ", missing_values, "\n")
    } else if (grepl("continuous", response, ignore.case = TRUE)) {
      cat("Okay! `", var_name, "` is a continuous variable. Let's take a look.\n")
      summary_result <- summarize_variable(data, var_name)
      summary_stats <- summary_result$summary_stats
      missing_values <- summary_result$missing_values
      print(summary_stats)
      cat("Number of missing values: ", missing_values, "\n")
    } else {
      cat("Hmm, I couldn't determine the type of variable. Could you please specify if it is categorical or continuous?\n")
      next
    }

    # Plotting
    cat("Would you like to see a plot of this variable? (yes/no): ")
    plot_choice <- readline()
    if (tolower(plot_choice) == "yes") {
      plot_variable(data, var_name)
    }

    # Ask if they want to investigate another variable
    cat("Do you want to investigate another variable on its own? (yes/no): ")
    continue_choice <- readline()

    if (tolower(continue_choice) == "no") {
      # Ask if user wants to investigate two variables together
      cat("Do you want to investigate two variables together? (yes/no): ")
      two_vars_choice <- readline()
      if (tolower(two_vars_choice) == "yes") {
        show_initial_data(data)
        cat("What is the first variable?\n")
        var_name1 <- readline(prompt = ">>> ")
        cat("What is the second variable?\n")
        var_name2 <- readline(prompt = ">>> ")

        if (!var_name1 %in% names(data) || !var_name2 %in% names(data)) {
          cat("One or both variables not found in the data set. Please try again.\n")
          next
        }

        summary_stats <- summarize_two_variables(data, var_name1, var_name2)
        print(summary_stats)

        # Ask if they want to see a plot of these variables together
        cat("Would you like to see a plot of these variables? (yes/no): ")
        plot_choice <- readline()
        if (tolower(plot_choice) == "yes") {
          plot_two_variables(data, var_name1, var_name2)
        } else {
          cat("Very good! Goodbye!\n")
          break
        }
      } else {
        cat("Very good! Goodbye!\n")
        break
      }
    } else {
      cat("What single variable do you want to investigate next?\n")
    }
  }
}

# Run the interactive console assistant
run_data_explorer()
