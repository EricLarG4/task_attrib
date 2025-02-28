#################################################################################
# Task Assignment Script
# Purpose: Assign tasks to workers based on their scores while respecting
#          maximum SGSE weight limits for each worker
#################################################################################

# Load required packages
librarian::shelf(tidyverse, openxlsx)

#################################################################################
# Task Assignment Function
#################################################################################

#' Assign tasks to workers based on scores while respecting SGSE limits
#'
#' @description This function optimally assigns tasks to workers by maximizing scores
#' while ensuring each worker does not exceed their maximum SGSE weight capacity.
#' Worker scores are dynamically adjusted based on their current SGSE usage
#' percentage, scaled by a coefficient.
#'
#' @param input_filepath Path to the Excel file containing task scores and SGSE limits
#' @param num_workers Number of workers to consider for task assignment
#' @param scores_sheet Name of the sheet containing task scores
#' @param max_sgse_sheet Name of the sheet containing maximum SGSE limits for each worker
#' @param score_adjust_coef Coefficient to adjust scores based on SGSE usage (default: 1)
#'
#' @return A dataframe showing the assigned worker for each task along with its desirability score
#' @export
assign_tasks <- function(
  input_filepath = 'manual_input.xlsx',
  num_workers = 5,
  scores_sheet = "scores",
  max_sgse_sheet = "max_sgse",
  score_adjust_coef = 1
) {
  # Log the score adjustment coefficient being used
  cat("Using score adjustment coefficient:", score_adjust_coef, "\n")

  # Read input data from Excel using only openxlsx
  df_scores <- list(
    scores = openxlsx::read.xlsx(input_filepath, sheet = scores_sheet),
    max_sgse = openxlsx::read.xlsx(input_filepath, sheet = max_sgse_sheet)
  )

  # Generate worker IDs
  worker_list <- paste0('worker_', 1:num_workers)

  # Initialize worker SGSE tracking
  worker_sgse <- data.frame(
    worker = worker_list,
    total_sgse = rep(0, length(worker_list))
  ) %>%
    left_join(df_scores$max_sgse, by = "worker") %>%
    mutate(percentage_used = 0)

  # Calculate initial task desirability scores
  desirability <- df_scores$scores %>%
    pivot_longer(
      cols = starts_with("worker"),
      names_to = "worker",
      values_to = "score"
    ) %>%
    group_by(tasks) %>%
    summarize(mean_score = mean(score), .groups = "drop")

  # Function to adjust worker scores based on current SGSE usage
  adjust_scores <- function(scores_df, worker_sgse, coef) {
    scores_df %>%
      pivot_longer(
        cols = starts_with("worker"),
        names_to = "worker",
        values_to = "score"
      ) %>%
      left_join(worker_sgse, by = "worker") %>%
      mutate(adjusted_score = score * (1 - coef * percentage_used / 100)) %>%
      select(tasks, worker, adjusted_score)
  }

  # Initialize task assignment dataframe sorted by desirability
  df_assign <- data.frame(
    tasks = df_scores$scores$tasks,
    sgse = df_scores$scores$sgse,
    worker = NA
  ) %>%
    left_join(desirability, by = "tasks") %>%
    arrange(desc(mean_score))

  # Process all tasks in order of desirability
  for (idx in 1:nrow(df_assign)) {
    task <- df_assign$tasks[idx]

    # Recalculate adjusted scores based on current SGSE distribution
    # This ensures we're using the most up-to-date scores after each assignment
    adjusted_scores <- adjust_scores(
      df_scores$scores,
      worker_sgse,
      score_adjust_coef
    )

    # Find best workers for this task based on adjusted scores
    best_workers <- adjusted_scores %>%
      filter(tasks == task) %>%
      arrange(desc(adjusted_score)) %>%
      pull(worker)

    # Try to assign task to highest-scoring worker with available capacity
    task_sgse <- df_assign$sgse[idx]

    for (candidate_worker in best_workers) {
      worker_data <- worker_sgse %>% filter(worker == candidate_worker)
      current_sgse <- worker_data$total_sgse
      max_sgse <- worker_data$max_sgse

      # Check if worker has capacity for this task
      if ((current_sgse + task_sgse) <= max_sgse) {
        # Log if we couldn't use the highest-scoring worker
        if (candidate_worker != best_workers[1]) {
          cat(
            "Note: Best worker",
            best_workers[1],
            "could not be assigned to",
            task,
            "- assigning to",
            candidate_worker,
            "instead.\n"
          )
        }

        # Assign task and update worker's SGSE
        df_assign$worker[idx] <- candidate_worker

        worker_sgse <- worker_sgse %>%
          mutate(
            total_sgse = ifelse(
              worker == candidate_worker,
              total_sgse + task_sgse,
              total_sgse
            ),
            percentage_used = round(100 * total_sgse / max_sgse, 1)
          )

        break
      }
    }
  }

  # Check for unassigned tasks
  unassigned <- sum(is.na(df_assign$worker))
  if (unassigned > 0) {
    cat(
      "\nWarning:",
      unassigned,
      "tasks could not be assigned due to SGSE constraints.\n"
    )
  }

  # Output final assignment results
  cat("\nFinal assignment results:\n")
  print(df_assign)

  # Display tasks assigned to each worker
  cat("\nTasks assigned per worker:\n")
  cat("(Score adjustment coefficient:", score_adjust_coef, ")\n")

  for (wrkr in worker_list) {
    worker_tasks <- df_assign %>%
      filter(worker == wrkr) %>%
      arrange(desc(mean_score))

    total_worker_sgse <- sum(worker_tasks$sgse, na.rm = TRUE)
    worker_max_sgse <- worker_sgse %>%
      filter(worker == wrkr) %>%
      pull(max_sgse)

    cat(
      "\n",
      wrkr,
      " (",
      total_worker_sgse,
      "/",
      worker_max_sgse,
      " SGSE, ",
      round(100 * total_worker_sgse / worker_max_sgse, 1),
      "%):\n",
      sep = ""
    )

    if (nrow(worker_tasks) > 0) {
      for (i in 1:nrow(worker_tasks)) {
        cat(
          "  - ",
          worker_tasks$tasks[i],
          " (SGSE: ",
          worker_tasks$sgse[i],
          ", Score: ",
          round(worker_tasks$mean_score[i], 1),
          ")\n",
          sep = ""
        )
      }
    } else {
      cat("  No tasks assigned\n")
    }
  }

  # Display final SGSE distribution
  cat("\nWorker SGSE distribution:\n")
  print(worker_sgse)

  # Display final adjusted scores
  final_scores <- adjust_scores(
    df_scores$scores,
    worker_sgse,
    score_adjust_coef
  ) %>%
    pivot_wider(names_from = "worker", values_from = "adjusted_score")

  cat("\nFinal adjusted scores (using coefficient: ", score_adjust_coef, "):\n", sep = "")	
  print(final_scores)

  return(df_assign)
}

# Execute the task assignment function with default parameters
assign_tasks('manual_input.xlsx', 5, "scores", "max_sgse", 0)
