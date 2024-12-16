start_logger <- function() {
  # Set the log threshold and other options for logging
  logger::log_threshold(getOption("logger.threshold", default = logger::INFO))
  logger::log_appender(logger::appender_file("./_targets.log", max_lines = 1000))
  logger::log_info("Starting logger and targets pipeline.")
}
