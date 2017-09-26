.onAttach <- function(...) {
  packageStartupMessage('Super Learner')
  packageStartupMessage('Version: ', utils::packageDescription('SuperLearner')$Version)
  packageStartupMessage('Package created on ', utils::packageDescription('SuperLearner')$Date, '\n')
}
