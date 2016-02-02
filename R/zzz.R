.onAttach <- function(...) {
  packageStartupMessage('Super Learner')
  packageStartupMessage('Version: ', utils::packageDescription('SuperLearner')$Version)
  packageStartupMessage('Package created on ', utils::packageDescription('SuperLearner')$Date, '\n')
  # packageStartupMessage('Use SuperLearnerNews() to see changes from previous versions and latest news', '\n')
  # packageStartupMessage('Suggested packages to install for the Super Learner library:')
  # packageStartupMessage(utils::packageDescription('SuperLearner')$Suggests)
}
