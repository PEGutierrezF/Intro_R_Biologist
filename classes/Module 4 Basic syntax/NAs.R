

na.rm = TRUE → ignora los NA solo dentro de esa función.
na.omit() → elimina los NA del objeto y crea una versión sin ellos.


summary(penguins$body_mass)
mean(penguins$body_mass, na.rm = TRUE)

x_clean <- na.omit(penguins$body_mass)
mean(x_clean)


length(penguins$body_mass)
length(x_clean)
